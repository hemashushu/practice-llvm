// see also
// https://llvm.org/docs/tutorial/MyFirstLanguageFrontend/index.html

use libc;

use std::collections::HashSet;
use std::fs::File;
use std::io::Read;
use std::ptr;
use std::{collections::HashMap, ffi::CString};

use llvm_sys::{self, prelude::*};

mod parser;

use parser::Expression;

macro_rules! c_str {
    ($s:expr) => {
        concat!($s, "\0").as_ptr() as *const libc::c_char /* *const i8 */
    };
}

// pub fn c_str(s: &str) -> *const i8 /* *const core::ffi::c_char */ {
//     unsafe {
//         let c = CStr::from_bytes_with_nul_unchecked(s.as_bytes());
//         c.as_ptr()
//     }
// }
//
// pub fn unwrap_c_str(p: *const i8) -> &'static str {
//     unsafe {
//         let cstr = CStr::from_ptr(p);
//         cstr.to_str().unwrap()
//     }
// }

fn main() {
    let program = parse();
    println!("{:#?}", program);

    unsafe {
        codegen(program);
    }
}

fn parse() -> Vec<Expression> {
    let mut text_content = String::new();
    let mut file = File::open("my_module.script").unwrap();
    file.read_to_string(&mut text_content).unwrap();

    parser::parse::program(&text_content).unwrap()
}

unsafe fn codegen(program: Vec<Expression>) {
    let context = llvm_sys::core::LLVMContextCreate();
    let module = llvm_sys::core::LLVMModuleCreateWithName(c_str!("my_module"));
    let builder = llvm_sys::core::LLVMCreateBuilderInContext(context);

    let int_type = llvm_sys::core::LLVMInt64TypeInContext(context);
    let function_type = llvm_sys::core::LLVMFunctionType(int_type, ptr::null_mut(), 0, 0);
    let function = llvm_sys::core::LLVMAddFunction(module, c_str!("myfunc"), function_type);

    let base_block =
        llvm_sys::core::LLVMAppendBasicBlockInContext(context, function, c_str!("entry"));
    llvm_sys::core::LLVMPositionBuilderAtEnd(builder, base_block);

    let mut names = HashMap::new();
    insert_allocations(context, builder, &mut names, &program);

    let int_type = llvm_sys::core::LLVMInt64TypeInContext(context);
    let zero = llvm_sys::core::LLVMConstInt(int_type, 0, 0);
    let mut return_value = zero;

    for expression in program {
        return_value = codegen_expr(context, builder, function, &mut names, expression);
    }
    llvm_sys::core::LLVMBuildRet(builder, return_value);

    // print to stdout
    // llvm::core::LLVMDumpModule(module);

    // print bitcode to file
    // LLVMWriteBitcodeToFile(module, c_str!("my_module.bc"));

    // print to file
    llvm_sys::core::LLVMPrintModuleToFile(module, c_str!("my_module.ll"), ptr::null_mut());

    llvm_sys::core::LLVMDisposeBuilder(builder);
    llvm_sys::core::LLVMDisposeModule(module);
    llvm_sys::core::LLVMContextDispose(context);
}

unsafe fn insert_allocations(
    context: LLVMContextRef,
    builder: LLVMBuilderRef,
    names: &mut HashMap<String, LLVMValueRef>,
    expressions: &[Expression],
) {
    let mut variable_names = HashSet::new();
    for expression in expressions {
        match *expression {
            Expression::Assign(ref name, _) => {
                variable_names.insert(name);
            }

            _ => {}
        }
    }

    for variable_name in variable_names {
        let int_type = llvm_sys::core::LLVMInt64TypeInContext(context);
        let name = CString::new(variable_name.as_bytes()).unwrap();
        let pointer = llvm_sys::core::LLVMBuildAlloca(builder, int_type, name.as_ptr());

        names.insert(variable_name.to_owned(), pointer);
    }
}

unsafe fn codegen_expr(
    context: LLVMContextRef,
    builder: LLVMBuilderRef,
    func: LLVMValueRef,
    names: &mut HashMap<String, LLVMValueRef>,
    expression: Expression,
) -> LLVMValueRef {
    match expression {
        Expression::Literal(int_literal) => {
            let int_type = llvm_sys::core::LLVMInt64TypeInContext(context);
            llvm_sys::core::LLVMConstInt(int_type, int_literal.parse().unwrap(), 0)
        }
        Expression::Identifier(name) => {
            let pointer = names.get(&name).unwrap();
            let name = CString::new(name).unwrap();
            let int_type = llvm_sys::core::LLVMInt64TypeInContext(context);
            llvm_sys::core::LLVMBuildLoad2(builder, int_type, *pointer, name.as_ptr())
        }
        Expression::If(condition, then_body, else_body) => {
            let condition_value = codegen_expr(context, builder, func, names, *condition);
            let int_type = llvm_sys::core::LLVMInt64TypeInContext(context);
            let zero = llvm_sys::core::LLVMConstInt(int_type, 0, 0);

            let is_nonzero = llvm_sys::core::LLVMBuildICmp(
                builder,
                llvm_sys::LLVMIntPredicate::LLVMIntNE,
                condition_value,
                zero,
                c_str!("is_nonzero"),
            );

            let then_block =
                llvm_sys::core::LLVMAppendBasicBlockInContext(context, func, c_str!("then_block"));

            let else_block =
                llvm_sys::core::LLVMAppendBasicBlockInContext(context, func, c_str!("else_block"));

            let merge_block =
                llvm_sys::core::LLVMAppendBasicBlockInContext(context, func, c_str!("merge_block"));

            llvm_sys::core::LLVMBuildCondBr(builder, is_nonzero, then_block, else_block);

            // `then` block
            llvm_sys::core::LLVMPositionBuilderAtEnd(builder, then_block);
            let mut then_return = zero;
            for expr in then_body {
                then_return = codegen_expr(context, builder, func, names, expr);
            }
            llvm_sys::core::LLVMBuildBr(builder, merge_block);
            let then_block = llvm_sys::core::LLVMGetInsertBlock(builder);

            // `else` block
            llvm_sys::core::LLVMPositionBuilderAtEnd(builder, else_block);
            let mut else_return = zero;
            for expr in else_body {
                else_return = codegen_expr(context, builder, func, names, expr);
            }
            llvm_sys::core::LLVMBuildBr(builder, merge_block);
            let else_block = llvm_sys::core::LLVMGetInsertBlock(builder);

            // `iftmp` block
            llvm_sys::core::LLVMPositionBuilderAtEnd(builder, merge_block);
            let phi_name = CString::new("iftmp").unwrap();
            let phi = llvm_sys::core::LLVMBuildPhi(builder, int_type, phi_name.as_ptr());

            let mut values = vec![then_return, else_return];
            let mut blocks = vec![then_block, else_block];

            llvm_sys::core::LLVMAddIncoming(phi, values.as_mut_ptr(), blocks.as_mut_ptr(), 2);
            phi
        }
        Expression::Assign(s, e) => {
            let new_value = codegen_expr(context, builder, func, names, *e);
            let pointer = names.get(&s).unwrap();
            llvm_sys::core::LLVMBuildStore(builder, new_value, *pointer);
            new_value
        }
        Expression::Add(lhs, rhs) => {
            let lhs = codegen_expr(context, builder, func, names, *lhs);
            let rhs = codegen_expr(context, builder, func, names, *rhs);

            llvm_sys::core::LLVMBuildAdd(builder, lhs, rhs, c_str!("addtmp"))
        }

        Expression::Sub(lhs, rhs) => {
            let lhs = codegen_expr(context, builder, func, names, *lhs);
            let rhs = codegen_expr(context, builder, func, names, *rhs);

            llvm_sys::core::LLVMBuildSub(builder, lhs, rhs, c_str!("subtmp"))
        }

        Expression::Mul(lhs, rhs) => {
            let lhs = codegen_expr(context, builder, func, names, *lhs);
            let rhs = codegen_expr(context, builder, func, names, *rhs);

            llvm_sys::core::LLVMBuildMul(builder, lhs, rhs, c_str!("multmp"))
        }

        Expression::Div(lhs, rhs) => {
            let lhs = codegen_expr(context, builder, func, names, *lhs);
            let rhs = codegen_expr(context, builder, func, names, *rhs);

            llvm_sys::core::LLVMBuildUDiv(builder, lhs, rhs, c_str!("divtmp"))
        }
    }
}
