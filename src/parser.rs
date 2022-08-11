use peg::parser;

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Expression {
    If(Box<Expression>, Vec<Expression>, Vec<Expression>),
    Assign(String, Box<Expression>),

    Add(Box<Expression>, Box<Expression>),
    Sub(Box<Expression>, Box<Expression>),
    Mul(Box<Expression>, Box<Expression>),
    Div(Box<Expression>, Box<Expression>),

    Identifier(String),
    Literal(String),
}

parser! {
pub grammar parse() for str {

    pub rule program() -> Vec<Expression>
        = statements()

    rule statements() ->  Vec<Expression>
        = e:(expression() ** "\n") "\n" {e}

    rule expression() -> Expression
        = if_expression()
        / i:identifier_str() _ "=" _ s:sum() { Expression::Assign(i, Box::new(s)) }
        / sum()

    rule _ = [' ' | '\t']*

    rule if_expression() -> Expression
        = "if" _ e:expression() _ "{" _ "\n" _ then_body:statements() _ "}" _ "else" _ "{" _ "\n" _  else_body: statements() _ "}" {
            Expression::If(Box::new(e), then_body, else_body)
        }

    rule sum() -> Expression
        = l:product() _ "+" _ r:product() { Expression::Add(Box::new(l), Box::new(r)) }
        / l:product() _ "-" _ r:product() { Expression::Sub(Box::new(l), Box::new(r)) }
        / product()

    rule product() -> Expression
        = l:atom() _ "*" _ r:atom() { Expression::Mul(Box::new(l), Box::new(r)) }
        / l:atom() _ "/" _ r:atom() { Expression::Div(Box::new(l), Box::new(r)) }
        / atom()

    rule atom() -> Expression
        = n:number() {n}
        / s:identifier_str() { Expression::Identifier(s) }
        / "(" _ v:expression() _ ")" { v }

    rule identifier_str() -> String
        = s:$(['a'..='z']+) {s.to_owned() }

    rule number() -> Expression
        = n:$(['0'..='9']+) { Expression::Literal(n.to_owned()) }
}}
