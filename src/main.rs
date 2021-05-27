use std::{
    cmp::{Ord, Ordering},
    convert::TryFrom,
    error::Error,
    fmt,
    io::prelude::*,
    iter::Peekable,
    slice::Iter,
};

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum Token {
    Plus,
    Dash,
    Star,
    Slash,
    Carrot,
    RightParen,
    LeftParen,
    End,
    Number(i64),
}

impl Token {
    fn is_binary(&self) -> bool {
        match self {
            Token::Plus => true,
            Token::Dash => true,
            Token::Star => true,
            Token::Slash => true,
            Token::Carrot => true,
            _ => false,
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
enum Operator {
    Add,
    Multiply,
    Divide,
    Subtract,
    Power,
    Negative,
    Sentinel,
}

impl Operator {
    fn cmp_val(&self) -> usize {
        match self {
            Operator::Negative => 4,
            Operator::Power => 6,
            Operator::Multiply => 5,
            Operator::Divide => 5,
            Operator::Add => 3,
            Operator::Subtract => 3,
            Operator::Sentinel => 1,
        }
    }
}

impl TryFrom<Token> for Operator {
    type Error = &'static str;

    fn try_from(token: Token) -> Result<Self, Self::Error> {
        match token {
            Token::Plus => Ok(Operator::Add),
            Token::Star => Ok(Operator::Multiply),
            Token::Dash => Ok(Operator::Subtract),
            Token::Carrot => Ok(Operator::Power),
            Token::Slash => Ok(Operator::Divide),
            _ => Err("Can only convert operators"),
        }
    }
}

impl PartialOrd for Operator {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp_val().cmp(&other.cmp_val()))
    }
}

impl Ord for Operator {
    fn cmp(&self, other: &Self) -> Ordering {
        self.cmp_val().cmp(&other.cmp_val())
    }
}

#[derive(Debug, PartialEq, Eq)]
enum Expression {
    Binary(Operator, Box<Expression>, Box<Expression>),
    Unary(Operator, Box<Expression>),
    Number(i64),
}

impl Expression {
    fn eval(&mut self) -> i64 {
        match self {
            Expression::Number(n) => *n,
            Expression::Unary(_negative, expr) => -1 * expr.eval(),
            Expression::Binary(Operator::Add, expr1, expr2) => expr1.eval() + expr2.eval(),
            Expression::Binary(Operator::Multiply, expr1, expr2) => expr1.eval() * expr2.eval(),
            Expression::Binary(Operator::Subtract, expr1, expr2) => expr1.eval() - expr2.eval(),
            Expression::Binary(Operator::Power, expr1, expr2) => {
                let expr1 = expr1.eval();
                let mut expr2 = expr2.eval();
                if expr2 < 0 {
                    expr2 *= -1;
                    println!("Negative numbers aren't allowed in exponents");
                }

                match expr1.checked_pow(expr2 as u32) {
                    Some(v) => v,
                    None => {
                        eprintln!("{} ^ {} would overflow", expr1, expr2);
                        0
                    }
                }
            }
            Expression::Binary(Operator::Divide, expr1, expr2) => expr1.eval() / expr2.eval(),
            _ => {
                panic!("Unreachable code: for expr {:?}", self);
            }
        }
    }
}

#[derive(Debug)]
struct SyntaxError {
    message: String,
    level: String,
}

impl SyntaxError {
    fn new_lex_error(message: String) -> Self {
        SyntaxError {
            message,
            level: "Lex".to_string(),
        }
    }

    fn new_parse_error(message: String) -> Self {
        SyntaxError {
            message,
            level: "Parse".to_string(),
        }
    }
}

impl fmt::Display for SyntaxError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} Error {}", self.level, self.message)
    }
}

impl Error for SyntaxError {}

struct ClimbingParser<'a> {
    iter: &'a mut Peekable<Iter<'a, Token>>,
}

impl<'a> ClimbingParser<'a> {
    fn new(iter: &'a mut Peekable<Iter<'a, Token>>) -> Self {
        ClimbingParser { iter }
    }

    fn assert_next(&mut self, token: Token) -> Result<(), SyntaxError> {
        let next = self.iter.next();
        if let None = next {
            return Err(SyntaxError::new_parse_error(
                "Unexpected end of input".to_string(),
            ));
        }

        if *next.unwrap() != token {
            return Err(SyntaxError::new_parse_error(format!(
                "Expected {:?} actual {:?}",
                token,
                next.unwrap(),
            )));
        }

        Ok(())
    }

    fn primary(&mut self) -> Result<Expression, SyntaxError> {
        match self.iter.next().unwrap() {
            Token::Dash => {
                let op = Operator::Negative;
                let expr = self.expression(op.cmp_val())?;
                Ok(Expression::Unary(op, Box::new(expr)))
            }
            Token::RightParen => {
                let expr = self.expression(0)?;
                self.assert_next(Token::LeftParen)?;
                Ok(expr)
            }
            Token::Number(n) => Ok(Expression::Number(*n)),
            tok => Err(SyntaxError::new_parse_error(format!(
                "Unexpected token {:?}",
                tok
            ))),
        }
    }

    fn expression(&mut self, precedence: usize) -> Result<Expression, SyntaxError> {
        let mut expr = self.primary()?;
        while let Some(tok) = self.iter.peek() {
            if !tok.is_binary() {
                break;
            }
            let operator = Operator::try_from(**tok).unwrap();
            if operator.cmp_val() < precedence {
                break;
            }
            self.iter.next();
            let inner_precedence = match operator {
                Operator::Power => operator.cmp_val(),
                _ => 1 + operator.cmp_val(),
            };
            let rhs = self.expression(inner_precedence)?;
            expr = Expression::Binary(operator, Box::new(expr), Box::new(rhs));
        }

        Ok(expr)
    }

    fn parse(&mut self) -> Result<Expression, SyntaxError> {
        let ast = self.expression(0)?;
        self.assert_next(Token::End)?;
        Ok(ast)
    }
}

struct DescentParser<'a> {
    iter: &'a mut Peekable<Iter<'a, Token>>,
}

impl<'a> DescentParser<'a> {
    fn new(iter: &'a mut Peekable<Iter<'a, Token>>) -> Self {
        DescentParser { iter }
    }

    fn assert_next(&mut self, token: Token) -> Result<(), SyntaxError> {
        let next = self.iter.next();
        if let None = next {
            return Err(SyntaxError::new_parse_error(
                "Unexpected end of input".to_string(),
            ));
        }

        if *next.unwrap() != token {
            return Err(SyntaxError::new_parse_error(format!(
                "Expected {:?} actual {:?}",
                token,
                next.unwrap(),
            )));
        }

        Ok(())
    }

    fn primary(&mut self) -> Result<Expression, SyntaxError> {
        let next = self.iter.next().unwrap();

        match next {
            Token::Number(n) => Ok(Expression::Number(*n)),
            Token::RightParen => {
                let expr = self.expression()?;
                self.assert_next(Token::LeftParen)?;
                Ok(expr)
            }
            Token::Dash => {
                let expr = self.factor()?;
                Ok(Expression::Unary(Operator::Negative, Box::new(expr)))
            }
            _ => Err(SyntaxError::new_parse_error(format!(
                "Unexpected token {:?}",
                next
            ))),
        }
    }

    fn factor(&mut self) -> Result<Expression, SyntaxError> {
        let expr = self.primary()?;
        let next = self.iter.peek().unwrap();

        if *next == &Token::Carrot {
            self.iter.next();
            let rhs = self.factor()?;
            return Ok(Expression::Binary(
                Operator::Power,
                Box::new(expr),
                Box::new(rhs),
            ));
        }

        Ok(expr)
    }

    fn term(&mut self) -> Result<Expression, SyntaxError> {
        let mut expr: Expression = self.factor()?;

        loop {
            let next = self.iter.peek().unwrap();
            match next {
                Token::Star => {
                    self.iter.next();
                    let rhs = self.factor()?;
                    expr = Expression::Binary(Operator::Multiply, Box::new(expr), Box::new(rhs));
                }
                Token::Slash => {
                    self.iter.next();
                    let rhs = self.factor()?;
                    expr = Expression::Binary(Operator::Divide, Box::new(expr), Box::new(rhs));
                }
                _ => break,
            };
        }

        Ok(expr)
    }

    fn expression(&mut self) -> Result<Expression, SyntaxError> {
        let mut expr: Expression = self.term()?;

        loop {
            let next = self.iter.peek().unwrap();
            match next {
                Token::Plus => {
                    self.iter.next();
                    let rhs = self.term()?;
                    expr = Expression::Binary(Operator::Add, Box::new(expr), Box::new(rhs));
                }
                Token::Dash => {
                    self.iter.next();
                    let rhs = self.term()?;
                    expr = Expression::Binary(Operator::Subtract, Box::new(expr), Box::new(rhs));
                }
                _ => break,
            };
        }

        Ok(expr)
    }

    fn parse(&mut self) -> Result<Expression, SyntaxError> {
        let ast = self.expression()?;
        self.assert_next(Token::End)?;
        Ok(ast)
    }
}

struct ShuntingYardParser<'a> {
    iter: &'a mut Peekable<Iter<'a, Token>>,
    operator_stack: Vec<Operator>,
    operand_stack: Vec<Expression>,
}

impl<'a> ShuntingYardParser<'a> {
    fn new(iter: &'a mut Peekable<Iter<'a, Token>>) -> Self {
        ShuntingYardParser {
            iter,
            operator_stack: vec![Operator::Sentinel],
            operand_stack: vec![],
        }
    }

    fn assert_next(&mut self, token: Token) -> Result<(), SyntaxError> {
        let next = self.iter.next();
        if let None = next {
            return Err(SyntaxError::new_parse_error(
                "Unexpected end of input".to_string(),
            ));
        }

        if *next.unwrap() != token {
            return Err(SyntaxError::new_parse_error(format!(
                "Expected {:?} actual {:?}",
                next.unwrap(),
                token
            )));
        }

        Ok(())
    }

    fn primary(&mut self) -> Result<(), SyntaxError> {
        let next = self.iter.peek();
        if let None = next {
            return Err(SyntaxError::new_parse_error(
                "Unexpected end of input".to_string(),
            ));
        }
        let next = *self.iter.peek().unwrap();

        match next {
            Token::Number(num) => {
                let _token = self.iter.next();
                self.operand_stack.push(Expression::Number(*num));
            }
            Token::RightParen => {
                let _token = self.iter.next();
                self.operator_stack.push(Operator::Sentinel);
                self.expression()?;
                self.assert_next(Token::LeftParen)?;
                self.operator_stack.pop();
            }
            Token::Dash => {
                let _token = self.iter.next();
                self.push_operator(Operator::Negative);
                self.primary()?;
            }
            _ => {
                return Err(SyntaxError::new_parse_error(format!(
                    "Unexpected token {:?}",
                    next
                )));
            }
        }
        Ok(())
    }

    fn expression(&mut self) -> Result<(), SyntaxError> {
        self.primary()?;

        while let Some(tok) = self.iter.peek() {
            if tok.is_binary() {
                let token = self.iter.next().unwrap();
                let op = Operator::try_from(*token).unwrap();
                self.push_operator(op);
                self.primary()?;
            } else {
                break;
            }
        }

        while *self.operator_stack.last().unwrap() != Operator::Sentinel {
            self.pop_operator();
        }

        Ok(())
    }

    fn parse(&mut self) -> Result<(), SyntaxError> {
        self.expression()?;
        self.assert_next(Token::End)
    }

    fn pop_operator(&mut self) {
        let top = self.operator_stack.pop().unwrap();
        let right = self.operand_stack.pop().unwrap();
        match top {
            Operator::Negative => {
                self.operand_stack
                    .push(Expression::Unary(top, Box::new(right)));
            }
            _ => {
                let left = self.operand_stack.pop().unwrap();
                self.operand_stack
                    .push(Expression::Binary(top, Box::new(left), Box::new(right)));
            }
        };
    }

    fn push_operator(&mut self, op: Operator) {
        while &op < self.operator_stack.last().unwrap() {
            self.pop_operator();
        }
        self.operator_stack.push(op);
    }
}

fn lex(code: String) -> Result<Vec<Token>, SyntaxError> {
    let mut iter = code.chars().peekable();
    let mut tokens: Vec<Token> = Vec::new();
    let mut leftover: Option<char> = None;

    loop {
        let ch = match leftover {
            Some(ch) => ch,
            None => match iter.next() {
                None => break,
                Some(ch) => ch,
            },
        };
        leftover = None;
        match ch {
            ' ' => continue,
            '+' => tokens.push(Token::Plus),
            '*' => tokens.push(Token::Star),
            '/' => tokens.push(Token::Slash),
            '^' => tokens.push(Token::Carrot),
            ')' => tokens.push(Token::LeftParen),
            '(' => tokens.push(Token::RightParen),
            '-' => tokens.push(Token::Dash),
            ch if ch.is_ascii_digit() => {
                let number_stream: String = iter
                    .by_ref()
                    .take_while(|c| match c.is_ascii_digit() {
                        true => true,
                        false => {
                            leftover = Some(*c);
                            false
                        }
                    })
                    .collect();
                let number: i64 = format!("{}{}", ch, number_stream).parse().unwrap();
                tokens.push(Token::Number(number));
            }
            _ => {
                return Err(SyntaxError::new_lex_error(format!(
                    "Unrecognized character {}",
                    ch
                )))
            }
        }
    }

    tokens.push(Token::End);

    Ok(tokens)
}

fn get_line() -> String {
    print!("> ");
    std::io::stdout().flush().unwrap();
    let mut input = String::new();
    match std::io::stdin().read_line(&mut input) {
        Ok(_s) => {}
        Err(_e) => {}
    };
    input.trim().to_string()
}

fn eval_shunting_yard(code: String) -> Result<(), Box<dyn Error>> {
    let tokens = lex(code)?;
    let mut token_iter = tokens.iter().peekable();
    let mut parser = ShuntingYardParser::new(&mut token_iter);
    let result = parser.parse();
    match result {
        Ok(()) => {}
        Err(e) => return Err(Box::new(e)),
    }

    for mut expr in parser.operand_stack {
        println!("{}", expr.eval());
    }

    Ok(())
}

fn eval_climbing(code: String) -> Result<(), Box<dyn Error>> {
    let tokens = lex(code)?;
    let mut token_iter = tokens.iter().peekable();
    let mut parser = ClimbingParser::new(&mut token_iter);
    let result = parser.parse();
    match result {
        Ok(mut ast) => println!("{}", ast.eval()),
        Err(e) => return Err(Box::new(e)),
    }

    Ok(())
}

fn eval_descent(code: String) -> Result<(), Box<dyn Error>> {
    let tokens = lex(code)?;
    let mut token_iter = tokens.iter().peekable();
    let mut parser = DescentParser::new(&mut token_iter);
    let result = parser.parse();
    match result {
        Ok(mut ast) => println!("{}", ast.eval()),
        Err(e) => return Err(Box::new(e)),
    }

    Ok(())
}

fn eval(code: String, shunting_yard: bool, climbing: bool) -> Result<(), Box<dyn Error>> {
    if shunting_yard {
        return eval_shunting_yard(code);
    }
    if climbing {
        return eval_climbing(code);
    }

    eval_descent(code)
}

fn run_repl() -> Result<(), Box<dyn Error>> {
    loop {
        let line = get_line();
        if line == "quit" {
            break ();
        }
        if let Err(e) = eval(line, false, true) {
            println!("Error: {}", e);
        }
    }
    Ok(())
}

fn run() -> Result<(), Box<dyn Error>> {
    run_repl()
}

fn main() {
    if let Err(e) = run() {
        eprintln!("Error: {}", e);
    }
}
