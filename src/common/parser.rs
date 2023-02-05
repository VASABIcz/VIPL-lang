use core::fmt;
use std::error::Error;
use std::fmt::{Debug, Display, Formatter, write};
use std::fs::read;

use crate::ast::{Expression, FunctionCall, ModType, Node, Op, Statement, VariableCreate, VariableMod, While};
use crate::ast;
use crate::ast::Expression::IntLiteral;
use crate::lexer::{lexingUnits, SourceProvider, Token, tokenize, TokenType};
use crate::lexer::TokenType::{CCB, Colon, Comma, CRB, Identifier, Minus, ORB, Return};
use crate::parser::ParsingUnitSearchType::{Ahead, Around};
use crate::vm::{DataType, VariableMetadata};

#[derive(Debug)]
struct NoSuchParsingUnit {
    typ: ParsingUnitSearchType,
    token: Option<Token>,
}

impl Display for NoSuchParsingUnit {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "no such {:?} parsing unit to parse token {:?}", self.typ, self.token)
    }
}

impl Error for NoSuchParsingUnit {}

#[derive(Debug)]
struct InvalidOperation {
    operation: Operation,
    expected: String,
}

impl Display for InvalidOperation {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "expected {:?} to be {}", self.operation, self.expected)
    }
}

impl Error for InvalidOperation {}

fn getParsingUnit<'a>(
    tokens: &mut TokenProvider,
    typ: ParsingUnitSearchType,
    parsingUnits: &'a [Box<dyn ParsingUnit>],
) -> Option<&'a Box<dyn ParsingUnit>> {
    parsingUnits.iter().find(|it| {
        let parserType = it.getType();

        let canParse = match typ {
            ParsingUnitSearchType::Around => parserType != ParsingUnitSearchType::Ahead,
            ParsingUnitSearchType::Back => parserType == ParsingUnitSearchType::Back,
            ParsingUnitSearchType::Ahead => parserType == ParsingUnitSearchType::Ahead,
        };

        canParse && it.canParse(tokens)
    })
}

pub fn parseOne(
    tokens: &mut TokenProvider,
    typ: ParsingUnitSearchType,
    parsingUnits: &[Box<dyn ParsingUnit>],
    previous: Option<Operation>,
) -> Result<Operation, Box<dyn Error>> {
    let u = getParsingUnit(tokens, typ.clone(), parsingUnits).ok_or(Box::new(NoSuchParsingUnit { typ, token: tokens.peekOne().map(|it| { it.clone() }) }))?;
    /*
    if u.is_none() {
        println!("next {:?}", tokens.peekOne());
    }

     */
    u.parse(tokens, previous, parsingUnits)
}

pub fn parse(
    tokens: &mut TokenProvider,
    typ: ParsingUnitSearchType,
    parsingUnits: &[Box<dyn ParsingUnit>],
    previous: Option<Operation>,
    isPrevUser: &mut bool,
) -> Result<Vec<Operation>, Box<dyn Error>> {
    let mut buf = vec![];
    let mut counter = 0;


    'main: while !tokens.isDone() {
        counter += 1;
        for unit in parsingUnits.iter() {
            let parserType = unit.getType();

            let canParse = match typ {
                ParsingUnitSearchType::Around => true,
                ParsingUnitSearchType::Back => parserType == ParsingUnitSearchType::Back,
                ParsingUnitSearchType::Ahead => parserType == ParsingUnitSearchType::Ahead,
            };

            if canParse && unit.canParse(&tokens) {
                let res = if !*isPrevUser && (parserType == Around || parserType == Around) && counter == 1 {
                    *isPrevUser = true;
                    println!("fuuuck");
                    unit.parse(tokens, previous.clone(), parsingUnits)?
                } else {
                    unit.parse(tokens, None, parsingUnits)?
                };
                buf.push(res);
                continue 'main;
            }
        }
        return Err(Box::new(NoSuchParsingUnit { typ, token: tokens.peekOne().map(|it| { it.clone() }) }))
    }
    Ok(buf)
}

pub fn parseTokens(
    toks: Vec<Token>,
) -> Result<Vec<Operation>, Box<dyn Error>> {
    let mut buf = vec![];
    let parsingUnits = parsingUnits();
    let mut tokens = TokenProvider::new(toks);

    'main: while !tokens.isDone() {
        for unit in parsingUnits.iter() {
            let parserType = unit.getType();

            let canParse = parserType == ParsingUnitSearchType::Ahead;

            if canParse && unit.canParse(&tokens) {
                let res = unit.parse(&mut tokens, None, &parsingUnits)?;
                buf.push(res);
                continue 'main;
            }
        }
        println!("c");
        return Err(Box::new(NoSuchParsingUnit { typ: Ahead, token: tokens.peekOne().map(|it| { it.clone() }) }))
    }
    Ok(buf)
}

/*
fn parse(parsingUnits: &mut [Box<dyn ParsingUnit>], mut tokens: TokenProvider, typ: ParsingUnitSearchType) -> Vec<Operation> {
    let mut buf = vec![];

    'main: while !tokens.isDone() {
        for unit in parsingUnits.iter_mut() {
            let parserType = unit.getType();

            let canParse = match typ {
                ParsingUnitSearchType::Around => {
                    true
                }
                ParsingUnitSearchType::Back => {
                    parserType == ParsingUnitSearchType::Back
                }
                ParsingUnitSearchType::Ahead => {
                    parserType == ParsingUnitSearchType::Ahead
                }
            };

            if unit.canParse(&tokens) {
                let res = unit.parse(&mut tokens, None);
                buf.push(res);
                continue 'main
            }
        }
        panic!("error {:?}", tokens.peekOne());
    }
    buf
}

 */

pub struct TokenProvider {
    pub tokens: Vec<Token>,
    pub index: usize,
}

#[derive(Debug)]
struct InvalidToken {
    msg: String,
}

impl fmt::Display for InvalidToken {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.msg)
    }
}

impl Error for InvalidToken {}

impl TokenProvider {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, index: 0 }
    }

    fn peekOne(&self) -> Option<&Token> {
        self.tokens.get(self.index)
    }

    fn peekIndex(&self, offset: usize) -> Option<&Token> {
        self.tokens.get(self.index + offset)
    }

    fn consume(&mut self) {
        self.index += 1
    }

    fn getAssert(&mut self, typ: TokenType) -> Result<&Token, Box<dyn Error>> {
        let i = self.index;
        self.consume();
        let t = match self.tokens.get(i) {
            None => {
                return Err(Box::new(InvalidToken { msg: format!("invalid token got None expected {:?}", typ) }))
            }
            Some(v) => v
        };
        if t.typ != typ {
            return Err(Box::new(InvalidToken { msg: format!("invalid token got {:?} expected {:?}", t, typ) }))
        }
        Ok(t)
    }
    fn isPeekType(&self, typ: TokenType) -> bool {
        let t = self.peekOne();

        match t {
            None => false,
            Some(v) => v.typ == typ
        }
    }

    fn isPeekIndexType(&self, typ: TokenType, offset: usize) -> bool {
        let t = self.peekIndex(offset);

        match t {
            None => false,
            Some(v) => v.typ == typ
        }
    }

    fn getIdentifier(&mut self) -> Result<String, Box<dyn Error>> {
        let t = self.getAssert(TokenType::Identifier)?;

        Ok(t.str.clone())
    }

    fn getToken(&mut self) -> Result<Token, Box<dyn Error>> {
        let i = self.index;
        self.consume();
        match self.tokens.get(i) {
            None => {
                return Err(Box::new(InvalidToken { msg: format!("invalid token got None") }))
            }
            Some(v) => Ok(v.clone())
        }
    }

    pub fn isDone(&self) -> bool {
        self.index >= self.tokens.len()
    }
}

#[derive(Debug, Clone)]
pub enum Operation {
    FunctionDef(Node),
    Statement(Statement),
    Expression(Expression),
}

impl Operation {
    fn asExpr(self) -> Result<Expression, Box<dyn Error>> {
        match self {
            Operation::Expression(e) => Ok(e),
            _ => Err(Box::new(InvalidOperation { operation: self.clone(), expected: "Expression".to_string() })),
        }
    }

    fn asStatement(self) -> Result<Statement, Box<dyn Error>> {
        let clone = self.clone();
        match self {
            Operation::Statement(s) => Ok(s),
            Operation::Expression(e) => match e {
                Expression::FunctionCall(f) => Ok(Statement::FunctionExpr(f)),
                _ => Err(Box::new(InvalidOperation {
                    operation: clone,
                    expected: String::from("Statement"),
                })),
            },
            _ => Err(Box::new(InvalidOperation {
                operation: clone,
                expected: String::from("Statement"),
            })),
        }
    }
}

#[derive(Eq, PartialEq, Debug, Clone)]
pub enum ParsingUnitSearchType {
    Around,
    Back,
    Ahead,
}

pub trait ParsingUnit {
    fn getType(&self) -> ParsingUnitSearchType;

    fn canParse(&self, tokenProvider: &TokenProvider) -> bool;

    fn parse(
        &self,
        tokenProvider: &mut TokenProvider,
        previous: Option<Operation>,
        parser: &[Box<dyn ParsingUnit>],
    ) -> Result<Operation, Box<dyn Error>>;

    fn getPriority(&self) -> usize;

    fn setPriority(&mut self, priority: usize);
}

pub struct FunctionParsingUnit;

impl ParsingUnit for FunctionParsingUnit {
    fn getType(&self) -> ParsingUnitSearchType {
        ParsingUnitSearchType::Ahead
    }

    fn canParse(&self, tokenProvider: &TokenProvider) -> bool {
        tokenProvider.isPeekType(TokenType::Fn)
    }

    fn parse(
        &self,
        tokens: &mut TokenProvider,
        _previous: Option<Operation>,
        parser: &[Box<dyn ParsingUnit>],
    ) -> Result<Operation, Box<dyn Error>> {
        tokens.getAssert(TokenType::Fn);
        let name = tokens.getIdentifier()?;
        let mut args = vec![];
        let mut argCount = 0;
        let mut returnType = None;

        tokens.getAssert(TokenType::ORB);
        while !tokens.isPeekType(TokenType::CRB) {
            let argName = tokens.getIdentifier()?;
            tokens.getAssert(TokenType::Colon)?;
            let argType = tokens.getIdentifier()?;
            let t = DataType::fromString(&argType);
            args.push(VariableMetadata {
                name: argName,
                typ: t,
            });
            argCount += 1;
            if tokens.isPeekType(Comma) {
                tokens.consume();
            }
        }
        tokens.getAssert(CRB)?;

        if tokens.isPeekType(Colon) {
            tokens.getAssert(Colon)?;
            returnType = Some(DataType::fromString(&tokens.getIdentifier()?));
        }

        let statements = parseBody(tokens, parser)?;

        Ok(Operation::FunctionDef(Node::FunctionDef(crate::ast::FunctionDef {
            name,
            args,
            argCount,
            body: statements,
            returnType,
        })))
    }

    fn getPriority(&self) -> usize {
        usize::MAX
    }

    fn setPriority(&mut self, priority: usize) {}
}

pub struct StatementVarCreateParsingUnit;

impl ParsingUnit for StatementVarCreateParsingUnit {
    fn getType(&self) -> ParsingUnitSearchType {
        ParsingUnitSearchType::Ahead
    }

    fn canParse(&self, tokens: &TokenProvider) -> bool {
        // tokens.isPeekType(TokenType::Semicolon)
        tokens.isPeekType(TokenType::Identifier) && tokens.isPeekIndexType(TokenType::Equals, 1)
    }

    fn parse(
        &self,
        tokens: &mut TokenProvider,
        _previous: Option<Operation>,
        parser: &[Box<dyn ParsingUnit>],
    ) -> Result<Operation, Box<dyn Error>> {
        let name = tokens.getIdentifier()?;

        tokens.getAssert(TokenType::Equals)?;

        // tokens.getAssert(TokenType::Semicolon);
        let res = parseOne(tokens, Ahead, parser, None)?;
        let par = getParsingUnit(tokens, Around, parser);

        let op = match par {
            None => res.asExpr()?,
            Some(p) => p.parse(tokens, Some(res), parser)?.asExpr()?,
        };


        Ok(Operation::Statement(Statement::VariableCreate(VariableCreate { name, init: Some(op) })))
    }

    fn getPriority(&self) -> usize {
        usize::MAX
    }

    fn setPriority(&mut self, priority: usize) {}
}

pub struct CallParsingUnit;

impl ParsingUnit for CallParsingUnit {
    fn getType(&self) -> ParsingUnitSearchType {
        Ahead
    }

    fn canParse(&self, tokenProvider: &TokenProvider) -> bool {
        tokenProvider.isPeekType(Identifier) && tokenProvider.isPeekIndexType(ORB, 1)
    }

    fn parse(
        &self,
        tokens: &mut TokenProvider,
        previous: Option<Operation>,
        parser: &[Box<dyn ParsingUnit>],
    ) -> Result<Operation, Box<dyn Error>> {
        let name = tokens.getIdentifier()?;
        tokens.getAssert(TokenType::ORB)?;

        let mut args = vec![];

        while !tokens.isPeekType(TokenType::CRB) {
            let res = parseOne(tokens, Ahead, parser, None)?;
            let par = getParsingUnit(tokens, Around, parser);

            let op = match par {
                None => res,
                Some(p) => p.parse(tokens, Some(res), parser)?,
            };

            args.push(op.asExpr()?);
            if !tokens.isPeekType(TokenType::CRB) {
                tokens.getAssert(TokenType::Comma)?;
            }
        }

        tokens.getAssert(TokenType::CRB)?;

        Ok(Operation::Expression(Expression::FunctionCall(FunctionCall {
            name,
            arguments: args,
        })))
    }

    fn getPriority(&self) -> usize {
        usize::MAX
    }

    fn setPriority(&mut self, priority: usize) {}
}

pub struct ArithmeticParsingUnit {
    pub op: Op,
    pub typ: TokenType,
    pub priority: usize
}

impl ParsingUnit for ArithmeticParsingUnit {
    fn getType(&self) -> ParsingUnitSearchType {
        Around
    }

    fn canParse(&self, tokenProvider: &TokenProvider) -> bool {
        tokenProvider.isPeekType(self.typ)
    }

    fn parse(
        &self,
        tokens: &mut TokenProvider,
        previous: Option<Operation>,
        parser: &[Box<dyn ParsingUnit>],
    ) -> Result<Operation, Box<dyn Error>> {
        tokens.consume();
        let res = parseOne(tokens, Ahead, parser, None)?;
        let par = getParsingUnit(tokens, Around, parser);

        match par {
            None => Ok(Operation::Expression(Expression::ArithmeticOp {
                left: Box::new(previous.unwrap().asExpr()?),
                right: Box::new(res.asExpr()?),
                op: self.op.clone(),
            })),
            Some(p) => {
                if self.priority < p.getPriority() {
                    Ok(p.parse(tokens, Some(Operation::Expression(Expression::ArithmeticOp {
                        left: Box::new(previous.unwrap().asExpr()?),
                        right: Box::new(res.asExpr()?),
                        op: self.op.clone(),
                    })), parser)?)
                } else {
                    Ok(Operation::Expression(Expression::ArithmeticOp {
                        left: Box::new(previous.unwrap().asExpr()?),
                        right: Box::new(p.parse(tokens, Some(res), parser)?.asExpr()?),
                        op: self.op.clone(),
                    }))
                }
            },
        }
    }

    fn getPriority(&self) -> usize {
        self.priority
    }

    fn setPriority(&mut self, priority: usize) {
        self.priority = priority
    }
}

pub struct NumericParsingUnit;

impl ParsingUnit for NumericParsingUnit {
    fn getType(&self) -> ParsingUnitSearchType {
        Ahead
    }

    fn canParse(&self, tokenProvider: &TokenProvider) -> bool {
        let peek = match tokenProvider.peekOne() {
            None => return false,
            Some(v) => v
        }.typ;

        if peek == TokenType::IntLiteral
            || peek == TokenType::LongLiteral
            || peek == TokenType::FloatLiteral
            || peek == TokenType::DoubleLiteral {
            return true
        }

        let peek1 = match tokenProvider.peekIndex(1) {
            None => return false,
            Some(v) => v
        }.typ;

        if peek == TokenType::Minus && (peek1 == TokenType::DoubleLiteral || peek1 == TokenType::LongLiteral || peek1 == TokenType::FloatLiteral || peek1 == TokenType::IntLiteral) {
            return true
        }
        return false
    }

    fn parse(
        &self,
        tokenProvider: &mut TokenProvider,
        _previous: Option<Operation>,
        _parser: &[Box<dyn ParsingUnit>],
    ) -> Result<Operation, Box<dyn Error>> {
        let mut peek = tokenProvider.peekOne().unwrap();

        let mut buf = String::new();

        if peek.typ == Minus {
            buf.push('-');
            tokenProvider.consume();
            peek = tokenProvider.peekOne().unwrap()
        }

        let res = match peek.typ {
            TokenType::IntLiteral => {
                buf.push_str(&peek.str);
                Operation::Expression(IntLiteral(buf))
            },
            TokenType::LongLiteral => {
                buf.push_str(&peek.str);
                Operation::Expression(Expression::LongLiteral(buf))
            }
            TokenType::FloatLiteral => {
                buf.push_str(&peek.str);
                Operation::Expression(Expression::FloatLiteral(buf))
            }
            TokenType::DoubleLiteral => {
                buf.push_str(&peek.str);
                Operation::Expression(Expression::DoubleLiteral(buf))
            }
            _ => {
                return Err(Box::new(InvalidToken { msg: format!("expected numeric token got {:?}", peek) }))
            },
        };
        tokenProvider.consume();
        Ok(res)
    }

    fn getPriority(&self) -> usize {
        usize::MAX
    }

    fn setPriority(&mut self, priority: usize) {}
}

pub struct BoolParsingUnit;

impl ParsingUnit for BoolParsingUnit {
    fn getType(&self) -> ParsingUnitSearchType {
        Ahead
    }

    fn canParse(&self, tokenProvider: &TokenProvider) -> bool {
        tokenProvider.isPeekType(TokenType::True) || tokenProvider.isPeekType(TokenType::False)
    }

    fn parse(
        &self,
        tokenProvider: &mut TokenProvider,
        _previous: Option<Operation>,
        _parser: &[Box<dyn ParsingUnit>],
    ) -> Result<Operation, Box<dyn Error>> {
        if tokenProvider.isPeekType(TokenType::False) {
            tokenProvider.getAssert(TokenType::False);
            return Ok(Operation::Expression(Expression::BoolLiteral(false)));
        }
        tokenProvider.getAssert(TokenType::True);
        Ok(Operation::Expression(Expression::BoolLiteral(true)))
    }

    fn getPriority(&self) -> usize {
        usize::MAX
    }

    fn setPriority(&mut self, priority: usize) {}
}

pub struct VariableParsingUnit;

impl ParsingUnit for VariableParsingUnit {
    fn getType(&self) -> ParsingUnitSearchType {
        Ahead
    }

    fn canParse(&self, tokenProvider: &TokenProvider) -> bool {
        tokenProvider.isPeekType(Identifier)
    }

    fn parse(
        &self,
        tokenProvider: &mut TokenProvider,
        _previous: Option<Operation>,
        _parser: &[Box<dyn ParsingUnit>],
    ) -> Result<Operation, Box<dyn Error>> {
        Ok(Operation::Expression(Expression::Variable(tokenProvider.getIdentifier()?)))
    }

    fn getPriority(&self) -> usize {
        usize::MAX
    }

    fn setPriority(&mut self, priority: usize) {}
}

pub struct ReturnParsingUnit;

impl ParsingUnit for ReturnParsingUnit {
    fn getType(&self) -> ParsingUnitSearchType {
        Ahead
    }

    fn canParse(&self, tokenProvider: &TokenProvider) -> bool {
        tokenProvider.isPeekType(Return)
    }

    fn parse(&self, tokenProvider: &mut TokenProvider, previous: Option<Operation>, parser: &[Box<dyn ParsingUnit>]) -> Result<Operation, Box<dyn Error>> {
        tokenProvider.getAssert(Return);
        let exp = parseExpr(tokenProvider, parser)?;
        return Ok(Operation::Statement(Statement::Return(ast::Return { exp })))
    }

    fn getPriority(&self) -> usize {
        usize::MAX
    }

    fn setPriority(&mut self, priority: usize) {}
}

pub struct WhileParsingUnit;

impl ParsingUnit for WhileParsingUnit {
    fn getType(&self) -> ParsingUnitSearchType {
        Ahead
    }

    fn canParse(&self, tokenProvider: &TokenProvider) -> bool {
        tokenProvider.isPeekType(TokenType::While)
    }

    fn parse(
        &self,
        tokenProvider: &mut TokenProvider,
        _previous: Option<Operation>,
        parser: &[Box<dyn ParsingUnit>],
    ) -> Result<Operation, Box<dyn Error>> {
        tokenProvider.getAssert(TokenType::While);

        let op = parseExpr(tokenProvider, parser)?;

        let statements = parseBody(tokenProvider, parser)?;

        Ok(Operation::Statement(Statement::While(While {
            exp: op,
            body: statements,
        })))
    }

    fn getPriority(&self) -> usize {
        usize::MAX
    }

    fn setPriority(&mut self, priority: usize) {}
}

pub struct IfParsingUnit;

fn parseBody(tokenProvider: &mut TokenProvider, parser: &[Box<dyn ParsingUnit>]) -> Result<Vec<Statement>, Box<dyn Error>> {
    let mut statements = vec![];

    tokenProvider.getAssert(TokenType::OCB);

    while !tokenProvider.isPeekType(CCB) {
        statements.push(
            parseOne(tokenProvider, Ahead, parser, None)?.asStatement()?,
        );
    }

    tokenProvider.getAssert(TokenType::CCB);

    Ok(statements)
}

fn parseExpr(tokenProvider: &mut TokenProvider, parser: &[Box<dyn ParsingUnit>]) -> Result<Expression, Box<dyn Error>> {
    let mut encounteredBrackets = false;

    if tokenProvider.isPeekType(TokenType::ORB) {
        tokenProvider.getAssert(TokenType::ORB)?;
        encounteredBrackets = true;
    }

    let res = parseOne(tokenProvider, Ahead, parser, None)?;
    let par = getParsingUnit(tokenProvider, Around, parser);

    let op = match par {
        None => res,
        Some(p) => p.parse(tokenProvider, Some(res), parser)?,
    };

    if encounteredBrackets {
        tokenProvider.getAssert(TokenType::CRB);
    }

    op.asExpr()
}

impl ParsingUnit for IfParsingUnit {
    fn getType(&self) -> ParsingUnitSearchType {
        Ahead
    }

    fn canParse(&self, tokenProvider: &TokenProvider) -> bool {
        tokenProvider.isPeekType(TokenType::If)
    }

    fn parse(
        &self,
        tokenProvider: &mut TokenProvider,
        _previous: Option<Operation>,
        parser: &[Box<dyn ParsingUnit>],
    ) -> Result<Operation, Box<dyn Error>> {
        tokenProvider.getAssert(TokenType::If);

        let cond = parseExpr(tokenProvider, parser)?;

        let statements = parseBody(tokenProvider, parser)?;

        if !tokenProvider.isPeekType(TokenType::Else) {
            return Ok(Operation::Statement(ast::Statement::If(ast::If {
                condition: cond,
                body: statements,
                elseBody: None,
            })));
        }

        tokenProvider.getAssert(TokenType::Else)?;

        let elseBody = parseBody(tokenProvider, parser)?;

        Ok(Operation::Statement(ast::Statement::If(ast::If {
            condition: cond,
            body: statements,
            elseBody: Some(elseBody),
        })))
    }

    fn getPriority(&self) -> usize {
        usize::MAX
    }

    fn setPriority(&mut self, priority: usize) {}
}

struct BracketsParsingUnit;

impl ParsingUnit for BracketsParsingUnit {
    fn getType(&self) -> ParsingUnitSearchType {
        Ahead
    }

    fn canParse(&self, tokenProvider: &TokenProvider) -> bool {
        tokenProvider.isPeekType(ORB)
    }

    fn parse(&self, tokenProvider: &mut TokenProvider, previous: Option<Operation>, parser: &[Box<dyn ParsingUnit>]) -> Result<Operation, Box<dyn Error>> {
        Ok(Operation::Expression(parseExpr(tokenProvider, parser)?))
    }

    fn getPriority(&self) -> usize {
        usize::MAX
    }

    fn setPriority(&mut self, priority: usize) {}
}

struct VarModParsingUnit;

impl ParsingUnit for VarModParsingUnit {
    fn getType(&self) -> ParsingUnitSearchType {
        Ahead
    }

    fn canParse(&self, tokenProvider: &TokenProvider) -> bool {
        tokenProvider.isPeekType(Identifier)
            &&
            tokenProvider.isPeekIndexType(TokenType::AddAs, 1)
            || tokenProvider.isPeekIndexType(TokenType::SubAs, 1)
            || tokenProvider.isPeekIndexType(TokenType::MulAs, 1)
            || tokenProvider.isPeekIndexType(TokenType::DivAs, 1)
    }

    fn parse(&self, tokenProvider: &mut TokenProvider, previous: Option<Operation>, parser: &[Box<dyn ParsingUnit>]) -> Result<Operation, Box<dyn Error>> {
        let varName = tokenProvider.getIdentifier()?;
        let modType = match tokenProvider.getToken()?.typ {
            TokenType::AddAs => ModType::Add,
            TokenType::SubAs => ModType::Sub,
            TokenType::DivAs => ModType::Div,
            TokenType::MulAs => ModType::Mul,
            _ => panic!()
        };

        let expr = parseExpr(tokenProvider, parser)?;

        Ok(Operation::Statement(Statement::VariableMod(VariableMod {
            varName,
            modType,
            expr,
        })))
    }

    fn getPriority(&self) -> usize { usize::MAX }

    fn setPriority(&mut self, priority: usize) {}
}

pub fn parsingUnits() -> Vec<Box<dyn ParsingUnit>> {
    vec![
        Box::new(VarModParsingUnit),
        Box::new(WhileParsingUnit),
        Box::new(FunctionParsingUnit),
        Box::new(StatementVarCreateParsingUnit),
        Box::new(NumericParsingUnit),
        Box::new(CallParsingUnit),
        Box::new(ArithmeticParsingUnit {
            op: Op::Mul,
            typ: TokenType::Mul,
            priority: 0,
        }),
        Box::new(ArithmeticParsingUnit {
            op: Op::Div,
            typ: TokenType::Div,
            priority: 1
        }),
        Box::new(ArithmeticParsingUnit {
            op: Op::Add,
            typ: TokenType::Plus,
            priority: 2
        }),
        Box::new(ArithmeticParsingUnit {
            op: Op::Sub,
            typ: TokenType::Minus,
            priority: 3
        }),
        Box::new(ArithmeticParsingUnit {
            op: Op::Eq,
            typ: TokenType::Eq,
            priority: 4
        }),
        Box::new(ArithmeticParsingUnit {
            op: Op::Less,
            typ: TokenType::Less,
            priority: 5
        }),
        Box::new(ArithmeticParsingUnit {
            op: Op::Gt,
            typ: TokenType::Gt,
            priority: 6,
        }),
        Box::new(BracketsParsingUnit),
        Box::new(VariableParsingUnit),
        Box::new(IfParsingUnit),
        Box::new(BoolParsingUnit),
        Box::new(ReturnParsingUnit),
    ]
}