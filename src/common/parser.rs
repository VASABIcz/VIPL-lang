use core::fmt;
use std::collections::HashMap;
use std::error::Error;
use std::fmt::{Debug, Display, Formatter};
use std::usize;

use crate::ast;
use crate::ast::{
    ArrayAccess, Expression, FunctionCall, ModType, Node, Op, Statement, StructDef, VariableCreate,
    VariableMod, While,
};
use crate::ast::Expression::IntLiteral;
use crate::lexer::{LexingUnit, Token, TokenType};
use crate::lexer::TokenType::{
    CCB, CharLiteral, Colon, Comma, Continue, CRB, CSB, Equals, Identifier, Loop, Minus,
    Native, Not, OCB, ORB, OSB, Return, StringLiteral, Struct,
};
use crate::parser::ParsingUnitSearchType::{Ahead, Around, Back};
use crate::vm::{DataType, Generic, MyStr, ObjectMeta, VariableMetadata};

#[derive(Debug)]
struct NoSuchParsingUnit {
    typ: ParsingUnitSearchType,
    token: Option<Token>,
}

impl Display for NoSuchParsingUnit {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "no such {:?} parsing unit to parse token {:?}",
            self.typ, self.token
        )
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
    let u =
        getParsingUnit(tokens, typ.clone(), parsingUnits).ok_or(Box::new(NoSuchParsingUnit {
            typ,
            token: tokens.peekOne().cloned(),
        }))?;
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
    let mut opBuf = None;

    'main: while !tokens.isDone() {
        counter += 1;
        for unit in parsingUnits.iter() {
            let parserType = unit.getType();

            let canParse = match typ {
                Around => true,
                Back => parserType == Back || parserType == Around,
                Ahead => parserType == Ahead || parserType == Around,
            };

            if canParse && unit.canParse(tokens) {
                let res =
                    if !*isPrevUser && (parserType == Around || parserType == Back) && counter == 1
                    {
                        *isPrevUser = true;
                        unit.parse(tokens, previous.clone(), parsingUnits)?
                    } else {

                        if parserType == Around || parserType == Back {
                            unit.parse(tokens, opBuf.clone().take(), parsingUnits)?
                        } else {
                            unit.parse(tokens, None, parsingUnits)?
                        }
                    };

                if parserType == Back {
                    opBuf = Some(res);
                    continue 'main;
                }

                buf.push(res);
                continue 'main;
            }
        }

        for unit in parsingUnits.iter() {
            let parserType = unit.getType();

            if unit.canParse(tokens) {
                let res =
                    if !*isPrevUser && (parserType == Around || parserType == Back) && counter == 1
                    {
                        *isPrevUser = true;
                        unit.parse(tokens, previous.clone(), parsingUnits)?
                    } else {

                        if parserType == Around || parserType == Back {
                            unit.parse(tokens, opBuf.clone().take(), parsingUnits)?
                        } else {
                            unit.parse(tokens, None, parsingUnits)?
                        }
                    };

                if parserType == Back {
                    opBuf = Some(res);
                    continue 'main;
                }

                // println!("{:?}", &res);
                buf.push(res);
                continue 'main;
            }
        }
        return Err(Box::new(NoSuchParsingUnit {
            typ,
            token: tokens.peekOne().cloned(),
        }));
    }
    match opBuf {
        None => {}
        Some(v) => buf.push(v),
    }
    Ok(buf)
}

pub fn parseTokens(toks: Vec<Token>) -> Result<Vec<Operation>, Box<dyn Error>> {
    let mut buf = vec![];
    let parsingUnits = unsafe { &parsingUnits() };
    let mut tokens = TokenProvider::new(toks);

    'main: while !tokens.isDone() {
        for unit in parsingUnits.iter() {
            let parserType = unit.getType();

            let canParse = parserType == ParsingUnitSearchType::Ahead;

            if canParse && unit.canParse(&tokens) {
                // println!("sus parse");
                let res = unit.parse(&mut tokens, None, parsingUnits)?;
                buf.push(res);
                continue 'main;
            }
        }
        return Err(Box::new(NoSuchParsingUnit {
            typ: Ahead,
            token: tokens.peekOne().cloned(),
        }));
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
                return Err(Box::new(InvalidToken {
                    msg: format!("invalid token got None expected {typ:?}"),
                }))
            }
            Some(v) => v,
        };
        if t.typ != typ {
            return Err(Box::new(InvalidToken {
                msg: format!("invalid token got {t:?} expected {typ:?}"),
            }));
        }
        Ok(t)
    }
    fn isPeekType(&self, typ: TokenType) -> bool {
        let t = self.peekOne();

        match t {
            None => false,
            Some(v) => v.typ == typ,
        }
    }

    fn isPeekIndexType(&self, typ: TokenType, offset: usize) -> bool {
        let t = self.peekIndex(offset);

        match t {
            None => false,
            Some(v) => v.typ == typ,
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
            None => Err(Box::new(InvalidToken {
                msg: "invalid token got None".to_string(),
            })),
            Some(v) => Ok(v.clone()),
        }
    }

    pub fn isDone(&self) -> bool {
        self.index >= self.tokens.len()
    }
}

#[derive(Debug, Clone)]
pub enum Operation {
    Global(Node),
    Statement(Statement),
    Expr(Expression),
}

impl Operation {
    fn asExpr(self) -> Result<Expression, Box<dyn Error>> {
        match self {
            Operation::Expr(e) => Ok(e),
            _ => Err(Box::new(InvalidOperation {
                operation: self.clone(),
                expected: "Expression".to_string(),
            })),
        }
    }

    fn asStatement(self) -> Result<Statement, Box<dyn Error>> {
        let clone = self.clone();
        match self {
            Operation::Statement(s) => Ok(s),
            Operation::Expr(e) => match e {
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
        let mut isNative = false;

        tokens.getAssert(TokenType::Fn)?;

        if tokens.isPeekType(Native) {
            tokens.getAssert(Native)?;

            isNative = true;
        }

        let name = tokens.getIdentifier()?;
        let mut args = vec![];
        let mut argCount = 0;
        let mut returnType = None;

        tokens.getAssert(ORB)?;
        while !tokens.isPeekType(CRB) {
            let argName = tokens.getIdentifier()?;
            tokens.getAssert(Colon)?;

            let t = parseDataType(tokens)?;

            args.push(VariableMetadata {
                name: MyStr::Runtime(argName.into_boxed_str()),
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
            returnType = Some(parseDataType(tokens)?);
        }

        let statements = parseBody(tokens, parser)?;

        Ok(Operation::Global(Node::FunctionDef(
            crate::ast::FunctionDef {
                name,
                args,
                argCount,
                body: statements,
                returnType,
                isNative,
            },
        )))
    }

    fn getPriority(&self) -> usize {
        usize::MAX
    }

    fn setPriority(&mut self, _priority: usize) {}
}

pub struct StatementVarParsingUnit;

impl ParsingUnit for StatementVarParsingUnit {
    fn getType(&self) -> ParsingUnitSearchType {
        ParsingUnitSearchType::Ahead
    }

    fn canParse(&self, tokens: &TokenProvider) -> bool {
        // tokens.isPeekType(TokenType::Semicolon)
        tokens.isPeekType(TokenType::Identifier)
            && (tokens.isPeekIndexType(TokenType::Equals, 1)
            || tokens.isPeekIndexType(TokenType::Colon, 1))
    }

    fn parse(
        &self,
        tokens: &mut TokenProvider,
        _previous: Option<Operation>,
        parser: &[Box<dyn ParsingUnit>],
    ) -> Result<Operation, Box<dyn Error>> {
        let name = tokens.getIdentifier()?;
        let mut typeHint = None;

        if tokens.isPeekType(Colon) {
            tokens.getAssert(Colon)?;
            typeHint = Some(parseDataType(tokens)?);
        }

        tokens.getAssert(TokenType::Equals)?;

        // tokens.getAssert(TokenType::Semicolon);
        let res = parseOne(tokens, Ahead, parser, None)?;
        let par = getParsingUnit(tokens, Around, parser);

        let op = match par {
            None => res.asExpr()?,
            Some(p) => p.parse(tokens, Some(res), parser)?.asExpr()?,
        };

        Ok(Operation::Statement(Statement::Variable(VariableCreate {
            name,
            init: Some(op),
            typeHint,
        })))
    }

    fn getPriority(&self) -> usize {
        usize::MAX
    }

    fn setPriority(&mut self, _priority: usize) {}
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
        _previous: Option<Operation>,
        parser: &[Box<dyn ParsingUnit>],
    ) -> Result<Operation, Box<dyn Error>> {
        let name = tokens.getIdentifier()?;
        tokens.getAssert(ORB)?;

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

        Ok(Operation::Expr(Expression::FunctionCall(FunctionCall {
            name: name.into(),
            arguments: args,
        })))
    }

    fn getPriority(&self) -> usize {
        usize::MAX
    }

    fn setPriority(&mut self, _priority: usize) {}
}

pub struct ArithmeticParsingUnit {
    pub op: Op,
    pub typ: TokenType,
    pub priority: usize,
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
            None => Ok(Operation::Expr(Expression::ArithmeticOp {
                // FIXME
                left: Box::new(previous.unwrap().asExpr()?),
                right: Box::new(res.asExpr()?),
                op: self.op.clone(),
            })),
            Some(p) => {
                if self.priority < p.getPriority() {
                    Ok(p.parse(
                        tokens,
                        Some(Operation::Expr(Expression::ArithmeticOp {
                            left: Box::new(previous.unwrap().asExpr()?),
                            right: Box::new(res.asExpr()?),
                            op: self.op.clone(),
                        })),
                        parser,
                    )?)
                } else {
                    Ok(Operation::Expr(Expression::ArithmeticOp {
                        left: Box::new(previous.unwrap().asExpr()?),
                        right: Box::new(p.parse(tokens, Some(res), parser)?.asExpr()?),
                        op: self.op.clone(),
                    }))
                }
            }
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
            Some(v) => v,
        }
            .typ;

        if peek == TokenType::IntLiteral
            || peek == TokenType::LongLiteral
            || peek == TokenType::FloatLiteral
            || peek == TokenType::DoubleLiteral
        {
            return true;
        }

        let peek1 = match tokenProvider.peekIndex(1) {
            None => return false,
            Some(v) => v,
        }
            .typ;

        if peek == Minus
            && (peek1 == TokenType::DoubleLiteral
            || peek1 == TokenType::LongLiteral
            || peek1 == TokenType::FloatLiteral
            || peek1 == TokenType::IntLiteral)
        {
            return true;
        }
        false
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
                Operation::Expr(IntLiteral(buf))
            }
            TokenType::LongLiteral => {
                buf.push_str(&peek.str);
                Operation::Expr(Expression::LongLiteral(buf))
            }
            TokenType::FloatLiteral => {
                buf.push_str(&peek.str);
                Operation::Expr(Expression::FloatLiteral(buf))
            }
            TokenType::DoubleLiteral => {
                buf.push_str(&peek.str);
                Operation::Expr(Expression::DoubleLiteral(buf))
            }
            _ => {
                return Err(Box::new(InvalidToken {
                    msg: format!("expected numeric token got {peek:?}"),
                }))
            }
        };
        tokenProvider.consume();
        Ok(res)
    }

    fn getPriority(&self) -> usize {
        usize::MAX
    }

    fn setPriority(&mut self, _priority: usize) {}
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
            tokenProvider.getAssert(TokenType::False)?;
            return Ok(Operation::Expr(Expression::BoolLiteral(false)));
        }
        tokenProvider.getAssert(TokenType::True)?;
        Ok(Operation::Expr(Expression::BoolLiteral(true)))
    }

    fn getPriority(&self) -> usize {
        usize::MAX
    }

    fn setPriority(&mut self, _priority: usize) {}
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
        Ok(Operation::Expr(Expression::Variable(
            tokenProvider.getIdentifier()?,
        )))
    }

    fn getPriority(&self) -> usize {
        usize::MAX
    }

    fn setPriority(&mut self, _priority: usize) {}
}

pub struct ReturnParsingUnit;

impl ParsingUnit for ReturnParsingUnit {
    fn getType(&self) -> ParsingUnitSearchType {
        Ahead
    }

    fn canParse(&self, tokenProvider: &TokenProvider) -> bool {
        tokenProvider.isPeekType(Return)
    }

    fn parse(
        &self,
        tokenProvider: &mut TokenProvider,
        _previous: Option<Operation>,
        parser: &[Box<dyn ParsingUnit>],
    ) -> Result<Operation, Box<dyn Error>> {
        tokenProvider.getAssert(Return)?;
        let exp = parseExpr(tokenProvider, parser)?;
        Ok(Operation::Statement(Statement::Return(ast::Return { exp })))
    }

    fn getPriority(&self) -> usize {
        usize::MAX
    }

    fn setPriority(&mut self, _priority: usize) {}
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
        tokenProvider.getAssert(TokenType::While)?;

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

    fn setPriority(&mut self, _priority: usize) {}
}

pub struct IfParsingUnit;

fn parseBody(
    tokenProvider: &mut TokenProvider,
    parser: &[Box<dyn ParsingUnit>],
) -> Result<Vec<Statement>, Box<dyn Error>> {
    let mut statements = vec![];

    tokenProvider.getAssert(TokenType::OCB)?;

    while !tokenProvider.isPeekType(CCB) {
        statements.push(parseOne(tokenProvider, Ahead, parser, None)?.asStatement()?);
    }

    tokenProvider.getAssert(TokenType::CCB)?;

    Ok(statements)
}

fn parseExpr(
    tokenProvider: &mut TokenProvider,
    parser: &[Box<dyn ParsingUnit>],
) -> Result<Expression, Box<dyn Error>> {
    let res = parseOne(tokenProvider, Ahead, parser, None)?;
    let par = getParsingUnit(tokenProvider, Around, parser);

    let op = match par {
        None => res,
        Some(p) => p.parse(tokenProvider, Some(res), parser)?,
    };

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
        tokenProvider.getAssert(TokenType::If)?;

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

    fn setPriority(&mut self, _priority: usize) {}
}

struct BracketsParsingUnit;

impl ParsingUnit for BracketsParsingUnit {
    fn getType(&self) -> ParsingUnitSearchType {
        Ahead
    }

    fn canParse(&self, tokenProvider: &TokenProvider) -> bool {
        tokenProvider.isPeekType(ORB)
    }

    fn parse(
        &self,
        tokenProvider: &mut TokenProvider,
        _previous: Option<Operation>,
        parser: &[Box<dyn ParsingUnit>],
    ) -> Result<Operation, Box<dyn Error>> {
        tokenProvider.getAssert(ORB)?;
        let expr = Ok(Operation::Expr(parseExpr(tokenProvider, parser)?));
        tokenProvider.getAssert(CRB)?;
        expr
    }

    fn getPriority(&self) -> usize {
        usize::MAX
    }

    fn setPriority(&mut self, _priority: usize) {}
}

struct VarModParsingUnit;

impl ParsingUnit for VarModParsingUnit {
    fn getType(&self) -> ParsingUnitSearchType {
        Ahead
    }

    fn canParse(&self, tokenProvider: &TokenProvider) -> bool {
        tokenProvider.isPeekType(Identifier) && tokenProvider.isPeekIndexType(TokenType::AddAs, 1)
            || tokenProvider.isPeekIndexType(TokenType::SubAs, 1)
            || tokenProvider.isPeekIndexType(TokenType::MulAs, 1)
            || tokenProvider.isPeekIndexType(TokenType::DivAs, 1)
    }

    fn parse(
        &self,
        tokenProvider: &mut TokenProvider,
        _previous: Option<Operation>,
        parser: &[Box<dyn ParsingUnit>],
    ) -> Result<Operation, Box<dyn Error>> {
        let varName = tokenProvider.getIdentifier()?;
        let modType = match tokenProvider.getToken()?.typ {
            TokenType::AddAs => ModType::Add,
            TokenType::SubAs => ModType::Sub,
            TokenType::DivAs => ModType::Div,
            TokenType::MulAs => ModType::Mul,
            _ => panic!(),
        };

        let expr = parseExpr(tokenProvider, parser)?;

        Ok(Operation::Statement(Statement::VariableMod(VariableMod {
            varName,
            modType,
            expr,
        })))
    }

    fn getPriority(&self) -> usize {
        usize::MAX
    }

    fn setPriority(&mut self, _priority: usize) {}
}

struct CharParsingUnit;

impl ParsingUnit for CharParsingUnit {
    fn getType(&self) -> ParsingUnitSearchType {
        Ahead
    }

    fn canParse(&self, tokenProvider: &TokenProvider) -> bool {
        tokenProvider.isPeekType(CharLiteral)
    }

    fn parse(
        &self,
        tokenProvider: &mut TokenProvider,
        _previous: Option<Operation>,
        _parser: &[Box<dyn ParsingUnit>],
    ) -> Result<Operation, Box<dyn Error>> {
        let c = tokenProvider.getAssert(CharLiteral)?;
        let mut chars = c.str.chars();
        match &chars.next() {
            None => Err(Box::new(InvalidToken {
                msg: "char literal cannot be empty".to_string(),
            })),
            Some(c) => {
                if *c == '\\' {
                    match chars.next() {
                        None => panic!(),
                        Some(c) => {
                            let e = match c {
                                'n' => '\n',
                                'r' => '\r',
                                't' => '\t',
                                '\\' => '\\',
                                _ => panic!(),
                            };
                            return Ok(Operation::Expr(Expression::CharLiteral(e)));
                        }
                    }
                }
                Ok(Operation::Expr(Expression::CharLiteral(*c)))
            }
        }
    }

    fn getPriority(&self) -> usize {
        todo!()
    }

    fn setPriority(&mut self, _priority: usize) {
        todo!()
    }
}

struct StringParsingUnit;

impl ParsingUnit for StringParsingUnit {
    fn getType(&self) -> ParsingUnitSearchType {
        Ahead
    }

    fn canParse(&self, tokenProvider: &TokenProvider) -> bool {
        tokenProvider.isPeekType(TokenType::StringLiteral)
    }

    fn parse(
        &self,
        tokenProvider: &mut TokenProvider,
        _previous: Option<Operation>,
        _parser: &[Box<dyn ParsingUnit>],
    ) -> Result<Operation, Box<dyn Error>> {
        let str = tokenProvider.getAssert(StringLiteral)?;
        Ok(Operation::Expr(Expression::StringLiteral(str.str.clone())))
    }

    fn getPriority(&self) -> usize {
        usize::MAX
    }

    fn setPriority(&mut self, _priority: usize) {}
}

/*
struct NewParsingUnit;

impl ParsingUnit for NewParsingUnit {
    fn getType(&self) -> ParsingUnitSearchType {
        Ahead
    }

    fn canParse(&self, tokenProvider: &TokenProvider) -> bool {
        tokenProvider.isPeekType(New)
    }

    fn parse(&self, tokenProvider: &mut TokenProvider, previous: Option<Operation>, parser: &[Box<dyn ParsingUnit>]) -> Result<Operation, Box<dyn Error>> {
        tokenProvider.getAssert(New)?
        return Ok(None)
    }

    fn getPriority(&self) -> usize {
        todo!()
    }

    fn setPriority(&mut self, priority: usize) {
        todo!()
    }
}

 */

pub fn parseDataType(tokens: &mut TokenProvider) -> Result<DataType, Box<dyn Error>> {
    let t = tokens.getIdentifier()?;

    match t.as_str() {
        "bool" => return Ok(DataType::Bool),
        "char" => return Ok(DataType::Char),
        "int" => return Ok(DataType::Int),
        "float" => return Ok(DataType::Float),
        _ => {}
    }

    let mut generics = vec![];

    if tokens.isPeekType(TokenType::Gt) {
        tokens.getAssert(TokenType::Gt)?;
        while !tokens.isPeekType(TokenType::Less) {
            generics.push(Generic::Type(parseDataType(tokens)?));
        }
        tokens.getAssert(TokenType::Less)?;
    }
    Ok(DataType::Object(ObjectMeta {
        name: MyStr::Runtime(t.into_boxed_str()),
        generics: generics.into_boxed_slice(),
    }))
}

struct ArrayLiteralParsingUnit;

impl ParsingUnit for ArrayLiteralParsingUnit {
    fn getType(&self) -> ParsingUnitSearchType {
        Ahead
    }

    fn canParse(&self, tokenProvider: &TokenProvider) -> bool {
        tokenProvider.isPeekType(OSB)
    }

    fn parse(
        &self,
        tokenProvider: &mut TokenProvider,
        _previous: Option<Operation>,
        parser: &[Box<dyn ParsingUnit>],
    ) -> Result<Operation, Box<dyn Error>> {
        tokenProvider.getAssert(OSB)?;

        let mut buf = vec![];

        while !tokenProvider.isPeekType(CSB) {
            buf.push(parseExpr(tokenProvider, parser)?);
            if tokenProvider.isPeekType(Comma) {
                tokenProvider.getAssert(Comma)?;
            }
        }
        tokenProvider.getAssert(CSB)?;

        Ok(Operation::Expr(Expression::ArrayLiteral(buf)))
    }

    fn getPriority(&self) -> usize {
        usize::MAX
    }

    fn setPriority(&mut self, _priority: usize) {}
}

struct ArrayIndexingParsingUnit;

impl ParsingUnit for ArrayIndexingParsingUnit {
    fn getType(&self) -> ParsingUnitSearchType {
        Back
    }

    fn canParse(&self, tokenProvider: &TokenProvider) -> bool {
        tokenProvider.isPeekType(OSB)
    }

    fn parse(
        &self,
        tokenProvider: &mut TokenProvider,
        previous: Option<Operation>,
        parser: &[Box<dyn ParsingUnit>],
    ) -> Result<Operation, Box<dyn Error>> {
        tokenProvider.getAssert(OSB)?;
        let expr = parseExpr(tokenProvider, parser)?;
        tokenProvider.getAssert(CSB)?;

        Ok(Operation::Expr(Expression::ArrayIndexing(Box::new(
            ArrayAccess {
                expr: previous.ok_or("cannot index non existing item")?.asExpr()?,
                index: expr,
            },
        ))))
    }

    fn getPriority(&self) -> usize {
        usize::MAX
    }

    fn setPriority(&mut self, _priority: usize) {}
}

struct ArrayAssignParsingUnit;

impl ParsingUnit for ArrayAssignParsingUnit {
    fn getType(&self) -> ParsingUnitSearchType {
        Around
    }

    fn canParse(&self, tokenProvider: &TokenProvider) -> bool {
        tokenProvider.isPeekType(Equals)
    }

    fn parse(
        &self,
        tokenProvider: &mut TokenProvider,
        previous: Option<Operation>,
        parser: &[Box<dyn ParsingUnit>],
    ) -> Result<Operation, Box<dyn Error>> {
        tokenProvider.getAssert(Equals)?;
        let value = parseExpr(tokenProvider, parser)?;

        let arrayExpr = previous
            .ok_or("array asign must have expression")?
            .asExpr()?;
        let arrayAccess = match arrayExpr {
            Expression::ArrayIndexing(v) => Some(v),
            _ => None,
        }
            .ok_or("expected array indexing")?;

        Ok(Operation::Statement(Statement::ArrayAssign {
            left: *arrayAccess,
            right: value,
        }))
    }

    fn getPriority(&self) -> usize {
        usize::MAX
    }

    fn setPriority(&mut self, _priority: usize) {
        todo!()
    }
}

struct ContinueParsingUnit;

impl ParsingUnit for ContinueParsingUnit {
    fn getType(&self) -> ParsingUnitSearchType {
        Ahead
    }

    fn canParse(&self, tokenProvider: &TokenProvider) -> bool {
        tokenProvider.isPeekType(Continue)
    }

    fn parse(
        &self,
        tokenProvider: &mut TokenProvider,
        _previous: Option<Operation>,
        _parser: &[Box<dyn ParsingUnit>],
    ) -> Result<Operation, Box<dyn Error>> {
        tokenProvider.getAssert(Continue)?;
        Ok(Operation::Statement(Statement::Continue))
    }

    fn getPriority(&self) -> usize {
        todo!()
    }

    fn setPriority(&mut self, _priority: usize) {
        todo!()
    }
}

struct BreakParsingUnit;

impl ParsingUnit for BreakParsingUnit {
    fn getType(&self) -> ParsingUnitSearchType {
        Ahead
    }

    fn canParse(&self, tokenProvider: &TokenProvider) -> bool {
        tokenProvider.isPeekType(TokenType::Break)
    }

    fn parse(
        &self,
        tokenProvider: &mut TokenProvider,
        _previous: Option<Operation>,
        _parser: &[Box<dyn ParsingUnit>],
    ) -> Result<Operation, Box<dyn Error>> {
        tokenProvider.getAssert(TokenType::Break)?;
        Ok(Operation::Statement(Statement::Break))
    }

    fn getPriority(&self) -> usize {
        todo!()
    }

    fn setPriority(&mut self, _priority: usize) {
        todo!()
    }
}

struct LoopParsingUnit;

impl ParsingUnit for LoopParsingUnit {
    fn getType(&self) -> ParsingUnitSearchType {
        Ahead
    }

    fn canParse(&self, tokenProvider: &TokenProvider) -> bool {
        tokenProvider.isPeekType(Loop)
    }

    fn parse(
        &self,
        tokenProvider: &mut TokenProvider,
        _previous: Option<Operation>,
        parser: &[Box<dyn ParsingUnit>],
    ) -> Result<Operation, Box<dyn Error>> {
        tokenProvider.getAssert(Loop)?;
        let body = parseBody(tokenProvider, parser)?;
        Ok(Operation::Statement(Statement::Loop(body)))
    }

    fn getPriority(&self) -> usize {
        todo!()
    }

    fn setPriority(&mut self, _priority: usize) {
        todo!()
    }
}

struct NotParsingUnit;

impl ParsingUnit for NotParsingUnit {
    fn getType(&self) -> ParsingUnitSearchType {
        Ahead
    }

    fn canParse(&self, tokenProvider: &TokenProvider) -> bool {
        tokenProvider.isPeekType(Not)
    }

    fn parse(
        &self,
        tokenProvider: &mut TokenProvider,
        _previous: Option<Operation>,
        parser: &[Box<dyn ParsingUnit>],
    ) -> Result<Operation, Box<dyn Error>> {
        tokenProvider.getAssert(Not)?;

        let expr = parseExpr(tokenProvider, parser)?;

        Ok(Operation::Expr(Expression::NotExpression(Box::new(expr))))
    }

    fn getPriority(&self) -> usize {
        todo!()
    }

    fn setPriority(&mut self, _priority: usize) {
        todo!()
    }
}

struct StructParsingUnit;

impl ParsingUnit for StructParsingUnit {
    fn getType(&self) -> ParsingUnitSearchType {
        Ahead
    }

    fn canParse(&self, tokenProvider: &TokenProvider) -> bool {
        tokenProvider.isPeekType(Struct)
    }

    fn parse(
        &self,
        tokenProvider: &mut TokenProvider,
        _previous: Option<Operation>,
        _parser: &[Box<dyn ParsingUnit>],
    ) -> Result<Operation, Box<dyn Error>> {
        tokenProvider.getAssert(Struct)?;
        let name = tokenProvider.getIdentifier()?;

        let mut fields = HashMap::new();

        tokenProvider.getAssert(OCB)?;

        while !tokenProvider.isPeekType(CCB) {
            let fieldName = tokenProvider.getIdentifier()?;
            tokenProvider.getAssert(Colon)?;
            let fieldType = parseDataType(tokenProvider)?;

            if fields.contains_key(&fieldName) {
                None.ok_or("struct cant have duplicate fields")?;
            }

            fields.insert(fieldName, fieldType);
        }

        tokenProvider.getAssert(CCB)?;

        Ok(Operation::Global(Node::StructDef(StructDef {
            name,
            fields,
        })))
    }

    fn getPriority(&self) -> usize {
        todo!()
    }

    fn setPriority(&mut self, _priority: usize) {
        todo!()
    }
}

pub fn parsingUnits() -> Vec<Box<dyn ParsingUnit>> {
    vec![
        Box::new(VarModParsingUnit),
        Box::new(WhileParsingUnit),
        Box::new(LoopParsingUnit),
        Box::new(FunctionParsingUnit),
        Box::new(StatementVarParsingUnit),
        Box::new(NumericParsingUnit),
        Box::new(CharParsingUnit),
        Box::new(ArrayIndexingParsingUnit),
        Box::new(StringParsingUnit),
        Box::new(ArrayLiteralParsingUnit),
        Box::new(ArrayAssignParsingUnit),
        Box::new(CallParsingUnit),
        Box::new(BreakParsingUnit),
        Box::new(NotParsingUnit),
        Box::new(ContinueParsingUnit),
        Box::new(ArithmeticParsingUnit {
            op: Op::Mul,
            typ: TokenType::Mul,
            priority: 0,
        }),
        Box::new(ArithmeticParsingUnit {
            op: Op::Div,
            typ: TokenType::Div,
            priority: 1,
        }),
        Box::new(ArithmeticParsingUnit {
            op: Op::Add,
            typ: TokenType::Plus,
            priority: 2,
        }),
        Box::new(ArithmeticParsingUnit {
            op: Op::Sub,
            typ: TokenType::Minus,
            priority: 3,
        }),
        Box::new(ArithmeticParsingUnit {
            op: Op::Eq,
            typ: TokenType::Eq,
            priority: 4,
        }),
        Box::new(ArithmeticParsingUnit {
            op: Op::Less,
            typ: TokenType::Less,
            priority: 5,
        }),
        Box::new(ArithmeticParsingUnit {
            op: Op::Gt,
            typ: TokenType::Gt,
            priority: 6,
        }),
        Box::new(ArithmeticParsingUnit {
            op: Op::And,
            typ: TokenType::And,
            priority: 7,
        }),
        Box::new(ArithmeticParsingUnit {
            op: Op::Or,
            typ: TokenType::Or,
            priority: 8,
        }),
        Box::new(BracketsParsingUnit),
        Box::new(VariableParsingUnit),
        Box::new(IfParsingUnit),
        Box::new(BoolParsingUnit),
        Box::new(ReturnParsingUnit),
        Box::new(StructParsingUnit),
    ]
}
