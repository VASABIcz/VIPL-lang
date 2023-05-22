use core::fmt;
use std::collections::HashMap;
use std::error::Error;
use std::fmt::{Debug, Display, Formatter};
use std::intrinsics::fabsf32;
use std::usize;
use libc::read;

use crate::ast;
use crate::ast::{ArrayAccess, Expression, FunctionCall, ModType, Node, BinaryOp, Statement, StructDef, VariableCreate, VariableModd, While, ArithmeticOp};
use crate::ast::Expression::{IntLiteral, NamespaceAccess};
use crate::ast::BinaryOp::Add;
use crate::ast::Statement::{Assignable, StatementExpression};
use crate::bytecodeGen::Body;
use crate::errors::{InvalidCharLiteral, InvalidToken, NoSuchParsingUnit, ParserError};
use crate::lexer::{LexingUnit, Token, TokenType};
use crate::lexer::TokenType::*;
use crate::parser::ParsingUnitSearchType::{Ahead, Around, Back};
use crate::vm::variableMetadata::VariableMetadata;
use crate::vm::dataType::{DataType, Generic, ObjectMeta};
use crate::vm::myStr::MyStr;


const VALID_EXPRESSION_TOKENS: [TokenType; 5] = [
    StringLiteral,
    TokenType::IntLiteral,
    LongLiteral,
    Identifier,
    CharLiteral,
];

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

fn getParsingUnit<'a, OUT, IN: PartialEq + Clone + Debug>(
    tokens: &mut TokenProvider<IN>,
    typ: ParsingUnitSearchType,
    parsingUnits: &'a [Box<dyn ParsingUnit<OUT, IN>>],
    previous: Option<OUT>
) -> Option<&'a Box<dyn ParsingUnit<OUT, IN>>> {
    parsingUnits.iter().find(|it| {
        let parserType = it.getType();

        let canParse = match typ {
            ParsingUnitSearchType::Around => parserType != ParsingUnitSearchType::Ahead,
            ParsingUnitSearchType::Back => parserType == ParsingUnitSearchType::Back,
            ParsingUnitSearchType::Ahead => parserType == ParsingUnitSearchType::Ahead,
        };

        let p = match &previous {
            Some(v) => Some(v),
            None => None
        };

        canParse && it.canParse(tokens, p)
    })
}

fn parseExprOneLine(
    tokenProvider: &mut TokenProvider<TokenType>,
    parser: &[Box<dyn ParsingUnit<Operation, TokenType>>],
) -> Result<Expression, ParserError<TokenType>> {
    let res = parseOneOneLine(tokenProvider, Ahead, parser, None)?;
    res.asExpr()
}

pub fn parseOneOneLine<OUT: Clone + Debug, IN: Clone + Debug + Send + Sync + PartialEq + 'static + Copy> (
    tokens: &mut TokenProvider<IN>,
    typ: ParsingUnitSearchType,
    parsingUnits: &[Box<dyn ParsingUnit<OUT, IN>>],
    previous: Option<OUT>,
) -> Result<OUT, ParserError<IN>> {
    let row = tokens.peekOneRes()?.location.row;

    let u = getParsingUnit(tokens, typ.clone(), parsingUnits, previous.clone()).ok_or(ParserError::NoSuchParsingUnit(NoSuchParsingUnit{ typ, token: tokens.peekOne().cloned() }))?;

    println!("parsing {:?}", u);
    let mut first = u.parse(tokens, previous, parsingUnits)?;
    println!("parsed {:?}", first);

    while let Some(v) = getParsingUnit(tokens, ParsingUnitSearchType::Back, parsingUnits, Some(first.clone())) && tokens.peekOneRes()?.location.row == row {
        first = v.parse(tokens, Some(first), parsingUnits)?;
    }

    if let Some(v) = getParsingUnit(tokens, ParsingUnitSearchType::Around, parsingUnits, Some(first.clone())) && tokens.peekOneRes()?.location.row == row {
        first = v.parse(tokens, Some(first), parsingUnits)?;
    }

    Ok(first.clone())
}

pub fn parseOne<OUT: Clone + Debug, IN: Clone + Debug + Send + Sync + PartialEq + 'static + Copy> (
    tokens: &mut TokenProvider<IN>,
    typ: ParsingUnitSearchType,
    parsingUnits: &[Box<dyn ParsingUnit<OUT, IN>>],
    previous: Option<OUT>,
) -> Result<OUT, ParserError<IN>> {
    let u =
        getParsingUnit(tokens, typ.clone(), parsingUnits, previous.clone()).ok_or(ParserError::NoSuchParsingUnit(NoSuchParsingUnit{ typ, token: tokens.peekOne().cloned() }))?;

    println!("parsing {:?}", u);
    let mut first = u.parse(tokens, previous, parsingUnits)?;

    while let Some(v) = getParsingUnit(tokens, ParsingUnitSearchType::Back, parsingUnits, Some(first.clone())) {
        first = v.parse(tokens, Some(first), parsingUnits)?;
    }

    if let Some(v) = getParsingUnit(tokens, ParsingUnitSearchType::Around, parsingUnits, Some(first.clone())) {
        first = v.parse(tokens, Some(first), parsingUnits)?;
    }

    Ok(first.clone())
}

pub fn parse<OUT: Clone, IN: Clone + Debug + Sync + Send + PartialEq + 'static + Copy>(
    tokens: &mut TokenProvider<IN>,
    typ: ParsingUnitSearchType,
    parsingUnits: &[Box<dyn ParsingUnit<OUT, IN>>],
    previous: Option<OUT>,
    isPrevUser: &mut bool,
) -> Result<Vec<OUT>, ParserError<IN>> {
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

            if canParse && unit.canParse(tokens, buf.last()) {
                let res =
                    if !*isPrevUser && (parserType == Around || parserType == Back) && counter == 1
                    {
                        *isPrevUser = true;
                        unit.parse(tokens, previous.clone(), parsingUnits)?
                    } else if parserType == Around || parserType == Back {
                        unit.parse(tokens, opBuf.clone().take(), parsingUnits)?
                    } else {
                        unit.parse(tokens, None, parsingUnits)?
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

            if unit.canParse(tokens, buf.last()) {
                let res =
                    if !*isPrevUser && (parserType == Around || parserType == Back) && counter == 1
                    {
                        *isPrevUser = true;
                        unit.parse(tokens, previous.clone(), parsingUnits)?
                    } else if parserType == Around || parserType == Back {
                        unit.parse(tokens, opBuf.clone().take(), parsingUnits)?
                    } else {
                        unit.parse(tokens, None, parsingUnits)?
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
        return Err(ParserError::NoSuchParsingUnit(NoSuchParsingUnit{ typ, token: tokens.peekOne().cloned() }));
    }
    match opBuf {
        None => {}
        Some(v) => buf.push(v),
    }
    Ok(buf)
}

pub fn parseType<OUT, IN: PartialEq + Clone + Debug>(parsingUnits: &Vec<Box<dyn ParsingUnit<OUT, IN>>>, tokens: &mut TokenProvider<IN>, typ: ParsingUnitSearchType, buf: &mut Vec<OUT>) -> Result<bool, ParserError<IN>> {
    for unit in parsingUnits.iter() {
        let parserType = unit.getType();

        if parserType != typ {
            continue
        }

        if !unit.canParse(&tokens, buf.last()) {
            continue
        }

        match typ {
            Around | Back => {
                let res = match unit.parse(tokens, buf.pop(), parsingUnits) {
                    Ok(v) => v,
                    Err(e) => {
                        eprintln!("exception inside {:?}", unit);
                        return Err(e)
                    }
                };
                buf.push(res);
                return Ok(true)
            }
            Ahead => {
                let res = match unit.parse(tokens, None, parsingUnits) {
                    Ok(v) => v,
                    Err(e) => {
                        eprintln!("exception inside {:?}", unit);
                        return Err(e)
                    }
                };
                buf.push(res);
                return Ok(true)
            }
        }
    }
    Ok(false)
}

pub fn parseTokens(toks: Vec<Token<TokenType>>) -> Result<Vec<Operation>, ParserError<TokenType>> {
    let mut buf = vec![];
    let parsingUnits = &parsingUnits();
    let mut tokens = TokenProvider::new(toks);

    while !tokens.isDone() {
        if buf.is_empty() {
            if parseType(parsingUnits, &mut tokens, Ahead, &mut buf)? {
                continue
            }

            if parseType(parsingUnits, &mut tokens, Back, &mut buf)? {
                continue
            }

            if parseType(parsingUnits, &mut tokens, Around, &mut buf)? {
                continue
            }
        }
        else {
            if parseType(parsingUnits, &mut tokens, Back, &mut buf)? {
                continue
            }

            if parseType(parsingUnits, &mut tokens, Around, &mut buf)? {
                continue
            }
            if parseType(parsingUnits, &mut tokens, Ahead, &mut buf)? {
                continue
            }
        }

        return Err(ParserError::NoSuchParsingUnit(NoSuchParsingUnit{ typ: ParsingUnitSearchType::Ahead, token: tokens.peekOne().cloned() }));
    }
    Ok(buf)
}

pub struct TokenProvider<T: PartialEq + Clone> {
    pub tokens: Vec<Token<T>>,
    pub index: usize,
}

impl TokenProvider<TokenType> {
    pub fn getIdentifier(&mut self) -> Result<String, ParserError<TokenType>> {
        let t = self.getAssert(TokenType::Identifier)?;

        Ok(t.str.clone())
    }
}

impl<T: PartialEq + Debug + Clone + Copy + 'static> TokenProvider<T> {
    pub fn new(tokens: Vec<Token<T>>) -> TokenProvider<T> {
        Self { tokens, index: 0 }
    }

    fn peekOne(&self) -> Option<&Token<T>> {
        self.tokens.get(self.index)
    }

    pub fn peekOneRes(&self) -> Result<&Token<T>, ParserError<T>> {
        Ok(self.tokens.get(self.index).unwrap())
    }

    pub fn isPeekRow(&self, row: usize) -> bool {
        match self.peekOne() {
            None => false,
            Some(v) => {
                v.location.row == row
            }
        }
    }

    pub fn isPeekIndexRow(&self, index: usize, row: usize) -> bool {
        match self.peekIndex(index) {
            None => false,
            Some(v) => {
                v.location.row == row
            }
        }
    }

    fn peekIndex(&self, offset: usize) -> Option<&Token<T>> {
        self.tokens.get(self.index + offset)
    }

    fn consume(&mut self) {
        self.index += 1
    }

    pub fn isPeekTypeMany(&self, types: &[T]) -> bool {
        for (index, typ) in types.into_iter().enumerate() {
            if !self.isPeekIndexType(*typ, index) {
                return false
            }
        }

        return true
    }

    pub fn parseManyWithSeparatorUntil<F: core::ops::Fn(&mut TokenProvider<T>) -> Result<OUT, ParserError<T>>, OUT>(&mut self, mut f: F, sep: Option<T>, end: T) -> Result<Vec<OUT>, ParserError<T>> {
        let mut buf = vec![];

        if self.isPeekType(end) {
            self.getAssert(end)?;
            return Ok(vec![]);
        }

        match f(self) {
            Ok(v) => buf.push(v),
            Err(e) => {
                return Err(e)
            }
        }

        while sep.map(|it|{self.isPeekType(it)}).unwrap_or(!self.isPeekType(end)) {
            if let Some(v) = sep {
                self.getAssert(v)?;
            }
            match f(self) {
                Ok(v) => buf.push(v),
                Err(e) => {
                    return Err(e)
                }
            }
        }

        self.getAssert(end)?;

        Ok(buf)
    }

    pub fn getAssert(&mut self, typ: T) -> Result<&Token<T>, ParserError<T>> {
        let i = self.index;
        self.consume();
        let t = match self.tokens.get(i) {
            None => {
                return Err(ParserError::InvalidToken(InvalidToken{ expected: typ, actual: None }))
            }
            Some(v) => v,
        };
        if t.typ != typ {
            // panic!();
            return Err(ParserError::InvalidToken(InvalidToken{ expected: typ, actual: Some(t.clone()) }));
        }
        Ok(t)
    }

    pub fn isPeekType(&self, typ: T) -> bool {
        let t = self.peekOne();

        match t {
            None => false,
            Some(v) => v.typ == typ,
        }
    }

    pub fn isPeekTypeOf(&self, f: fn(T) -> bool) -> bool {
        let t = self.peekOne();

        match t {
            None => false,
            Some(v) => f(v.typ),
        }
    }

    pub fn isPeekIndexType(&self, typ: T, offset: usize) -> bool {
        let t = self.peekIndex(offset);

        match t {
            None => false,
            Some(v) => v.typ == typ,
        }
    }

    pub fn isDone(&self) -> bool {
        self.index >= self.tokens.len()
    }

    pub fn isPeekIndexOf(&self, f: fn(T) -> bool, offset: usize) -> bool {
        let t = self.peekIndex(offset);

        match t {
            None => false,
            Some(v) => f(v.typ),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Operation {
    Global(Node),
    Statement(Statement),
    Expr(Expression),
}

impl Operation {
    pub fn isExpr(&self) -> bool {
        match self {
            Operation::Expr(_) => true,
            _ => false
        }
    }

    pub fn isStatement(&self) -> bool {
        match self {
            Operation::Statement(_) => true,
            _ => false
        }
    }

    pub fn isGlobal(&self) -> bool {
        match self {
            Operation::Global(_) => true,
            _ => false
        }
    }

    pub fn asExpr(self) -> Result<Expression, ParserError<TokenType>> {
        match self {
            Operation::Expr(e) => Ok(e),
            _ => Err(ParserError::Unknown(Box::new(InvalidOperation {
                operation: self.clone(),
                expected: "Expression".to_string(),
            }))),
        }
    }

    fn asExprRef(&self) -> Result<&Expression, Box<dyn Error>> {
        match self {
            Operation::Expr(e) => Ok(e),
            _ => Err(Box::new(InvalidOperation {
                operation: self.clone(),
                expected: "Expression".to_string(),
            })),
        }
    }

    fn asStatement(self) -> Result<Statement, ParserError<TokenType>> {
        let clone = self.clone();
        match self {
            Operation::Statement(s) => Ok(s),
            Operation::Expr(e) => match e {
                Expression::NamespaceAccess(f) => todo!(),
                Expression::Callable(_, _) => { Ok(StatementExpression(e.clone())) },
                _ => {
                    Err(ParserError::Unknown(Box::new(InvalidOperation {
                        operation: clone,
                        expected: String::from("Statement"),
                    })))
                },
            },
            _ => Err(ParserError::Unknown(Box::new(InvalidOperation {
                operation: clone,
                expected: String::from("Statement"),
            }))),
        }
    }
}

#[derive(Eq, PartialEq, Debug, Clone)]
pub enum ParsingUnitSearchType {
    Around,
    Back,
    Ahead,
}


pub trait ParsingUnit<OUT, IN: PartialEq + Clone + Debug>: Debug {
    fn getType(&self) -> ParsingUnitSearchType;

    fn canParse(&self, tokenProvider: &TokenProvider<IN>, previous: Option<&OUT>) -> bool;

    fn parse(
        &self,
        tokenProvider: &mut TokenProvider<IN>,
        previous: Option<OUT>,
        parser: &[Box<dyn ParsingUnit<OUT, IN>>],
    ) -> Result<OUT, ParserError<IN>>;

    fn getPriority(&self) -> usize;

    fn setPriority(&mut self, priority: usize);
}

#[derive(Debug)]
pub struct FunctionParsingUnit;

impl ParsingUnit<Operation, TokenType> for FunctionParsingUnit {
    fn getType(&self) -> ParsingUnitSearchType {
        ParsingUnitSearchType::Ahead
    }

    fn canParse(&self, tokenProvider: &TokenProvider<TokenType>, previous: Option<&Operation>) -> bool {
        tokenProvider.isPeekType(TokenType::Fn) && tokenProvider.isPeekIndexType(Identifier, 1)
    }

    fn parse(
        &self,
        tokens: &mut TokenProvider<TokenType>,
        _previous: Option<Operation>,
        parser: &[Box<dyn ParsingUnit<Operation, TokenType>>],
    ) -> Result<Operation, ParserError<TokenType>> {
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

        let mut isOneLine = false;

        let body = if tokens.isPeekType(Equals) {
            tokens.getAssert(Equals)?;
            isOneLine = true;
            vec![ast::Statement::Return(parseExpr(tokens, parser)?)]
        }
        else {
            parseBody(tokens, parser)?
        };

        Ok(Operation::Global(Node::FunctionDef(
            ast::FunctionDef {
                name,
                localsMeta: args,
                argsCount: argCount,
                body: Body::new(body),
                returnType,
                isNative,
                isOneLine,
            },
        )))
    }

    fn getPriority(&self) -> usize {
        usize::MAX
    }

    fn setPriority(&mut self, _priority: usize) {}
}

#[derive(Debug)]
pub struct ArithmeticParsingUnit {
    pub op: BinaryOp,
    pub typ: TokenType,
    pub priority: usize,
}

impl ParsingUnit<Operation, TokenType> for ArithmeticParsingUnit {
    fn getType(&self) -> ParsingUnitSearchType {
        Around
    }

    fn canParse(&self, tokenProvider: &TokenProvider<TokenType>, previous: Option<&Operation>) -> bool {
        tokenProvider.isPeekType(self.typ)
    }

    fn parse(
        &self,
        tokens: &mut TokenProvider<TokenType>,
        previous: Option<Operation>,
        parser: &[Box<dyn ParsingUnit<Operation, TokenType>>],
    ) -> Result<Operation, ParserError<TokenType>> {
        tokens.consume();
        let res = parseOne(tokens, Ahead, parser, None)?;
        let par = getParsingUnit(tokens, Around, parser, previous.clone());

        match par {
            None => Ok(Operation::Expr(Expression::BinaryOperation {
                // FIXME
                left: Box::new(previous.unwrap().asExpr()?),
                right: Box::new(res.asExpr()?),
                op: self.op.clone(),
            })),
            Some(p) => {
                if self.priority < p.getPriority() {
                    Ok(p.parse(
                        tokens,
                        Some(Operation::Expr(Expression::BinaryOperation {
                            left: Box::new(previous.unwrap().asExpr()?),
                            right: Box::new(res.asExpr()?),
                            op: self.op.clone(),
                        })),
                        parser,
                    )?)
                } else {
                    Ok(Operation::Expr(Expression::BinaryOperation {
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

#[derive(Debug)]
pub struct NumericParsingUnit;

impl ParsingUnit<Operation, TokenType> for NumericParsingUnit {
    fn getType(&self) -> ParsingUnitSearchType {
        Ahead
    }

    fn canParse(&self, tokenProvider: &TokenProvider<TokenType>, previous: Option<&Operation>) -> bool {
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
        tokenProvider: &mut TokenProvider<TokenType>,
        _previous: Option<Operation>,
        _parser: &[Box<dyn ParsingUnit<Operation, TokenType>>],
    ) -> Result<Operation, ParserError<TokenType>> {
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
                return Err(ParserError::InvalidToken(InvalidToken{ expected: TokenType::IntLiteral, actual: Some(peek.clone()) }))
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

#[derive(Debug)]
pub struct BoolParsingUnit;

impl ParsingUnit<Operation, TokenType> for BoolParsingUnit {
    fn getType(&self) -> ParsingUnitSearchType {
        Ahead
    }

    fn canParse(&self, tokenProvider: &TokenProvider<TokenType>, previous: Option<&Operation>) -> bool {
        tokenProvider.isPeekType(TokenType::True) || tokenProvider.isPeekType(TokenType::False)
    }

    fn parse(
        &self,
        tokenProvider: &mut TokenProvider<TokenType>,
        _previous: Option<Operation>,
        _parser: &[Box<dyn ParsingUnit<Operation, TokenType>>],
    ) -> Result<Operation, ParserError<TokenType>> {
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

#[derive(Debug)]
pub struct VariableParsingUnit;

impl ParsingUnit<Operation, TokenType> for VariableParsingUnit {
    fn getType(&self) -> ParsingUnitSearchType {
        Ahead
    }

    fn canParse(&self, tokenProvider: &TokenProvider<TokenType>, previous: Option<&Operation>) -> bool {
        tokenProvider.isPeekType(Identifier)
    }

    fn parse(
        &self,
        tokenProvider: &mut TokenProvider<TokenType>,
        _previous: Option<Operation>,
        _parser: &[Box<dyn ParsingUnit<Operation, TokenType>>],
    ) -> Result<Operation, ParserError<TokenType>> {
        Ok(Operation::Expr(Expression::Variable(
            tokenProvider.getIdentifier()?,
        )))
    }

    fn getPriority(&self) -> usize {
        usize::MAX
    }

    fn setPriority(&mut self, _priority: usize) {}
}

#[derive(Debug)]
pub struct ReturnParsingUnit;

impl ParsingUnit<Operation, TokenType> for ReturnParsingUnit {
    fn getType(&self) -> ParsingUnitSearchType {
        Ahead
    }

    fn canParse(&self, tokenProvider: &TokenProvider<TokenType>, previous: Option<&Operation>) -> bool {
        tokenProvider.isPeekType(Return)
    }

    fn parse(
        &self,
        tokenProvider: &mut TokenProvider<TokenType>,
        _previous: Option<Operation>,
        parser: &[Box<dyn ParsingUnit<Operation, TokenType>>],
    ) -> Result<Operation, ParserError<TokenType>> {
        tokenProvider.getAssert(Return)?;
        let exp = parseExpr(tokenProvider, parser)?;
        Ok(Operation::Statement(Statement::Return(exp)))
    }

    fn getPriority(&self) -> usize {
        usize::MAX
    }

    fn setPriority(&mut self, _priority: usize) {}
}

#[derive(Debug)]
pub struct WhileParsingUnit;

impl ParsingUnit<Operation, TokenType> for WhileParsingUnit {
    fn getType(&self) -> ParsingUnitSearchType {
        Ahead
    }

    fn canParse(&self, tokenProvider: &TokenProvider<TokenType>, previous: Option<&Operation>) -> bool {
        tokenProvider.isPeekType(TokenType::While)
    }

    fn parse(
        &self,
        tokenProvider: &mut TokenProvider<TokenType>,
        _previous: Option<Operation>,
        parser: &[Box<dyn ParsingUnit<Operation, TokenType>>],
    ) -> Result<Operation, ParserError<TokenType>> {
        tokenProvider.getAssert(TokenType::While)?;

        let op = parseExpr(tokenProvider, parser)?;

        let statements = parseBody(tokenProvider, parser)?;

        Ok(Operation::Statement(Statement::While(While {
            exp: op,
            body: Body::new(statements),
        })))
    }

    fn getPriority(&self) -> usize {
        usize::MAX
    }

    fn setPriority(&mut self, _priority: usize) {}
}

#[derive(Debug)]
pub struct IfParsingUnit;

fn parseBody(
    tokenProvider: &mut TokenProvider<TokenType>,
    parser: &[Box<dyn ParsingUnit<Operation, TokenType>>],
) -> Result<Vec<Statement>, ParserError<TokenType>> {
    let mut statements = vec![];

    tokenProvider.getAssert(TokenType::OCB)?;


    while !tokenProvider.isPeekType(CCB) {
        statements.push(parseOne(tokenProvider, Ahead, parser, None)?.asStatement()?);
    }

    tokenProvider.getAssert(TokenType::CCB)?;

    Ok(statements)
}

fn parseExpr(
    tokenProvider: &mut TokenProvider<TokenType>,
    parser: &[Box<dyn ParsingUnit<Operation, TokenType>>],
) -> Result<Expression, ParserError<TokenType>> {
    let res = parseOne(tokenProvider, Ahead, parser, None)?;
    res.asExpr()
}

impl ParsingUnit<Operation, TokenType> for IfParsingUnit {
    fn getType(&self) -> ParsingUnitSearchType {
        Ahead
    }

    fn canParse(&self, tokenProvider: &TokenProvider<TokenType>, previous: Option<&Operation>) -> bool {
        tokenProvider.isPeekType(TokenType::If)
    }

    fn parse(
        &self,
        tokenProvider: &mut TokenProvider<TokenType>,
        _previous: Option<Operation>,
        parser: &[Box<dyn ParsingUnit<Operation, TokenType>>],
    ) -> Result<Operation, ParserError<TokenType>> {
        let mut elseIfs = vec![];
        let mut elseBody = None;

        tokenProvider.getAssert(If)?;


        let condition = parseExpr(tokenProvider, parser)?;

        let body = parseBody(tokenProvider, parser)?;

        while tokenProvider.isPeekType(Else) && tokenProvider.isPeekIndexType(If, 1) {
            tokenProvider.getAssert(Else)?;
            tokenProvider.getAssert(If)?;

            let cond = parseExpr(tokenProvider, parser)?;
            let statements = parseBody(tokenProvider, parser)?;

            elseIfs.push((cond, statements))
        }

        if tokenProvider.isPeekType(Else) {
            tokenProvider.getAssert(Else)?;
            elseBody = Some(parseBody(tokenProvider, parser)?);
        }

        Ok(Operation::Statement(Statement::If(ast::If {
            condition,
            body: Body::new(body),
            elseBody,
            elseIfs,
        })))
    }

    fn getPriority(&self) -> usize {
        usize::MAX
    }

    fn setPriority(&mut self, _priority: usize) {}
}

#[derive(Debug)]
struct BracketsParsingUnit;

impl ParsingUnit<Operation, TokenType> for BracketsParsingUnit {
    fn getType(&self) -> ParsingUnitSearchType {
        Ahead
    }

    fn canParse(&self, tokenProvider: &TokenProvider<TokenType>, previous: Option<&Operation>) -> bool {
        tokenProvider.isPeekType(ORB)
    }

    fn parse(
        &self,
        tokenProvider: &mut TokenProvider<TokenType>,
        _previous: Option<Operation>,
        parser: &[Box<dyn ParsingUnit<Operation, TokenType>>],
    ) -> Result<Operation, ParserError<TokenType>> {
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

#[derive(Debug)]
struct CharParsingUnit;

impl ParsingUnit<Operation, TokenType> for CharParsingUnit {
    fn getType(&self) -> ParsingUnitSearchType {
        Ahead
    }

    fn canParse(&self, tokenProvider: &TokenProvider<TokenType>, previous: Option<&Operation>) -> bool {
        tokenProvider.isPeekType(CharLiteral)
    }

    fn parse(
        &self,
        tokenProvider: &mut TokenProvider<TokenType>,
        _previous: Option<Operation>,
        _parser: &[Box<dyn ParsingUnit<Operation, TokenType>>],
    ) -> Result<Operation, ParserError<TokenType>> {
        let c = tokenProvider.getAssert(CharLiteral)?;
        let mut chars = c.str.chars();
        match &chars.next() {
            None => Err(ParserError::InvalidCharLiteral(InvalidCharLiteral{ token: c.clone() })),
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

#[derive(Debug)]
struct StringParsingUnit;

impl ParsingUnit<Operation, TokenType> for StringParsingUnit {
    fn getType(&self) -> ParsingUnitSearchType {
        Ahead
    }

    fn canParse(&self, tokenProvider: &TokenProvider<TokenType>, previous: Option<&Operation>) -> bool {
        tokenProvider.isPeekType(TokenType::StringLiteral)
    }

    fn parse(
        &self,
        tokenProvider: &mut TokenProvider<TokenType>,
        _previous: Option<Operation>,
        _parser: &[Box<dyn ParsingUnit<Operation, TokenType>>],
    ) -> Result<Operation, ParserError<TokenType>> {
        let str = tokenProvider.getAssert(StringLiteral)?;
        Ok(Operation::Expr(Expression::StringLiteral(str.str.clone())))
    }

    fn getPriority(&self) -> usize {
        usize::MAX
    }

    fn setPriority(&mut self, _priority: usize) {}
}

pub fn parseDataType(tokens: &mut TokenProvider<TokenType>) -> Result<DataType, ParserError<TokenType>> {
    if tokens.isPeekType(Identifier) && tokens.isPeekIndexType(Gt, 1) {
        let mut generics = vec![];

        let t = tokens.getIdentifier()?;
        tokens.getAssert(TokenType::Gt)?;

        while !tokens.isPeekType(TokenType::Less) {
            generics.push(Generic::Type(parseDataType(tokens)?));
        }
        tokens.getAssert(TokenType::Less)?;

        Ok(DataType::Object(ObjectMeta {
            name: MyStr::Runtime(t.into_boxed_str()),
            generics: generics.into_boxed_slice(),
        }))
    }
    else if tokens.isPeekType(TokenType::ORB) {
        tokens.getAssert(ORB)?;
        let args = tokens.parseManyWithSeparatorUntil(|it| {
            parseDataType(it)
        }, Some(Comma), CRB)?;
        let ret = if tokens.isPeekType(Colon) {
            tokens.getAssert(Colon)?;
            parseDataType(tokens)?
        }
        else {
            DataType::Void
        };
        Ok(DataType::Function { args, ret: Box::new(ret) })
    }
    else if tokens.isPeekType(Not) {
        tokens.getAssert(Not)?;
        Ok(DataType::Void)
    }
    else {
        let t = tokens.getIdentifier()?;

        match t.as_str() {
            "bool" => return Ok(DataType::Bool),
            "char" => return Ok(DataType::Char),
            "int" => return Ok(DataType::Int),
            "float" => return Ok(DataType::Float),
            c => return Ok(DataType::Object(ObjectMeta { name: c.to_string().into(), generics: Box::new([]) }))
        }
    }
}

#[derive(Debug)]
struct ArrayLiteralParsingUnit;

impl ParsingUnit<Operation, TokenType> for ArrayLiteralParsingUnit {
    fn getType(&self) -> ParsingUnitSearchType {
        Ahead
    }

    fn canParse(&self, tokenProvider: &TokenProvider<TokenType>, previous: Option<&Operation>) -> bool {
        tokenProvider.isPeekType(OSB)
    }

    fn parse(
        &self,
        tokenProvider: &mut TokenProvider<TokenType>,
        _previous: Option<Operation>,
        parser: &[Box<dyn ParsingUnit<Operation, TokenType>>],
    ) -> Result<Operation, ParserError<TokenType>> {
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

#[derive(Debug)]
struct ArrayIndexingParsingUnit;

impl ParsingUnit<Operation, TokenType> for ArrayIndexingParsingUnit {
    fn getType(&self) -> ParsingUnitSearchType {
        Back
    }

    fn canParse(&self, tokenProvider: &TokenProvider<TokenType>, previous: Option<&Operation>) -> bool {
        tokenProvider.isPeekType(OSB)
    }

    fn parse(
        &self,
        tokenProvider: &mut TokenProvider<TokenType>,
        previous: Option<Operation>,
        parser: &[Box<dyn ParsingUnit<Operation, TokenType>>],
    ) -> Result<Operation, ParserError<TokenType>> {
        tokenProvider.getAssert(OSB)?;
        let expr = parseExpr(tokenProvider, parser)?;
        tokenProvider.getAssert(CSB)?;

        Ok(Operation::Expr(Expression::ArrayIndexing(Box::new(
            ArrayAccess {
                // FIXME
                expr: previous.unwrap().asExpr()?,
                index: expr,
            },
        ))))
    }

    fn getPriority(&self) -> usize {
        usize::MAX
    }

    fn setPriority(&mut self, _priority: usize) {}
}

#[derive(Debug)]
struct ContinueParsingUnit;

impl ParsingUnit<Operation, TokenType> for ContinueParsingUnit {
    fn getType(&self) -> ParsingUnitSearchType {
        Ahead
    }

    fn canParse(&self, tokenProvider: &TokenProvider<TokenType>, previous: Option<&Operation>) -> bool {
        tokenProvider.isPeekType(Continue)
    }

    fn parse(
        &self,
        tokenProvider: &mut TokenProvider<TokenType>,
        _previous: Option<Operation>,
        _parser: &[Box<dyn ParsingUnit<Operation, TokenType>>],
    ) -> Result<Operation, ParserError<TokenType>> {
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

#[derive(Debug)]
struct BreakParsingUnit;

impl ParsingUnit<Operation, TokenType> for BreakParsingUnit {
    fn getType(&self) -> ParsingUnitSearchType {
        Ahead
    }

    fn canParse(&self, tokenProvider: &TokenProvider<TokenType>, previous: Option<&Operation>) -> bool {
        tokenProvider.isPeekType(TokenType::Break)
    }

    fn parse(
        &self,
        tokenProvider: &mut TokenProvider<TokenType>,
        _previous: Option<Operation>,
        _parser: &[Box<dyn ParsingUnit<Operation, TokenType>>],
    ) -> Result<Operation, ParserError<TokenType>> {
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

#[derive(Debug)]
struct LoopParsingUnit;

impl ParsingUnit<Operation, TokenType> for LoopParsingUnit {
    fn getType(&self) -> ParsingUnitSearchType {
        Ahead
    }

    fn canParse(&self, tokenProvider: &TokenProvider<TokenType>, previous: Option<&Operation>) -> bool {
        tokenProvider.isPeekType(Loop)
    }

    fn parse(
        &self,
        tokenProvider: &mut TokenProvider<TokenType>,
        _previous: Option<Operation>,
        parser: &[Box<dyn ParsingUnit<Operation, TokenType>>],
    ) -> Result<Operation, ParserError<TokenType>> {
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

#[derive(Debug)]
struct NotParsingUnit;

impl ParsingUnit<Operation, TokenType> for NotParsingUnit {
    fn getType(&self) -> ParsingUnitSearchType {
        Ahead
    }

    fn canParse(&self, tokenProvider: &TokenProvider<TokenType>, previous: Option<&Operation>) -> bool {
        tokenProvider.isPeekType(Not)
    }

    fn parse(
        &self,
        tokenProvider: &mut TokenProvider<TokenType>,
        _previous: Option<Operation>,
        parser: &[Box<dyn ParsingUnit<Operation, TokenType>>],
    ) -> Result<Operation, ParserError<TokenType>> {
        let t = tokenProvider.getAssert(Not)?.location;

        let expr = parseExpr(tokenProvider, parser)?;

        Ok(Operation::Expr(Expression::NotExpression(Box::new(expr),t)))
    }

    fn getPriority(&self) -> usize {
        todo!()
    }

    fn setPriority(&mut self, _priority: usize) {
        todo!()
    }
}

#[derive(Debug)]
struct StructParsingUnit;

impl ParsingUnit<Operation, TokenType> for StructParsingUnit {
    fn getType(&self) -> ParsingUnitSearchType {
        Ahead
    }

    fn canParse(&self, tokenProvider: &TokenProvider<TokenType>, previous: Option<&Operation>) -> bool {
        tokenProvider.isPeekType(Struct)
    }

    fn parse(
        &self,
        tokenProvider: &mut TokenProvider<TokenType>,
        _previous: Option<Operation>,
        _parser: &[Box<dyn ParsingUnit<Operation, TokenType>>],
    ) -> Result<Operation, ParserError<TokenType>> {
        tokenProvider.getAssert(Struct)?;
        let name = tokenProvider.getIdentifier()?;

        let mut fields = HashMap::new();

        tokenProvider.getAssert(OCB)?;

        while !tokenProvider.isPeekType(CCB) {
            let fieldName = tokenProvider.getIdentifier()?;
            tokenProvider.getAssert(Colon)?;
            let fieldType = parseDataType(tokenProvider)?;

            if fields.contains_key(&fieldName) {
                // FIXME
                panic!()
                // None.ok_or("struct cant have duplicate fields")?;
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

#[derive(Debug)]
struct ImportParsingUnit;

impl ParsingUnit<Operation, TokenType> for ImportParsingUnit {
    fn getType(&self) -> ParsingUnitSearchType {
        Ahead
    }

    fn canParse(&self, tokenProvider: &TokenProvider<TokenType>, previous: Option<&Operation>) -> bool {
        tokenProvider.isPeekType(Import)
    }

    fn parse(&self, tokenProvider: &mut TokenProvider<TokenType>, previous: Option<Operation>, parser: &[Box<dyn ParsingUnit<Operation, TokenType>>]) -> Result<Operation, ParserError<TokenType>> {
        tokenProvider.getAssert(Import)?;
        let mut buf = vec![];

        let f = tokenProvider.getIdentifier()?;
        buf.push(f);

        while tokenProvider.isPeekType(Namespace) {
            tokenProvider.getAssert(Namespace)?;
            buf.push(tokenProvider.getIdentifier()?);
        }

        Ok(Operation::Global(Node::Import(buf)))
    }

    fn getPriority(&self) -> usize { todo!() }

    fn setPriority(&mut self, priority: usize) { todo!() }
}

#[derive(Debug)]
struct NamespaceParsingUnit;

impl ParsingUnit<Operation, TokenType> for NamespaceParsingUnit {
    fn getType(&self) -> ParsingUnitSearchType {
        Ahead
    }

    fn canParse(&self, tokenProvider: &TokenProvider<TokenType>, previous: Option<&Operation>) -> bool {
        tokenProvider.isPeekType(Identifier) && tokenProvider.isPeekIndexType(Namespace, 1)
    }

    fn parse(&self, tokenProvider: &mut TokenProvider<TokenType>, previous: Option<Operation>, parser: &[Box<dyn ParsingUnit<Operation, TokenType>>]) -> Result<Operation, ParserError<TokenType>> {
        let mut buf = vec![];

        while tokenProvider.isPeekType(Identifier) {
            let i = tokenProvider.getIdentifier()?;
            buf.push(i);
            if tokenProvider.isPeekType(Namespace) {
                tokenProvider.getAssert(Namespace)?;
            }
            else {
                break
            }
        }

        Ok(Operation::Expr(NamespaceAccess(buf)))
    }

    fn getPriority(&self) -> usize {
        todo!()
    }

    fn setPriority(&mut self, priority: usize) {
        todo!()
    }
}

#[derive(Debug)]
struct CallableParsingUnit;

impl ParsingUnit<Operation, TokenType> for CallableParsingUnit {
    fn getType(&self) -> ParsingUnitSearchType { Around }

    fn canParse(&self, tokenProvider: &TokenProvider<TokenType>, previous: Option<&Operation>) -> bool {
        tokenProvider.isPeekType(ORB) && previous.map_or(false, |it| it.asExprRef().map_or(false, |it| { it.isCallable() }))
    }

    fn parse(&self, tokens: &mut TokenProvider<TokenType>, previous: Option<Operation>, parser: &[Box<dyn ParsingUnit<Operation, TokenType>>]) -> Result<Operation, ParserError<TokenType>> {
        tokens.getAssert(ORB)?;

        let mut args = vec![];

        while !tokens.isPeekType(TokenType::CRB) {
            let res = parseOne(tokens, Ahead, parser, None)?;
            let par = getParsingUnit(tokens, Around, parser, None);

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

        // FIXME
        Ok(Operation::Expr(Expression::Callable(Box::new(previous.unwrap().asExpr()?), args)))
    }

    fn getPriority(&self) -> usize {
        todo!()
    }

    fn setPriority(&mut self, priority: usize) {
        todo!()
    }
}

#[derive(Debug)]
struct LambdaParsingUnit;

impl ParsingUnit<Operation, TokenType> for LambdaParsingUnit {
    fn getType(&self) -> ParsingUnitSearchType {
        Ahead
    }

    fn canParse(&self, tokenProvider: &TokenProvider<TokenType>, previous: Option<&Operation>) -> bool {
        tokenProvider.isPeekType(Fn) && tokenProvider.isPeekIndexType(ORB, 1)
    }

    fn parse(&self, tokens: &mut TokenProvider<TokenType>, previous: Option<Operation>, parser: &[Box<dyn ParsingUnit<Operation, TokenType>>]) -> Result<Operation, ParserError<TokenType>> {
        tokens.getAssert(Fn)?;

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

        let mut isOneLine = false;

        let body = if tokens.isPeekType(Equals) {
            tokens.getAssert(Equals)?;
            isOneLine = true;
            vec![ast::Statement::Return(parseExpr(tokens, parser)?)]
        }
        else {
            parseBody(tokens, parser)?
        };

        Ok(Operation::Expr(Expression::Lambda(args, Body::new(body), returnType)))
    }

    fn getPriority(&self) -> usize {
        todo!()
    }

    fn setPriority(&mut self, priority: usize) {
        todo!()
    }
}

#[derive(Debug)]
struct IncParsingUnit;

impl ParsingUnit<Operation, TokenType> for IncParsingUnit {
    fn getType(&self) -> ParsingUnitSearchType {Back}

    fn canParse(&self, tokenProvider: &TokenProvider<TokenType>, previous: Option<&Operation>) -> bool {
        tokenProvider.isPeekType(TokenType::Inc) || tokenProvider.isPeekType(TokenType::Dec)
    }

    fn parse(&self, tokenProvider: &mut TokenProvider<TokenType>, previous: Option<Operation>, parser: &[Box<dyn ParsingUnit<Operation, TokenType>>]) -> Result<Operation, ParserError<TokenType>> {
        // FIXME
        let prev = previous.unwrap().asExpr()?;

        if tokenProvider.isPeekType(TokenType::Inc) {
            tokenProvider.getAssert(TokenType::Inc)?;

            Ok(Operation::Statement(Assignable(prev, Expression::IntLiteral("1".to_string()), Some(ArithmeticOp::Add))))
        }
        else {
            tokenProvider.getAssert(TokenType::Dec)?;

            Ok(Operation::Statement(Assignable(prev, Expression::IntLiteral("1".to_string()), Some(ArithmeticOp::Sub))))
        }
    }

    fn getPriority(&self) -> usize {
        todo!()
    }

    fn setPriority(&mut self, priority: usize) {
        todo!()
    }
}

#[derive(Debug)]
struct StructInitParsingUnit;

impl ParsingUnit<Operation, TokenType> for StructInitParsingUnit {
    fn getType(&self) -> ParsingUnitSearchType {
        Ahead
    }

    fn canParse(&self, tokenProvider: &TokenProvider<TokenType>, previous: Option<&Operation>) -> bool {
        tokenProvider.isPeekType(Identifier) && tokenProvider.isPeekIndexType(OCB, 1)
    }

    fn parse(&self, tokenProvider: &mut TokenProvider<TokenType>, previous: Option<Operation>, parser: &[Box<dyn ParsingUnit<Operation, TokenType>>]) -> Result<Operation, ParserError<TokenType>> {
        let name = tokenProvider.getIdentifier()?;

        tokenProvider.getAssert(OCB)?;

        let inits = tokenProvider.parseManyWithSeparatorUntil(|it| {
            let fieldName = it.getIdentifier()?;
            it.getAssert(Colon)?;
            let initializer = parseOne(it, Ahead, parser, None)?.asExpr()?;

            Ok((fieldName, initializer))
        }, None, CCB)?;

        Ok(Operation::Expr(Expression::StructInit(name, inits)))
    }

    fn getPriority(&self) -> usize {
        todo!()
    }

    fn setPriority(&mut self, priority: usize) {
        todo!()
    }
}

#[derive(Debug)]
struct FieldAccessParsingUnit;

impl ParsingUnit<Operation, TokenType> for FieldAccessParsingUnit {
    fn getType(&self) -> ParsingUnitSearchType {
        Back
    }

    fn canParse(&self, tokenProvider: &TokenProvider<TokenType>, previous: Option<&Operation>) -> bool {
        tokenProvider.isPeekType(Dot)
    }

    fn parse(&self, tokenProvider: &mut TokenProvider<TokenType>, previous: Option<Operation>, parser: &[Box<dyn ParsingUnit<Operation, TokenType>>]) -> Result<Operation, ParserError<TokenType>> {
        tokenProvider.getAssert(Dot)?;
        let fieldName = tokenProvider.getIdentifier()?;

        Ok(Operation::Expr(Expression::FieldAccess(Box::new(previous.unwrap().asExpr()?), fieldName)))
    }

    fn getPriority(&self) -> usize {
        todo!()
    }

    fn setPriority(&mut self, priority: usize) {
        todo!()
    }
}

#[derive(Debug)]
struct AssignableParsingUnit;

impl ParsingUnit<Operation, TokenType> for AssignableParsingUnit {
    fn getType(&self) -> ParsingUnitSearchType {
        Around
    }

    fn canParse(&self, tokenProvider: &TokenProvider<TokenType>, previous: Option<&Operation>) -> bool {
        tokenProvider.isPeekType(Equals)
            || tokenProvider.isPeekType(AddAs)
            || tokenProvider.isPeekType(SubAs)
            || tokenProvider.isPeekType(DivAs)
            || tokenProvider.isPeekType(MulAs)
        && previous.map(|it| {
            it.asExprRef().map(|it| {
                it.isAssignable()
            }).unwrap_or(false)
        }).unwrap_or(false)
    }

    fn parse(&self, tokenProvider: &mut TokenProvider<TokenType>, previous: Option<Operation>, parser: &[Box<dyn ParsingUnit<Operation, TokenType>>]) -> Result<Operation, ParserError<TokenType>> {
        let mut typ = None;

        if tokenProvider.isPeekType(AddAs) {
            typ = Some(ArithmeticOp::Add);
        }
        else if tokenProvider.isPeekType(SubAs) {
            typ = Some(ArithmeticOp::Sub);
        }
        else if tokenProvider.isPeekType(MulAs) {
            typ = Some(ArithmeticOp::Mul);
        }
        else if tokenProvider.isPeekType(DivAs) {
            typ = Some(ArithmeticOp::Div);
        }
        tokenProvider.consume();

        let next = parseOne(tokenProvider, Ahead, parser, None)?.asExpr()?;

        // FIXME
        Ok(Operation::Statement(Statement::Assignable(previous.unwrap().asExpr()?, next, typ)))
    }

    fn getPriority(&self) -> usize {
        todo!()
    }

    fn setPriority(&mut self, priority: usize) {
        todo!()
    }
}

#[derive(Debug)]
struct GlobalParsingUnit;

impl ParsingUnit<Operation, TokenType> for GlobalParsingUnit {
    fn getType(&self) -> ParsingUnitSearchType {
        Ahead
    }

    fn canParse(&self, tokenProvider: &TokenProvider<TokenType>, previous: Option<&Operation>) -> bool {
        tokenProvider.isPeekType(TokenType::Global)
    }

    fn parse(&self, tokenProvider: &mut TokenProvider<TokenType>, previous: Option<Operation>, parser: &[Box<dyn ParsingUnit<Operation, TokenType>>]) -> Result<Operation, ParserError<TokenType>> {
        tokenProvider.getAssert(Global)?;

        let name = tokenProvider.getIdentifier()?;
        let mut typeHint = None;

        if tokenProvider.isPeekType(Colon) {
            tokenProvider.getAssert(Colon)?;
            typeHint = Some(parseDataType(tokenProvider)?);
        }

        tokenProvider.getAssert(TokenType::Equals)?;

        let res = parseOne(tokenProvider, Ahead, parser, None)?;
        let par = getParsingUnit(tokenProvider, Around, parser, None);

        let op = match par {
            None => res.asExpr()?,
            Some(p) => p.parse(tokenProvider, Some(res), parser)?.asExpr()?,
        };

        Ok(Operation::Global(Node::GlobalVarDef(name, op)))
    }

    fn getPriority(&self) -> usize {
        todo!()
    }

    fn setPriority(&mut self, priority: usize) {
        todo!()
    }
}

#[derive(Debug)]
struct ForParsingUnit;

impl ParsingUnit<Operation, TokenType> for ForParsingUnit {
    fn getType(&self) -> ParsingUnitSearchType {
        Ahead
    }

    fn canParse(&self, tokenProvider: &TokenProvider<TokenType>, previous: Option<&Operation>) -> bool {
        tokenProvider.isPeekType(For)
    }

    fn parse(&self, tokenProvider: &mut TokenProvider<TokenType>, previous: Option<Operation>, parser: &[Box<dyn ParsingUnit<Operation, TokenType>>]) -> Result<Operation, ParserError<TokenType>> {
        tokenProvider.getAssert(For)?;

        let varName = tokenProvider.getIdentifier()?;

        tokenProvider.getAssert(In)?;

        let res = parseOne(tokenProvider, Ahead, parser, previous)?.asExpr()?;

        let body = parseBody(tokenProvider, parser)?;

        Ok(Operation::Statement(Statement::ForLoop(varName, res, body)))
    }

    fn getPriority(&self) -> usize {
        todo!()
    }

    fn setPriority(&mut self, priority: usize) {
        todo!()
    }
}

#[derive(Debug)]
struct NullParsingUnit;

impl ParsingUnit<Operation, TokenType> for NullParsingUnit {
    fn getType(&self) -> ParsingUnitSearchType {
        Ahead
    }

    fn canParse(&self, tokenProvider: &TokenProvider<TokenType>, previous: Option<&Operation>) -> bool {
        tokenProvider.isPeekType(Null)
    }

    fn parse(&self, tokenProvider: &mut TokenProvider<TokenType>, previous: Option<Operation>, parser: &[Box<dyn ParsingUnit<Operation, TokenType>>]) -> Result<Operation, ParserError<TokenType>> {
        tokenProvider.getAssert(Null)?;

        Ok(Operation::Expr(Expression::Null))
    }

    fn getPriority(&self) -> usize {
        todo!()
    }

    fn setPriority(&mut self, priority: usize) {
        todo!()
    }
}

#[derive(Debug)]
struct OneArgFunctionParsintUnit;

impl ParsingUnit<Operation, TokenType> for OneArgFunctionParsintUnit {
    fn getType(&self) -> ParsingUnitSearchType {
        Ahead
    }

    fn canParse(&self, tokenProvider: &TokenProvider<TokenType>, previous: Option<&Operation>) -> bool {
        let row = match tokenProvider.peekOne() {
            None => {
                return false
            }
            Some(v) => v.location.row
        };

        tokenProvider.isPeekType(Identifier) && tokenProvider.isPeekIndexOf(|it| VALID_EXPRESSION_TOKENS.contains(&it), 1) && tokenProvider.isPeekIndexRow(1, row)
    }

    fn parse(&self, tokenProvider: &mut TokenProvider<TokenType>, previous: Option<Operation>, parser: &[Box<dyn ParsingUnit<Operation, TokenType>>]) -> Result<Operation, ParserError<TokenType>> {
        let name = tokenProvider.getIdentifier()?;
        let arg = parseExprOneLine(tokenProvider, parser)?;

        return Ok(Operation::Expr(Expression::Callable(Box::new(Expression::Variable(name)), vec![arg])))
    }

    fn getPriority(&self) -> usize {
        todo!()
    }

    fn setPriority(&mut self, priority: usize) {
        todo!()
    }
}

#[derive(Debug)]
struct TwoArgFunctionParsintUnit;

impl ParsingUnit<Operation, TokenType> for TwoArgFunctionParsintUnit {
    fn getType(&self) -> ParsingUnitSearchType {
        Around
    }

    fn canParse(&self, tokenProvider: &TokenProvider<TokenType>, previous: Option<&Operation>) -> bool {
        previous.map_or(false, |it| it.isExpr()) && tokenProvider.isPeekType(Identifier) && tokenProvider.isPeekIndexOf(|it| VALID_EXPRESSION_TOKENS.contains(&it), 1)
    }

    fn parse(&self, tokenProvider: &mut TokenProvider<TokenType>, previous: Option<Operation>, parser: &[Box<dyn ParsingUnit<Operation, TokenType>>]) -> Result<Operation, ParserError<TokenType>> {

        let name = tokenProvider.getIdentifier()?;
        let arg = parseExprOneLine(tokenProvider, parser)?;
        println!("previous {:?} {} {:?}", previous, name, arg);

        return Ok(Operation::Expr(Expression::Callable(Box::new(Expression::Variable(name)), vec![previous.unwrap().asExpr()?, arg])))
    }

    fn getPriority(&self) -> usize {
        todo!()
    }

    fn setPriority(&mut self, priority: usize) {
        todo!()
    }
}

#[derive(Debug)]
struct TernaryOperatorParsingUnit;

impl ParsingUnit<Operation, TokenType> for TernaryOperatorParsingUnit {
    fn getType(&self) -> ParsingUnitSearchType {
        Around
    }

    fn canParse(&self, tokenProvider: &TokenProvider<TokenType>, previous: Option<&Operation>) -> bool {
        tokenProvider.isPeekType(QuestionMark) && previous.map_or(false, |it| it.isExpr())
    }

    fn parse(&self, tokenProvider: &mut TokenProvider<TokenType>, previous: Option<Operation>, parser: &[Box<dyn ParsingUnit<Operation, TokenType>>]) -> Result<Operation, ParserError<TokenType>> {
        tokenProvider.getAssert(QuestionMark)?;

        let a = parseExpr(tokenProvider, parser)?;

        tokenProvider.getAssert(Colon)?;

        let b = parseExpr(tokenProvider, parser)?;


        Ok(Operation::Expr(Expression::TernaryOperator(Box::new(previous.unwrap().asExpr()?), Box::new(a), Box::new(b))))
    }

    fn getPriority(&self) -> usize {
        todo!()
    }

    fn setPriority(&mut self, priority: usize) {
        todo!()
    }
}

pub fn parsingUnits() -> Vec<Box<dyn ParsingUnit<Operation, TokenType>>> {
    vec![
        Box::new(AssignableParsingUnit),
        Box::new(StructInitParsingUnit),
        Box::new(NamespaceParsingUnit),
        Box::new(WhileParsingUnit),
        Box::new(LoopParsingUnit),
        Box::new(FunctionParsingUnit),
        Box::new(NumericParsingUnit),
        Box::new(CharParsingUnit),
        Box::new(ArrayIndexingParsingUnit),
        Box::new(StringParsingUnit),
        Box::new(ArrayLiteralParsingUnit),
        Box::new(CallableParsingUnit),
        Box::new(BreakParsingUnit),
        Box::new(NotParsingUnit),
        Box::new(ContinueParsingUnit),
        Box::new(ImportParsingUnit),
        Box::new(NullParsingUnit),
        Box::new(ArithmeticParsingUnit {
            op: BinaryOp::Mul,
            typ: TokenType::Mul,
            priority: 0,
        }),
        Box::new(ArithmeticParsingUnit {
            op: BinaryOp::Div,
            typ: TokenType::Div,
            priority: 1,
        }),
        Box::new(ArithmeticParsingUnit {
            op: BinaryOp::Add,
            typ: TokenType::Plus,
            priority: 2,
        }),
        Box::new(ArithmeticParsingUnit {
            op: BinaryOp::Sub,
            typ: TokenType::Minus,
            priority: 3,
        }),
        Box::new(ArithmeticParsingUnit {
            op: BinaryOp::Eq,
            typ: TokenType::Eq,
            priority: 4,
        }),
        Box::new(ArithmeticParsingUnit {
            op: BinaryOp::Less,
            typ: TokenType::Less,
            priority: 5,
        }),
        Box::new(ArithmeticParsingUnit {
            op: BinaryOp::Gt,
            typ: TokenType::Gt,
            priority: 6,
        }),
        Box::new(ArithmeticParsingUnit {
            op: BinaryOp::And,
            typ: TokenType::And,
            priority: 7,
        }),
        Box::new(ArithmeticParsingUnit {
            op: BinaryOp::Or,
            typ: TokenType::Or,
            priority: 8,
        }),
        Box::new(BracketsParsingUnit),
        Box::new(OneArgFunctionParsintUnit),
        Box::new(VariableParsingUnit),
        Box::new(IfParsingUnit),
        Box::new(BoolParsingUnit),
        Box::new(ReturnParsingUnit),
        Box::new(StructParsingUnit),
        Box::new(IncParsingUnit),
        Box::new(FieldAccessParsingUnit),
        Box::new(GlobalParsingUnit),
        Box::new(ForParsingUnit),
        Box::new(LambdaParsingUnit),
        Box::new(TwoArgFunctionParsintUnit),
        Box::new(TernaryOperatorParsingUnit)
    ]
}
