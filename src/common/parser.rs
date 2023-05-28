use core::fmt;
use std::collections::HashMap;
use std::error::Error;
use std::fmt::{Debug, Display, Formatter};
use std::usize;

use crate::ast;
use crate::ast::Expression::{IntLiteral, NamespaceAccess};
use crate::ast::Statement::{Assignable, StatementExpression};
use crate::ast::{
    ASTNode, ArithmeticOp, ArrayAccess, BinaryOp, Expression, Node, Statement, StructDef,
    VariableCreate, VariableModd, WhileS,
};
use crate::bytecodeGen::Body;

use crate::errors::{InvalidToken, NoSuchParsingUnit, ParserError, SymbolType};

use crate::lexer::{SourceProvider, Token, TokenType};
use crate::lexer::TokenType::{CCB, Colon, Comma, CRB, Gt, Identifier, Not, ORB};
use crate::parser::ParsingUnitSearchType::{Ahead, Around, Behind};
use crate::parsingUnits::{parsingUnits};
use crate::vm::dataType::{DataType, Generic, ObjectMeta};
use crate::vm::variableMetadata::VariableMetadata;

#[derive(Debug)]
pub struct VIPLParsingState {
    pub symbols: HashMap<String, SymbolType>
}

/*pub fn parse<OUT: Clone, IN: Clone + Debug + Sync + Send + PartialEq + 'static + Copy, STATE: Debug>(
    tokens: &mut TokenProvider<IN>,
    typ: ParsingUnitSearchType,
    parsingUnits: &[Box<dyn ParsingUnit<OUT, IN, STATE>>],
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
                Behind => parserType == Behind || parserType == Around,
                Ahead => parserType == Ahead || parserType == Around,
            };

            if canParse && unit.canParse(self) {
                let res =
                    if !*isPrevUser && (parserType == Around || parserType == Behind) && counter == 1
                    {
                        *isPrevUser = true;
                        unit.parse(tokens, previous.clone(), parsingUnits)?
                    } else if parserType == Around || parserType == Behind {
                        unit.parse(tokens, opBuf.clone().take(), parsingUnits)?
                    } else {
                        unit.parse(tokens, None, parsingUnits)?
                    };

                if parserType == Behind {
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
                    if !*isPrevUser && (parserType == Around || parserType == Behind) && counter == 1
                    {
                        *isPrevUser = true;
                        unit.parse(tokens, previous.clone(), parsingUnits)?
                    } else if parserType == Around || parserType == Behind {
                        unit.parse(tokens, opBuf.clone().take(), parsingUnits)?
                    } else {
                        unit.parse(tokens, None, parsingUnits)?
                    };

                if parserType == Behind {
                    opBuf = Some(res);
                    continue 'main;
                }

                // println!("{:?}", &res);
                buf.push(res);
                continue 'main;
            }
        }
        return Err(ParserError::NoSuchParsingUnit(NoSuchParsingUnit {
            typ,
            token: tokens.peekOne().cloned(),
        }));
    }
    match opBuf {
        None => {}
        Some(v) => buf.push(v),
    }
    Ok(buf)
}*/


#[derive(Clone, Debug)]
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

    pub fn peekOne(&self) -> Option<&Token<T>> {
        self.tokens.get(self.index)
    }

    pub fn peekOneRes(&self) -> Result<&Token<T>, ParserError<T>> {
        Ok(self.tokens.get(self.index).unwrap())
    }

    pub fn isPeekRow(&self, row: usize) -> bool {
        match self.peekOne() {
            None => false,
            Some(v) => v.location.row == row,
        }
    }

    pub fn isPeekIndexRow(&self, index: usize, row: usize) -> bool {
        match self.peekIndex(index) {
            None => false,
            Some(v) => v.location.row == row,
        }
    }

    pub fn peekIndex(&self, offset: usize) -> Option<&Token<T>> {
        self.tokens.get(self.index + offset)
    }

    pub fn consume(&mut self) {
        println!("consuming {:?}", self.tokens[self.index]);

        self.index += 1
    }

    pub fn isPeekTypeMany(&self, types: &[T]) -> bool {
        for (index, typ) in types.into_iter().enumerate() {
            if !self.isPeekIndexType(*typ, index) {
                return false;
            }
        }

        return true;
    }

    pub fn parseManyWithSeparatorUntil<
        F: core::ops::FnMut(&mut TokenProvider<T>) -> Result<OUT, ParserError<T>>,
        OUT,
    >(
        &mut self,
        mut f: F,
        sep: Option<T>,
        end: T,
    ) -> Result<Vec<OUT>, ParserError<T>> {
        let mut buf = vec![];

        if self.isPeekType(end) {
            self.getAssert(end)?;
            return Ok(vec![]);
        }

        match f(self) {
            Ok(v) => buf.push(v),
            Err(e) => return Err(e),
        }

        while sep
            .map(|it| self.isPeekType(it))
            .unwrap_or(!self.isPeekType(end))
        {
            if let Some(v) = sep {
                self.getAssert(v)?;
            }
            match f(self) {
                Ok(v) => buf.push(v),
                Err(e) => return Err(e),
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
                return Err(ParserError::InvalidToken(InvalidToken {
                    expected: typ,
                    actual: None,
                }))
            }
            Some(v) => v,
        };
        if t.typ != typ {
            // panic!();
            return Err(ParserError::InvalidToken(InvalidToken {
                expected: typ,
                actual: Some(t.clone()),
            }));
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

#[derive(Eq, PartialEq, Debug, Clone, Copy)]
pub enum ParsingUnitSearchType {
    Around,
    Behind,
    Ahead,
}

pub trait ParsingUnit<OUT: Debug, IN: PartialEq + Clone + Debug, STATE: Debug>: Debug {
    fn getType(&self) -> ParsingUnitSearchType;

    fn canParse(&self, parser: &Parser<IN, OUT, STATE>) -> bool;

    fn parse(
        &self,
        parser: &mut Parser<IN, OUT, STATE>
    ) -> Result<OUT, ParserError<IN>>;

    fn getPriority(&self) -> usize;

    fn setPriority(&mut self, priority: usize);
}

#[derive(Debug)]
pub struct Parser<IN: Clone + PartialEq + Debug + 'static, OUT: Debug, STATE: Debug> {
    pub tokens: TokenProvider<IN>,
    pub units: Vec<Box<dyn ParsingUnit<OUT, IN, STATE>>>,
    pub state: STATE,
    pub previousBuf: Vec<OUT>
}

impl<IN: Clone + PartialEq + Debug + Copy, OUT: Debug, STATE: Debug> Parser<IN, OUT, STATE> {
    pub fn previous(&self) -> Option<&OUT> {
        self.previousBuf.last()
    }

    pub fn prevPop(&mut self) -> Option<OUT> {
        self.previousBuf.pop()
    }

    pub fn parseType(
        &mut self,
        typ: ParsingUnitSearchType
    ) -> Result<bool, ParserError<IN>> {
        let s2 = unsafe { &mut *(self as *mut Parser<_, _, _>) };

        for unit in &mut self.units {
            let parserType = unit.getType();

            if parserType != typ {
                continue;
            }

            if !unit.canParse(s2) {
                continue;
            }

            println!("parsing {:?}", unit);
            match typ {
                Around | Behind => {
                    let res = match unit.parse(s2) {
                        Ok(v) => v,
                        Err(e) => {
                            return Err(e);
                        }
                    };
                    self.previousBuf.push(res);
                    return Ok(true);
                }
                Ahead => {
                    let res = match unit.parse(s2) {
                        Ok(v) => v,
                        Err(e) => {
                            return Err(e);
                        }
                    };
                    self.previousBuf.push(res);
                    return Ok(true);
                }
            }
        }
        Ok(false)
    }

    pub fn parseOneOneLine(
        &mut self,
        typ: ParsingUnitSearchType,
    ) -> Result<OUT, ParserError<IN>> {
        let s2 = unsafe { &mut *(self as *mut Parser<_, _, _>) };

        let row = self.tokens.peekOneRes()?.location.row;

        let u = self.getParsingUnit(typ.clone()).ok_or(
            ParserError::NoSuchParsingUnit(NoSuchParsingUnit {
                typ,
                token: self.tokens.peekOne().cloned(),
            }),
        )?;

        self.previousBuf.push(u.parse(s2)?);

        while let Some(v) = self.getParsingUnit(Behind) && self.tokens.peekOneRes()?.location.row == row {
            self.previousBuf.push(v.parse(s2)?);
        }

        if let Some(v) = self.getParsingUnit(Around) && self.tokens.peekOneRes()?.location.row == row {
            self.previousBuf.push(v.parse(s2)?);
        }

        Ok(self.prevPop().unwrap())
    }

    pub fn parseOne(
        &mut self,
        typ: ParsingUnitSearchType
    ) -> Result<OUT, ParserError<IN>> {
        let s2 = unsafe { &mut *(self as *mut Parser<_, _, _>) };

        let u = self.getParsingUnit(typ).ok_or(
            ParserError::NoSuchParsingUnit(NoSuchParsingUnit {
                typ,
                token: self.tokens.peekOne().cloned(),
            }),
        )?;

        self.previousBuf.push(u.parse(s2)?);

        while let Some(v) = self.getParsingUnit(Behind) {
            self.previousBuf.push(v.parse(s2)?);
        }

        if let Some(v) = self.getParsingUnit(Around) {
            self.previousBuf.push(v.parse(s2)?);
        }

        Ok(s2.prevPop().unwrap())
    }

    pub fn getParsingUnit(
        &self,
        typ: ParsingUnitSearchType,
    ) -> Option<&Box<dyn ParsingUnit<OUT, IN, STATE>>> {
        self.units.iter().find(|it| {
            let parserType = it.getType();

            let canParse = match typ {
                Around => parserType != Ahead,
                Behind => parserType == Behind,
                Ahead => parserType == Ahead,
            };

            canParse && it.canParse(self)
        })
    }
}

pub fn parseTokens(toks: Vec<Token<TokenType>>) -> Result<Vec<ASTNode>, ParserError<TokenType>> {
    let parsingUnits = parsingUnits();
    let tokens = TokenProvider::new(toks);

    let mut parser = Parser{
        tokens,
        units: parsingUnits,
        state: VIPLParsingState{ symbols: Default::default() },
        previousBuf: vec![],
    };

    while !parser.tokens.isDone() {
        if parser.previousBuf.is_empty() {
            if parser.parseType(Ahead)? {
                continue;
            }

            if parser.parseType(Behind)? {
                continue;
            }

            if parser.parseType(Around)? {
                continue;
            }
        } else {
            if parser.parseType(Behind)? {
                continue;
            }

            if parser.parseType(Around)? {
                continue;
            }
            if parser.parseType(Ahead)? {
                continue;
            }
        }

        return Err(ParserError::NoSuchParsingUnit(NoSuchParsingUnit {
            typ: Ahead,
            token: parser.tokens.peekOne().cloned(),
        }));
    }
    Ok(parser.previousBuf)
}

impl Parser<TokenType, ASTNode, VIPLParsingState> {
    pub fn parseExprOneLine(
        &mut self
    ) -> Result<Expression, ParserError<TokenType>> {
        let res = self.parseOneOneLine(Ahead)?;
        println!("wtf {:?}", res);
        res.asExpr()
    }

    pub fn parseDataType(
        &mut self
    ) -> Result<DataType, ParserError<TokenType>> {
        parseDataType(&mut self.tokens)
    }

    pub fn parseExpr(
        &mut self
    ) -> Result<Expression, ParserError<TokenType>> {
        let res = self.parseOne(Ahead)?;
        res.asExpr()
    }

    pub fn parseBody(
        &mut self,
    ) -> Result<Body, ParserError<TokenType>> {
        let mut statements = vec![];

        self.tokens.getAssert(TokenType::OCB)?;

        while !self.tokens.isPeekType(CCB) {
            statements.push(self.parseOne(Ahead)?.asStatement()?);
        }

        self.tokens.getAssert(TokenType::CCB)?;

        Ok(Body::new(statements))
    }
}

pub fn parseDataType(
    tokens: &mut TokenProvider<TokenType>,
) -> Result<DataType, ParserError<TokenType>> {
    if tokens.isPeekType(Identifier) && tokens.isPeekIndexType(Gt, 1) {
        let mut generics = vec![];

        let t = tokens.getIdentifier()?;
        tokens.getAssert(TokenType::Gt)?;

        while !tokens.isPeekType(TokenType::Less) {
            generics.push(Generic::Type(parseDataType(tokens)?));
        }
        tokens.getAssert(TokenType::Less)?;

        Ok(DataType::Object(ObjectMeta {
            name: t,
            generics: generics.into_boxed_slice(),
        }))
    } else if tokens.isPeekType(TokenType::ORB) {
        tokens.getAssert(ORB)?;
        let args = tokens.parseManyWithSeparatorUntil(|it| parseDataType(it), Some(Comma), CRB)?;
        let ret = if tokens.isPeekType(Colon) {
            tokens.getAssert(Colon)?;
            parseDataType(tokens)?
        } else {
            DataType::Void
        };
        Ok(DataType::Function {
            args,
            ret: Box::new(ret),
        })
    } else if tokens.isPeekType(Not) {
        tokens.getAssert(Not)?;
        Ok(DataType::Void)
    } else {
        let t = tokens.getIdentifier()?;

        match t.as_str() {
            "bool" => return Ok(DataType::Bool),
            "char" => return Ok(DataType::Char),
            "int" => return Ok(DataType::Int),
            "float" => return Ok(DataType::Float),
            c => {
                return Ok(DataType::Object(ObjectMeta {
                    name: c.to_string().into(),
                    generics: Box::new([]),
                }))
            }
        }
    }
}