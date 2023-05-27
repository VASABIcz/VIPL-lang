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

use crate::errors::{InvalidToken, NoSuchParsingUnit, ParserError};

use crate::lexer::{Token, TokenType};
use crate::parser::ParsingUnitSearchType::{Ahead, Around, Back};
use crate::parsingUnits::{parseBody, parseDataType, parseExpr, parsingUnits};
use crate::vm::dataType::{DataType, Generic, ObjectMeta};
use crate::vm::variableMetadata::VariableMetadata;

pub fn getParsingUnit<'a, OUT, IN: PartialEq + Clone + Debug>(
    tokens: &mut TokenProvider<IN>,
    typ: ParsingUnitSearchType,
    parsingUnits: &'a [Box<dyn ParsingUnit<OUT, IN>>],
    previous: Option<OUT>,
) -> Option<&'a Box<dyn ParsingUnit<OUT, IN>>> {
    parsingUnits.iter().find(|it| {
        let parserType = it.getType();

        let canParse = match typ {
            Around => parserType != Ahead,
            Back => parserType == Back,
            Ahead => parserType == Ahead,
        };

        let p = match &previous {
            Some(v) => Some(v),
            None => None,
        };

        canParse && it.canParse(tokens, p)
    })
}

pub fn parseExprOneLine(
    tokenProvider: &mut TokenProvider<TokenType>,
    parser: &[Box<dyn ParsingUnit<ASTNode, TokenType>>],
) -> Result<Expression, ParserError<TokenType>> {
    let res = parseOneOneLine(tokenProvider, Ahead, parser, None)?;
    res.asExpr()
}

pub fn parseOneOneLine<
    OUT: Clone + Debug,
    IN: Clone + Debug + Send + Sync + PartialEq + 'static + Copy,
>(
    tokens: &mut TokenProvider<IN>,
    typ: ParsingUnitSearchType,
    parsingUnits: &[Box<dyn ParsingUnit<OUT, IN>>],
    previous: Option<OUT>,
) -> Result<OUT, ParserError<IN>> {
    let row = tokens.peekOneRes()?.location.row;

    let u = getParsingUnit(tokens, typ.clone(), parsingUnits, previous.clone()).ok_or(
        ParserError::NoSuchParsingUnit(NoSuchParsingUnit {
            typ,
            token: tokens.peekOne().cloned(),
        }),
    )?;

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

pub fn parseOne<
    OUT: Clone + Debug,
    IN: Clone + Debug + Send + Sync + PartialEq + 'static + Copy,
>(
    tokens: &mut TokenProvider<IN>,
    typ: ParsingUnitSearchType,
    parsingUnits: &[Box<dyn ParsingUnit<OUT, IN>>],
    previous: Option<OUT>,
) -> Result<OUT, ParserError<IN>> {
    let u = getParsingUnit(tokens, typ.clone(), parsingUnits, previous.clone()).ok_or(
        ParserError::NoSuchParsingUnit(NoSuchParsingUnit {
            typ,
            token: tokens.peekOne().cloned(),
        }),
    )?;

    println!("parsing {:?}", u);
    let mut first = u.parse(tokens, previous, parsingUnits)?;

    while let Some(v) = getParsingUnit(
        tokens,
        ParsingUnitSearchType::Back,
        parsingUnits,
        Some(first.clone()),
    ) {
        first = v.parse(tokens, Some(first), parsingUnits)?;
    }

    if let Some(v) = getParsingUnit(
        tokens,
        ParsingUnitSearchType::Around,
        parsingUnits,
        Some(first.clone()),
    ) {
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
}

pub fn parseType<OUT, IN: PartialEq + Clone + Debug>(
    parsingUnits: &Vec<Box<dyn ParsingUnit<OUT, IN>>>,
    tokens: &mut TokenProvider<IN>,
    typ: ParsingUnitSearchType,
    buf: &mut Vec<OUT>,
) -> Result<bool, ParserError<IN>> {
    for unit in parsingUnits.iter() {
        let parserType = unit.getType();

        if parserType != typ {
            continue;
        }

        if !unit.canParse(&tokens, buf.last()) {
            continue;
        }

        match typ {
            Around | Back => {
                let res = match unit.parse(tokens, buf.pop(), parsingUnits) {
                    Ok(v) => v,
                    Err(e) => {
                        eprintln!("exception inside {:?}", unit);
                        return Err(e);
                    }
                };
                buf.push(res);
                return Ok(true);
            }
            Ahead => {
                let res = match unit.parse(tokens, None, parsingUnits) {
                    Ok(v) => v,
                    Err(e) => {
                        eprintln!("exception inside {:?}", unit);
                        return Err(e);
                    }
                };
                buf.push(res);
                return Ok(true);
            }
        }
    }
    Ok(false)
}

pub fn parseTokens(toks: Vec<Token<TokenType>>) -> Result<Vec<ASTNode>, ParserError<TokenType>> {
    let mut buf = vec![];
    let parsingUnits = &parsingUnits();
    let mut tokens = TokenProvider::new(toks);

    while !tokens.isDone() {
        if buf.is_empty() {
            if parseType(parsingUnits, &mut tokens, Ahead, &mut buf)? {
                continue;
            }

            if parseType(parsingUnits, &mut tokens, Back, &mut buf)? {
                continue;
            }

            if parseType(parsingUnits, &mut tokens, Around, &mut buf)? {
                continue;
            }
        } else {
            if parseType(parsingUnits, &mut tokens, Back, &mut buf)? {
                continue;
            }

            if parseType(parsingUnits, &mut tokens, Around, &mut buf)? {
                continue;
            }
            if parseType(parsingUnits, &mut tokens, Ahead, &mut buf)? {
                continue;
            }
        }

        return Err(ParserError::NoSuchParsingUnit(NoSuchParsingUnit {
            typ: ParsingUnitSearchType::Ahead,
            token: tokens.peekOne().cloned(),
        }));
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
        F: core::ops::Fn(&mut TokenProvider<T>) -> Result<OUT, ParserError<T>>,
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
