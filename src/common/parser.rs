use core::fmt;
use std::collections::HashMap;
use std::error::Error;
use std::fmt::{Debug, Display, Formatter};
use std::usize;

use crate::ast;
use crate::ast::RawExpression::{IntLiteral, NamespaceAccess};
use crate::ast::RawStatement::{Assignable, StatementExpression};
use crate::ast::{ASTNode, ArithmeticOp, ArrayAccess, BinaryOp, RawExpression, RawNode, RawStatement, StructDef, VariableCreate, VariableModd, WhileS};
use crate::bytecodeGen::Body;

use crate::errors::{CodeGenError, InvalidToken, LexerError, NoSuchParsingUnit, ParserError, SymbolType};

use crate::lexer::{SourceProvider, Token};
use crate::lexingUnits::TokenType;
use crate::naughtyBox::Naughty;
use crate::parser::ParsingUnitSearchType::{Ahead, Around, Behind};
use crate::parsingUnits::{parsingUnits};
use crate::vm::dataType::{Generic, ObjectMeta};
use crate::viplParser::VIPLParser;
use crate::vm::variableMetadata::VariableMetadata;

const DEBUG: bool = false;

#[derive(Clone, Debug)]
pub struct TokenProvider<T: PartialEq + Clone> {
    pub tokens: Vec<Token<T>>,
    pub index: usize,
}

impl<T: PartialEq + Debug + Clone + Copy + 'static> TokenProvider<T> {
    pub fn findOffsetIgnoring(&self, increment: T, decrement: T, startOffset: isize) -> Option<isize> {
        let mut count = 0;
        let mut index = (self.index as isize) + startOffset;

        loop {
            let peek = self.peekOffset(index)?;

            if peek.typ == decrement {
                count -= 1;
            }
            else if peek.typ == increment {
                count += 1;
            }

            if count == 0 {
                return Some(index - self.index as isize)
            }

            index += 1;
        }
    }

    pub fn new(tokens: Vec<Token<T>>) -> TokenProvider<T> {
        Self { tokens, index: 0 }
    }

    pub fn peekOne(&self) -> Option<&Token<T>> {
        self.tokens.get(self.index)
    }

    pub fn peekOneRes(&self) -> Result<&Token<T>, ParserError<T>> {
        Ok(self.tokens.get(self.index).ok_or_else(||ParserError::NoToken)?)
    }

    pub fn isPeekRow(&self, row: usize) -> bool {
        match self.peekOne() {
            None => false,
            Some(v) => v.location.row == row,
        }
    }

    pub fn isPeekSameRow(&self) -> bool {
        if self.index == 0 {
            return true
        }

        match self.peekOne() {
            None => false,
            Some(v) => v.location.row == self.tokens.get(self.index-1).unwrap().location.row,
        }
    }

    pub fn peekOffsetRow(&self, index: isize, row: usize) -> bool {
        match self.peekOffset(index) {
            None => false,
            Some(v) => v.location.row == row,
        }
    }

    pub fn peekOffset(&self, offset: isize) -> Option<&Token<T>> {
        self.tokens.get((self.index as isize + offset) as usize)
    }

    pub fn consume(&mut self) {
        if DEBUG {
            println!("consuming {:?}", self.tokens[self.index]);
        }

        self.index += 1
    }

    pub fn isPeekTypeMany(&self, types: &[T]) -> bool {
        for (index, typ) in types.into_iter().enumerate() {
            if !self.isPeekIndexType(*typ, index as isize) {
                return false;
            }
        }

        true
    }

    pub fn isPeekOneOf(&self, types: &[T]) -> bool {
        match self.peekOne() {
            None => false,
            Some(v) => types.contains(&v.typ)
        }
    }

    pub fn isPeekOffsetOneOf(&self, types: &[T], offset: isize) -> bool {
        match self.peekOffset(offset) {
            None => false,
            Some(v) => types.contains(&v.typ)
        }
    }

    pub fn parseManyWithSeparatorUntil<
        F: FnMut(&mut TokenProvider<T>) -> Result<OUT, ParserError<T>>,
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

    pub fn ifPeekGet(&mut self, typ: T) -> Option<Token<T>> {
        if self.isPeekType(typ) {
            Some(self.getAssert(typ).unwrap().clone())
        }
        else {
            None
        }

    }

    pub fn isPeekTypeOf(&self, f: fn(T) -> bool) -> bool {
        let t = self.peekOne();

        match t {
            None => false,
            Some(v) => f(v.typ),
        }
    }

    pub fn isPeekIndexType(&self, typ: T, offset: isize) -> bool {
        let t = self.peekOffset(offset);

        match t {
            None => false,
            Some(v) => v.typ == typ,
        }
    }

    pub fn isDone(&self) -> bool {
        self.index >= self.tokens.len()
    }

    pub fn isPeekIndexOf(&self, f: fn(T) -> bool, offset: isize) -> bool {
        let t = self.peekOffset(offset);

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

    fn getPriority(&self) -> usize {
        todo!()
    }
}

#[derive(Debug)]
pub struct Parser<'a, IN: Clone + PartialEq + Debug + 'static, OUT: Debug, STATE: Debug> {
    pub tokens: TokenProvider<IN>,
    pub units: &'a mut [Box<dyn ParsingUnit<OUT, IN, STATE>>],
    pub state: STATE,
    pub previousBuf: Vec<OUT>
}

impl<IN: Clone + PartialEq + Debug + Copy, OUT: Debug, STATE: Debug> Parser<'_, IN, OUT, STATE> {
    pub fn prevPop(&mut self) -> Result<OUT, ParserError<IN>> {
        self.previousBuf.pop().ok_or(ParserError::Unknown("fuuck".to_string().into()))
    }

    pub fn parseType(
        &mut self,
        typ: ParsingUnitSearchType
    ) -> Result<bool, ParserError<IN>> {
        let s2 = unsafe { &mut *(self as *mut Parser<_, _, _>) };

        for unit in &mut *self.units {
            let parserType = unit.getType();

            if parserType != typ {
                continue;
            }

            if !unit.canParse(s2) {
                continue;
            }

            if DEBUG {
                println!("parsing {:?}", unit);
            }

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

        // FIXME just a quick workaround
        while let Some(v) = self.getParsingUnit(Around) {
            self.previousBuf.push(v.parse(s2)?);
        }

        Ok(s2.prevPop().unwrap())
    }

    pub fn parseOneJust(
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