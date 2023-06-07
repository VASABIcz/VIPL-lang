#![allow(non_snake_case)]

use crate::errors::{LexerError, UnknownToken};
use crate::lexer;
use std::error::Error;
use std::fmt::{Debug, Formatter};
use std::ops::{Index, Range};
use crate::lexingUnits::TokenType;

const DEBUG: bool = false;

pub fn tokenize<T: Debug + Send + Sync + PartialEq + Clone>(
    lexingUnits: &mut [Box<dyn LexingUnit<T>>],
    mut source: SourceProvider,
) -> Result<Vec<Token<T>>, LexerError> {
    let mut buf = vec![];

    'main: while !source.isDone() {
        for unit in lexingUnits.iter_mut() {
            if unit.canParse(&source) {
                if DEBUG {
                    println!("lexer working on {:?}", unit);
                }
                if let Some(v) = unit.parse(&mut source)? {
                    buf.push(v)
                }

                continue 'main;
            }
        }
        return Err(LexerError::UnknownToken(UnknownToken {
            source: source.getRemaing(),
            location: source.getLocation(),
        }));
    }
    Ok(buf)
}

pub fn tokenizeSource(src: &str, units: &mut [Box<dyn LexingUnit<TokenType>>]) -> Result<Vec<Token<TokenType>>, LexerError> {
    let source: SourceProvider = SourceProvider {
        data: src,
        index: 0,
        row: 0,
        col: 0,
    };

    tokenize(units, source)
}

#[derive(Debug)]
pub struct SourceProvider<'a> {
    pub data: &'a str,
    pub index: usize,
    pub row: usize,
    pub col: usize,
}

impl SourceProvider<'_> {
    fn getRemaing(&self) -> String {
        let mut buf = String::new();

        let mut i = 0;

        while let Some(v) = self.peekCharOffset(i) {
            if v.is_whitespace() {
                break;
            }
            i += 1;

            buf.push(v);
        }

        buf
    }

    fn peekStr(&self, amount: usize) -> Option<&str> {
        if self.index >= self.data.len() {
            return None;
        }

        let res = if self.index + amount > self.data.len() {
            self.data.index(self.index..(self.data.len() - 1))
        } else {
            self.data.index(self.index..self.index + amount)
        };
        Some(res)
    }

    fn peekChar(&self) -> Option<char> {
        self.data.chars().nth(self.index)
    }

    fn peekCharOffset(&self, offset: usize) -> Option<char> {
        self.data.chars().nth(self.index + offset)
    }

    pub fn consumeMany(&mut self, amount: usize) {
        for _ in 0..amount {
            // FIXME
            self.consumeOne();
        }
    }

    pub fn consumeOne(&mut self) {
        self.col += 1;
        if self.isPeek("\n") {
            self.col = 0;
            self.row += 1;
        }
        self.index += 1
    }

    pub fn isDone(&self) -> bool {
        self.index >= self.data.len()
    }

    pub fn isPeek(&self, s: &str) -> bool {
        self.peekStr(s.len()).map_or(false, |it| it == s)
    }

    pub fn assertConsume(&mut self, s: &str) -> Result<(), LexerError> {
        if !self.isPeek(s) {
            None.ok_or_else(||LexerError::ExpectedChar(s.to_string(), self.getLocation()))?
        } else {
            self.consumeMany(s.len());
            Ok(())
        }
    }

    pub fn isPeekOffset(&self, s: &str, offset: usize) -> bool {
        self.peekStr(s.len() + offset)
            .map_or(false, |it| &it[offset..] == s)
    }

    pub fn isPeekChar<T: Fn(char) -> bool>(&self, f: T) -> bool {
        self.peekStr(1)
            .map_or(false, |it| f(it.bytes().next().unwrap() as char))
    }

    pub fn isPeekOffsetChar(&self, f: fn(char) -> bool, offset: usize) -> bool {
        self.peekStr(offset + 1)
            .map_or(false, |it| f(*it.as_bytes().get(offset).unwrap() as char))
    }

    pub fn getLocation(&self) -> Location {
        println!("get loc: {} {} {}", self.row, self.col, self.index);
        Location {
            row: self.row,
            col: self.col,
            index: self.index,
        }
    }

    pub fn assertAmount<T: Debug + PartialEq + Clone>(
        &mut self,
        amount: usize,
        typ: T,
    ) -> Result<Token<T>, LexerError> {
        let s = self
            .peekStr(amount)
            .ok_or(LexerError::NotEnoughCharacters(amount, self.getLocation()))?
            .to_string();

        let loc = self.getLocation();

        self.consumeMany(amount);

        Ok(Token {
            typ,
            str: s,
            location: loc,
        })
    }

    pub fn assertChar(&mut self) -> Result<char, LexerError> {
        let c = self
            .peekStr(1)
            .ok_or(LexerError::ReachedEOF(self.getLocation()))?
            .chars()
            .next()
            .unwrap();
        self.consumeOne();
        Ok(c)
    }

    pub fn consumeWhileMatches<T: Debug + PartialEq + Clone>(
        &mut self,
        f: fn(char) -> bool,
        typ: Option<T>,
    ) -> Result<Option<Token<T>>, LexerError> {
        let start = self.index;

        let loc = self.getLocation();

        while self.isPeekChar(f) {
            self.consumeOne();
        }

        Ok(typ.map(|it| Token {
            typ: it,
            str: self.data[start..self.index].to_string(),
            location: loc,
        }))
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct Location {
    pub row: usize,
    pub col: usize,
    pub index: usize,
}

impl Location {
    pub fn toRange(&self, len: usize) -> Range<usize> {
        self.col..self.col+len
    }
}

#[derive(Clone, Debug)]
pub struct Token<T: PartialEq + Clone + Clone> {
    pub typ: T,
    pub str: String,
    pub location: Location,
}

pub trait LexingUnit<T: Debug + PartialEq + Clone>: Send + Sync + Debug {
    fn canParse(&self, lexer: &SourceProvider) -> bool;

    fn parse(&mut self, lexer: &mut SourceProvider) -> Result<Option<Token<T>>, LexerError>;
}

#[derive(Debug)]
pub struct AlphabeticKeywordLexingUnit<T: Debug + Clone + Copy> {
    pub keyword: &'static str,
    pub tokenType: T,
}

impl<T: Debug + Send + Sync + Clone + Copy + PartialEq> LexingUnit<T>
    for AlphabeticKeywordLexingUnit<T>
{
    fn canParse(&self, lexer: &SourceProvider) -> bool {
        lexer.isPeek(self.keyword)
            && lexer.isPeekOffsetChar(
                |it| !it.is_ascii_digit() && !it.is_ascii_alphabetic() && !(it == '_'),
                self.keyword.len(),
            )
    }

    fn parse(&mut self, source: &mut SourceProvider) -> Result<Option<Token<T>>, LexerError> {
        let token = source.assertAmount(self.keyword.len(), self.tokenType)?;

        Ok(Some(token))
    }
}

impl<T: Debug + Send + Sync + Clone + Copy + 'static + PartialEq> AlphabeticKeywordLexingUnit<T> {
    #[allow(clippy::new_ret_no_self)]
    pub fn new(keyword: &'static str, tokenType: T) -> Box<dyn LexingUnit<T>> {
        Box::new(Self { keyword, tokenType })
    }
}

#[derive(Debug)]
pub struct KeywordLexingUnit<T> {
    pub keyword: &'static str,
    pub tokenType: T,
}

#[derive(Debug)]
pub struct RangeLexingUnit<T: Debug + Clone + Copy> {
    pub start: &'static str,
    pub end: &'static str,
    pub tokenType: Option<T>,
}

impl<T: Debug + Send + Sync + Clone + Copy + PartialEq> LexingUnit<T> for RangeLexingUnit<T> {
    fn canParse(&self, lexer: &SourceProvider) -> bool {
        lexer.isPeek(self.start)
    }

    fn parse(&mut self, lexer: &mut SourceProvider) -> Result<Option<Token<T>>, LexerError> {
        let loc = lexer.getLocation();

        lexer.consumeMany(self.start.len());

        let mut buf = String::from(self.start);

        'lop: while !lexer.isPeek(self.end) {
            match lexer.peekChar() {
                Some(v) => {
                    buf.push(v);
                    lexer.consumeOne();
                }
                None => break 'lop,
            }
        }

        buf += self.end;

        lexer.consumeMany(self.end.len());

        Ok(self.tokenType.map(|v| Token {
            typ: v,
            str: buf,
            location: loc,
        }))
    }
}

impl<T: Debug + Send + Sync + Clone + Copy + 'static + PartialEq> RangeLexingUnit<T> {
    #[allow(clippy::new_ret_no_self)]
    pub fn new(
        start: &'static str,
        end: &'static str,
        tokenType: Option<T>,
    ) -> Box<dyn LexingUnit<T>> {
        Box::new(Self {
            start,
            end,
            tokenType,
        })
    }
}

impl<T: Debug + Send + Sync + 'static + Copy + PartialEq> KeywordLexingUnit<T> {
    #[allow(clippy::new_ret_no_self)]
    pub fn new(keyword: &'static str, tokenType: T) -> Box<dyn LexingUnit<T>> {
        Box::new(Self { keyword, tokenType })
    }
}

#[derive(Debug)]
pub struct IdentifierLexingUnit<T: Debug + Clone> {
    pub tokenType: T,
}

impl<T: Debug + Clone + Send + Sync + 'static + PartialEq> IdentifierLexingUnit<T> {
    #[allow(clippy::new_ret_no_self)]
    pub fn new(typ: T) -> Box<dyn LexingUnit<T>> {
        Box::new(Self { tokenType: typ })
    }
}

impl<T: Debug + Clone + Sync + Send + PartialEq> LexingUnit<T> for IdentifierLexingUnit<T> {
    fn canParse(&self, lexer: &SourceProvider) -> bool {
        lexer.isPeekChar(|it| it.is_ascii_alphabetic() || it == '_')
    }

    fn parse(&mut self, lexer: &mut SourceProvider) -> Result<Option<Token<T>>, LexerError> {
        Ok(lexer.consumeWhileMatches(
            |it| it.is_alphanumeric() || it == '_',
            Some(self.tokenType.clone()),
        )?)
    }
}

#[derive(Debug)]
pub struct WhitespaceLexingUnit;

impl WhitespaceLexingUnit {
    #[allow(clippy::new_ret_no_self)]
    pub fn new<T: Debug + Send + Sync + PartialEq + Clone>() -> Box<dyn LexingUnit<T>> {
        Box::new(Self {})
    }
}

impl<T: Debug + Send + Sync + PartialEq + Clone> LexingUnit<T> for WhitespaceLexingUnit {
    fn canParse(&self, lexer: &SourceProvider) -> bool {
        lexer.isPeekChar(|it| it.is_whitespace())
    }

    fn parse(&mut self, lexer: &mut SourceProvider) -> Result<Option<Token<T>>, LexerError> {
        lexer.consumeWhileMatches::<T>(|it| it.is_whitespace(), None)?;

        Ok(None)
    }
}

impl<T: Debug + Send + Sync + Clone + Copy + PartialEq> LexingUnit<T> for KeywordLexingUnit<T> {
    fn canParse(&self, lexer: &SourceProvider) -> bool {
        lexer.isPeek(self.keyword)
    }

    fn parse(&mut self, source: &mut SourceProvider) -> Result<Option<Token<T>>, LexerError> {
        let t = source.assertAmount(self.keyword.len(), self.tokenType)?;

        Ok(Some(t))
    }
}
