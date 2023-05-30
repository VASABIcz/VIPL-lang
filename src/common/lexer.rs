#![allow(non_snake_case)]

use std::collections::HashMap;
use crate::errors::{LexerError, UnknownToken};
use crate::lexer;
use crate::lexer::TokenType::Identifier;
use std::error::Error;
use std::fmt::{Debug, Formatter};
use std::ops::Index;

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

    pub fn assertConsume(&mut self, s: &str) -> Result<(), Box<dyn Error>> {
        if !self.isPeek(s) {
            None.ok_or(format!("expected {}", s))?
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

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum TokenType {
    IntLiteral,
    LongLiteral,
    FloatLiteral,
    DoubleLiteral,
    StringLiteral,
    CharLiteral,
    LambdaBegin,

    Identifier,

    Plus,
    Minus,
    Div,
    Mul,

    Fn,
    Var,
    While,
    Loop,
    For,
    True,
    False,
    If,
    Else,
    Continue,
    Break,
    Return,
    New,
    Struct,
    Native,
    Namespace,
    Import,
    Global,
    In,
    Null,
    As,

    ORB,
    CRB,
    OSB,
    CSB,
    OCB,
    CCB,

    Semicolon,
    Equals,
    Colon,
    Comma,

    Eq,
    Gt,
    Less,
    Not,

    AddAs,
    SubAs,
    DivAs,
    MulAs,

    And,
    Or,

    Dot,

    Inc,
    Dec,
    NewLine,
    QuestionMark,
    Repeat,
}

pub trait Stringable {
    fn toStr(&self) -> &'static str;
}

impl Stringable for TokenType {
    fn toStr(&self) -> &'static str {
        match self {
            TokenType::IntLiteral => "IntLiteral",
            TokenType::LongLiteral => "LongLiteral",
            TokenType::FloatLiteral => "FloatLiteral",
            TokenType::DoubleLiteral => "DoubleLiteral",
            TokenType::StringLiteral => "StringLiteral",
            TokenType::CharLiteral => "CharLiteral",
            TokenType::LambdaBegin => "LambdaBegin",
            Identifier => "Identifier",
            TokenType::Plus => "+",
            TokenType::Minus => "-",
            TokenType::Div => "/",
            TokenType::Mul => "*",
            TokenType::Fn => "fn",
            TokenType::Var => "var",
            TokenType::While => "while",
            TokenType::Loop => "loop",
            TokenType::For => "for",
            TokenType::True => "true",
            TokenType::False => "false",
            TokenType::If => "if",
            TokenType::Else => "else",
            TokenType::Continue => "continue",
            TokenType::Break => "break",
            TokenType::Return => "return",
            TokenType::New => "new",
            TokenType::Struct => "struct",
            TokenType::Native => "native",
            TokenType::Namespace => "::",
            TokenType::Import => "import",
            TokenType::Global => "global",
            TokenType::In => "int",
            TokenType::Null => "null",
            TokenType::ORB => "(",
            TokenType::CRB => ")",
            TokenType::OSB => "[",
            TokenType::CSB => "]",
            TokenType::OCB => "{",
            TokenType::CCB => "}",
            TokenType::Semicolon => ";",
            TokenType::Equals => "=",
            TokenType::Colon => ":",
            TokenType::Comma => ",",
            TokenType::Eq => "==",
            TokenType::Gt => ">",
            TokenType::Less => "<",
            TokenType::Not => "!",
            TokenType::AddAs => "+=",
            TokenType::SubAs => "-=",
            TokenType::DivAs => "/=",
            TokenType::MulAs => "*=",
            TokenType::And => "&&",
            TokenType::Or => "||",
            TokenType::Dot => ".",
            TokenType::Inc => "++",
            TokenType::Dec => "--",
            TokenType::NewLine => "\\n",
            TokenType::QuestionMark => "?",
            TokenType::Repeat => "Repeat",
            TokenType::As => "As"
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct Location {
    pub row: usize,
    pub col: usize,
    pub index: usize,
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
        lexer.consumeMany(self.start.len());

        let mut buf = String::new();

        'lop: while !lexer.isPeek(self.end) {
            match lexer.peekChar() {
                Some(v) => {
                    buf.push(v);
                    lexer.consumeOne();
                }
                None => break 'lop,
            }
        }
        let loc = lexer.getLocation();
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
struct NumericLexingUnit {}

impl NumericLexingUnit {
    #[allow(clippy::new_ret_no_self)]
    pub fn new() -> Box<dyn LexingUnit<TokenType>> {
        Box::new(Self {})
    }
}

impl LexingUnit<TokenType> for NumericLexingUnit {
    fn canParse(&self, lexer: &SourceProvider) -> bool {
        lexer.isPeekChar(|it| it.is_ascii_digit())
    }

    fn parse(
        &mut self,
        lexer: &mut SourceProvider,
    ) -> Result<Option<Token<TokenType>>, LexerError> {
        let mut buf = String::new();
        let mut typ = TokenType::IntLiteral;
        let encounteredDot = false;

        let loc = lexer.getLocation();

        while lexer.isPeekChar(|c| c == '.' || c == '_' || c.is_numeric()) {
            let c = lexer.assertChar()?;
            if c == '.' && encounteredDot {
                // fixme return result from lexer
                // panic!("number cant have more than 1 dots")
                return None.ok_or("number cant have more than one dot".into());
            }
            if c == '.' {
                typ = TokenType::FloatLiteral
            }
            if c != '_' {
                buf.push(c)
            }
        }

        if lexer.isPeek("f") {
            typ = TokenType::FloatLiteral;
            lexer.consumeOne()
        } else if lexer.isPeek("L") {
            typ = TokenType::LongLiteral;
            lexer.consumeOne()
        } else if lexer.isPeek("D") {
            typ = TokenType::DoubleLiteral;
            lexer.consumeOne()
        }

        Ok(Some(Token {
            typ,
            str: buf,
            location: loc,
        }))
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

#[derive(Debug, Default)]
pub struct LookupLexingUnit<T: Debug + PartialEq> {
    pub lookup: HashMap<char, Vec<Box<dyn LexingUnit<T>>>>
}

impl<T: Clone + Debug + PartialEq> LookupLexingUnit<T> {
    pub fn register(&mut self, c: char, unit: Box<dyn LexingUnit<T>>) {
        match self.lookup.get_mut(&c) {
            None => {
                self.lookup.insert(c, vec![unit]);
            }
            Some(v) => {
                v.push(unit)
            }
        }
    }
}

impl<T: PartialEq + Debug + Clone> LexingUnit<T> for LookupLexingUnit<T> {
    fn canParse(&self, lexer: &SourceProvider) -> bool {
        lexer.isPeekChar(|it| {
            self.lookup.contains_key(&it)
        })
    }

    fn parse(&mut self, lexer: &mut SourceProvider) -> Result<Option<Token<T>>, LexerError> {
        let c = lexer.peekChar().unwrap();

        let a = self.lookup.get_mut(&c).unwrap();

        for unit in a.iter_mut() {
            if !unit.canParse(lexer) {
                continue
            }

            let b = unit.parse(lexer);

            return b;
        }
        Err(LexerError::UnknownToken(UnknownToken { source: lexer.getRemaing(), location: lexer.getLocation() }))
    }
}

pub fn lexingUnits() -> Vec<Box<dyn LexingUnit<TokenType>>> {
    let mut l = LookupLexingUnit{ lookup: Default::default() };

    l.register('f', AlphabeticKeywordLexingUnit::new("fn", TokenType::Fn));
    l.register('w', AlphabeticKeywordLexingUnit::new("while", TokenType::While));
    l.register('i', AlphabeticKeywordLexingUnit::new("if", TokenType::If));
    l.register('e', AlphabeticKeywordLexingUnit::new("else", TokenType::Else));
    l.register('f', AlphabeticKeywordLexingUnit::new("for", TokenType::For));
    l.register('l', AlphabeticKeywordLexingUnit::new("loop", TokenType::Loop));
    l.register('r', AlphabeticKeywordLexingUnit::new("return", TokenType::Return));
    l.register('b', AlphabeticKeywordLexingUnit::new("break", TokenType::Break));
    l.register('c', AlphabeticKeywordLexingUnit::new("continue", TokenType::Continue));
    l.register('f', AlphabeticKeywordLexingUnit::new("false", TokenType::False));
    l.register('t', AlphabeticKeywordLexingUnit::new("true", TokenType::True));
    l.register('s', AlphabeticKeywordLexingUnit::new("struct", TokenType::Struct));
    l.register('i', AlphabeticKeywordLexingUnit::new("import", TokenType::Import));
    l.register('g', AlphabeticKeywordLexingUnit::new("global", TokenType::Global));
    l.register('i', AlphabeticKeywordLexingUnit::new("in", TokenType::In));
    l.register('n', AlphabeticKeywordLexingUnit::new("null", TokenType::Null));
    l.register('r', AlphabeticKeywordLexingUnit::new("repeat", TokenType::Repeat));
    l.register('a', AlphabeticKeywordLexingUnit::new("as", TokenType::As));

    l.register('&', KeywordLexingUnit::new("&&", TokenType::And));
    l.register('|', KeywordLexingUnit::new("||", TokenType::Or));
    l.register('+', KeywordLexingUnit::new("+=", TokenType::AddAs));
    l.register('-', KeywordLexingUnit::new("-=", TokenType::SubAs));
    l.register('*', KeywordLexingUnit::new("*=", TokenType::MulAs));
    l.register('/', KeywordLexingUnit::new("/=", TokenType::DivAs));
    l.register('=', KeywordLexingUnit::new("==", TokenType::Eq));
    l.register('>', KeywordLexingUnit::new(">", TokenType::Gt));
    l.register('<', KeywordLexingUnit::new("<", TokenType::Less));
    l.register('!', KeywordLexingUnit::new("!", TokenType::Not));
    l.register(':', KeywordLexingUnit::new("::", TokenType::Namespace));
    l.register('.', KeywordLexingUnit::new(".", TokenType::Dot));
    l.register(';', KeywordLexingUnit::new(";", TokenType::Semicolon));
    l.register('=', KeywordLexingUnit::new("=", TokenType::Equals));
    l.register(':', KeywordLexingUnit::new(":", TokenType::Colon));
    l.register(',', KeywordLexingUnit::new(",", TokenType::Comma));
    l.register('?', KeywordLexingUnit::new("?", TokenType::QuestionMark));
    l.register('+', KeywordLexingUnit::new("+", TokenType::Plus));
    l.register('-', KeywordLexingUnit::new("-", TokenType::Minus));
    l.register('*', KeywordLexingUnit::new("*", TokenType::Mul));
    l.register('/', KeywordLexingUnit::new("/", TokenType::Div));
    l.register('(', KeywordLexingUnit::new("(", TokenType::ORB));
    l.register(')', KeywordLexingUnit::new(")", TokenType::CRB));
    l.register('[', KeywordLexingUnit::new("[", TokenType::OSB));
    l.register(']', KeywordLexingUnit::new("]", TokenType::CSB));
    l.register('{', KeywordLexingUnit::new("{", TokenType::OCB));
    l.register('}', KeywordLexingUnit::new("}", TokenType::CCB));


    vec![
        WhitespaceLexingUnit::new(),
        NumericLexingUnit::new(),
        Box::new(l),
        RangeLexingUnit::new("\'", "\'", Some(TokenType::CharLiteral)),
        RangeLexingUnit::new("\"", "\"", Some(TokenType::StringLiteral)),
        RangeLexingUnit::new("//", "\n", None),
        RangeLexingUnit::new("/*", "*/", None),
        IdentifierLexingUnit::new(TokenType::Identifier),
    ]
}
