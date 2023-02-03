#![allow(non_snake_case)]

use std::error::Error;
use std::fmt::{Formatter, write};
use std::ops::Index;

use crate::lexer::TokenType::Break;

#[derive(Debug)]
struct UnknownToken {
    source: String,
}

impl std::fmt::Display for UnknownToken {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "failed to parse remaining source: {}", self.source)
    }
}

impl Error for UnknownToken {}

pub fn tokenize(lexingUnits: &mut [Box<dyn LexingUnit>], mut source: SourceProvider) -> Result<Vec<Token>, Box<dyn Error>> {
    let mut buf = vec![];

    'main: while !source.isDone() {
        for unit in lexingUnits.iter_mut() {
            let reqSize = unit.getRequestSize();
            let peek = source.peekStr(reqSize).unwrap();
            if peek.len() < reqSize {
                continue;
            }
            if unit.canParse(source.peekStr(reqSize).unwrap()) {
                match unit.parse(&mut source) {
                    None => {}
                    Some(v) => buf.push(v),
                }

                continue 'main;
            }
        }
        return Err(Box::try_from(UnknownToken { source: source.peekStr(source.data.len() - source.index).unwrap().to_string() }).unwrap())
    }
    Ok(buf)
}

pub fn tokenizeSource(src: &str) -> Result<Vec<Token>, Box<dyn Error>> {
    let mut lexingUnits = lexingUnits();
    let mut source: SourceProvider = SourceProvider { data: src, index: 0 };

    let mut buf = vec![];

    'main: while !source.isDone() {
        for unit in lexingUnits.iter_mut() {
            let reqSize = unit.getRequestSize();
            let peek = source.peekStr(reqSize).unwrap();
            if peek.len() < reqSize {
                continue;
            }
            if unit.canParse(source.peekStr(reqSize).unwrap()) {
                match unit.parse(&mut source) {
                    None => {}
                    Some(v) => buf.push(v),
                }

                continue 'main;
            }
        }
        return Err(Box::try_from(UnknownToken { source: source.peekStr(source.data.len() - source.index).unwrap().to_string() }).unwrap())
        //panic!("error {:?}", source.peekStr(4));
    }
    Ok(buf)
}

#[derive(Debug)]
pub struct SourceProvider<'a> {
    pub data: &'a str,
    pub index: usize,
}

impl SourceProvider<'_> {
    pub fn peekStr(&self, amount: usize) -> Option<&str> {
        if self.index >= self.data.len() {
            return None
        }

        let res =if self.index + amount > self.data.len() {
            self.data.index(self.index..(self.data.len() - 1))
        }
        else {
            self.data.index(self.index..self.index + amount)
        };
        Some(res)
    }

    pub fn peekChar(&self) -> Option<char> {
        self.data.chars().nth(self.index)
    }

    pub fn consumeMany(&mut self, amount: usize) {
        self.index += amount
    }

    pub fn consumeOne(&mut self) {
        self.index += 1
    }

    pub fn isDone(&self) -> bool {
        self.index >= self.data.len()
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
    MulAs
}

#[derive(Clone, Debug)]
pub struct Token {
    pub typ: TokenType,
    pub str: String,
}

pub trait LexingUnit {
    fn getRequestSize(&self) -> usize;

    fn canParse(&self, data: &str) -> bool;

    fn parse(&mut self, lexer: &mut SourceProvider) -> Option<Token>;
}

pub struct KeywordLexingUnit {
    pub keyword: &'static str,
    pub tokenType: TokenType,
}

pub struct RangeLexingUnit {
    pub start: &'static str,
    pub end: &'static str,
    pub tokenType: Option<TokenType>,
}

impl LexingUnit for RangeLexingUnit {
    fn getRequestSize(&self) -> usize {
        self.start.len()
    }

    fn canParse(&self, data: &str) -> bool {
        data == self.start
    }

    fn parse(&mut self, lexer: &mut SourceProvider) -> Option<Token> {
        println!("parsing {:?}", self.tokenType);
        lexer.consumeMany(self.start.len());

        let mut buf = String::new();

        'lop: while lexer.peekStr(self.end.len()).map_or(false, |v	|{v != self.end}) {
            match lexer.peekChar() {
                Some(v) => {
                    buf.push(v);
                    lexer.consumeOne();
                },
                None => break 'lop
            }
        }
        lexer.consumeMany(self.end.len());

        self.tokenType.map(	|v	| { Token { typ: v, str: buf } })
    }
}

impl RangeLexingUnit {
    #[allow(clippy::new_ret_no_self)]
    pub fn new(start: &'static str, end: &'static str, tokenType: Option<TokenType>) -> Box<dyn LexingUnit> {
        Box::new(Self { start, end, tokenType })
    }
}

impl KeywordLexingUnit {
    #[allow(clippy::new_ret_no_self)]
    pub fn new(keyword: &'static str, tokenType: TokenType) -> Box<dyn LexingUnit> {
        Box::new(Self { keyword, tokenType })
    }
}

struct NumericLexingUnit {}

impl NumericLexingUnit {
    #[allow(clippy::new_ret_no_self)]
    pub fn new() -> Box<dyn LexingUnit> {
        Box::new(Self {})
    }
}

impl LexingUnit for NumericLexingUnit {
    fn getRequestSize(&self) -> usize {
        1
    }

    fn canParse(&self, data: &str) -> bool {
        let c = data.chars().next().unwrap();
        c == '-' || c.is_ascii_digit()
    }

    fn parse(&mut self, lexer: &mut SourceProvider) -> Option<Token> {
        let mut buf = String::new();
        let mut typ = TokenType::IntLiteral;
        let encounteredDot = false;

        if lexer.peekChar().unwrap() == '-' {
            buf.push('-');
            lexer.consumeOne();
        }

        while lexer.peekChar().map_or(false, |c| { c == '.' || c == '_' || c.is_numeric()}) {
            let c = lexer.peekChar().unwrap();
            if c == '.' && encounteredDot {
                // fixme return result from lexer
                // panic!("number cant have more than 1 dots")
                return None
            }
            if c == '.' {
                typ = TokenType::FloatLiteral
            }
            if c != '_' {
                buf.push(c)
            }
            lexer.consumeOne();
        }

        match lexer.peekChar() {
            None => {}
            Some(v) => {
                match v {
                    'f' => {
                        typ = TokenType::FloatLiteral;
                        lexer.consumeOne()
                    }
                    'L' => {
                        typ = TokenType::LongLiteral;
                        lexer.consumeOne()
                    }
                    'D' => {
                        typ = TokenType::DoubleLiteral;
                        lexer.consumeOne()
                    }
                    _ => {}
                }
            }
        }

        Some(Token { typ, str: buf })
    }
}

struct IdentifierLexingUnit {}

impl IdentifierLexingUnit {
    #[allow(clippy::new_ret_no_self)]
    pub fn new() -> Box<dyn LexingUnit> {
        Box::new(Self {})
    }
}

impl LexingUnit for IdentifierLexingUnit {
    fn getRequestSize(&self) -> usize {
        1
    }

    fn canParse(&self, data: &str) -> bool {
        let c = data.chars().next().unwrap();
        c == '_' || c.is_alphabetic()
    }

    fn parse(&mut self, lexer: &mut SourceProvider) -> Option<Token> {
        let mut buf = String::new();
        while lexer.peekChar().map_or(false, |v|{ match v { c => c == '_' || c.is_alphabetic() || c.is_numeric() }}) {
            buf.push(lexer.peekChar().unwrap());
            lexer.consumeOne();
        }

        Some(Token {
            typ: TokenType::Identifier,
            str: buf,
        })
    }
}

struct WhitespaceLexingUnit {}

impl WhitespaceLexingUnit {
    #[allow(clippy::new_ret_no_self)]
    pub fn new() -> Box<dyn LexingUnit> {
        Box::new(Self {})
    }
}

impl LexingUnit for WhitespaceLexingUnit {
    fn getRequestSize(&self) -> usize {
        1
    }

    fn canParse(&self, data: &str) -> bool {
        let c = data.chars().next().unwrap();

        c.is_whitespace()
    }

    fn parse(&mut self, lexer: &mut SourceProvider) -> Option<Token> {
        while lexer.peekChar().map_or(false,  |v| { v.is_whitespace() }) {
            lexer.consumeOne()
        }

        None
    }
}

impl LexingUnit for KeywordLexingUnit {
    fn getRequestSize(&self) -> usize {
        self.keyword.len()
    }

    fn canParse(&self, data: &str) -> bool {
        data == self.keyword
    }

    fn parse(&mut self, source: &mut SourceProvider) -> Option<Token> {
        let str = source.peekStr(self.keyword.len())?.to_string();
        source.consumeMany(self.keyword.len());

        Some(Token {
            typ: self.tokenType,
            str,
        })
    }
}

pub fn lexingUnits() -> Vec<Box<dyn LexingUnit>> {
    let buf = vec![
        WhitespaceLexingUnit::new(),
        NumericLexingUnit::new(),
        KeywordLexingUnit::new("fn", TokenType::Fn),
        KeywordLexingUnit::new("var", TokenType::Var),
        KeywordLexingUnit::new("while", TokenType::While),
        KeywordLexingUnit::new("if", TokenType::If),
        KeywordLexingUnit::new("else", TokenType::Else),
        KeywordLexingUnit::new("for", TokenType::For),
        KeywordLexingUnit::new("loop", TokenType::Loop),
        KeywordLexingUnit::new("return", TokenType::Return),
        KeywordLexingUnit::new("break", TokenType::Break),
        KeywordLexingUnit::new("continue", TokenType::Continue),
        KeywordLexingUnit::new("false", TokenType::False),
        KeywordLexingUnit::new("true", TokenType::True),

        //
        KeywordLexingUnit::new("+=", TokenType::AddAs),
        KeywordLexingUnit::new("-=", TokenType::SubAs),
        KeywordLexingUnit::new("*=", TokenType::MulAs),
        KeywordLexingUnit::new("/=", TokenType::DivAs),
        KeywordLexingUnit::new("==", TokenType::Eq),
        KeywordLexingUnit::new(">", TokenType::Less),
        KeywordLexingUnit::new("<", TokenType::Gt),
        KeywordLexingUnit::new("!", TokenType::Not),
        //
        KeywordLexingUnit::new(";", TokenType::Semicolon),
        KeywordLexingUnit::new("=", TokenType::Equals),
        KeywordLexingUnit::new(":", TokenType::Colon),
        KeywordLexingUnit::new(",", TokenType::Comma),
        // ops
        KeywordLexingUnit::new("+", TokenType::Plus),
        KeywordLexingUnit::new("-", TokenType::Minus),
        KeywordLexingUnit::new("*", TokenType::Mul),
        KeywordLexingUnit::new("/", TokenType::Div),
        // brackets
        KeywordLexingUnit::new("(", TokenType::ORB),
        KeywordLexingUnit::new(")", TokenType::CRB),
        KeywordLexingUnit::new("[", TokenType::OSB),
        KeywordLexingUnit::new("]", TokenType::CSB),
        KeywordLexingUnit::new("{", TokenType::OCB),
        KeywordLexingUnit::new("}", TokenType::CCB),
        IdentifierLexingUnit::new(),
        RangeLexingUnit::new("\'", "\'", Some(TokenType::CharLiteral)),
        RangeLexingUnit::new("\"", "\"", Some(TokenType::StringLiteral)),
    ];

    buf
}

#[test]
fn testLexer() {
    let mut lexingUnits = lexingUnits();
    let input = "x = 69 fn main() { x = -420.69 print(69*x) while (x == 1) { print(69) } if () }";

    let src = SourceProvider {
        data: input,
        index: 0,
    };

    let tokens = tokenize(&mut lexingUnits, src);
    println!("{:?}", tokens);
}
