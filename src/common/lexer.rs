#![allow(non_snake_case)]

use std::error::Error;
use std::fmt::{Debug, Formatter};
use std::ops::Index;
use crate::errors::UnknownToken;
use crate::lexer::TokenType::Identifier;

pub fn tokenize(
    lexingUnits: &mut [Box<dyn LexingUnit>],
    mut source: SourceProvider,
) -> Result<Vec<Token>, Box<dyn Error>> {
    let mut buf = vec![];

    'main: while !source.isDone() {
        for unit in lexingUnits.iter_mut() {
            if unit.canParse(&source) {
                println!("working on {:?}", unit);
                if let Some(v) = unit.parse(&mut source)? {
                    buf.push(v)
                }

                continue 'main;
            }
        }
        return Err(Box::try_from(UnknownToken {
            source: source
                .peekStr(source.data.len() - source.index)
                .unwrap()
                .to_string(),
        })
            .unwrap());
    }
    Ok(buf)
}

pub fn tokenizeSource(src: &str) -> Result<Vec<Token>, Box<dyn Error>> {
    let mut lexingUnits = lexingUnits();
    let mut source: SourceProvider = SourceProvider {
        data: src,
        index: 0,
    };

    return tokenize(&mut lexingUnits, source)
}

#[derive(Debug)]
pub struct SourceProvider<'a> {
    pub data: &'a str,
    pub index: usize,
}

impl SourceProvider<'_> {
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

    pub fn consumeMany(&mut self, amount: usize) {
        self.index += amount
    }

    pub fn consumeOne(&mut self) {
        self.index += 1
    }

    pub fn isDone(&self) -> bool {
        self.index >= self.data.len()
    }

    pub fn isPeek(&self, s: &str) -> bool {
        self.peekStr(s.len()).map_or(false, |it| {
            it == s
        })
    }

    pub fn isPeekOffset(&self, s: &str, offset: usize) -> bool {
        self.peekStr(s.len()+offset).map_or(false, |it| {
            &it[offset..] == s
        })
    }

    pub fn isPeekChar(&self, f: fn(char) -> bool) -> bool {
        self.peekStr(1).map_or(false, |it| {
            f(it.bytes().next().unwrap() as char)
        })
    }

    pub fn isPeekOffsetChar(&self, f: fn(char) -> bool, offset: usize) -> bool {
        self.peekStr(offset+1).map_or(false, |it| {
            f(*it.as_bytes().get(offset).unwrap() as char)
        })
    }

    pub fn assertAmount(&mut self, amount: usize, typ: TokenType) -> Result<Token, Box<dyn Error>> {
        let s = self.peekStr(amount).ok_or(format!("insuficient amount required {}", amount))?.to_string();

        self.consumeMany(amount);

        Ok(Token {
            typ,
            str: s,
        })
    }

    pub fn assertChar(&mut self) -> Result<char, Box<dyn Error>> {
        let c = self.peekStr(1).ok_or("expected char go EOL")?.chars().next().unwrap();
        self.consumeOne();
        Ok(c)
    }

    pub fn consumeWhileMatches(&mut self, f: fn (char) -> bool, typ: Option<TokenType>) -> Result<Option<Token>, Box<dyn Error>> {
        let start = self.index;
        while self.isPeekChar(f) {
            self.consumeOne();
        }

        Ok(typ.map(|it| {
            Token { typ: it, str: self.data[start..self.index].to_string() }
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
    Dec
}

#[derive(Clone, Debug)]
pub struct Token {
    pub typ: TokenType,
    pub str: String,
}

pub trait LexingUnit: Send + Sync + Debug {
    fn canParse(&self, lexer: &SourceProvider) -> bool;

    fn parse(&mut self, lexer: &mut SourceProvider) -> Result<Option<Token>, Box<dyn Error>>;
}

#[derive(Debug)]
pub struct AlphabeticKeywordLexingUnit {
    pub keyword: &'static str,
    pub tokenType: TokenType,
}

impl LexingUnit for AlphabeticKeywordLexingUnit {
    fn canParse(&self, lexer: &SourceProvider) -> bool {
        lexer.isPeek(self.keyword) && lexer.isPeekOffsetChar(|it| {
            !it.is_ascii_digit() && !it.is_ascii_alphabetic() && !(it == '_')
        }, self.keyword.len())
    }

    fn parse(&mut self, source: &mut SourceProvider) -> Result<Option<Token>, Box<dyn Error>> {
        let token = source.assertAmount(self.keyword.len(), self.tokenType)?;

        Ok(Some(token))
    }
}

impl AlphabeticKeywordLexingUnit {
    #[allow(clippy::new_ret_no_self)]
    pub fn new(keyword: &'static str, tokenType: TokenType) -> Box<dyn LexingUnit> {
        Box::new(Self { keyword, tokenType })
    }
}

#[derive(Debug)]
pub struct KeywordLexingUnit {
    pub keyword: &'static str,
    pub tokenType: TokenType,
}

#[derive(Debug)]
pub struct RangeLexingUnit {
    pub start: &'static str,
    pub end: &'static str,
    pub tokenType: Option<TokenType>,
}

impl LexingUnit for RangeLexingUnit {
    fn canParse(&self, lexer: &SourceProvider) -> bool {
        lexer.isPeek(self.start)
    }

    fn parse(&mut self, lexer: &mut SourceProvider) -> Result<Option<Token>, Box<dyn Error>> {
        lexer.consumeMany(self.start.len());

        let mut buf = String::new();

        'lop: while !lexer.isPeek(self.end)
        {
            match lexer.peekChar() {
                Some(v) => {
                    buf.push(v);
                    lexer.consumeOne();
                }
                None => break 'lop,
            }
        }
        lexer.consumeMany(self.end.len());

        Ok(self.tokenType.map(|v| Token { typ: v, str: buf }))
    }
}

impl RangeLexingUnit {
    #[allow(clippy::new_ret_no_self)]
    pub fn new(
        start: &'static str,
        end: &'static str,
        tokenType: Option<TokenType>,
    ) -> Box<dyn LexingUnit> {
        Box::new(Self {
            start,
            end,
            tokenType,
        })
    }
}

impl KeywordLexingUnit {
    #[allow(clippy::new_ret_no_self)]
    pub fn new(keyword: &'static str, tokenType: TokenType) -> Box<dyn LexingUnit> {
        Box::new(Self { keyword, tokenType })
    }
}

#[derive(Debug)]
struct NumericLexingUnit;

impl NumericLexingUnit {
    #[allow(clippy::new_ret_no_self)]
    pub fn new() -> Box<dyn LexingUnit> {
        Box::new(Self {})
    }
}

impl LexingUnit for NumericLexingUnit {
    fn canParse(&self, lexer: &SourceProvider) -> bool {
        lexer.isPeekChar(|it|{
            it.is_ascii_digit()
        })
    }

    fn parse(&mut self, lexer: &mut SourceProvider) -> Result<Option<Token>, Box<dyn Error>> {
        let mut buf = String::new();
        let mut typ = TokenType::IntLiteral;
        let encounteredDot = false;

        while lexer.isPeekChar(|c| { c == '.' || c == '_' || c.is_numeric() })
        {
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
        }
        else if lexer.isPeek("L") {
            typ = TokenType::LongLiteral;
            lexer.consumeOne()
        }
        else if lexer.isPeek("D") {
            typ = TokenType::DoubleLiteral;
            lexer.consumeOne()
        }

        Ok(Some(Token { typ, str: buf }))
    }
}

#[derive(Debug)]
struct IdentifierLexingUnit;

impl IdentifierLexingUnit {
    #[allow(clippy::new_ret_no_self)]
    pub fn new() -> Box<dyn LexingUnit> {
        Box::new(Self {})
    }
}

impl LexingUnit for IdentifierLexingUnit {
    fn canParse(&self, lexer: &SourceProvider) -> bool {
        lexer.isPeekChar(|it| {
            it.is_ascii_alphabetic() || it == '_'
        })
    }

    fn parse(&mut self, lexer: &mut SourceProvider) -> Result<Option<Token>, Box<dyn Error>> {
        Ok(lexer.consumeWhileMatches(|it| {
            it.is_alphanumeric() || it == '_'
        }, Some(Identifier))?)
    }
}

#[derive(Debug)]
struct WhitespaceLexingUnit;

impl WhitespaceLexingUnit {
    #[allow(clippy::new_ret_no_self)]
    pub fn new() -> Box<dyn LexingUnit> {
        Box::new(Self {})
    }
}

impl LexingUnit for WhitespaceLexingUnit {
    fn canParse(&self, lexer: &SourceProvider) -> bool {
        lexer.isPeekChar(|it| {
            it.is_whitespace()
        })
    }

    fn parse(&mut self, lexer: &mut SourceProvider) -> Result<Option<Token>, Box<dyn Error>> {
        lexer.consumeWhileMatches(|it| {
            it.is_whitespace()
        }, None)?;

        Ok(None)
    }
}

impl LexingUnit for KeywordLexingUnit {
    fn canParse(&self, lexer: &SourceProvider) -> bool {
        lexer.isPeek(self.keyword)
    }

    fn parse(&mut self, source: &mut SourceProvider) -> Result<Option<Token>, Box<dyn Error>> {
        let t = source.assertAmount(self.keyword.len(), self.tokenType)?;

        Ok(Some(t))
    }
}



pub fn lexingUnits() -> Vec<Box<dyn LexingUnit>> {
    vec![
        WhitespaceLexingUnit::new(),
        NumericLexingUnit::new(),
        AlphabeticKeywordLexingUnit::new("fn", TokenType::Fn),
        AlphabeticKeywordLexingUnit::new("var", TokenType::Var),
        AlphabeticKeywordLexingUnit::new("while", TokenType::While),
        AlphabeticKeywordLexingUnit::new("if", TokenType::If),
        AlphabeticKeywordLexingUnit::new("else", TokenType::Else),
        AlphabeticKeywordLexingUnit::new("for", TokenType::For),
        AlphabeticKeywordLexingUnit::new("loop", TokenType::Loop),
        AlphabeticKeywordLexingUnit::new("return", TokenType::Return),
        AlphabeticKeywordLexingUnit::new("break", TokenType::Break),
        AlphabeticKeywordLexingUnit::new("continue", TokenType::Continue),
        AlphabeticKeywordLexingUnit::new("false", TokenType::False),
        AlphabeticKeywordLexingUnit::new("true", TokenType::True),
        AlphabeticKeywordLexingUnit::new("new", TokenType::New),
        AlphabeticKeywordLexingUnit::new("struct", TokenType::Struct),
        AlphabeticKeywordLexingUnit::new("native", TokenType::Native),
        AlphabeticKeywordLexingUnit::new("import", TokenType::Import),
        AlphabeticKeywordLexingUnit::new("global", TokenType::Global),
        AlphabeticKeywordLexingUnit::new("in", TokenType::In),

        KeywordLexingUnit::new("&&", TokenType::And),
        KeywordLexingUnit::new("||", TokenType::Or),
        RangeLexingUnit::new("//", "\n", None),
        RangeLexingUnit::new("/*", "*\\", None),
        //
        KeywordLexingUnit::new("+=", TokenType::AddAs),
        KeywordLexingUnit::new("-=", TokenType::SubAs),
        KeywordLexingUnit::new("*=", TokenType::MulAs),
        KeywordLexingUnit::new("/=", TokenType::DivAs),
        KeywordLexingUnit::new("==", TokenType::Eq),
        KeywordLexingUnit::new(">", TokenType::Less),
        KeywordLexingUnit::new("<", TokenType::Gt),
        KeywordLexingUnit::new("!", TokenType::Not),
        KeywordLexingUnit::new("::", TokenType::Namespace),
        KeywordLexingUnit::new(".", TokenType::Dot),
        KeywordLexingUnit::new("->", TokenType::LambdaBegin),
        KeywordLexingUnit::new("++", TokenType::Inc),
        KeywordLexingUnit::new("--", TokenType::Dec),
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
        RangeLexingUnit::new("\'", "\'", Some(TokenType::CharLiteral)),
        RangeLexingUnit::new("\"", "\"", Some(TokenType::StringLiteral)),
        IdentifierLexingUnit::new(),
    ]
}