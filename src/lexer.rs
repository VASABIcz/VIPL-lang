#![allow(non_snake_case)]
use std::ops::Index;

pub fn tokenize(lexingUnits: &mut [Box<dyn LexingUnit>], mut source: SourceProvider) -> Vec<Token> {
    let mut buf = vec![];

    'main: while !source.isDone() {
        for unit in lexingUnits.iter_mut() {
            let reqSize = unit.getRequestSize();
            let peek = source.peekStr(reqSize);
            if peek.len() < reqSize {
                continue;
            }
            if unit.canParse(source.peekStr(reqSize)) {
                match unit.parse(&mut source) {
                    None => {}
                    Some(v) => buf.push(v),
                }

                continue 'main;
            }
        }
        panic!("error {:?}", source.peekStr(4));
    }
    buf
}

#[derive(Debug)]
pub struct SourceProvider<'a> {
    pub data: &'a str,
    pub index: usize,
}

impl SourceProvider<'_> {
    pub fn peekStr(&self, amount: usize) -> &str {
        if self.index + amount > self.data.len() {
            return self.data.index(self.index..(self.data.len() - 1));
        }
        self.data.index(self.index..self.index + amount)
    }

    pub fn peekChar(&self) -> char {
        self.data.chars().nth(self.index).unwrap()
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
    Identifier,

    Plus,
    Minus,
    Div,
    Mul,

    Fn,
    Var,
    While,
    True,
    False,
    If,
    Else,

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
        c == '-' || data.chars().next().unwrap().is_ascii_digit()
    }

    fn parse(&mut self, lexer: &mut SourceProvider) -> Option<Token> {
        let mut buf = String::new();
        let mut typ = TokenType::IntLiteral;
        let encounteredDot = false;

        if lexer.peekChar() == '-' {
            buf.push('-');
            lexer.consumeOne();
        }

        while match lexer.peekChar() {
            c => c == '.' || c == '_' || c.is_numeric(),
        } {
            let c = lexer.peekChar();
            if c == '.' && encounteredDot {
                panic!("number cant have more than 1 dots")
            }
            if c == '.' {
                typ = TokenType::FloatLiteral
            }
            if c != '_' {
                buf.push(c)
            }
            lexer.consumeOne();
        }

        let peek = lexer.peekChar();
        match peek {
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
        while match lexer.peekChar() {
            c => c == '_' || c.is_alphabetic() || c.is_numeric(),
        } {
            buf.push(lexer.peekChar());
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
        while lexer.peekChar().is_whitespace() {
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
        let str = source.peekStr(self.keyword.len()).to_string();
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
        KeywordLexingUnit::new("false", TokenType::False),
        KeywordLexingUnit::new("true", TokenType::True),
        KeywordLexingUnit::new("==", TokenType::Eq),
        KeywordLexingUnit::new(">", TokenType::Less),
        KeywordLexingUnit::new("<", TokenType::Gt),
        KeywordLexingUnit::new("!", TokenType::Not),
        //
        KeywordLexingUnit::new(";", TokenType::Semicolon),
        KeywordLexingUnit::new("=", TokenType::Equals),
        KeywordLexingUnit::new(":", TokenType::Colon),
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
    ];

    buf
}

#[test]
fn testLexer() {
    let lexingUnits = lexingUnits();
    let input = "x = 69 fn main() { x = -420.69 print(69*x) while (x == 1) { print(69) } if () }";

    let src = SourceProvider {
        data: input,
        index: 0,
    };

    let tokens = tokenize(&mut lexingUnits.into_boxed_slice(), src);
    println!("{:?}", tokens);
}
