use crate::{DataType, OpCode};
use crate::OpCode::Not;

#[derive(Clone)]
struct SourceProvider {
    index: usize,
    text: Vec<char>
}

#[derive(Clone, Debug)]
enum TokenType {
    IntLiteral(isize),
    FloatLiteral(f32),
    StringLiteral(String),
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

    ORB,
    CRB,
    OSB,
    CSB,
    OCB,
    CCB,

    Semicolon,
    Equals,
    Colon,

    Eq,
    Gt,
    Less,
    Not
}

#[derive(Debug, Clone)]
struct Token {
    pub data: String,
    pub tokenType: TokenType
}

impl SourceProvider {
    fn consume(&mut self) {
        self.index += 1;
    }

    fn consumeMany(&mut self, n: usize) {
        self.index += n;
    }

    fn peekChar(&self) -> Option<&char> {
        self.text.get(self.index)
    }

    fn peekMany(&self, n: usize) -> Option<&[char]> {
        if self.index + n >= self.text.len() {
            return None
        }
        Some(&self.text[self.index..self.index + n])
    }
}


trait LexingUnit {
    fn canParse(&mut self, lexer: &mut Lexer) -> bool;

    fn parse(&mut self, lexer: &mut Lexer);
}

struct KeywordLexingUnit {
    pub keyword: String,
    pub tokenType: TokenType
}

impl LexingUnit for KeywordLexingUnit {
    fn canParse(&mut self,lexer: &mut Lexer) -> bool {
        match lexer.source.peekMany(self.keyword.len()) {
            None => false,
            Some(v) => {
                v.iter().collect::<String>() == self.keyword
            }
        }
    }

    fn parse(&mut self,lexer: &mut Lexer) {
        lexer.source.consumeMany(self.keyword.len());
        lexer.tokens.push(Token {
            data: self.keyword.clone(),
            tokenType: self.tokenType.clone(),
        })
    }
}

struct Lexer {
    pub source: SourceProvider,
    pub tokens: Vec<Token>,
    pub lexingUnits: Vec<Box<dyn LexingUnit>>
}

impl Lexer {
    fn parse(&mut self) {
        while self.source.peekChar() != None {
            let u = match self.lexingUnits.get_mut(0) {
                None => break,
                Some(v) => v
            };
            let p = u.canParse(self);
            if p {
                u.parse(self)
            }
        }
    }
}

fn test() {
    let l = Lexer {
        source: SourceProvider { index: 0, text: "lmaolol".chars().collect() },
        tokens: vec![],
        lexingUnits: vec![
            Box::new(KeywordLexingUnit {
                keyword: "lmao".to_string(),
                tokenType: TokenType::OCB,
            }),
            Box::new(KeywordLexingUnit {
                keyword: "lol".to_string(),
                tokenType: TokenType::CCB,
            })
        ],
    };
    l.parse();
    println!("{:?}", l.tokens)
}