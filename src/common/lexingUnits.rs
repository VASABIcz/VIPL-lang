use crate::errors::LexerError;
use crate::lexer::{AlphabeticKeywordLexingUnit, IdentifierLexingUnit, KeywordLexingUnit, LexingUnit, RangeLexingUnit, SourceProvider, Token, WhitespaceLexingUnit};

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum TokenType {
    IntLiteral,
    LongLiteral,
    FloatLiteral,
    DoubleLiteral,
    StringLiteral,
    FormatStringLiteral,
    CharLiteral,
    LambdaBegin,

    Identifier,

    Plus,
    Minus,
    Div,
    Mul,
    Modulo,

    Fn,
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
    Struct,
    Namespace,
    Import,
    From,
    Global,
    In,
    Null,
    As,
    Is,

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
    NotEq,
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

    NewLine,
    QuestionMark,
    Repeat,
    ShiftRight,
    ShiftLeft,
    BitwiseAnd,
    BitwiseOr,
    BitwiseNot,
    Xor,
    NullAssert
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
            TokenType::Identifier => "Identifier",
            TokenType::Plus => "+",
            TokenType::Minus => "-",
            TokenType::Div => "/",
            TokenType::Mul => "*",
            TokenType::Fn => "fn",
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
            TokenType::Struct => "struct",
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
            TokenType::NewLine => "\\n",
            TokenType::QuestionMark => "?",
            TokenType::Repeat => "Repeat",
            TokenType::As => "As",
            TokenType::From => "From",
            TokenType::FormatStringLiteral => "FormatStringLiteral",
            TokenType::Modulo => "%",
            TokenType::Is => "Is",
            TokenType::NotEq => "!=",
            TokenType::ShiftRight => ">>",
            TokenType::ShiftLeft => "<<",
            TokenType::BitwiseAnd => "&",
            TokenType::BitwiseOr => "|",
            TokenType::BitwiseNot => "~",
            TokenType::Xor => "^",
            TokenType::NullAssert => "!!"
        }
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
        let mut encounteredDot = false;

        let loc = lexer.getLocation();

        while lexer.isPeekChar(|c| c == '.' || c == '_' || c.is_numeric()) {
            if lexer.isPeek(".") && !encounteredDot {
                if !lexer.isPeekOffsetChar(|c| c.is_numeric(), 1) {
                    return Ok(Some(Token {
                        typ,
                        str: buf,
                        location: loc,
                    }))
                }

                typ = TokenType::FloatLiteral;
                encounteredDot = true;
                buf.push(lexer.assertChar()?);
                buf.push(lexer.assertChar()?);
                continue
            }

            let c = lexer.assertChar()?;
            if c == '.' && encounteredDot {
                // fixme return result from lexer
                // panic!("number cant have more than 1 dots")
                return None.ok_or("number cant have more than one dot".into());
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

pub fn lexingUnits() -> Vec<Box<dyn LexingUnit<TokenType>>> {
    vec![
        WhitespaceLexingUnit::new(),
        NumericLexingUnit::new(),
        AlphabeticKeywordLexingUnit::new("fn", TokenType::Fn),
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
        AlphabeticKeywordLexingUnit::new("struct", TokenType::Struct),
        AlphabeticKeywordLexingUnit::new("import", TokenType::Import),
        AlphabeticKeywordLexingUnit::new("global", TokenType::Global),
        AlphabeticKeywordLexingUnit::new("in", TokenType::In),
        AlphabeticKeywordLexingUnit::new("null", TokenType::Null),
        AlphabeticKeywordLexingUnit::new("repeat", TokenType::Repeat),
        AlphabeticKeywordLexingUnit::new("as", TokenType::As),
        AlphabeticKeywordLexingUnit::new("is", TokenType::Is),
        AlphabeticKeywordLexingUnit::new("from", TokenType::From),

        KeywordLexingUnit::new("&&", TokenType::And),
        KeywordLexingUnit::new("||", TokenType::Or),
        RangeLexingUnit::new("//", "\n", None),
        RangeLexingUnit::new("#!", "\n", None),
        RangeLexingUnit::new("/*", "*/", None),
        //
        KeywordLexingUnit::new("+=", TokenType::AddAs),
        KeywordLexingUnit::new("-=", TokenType::SubAs),
        KeywordLexingUnit::new("*=", TokenType::MulAs),
        KeywordLexingUnit::new("/=", TokenType::DivAs),
        KeywordLexingUnit::new("==", TokenType::Eq),
        KeywordLexingUnit::new("!=", TokenType::NotEq),
        KeywordLexingUnit::new(">>", TokenType::ShiftRight),
        KeywordLexingUnit::new("<<", TokenType::ShiftLeft),
        KeywordLexingUnit::new("::", TokenType::Namespace),
        KeywordLexingUnit::new("->", TokenType::LambdaBegin),
        KeywordLexingUnit::new("!!", TokenType::NullAssert),

        KeywordLexingUnit::new(">", TokenType::Less),
        KeywordLexingUnit::new("<", TokenType::Gt),
        KeywordLexingUnit::new("!", TokenType::Not),
        KeywordLexingUnit::new(".", TokenType::Dot),
        KeywordLexingUnit::new("&", TokenType::BitwiseAnd),
        KeywordLexingUnit::new("|", TokenType::BitwiseOr),
        KeywordLexingUnit::new("^", TokenType::Xor),
        //
        KeywordLexingUnit::new(";", TokenType::Semicolon),
        KeywordLexingUnit::new("=", TokenType::Equals),
        KeywordLexingUnit::new(":", TokenType::Colon),
        KeywordLexingUnit::new(",", TokenType::Comma),
        KeywordLexingUnit::new("?", TokenType::QuestionMark),
        // ops
        KeywordLexingUnit::new("+", TokenType::Plus),
        KeywordLexingUnit::new("%", TokenType::Modulo),
        KeywordLexingUnit::new("-", TokenType::Minus),
        KeywordLexingUnit::new("*", TokenType::Mul),
        KeywordLexingUnit::new("/", TokenType::Div),
        KeywordLexingUnit::new("~", TokenType::BitwiseNot),
        // brackets
        KeywordLexingUnit::new("(", TokenType::ORB),
        KeywordLexingUnit::new(")", TokenType::CRB),
        KeywordLexingUnit::new("[", TokenType::OSB),
        KeywordLexingUnit::new("]", TokenType::CSB),
        KeywordLexingUnit::new("{", TokenType::OCB),
        KeywordLexingUnit::new("}", TokenType::CCB),
        RangeLexingUnit::new("\'", "\'", Some(TokenType::CharLiteral)),
        RangeLexingUnit::new("f\"", "\"", Some(TokenType::FormatStringLiteral)),
        RangeLexingUnit::new("\"", "\"", Some(TokenType::StringLiteral)),
        IdentifierLexingUnit::new(TokenType::Identifier),
    ]
}