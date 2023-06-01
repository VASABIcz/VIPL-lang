use crate::ast::{ASTNode, Expression};
use crate::lexer::{Location, Token};
use crate::parser::ParsingUnitSearchType;
use crate::vm::dataType::DataType;
use std::convert::Infallible;
use std::error::Error;
use std::fmt;
use std::fmt::{Debug, Display, Formatter};
use std::ops::FromResidual;
use crate::lexingUnits::{Stringable, TokenType};

#[derive(Debug)]
pub struct NoValue {
    pub(crate) msg: String,
}

#[derive(Debug, Clone)]
pub struct InvalidToken<T: Clone + PartialEq + Debug> {
    pub expected: T,
    pub actual: Option<Token<T>>,
}

impl<T: Clone + PartialEq + Debug> Display for InvalidToken<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self.actual {
            None => write!(f, "expected token type {:?}, but got none", self.expected),
            Some(v) => write!(
                f,
                "expected token type {:?}, but got {:?}",
                self.expected, v
            ),
        }
    }
}

impl<T: PartialEq + Clone + Debug> Error for InvalidToken<T> {}

#[derive(Debug, Clone)]
pub struct InvalidCharLiteral<T: Clone + PartialEq> {
    pub token: Token<T>,
}

impl<T: Clone + PartialEq> Display for InvalidCharLiteral<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}

impl<T: PartialEq + Clone + Debug> Error for InvalidCharLiteral<T> {}

impl Display for NoValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.msg)
    }
}

impl Error for NoValue {}

#[derive(Debug)]
pub(crate) struct TypeNotFound {
    pub(crate) typ: String,
}

impl Display for TypeNotFound {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.typ)
    }
}

impl Error for TypeNotFound {}

#[derive(Debug, Clone)]
pub struct UnknownToken {
    pub source: String,
    pub location: Location,
}

impl Display for UnknownToken {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "failed to parse remaining source: \"{}\"",
            self.source.replace("\n", "").escape_debug()
        )
    }
}

impl Error for UnknownToken {}

#[derive(Debug)]
pub struct InvalidTypeException {
    pub(crate) expected: DataType,
    pub(crate) actual: Option<DataType>,
}

impl Display for InvalidTypeException {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self.actual {
            None => {
                write!(f, "expected {:?}, got None", self.expected)
            }
            Some(v) => {
                write!(f, "expected {:?}, got {:?}", self.expected, v)
            }
        }
    }
}

impl Error for InvalidTypeException {}

#[derive(Debug)]
pub struct InvalidOpcode {
    pub(crate) msg: String,
}

impl Display for InvalidOpcode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.msg)
    }
}

impl Error for InvalidOpcode {}

#[derive(Debug)]
pub struct GenericException {
    pub(crate) msg: String,
}

impl Display for GenericException {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.msg)
    }
}

impl Error for GenericException {}

#[derive(Debug)]
pub struct OutOfBoundsException {
    pub(crate) max: isize,
    pub(crate) index: isize,
    pub(crate) msg: String,
}

impl Display for OutOfBoundsException {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "tried to index {} out of bounds index {} bounds 0-{}",
            self.msg, self.index, self.index
        )
    }
}

impl Error for OutOfBoundsException {}

#[derive(Debug)]
pub enum LexerError {
    UnknownToken(UnknownToken),
    NotEnoughCharacters(usize, Location),
    ReachedEOF(Location),
    ExpectedChar(String, Location),
    Unknown(Box<dyn Error>, Option<Location>),
}

impl From<&str> for LexerError {
    fn from(value: &str) -> Self {
        Self::Unknown(value.into(), None)
    }
}

impl Display for LexerError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            LexerError::UnknownToken(v) => write!(f, "{}", v),
            LexerError::NotEnoughCharacters(s, v) => todo!(),
            LexerError::ReachedEOF(v) => todo!(),
            LexerError::Unknown(v, _) => write!(f, "{}", v),
            LexerError::ExpectedChar(v, loc) => write!(f, "{}", v)
        }
    }
}

#[derive(Debug)]
pub enum ParserError<T: PartialEq + Clone + Debug> {
    InvalidToken(InvalidToken<T>),
    NoSuchParsingUnit(NoSuchParsingUnit<T>),
    InvalidCharLiteral(InvalidCharLiteral<T>),
    InvalidOperation(InvalidOperation),
    NoToken,
    Unknown(Box<dyn Error>),
}

impl<T: PartialEq + Clone + Debug> Display for ParserError<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ParserError::InvalidToken(v) => write!(f, "{}", v),
            ParserError::NoSuchParsingUnit(v) => write!(f, "{}", v),
            ParserError::InvalidCharLiteral(v) => write!(f, "{}", v),
            ParserError::Unknown(v) => write!(f, "{}", v),
            ParserError::InvalidOperation(v) => write!(f, "{}", v),
            ParserError::NoToken => write!(f, "NoToken")
        }
    }
}

#[derive(Debug)]
pub enum LoadFileError<T: PartialEq + Clone + Debug> {
    ParserError(ParserError<T>),
    LexerError(LexerError),
}

impl<T: PartialEq + Clone + Debug> Display for LoadFileError<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            LoadFileError::ParserError(v) => write!(f, "{}", v),
            LoadFileError::LexerError(v) => write!(f, "{}", v),
        }
    }
}

impl<T: Clone + Debug + PartialEq> LoadFileError<T> {
    pub fn getDomain(&self) -> &'static str {
        match self {
            LoadFileError::ParserError(_) => "Parser",
            LoadFileError::LexerError(_) => "Lexer",
        }
    }

    pub fn getType(&self) -> &'static str {
        match self {
            LoadFileError::ParserError(v) => match v {
                ParserError::InvalidToken(_) => "InvalidToken",
                ParserError::NoSuchParsingUnit(_) => "NoSuchParsingUnit",
                ParserError::InvalidCharLiteral(_) => "InvalidCharLiteral",
                ParserError::Unknown(_) => "Unknown",
                ParserError::InvalidOperation(_) => "InvalidOperation",
                ParserError::NoToken => "NoToken"
            },
            LoadFileError::LexerError(v) => match v {
                LexerError::UnknownToken(_) => "UnknownToken",
                LexerError::NotEnoughCharacters(_, _) => "NotEnoughCharacters",
                LexerError::ReachedEOF(_) => "ReachedEOF",
                LexerError::Unknown(_, _) => "Unknown",
                LexerError::ExpectedChar(_, _) => "ExpectedChar"
            },
        }
    }

    pub fn getLocation(&self) -> Option<Location> {
        match self {
            LoadFileError::ParserError(v) => match v {
                ParserError::InvalidToken(t) => t.actual.as_ref().map(|it| it.location),
                ParserError::NoSuchParsingUnit(t) => t.token.as_ref().map(|it| it.location),
                ParserError::InvalidCharLiteral(t) => Some(t.token.location),
                ParserError::Unknown(e) => None,
                ParserError::InvalidOperation(v) => None,
                ParserError::NoToken => None
            },
            LoadFileError::LexerError(v) => match v {
                LexerError::UnknownToken(t) => Some(t.location),
                LexerError::NotEnoughCharacters(_, l) => Some(*l),
                LexerError::ReachedEOF(e) => Some(*e),
                LexerError::Unknown(e, l) => *l,
                LexerError::ExpectedChar(_, l) => Some(*l)
            },
        }
    }
}

impl LoadFileError<TokenType> {
    pub fn betterMessage(&self) -> Option<String> {
        match self {
            LoadFileError::ParserError(v) => match v {
                ParserError::InvalidToken(e) => match &e.actual {
                    None => Some(format!(
                        "expected {:?} `{}`, got None",
                        e.expected,
                        e.expected.toStr()
                    )),
                    Some(v) => Some(format!(
                        "expected {:?} `{}`, got {:?} `{}`",
                        e.expected,
                        e.expected.toStr(),
                        v.typ,
                        v.typ.toStr()
                    )),
                },
                _ => None,
            },
            _ => None,
        }
    }

    pub fn printUWU(&self, filePath: &str) {
        let domain = self.getDomain();
        let typ = self.getType();
        let location = self.getLocation();

        let message = match self.betterMessage() {
            Some(v) => v,
            None => format!("{}", self),
        };

        eprintln!();
        eprintln!(
            "\x1b[31merror\x1b[0m[\x1b[35m{}\x1b[0m:\x1b[36m{}\x1b[0m]\x1b[0m: {}",
            domain, typ, message
        );
        if let Some(l) = location {
            eprintln!("  -> {}:{}:{}", filePath, l.row + 1, l.col + 1)
        } else {
            eprintln!("  -> {}", filePath)
        }
    }
}

impl<T: PartialEq + Clone + Debug> From<ParserError<T>> for LoadFileError<T> {
    fn from(value: ParserError<T>) -> Self {
        LoadFileError::ParserError(value)
    }
}

impl<T: PartialEq + Clone + Debug> From<LexerError> for LoadFileError<T> {
    fn from(value: LexerError) -> Self {
        LoadFileError::LexerError(value)
    }
}

#[derive(Debug, Clone)]
pub struct TypeError {
    pub expected: DataType,
    pub actual: DataType,
    pub exp: Option<Expression>,
}

#[derive(Debug, Clone, Copy)]
pub enum SymbolType {
    Function,
    Variable,
    Struct,
    DataType,
    Namespace,
    Global,
}

#[derive(Debug, Clone)]
pub struct SymbolNotFoundE {
    name: String,
    typ: SymbolType,
}

impl SymbolNotFoundE {
    pub fn obj(name: &str) -> Self {
        Self {
            name: name.to_string(),
            typ: SymbolType::Struct,
        }
    }

    pub fn fun(name: &str) -> Self {
        Self {
            name: name.to_string(),
            typ: SymbolType::Function,
        }
    }

    pub fn var(name: &str) -> Self {
        Self {
            name: name.to_string(),
            typ: SymbolType::Variable,
        }
    }

    pub fn namespace(name: &str) -> Self {
        Self {
            name: name.to_string(),
            typ: SymbolType::Namespace,
        }
    }

    pub fn global(name: &str) -> Self {
        Self {
            name: name.to_string(),
            typ: SymbolType::Global,
        }
    }
}

#[derive(Debug, Clone)]
pub enum CodeGenError {
    CannotAssignVoid,
    TypeError(TypeError),
    SymbolNotFound(SymbolNotFoundE),
    UntypedEmptyArray,
    ArrayWithoutGenericParameter,
    UnexpectedVoid,
    ContinueOutsideLoop,
    BreakOutsideLoop,
    LiteralParseError,
    ExpressionIsNotAssignable,
    ExpectedReference,
    UnexpectedAny,
    ExpectedLambda,
    ExpectedCallable,
    VeryBadState,
    ExpectedRawType
}

#[derive(Debug, Clone)]
pub struct NoSuchParsingUnit<T: Debug + PartialEq + Clone> {
    pub typ: ParsingUnitSearchType,
    pub token: Option<Token<T>>,
}

impl<T: Debug + PartialEq + Clone> Display for NoSuchParsingUnit<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "no such {:?} parsing unit to parse token {:?}",
            self.typ, self.token
        )
    }
}

impl<T: Debug + Send + Sync + PartialEq + Clone> Error for NoSuchParsingUnit<T> {}

#[derive(Debug)]
enum CompilerError<T: Debug + Clone + PartialEq> {
    LoadFileError(LoadFileError<T>),
    CodeGenError(CodeGenError),
}

#[derive(Debug)]
pub struct InvalidOperation {
    pub operation: ASTNode,
    pub expected: String,
}

impl Display for InvalidOperation {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "expected {:?} to be {}", self.operation, self.expected)
    }
}

impl Error for InvalidOperation {}
