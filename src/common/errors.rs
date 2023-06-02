use crate::ast::{ASTNode, Expression};
use crate::lexer::{Location, Token};
use crate::parser::ParsingUnitSearchType;
use crate::vm::dataType::DataType;
use std::convert::Infallible;
use std::error::Error;
use std::fmt;
use std::fmt::{Debug, Display, Formatter};
use strum_macros::{Display, IntoStaticStr};
use crate::lexingUnits::{Stringable, TokenType};

impl VIPLError for CodeGenError {
    fn getDomain(&self) -> String {
        String::from("Codegen")
    }

    fn getType(&self) -> String {
        self.to_string()
    }

    fn getLocation(&self) -> Option<Location> {
        None
    }

    fn getSource(&self) -> Option<String> {
        None
    }

    fn getMessage(&self) -> Option<String> {
        Some(match self {
            CodeGenError::TypeError(a) => format!("expected {} got {}", a.expected.toString(), a.actual.toString()),
            CodeGenError::SymbolNotFound(b) => format!("symbol \"{}\" of type {} not found", b.name, b.typ),
            _ => {
                return None;
            }
        })
    }
}

pub trait VIPLError: Debug {
    fn getDomain(&self) -> String;
    fn getType(&self) -> String;
    fn getLocation(&self) -> Option<Location>;
    fn getSource(&self) -> Option<String>;
    fn getMessage(&self) -> Option<String>;

    fn printUWU(&self) {
        let domain = self.getDomain();
        let typ = self.getType();
        let location = self.getLocation();
        let src = self.getSource();

        let message = match self.getMessage() {
            None => format!("{:?}", self),
            Some(v) => v
        };

        eprintln!(
            "\x1b[31merror\x1b[0m[\x1b[35m{}\x1b[0m:\x1b[36m{}\x1b[0m]\x1b[0m: {}",
            domain, typ, message
        );

        match src {
            None => {}
            Some(filePath) => {
                if let Some(l) = location {
                    eprintln!("  -> {}:{}:{}", filePath, l.row + 1, l.col + 1)
                } else {
                    eprintln!("  -> {}", filePath)
                }
            }
        }
    }
}

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
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "failed to parse remaining source: \"{}\"",
            self.source.replace('\n', "").escape_debug()
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

#[derive(Debug, Display)]
pub enum ParserError<T: PartialEq + Clone + Debug> {
    InvalidToken(InvalidToken<T>),
    NoSuchParsingUnit(NoSuchParsingUnit<T>),
    InvalidCharLiteral(InvalidCharLiteral<T>),
    InvalidOperation(InvalidOperation),
    NoToken,
    Unknown(Box<dyn Error>),
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

impl<T: Debug + Clone + PartialEq> VIPLError for ParserError<T> {
    fn getDomain(&self) -> String {
        String::from("Parser")
    }

    fn getType(&self) -> String {
        self.to_string()
    }

    fn getLocation(&self) -> Option<Location> {
        match self {
            ParserError::InvalidToken(a) => a.actual.clone().map(|it| it.location),
            _ => None
        }
    }

    fn getSource(&self) -> Option<String> {
        None
    }

    fn getMessage(&self) -> Option<String> {
        let a = match self {
            ParserError::InvalidToken(a) => format!("invalid token, expected {:?} to be {:?}",a.actual, a.expected),
            ParserError::NoSuchParsingUnit(v) => format!("no such {:?} parsing unit to parse token {:?}", v.typ, v.token),
            ParserError::InvalidCharLiteral(l) => format!("\'{}\' is invalid char literal", l.token.str),
            ParserError::InvalidOperation(v) => format!("expected {:?} to be {}", v.operation, v.expected),
            ParserError::NoToken => format!(""),
            ParserError::Unknown(_) => format!("")
        };

        Some(a)
    }
}

impl VIPLError for LexerError {
    fn getDomain(&self) -> String {
        String::from("Lexer")
    }

    fn getType(&self) -> String {
        self.to_string()
    }

    fn getLocation(&self) -> Option<Location> {
        match self {
            LexerError::UnknownToken(a) => Some(a.location),
            LexerError::NotEnoughCharacters(a, b) => Some(*b),
            LexerError::ReachedEOF(a) => Some(*a),
            LexerError::ExpectedChar(a, b) => Some(*b),
            LexerError::Unknown(a, b) => *b
        }
    }

    fn getSource(&self) -> Option<String> {
        None
    }

    fn getMessage(&self) -> Option<String> {
        let a = match self {
            LexerError::UnknownToken(v) => format!("failed to parse remaining source: \"{}\"", v),
            LexerError::NotEnoughCharacters(a, b) => "not enough characters to parse next token".to_string(),
            LexerError::ReachedEOF(_) => "unexpectedly reached end of line".to_string(),
            LexerError::ExpectedChar(c, a) => format!("expected \"{}\"", c),
            LexerError::Unknown(a, b) => format!("{}", a)
        };

        Some(a)
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

#[derive(Debug, Clone, Copy, Display)]
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

#[derive(Debug, Clone, Display)]
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
