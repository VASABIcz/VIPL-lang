use std::error::Error;
use std::fmt;
use std::fmt::{Debug, Display, Formatter};

use strum_macros::Display;

use crate::ast::{ASTNode, Expression, Statement};
use crate::lexer::{Location, Token};
use crate::parser::ParsingUnitSearchType;
use crate::utils::{errorBody, errorBody2, errorBodys, getRanges, visualizeRange};
use crate::vm::dataType::DataType;

impl VIPLError for CodeGenError {
    fn getDomain(&self) -> String {
        String::from("Codegen")
    }

    fn getType(&self) -> String {
        self.to_string()
    }

    fn getLocation(&self) -> Option<Location> {
        match self {
            CodeGenError::UnexpectedVoid(a) => {
                a.getLocation().first().map(|it| it.location)
            }
            CodeGenError::TypeError(t) => {
                t.exp.clone().and_then(|it| it.loc.first().cloned()).map(|it| it.location)
            }
            CodeGenError::AssignableTypeError { var, varType, exp, expType } => {
                var.loc.first().map(|it| it.location)
            }
            CodeGenError::ContinueOutsideLoop(s) => {
                s.loc.first().map(|it| it.location)
            }
            CodeGenError::BreakOutsideLoop(s) => {
                s.loc.first().map(|it| it.location)
            }
            _ => None
        }
    }

    fn getSource(&self) -> Option<String> {
        None
    }

    fn getMessage(&self, src: &str) -> Option<String> {
        Some(match self {
            CodeGenError::TypeError(a1) => {
                let mut buf = String::new();

                let loc = match a1.exp.clone() {
                    None => {
                        return None
                    }
                    Some(v) => v.loc
                };

                let locations = loc.iter().map(|it| it.location).collect::<Vec<_>>();
                let row = locations.first().unwrap().row;

                let ranges = getRanges(&loc, row);

                let vis = visualizeRange(&ranges, ' ', '^', 0).trim_end().to_string();

                let a = src.split('\n').enumerate().find(|(i, it)| {
                    *i == row
                }).unwrap();

                buf += &format!("expected {} got {}", a1.expected.toString(), a1.actual.toString());

                buf
            },
            CodeGenError::SymbolNotFound(b) => format!("symbol \"{}\" of type {} not found", b.name, b.typ),
            CodeGenError::UnexpectedVoid(v) => {
                let mut buf = String::new();

                let locations = v.getLocation().iter().map(|it| it.location).collect::<Vec<_>>();
                let row = locations.first().unwrap().row;


                let a = src.split('\n').enumerate().find(|(i, it)| {
                    *i == row
                }).unwrap();

                let ranges = getRanges(v.getLocation(), row);

                let vis = visualizeRange(&ranges, ' ', '^', 0).trim_end().to_string();

                buf += &format!("unexpected void");

                buf
            },
            CodeGenError::AssignableTypeError{ var, varType, exp, expType } => {
                let mut buf = String::new();

                let locations = var.loc.iter().map(|it| it.location).collect::<Vec<_>>();
                let row = var.loc.first().unwrap();

                let locations1 = exp.loc.iter().map(|it| it.location).collect::<Vec<_>>();
                let row1 = exp.loc.first().unwrap();


                let a = src.split('\n').enumerate().find(|(i, it)| {
                    *i == row.location.row
                }).unwrap();

                let ranges = getRanges(&var.loc, row.location.row);
                let ranges1 = getRanges(&exp.loc, row1.location.row);

                let vis = visualizeRange(&ranges, ' ', '^', 0).trim_end().to_string();
                let vis1 = visualizeRange(&ranges1, ' ', '^', 0).trim_end().to_string();

                buf += &format!("cannot assign {} to variable of type {}", expType.toString(), varType.toString());

                buf
            }
            _ => {
                return None;
            }
        })
    }

    fn getBody(&self, src: &str) -> Option<String> {
        Some(match self {
            CodeGenError::TypeError(a1) => {
                let a = a1.exp.clone().map(|it| errorBody(src, &[(&it, Some(&format!("this expression is of type {}", a1.actual.toString())))]));

                match a {
                    None => {
                        return None
                    }
                    Some(v) => v
                }
            },
            CodeGenError::SymbolNotFound(b) => format!("symbol \"{}\" of type {} not found", b.name, b.typ),
            CodeGenError::UnexpectedVoid(v) => {
                errorBody(src, &[(&v.clone().asExpr().unwrap(), Some("this expression evaluates to void"))])
            },
            CodeGenError::AssignableTypeError{ var, varType, exp, expType } => {
                errorBody(src, &[(exp, Some(&format!("this expression evaluates to {}", expType.toString()))), (var, Some(&format!("variable is type of {}", varType.toString())))])
            }
            CodeGenError::ContinueOutsideLoop(s) => {
                errorBody2(src, &[(s, Some("this continue is outside of loop"))])
            }
            CodeGenError::BreakOutsideLoop(s) => {
                errorBody2(src, &[(s, Some("this break is outside of loop"))])
            }
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
    fn getMessage(&self, src: &str) -> Option<String>;
    fn getBody(&self, src: &str) -> Option<String>;

    fn printUWU(&self, src: &str, path: Option<&str>) {
        let domain = self.getDomain();
        let typ = self.getType();
        let location = self.getLocation();
        let src2 = path;
        let body = self.getBody(src);

        let message = match self.getMessage(src) {
            None => format!("{:?}", self),
            Some(v) => v
        };

        eprintln!(
            "\x1b[31merror\x1b[0m[\x1b[35m{}\x1b[0m:\x1b[36m{}\x1b[0m]\x1b[0m: {}",
            domain, typ, message
        );

        match src2 {
            None => {}
            Some(filePath) => {
                if let Some(l) = location {
                    eprintln!("  +-> {}:{}:{}", filePath, l.row + 1, l.col + 1)
                } else {
                    eprintln!("  +-> {}", filePath)
                }
            }
        }
        if let Some(v) = body {
            eprintln!("{}", v)
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

#[derive(Debug, Display)]
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

#[derive(Debug, Display)]
pub enum ParserError<T: PartialEq + Clone + Debug> {
    InvalidToken(InvalidToken<T>),
    NoSuchParsingUnit(NoSuchParsingUnit<T>),
    InvalidCharLiteral(Token<T>),
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
            LoadFileError::LexerError(v) => write!(f, "{:?}", v),
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
            ParserError::InvalidCharLiteral(t) => Some(t.location),
            ParserError::NoSuchParsingUnit(t) => t.token.clone().map(|it| it.location),
            _ => None
        }
    }

    fn getSource(&self) -> Option<String> {
        None
    }

    fn getMessage(&self, src: &str) -> Option<String> {
        let a = match self {
            ParserError::InvalidToken(a) => format!("invalid token, expected {:?} to be {:?}",a.actual, a.expected),
            ParserError::NoSuchParsingUnit(v) => format!("no such {:?} parsing unit to parse token {:?}", v.typ, v.token),
            ParserError::InvalidCharLiteral(l) => format!("{} is invalid char literal", l.str),
            ParserError::InvalidOperation(v) => format!("expected {:?} to be {}", v.operation, v.expected),
            ParserError::NoToken => format!(""),
            ParserError::Unknown(_) => format!("")
        };

        Some(a)
    }

    fn getBody(&self, src: &str) -> Option<String> {
        match self {
            ParserError::InvalidCharLiteral(t) => {
                Some(errorBodys(src, &[(vec![(t.location.toRange(t.str.len()))], t.location.row, Some("this is invalid char literal"))]))
            }
            _ => None
        }
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

    fn getMessage(&self, src: &str) -> Option<String> {
        let a = match self {
            LexerError::UnknownToken(v) => format!("failed to parse remaining source: \"{}\"", v.source),
            LexerError::NotEnoughCharacters(a, b) => "not enough characters to parse next token".to_string(),
            LexerError::ReachedEOF(_) => "unexpectedly reached end of line".to_string(),
            LexerError::ExpectedChar(c, a) => format!("expected \"{}\"", c),
            LexerError::Unknown(a, b) => format!("{}", a)
        };

        Some(a)
    }

    fn getBody(&self, src: &str) -> Option<String> {
        match self {
            LexerError::UnknownToken(t) => {
                Some(errorBodys(src, &[(vec![t.location.toRange(t.source.len())], t.location.row, Some("unknown token"))]))
            }
            _ => None
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
    expected: DataType,
    actual: DataType,
    exp: Option<Expression>,
}

impl TypeError {
    pub fn new(expected: DataType, actual: DataType, exp: Expression) -> Self {
        Self {
            expected: expected,
            actual: actual,
            exp: Some(exp),
        }
    }

    pub fn newNone(expected: DataType, actual: DataType) -> Self {
        Self {
            expected: expected,
            actual: actual,
            exp: None,
        }
    }
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
    AssignableTypeError{var: Expression, varType: DataType, exp: Expression, expType: DataType},
    SymbolNotFound(SymbolNotFoundE),
    UntypedEmptyArray,
    ArrayWithoutGenericParameter,
    UnexpectedVoid(ASTNode),
    ContinueOutsideLoop(Statement),
    BreakOutsideLoop(Statement),
    LiteralParseError,
    ExpressionIsNotAssignable,
    ExpectedReference,
    UnexpectedAny,
    ExpectedLambda,
    ExpectedCallable,
    VeryBadState,
    ExpectedRawType,
    InvalidReturns
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
