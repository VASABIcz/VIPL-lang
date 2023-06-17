use crate::ast::RawStatement::StatementExpression;
use crate::bytecodeGen::Body;
use std::collections::HashMap;
use std::error::Error;
use std::fmt::{Display, Formatter};
use std::ops::{Index, Range};
use strum_macros::Display;

use crate::errors::{InvalidOperation, InvalidTypeException, ParserError, TypeNotFound};
use crate::fastAccess::FastAccess;
use crate::lexer::{Location, Token};
use crate::lexingUnits::TokenType;
use crate::utils::{genFunName, getRanges};
use crate::vm::dataType::DataType::{Bool, Char, Reference};
use crate::vm::dataType::Generic::Any;
use crate::vm::dataType::{DataType, Generic, ObjectMeta};
use crate::vm::namespace::StructMeta;
use crate::vm::variableMetadata::VariableMetadata;

#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Gt,
    Less,
    Eq,
    NotEq,
    And,
    Or,
    Modulo,
    ShiftLeft,
    ShiftRight,
    BitwiseOr,
    BitwiseAnd,
    Xor
}

#[derive(Debug, Clone, PartialEq)]
pub enum ArithmeticOp {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Debug, Display, Clone)]
pub enum RawExpression {
    BinaryOperation {
        left: Box<Expression>,
        right: Box<Expression>,
        op: BinaryOp,
    },
    IntLiteral(String),
    FloatLiteral(String),
    StringLiteral(String),
    FormatStringLiteral(String),
    BoolLiteral(bool),
    Variable(String),
    CharLiteral(char),
    ArrayLiteral(Vec<Expression>),
    ArrayIndexing(Box<ArrayAccess>),
    NotExpression(Box<Expression>, Location),
    NamespaceAccess(Vec<String>),
    Lambda(Vec<VariableMetadata>, Body, Option<DataType>),
    Callable(Box<Expression>, Vec<Expression>),
    StructInit(Vec<String>, Vec<(String, Expression)>),
    FieldAccess(Box<Expression>, String),
    TernaryOperator(Box<Expression>, Box<Expression>, Box<Expression>),
    Null,
    TypeCast(Box<Expression>, DataType),
    TypeCheck(Box<Expression>, DataType),
    Negate(Box<Expression>),
    BitwiseNot(Box<Expression>),
    NullAssert(Box<Expression>)
}

impl RawExpression {
    pub fn isCallable(&self) -> bool {
        return matches!(self, RawExpression::Variable(..) | RawExpression::NamespaceAccess(..) | RawExpression::FieldAccess(..));
    }

    pub fn isAssignable(&self) -> bool {
        return matches!(self, RawExpression::Variable(..) | RawExpression::ArrayIndexing(..) | RawExpression::NamespaceAccess(..) | RawExpression::FieldAccess(..));
    }

    pub fn isConstructable(&self) -> bool {
        return matches!(self, RawExpression::Variable(..) | RawExpression::NamespaceAccess(..));
    }
}

#[derive(Debug, Clone)]
pub struct ArrayAccess {
    pub expr: Expression,
    pub index: Expression,
}

#[derive(Debug, Clone)]
pub struct FunctionCall {
    pub name: String,
    pub arguments: Vec<Expression>,
}

#[derive(Debug, Display, Clone)]
pub enum RawStatement {
    While(WhileS),
    If(If),
    Return(Expression),
    Continue,
    Break,
    Loop(Body),
    StatementExpression(Expression),
    Assignable(Expression, Expression, Option<ArithmeticOp>),
    ForLoop(String, Expression, Body),
    Repeat(String, usize, Body),
}

#[derive(Debug)]
pub struct VariableModd {
    pub varName: String,
    pub modType: ModType,
    pub expr: Expression,
}

#[derive(Debug, Clone)]
pub enum ModType {
    Add,
    Sub,
    Div,
    Mul,
}

#[derive(Debug)]
pub struct Return {
    pub exp: Expression,
}

#[derive(Debug, Clone)]
pub struct If {
    pub condition: Expression,
    pub body: Body,
    pub elseBody: Option<Body>,
    pub elseIfs: Vec<(Expression, Body)>,
}

#[derive(Debug, Clone)]
pub struct StructDef {
    pub name: String,
    pub fields: HashMap<String, DataType>,
}

impl From<StructDef> for StructMeta {
    fn from(v: StructDef) -> Self {
        let mut fields = FastAccess::default();

        for (k, v) in v.fields {
            fields.insert(k.clone(), VariableMetadata::n(&k, v));
        }

        StructMeta::new(v.name, fields)
    }
}

#[derive(Debug, Display, Clone)]
pub enum RawNode {
    FunctionDef(FunctionDef),
    StructDef(StructDef),
    GlobalVarDef(String, Expression),
    NamespaceImport(Vec<String>, Option<String>),
    Import(Vec<String>, Vec<(String, Option<String>)>),
}

#[derive(Debug, Clone)]
pub struct FunctionDef {
    pub name: String,
    pub localsMeta: Vec<VariableMetadata>,
    pub argsCount: usize,
    pub body: Body,
    pub returnType: Option<DataType>,
    pub isNative: bool,
    pub isOneLine: bool,
}

impl FunctionDef {
    pub fn genName(&self) -> String {
        genFunName(
            &self.name,
            &self
                .localsMeta
                .iter()
                .map(|it| it.typ.clone())
                .collect::<Vec<_>>(),
        )
    }
}

#[derive(Debug, Clone)]
pub struct WhileS {
    pub exp: Expression,
    pub body: Body,
}

#[derive(Debug)]
pub struct VariableCreate {
    pub name: String,
    pub init: Option<Expression>,
    pub typeHint: Option<TokenType>,
}

#[derive(Debug, Clone)]
pub struct Expression {
    pub exp: RawExpression,
    pub loc: Vec<Token<TokenType>>
}

impl Expression {
    pub fn isNull(&self) -> bool {
        matches!(self.exp,  RawExpression::Null)
    }

    pub fn getRanges(&self, row: usize) -> Vec<Range<usize>> {
        getRanges(&self.loc, row)
    }

    pub fn getRow(&self) -> usize {
        self.loc.first().unwrap().location.row
    }

    pub fn getIdentifier(self) -> Result<Vec<String>, ParserError<TokenType>> {
        match self.exp {
            RawExpression::Variable(a) => Ok(vec![a]),
            RawExpression::NamespaceAccess(a) => Ok(a),
            _ => Err(ParserError::Unknown("getIdentifier".into()))
        }
    }
}

#[derive(Debug, Clone)]
pub struct Statement {
    pub exp: RawStatement,
    pub loc: Vec<Token<TokenType>>
}

impl Statement {
    pub fn getRanges(&self, row: usize) -> Vec<Range<usize>> {
        getRanges(&self.loc, row)
    }

    pub fn getRow(&self) -> usize {
        self.loc.first().unwrap().location.row
    }
}

#[derive(Debug, Clone)]
pub struct Node {
    pub exp: RawNode,
    pub loc: Vec<Token<TokenType>>
}

#[derive(Debug, Clone)]
pub enum ASTNode {
    Global(Node),
    Statement(Statement),
    Expr(Expression),
}

impl ASTNode {
    pub fn getLocation(&self) -> &Vec<Token<TokenType>> {
        match self {
            ASTNode::Global(v) => &v.loc,
            ASTNode::Statement(v) => &v.loc,
            ASTNode::Expr(v) => &v.loc
        }
    }
}

impl ASTNode {
    pub fn isExpr(&self) -> bool {
        match self {
            ASTNode::Expr(_) => true,
            _ => false,
        }
    }

    pub fn isStatement(&self) -> bool {
        match self {
            ASTNode::Statement(_) => true,
            _ => false,
        }
    }

    pub fn isGlobal(&self) -> bool {
        match self {
            ASTNode::Global(_) => true,
            _ => false,
        }
    }

    pub fn asExpr(self) -> Result<Expression, ParserError<TokenType>> {
        match self {
            ASTNode::Expr(e) => Ok(e),
            _ => {
                Err(ParserError::InvalidOperation(InvalidOperation {
                    operation: self,
                    expected: "Expression".to_string(),
                }))
            },
        }
    }

    pub fn asStatement(self) -> Result<Statement, ParserError<TokenType>> {
        match self {
            ASTNode::Statement(s) => Ok(s),
            ASTNode::Expr(e) => match e.exp {
                RawExpression::NamespaceAccess(f) => todo!(),
                RawExpression::Callable(_, _) => {
                    let loc = e.loc.clone();
                    Ok(Statement{ exp: StatementExpression(e), loc })
                },
                _ => Err(ParserError::InvalidOperation(InvalidOperation {
                    operation: ASTNode::Expr(e),
                    expected: String::from("Statement"),
                })),
            },
            _ => Err(ParserError::InvalidOperation(InvalidOperation {
                operation: self,
                expected: String::from("Statement"),
            })),
        }
    }
}
