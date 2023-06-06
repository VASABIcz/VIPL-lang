use crate::ast::RawStatement::StatementExpression;
use crate::bytecodeGen::Body;
use std::collections::HashMap;
use std::error::Error;
use std::fmt::{Display, Formatter};
use std::ops::{Index, Range};

use crate::errors::{InvalidOperation, InvalidTypeException, ParserError, TypeNotFound};
use crate::fastAccess::FastAcess;
use crate::lexer::{Location, Token};
use crate::lexingUnits::TokenType;
use crate::utils::{genFunName, getRanges};
use crate::vm::dataType::DataType::{Bool, Char, Object};
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
    And,
    Or,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ArithmeticOp {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Debug, Clone)]
pub enum RawExpression {
    BinaryOperation {
        left: Box<Expression>,
        right: Box<Expression>,
        op: BinaryOp,
    },
    IntLiteral(String),
    FloatLiteral(String),
    StringLiteral(String),
    BoolLiteral(bool),
    Variable(String),
    CharLiteral(char),
    ArrayLiteral(Vec<Expression>),
    ArrayIndexing(Box<ArrayAccess>),
    NotExpression(Box<Expression>, Location),
    NamespaceAccess(Vec<String>),
    Lambda(Vec<VariableMetadata>, Body, Option<DataType>),
    Callable(Box<Expression>, Vec<Expression>),
    StructInit(String, Vec<(String, Expression)>),
    FieldAccess(Box<Expression>, String),
    TernaryOperator(Box<Expression>, Box<Expression>, Box<Expression>),
    Null,
    TypeCast(Box<Expression>, DataType)
}

impl RawExpression {
    pub fn isCallable(&self) -> bool {
        return match self {
            RawExpression::Variable(..) => true,
            RawExpression::NamespaceAccess(..) => true,
            RawExpression::FieldAccess(..) => true,
            _ => false,
        };
    }

    pub fn isAssignable(&self) -> bool {
        return match self {
            RawExpression::Variable(..) => true,
            RawExpression::ArrayIndexing(..) => true,
            RawExpression::NamespaceAccess(..) => true,
            RawExpression::FieldAccess(..) => true,
            _ => false,
        };
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

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
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

impl Into<StructMeta> for StructDef {
    fn into(self) -> StructMeta {
        let mut fields = FastAcess::default();

        for (k, v) in self.fields {
            fields.insert(k.clone(), VariableMetadata::n(&k, v));
        }

        StructMeta::new(self.name, fields)
    }
}

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
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
    pub fn getRanges(&self, row: usize) -> Vec<Range<usize>> {
        getRanges(&self.loc, row)
    }

    pub fn getRow(&self) -> usize {
        self.loc.first().unwrap().location.row
    }
}

#[derive(Debug, Clone)]
pub struct Statement {
    pub exp: RawStatement,
    pub loc: Vec<Token<TokenType>>
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
                    operation: self.clone(),
                    expected: "Expression".to_string(),
                }))
            },
        }
    }

    pub fn asExprRef(&self) -> Result<&Expression, ParserError<TokenType>> {
        match self {
            ASTNode::Expr(e) => Ok(e),
            _ => Err(ParserError::InvalidOperation(InvalidOperation {
                operation: self.clone(),
                expected: "Expression".to_string(),
            })),
        }
    }

    pub fn asStatement(self) -> Result<Statement, ParserError<TokenType>> {
        let clone = self.clone();
        match self {
            ASTNode::Statement(s) => Ok(s),
            ASTNode::Expr(e) => match e.exp {
                RawExpression::NamespaceAccess(f) => todo!(),
                RawExpression::Callable(_, _) => Ok(Statement{ exp: StatementExpression(e.clone()), loc: e.loc.clone() }),
                _ => Err(ParserError::InvalidOperation(InvalidOperation {
                    operation: clone,
                    expected: String::from("Statement"),
                })),
            },
            _ => Err(ParserError::InvalidOperation(InvalidOperation {
                operation: clone,
                expected: String::from("Statement"),
            })),
        }
    }
}
