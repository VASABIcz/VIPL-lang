use std::collections::HashMap;
use std::error::Error;
use std::fmt::{Display, Formatter};
use std::ops::Index;

use crate::errors::{InvalidTypeException, TypeNotFound};
use crate::utils::genFunName;
use crate::vm::variableMetadata::VariableMetadata;
use crate::vm::dataType::{DataType, Generic, ObjectMeta};
use crate::vm::dataType::DataType::{Bool, Char, Object};
use crate::vm::dataType::Generic::Any;
use crate::vm::myStr::MyStr;
use crate::vm::namespace::StructMeta;

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
    Div
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    BinaryOperation {
        left: Box<Expression>,
        right: Box<Expression>,
        op: BinaryOp,
    },
    IntLiteral(String),
    LongLiteral(String),
    FloatLiteral(String),
    DoubleLiteral(String),
    StringLiteral(String),
    BoolLiteral(bool),
    Variable(String),
    CharLiteral(char),
    ArrayLiteral(Vec<Expression>),
    ArrayIndexing(Box<ArrayAccess>),
    NotExpression(Box<Expression>),
    NamespaceAccess(Vec<String>),
    Lambda(Vec<(String, Option<DataType>)>, Vec<Statement>),
    Callable(Box<Expression>, Vec<Expression>),
    StructInit(String, Vec<(String, Expression)>),
    FieldAccess(Box<Expression>, String),
    Null
}

impl Expression {
    pub fn isCallable(&self) -> bool {
        return match self {
            Expression::Variable(..) => true,
            Expression::NamespaceAccess(..) => true,
            _ => false
        }
    }

    pub fn isAssignable(&self) -> bool {
        return match self {
            Expression::Variable(..) => true,
            Expression::ArrayIndexing(..) => true,
            Expression::NamespaceAccess(..) => true,
            Expression::FieldAccess(..) => true,
            _ => false
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ArrayAccess {
    pub expr: Expression,
    pub index: Expression,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionCall {
    pub name: MyStr,
    pub arguments: Vec<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    While(While),
    If(If),
    Return(Return),
    Continue,
    Break,
    Loop(Vec<Statement>),
    NamespaceFunction(Vec<String>, FunctionCall),
    StatementExpression(Expression),
    Assignable(Expression, Expression, Option<ArithmeticOp>),
    ForLoop(String, Expression, Vec<Statement>)
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

#[derive(Debug, Clone, PartialEq)]
pub struct Return {
    pub exp: Expression,
}

#[derive(Debug, Clone, PartialEq)]
pub struct If {
    pub condition: Expression,
    pub body: Vec<Statement>,
    pub elseBody: Option<Vec<Statement>>,
}

#[derive(Debug, Clone)]
pub struct StructDef {
    pub name: String,
    pub fields: HashMap<String, DataType>,
}

impl Into<StructMeta> for StructDef {
    fn into(self) -> StructMeta {
        let mut fieldsLookup = HashMap::new();
        let mut fields = vec![];

        for (k, v) in self.fields {
            fields.push(VariableMetadata{ name: k.clone().into(), typ: v });
            fieldsLookup.insert(k, fields.len()-1);
        }

        StructMeta {
            name: self.name,
            fieldsLookup,
            fields,
        }
    }
}

#[derive(Debug, Clone)]
pub enum Node {
    FunctionDef(FunctionDef),
    StructDef(StructDef),
    GlobalVarDef(String, Expression),
    Import(Vec<String>)
}

#[derive(Debug, Clone)]
pub struct FunctionDef {
    pub name: String,
    pub localsMeta: Vec<VariableMetadata>,
    pub argsCount: usize,
    pub body: Vec<Statement>,
    pub returnType: Option<DataType>,
    pub isNative: bool,
}

impl FunctionDef {
    pub fn genName(&self) -> String {
        genFunName(&self.name, &self.localsMeta.iter().map(|it| {
            it.typ.clone()
        }).collect::<Vec<_>>())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct While {
    pub exp: Expression,
    pub body: Vec<Statement>,
}

#[derive(Debug, Clone)]
pub struct VariableCreate {
    pub name: String,
    pub init: Option<Expression>,
    pub typeHint: Option<DataType>,
}