use crate::ast::Statement::StatementExpression;
use crate::bytecodeGen::Body;
use std::collections::HashMap;
use std::error::Error;
use std::fmt::{Display, Formatter};
use std::ops::Index;

use crate::errors::{InvalidOperation, InvalidTypeException, ParserError, TypeNotFound};
use crate::fastAccess::FastAcess;
use crate::lexer::{Location, Token, TokenType};
use crate::utils::genFunName;
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
    NotExpression(Box<Expression>, Location),
    NamespaceAccess(Vec<String>),
    Lambda(Vec<VariableMetadata>, Body, Option<DataType>),
    Callable(Box<Expression>, Vec<Expression>),
    StructInit(String, Vec<(String, Expression)>),
    FieldAccess(Box<Expression>, String),
    TernaryOperator(Box<Expression>, Box<Expression>, Box<Expression>),
    Null,
}

impl Expression {
    pub fn isCallable(&self) -> bool {
        return match self {
            Expression::Variable(..) => true,
            Expression::NamespaceAccess(..) => true,
            Expression::FieldAccess(..) => true,
            _ => false,
        };
    }

    pub fn isAssignable(&self) -> bool {
        return match self {
            Expression::Variable(..) => true,
            Expression::ArrayIndexing(..) => true,
            Expression::NamespaceAccess(..) => true,
            Expression::FieldAccess(..) => true,
            _ => false,
        };
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ArrayAccess {
    pub expr: Expression,
    pub index: Expression,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionCall {
    pub name: String,
    pub arguments: Vec<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    While(WhileS),
    If(If),
    Return(Expression),
    Continue,
    Break,
    Loop(Body),
    NamespaceFunction(Vec<String>, FunctionCall),
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

#[derive(Debug, Clone, PartialEq)]
pub struct Return {
    pub exp: Expression,
}

#[derive(Debug, Clone, PartialEq)]
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
pub enum Node {
    FunctionDef(FunctionDef),
    StructDef(StructDef),
    GlobalVarDef(String, Expression),
    Import(Vec<String>),
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

#[derive(Debug, Clone, PartialEq)]
pub struct WhileS {
    pub exp: Expression,
    pub body: Body,
}

#[derive(Debug, Clone)]
pub struct VariableCreate {
    pub name: String,
    pub init: Option<Expression>,
    pub typeHint: Option<DataType>,
}

#[derive(Debug, Clone)]
pub enum ASTNode {
    Global(Node),
    Statement(Statement),
    Expr(Expression),
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
                panic!();
                Err(ParserError::Unknown(Box::new(InvalidOperation {
                    operation: self.clone(),
                    expected: "Expression".to_string(),
                })))
            },
        }
    }

    pub fn asExprRef(&self) -> Result<&Expression, Box<dyn Error>> {
        match self {
            ASTNode::Expr(e) => Ok(e),
            _ => Err(Box::new(InvalidOperation {
                operation: self.clone(),
                expected: "Expression".to_string(),
            })),
        }
    }

    pub fn asStatement(self) -> Result<Statement, ParserError<TokenType>> {
        let clone = self.clone();
        match self {
            ASTNode::Statement(s) => Ok(s),
            ASTNode::Expr(e) => match e {
                Expression::NamespaceAccess(f) => todo!(),
                Expression::Callable(_, _) => Ok(StatementExpression(e.clone())),
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
