use std::collections::HashMap;
use crate::{DataType, VariableMetadata};
use crate::DataType::Float;

#[derive(Debug, Clone)]
pub enum Op {
    Add,
    Sub,
    Mul,
    Div,
    Gt,
    Less,
    Eq
}

#[derive(Debug)]
pub enum Expression {
    ArithmeticOp {
        left: Box<Expression>,
        right: Box<Expression>,
        op: Op
    },
    IntLiteral(String),
    LongLiteral(String),
    FloatLiteral(String),
    DoubleLiteral(String),
    StringLiteral(String),
    BoolLiteral(bool),
    FunctionCall(FunctionCall),
    Variable(String)
}

#[derive(Debug)]
pub struct FunctionCall {
    pub name: String,
    pub arguments: Vec<Expression>
}

impl Expression {
    fn toDataType(&self, typesMapping: &HashMap<String, DataType>, functionCache: &HashMap<String, Node>) -> Option<DataType> {
        match self {
            Expression::ArithmeticOp {
                left,
                right,
                op
            } => {
                let leftType = left.toDataType(typesMapping, functionCache);
                let rightType = left.toDataType(typesMapping, functionCache);

                match DataType::Int {
                    DataType::Int => {}
                    DataType::Float => {
                    }
                    DataType::Bool => {}
                    DataType::Array { .. } => {}
                    DataType::Object { .. } => {}
                }
                Some(DataType::Float)
            }
            Expression::IntLiteral(_) => {
                Some(DataType::Int)
            }
            Expression::LongLiteral(_) => {
                // FIXME
                Some(DataType::Int)
            }
            Expression::FloatLiteral(_) => {
                Some(DataType::Float)
            }
            Expression::DoubleLiteral(_) => {
                // FIXME
                Some(DataType::Float)
            }
            Expression::StringLiteral(_) => {
                Some(DataType::Object { name: String::from("String") })
            }
            Expression::FunctionCall(f) => {
                match functionCache.get(&f.name).unwrap() {
                    Node::FunctionDef(r) => {
                        r.returnType.clone()
                    }
                    _ => panic!()
                }
            }
            Expression::Variable(name) => {
                Some(typesMapping[name].clone())
            }
            Expression::BoolLiteral(_) => {
                Some(DataType::Bool)
            }
        }
    }
}

#[derive(Debug)]
pub enum Statement {
    FunctionExpr(FunctionCall),
    While(While),
    VariableCreate(VariableCreate)
}

#[derive(Debug)]
pub enum Node {
    FunctionDef(FunctionDef),
}

#[derive(Debug)]
pub struct FunctionDef {
    pub name: String,
    pub args: Vec<VariableMetadata>,
    pub argCount: usize,
    pub body: Vec<Statement>,
    pub returnType: Option<DataType>
}

#[derive(Debug)]
pub struct While {
    pub exp: Expression,
    pub body: Vec<Statement>
}

#[derive(Debug)]
pub struct VariableCreate {
    pub name: String,
    pub init: Option<Expression>
}