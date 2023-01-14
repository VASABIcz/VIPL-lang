use crate::DataType::Float;
use crate::{DataType, VariableMetadata};
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub enum Op {
    Add,
    Sub,
    Mul,
    Div,
    Gt,
    Less,
    Eq,
}

#[derive(Debug, Clone)]
pub enum Expression {
    ArithmeticOp {
        left: Box<Expression>,
        right: Box<Expression>,
        op: Op,
    },
    IntLiteral(String),
    LongLiteral(String),
    FloatLiteral(String),
    DoubleLiteral(String),
    StringLiteral(String),
    BoolLiteral(bool),
    FunctionCall(FunctionCall),
    Variable(String),
}

#[derive(Debug, Clone)]
pub struct FunctionCall {
    pub name: String,
    pub arguments: Vec<Expression>,
}

impl Expression {
    pub fn toDataType(
        &self,
        typesMapping: &HashMap<String, (DataType, usize)>,
        functionReturns: &HashMap<String, Option<DataType>>,
    ) -> Option<DataType> {
        match self {
            Expression::ArithmeticOp { left, right, op } => {
                let leftType = left.toDataType(typesMapping, functionReturns);
                let rightType = left.toDataType(typesMapping, functionReturns);

                match DataType::Int {
                    DataType::Int => {}
                    DataType::Float => {}
                    DataType::Bool => {}
                    DataType::Array { .. } => {}
                    DataType::Object { .. } => {}
                }
                Some(DataType::Int)
            }
            Expression::IntLiteral(_) => Some(DataType::Int),
            Expression::LongLiteral(_) => {
                // FIXME
                Some(DataType::Int)
            }
            Expression::FloatLiteral(_) => Some(DataType::Float),
            Expression::DoubleLiteral(_) => {
                // FIXME
                Some(DataType::Float)
            }
            Expression::StringLiteral(_) => Some(DataType::Object {
                name: String::from("String"),
            }),
            Expression::FunctionCall(f) => functionReturns.get(&f.name).unwrap().clone(),
            Expression::Variable(name) => Some(typesMapping[name].0.clone()),
            Expression::BoolLiteral(_) => Some(DataType::Bool),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Statement {
    FunctionExpr(FunctionCall),
    While(While),
    VariableCreate(VariableCreate),
    If(If),
}

#[derive(Debug, Clone)]
pub struct If {
    pub condition: Expression,
    pub body: Vec<Statement>,
    pub elseBody: Option<Vec<Statement>>,
}

#[derive(Debug, Clone)]
pub enum Node {
    FunctionDef(FunctionDef),
}

#[derive(Debug, Clone)]
pub struct FunctionDef {
    pub name: String,
    pub args: Vec<VariableMetadata>,
    pub argCount: usize,
    pub body: Vec<Statement>,
    pub returnType: Option<DataType>,
}

#[derive(Debug, Clone)]
pub struct While {
    pub exp: Expression,
    pub body: Vec<Statement>,
}

#[derive(Debug, Clone)]
pub struct VariableCreate {
    pub name: String,
    pub init: Option<Expression>,
}
