use std::collections::HashMap;
use std::error::Error;
use std::fmt::{Display, Formatter};

use crate::vm::{DataType, genFunName, ObjectMeta, VariableMetadata};
use crate::vm::DataType::{Bool, Char, Object};

#[derive(Debug)]
pub(crate) struct TypeNotFound {
    typ: String,
}

impl Display for TypeNotFound {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.typ)
    }
}

impl Error for TypeNotFound {}

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
    CharLiteral(char),
    ArrayLiteral(Vec<Expression>),
    ArrayIndexing(Box<ArrayAccess>)
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

impl Expression {
    pub fn toDataType(
        &self,
        typesMapping: &HashMap<String, (DataType, usize)>,
        functionReturns: &HashMap<String, Option<DataType>>,
    ) -> Result<Option<DataType>, Box<dyn Error>> {
        match self {
            Expression::ArithmeticOp { left, right: _, op: o } => {
                match o {
                    Op::Gt => {
                        return Ok(Some(Bool))
                    }
                    Op::Less => {
                        return Ok(Some(Bool))
                    }
                    Op::Eq => {
                        return Ok(Some(Bool))
                    }
                    _ => {}
                }

                let _leftType = left.toDataType(typesMapping, functionReturns);
                let _rightType = left.toDataType(typesMapping, functionReturns);

                match DataType::Int {
                    DataType::Int => {}
                    DataType::Float => {}
                    DataType::Bool => {}
                    DataType::Array { .. } => {}
                    DataType::Object { .. } => {}
                    DataType::Char => {}
                }
                Ok(Some(DataType::Int))
            }
            Expression::IntLiteral(_) => Ok(Some(DataType::Int)),
            Expression::LongLiteral(_) => {
                // FIXME
                Ok(Some(DataType::Int))
            }
            Expression::FloatLiteral(_) => Ok(Some(DataType::Float)),
            Expression::DoubleLiteral(_) => {
                // FIXME
                Ok(Some(DataType::Float))
            }
            Expression::StringLiteral(_) => Ok(Some(DataType::Object(Box::new(ObjectMeta { name: "String".to_string(), generics: Box::new([]) })))),
            Expression::FunctionCall(f) => {
                let types = f.arguments.iter().filter_map(|x| { x.toDataType(typesMapping, functionReturns).ok()? }).collect::<Vec<DataType>>();
                let enc = genFunName(&f.name, &types);
                match functionReturns.get(&enc) {
                    None => {
                        panic!();
                        println!("{:?}", functionReturns);
                        Err(Box::new(TypeNotFound { typ: enc.clone() }))
                    },
                    Some(v) => Ok(v.clone())
                }
            }
            Expression::Variable(name) => {
                match typesMapping.get(name) {
                    None => Err(Box::new(TypeNotFound { typ: format!("variable {} not found", name) })),
                    Some(v) => {
                        Ok(Some(v.0.clone()))
                    }
                }
            }
            Expression::BoolLiteral(_) => Ok(Some(DataType::Bool)),
            Expression::CharLiteral(_) => Ok(Some(Char)),
            Expression::ArrayLiteral(e) => {
                let t = e.get(0).ok_or("array must have least one value")?.toDataType(typesMapping, functionReturns)?.ok_or("array item must have tyoe")?;
                Ok(Some(Object(Box::new(ObjectMeta { name: "Array".to_string(), generics: Box::new([t]) }))))
            }
            Expression::ArrayIndexing(i) => {
                let e = i.expr.toDataType(typesMapping, functionReturns)?.ok_or("cannot array index none")?;
                match e {
                    Object(o) => {
                        Ok(Some(o.generics.first().ok_or("array must have one generic parameter")?.clone()))
                    }
                    _ => panic!()
                }
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum Statement {
    FunctionExpr(FunctionCall),
    While(While),
    VariableCreate(VariableCreate),
    VariableMod(VariableMod),
    If(If),
    Return(Return),
    ArrayAssign { left: ArrayAccess, right: Expression },
    Continue,
    Break,
    Loop(Vec<Statement>),
}

#[derive(Debug, Clone)]
pub struct VariableMod {
    pub(crate) varName: String,
    pub(crate) modType: ModType,
    pub(crate) expr: Expression,
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