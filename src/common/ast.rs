use std::collections::HashMap;
use std::error::Error;
use std::fmt::{Display, Formatter};
use std::ops::Index;

use crate::bytecodeChecker::InvalidTypeException;
use crate::vm::{DataType, Func, Generic, genFunName, MyStr, ObjectMeta, VariableMetadata};
use crate::vm::DataType::{Bool, Char, Object};
use crate::vm::Generic::Any;

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

#[derive(Debug, Clone, PartialEq)]
pub enum Op {
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
    ArrayIndexing(Box<ArrayAccess>),
    NotExpression(Box<Expression>),
    NamespaceAccess(Vec<String>, Box<Expression>)
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

impl Expression {
    pub fn toDataType(
        &self,
        typesMapping: &HashMap<MyStr, (DataType, usize)>,
        functionReturns: &HashMap<MyStr, Option<DataType>>,
        typeHint: Option<DataType>,
    ) -> Result<Option<DataType>, Box<dyn Error>> {
        match self {
            Expression::ArithmeticOp {
                left,
                right: _,
                op: o,
            } => {
                match o {
                    Op::Gt => return Ok(Some(Bool)),
                    Op::Less => return Ok(Some(Bool)),
                    Op::Eq => return Ok(Some(Bool)),
                    Op::And => return Ok(Some(Bool)),
                    Op::Or => return Ok(Some(Bool)),
                    _ => {}
                }

                let _leftType = left.toDataType(typesMapping, functionReturns, None)?;
                let _rightType = left.toDataType(typesMapping, functionReturns, None)?;

                Ok(_leftType)
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
            Expression::StringLiteral(_) => Ok(Some(DataType::str())),
            Expression::FunctionCall(f) => {
                let types = f
                    .arguments
                    .iter()
                    .filter_map(|x| x.toDataType(typesMapping, functionReturns, None).ok()?)
                    .collect::<Vec<DataType>>();
                // println!("{:?}", &types);
                let enc = genFunName(f.name.as_str(), &types);
                match functionReturns.get(&MyStr::Runtime(enc.clone().into_boxed_str())) {
                    None => {
                        println!("{functionReturns:?}");
                        Err(Box::new(TypeNotFound { typ: enc }))
                    }
                    Some(v) => Ok(v.clone()),
                }
            }
            Expression::Variable(name) => {
                match typesMapping.get(&MyStr::Runtime(name.clone().into_boxed_str())) {
                    None => Err(Box::new(TypeNotFound {
                        typ: format!("variable {name} not found"),
                    })),
                    Some(v) => Ok(Some(v.0.clone())),
                }
            }
            Expression::BoolLiteral(_) => Ok(Some(DataType::Bool)),
            Expression::CharLiteral(_) => Ok(Some(Char)),
            Expression::ArrayLiteral(e) => {
                if e.is_empty() {
                    match typeHint
                        .ok_or("cannot infer type of empty array consider adding type hint")?
                    {
                        Object(o) => {
                            if o.name.as_str() == "Array" {
                                let e = o
                                    .generics
                                    .first()
                                    .ok_or("array type must have genneric type")?;
                                Ok(Some(DataType::arr(e.clone())))
                            } else {
                                Err(Box::new(InvalidTypeException {
                                    expected: DataType::Object(ObjectMeta {
                                        name: MyStr::from("Array"),
                                        generics: Box::new([Any]),
                                    }),
                                    actual: Some(Object(o)),
                                }))
                            }
                        }
                        v => Err(Box::new(InvalidTypeException {
                            expected: DataType::arr(Any),
                            actual: Some(v),
                        })),
                    }
                } else {
                    let t = e
                        .get(0)
                        .ok_or("array must have least one value")?
                        .toDataType(typesMapping, functionReturns, None)?
                        .ok_or("array item must have tyoe")?;
                    Ok(Some(DataType::arr(Generic::Type(t))))
                }
            }
            Expression::ArrayIndexing(i) => {
                let e = i
                    .expr
                    .toDataType(typesMapping, functionReturns, None)?
                    .ok_or("cannot array index none")?;
                match e {
                    Object(o) => {
                        if o.name.as_str() == "String" {
                            return Ok(Some(Char));
                        }
                        Ok(Some(
                            o.generics
                                .first()
                                .ok_or("array must have one generic parameter")?
                                .clone()
                                .ok_or("")?,
                        ))
                    }
                    _ => panic!(),
                }
            }
            Expression::NotExpression(i) => {
                let d = i.toDataType(typesMapping, functionReturns, None)?;

                match d.ok_or("not operator cant work ok none")? {
                    DataType::Bool => Ok(Some(DataType::Bool)),
                    _ => {
                        panic!()
                    }
                }
            }
            Expression::NamespaceAccess(n, i) => {
                i.toDataType(typesMapping, functionReturns, typeHint)
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum Statement {
    FunctionExpr(FunctionCall),
    While(While),
    Variable(VariableCreate),
    VariableMod(VariableMod),
    If(If),
    Return(Return),
    ArrayAssign {
        left: ArrayAccess,
        right: Expression,
    },
    Continue,
    Break,
    Loop(Vec<Statement>),
    NamespaceFunction(Vec<String>, FunctionCall)
}

#[derive(Debug, Clone)]
pub struct VariableMod {
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
    pub body: Vec<Statement>,
    pub elseBody: Option<Vec<Statement>>,
}

#[derive(Debug, Clone)]
pub struct StructDef {
    pub name: String,
    pub fields: HashMap<String, DataType>,
}

#[derive(Debug, Clone)]
pub enum Node {
    FunctionDef(FunctionDef),
    StructDef(StructDef),
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

#[derive(Debug, Clone)]
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