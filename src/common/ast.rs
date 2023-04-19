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
    Lambda(Vec<VariableMetadata>, DataType),
    Callable(Box<Expression>, Vec<Expression>),
    StructInit(String, Vec<(String, Expression)>),
    FieldAccess(Box<Expression>, String)
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

impl Expression {
    pub fn toDataType(
        &self,
        typesMapping: &HashMap<MyStr, (DataType, usize)>,
        functionReturns: &HashMap<MyStr, Option<DataType>>,
        typeHint: Option<DataType>,
    ) -> Result<Option<DataType>, Box<dyn Error>> {
        match self {
            Expression::BinaryOperation {
                left,
                right: _,
                op: o,
            } => {
                match o {
                    BinaryOp::Gt => return Ok(Some(Bool)),
                    BinaryOp::Less => return Ok(Some(Bool)),
                    BinaryOp::Eq => return Ok(Some(Bool)),
                    BinaryOp::And => return Ok(Some(Bool)),
                    BinaryOp::Or => return Ok(Some(Bool)),
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
            Expression::Variable(name) => {
                match typesMapping.get(&MyStr::Runtime(name.clone().into_boxed_str())) {
                    None => {
                        panic!();
                        Err(Box::new(TypeNotFound {
                            typ: format!("variable {name} not found"),
                        }))
                    },
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
            Expression::NamespaceAccess(n) => {
                panic!()
                //i.toDataType(typesMapping, functionReturns, typeHint)
            }
            Expression::Lambda(_, _) => todo!(),
            Expression::Callable(prev, args) => {
                let res = prev.toDataType(typesMapping, functionReturns, typeHint)?.unwrap();
                match res {
                    DataType::Function { args, ret } => {
                        Ok(Some(*ret.clone()))
                    }
                    _ => panic!()
                }
            }
            Expression::StructInit(name, _) => {
                Ok(Some(Object(ObjectMeta { name: name.clone().into(), generics: Box::new([]) })))
            }
            Expression::FieldAccess(_, _) => todo!()
        }
    }
}

#[derive(Debug, Clone)]
pub enum Statement {
    FunctionExpr(FunctionCall),
    While(While),
    Variable(VariableCreate),
    VariableMod(VariableModd),
    If(If),
    Return(Return),
    ArrayAssign {
        left: ArrayAccess,
        right: Expression,
    },
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