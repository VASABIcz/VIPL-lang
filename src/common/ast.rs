use crate::ast::RawStatement::StatementExpression;
use std::collections::HashMap;
use std::fmt::Display;
use std::ops::Range;
use strum_macros::Display;
use crate::codeGenCtx::{Body, SimpleCtx};

use crate::errors::{InvalidOperation, ParserError};
use crate::fastAccess::FastAccess;
use crate::lexer::{Location, Token};
use crate::lexingUnits::TokenType;
use crate::utils::{genFunName, getRanges};
use crate::vm::dataType::{DataType, RawDataType};
use crate::vm::namespace::StructMeta;
use crate::vm::variableMetadata::VariableMetadata;

#[derive(Debug, Clone, PartialEq, Copy)]
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

impl BinaryOp {
    pub fn toDataType(&self, a: RawDataType) -> RawDataType {
        match self {
            BinaryOp::Add => {
                if a.isFloat() {
                    RawDataType::Float
                }
                else {
                    RawDataType::Int
                }
            }
            BinaryOp::Sub => {
                if a.isFloat() {
                    RawDataType::Float
                }
                else {
                    RawDataType::Int
                }
            }
            BinaryOp::Mul => {
                if a.isFloat() {
                    RawDataType::Float
                }
                else {
                    RawDataType::Int
                }
            }
            BinaryOp::Div => {
                RawDataType::Float
            }
            BinaryOp::Gt => {
                RawDataType::Bool
            }
            BinaryOp::Less => {
                RawDataType::Bool
            }
            BinaryOp::Eq => {
                RawDataType::Bool
            }
            BinaryOp::NotEq => {
                RawDataType::Bool
            }
            BinaryOp::And => {
                assert!(a.isBool());

                RawDataType::Bool
            }
            BinaryOp::Or => {
                assert!(a.isBool());

                RawDataType::Bool
            }
            BinaryOp::Modulo => {
                assert!(!a.isFloat());

                RawDataType::Int
            }
            BinaryOp::ShiftLeft => {
                assert!(!a.isFloat());

                RawDataType::Int
            }
            BinaryOp::ShiftRight => {
                assert!(!a.isFloat());

                RawDataType::Int
            }
            BinaryOp::BitwiseOr => {
                assert!(!a.isFloat());

                RawDataType::Int
            }
            BinaryOp::BitwiseAnd => {
                assert!(!a.isFloat());

                RawDataType::Int
            }
            BinaryOp::Xor => {
                assert!(!a.isFloat());

                RawDataType::Int
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Copy)]
pub enum ArithmeticOp {
    Add,
    Sub,
    Mul,
    Div,
    Modulo,
    ShiftLeft,
    ShiftRight,
    BitwiseOr,
    BitwiseAnd,
    Xor
}

impl ArithmeticOp {
    pub fn toBinaryOp(self) -> BinaryOp {
        match self {
            ArithmeticOp::Add => BinaryOp::Add,
            ArithmeticOp::Sub => BinaryOp::Sub,
            ArithmeticOp::Mul => BinaryOp::Mul,
            ArithmeticOp::Div => BinaryOp::Div,
            ArithmeticOp::Modulo => BinaryOp::Modulo,
            ArithmeticOp::ShiftLeft => BinaryOp::ShiftLeft,
            ArithmeticOp::ShiftRight => BinaryOp::ShiftRight,
            ArithmeticOp::BitwiseOr => BinaryOp::BitwiseOr,
            ArithmeticOp::BitwiseAnd => BinaryOp::BitwiseAnd,
            ArithmeticOp::Xor => BinaryOp::Xor,
        }
    }
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
    NullAssert(Box<Expression>),
    Elvis(Box<Expression>, Box<Expression>)
}

impl RawExpression {
    pub fn isCallable(&self) -> bool {
        matches!(self, RawExpression::Variable(..) | RawExpression::NamespaceAccess(..) | RawExpression::FieldAccess(..))
    }

    pub fn isAssignable(&self) -> bool {
        matches!(self, RawExpression::Variable(..) | RawExpression::ArrayIndexing(..) | RawExpression::NamespaceAccess(..) | RawExpression::FieldAccess(..))
    }

    pub fn isConstructable(&self) -> bool {
        matches!(self, RawExpression::Variable(..) | RawExpression::NamespaceAccess(..))
    }

    pub fn toExpression(self) -> Expression {

        Expression{ exp: self, loc: vec![] }
    }
    
    pub fn stringify(self) -> RawExpression {
        return if matches!(self, RawExpression::StringLiteral(_)) {
            self
        } else {
            RawExpression::Callable(Box::new(RawExpression::Variable("toString".to_string()).toExpression()), vec![self.toExpression()])
        }
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
    Return(Option<Expression>),
    Continue,
    Break,
    Loop(Body),
    StatementExpression(Expression),
    Assignable(Expression, Expression, Option<ArithmeticOp>, Option<DataType>),
    ForLoop(String, Expression, Body),
    Repeat(String, usize, Body),
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

    pub fn isVariable(&self) -> bool {
        matches!(self.exp, RawExpression::Variable(_))
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
        matches!(self, ASTNode::Expr(_))
    }

    pub fn isStatement(&self) -> bool {
        matches!(self, ASTNode::Statement(_))
    }

    pub fn isGlobal(&self) -> bool {
        matches!(self, ASTNode::Global(_))
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

fn muateAST(expectedType: DataType, actualType: DataType, exp: Expression) -> Result<Expression, ()> {
    todo!()
}

fn ASTMutator<T>(node: ASTNode, ctx: SimpleCtx<T>, f: fn(ASTNode, SimpleCtx<T>) -> Result<ASTNode, ()>) -> ASTNode {
    match node {
        ASTNode::Global(g) => match g.exp {
            RawNode::FunctionDef(_) => {}
            RawNode::StructDef(_) => {}
            RawNode::GlobalVarDef(_, _) => {}
            RawNode::NamespaceImport(_, _) => {}
            RawNode::Import(_, _) => {}
        }
        ASTNode::Statement(s) => match s.exp {
            RawStatement::While(_) => {}
            RawStatement::If(_) => {}
            RawStatement::Return(_) => {}
            RawStatement::Continue => {}
            RawStatement::Break => {}
            RawStatement::Loop(_) => {}
            StatementExpression(_) => {}
            RawStatement::Assignable(_, _, _, _) => {}
            RawStatement::ForLoop(_, _, _) => {}
            RawStatement::Repeat(_, _, _) => {}
        }
        ASTNode::Expr(e) => match e.exp {
            RawExpression::BinaryOperation { .. } => {}
            RawExpression::IntLiteral(_) => {}
            RawExpression::FloatLiteral(_) => {}
            RawExpression::StringLiteral(_) => {}
            RawExpression::FormatStringLiteral(_) => {}
            RawExpression::BoolLiteral(_) => {}
            RawExpression::Variable(_) => {}
            RawExpression::CharLiteral(_) => {}
            RawExpression::ArrayLiteral(_) => {}
            RawExpression::ArrayIndexing(_) => {}
            RawExpression::NotExpression(_, _) => {}
            RawExpression::NamespaceAccess(_) => {}
            RawExpression::Lambda(_, _, _) => {}
            RawExpression::Callable(_, _) => {}
            RawExpression::StructInit(_, _) => {}
            RawExpression::FieldAccess(_, _) => {}
            RawExpression::TernaryOperator(_, _, _) => {}
            RawExpression::Null => {}
            RawExpression::TypeCast(_, _) => {}
            RawExpression::TypeCheck(_, _) => {}
            RawExpression::Negate(_) => {}
            RawExpression::BitwiseNot(_) => {}
            RawExpression::NullAssert(_) => {}
            RawExpression::Elvis(_, _) => {}
        }
    }
    todo!()
}