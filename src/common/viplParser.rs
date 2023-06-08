use std::collections::HashMap;
use crate::ast::{ASTNode, Expression, Node, RawExpression, RawNode, RawStatement, Statement};
use crate::bytecodeGen::Body;
use crate::errors::{CodeGenError, NoSuchParsingUnit, ParserError, SymbolType};

use crate::lexer::Token;
use crate::lexingUnits::TokenType;
use crate::lexingUnits::TokenType::*;
use crate::parser::{Parser, ParsingUnit, TokenProvider};
use crate::parser::ParsingUnitSearchType::{Ahead, Around, Behind};
use crate::parsingUnits::parsingUnits;
use crate::vm::dataType::{DataType, Generic, ObjectMeta};

pub const VALID_EXPRESSION_TOKENS: [TokenType; 5] = [
    StringLiteral,
    IntLiteral,
    LongLiteral,
    Identifier,
    CharLiteral,
];

pub type VIPLParser<'a> = Parser<'a, TokenType, ASTNode, VIPLParsingState>;

impl TokenProvider<TokenType> {
    pub fn getIdentifier(&mut self) -> Result<String, ParserError<TokenType>> {
        let t = self.getAssert(TokenType::Identifier)?;

        Ok(t.str.clone())
    }
}

#[derive(Debug)]
pub struct VIPLParsingState {
    pub symbols: HashMap<String, SymbolType>,
    pub parsingStart: Vec<Option<usize>>
}

pub fn parseTokens(toks: Vec<Token<TokenType>>, units: &mut [Box<dyn ParsingUnit<ASTNode, TokenType, VIPLParsingState>>]) -> Result<Vec<ASTNode>, ParserError<TokenType>> {
    let tokens = TokenProvider::new(toks);

    let mut parser = Parser{
        tokens,
        units,
        state: VIPLParsingState{ symbols: Default::default(), parsingStart: vec![] },
        previousBuf: vec![]
    };

    while !parser.tokens.isDone() {
        if parser.previousBuf.is_empty() {
            if parser.parseType(Ahead)? {
                continue;
            }

            if parser.parseType(Behind)? {
                continue;
            }

            if parser.parseType(Around)? {
                continue;
            }
        } else {
            if parser.parseType(Behind)? {
                continue;
            }

            if parser.parseType(Around)? {
                continue;
            }
            if parser.parseType(Ahead)? {
                continue;
            }
        }

        return Err(ParserError::NoSuchParsingUnit(NoSuchParsingUnit {
            typ: Ahead,
            token: parser.tokens.peekOne().cloned(),
        }));
    }
    Ok(parser.previousBuf)
}

impl Parser<'_, TokenType, ASTNode, VIPLParsingState> {
    pub fn pop(&mut self) -> Result<ASTNode, ParserError<TokenType>> {
        let r = self.previousBuf.pop().ok_or(ParserError::Unknown("fuuck".to_string().into()))?;

        if let Some(v) = self.state.parsingStart.last_mut() {
            if v.is_none() {
                *v = Some(self.tokens.index - 1);
            }
        }

        Ok(r)
    }

    pub fn parseWrappedExpression<F: std::ops::Fn(&mut VIPLParser) -> Result<RawExpression, ParserError<TokenType>>>(&mut self, f: F) -> Result<ASTNode, ParserError<TokenType>> {
        let mut startIndex = self.tokens.index;
        self.state.parsingStart.push(None);

        let res = f(self)?;

        println!("{:?}", self.state.parsingStart);
        if let Some(v) = self.state.parsingStart.last().unwrap() {
            if v < &startIndex {
                startIndex = *v;
            }
        }

        self.state.parsingStart.pop();

        Ok(ASTNode::Expr(Expression { exp: res, loc: self.tokens.tokens[startIndex..self.tokens.index].to_vec() }))
    }

    pub fn parseWrappedStatement<F: std::ops::Fn(&mut VIPLParser) -> Result<RawStatement, ParserError<TokenType>>>(&mut self, f: F) -> Result<ASTNode, ParserError<TokenType>> {
        let startIndex = self.tokens.index;

        let res = f(self)?;

        Ok(ASTNode::Statement(Statement { exp: res, loc: self.tokens.tokens[startIndex..self.tokens.index].to_vec() }))
    }

    pub fn parseWrappedNode<F: std::ops::Fn(&mut VIPLParser) -> Result<RawNode, ParserError<TokenType>>>(&mut self, f: F) -> Result<ASTNode, ParserError<TokenType>> {
        let startIndex = self.tokens.index;

        let res = f(self)?;

        Ok(ASTNode::Global(Node { exp: res, loc: self.tokens.tokens[startIndex..self.tokens.index].to_vec() }))
    }

    pub fn parseSymbol(&mut self) -> Result<Vec<String>, ParserError<TokenType>> {
        let mut buf = vec![];

        while self.tokens.isPeekType(Identifier) {
            let i = self.tokens.getIdentifier()?;
            buf.push(i);
            if !self.tokens.isPeekType(Namespace) {
                break;
            }
            self.tokens.getAssert(Namespace)?;
        }

        Ok(buf)
    }

    pub fn isPrevExp(&self) -> bool {

        match self.previousBuf.last() {
            None => false,
            Some(v) => match v {
                ASTNode::Expr(_) => true,
                _ => false
            }
        }
    }

    pub fn isPrevSta(&self) -> bool {
        match self.previousBuf.last() {
            None => false,
            Some(v) => match v {
                ASTNode::Statement(_) => true,
                _ => false
            }
        }
    }

    pub fn isPrevGlo(&self) -> bool {
        match self.previousBuf.last() {
            None => false,
            Some(v) => match v {
                ASTNode::Global(_) => true,
                _ => false
            }
        }
    }

    pub fn isPrevCallable(&self) -> bool {
        match self.previousBuf.last() {
            None => false,
            Some(v) => match v {
                ASTNode::Expr(e) => e.exp.isCallable(),
                _ => false
            }
        }
    }

    pub fn isPrevAssignable(&self) -> bool {
        match self.previousBuf.last() {
            None => false,
            Some(v) => match v {
                ASTNode::Expr(e) => e.exp.isAssignable(),
                _ => false
            }
        }
    }

    pub fn parseExprOneLine(
        &mut self
    ) -> Result<Expression, ParserError<TokenType>> {
        let res = self.parseOneOneLine(Ahead)?;
        res.asExpr()
    }

    pub fn parseDataType(
        &mut self
    ) -> Result<DataType, ParserError<TokenType>> {
        parseDataType(&mut self.tokens)
    }

    pub fn parseExpr(
        &mut self
    ) -> Result<Expression, ParserError<TokenType>> {
        let res = self.parseOne(Ahead)?;
        res.asExpr()
    }

    pub fn parseBody(
        &mut self,
    ) -> Result<Body, ParserError<TokenType>> {
        let mut statements = vec![];

        self.tokens.getAssert(TokenType::OCB)?;

        while !self.tokens.isPeekType(CCB) {
            statements.push(self.parseOne(Ahead)?.asStatement()?);
        }

        self.tokens.getAssert(TokenType::CCB)?;

        Ok(Body::new(statements))
    }
}

pub fn parseDataType(
    tokens: &mut TokenProvider<TokenType>,
) -> Result<DataType, ParserError<TokenType>> {
    if tokens.isPeekType(Identifier) && tokens.isPeekIndexType(Gt, 1) {
        let mut generics = vec![];

        let t = tokens.getIdentifier()?;
        tokens.getAssert(TokenType::Gt)?;

        while !tokens.isPeekType(TokenType::Less) {
            generics.push(Generic::Type(parseDataType(tokens)?));
        }
        tokens.getAssert(TokenType::Less)?;

        Ok(DataType::Object(ObjectMeta {
            name: t,
            generics: generics.into_boxed_slice(),
        }))
    } else if tokens.isPeekType(TokenType::ORB) {
        tokens.getAssert(ORB)?;
        let args = tokens.parseManyWithSeparatorUntil(|it| parseDataType(it), Some(Comma), CRB)?;
        let ret = if tokens.isPeekType(Colon) {
            tokens.getAssert(Colon)?;
            parseDataType(tokens)?
        } else {
            DataType::Void
        };
        Ok(DataType::Function {
            args,
            ret: Box::new(ret),
        })
    } else if tokens.isPeekType(Not) {
        tokens.getAssert(Not)?;
        Ok(DataType::Void)
    } else {
        let t = tokens.getIdentifier()?;

        match t.as_str() {
            "bool" => Ok(DataType::Bool),
            "char" => Ok(DataType::Char),
            "int" => Ok(DataType::Int),
            "float" => Ok(DataType::Float),
            "value" => Ok(DataType::Value),
            c => {
                Ok(DataType::Object(ObjectMeta {
                    name: c.to_string(),
                    generics: Box::new([]),
                }))
            }
        }
    }
}