use std::collections::HashMap;
use crate::ast::{ASTNode, Expression};
use crate::bytecodeGen::Body;
use crate::errors::{NoSuchParsingUnit, ParserError, SymbolType};
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
    pub symbols: HashMap<String, SymbolType>
}

pub fn parseTokens(toks: Vec<Token<TokenType>>, units: &mut [Box<dyn ParsingUnit<ASTNode, TokenType, VIPLParsingState>>]) -> Result<Vec<ASTNode>, ParserError<TokenType>> {
    let tokens = TokenProvider::new(toks);

    let mut parser = Parser{
        tokens,
        units,
        state: VIPLParsingState{ symbols: Default::default() },
        previousBuf: vec![],
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
                ASTNode::Expr(e) => e.isCallable(),
                _ => false
            }
        }
    }

    pub fn isPrevAssignable(&self) -> bool {
        match self.previousBuf.last() {
            None => false,
            Some(v) => match v {
                ASTNode::Expr(e) => e.isAssignable(),
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
            "bool" => return Ok(DataType::Bool),
            "char" => return Ok(DataType::Char),
            "int" => return Ok(DataType::Int),
            "float" => return Ok(DataType::Float),
            "value" => return Ok(DataType::Value),
            c => {
                return Ok(DataType::Object(ObjectMeta {
                    name: c.to_string().into(),
                    generics: Box::new([]),
                }))
            }
        }
    }
}