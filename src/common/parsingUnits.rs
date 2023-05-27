use std::collections::HashMap;

use crate::ast;
use crate::ast::Expression::NamespaceAccess;
use crate::ast::Statement::Assignable;
use crate::ast::{
    ASTNode, ArithmeticOp, ArrayAccess, BinaryOp, Expression, Node, Statement, StructDef, WhileS,
};
use crate::bytecodeGen::Body;
use crate::errors::{InvalidCharLiteral, InvalidToken, ParserError};
use crate::lexer::TokenType;
use crate::lexer::TokenType::*;
use crate::parser::ParsingUnitSearchType::{Ahead, Around, Back};
use crate::parser::{
    getParsingUnit, parseExprOneLine, parseOne, ParsingUnit, ParsingUnitSearchType, TokenProvider,
};
use crate::vm::dataType::{DataType, Generic, ObjectMeta};
use crate::vm::variableMetadata::VariableMetadata;

const VALID_EXPRESSION_TOKENS: [TokenType; 5] = [
    StringLiteral,
    IntLiteral,
    LongLiteral,
    Identifier,
    CharLiteral,
];

#[derive(Debug)]
pub struct BoolParsingUnit;

impl ParsingUnit<ASTNode, TokenType> for BoolParsingUnit {
    fn getType(&self) -> ParsingUnitSearchType {
        Ahead
    }

    fn canParse(
        &self,
        tokenProvider: &TokenProvider<TokenType>,
        previous: Option<&ASTNode>,
    ) -> bool {
        tokenProvider.isPeekType(TokenType::True) || tokenProvider.isPeekType(TokenType::False)
    }

    fn parse(
        &self,
        tokenProvider: &mut TokenProvider<TokenType>,
        _previous: Option<ASTNode>,
        _parser: &[Box<dyn ParsingUnit<ASTNode, TokenType>>],
    ) -> Result<ASTNode, ParserError<TokenType>> {
        if tokenProvider.isPeekType(TokenType::False) {
            tokenProvider.getAssert(TokenType::False)?;
            return Ok(ASTNode::Expr(Expression::BoolLiteral(false)));
        }
        tokenProvider.getAssert(TokenType::True)?;
        Ok(ASTNode::Expr(Expression::BoolLiteral(true)))
    }

    fn getPriority(&self) -> usize {
        usize::MAX
    }

    fn setPriority(&mut self, _priority: usize) {}
}

#[derive(Debug)]
pub struct VariableParsingUnit;

impl ParsingUnit<ASTNode, TokenType> for VariableParsingUnit {
    fn getType(&self) -> ParsingUnitSearchType {
        Ahead
    }

    fn canParse(
        &self,
        tokenProvider: &TokenProvider<TokenType>,
        previous: Option<&ASTNode>,
    ) -> bool {
        tokenProvider.isPeekType(Identifier)
    }

    fn parse(
        &self,
        tokenProvider: &mut TokenProvider<TokenType>,
        _previous: Option<ASTNode>,
        _parser: &[Box<dyn ParsingUnit<ASTNode, TokenType>>],
    ) -> Result<ASTNode, ParserError<TokenType>> {
        Ok(ASTNode::Expr(Expression::Variable(
            tokenProvider.getIdentifier()?,
        )))
    }

    fn getPriority(&self) -> usize {
        usize::MAX
    }

    fn setPriority(&mut self, _priority: usize) {}
}

#[derive(Debug)]
pub struct ReturnParsingUnit;

impl ParsingUnit<ASTNode, TokenType> for ReturnParsingUnit {
    fn getType(&self) -> ParsingUnitSearchType {
        Ahead
    }

    fn canParse(
        &self,
        tokenProvider: &TokenProvider<TokenType>,
        previous: Option<&ASTNode>,
    ) -> bool {
        tokenProvider.isPeekType(Return)
    }

    fn parse(
        &self,
        tokenProvider: &mut TokenProvider<TokenType>,
        _previous: Option<ASTNode>,
        parser: &[Box<dyn ParsingUnit<ASTNode, TokenType>>],
    ) -> Result<ASTNode, ParserError<TokenType>> {
        tokenProvider.getAssert(Return)?;
        let exp = parseExpr(tokenProvider, parser)?;
        Ok(ASTNode::Statement(Statement::Return(exp)))
    }

    fn getPriority(&self) -> usize {
        usize::MAX
    }

    fn setPriority(&mut self, _priority: usize) {}
}

#[derive(Debug)]
pub struct WhileParsingUnit;

impl ParsingUnit<ASTNode, TokenType> for WhileParsingUnit {
    fn getType(&self) -> ParsingUnitSearchType {
        Ahead
    }

    fn canParse(
        &self,
        tokenProvider: &TokenProvider<TokenType>,
        previous: Option<&ASTNode>,
    ) -> bool {
        tokenProvider.isPeekType(TokenType::While)
    }

    fn parse(
        &self,
        tokenProvider: &mut TokenProvider<TokenType>,
        _previous: Option<ASTNode>,
        parser: &[Box<dyn ParsingUnit<ASTNode, TokenType>>],
    ) -> Result<ASTNode, ParserError<TokenType>> {
        tokenProvider.getAssert(While)?;

        let op = parseExpr(tokenProvider, parser)?;

        let statements = parseBody(tokenProvider, parser)?;

        Ok(ASTNode::Statement(Statement::While(WhileS {
            exp: op,
            body: statements,
        })))
    }

    fn getPriority(&self) -> usize {
        usize::MAX
    }

    fn setPriority(&mut self, _priority: usize) {}
}

#[derive(Debug)]
pub struct IfParsingUnit;

pub fn parseBody(
    tokenProvider: &mut TokenProvider<TokenType>,
    parser: &[Box<dyn ParsingUnit<ASTNode, TokenType>>],
) -> Result<Body, ParserError<TokenType>> {
    let mut statements = vec![];

    tokenProvider.getAssert(TokenType::OCB)?;

    while !tokenProvider.isPeekType(CCB) {
        statements.push(parseOne(tokenProvider, Ahead, parser, None)?.asStatement()?);
    }

    tokenProvider.getAssert(TokenType::CCB)?;

    Ok(Body::new(statements))
}

pub fn parseExpr(
    tokenProvider: &mut TokenProvider<TokenType>,
    parser: &[Box<dyn ParsingUnit<ASTNode, TokenType>>],
) -> Result<Expression, ParserError<TokenType>> {
    let res = parseOne(tokenProvider, Ahead, parser, None)?;
    res.asExpr()
}

impl ParsingUnit<ASTNode, TokenType> for IfParsingUnit {
    fn getType(&self) -> ParsingUnitSearchType {
        Ahead
    }

    fn canParse(
        &self,
        tokenProvider: &TokenProvider<TokenType>,
        previous: Option<&ASTNode>,
    ) -> bool {
        tokenProvider.isPeekType(TokenType::If)
    }

    fn parse(
        &self,
        tokenProvider: &mut TokenProvider<TokenType>,
        _previous: Option<ASTNode>,
        parser: &[Box<dyn ParsingUnit<ASTNode, TokenType>>],
    ) -> Result<ASTNode, ParserError<TokenType>> {
        let mut elseIfs = vec![];
        let mut elseBody = None;

        tokenProvider.getAssert(If)?;

        let condition = parseExpr(tokenProvider, parser)?;

        let body = parseBody(tokenProvider, parser)?;

        while tokenProvider.isPeekType(Else) && tokenProvider.isPeekIndexType(If, 1) {
            tokenProvider.getAssert(Else)?;
            tokenProvider.getAssert(If)?;

            let cond = parseExpr(tokenProvider, parser)?;
            let statements = parseBody(tokenProvider, parser)?;

            elseIfs.push((cond, statements))
        }

        if tokenProvider.isPeekType(Else) {
            tokenProvider.getAssert(Else)?;
            elseBody = Some(parseBody(tokenProvider, parser)?);
        }

        Ok(ASTNode::Statement(Statement::If(ast::If {
            condition,
            body,
            elseBody,
            elseIfs,
        })))
    }

    fn getPriority(&self) -> usize {
        usize::MAX
    }

    fn setPriority(&mut self, _priority: usize) {}
}

#[derive(Debug)]
struct BracketsParsingUnit;

impl ParsingUnit<ASTNode, TokenType> for BracketsParsingUnit {
    fn getType(&self) -> ParsingUnitSearchType {
        Ahead
    }

    fn canParse(
        &self,
        tokenProvider: &TokenProvider<TokenType>,
        previous: Option<&ASTNode>,
    ) -> bool {
        tokenProvider.isPeekType(ORB)
    }

    fn parse(
        &self,
        tokenProvider: &mut TokenProvider<TokenType>,
        _previous: Option<ASTNode>,
        parser: &[Box<dyn ParsingUnit<ASTNode, TokenType>>],
    ) -> Result<ASTNode, ParserError<TokenType>> {
        tokenProvider.getAssert(ORB)?;
        let expr = Ok(ASTNode::Expr(parseExpr(tokenProvider, parser)?));
        tokenProvider.getAssert(CRB)?;
        expr
    }

    fn getPriority(&self) -> usize {
        usize::MAX
    }

    fn setPriority(&mut self, _priority: usize) {}
}

#[derive(Debug)]
struct CharParsingUnit;

impl ParsingUnit<ASTNode, TokenType> for CharParsingUnit {
    fn getType(&self) -> ParsingUnitSearchType {
        Ahead
    }

    fn canParse(
        &self,
        tokenProvider: &TokenProvider<TokenType>,
        previous: Option<&ASTNode>,
    ) -> bool {
        tokenProvider.isPeekType(CharLiteral)
    }

    fn parse(
        &self,
        tokenProvider: &mut TokenProvider<TokenType>,
        _previous: Option<ASTNode>,
        _parser: &[Box<dyn ParsingUnit<ASTNode, TokenType>>],
    ) -> Result<ASTNode, ParserError<TokenType>> {
        let c = tokenProvider.getAssert(CharLiteral)?;
        let mut chars = c.str.chars();
        match &chars.next() {
            None => Err(ParserError::InvalidCharLiteral(InvalidCharLiteral {
                token: c.clone(),
            })),
            Some(c) => {
                if *c == '\\' {
                    match chars.next() {
                        None => panic!(),
                        Some(c) => {
                            let e = match c {
                                'n' => '\n',
                                'r' => '\r',
                                't' => '\t',
                                '\\' => '\\',
                                _ => panic!(),
                            };
                            return Ok(ASTNode::Expr(Expression::CharLiteral(e)));
                        }
                    }
                }
                Ok(ASTNode::Expr(Expression::CharLiteral(*c)))
            }
        }
    }

    fn getPriority(&self) -> usize {
        todo!()
    }

    fn setPriority(&mut self, _priority: usize) {
        todo!()
    }
}

#[derive(Debug)]
struct StringParsingUnit;

impl ParsingUnit<ASTNode, TokenType> for StringParsingUnit {
    fn getType(&self) -> ParsingUnitSearchType {
        Ahead
    }

    fn canParse(
        &self,
        tokenProvider: &TokenProvider<TokenType>,
        previous: Option<&ASTNode>,
    ) -> bool {
        tokenProvider.isPeekType(TokenType::StringLiteral)
    }

    fn parse(
        &self,
        tokenProvider: &mut TokenProvider<TokenType>,
        _previous: Option<ASTNode>,
        _parser: &[Box<dyn ParsingUnit<ASTNode, TokenType>>],
    ) -> Result<ASTNode, ParserError<TokenType>> {
        let str = tokenProvider.getAssert(StringLiteral)?;
        Ok(ASTNode::Expr(Expression::StringLiteral(str.str.clone())))
    }

    fn getPriority(&self) -> usize {
        usize::MAX
    }

    fn setPriority(&mut self, _priority: usize) {}
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
            c => {
                return Ok(DataType::Object(ObjectMeta {
                    name: c.to_string().into(),
                    generics: Box::new([]),
                }))
            }
        }
    }
}

#[derive(Debug)]
struct ArrayLiteralParsingUnit;

impl ParsingUnit<ASTNode, TokenType> for ArrayLiteralParsingUnit {
    fn getType(&self) -> ParsingUnitSearchType {
        Ahead
    }

    fn canParse(
        &self,
        tokenProvider: &TokenProvider<TokenType>,
        previous: Option<&ASTNode>,
    ) -> bool {
        tokenProvider.isPeekType(OSB)
    }

    fn parse(
        &self,
        tokenProvider: &mut TokenProvider<TokenType>,
        _previous: Option<ASTNode>,
        parser: &[Box<dyn ParsingUnit<ASTNode, TokenType>>],
    ) -> Result<ASTNode, ParserError<TokenType>> {
        tokenProvider.getAssert(OSB)?;

        let mut buf = vec![];

        while !tokenProvider.isPeekType(CSB) {
            buf.push(parseExpr(tokenProvider, parser)?);
            if tokenProvider.isPeekType(Comma) {
                tokenProvider.getAssert(Comma)?;
            }
        }
        tokenProvider.getAssert(CSB)?;

        Ok(ASTNode::Expr(Expression::ArrayLiteral(buf)))
    }

    fn getPriority(&self) -> usize {
        usize::MAX
    }

    fn setPriority(&mut self, _priority: usize) {}
}

#[derive(Debug)]
struct ArrayIndexingParsingUnit;

impl ParsingUnit<ASTNode, TokenType> for ArrayIndexingParsingUnit {
    fn getType(&self) -> ParsingUnitSearchType {
        Back
    }

    fn canParse(
        &self,
        tokenProvider: &TokenProvider<TokenType>,
        previous: Option<&ASTNode>,
    ) -> bool {
        tokenProvider.isPeekType(OSB)
    }

    fn parse(
        &self,
        tokenProvider: &mut TokenProvider<TokenType>,
        previous: Option<ASTNode>,
        parser: &[Box<dyn ParsingUnit<ASTNode, TokenType>>],
    ) -> Result<ASTNode, ParserError<TokenType>> {
        tokenProvider.getAssert(OSB)?;
        let expr = parseExpr(tokenProvider, parser)?;
        tokenProvider.getAssert(CSB)?;

        Ok(ASTNode::Expr(Expression::ArrayIndexing(Box::new(
            ArrayAccess {
                // FIXME
                expr: previous.unwrap().asExpr()?,
                index: expr,
            },
        ))))
    }

    fn getPriority(&self) -> usize {
        usize::MAX
    }

    fn setPriority(&mut self, _priority: usize) {}
}

#[derive(Debug)]
struct ContinueParsingUnit;

impl ParsingUnit<ASTNode, TokenType> for ContinueParsingUnit {
    fn getType(&self) -> ParsingUnitSearchType {
        Ahead
    }

    fn canParse(
        &self,
        tokenProvider: &TokenProvider<TokenType>,
        previous: Option<&ASTNode>,
    ) -> bool {
        tokenProvider.isPeekType(Continue)
    }

    fn parse(
        &self,
        tokenProvider: &mut TokenProvider<TokenType>,
        _previous: Option<ASTNode>,
        _parser: &[Box<dyn ParsingUnit<ASTNode, TokenType>>],
    ) -> Result<ASTNode, ParserError<TokenType>> {
        tokenProvider.getAssert(Continue)?;
        Ok(ASTNode::Statement(Statement::Continue))
    }

    fn getPriority(&self) -> usize {
        todo!()
    }

    fn setPriority(&mut self, _priority: usize) {
        todo!()
    }
}

#[derive(Debug)]
struct BreakParsingUnit;

impl ParsingUnit<ASTNode, TokenType> for BreakParsingUnit {
    fn getType(&self) -> ParsingUnitSearchType {
        Ahead
    }

    fn canParse(
        &self,
        tokenProvider: &TokenProvider<TokenType>,
        previous: Option<&ASTNode>,
    ) -> bool {
        tokenProvider.isPeekType(TokenType::Break)
    }

    fn parse(
        &self,
        tokenProvider: &mut TokenProvider<TokenType>,
        _previous: Option<ASTNode>,
        _parser: &[Box<dyn ParsingUnit<ASTNode, TokenType>>],
    ) -> Result<ASTNode, ParserError<TokenType>> {
        tokenProvider.getAssert(TokenType::Break)?;
        Ok(ASTNode::Statement(Statement::Break))
    }

    fn getPriority(&self) -> usize {
        todo!()
    }

    fn setPriority(&mut self, _priority: usize) {
        todo!()
    }
}

#[derive(Debug)]
struct LoopParsingUnit;

impl ParsingUnit<ASTNode, TokenType> for LoopParsingUnit {
    fn getType(&self) -> ParsingUnitSearchType {
        Ahead
    }

    fn canParse(
        &self,
        tokenProvider: &TokenProvider<TokenType>,
        previous: Option<&ASTNode>,
    ) -> bool {
        tokenProvider.isPeekType(Loop)
    }

    fn parse(
        &self,
        tokenProvider: &mut TokenProvider<TokenType>,
        _previous: Option<ASTNode>,
        parser: &[Box<dyn ParsingUnit<ASTNode, TokenType>>],
    ) -> Result<ASTNode, ParserError<TokenType>> {
        tokenProvider.getAssert(Loop)?;
        let body = parseBody(tokenProvider, parser)?;
        Ok(ASTNode::Statement(Statement::Loop(body)))
    }

    fn getPriority(&self) -> usize {
        todo!()
    }

    fn setPriority(&mut self, _priority: usize) {
        todo!()
    }
}

#[derive(Debug)]
struct NotParsingUnit;

impl ParsingUnit<ASTNode, TokenType> for NotParsingUnit {
    fn getType(&self) -> ParsingUnitSearchType {
        Ahead
    }

    fn canParse(
        &self,
        tokenProvider: &TokenProvider<TokenType>,
        previous: Option<&ASTNode>,
    ) -> bool {
        tokenProvider.isPeekType(Not)
    }

    fn parse(
        &self,
        tokenProvider: &mut TokenProvider<TokenType>,
        _previous: Option<ASTNode>,
        parser: &[Box<dyn ParsingUnit<ASTNode, TokenType>>],
    ) -> Result<ASTNode, ParserError<TokenType>> {
        let t = tokenProvider.getAssert(Not)?.location;

        let expr = parseExpr(tokenProvider, parser)?;

        Ok(ASTNode::Expr(Expression::NotExpression(Box::new(expr), t)))
    }

    fn getPriority(&self) -> usize {
        todo!()
    }

    fn setPriority(&mut self, _priority: usize) {
        todo!()
    }
}

#[derive(Debug)]
struct StructParsingUnit;

impl ParsingUnit<ASTNode, TokenType> for StructParsingUnit {
    fn getType(&self) -> ParsingUnitSearchType {
        Ahead
    }

    fn canParse(
        &self,
        tokenProvider: &TokenProvider<TokenType>,
        previous: Option<&ASTNode>,
    ) -> bool {
        tokenProvider.isPeekType(Struct)
    }

    fn parse(
        &self,
        tokenProvider: &mut TokenProvider<TokenType>,
        _previous: Option<ASTNode>,
        _parser: &[Box<dyn ParsingUnit<ASTNode, TokenType>>],
    ) -> Result<ASTNode, ParserError<TokenType>> {
        tokenProvider.getAssert(Struct)?;
        let name = tokenProvider.getIdentifier()?;

        let mut fields = HashMap::new();

        tokenProvider.getAssert(OCB)?;

        while !tokenProvider.isPeekType(CCB) {
            let fieldName = tokenProvider.getIdentifier()?;
            tokenProvider.getAssert(Colon)?;
            let fieldType = parseDataType(tokenProvider)?;

            if fields.contains_key(&fieldName) {
                // FIXME
                panic!()
                // None.ok_or("struct cant have duplicate fields")?;
            }

            fields.insert(fieldName, fieldType);
        }

        tokenProvider.getAssert(CCB)?;

        Ok(ASTNode::Global(Node::StructDef(StructDef { name, fields })))
    }

    fn getPriority(&self) -> usize {
        todo!()
    }

    fn setPriority(&mut self, _priority: usize) {
        todo!()
    }
}

#[derive(Debug)]
struct ImportParsingUnit;

impl ParsingUnit<ASTNode, TokenType> for ImportParsingUnit {
    fn getType(&self) -> ParsingUnitSearchType {
        Ahead
    }

    fn canParse(
        &self,
        tokenProvider: &TokenProvider<TokenType>,
        previous: Option<&ASTNode>,
    ) -> bool {
        tokenProvider.isPeekType(Import)
    }

    fn parse(
        &self,
        tokenProvider: &mut TokenProvider<TokenType>,
        previous: Option<ASTNode>,
        parser: &[Box<dyn ParsingUnit<ASTNode, TokenType>>],
    ) -> Result<ASTNode, ParserError<TokenType>> {
        tokenProvider.getAssert(Import)?;
        let mut buf = vec![];

        let f = tokenProvider.getIdentifier()?;
        buf.push(f);

        while tokenProvider.isPeekType(Namespace) {
            tokenProvider.getAssert(Namespace)?;
            buf.push(tokenProvider.getIdentifier()?);
        }

        Ok(ASTNode::Global(Node::Import(buf)))
    }

    fn getPriority(&self) -> usize {
        todo!()
    }

    fn setPriority(&mut self, priority: usize) {
        todo!()
    }
}

#[derive(Debug)]
struct NamespaceParsingUnit;

impl ParsingUnit<ASTNode, TokenType> for NamespaceParsingUnit {
    fn getType(&self) -> ParsingUnitSearchType {
        Ahead
    }

    fn canParse(
        &self,
        tokenProvider: &TokenProvider<TokenType>,
        previous: Option<&ASTNode>,
    ) -> bool {
        tokenProvider.isPeekType(Identifier) && tokenProvider.isPeekIndexType(Namespace, 1)
    }

    fn parse(
        &self,
        tokenProvider: &mut TokenProvider<TokenType>,
        previous: Option<ASTNode>,
        parser: &[Box<dyn ParsingUnit<ASTNode, TokenType>>],
    ) -> Result<ASTNode, ParserError<TokenType>> {
        let mut buf = vec![];

        while tokenProvider.isPeekType(Identifier) {
            let i = tokenProvider.getIdentifier()?;
            buf.push(i);
            if tokenProvider.isPeekType(Namespace) {
                tokenProvider.getAssert(Namespace)?;
            } else {
                break;
            }
        }

        Ok(ASTNode::Expr(NamespaceAccess(buf)))
    }

    fn getPriority(&self) -> usize {
        todo!()
    }

    fn setPriority(&mut self, priority: usize) {
        todo!()
    }
}

#[derive(Debug)]
struct CallableParsingUnit;

impl ParsingUnit<ASTNode, TokenType> for CallableParsingUnit {
    fn getType(&self) -> ParsingUnitSearchType {
        Around
    }

    fn canParse(
        &self,
        tokenProvider: &TokenProvider<TokenType>,
        previous: Option<&ASTNode>,
    ) -> bool {
        tokenProvider.isPeekType(ORB)
            && previous.map_or(false, |it| {
                it.asExprRef().map_or(false, |it| it.isCallable())
            })
    }

    fn parse(
        &self,
        tokens: &mut TokenProvider<TokenType>,
        previous: Option<ASTNode>,
        parser: &[Box<dyn ParsingUnit<ASTNode, TokenType>>],
    ) -> Result<ASTNode, ParserError<TokenType>> {
        tokens.getAssert(ORB)?;

        let mut args = vec![];

        while !tokens.isPeekType(TokenType::CRB) {
            let res = parseOne(tokens, Ahead, parser, None)?;
            let par = getParsingUnit(tokens, Around, parser, None);

            let op = match par {
                None => res,
                Some(p) => p.parse(tokens, Some(res), parser)?,
            };

            args.push(op.asExpr()?);
            if !tokens.isPeekType(TokenType::CRB) {
                tokens.getAssert(TokenType::Comma)?;
            }
        }

        tokens.getAssert(TokenType::CRB)?;

        // FIXME
        Ok(ASTNode::Expr(Expression::Callable(
            Box::new(previous.unwrap().asExpr()?),
            args,
        )))
    }

    fn getPriority(&self) -> usize {
        todo!()
    }

    fn setPriority(&mut self, priority: usize) {
        todo!()
    }
}

#[derive(Debug)]
struct LambdaParsingUnit;

impl ParsingUnit<ASTNode, TokenType> for LambdaParsingUnit {
    fn getType(&self) -> ParsingUnitSearchType {
        Ahead
    }

    fn canParse(
        &self,
        tokenProvider: &TokenProvider<TokenType>,
        previous: Option<&ASTNode>,
    ) -> bool {
        tokenProvider.isPeekType(Fn) && tokenProvider.isPeekIndexType(ORB, 1)
    }

    fn parse(
        &self,
        tokens: &mut TokenProvider<TokenType>,
        previous: Option<ASTNode>,
        parser: &[Box<dyn ParsingUnit<ASTNode, TokenType>>],
    ) -> Result<ASTNode, ParserError<TokenType>> {
        tokens.getAssert(Fn)?;

        let mut args = vec![];
        let mut argCount = 0;
        let mut returnType = None;

        tokens.getAssert(ORB)?;
        while !tokens.isPeekType(CRB) {
            let argName = tokens.getIdentifier()?;
            tokens.getAssert(Colon)?;

            let t = parseDataType(tokens)?;

            args.push(VariableMetadata::new(argName, t));
            argCount += 1;
            if tokens.isPeekType(Comma) {
                tokens.consume();
            }
        }
        tokens.getAssert(CRB)?;

        if tokens.isPeekType(Colon) {
            tokens.getAssert(Colon)?;
            returnType = Some(parseDataType(tokens)?);
        }

        let mut isOneLine = false;

        let body = if tokens.isPeekType(Equals) {
            tokens.getAssert(Equals)?;
            isOneLine = true;
            Body::new(vec![ast::Statement::Return(parseExpr(tokens, parser)?)])
        } else {
            parseBody(tokens, parser)?
        };

        Ok(ASTNode::Expr(Expression::Lambda(args, body, returnType)))
    }

    fn getPriority(&self) -> usize {
        todo!()
    }

    fn setPriority(&mut self, priority: usize) {
        todo!()
    }
}

#[derive(Debug)]
struct IncParsingUnit;

impl ParsingUnit<ASTNode, TokenType> for IncParsingUnit {
    fn getType(&self) -> ParsingUnitSearchType {
        Back
    }

    fn canParse(
        &self,
        tokenProvider: &TokenProvider<TokenType>,
        previous: Option<&ASTNode>,
    ) -> bool {
        tokenProvider.isPeekType(TokenType::Inc) || tokenProvider.isPeekType(TokenType::Dec)
    }

    fn parse(
        &self,
        tokenProvider: &mut TokenProvider<TokenType>,
        previous: Option<ASTNode>,
        parser: &[Box<dyn ParsingUnit<ASTNode, TokenType>>],
    ) -> Result<ASTNode, ParserError<TokenType>> {
        // FIXME
        let prev = previous.unwrap().asExpr()?;

        if tokenProvider.isPeekType(TokenType::Inc) {
            tokenProvider.getAssert(TokenType::Inc)?;

            Ok(ASTNode::Statement(Assignable(
                prev,
                Expression::IntLiteral("1".to_string()),
                Some(ArithmeticOp::Add),
            )))
        } else {
            tokenProvider.getAssert(TokenType::Dec)?;

            Ok(ASTNode::Statement(Assignable(
                prev,
                Expression::IntLiteral("1".to_string()),
                Some(ArithmeticOp::Sub),
            )))
        }
    }

    fn getPriority(&self) -> usize {
        todo!()
    }

    fn setPriority(&mut self, priority: usize) {
        todo!()
    }
}

#[derive(Debug)]
struct StructInitParsingUnit;

impl ParsingUnit<ASTNode, TokenType> for StructInitParsingUnit {
    fn getType(&self) -> ParsingUnitSearchType {
        Ahead
    }

    fn canParse(
        &self,
        tokenProvider: &TokenProvider<TokenType>,
        previous: Option<&ASTNode>,
    ) -> bool {
        tokenProvider.isPeekType(Identifier) && tokenProvider.isPeekIndexType(OCB, 1)
    }

    fn parse(
        &self,
        tokenProvider: &mut TokenProvider<TokenType>,
        previous: Option<ASTNode>,
        parser: &[Box<dyn ParsingUnit<ASTNode, TokenType>>],
    ) -> Result<ASTNode, ParserError<TokenType>> {
        let name = tokenProvider.getIdentifier()?;

        tokenProvider.getAssert(OCB)?;

        let inits = tokenProvider.parseManyWithSeparatorUntil(
            |it| {
                let fieldName = it.getIdentifier()?;
                it.getAssert(Colon)?;
                let initializer = parseOne(it, Ahead, parser, None)?.asExpr()?;

                Ok((fieldName, initializer))
            },
            None,
            CCB,
        )?;

        Ok(ASTNode::Expr(Expression::StructInit(name, inits)))
    }

    fn getPriority(&self) -> usize {
        todo!()
    }

    fn setPriority(&mut self, priority: usize) {
        todo!()
    }
}

#[derive(Debug)]
struct FieldAccessParsingUnit;

impl ParsingUnit<ASTNode, TokenType> for FieldAccessParsingUnit {
    fn getType(&self) -> ParsingUnitSearchType {
        Back
    }

    fn canParse(
        &self,
        tokenProvider: &TokenProvider<TokenType>,
        previous: Option<&ASTNode>,
    ) -> bool {
        tokenProvider.isPeekType(Dot)
    }

    fn parse(
        &self,
        tokenProvider: &mut TokenProvider<TokenType>,
        previous: Option<ASTNode>,
        parser: &[Box<dyn ParsingUnit<ASTNode, TokenType>>],
    ) -> Result<ASTNode, ParserError<TokenType>> {
        tokenProvider.getAssert(Dot)?;
        let fieldName = tokenProvider.getIdentifier()?;

        Ok(ASTNode::Expr(Expression::FieldAccess(
            Box::new(previous.unwrap().asExpr()?),
            fieldName,
        )))
    }

    fn getPriority(&self) -> usize {
        todo!()
    }

    fn setPriority(&mut self, priority: usize) {
        todo!()
    }
}

#[derive(Debug)]
struct AssignableParsingUnit;

impl ParsingUnit<ASTNode, TokenType> for AssignableParsingUnit {
    fn getType(&self) -> ParsingUnitSearchType {
        Around
    }

    fn canParse(
        &self,
        tokenProvider: &TokenProvider<TokenType>,
        previous: Option<&ASTNode>,
    ) -> bool {
        tokenProvider.isPeekType(Equals)
            || tokenProvider.isPeekType(AddAs)
            || tokenProvider.isPeekType(SubAs)
            || tokenProvider.isPeekType(DivAs)
            || tokenProvider.isPeekType(MulAs)
                && previous
                    .map(|it| it.asExprRef().map(|it| it.isAssignable()).unwrap_or(false))
                    .unwrap_or(false)
    }

    fn parse(
        &self,
        tokenProvider: &mut TokenProvider<TokenType>,
        previous: Option<ASTNode>,
        parser: &[Box<dyn ParsingUnit<ASTNode, TokenType>>],
    ) -> Result<ASTNode, ParserError<TokenType>> {
        let mut typ = None;

        if tokenProvider.isPeekType(AddAs) {
            typ = Some(ArithmeticOp::Add);
        } else if tokenProvider.isPeekType(SubAs) {
            typ = Some(ArithmeticOp::Sub);
        } else if tokenProvider.isPeekType(MulAs) {
            typ = Some(ArithmeticOp::Mul);
        } else if tokenProvider.isPeekType(DivAs) {
            typ = Some(ArithmeticOp::Div);
        }
        tokenProvider.consume();

        let next = parseOne(tokenProvider, Ahead, parser, None)?.asExpr()?;

        // FIXME
        Ok(ASTNode::Statement(Statement::Assignable(
            previous.unwrap().asExpr()?,
            next,
            typ,
        )))
    }

    fn getPriority(&self) -> usize {
        todo!()
    }

    fn setPriority(&mut self, priority: usize) {
        todo!()
    }
}

#[derive(Debug)]
struct GlobalParsingUnit;

impl ParsingUnit<ASTNode, TokenType> for GlobalParsingUnit {
    fn getType(&self) -> ParsingUnitSearchType {
        Ahead
    }

    fn canParse(
        &self,
        tokenProvider: &TokenProvider<TokenType>,
        previous: Option<&ASTNode>,
    ) -> bool {
        tokenProvider.isPeekType(TokenType::Global)
    }

    fn parse(
        &self,
        tokenProvider: &mut TokenProvider<TokenType>,
        previous: Option<ASTNode>,
        parser: &[Box<dyn ParsingUnit<ASTNode, TokenType>>],
    ) -> Result<ASTNode, ParserError<TokenType>> {
        tokenProvider.getAssert(Global)?;

        let name = tokenProvider.getIdentifier()?;
        let mut typeHint = None;

        if tokenProvider.isPeekType(Colon) {
            tokenProvider.getAssert(Colon)?;
            typeHint = Some(parseDataType(tokenProvider)?);
        }

        tokenProvider.getAssert(TokenType::Equals)?;

        let res = parseOne(tokenProvider, Ahead, parser, None)?;
        let par = getParsingUnit(tokenProvider, Around, parser, None);

        let op = match par {
            None => res.asExpr()?,
            Some(p) => p.parse(tokenProvider, Some(res), parser)?.asExpr()?,
        };

        Ok(ASTNode::Global(Node::GlobalVarDef(name, op)))
    }

    fn getPriority(&self) -> usize {
        todo!()
    }

    fn setPriority(&mut self, priority: usize) {
        todo!()
    }
}

#[derive(Debug)]
struct ForParsingUnit;

impl ParsingUnit<ASTNode, TokenType> for ForParsingUnit {
    fn getType(&self) -> ParsingUnitSearchType {
        Ahead
    }

    fn canParse(
        &self,
        tokenProvider: &TokenProvider<TokenType>,
        previous: Option<&ASTNode>,
    ) -> bool {
        tokenProvider.isPeekType(For)
    }

    fn parse(
        &self,
        tokenProvider: &mut TokenProvider<TokenType>,
        previous: Option<ASTNode>,
        parser: &[Box<dyn ParsingUnit<ASTNode, TokenType>>],
    ) -> Result<ASTNode, ParserError<TokenType>> {
        tokenProvider.getAssert(For)?;

        let varName = tokenProvider.getIdentifier()?;

        tokenProvider.getAssert(In)?;

        let res = parseOne(tokenProvider, Ahead, parser, previous)?.asExpr()?;

        let body = parseBody(tokenProvider, parser)?;

        Ok(ASTNode::Statement(Statement::ForLoop(varName, res, body)))
    }

    fn getPriority(&self) -> usize {
        todo!()
    }

    fn setPriority(&mut self, priority: usize) {
        todo!()
    }
}

#[derive(Debug)]
struct NullParsingUnit;

impl ParsingUnit<ASTNode, TokenType> for NullParsingUnit {
    fn getType(&self) -> ParsingUnitSearchType {
        Ahead
    }

    fn canParse(
        &self,
        tokenProvider: &TokenProvider<TokenType>,
        previous: Option<&ASTNode>,
    ) -> bool {
        tokenProvider.isPeekType(Null)
    }

    fn parse(
        &self,
        tokenProvider: &mut TokenProvider<TokenType>,
        previous: Option<ASTNode>,
        parser: &[Box<dyn ParsingUnit<ASTNode, TokenType>>],
    ) -> Result<ASTNode, ParserError<TokenType>> {
        tokenProvider.getAssert(Null)?;

        Ok(ASTNode::Expr(Expression::Null))
    }

    fn getPriority(&self) -> usize {
        todo!()
    }

    fn setPriority(&mut self, priority: usize) {
        todo!()
    }
}

#[derive(Debug)]
struct OneArgFunctionParsintUnit;

impl ParsingUnit<ASTNode, TokenType> for OneArgFunctionParsintUnit {
    fn getType(&self) -> ParsingUnitSearchType {
        Ahead
    }

    fn canParse(
        &self,
        tokenProvider: &TokenProvider<TokenType>,
        previous: Option<&ASTNode>,
    ) -> bool {
        let row = match tokenProvider.peekOne() {
            None => return false,
            Some(v) => v.location.row,
        };

        tokenProvider.isPeekType(Identifier)
            && tokenProvider.isPeekIndexOf(|it| VALID_EXPRESSION_TOKENS.contains(&it), 1)
            && tokenProvider.isPeekIndexRow(1, row)
    }

    fn parse(
        &self,
        tokenProvider: &mut TokenProvider<TokenType>,
        previous: Option<ASTNode>,
        parser: &[Box<dyn ParsingUnit<ASTNode, TokenType>>],
    ) -> Result<ASTNode, ParserError<TokenType>> {
        let name = tokenProvider.getIdentifier()?;
        let arg = parseExprOneLine(tokenProvider, parser)?;

        return Ok(ASTNode::Expr(Expression::Callable(
            Box::new(Expression::Variable(name)),
            vec![arg],
        )));
    }

    fn getPriority(&self) -> usize {
        todo!()
    }

    fn setPriority(&mut self, priority: usize) {
        todo!()
    }
}

#[derive(Debug)]
struct TwoArgFunctionParsintUnit;

impl ParsingUnit<ASTNode, TokenType> for TwoArgFunctionParsintUnit {
    fn getType(&self) -> ParsingUnitSearchType {
        Around
    }

    fn canParse(
        &self,
        tokenProvider: &TokenProvider<TokenType>,
        previous: Option<&ASTNode>,
    ) -> bool {
        previous.map_or(false, |it| it.isExpr())
            && tokenProvider.isPeekType(Identifier)
            && tokenProvider.isPeekIndexOf(|it| VALID_EXPRESSION_TOKENS.contains(&it), 1)
    }

    fn parse(
        &self,
        tokenProvider: &mut TokenProvider<TokenType>,
        previous: Option<ASTNode>,
        parser: &[Box<dyn ParsingUnit<ASTNode, TokenType>>],
    ) -> Result<ASTNode, ParserError<TokenType>> {
        let name = tokenProvider.getIdentifier()?;
        let arg = parseExprOneLine(tokenProvider, parser)?;
        println!("previous {:?} {} {:?}", previous, name, arg);

        return Ok(ASTNode::Expr(Expression::Callable(
            Box::new(Expression::Variable(name)),
            vec![previous.unwrap().asExpr()?, arg],
        )));
    }

    fn getPriority(&self) -> usize {
        todo!()
    }

    fn setPriority(&mut self, priority: usize) {
        todo!()
    }
}

#[derive(Debug)]
struct TernaryOperatorParsingUnit;

impl ParsingUnit<ASTNode, TokenType> for TernaryOperatorParsingUnit {
    fn getType(&self) -> ParsingUnitSearchType {
        Around
    }

    fn canParse(
        &self,
        tokenProvider: &TokenProvider<TokenType>,
        previous: Option<&ASTNode>,
    ) -> bool {
        tokenProvider.isPeekType(QuestionMark) && previous.map_or(false, |it| it.isExpr())
    }

    fn parse(
        &self,
        tokenProvider: &mut TokenProvider<TokenType>,
        previous: Option<ASTNode>,
        parser: &[Box<dyn ParsingUnit<ASTNode, TokenType>>],
    ) -> Result<ASTNode, ParserError<TokenType>> {
        tokenProvider.getAssert(QuestionMark)?;

        let a = parseExpr(tokenProvider, parser)?;

        tokenProvider.getAssert(Colon)?;

        let b = parseExpr(tokenProvider, parser)?;

        Ok(ASTNode::Expr(Expression::TernaryOperator(
            Box::new(previous.unwrap().asExpr()?),
            Box::new(a),
            Box::new(b),
        )))
    }

    fn getPriority(&self) -> usize {
        todo!()
    }

    fn setPriority(&mut self, priority: usize) {
        todo!()
    }
}

#[derive(Debug)]
struct RepeatParsingUnit;

impl ParsingUnit<ASTNode, TokenType> for RepeatParsingUnit {
    fn getType(&self) -> ParsingUnitSearchType {
        Ahead
    }

    fn canParse(
        &self,
        tokenProvider: &TokenProvider<TokenType>,
        previous: Option<&ASTNode>,
    ) -> bool {
        tokenProvider.isPeekType(Repeat)
    }

    fn parse(
        &self,
        tokenProvider: &mut TokenProvider<TokenType>,
        previous: Option<ASTNode>,
        parser: &[Box<dyn ParsingUnit<ASTNode, TokenType>>],
    ) -> Result<ASTNode, ParserError<TokenType>> {
        tokenProvider.getAssert(Repeat)?;
        let varName = tokenProvider.getIdentifier()?;
        let count = tokenProvider
            .getAssert(TokenType::IntLiteral)?
            .str
            .parse::<usize>()
            .unwrap();

        let body = parseBody(tokenProvider, parser)?;

        Ok(ASTNode::Statement(Statement::Repeat(varName, count, body)))
    }

    fn getPriority(&self) -> usize {
        todo!()
    }

    fn setPriority(&mut self, priority: usize) {
        todo!()
    }
}

#[derive(Debug)]
pub struct FunctionParsingUnit;

impl ParsingUnit<ASTNode, TokenType> for FunctionParsingUnit {
    fn getType(&self) -> ParsingUnitSearchType {
        ParsingUnitSearchType::Ahead
    }

    fn canParse(
        &self,
        tokenProvider: &TokenProvider<TokenType>,
        previous: Option<&ASTNode>,
    ) -> bool {
        tokenProvider.isPeekType(TokenType::Fn) && tokenProvider.isPeekIndexType(Identifier, 1)
    }

    fn parse(
        &self,
        tokens: &mut TokenProvider<TokenType>,
        _previous: Option<ASTNode>,
        parser: &[Box<dyn ParsingUnit<ASTNode, TokenType>>],
    ) -> Result<ASTNode, ParserError<TokenType>> {
        let mut isNative = false;

        tokens.getAssert(TokenType::Fn)?;

        if tokens.isPeekType(Native) {
            tokens.getAssert(Native)?;

            isNative = true;
        }

        let name = tokens.getIdentifier()?;
        let mut args = vec![];
        let mut argCount = 0;
        let mut returnType = None;

        tokens.getAssert(ORB)?;
        while !tokens.isPeekType(CRB) {
            let argName = tokens.getIdentifier()?;
            tokens.getAssert(Colon)?;

            let t = parseDataType(tokens)?;

            args.push(VariableMetadata {
                name: argName,
                typ: t,
            });
            argCount += 1;
            if tokens.isPeekType(Comma) {
                tokens.consume();
            }
        }
        tokens.getAssert(CRB)?;

        if tokens.isPeekType(Colon) {
            tokens.getAssert(Colon)?;
            returnType = Some(parseDataType(tokens)?);
        }

        let mut isOneLine = false;

        let body = if tokens.isPeekType(Equals) {
            tokens.getAssert(Equals)?;
            isOneLine = true;
            Body::new(vec![ast::Statement::Return(parseExpr(tokens, parser)?)])
        } else {
            parseBody(tokens, parser)?
        };

        Ok(ASTNode::Global(Node::FunctionDef(ast::FunctionDef {
            name,
            localsMeta: args,
            argsCount: argCount,
            body,
            returnType,
            isNative,
            isOneLine,
        })))
    }

    fn getPriority(&self) -> usize {
        usize::MAX
    }

    fn setPriority(&mut self, _priority: usize) {}
}

#[derive(Debug)]
pub struct ArithmeticParsingUnit {
    pub op: BinaryOp,
    pub typ: TokenType,
    pub priority: usize,
}

impl ParsingUnit<ASTNode, TokenType> for ArithmeticParsingUnit {
    fn getType(&self) -> ParsingUnitSearchType {
        Around
    }

    fn canParse(
        &self,
        tokenProvider: &TokenProvider<TokenType>,
        previous: Option<&ASTNode>,
    ) -> bool {
        tokenProvider.isPeekType(self.typ)
    }

    fn parse(
        &self,
        tokens: &mut TokenProvider<TokenType>,
        previous: Option<ASTNode>,
        parser: &[Box<dyn ParsingUnit<ASTNode, TokenType>>],
    ) -> Result<ASTNode, ParserError<TokenType>> {
        tokens.consume();
        let res = parseOne(tokens, Ahead, parser, None)?;
        let par = getParsingUnit(tokens, Around, parser, previous.clone());

        match par {
            None => Ok(ASTNode::Expr(Expression::BinaryOperation {
                // FIXME
                left: Box::new(previous.unwrap().asExpr()?),
                right: Box::new(res.asExpr()?),
                op: self.op.clone(),
            })),
            Some(p) => {
                if self.priority < p.getPriority() {
                    Ok(p.parse(
                        tokens,
                        Some(ASTNode::Expr(Expression::BinaryOperation {
                            left: Box::new(previous.unwrap().asExpr()?),
                            right: Box::new(res.asExpr()?),
                            op: self.op.clone(),
                        })),
                        parser,
                    )?)
                } else {
                    Ok(ASTNode::Expr(Expression::BinaryOperation {
                        left: Box::new(previous.unwrap().asExpr()?),
                        right: Box::new(p.parse(tokens, Some(res), parser)?.asExpr()?),
                        op: self.op.clone(),
                    }))
                }
            }
        }
    }

    fn getPriority(&self) -> usize {
        self.priority
    }

    fn setPriority(&mut self, priority: usize) {
        self.priority = priority
    }
}

#[derive(Debug)]
pub struct NumericParsingUnit;

impl ParsingUnit<ASTNode, TokenType> for NumericParsingUnit {
    fn getType(&self) -> ParsingUnitSearchType {
        Ahead
    }

    fn canParse(
        &self,
        tokenProvider: &TokenProvider<TokenType>,
        previous: Option<&ASTNode>,
    ) -> bool {
        let peek = match tokenProvider.peekOne() {
            None => return false,
            Some(v) => v,
        }
        .typ;

        if peek == TokenType::IntLiteral
            || peek == TokenType::LongLiteral
            || peek == TokenType::FloatLiteral
            || peek == TokenType::DoubleLiteral
        {
            return true;
        }

        let peek1 = match tokenProvider.peekIndex(1) {
            None => return false,
            Some(v) => v,
        }
        .typ;

        if peek == Minus
            && (peek1 == TokenType::DoubleLiteral
                || peek1 == TokenType::LongLiteral
                || peek1 == TokenType::FloatLiteral
                || peek1 == TokenType::IntLiteral)
        {
            return true;
        }
        false
    }

    fn parse(
        &self,
        tokenProvider: &mut TokenProvider<TokenType>,
        _previous: Option<ASTNode>,
        _parser: &[Box<dyn ParsingUnit<ASTNode, TokenType>>],
    ) -> Result<ASTNode, ParserError<TokenType>> {
        let mut peek = tokenProvider.peekOne().unwrap();

        let mut buf = String::new();

        if peek.typ == Minus {
            buf.push('-');
            tokenProvider.consume();
            peek = tokenProvider.peekOne().unwrap()
        }

        let res = match peek.typ {
            TokenType::IntLiteral => {
                buf.push_str(&peek.str);
                ASTNode::Expr(Expression::IntLiteral(buf))
            }
            TokenType::LongLiteral => {
                buf.push_str(&peek.str);
                ASTNode::Expr(Expression::LongLiteral(buf))
            }
            TokenType::FloatLiteral => {
                buf.push_str(&peek.str);
                ASTNode::Expr(Expression::FloatLiteral(buf))
            }
            TokenType::DoubleLiteral => {
                buf.push_str(&peek.str);
                ASTNode::Expr(Expression::DoubleLiteral(buf))
            }
            _ => {
                return Err(ParserError::InvalidToken(InvalidToken {
                    expected: TokenType::IntLiteral,
                    actual: Some(peek.clone()),
                }))
            }
        };
        tokenProvider.consume();
        Ok(res)
    }

    fn getPriority(&self) -> usize {
        usize::MAX
    }

    fn setPriority(&mut self, _priority: usize) {}
}

pub fn parsingUnits() -> Vec<Box<dyn ParsingUnit<ASTNode, TokenType>>> {
    vec![
        Box::new(AssignableParsingUnit),
        Box::new(StructInitParsingUnit),
        Box::new(NamespaceParsingUnit),
        Box::new(WhileParsingUnit),
        Box::new(LoopParsingUnit),
        Box::new(FunctionParsingUnit),
        Box::new(NumericParsingUnit),
        Box::new(CharParsingUnit),
        Box::new(ArrayIndexingParsingUnit),
        Box::new(StringParsingUnit),
        Box::new(ArrayLiteralParsingUnit),
        Box::new(CallableParsingUnit),
        Box::new(BreakParsingUnit),
        Box::new(NotParsingUnit),
        Box::new(ContinueParsingUnit),
        Box::new(ImportParsingUnit),
        Box::new(NullParsingUnit),
        Box::new(ArithmeticParsingUnit {
            op: BinaryOp::Mul,
            typ: TokenType::Mul,
            priority: 0,
        }),
        Box::new(ArithmeticParsingUnit {
            op: BinaryOp::Div,
            typ: TokenType::Div,
            priority: 1,
        }),
        Box::new(ArithmeticParsingUnit {
            op: BinaryOp::Add,
            typ: TokenType::Plus,
            priority: 2,
        }),
        Box::new(ArithmeticParsingUnit {
            op: BinaryOp::Sub,
            typ: TokenType::Minus,
            priority: 3,
        }),
        Box::new(ArithmeticParsingUnit {
            op: BinaryOp::Eq,
            typ: TokenType::Eq,
            priority: 4,
        }),
        Box::new(ArithmeticParsingUnit {
            op: BinaryOp::Less,
            typ: TokenType::Less,
            priority: 5,
        }),
        Box::new(ArithmeticParsingUnit {
            op: BinaryOp::Gt,
            typ: TokenType::Gt,
            priority: 6,
        }),
        Box::new(ArithmeticParsingUnit {
            op: BinaryOp::And,
            typ: TokenType::And,
            priority: 7,
        }),
        Box::new(ArithmeticParsingUnit {
            op: BinaryOp::Or,
            typ: TokenType::Or,
            priority: 8,
        }),
        Box::new(BracketsParsingUnit),
        Box::new(OneArgFunctionParsintUnit),
        Box::new(VariableParsingUnit),
        Box::new(IfParsingUnit),
        Box::new(BoolParsingUnit),
        Box::new(ReturnParsingUnit),
        Box::new(StructParsingUnit),
        Box::new(IncParsingUnit),
        Box::new(FieldAccessParsingUnit),
        Box::new(GlobalParsingUnit),
        Box::new(ForParsingUnit),
        Box::new(LambdaParsingUnit),
        Box::new(TwoArgFunctionParsintUnit),
        Box::new(TernaryOperatorParsingUnit),
        Box::new(RepeatParsingUnit),
    ]
}
