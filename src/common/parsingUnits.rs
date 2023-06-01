use std::collections::HashMap;

use crate::ast;
use crate::ast::Expression::NamespaceAccess;
use crate::ast::Statement::Assignable;
use crate::ast::{
    ASTNode, ArithmeticOp, ArrayAccess, BinaryOp, Expression, Node, Statement, StructDef, WhileS,
};
use crate::bytecodeGen::Body;
use crate::errors::{InvalidCharLiteral, InvalidToken, ParserError};
use crate::lexingUnits::TokenType;
use crate::lexingUnits::TokenType::{AddAs, As, CCB, CharLiteral, Colon, Comma, Continue, CRB, CSB, DivAs, Dot, Else, Equals, Fn, For, From, Global, Identifier, If, Import, In, Loop, Minus, Mul, MulAs, Namespace, Not, Null, OCB, ORB, OSB, QuestionMark, Repeat, Return, StringLiteral, Struct, SubAs, While};
use crate::parser::ParsingUnitSearchType::{Ahead, Around, Behind};
use crate::parser::{Parser, ParsingUnit, ParsingUnitSearchType, TokenProvider};
use crate::viplParser::{parseDataType, VALID_EXPRESSION_TOKENS, VIPLParser, VIPLParsingState};
use crate::vm::dataType::{DataType, Generic, ObjectMeta};
use crate::vm::variableMetadata::VariableMetadata;

#[derive(Debug)]
pub struct BoolParsingUnit;

impl ParsingUnit<ASTNode, TokenType, VIPLParsingState> for BoolParsingUnit {
    fn getType(&self) -> ParsingUnitSearchType {
        Ahead
    }

    fn canParse(
        &self,
        parser: &VIPLParser
    ) -> bool {
        parser.tokens.isPeekType(TokenType::True) || parser.tokens.isPeekType(TokenType::False)
    }

    fn parse(
        &self,
        parser: &mut VIPLParser
    ) -> Result<ASTNode, ParserError<TokenType>> {
        if parser.tokens.isPeekType(TokenType::False) {
            parser.tokens.getAssert(TokenType::False)?;
            return Ok(ASTNode::Expr(Expression::BoolLiteral(false)));
        }
        parser.tokens.getAssert(TokenType::True)?;
        Ok(ASTNode::Expr(Expression::BoolLiteral(true)))
    }

    fn getPriority(&self) -> usize {
        usize::MAX
    }

    fn setPriority(&mut self, _priority: usize) {}
}

#[derive(Debug)]
pub struct VariableParsingUnit;

impl ParsingUnit<ASTNode, TokenType, VIPLParsingState> for VariableParsingUnit {
    fn getType(&self) -> ParsingUnitSearchType {
        Ahead
    }

    fn canParse(
        &self,
        parser: &VIPLParser
    ) -> bool {
        parser.tokens.isPeekType(Identifier)
    }

    fn parse(
        &self,
        parser: &mut VIPLParser
    ) -> Result<ASTNode, ParserError<TokenType>> {
        Ok(ASTNode::Expr(Expression::Variable(
            parser.tokens.getIdentifier()?,
        )))
    }

    fn getPriority(&self) -> usize {
        usize::MAX
    }

    fn setPriority(&mut self, _priority: usize) {}
}

#[derive(Debug)]
pub struct ReturnParsingUnit;

impl ParsingUnit<ASTNode, TokenType, VIPLParsingState> for ReturnParsingUnit {
    fn getType(&self) -> ParsingUnitSearchType {
        Ahead
    }

    fn canParse(
        &self,
        parser: &VIPLParser
    ) -> bool {
        parser.tokens.isPeekType(Return)
    }

    fn parse(
        &self,
        parser: &mut VIPLParser
    ) -> Result<ASTNode, ParserError<TokenType>> {
        parser.tokens.getAssert(Return)?;
        let exp = parser.parseExpr()?;
        Ok(ASTNode::Statement(Statement::Return(exp)))
    }

    fn getPriority(&self) -> usize {
        usize::MAX
    }

    fn setPriority(&mut self, _priority: usize) {}
}

#[derive(Debug)]
pub struct WhileParsingUnit;

impl ParsingUnit<ASTNode, TokenType, VIPLParsingState> for WhileParsingUnit {
    fn getType(&self) -> ParsingUnitSearchType {
        Ahead
    }

    fn canParse(
        &self,
        parser: &VIPLParser
    ) -> bool {
        parser.tokens.isPeekType(TokenType::While)
    }

    fn parse(
        &self,
        parser: &mut VIPLParser
    ) -> Result<ASTNode, ParserError<TokenType>> {
        parser.tokens.getAssert(While)?;

        let op = parser.parseExpr()?;

        let statements = parser.parseBody()?;

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

impl ParsingUnit<ASTNode, TokenType, VIPLParsingState> for IfParsingUnit {
    fn getType(&self) -> ParsingUnitSearchType {
        Ahead
    }

    fn canParse(
        &self,
        parser: &VIPLParser
    ) -> bool {
        parser.tokens.isPeekType(TokenType::If)
    }

    fn parse(
        &self,
        parser: &mut VIPLParser
    ) -> Result<ASTNode, ParserError<TokenType>> {
        let mut elseIfs = vec![];
        let mut elseBody = None;

        parser.tokens.getAssert(If)?;

        let condition = parser.parseExpr()?;

        let body = parser.parseBody()?;

        while parser.tokens.isPeekType(Else) && parser.tokens.isPeekIndexType(If, 1) {
            parser.tokens.getAssert(Else)?;
            parser.tokens.getAssert(If)?;

            let cond = parser.parseExpr()?;
            let statements = parser.parseBody()?;

            elseIfs.push((cond, statements))
        }

        if parser.tokens.isPeekType(Else) {
            parser.tokens.getAssert(Else)?;
            elseBody = Some(parser.parseBody()?);
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

impl ParsingUnit<ASTNode, TokenType, VIPLParsingState> for BracketsParsingUnit {
    fn getType(&self) -> ParsingUnitSearchType {
        Ahead
    }

    fn canParse(
        &self,
        parser: &VIPLParser
    ) -> bool {
        parser.tokens.isPeekType(ORB)
    }

    fn parse(
        &self,
        parser: &mut VIPLParser
    ) -> Result<ASTNode, ParserError<TokenType>> {
        parser.tokens.getAssert(ORB)?;
        let expr = Ok(ASTNode::Expr(parser.parseExpr()?));
        parser.tokens.getAssert(CRB)?;
        expr
    }

    fn getPriority(&self) -> usize {
        usize::MAX
    }

    fn setPriority(&mut self, _priority: usize) {}
}

#[derive(Debug)]
struct CharParsingUnit;

impl ParsingUnit<ASTNode, TokenType, VIPLParsingState> for CharParsingUnit {
    fn getType(&self) -> ParsingUnitSearchType {
        Ahead
    }

    fn canParse(
        &self,
        parser: &VIPLParser
    ) -> bool {
        parser.tokens.isPeekType(CharLiteral)
    }

    fn parse(
        &self,
        parser: &mut VIPLParser
    ) -> Result<ASTNode, ParserError<TokenType>> {
        let c = parser.tokens.getAssert(CharLiteral)?;
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

impl ParsingUnit<ASTNode, TokenType, VIPLParsingState> for StringParsingUnit {
    fn getType(&self) -> ParsingUnitSearchType {
        Ahead
    }

    fn canParse(
        &self,
        parser: &VIPLParser
    ) -> bool {
        parser.tokens.isPeekType(StringLiteral)
    }

    fn parse(
        &self,
        parser: &mut VIPLParser
    ) -> Result<ASTNode, ParserError<TokenType>> {
        let str = parser.tokens.getAssert(StringLiteral)?;
        Ok(ASTNode::Expr(Expression::StringLiteral(str.str.clone())))
    }

    fn getPriority(&self) -> usize {
        usize::MAX
    }

    fn setPriority(&mut self, _priority: usize) {}
}

#[derive(Debug)]
struct ArrayLiteralParsingUnit;

impl ParsingUnit<ASTNode, TokenType, VIPLParsingState> for ArrayLiteralParsingUnit {
    fn getType(&self) -> ParsingUnitSearchType {
        Ahead
    }

    fn canParse(
        &self,
        parser: &VIPLParser
    ) -> bool {
        parser.tokens.isPeekType(OSB)
    }

    fn parse(
        &self,
        parser: &mut VIPLParser
    ) -> Result<ASTNode, ParserError<TokenType>> {
        parser.tokens.getAssert(OSB)?;

        let mut buf = vec![];

        while !parser.tokens.isPeekType(CSB) {
            buf.push(parser.parseExpr()?);
            if parser.tokens.isPeekType(Comma) {
                parser.tokens.getAssert(Comma)?;
            }
        }
        parser.tokens.getAssert(CSB)?;

        Ok(ASTNode::Expr(Expression::ArrayLiteral(buf)))
    }

    fn getPriority(&self) -> usize {
        usize::MAX
    }

    fn setPriority(&mut self, _priority: usize) {}
}

#[derive(Debug)]
struct ArrayIndexingParsingUnit;

impl ParsingUnit<ASTNode, TokenType, VIPLParsingState> for ArrayIndexingParsingUnit {
    fn getType(&self) -> ParsingUnitSearchType {
        Behind
    }

    fn canParse(
        &self,
        parser: &VIPLParser
    ) -> bool {
        parser.tokens.isPeekType(OSB)
    }

    fn parse(
        &self,
        parser: &mut VIPLParser
    ) -> Result<ASTNode, ParserError<TokenType>> {
        parser.tokens.getAssert(OSB)?;
        let expr = parser.parseExpr()?;
        parser.tokens.getAssert(CSB)?;

        Ok(ASTNode::Expr(Expression::ArrayIndexing(Box::new(
            ArrayAccess {
                // FIXME
                expr: parser.prevPop()?.asExpr()?,
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

impl ParsingUnit<ASTNode, TokenType, VIPLParsingState> for ContinueParsingUnit {
    fn getType(&self) -> ParsingUnitSearchType {
        Ahead
    }

    fn canParse(
        &self,
        parser: &VIPLParser
    ) -> bool {
        parser.tokens.isPeekType(Continue)
    }

    fn parse(
        &self,
        parser: &mut VIPLParser
    ) -> Result<ASTNode, ParserError<TokenType>> {
        parser.tokens.getAssert(Continue)?;
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

impl ParsingUnit<ASTNode, TokenType, VIPLParsingState> for BreakParsingUnit {
    fn getType(&self) -> ParsingUnitSearchType {
        Ahead
    }

    fn canParse(
        &self,
        parser: &VIPLParser
    ) -> bool {
        parser.tokens.isPeekType(TokenType::Break)
    }

    fn parse(
        &self,
        parser: &mut VIPLParser
    ) -> Result<ASTNode, ParserError<TokenType>> {
        parser.tokens.getAssert(TokenType::Break)?;
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

impl ParsingUnit<ASTNode, TokenType, VIPLParsingState> for LoopParsingUnit {
    fn getType(&self) -> ParsingUnitSearchType {
        Ahead
    }

    fn canParse(
        &self,
        parser: &VIPLParser
    ) -> bool {
        parser.tokens.isPeekType(Loop)
    }

    fn parse(
        &self,
        parser: &mut VIPLParser
    ) -> Result<ASTNode, ParserError<TokenType>> {
        parser.tokens.getAssert(Loop)?;
        let body = parser.parseBody()?;
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

impl ParsingUnit<ASTNode, TokenType, VIPLParsingState> for NotParsingUnit {
    fn getType(&self) -> ParsingUnitSearchType {
        Ahead
    }

    fn canParse(
        &self,
        parser: &VIPLParser
    ) -> bool {
        parser.tokens.isPeekType(Not)
    }

    fn parse(
        &self,
        parser: &mut VIPLParser
    ) -> Result<ASTNode, ParserError<TokenType>> {
        let t = parser.tokens.getAssert(Not)?.location;

        let expr = parser.parseExpr()?;

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

impl ParsingUnit<ASTNode, TokenType, VIPLParsingState> for StructParsingUnit {
    fn getType(&self) -> ParsingUnitSearchType {
        Ahead
    }

    fn canParse(
        &self,
        parser: &VIPLParser
    ) -> bool {
        parser.tokens.isPeekType(Struct)
    }

    fn parse(
        &self,
        parser: &mut VIPLParser
    ) -> Result<ASTNode, ParserError<TokenType>> {
        parser.tokens.getAssert(Struct)?;
        let name = parser.tokens.getIdentifier()?;

        let mut fields = HashMap::new();

        parser.tokens.getAssert(OCB)?;

        while !parser.tokens.isPeekType(CCB) {
            let fieldName = parser.tokens.getIdentifier()?;
            parser.tokens.getAssert(Colon)?;
            let fieldType = parser.parseDataType()?;

            if fields.contains_key(&fieldName) {
                // FIXME
                panic!()
                // None.ok_or("struct cant have duplicate fields")?;
            }

            fields.insert(fieldName, fieldType);
        }

        parser.tokens.getAssert(CCB)?;

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
struct SymbolImport;

impl ParsingUnit<ASTNode, TokenType, VIPLParsingState> for SymbolImport {
    fn getType(&self) -> ParsingUnitSearchType { Ahead }

    fn canParse(&self, parser: &Parser<TokenType, ASTNode, VIPLParsingState>) -> bool {
        parser.tokens.isPeekType(From)
    }

    fn parse(&self, parser: &mut Parser<TokenType, ASTNode, VIPLParsingState>) -> Result<ASTNode, ParserError<TokenType>> {
        parser.tokens.getAssert(From)?;

        let namespaceName = parser.parseSymbol()?;

        parser.tokens.getAssert(Import)?;

        if parser.tokens.isPeekType(Mul) {
            parser.tokens.getAssert(Mul)?;

            return Ok(ASTNode::Global(Node::Import(namespaceName, vec![(String::from('*'), None)])))
        }

        let symbol = parser.tokens.getIdentifier()?;

        let rename = if parser.tokens.isPeekType(As) {
            parser.tokens.getAssert(As)?;
            Some(parser.tokens.getIdentifier()?)
        }
        else {
            None
        };

        Ok(ASTNode::Global(Node::Import(namespaceName, vec![(symbol, rename)])))
    }

    fn getPriority(&self) -> usize {
        todo!()
    }

    fn setPriority(&mut self, priority: usize) {
        todo!()
    }
}

#[derive(Debug)]
struct NamespaceImport;

impl ParsingUnit<ASTNode, TokenType, VIPLParsingState> for NamespaceImport {
    fn getType(&self) -> ParsingUnitSearchType {
        Ahead
    }

    fn canParse(
        &self,
        parser: &VIPLParser
    ) -> bool {
        parser.tokens.isPeekType(Import)
    }

    fn parse(
        &self,
        parser: &mut VIPLParser
    ) -> Result<ASTNode, ParserError<TokenType>> {
        parser.tokens.getAssert(Import)?;

        let s = parser.parseSymbol()?;

        let rename = if parser.tokens.isPeekType(As) {
            parser.tokens.getAssert(As)?;
            Some(parser.tokens.getIdentifier()?)
        }
        else {
            None
        };

        Ok(ASTNode::Global(Node::NamespaceImport(s, rename)))
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

impl ParsingUnit<ASTNode, TokenType, VIPLParsingState> for NamespaceParsingUnit {
    fn getType(&self) -> ParsingUnitSearchType {
        Ahead
    }

    fn canParse(
        &self,
        parser: &VIPLParser
    ) -> bool {
        parser.tokens.isPeekType(Identifier) && parser.tokens.isPeekIndexType(Namespace, 1)
    }

    fn parse(
        &self,
        parser: &mut VIPLParser
    ) -> Result<ASTNode, ParserError<TokenType>> {
        let mut buf = vec![];

        while parser.tokens.isPeekType(Identifier) {
            let i = parser.tokens.getIdentifier()?;
            buf.push(i);
            if parser.tokens.isPeekType(Namespace) {
                parser.tokens.getAssert(Namespace)?;
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

impl ParsingUnit<ASTNode, TokenType, VIPLParsingState> for CallableParsingUnit {
    fn getType(&self) -> ParsingUnitSearchType {
        Behind
    }

    fn canParse(
        &self,
        parser: &VIPLParser
    ) -> bool {
        parser.tokens.isPeekType(ORB) && parser.isPrevCallable()
    }

    fn parse(
        &self,
        parser: &mut VIPLParser
    ) -> Result<ASTNode, ParserError<TokenType>> {
        let s2 = unsafe { &mut *(parser as *mut Parser<_, _, _>) };

        parser.tokens.getAssert(ORB)?;

        let mut args = vec![];

        while !parser.tokens.isPeekType(TokenType::CRB) {
            let res = parser.parseOne(Ahead)?;
            let par = parser.getParsingUnit(Around);

            let op = match par {
                None => res,
                Some(p) => p.parse(s2)?,
            };

            args.push(op.asExpr()?);
            if !parser.tokens.isPeekType(TokenType::CRB) {
                parser.tokens.getAssert(TokenType::Comma)?;
            }
        }

        parser.tokens.getAssert(TokenType::CRB)?;

        // FIXME
        Ok(ASTNode::Expr(Expression::Callable(
            Box::new(parser.prevPop()?.asExpr()?),
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

impl ParsingUnit<ASTNode, TokenType, VIPLParsingState> for LambdaParsingUnit {
    fn getType(&self) -> ParsingUnitSearchType {
        Ahead
    }

    fn canParse(
        &self,
        parser: &VIPLParser
    ) -> bool {
        parser.tokens.isPeekType(Fn) && parser.tokens.isPeekIndexType(ORB, 1)
    }

    fn parse(
        &self,
        parser: &mut VIPLParser
    ) -> Result<ASTNode, ParserError<TokenType>> {
        parser.tokens.getAssert(Fn)?;

        let mut args = vec![];
        let mut argCount = 0;
        let mut returnType = None;

        parser.tokens.getAssert(ORB)?;
        while !parser.tokens.isPeekType(CRB) {
            let argName = parser.tokens.getIdentifier()?;
            parser.tokens.getAssert(Colon)?;

            let t = parser.parseDataType()?;

            args.push(VariableMetadata::new(argName, t));
            argCount += 1;
            if parser.tokens.isPeekType(Comma) {
                parser.tokens.consume();
            }
        }
        parser.tokens.getAssert(CRB)?;

        if parser.tokens.isPeekType(Colon) {
            parser.tokens.getAssert(Colon)?;
            returnType = Some(parser.parseDataType()?);
        }

        let mut isOneLine = false;

        let body = if parser.tokens.isPeekType(Equals) {
            parser.tokens.getAssert(Equals)?;
            isOneLine = true;
            Body::new(vec![ast::Statement::Return(parser.parseExpr()?)])
        } else {
            parser.parseBody()?
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
struct StructInitParsingUnit;

impl ParsingUnit<ASTNode, TokenType, VIPLParsingState> for StructInitParsingUnit {
    fn getType(&self) -> ParsingUnitSearchType {
        Ahead
    }

    fn canParse(
        &self,
        parser: &VIPLParser
    ) -> bool {
        parser.tokens.isPeekType(Identifier) && parser.tokens.isPeekIndexType(OCB, 1)
    }

    fn parse(
        &self,
        parser: &mut VIPLParser
    ) -> Result<ASTNode, ParserError<TokenType>> {
        let s2: &mut VIPLParser = unsafe { &mut *(parser as *mut Parser<_, _, _>) };

        let name = parser.tokens.getIdentifier()?;


        parser.tokens.getAssert(OCB)?;

        let inits = parser.tokens.parseManyWithSeparatorUntil(
            |it| {
                let fieldName = it.getIdentifier()?;
                it.getAssert(Colon)?;
                let initializer = s2.parseOne(Ahead)?.asExpr()?;

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

impl ParsingUnit<ASTNode, TokenType, VIPLParsingState> for FieldAccessParsingUnit {
    fn getType(&self) -> ParsingUnitSearchType {
        Behind
    }

    fn canParse(
        &self,
        parser: &VIPLParser
    ) -> bool {
        parser.tokens.isPeekType(Dot)
    }

    fn parse(
        &self,
        parser: &mut VIPLParser
    ) -> Result<ASTNode, ParserError<TokenType>> {
        parser.tokens.getAssert(Dot)?;
        let fieldName = parser.tokens.getIdentifier()?;

        Ok(ASTNode::Expr(Expression::FieldAccess(
            Box::new(parser.prevPop()?.asExpr()?),
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

impl ParsingUnit<ASTNode, TokenType, VIPLParsingState> for AssignableParsingUnit {
    fn getType(&self) -> ParsingUnitSearchType {
        Around
    }

    fn canParse(
        &self,
        parser: &VIPLParser
    ) -> bool {
        parser.tokens.isPeekType(Equals)
            || parser.tokens.isPeekType(AddAs)
            || parser.tokens.isPeekType(SubAs)
            || parser.tokens.isPeekType(DivAs)
            || parser.tokens.isPeekType(MulAs)
                && parser.isPrevAssignable()
    }

    fn parse(
        &self,
        parser: &mut VIPLParser
    ) -> Result<ASTNode, ParserError<TokenType>> {
        let prev = parser.prevPop()?.asExpr()?;

        let mut typ = None;

        if parser.tokens.isPeekType(AddAs) {
            typ = Some(ArithmeticOp::Add);
        } else if parser.tokens.isPeekType(SubAs) {
            typ = Some(ArithmeticOp::Sub);
        } else if parser.tokens.isPeekType(MulAs) {
            typ = Some(ArithmeticOp::Mul);
        } else if parser.tokens.isPeekType(DivAs) {
            typ = Some(ArithmeticOp::Div);
        }
        parser.tokens.consume();

        let next = parser.parseOne(Ahead)?.asExpr()?;

        // FIXME
        Ok(ASTNode::Statement(Statement::Assignable(
            prev,
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

impl ParsingUnit<ASTNode, TokenType, VIPLParsingState> for GlobalParsingUnit {
    fn getType(&self) -> ParsingUnitSearchType {
        Ahead
    }

    fn canParse(
        &self,
        parser: &VIPLParser
    ) -> bool {
        parser.tokens.isPeekType(TokenType::Global)
    }

    fn parse(
        &self,
        parser: &mut VIPLParser
    ) -> Result<ASTNode, ParserError<TokenType>> {
        let s2 = unsafe { &mut *(parser as *mut Parser<_, _, _>) };

        parser.tokens.getAssert(Global)?;

        let name = parser.tokens.getIdentifier()?;
        let mut typeHint = None;

        if parser.tokens.isPeekType(Colon) {
            parser.tokens.getAssert(Colon)?;
            typeHint = Some(parser.parseDataType()?);
        }

        parser.tokens.getAssert(TokenType::Equals)?;

        let res = parser.parseOne(Ahead)?;
        let par = parser.getParsingUnit(Around);

        let op = match par {
            None => res.asExpr()?,
            Some(p) => p.parse(s2)?.asExpr()?,
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

impl ParsingUnit<ASTNode, TokenType, VIPLParsingState> for ForParsingUnit {
    fn getType(&self) -> ParsingUnitSearchType {
        Ahead
    }

    fn canParse(
        &self,
        parser: &VIPLParser
    ) -> bool {
        parser.tokens.isPeekType(For)
    }

    fn parse(
        &self,
        parser: &mut VIPLParser
    ) -> Result<ASTNode, ParserError<TokenType>> {
        parser.tokens.getAssert(For)?;

        let varName = parser.tokens.getIdentifier()?;

        parser.tokens.getAssert(In)?;

        let res = parser.parseOne(Ahead)?.asExpr()?;

        let body = parser.parseBody()?;

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

impl ParsingUnit<ASTNode, TokenType, VIPLParsingState> for NullParsingUnit {
    fn getType(&self) -> ParsingUnitSearchType {
        Ahead
    }

    fn canParse(
        &self,
        parser: &VIPLParser
    ) -> bool {
        parser.tokens.isPeekType(Null)
    }

    fn parse(
        &self,
        parser: &mut VIPLParser
    ) -> Result<ASTNode, ParserError<TokenType>> {
        parser.tokens.getAssert(Null)?;

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

impl ParsingUnit<ASTNode, TokenType, VIPLParsingState> for OneArgFunctionParsintUnit {
    fn getType(&self) -> ParsingUnitSearchType {
        Behind
    }

    fn canParse(
        &self,
        parser: &VIPLParser
    ) -> bool {
        parser.tokens.isPeekTypeOf(|it| VALID_EXPRESSION_TOKENS.contains(&it))
            && parser.isPrevCallable()
            && parser.tokens.isPeekSameRow()
    }

    fn parse(
        &self,
        parser: &mut VIPLParser
    ) -> Result<ASTNode, ParserError<TokenType>> {
        let prev = parser.prevPop()?.asExpr()?;

        let arg = parser.parseExprOneLine()?;

        return Ok(ASTNode::Expr(Expression::Callable(
            Box::new(prev),
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

impl ParsingUnit<ASTNode, TokenType, VIPLParsingState> for TwoArgFunctionParsintUnit {
    fn getType(&self) -> ParsingUnitSearchType {
        Around
    }

    fn canParse(
        &self,
        parser: &VIPLParser
    ) -> bool {
        parser.isPrevExp()
            && parser.tokens.isPeekSameRow()
            && parser.tokens.isPeekType(Identifier)
            && parser.tokens.isPeekOffsetOneOf(&VALID_EXPRESSION_TOKENS, 1)
    }

    fn parse(
        &self,
        parser: &mut VIPLParser
    ) -> Result<ASTNode, ParserError<TokenType>> {
        let prev = parser.prevPop()?.asExpr()?;
        let name = parser.tokens.getIdentifier()?;
        let arg = parser.parseExprOneLine()?;

        return Ok(ASTNode::Expr(Expression::Callable(
            Box::new(Expression::Variable(name)),
            vec![prev, arg],
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

impl ParsingUnit<ASTNode, TokenType, VIPLParsingState> for TernaryOperatorParsingUnit {
    fn getType(&self) -> ParsingUnitSearchType {
        Around
    }

    fn canParse(
        &self,
        parser: &VIPLParser
    ) -> bool {
        parser.tokens.isPeekType(QuestionMark) && parser.isPrevExp()
    }

    fn parse(
        &self,
        parser: &mut VIPLParser
    ) -> Result<ASTNode, ParserError<TokenType>> {
        let prev = parser.prevPop()?.asExpr()?;
        parser.tokens.getAssert(QuestionMark)?;

        let a = parser.parseExpr()?;

        parser.tokens.getAssert(Colon)?;

        let b = parser.parseExpr()?;

        Ok(ASTNode::Expr(Expression::TernaryOperator(
            Box::new(prev),
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

impl ParsingUnit<ASTNode, TokenType, VIPLParsingState> for RepeatParsingUnit {
    fn getType(&self) -> ParsingUnitSearchType {
        Ahead
    }

    fn canParse(
        &self,
        parser: &VIPLParser
    ) -> bool {
        parser.tokens.isPeekType(Repeat)
    }

    fn parse(
        &self,
        parser: &mut VIPLParser
    ) -> Result<ASTNode, ParserError<TokenType>> {
        parser.tokens.getAssert(Repeat)?;
        let varName = parser.tokens.getIdentifier()?;
        let count = parser.tokens
            .getAssert(TokenType::IntLiteral)?
            .str
            .parse::<usize>().map_err(|_|ParserError::Unknown("".into()))?;

        let body = parser.parseBody()?;

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

impl ParsingUnit<ASTNode, TokenType, VIPLParsingState> for FunctionParsingUnit {
    fn getType(&self) -> ParsingUnitSearchType {
        ParsingUnitSearchType::Ahead
    }

    fn canParse(
        &self,
        parser: &VIPLParser
    ) -> bool {
        parser.tokens.isPeekType(TokenType::Fn) && parser.tokens.isPeekIndexType(Identifier, 1)
    }

    fn parse(
        &self,
        parser: &mut VIPLParser
    ) -> Result<ASTNode, ParserError<TokenType>> {
        let mut isNative = false;

        parser.tokens.getAssert(TokenType::Fn)?;

        let name = parser.tokens.getIdentifier()?;
        let mut args = vec![];
        let mut argCount = 0;
        let mut returnType = None;

        parser.tokens.getAssert(ORB)?;
        while !parser.tokens.isPeekType(CRB) {
            let argName = parser.tokens.getIdentifier()?;
            parser.tokens.getAssert(Colon)?;

            let t = parser.parseDataType()?;

            args.push(VariableMetadata {
                name: argName,
                typ: t,
            });
            argCount += 1;
            if parser.tokens.isPeekType(Comma) {
                parser.tokens.consume();
            }
        }
        parser.tokens.getAssert(CRB)?;

        if parser.tokens.isPeekType(Colon) {
            parser.tokens.getAssert(Colon)?;
            returnType = Some(parseDataType(&mut parser.tokens)?);
        }

        let mut isOneLine = false;

        let body = if parser.tokens.isPeekType(Equals) {
            parser.tokens.getAssert(Equals)?;
            isOneLine = true;
            Body::new(vec![ast::Statement::Return(parser.parseExpr()?)])
        } else {
            parser.parseBody()?
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

impl ParsingUnit<ASTNode, TokenType, VIPLParsingState> for ArithmeticParsingUnit {
    fn getType(&self) -> ParsingUnitSearchType {
        Around
    }

    fn canParse(
        &self,
        parser: &VIPLParser
    ) -> bool {
        parser.tokens.isPeekType(self.typ)
    }

    fn parse(
        &self,
        parser: &mut VIPLParser
    ) -> Result<ASTNode, ParserError<TokenType>> {
        let prev = parser.prevPop()?.asExpr()?;
        parser.tokens.consume();
        let res = parser.parseOneJust(Ahead)?;

        parser.previousBuf.push(res);

        let par = parser.getParsingUnit(Around);

        match par {
            None => {
                let res = parser.prevPop()?;
                Ok(ASTNode::Expr(Expression::BinaryOperation {
                    // FIXME
                    left: Box::new(prev),
                    right: Box::new(res.asExpr()?),
                    op: self.op.clone(),
                }))
            },
            Some(p) => unsafe {
               if self.priority < p.getPriority() {
                   let xd = &mut *(parser as *const VIPLParser as *mut VIPLParser);

                   let res = xd.prevPop()?;

                   xd.previousBuf.push(
                       ASTNode::Expr(Expression::BinaryOperation {
                           left: Box::new(prev),
                           right: Box::new(res.asExpr()?),
                           op: self.op.clone(),
                       }
                   ));

                   Ok(p.parse(xd)?)
                } else {
                   Ok(ASTNode::Expr(Expression::BinaryOperation {
                       left: Box::new(prev),
                       right: Box::new(p.parse(&mut *(parser as *const VIPLParser as *mut VIPLParser))?.asExpr()?),
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

impl ParsingUnit<ASTNode, TokenType, VIPLParsingState> for NumericParsingUnit {
    fn getType(&self) -> ParsingUnitSearchType {
        Ahead
    }

    fn canParse(
        &self,
        parser: &VIPLParser
    ) -> bool {
        let peek = match parser.tokens.peekOne() {
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

        let peek1 = match parser.tokens.peekIndex(1) {
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
        parser: &mut VIPLParser
    ) -> Result<ASTNode, ParserError<TokenType>> {
        let mut peek = parser.tokens.peekOneRes()?;

        let mut buf = String::new();

        if peek.typ == Minus {
            buf.push('-');
            parser.tokens.consume();
            peek = parser.tokens.peekOneRes()?
        }

        let res = match peek.typ {
            TokenType::IntLiteral => {
                buf.push_str(&peek.str);
                ASTNode::Expr(Expression::IntLiteral(buf))
            }
            TokenType::LongLiteral => {
                buf.push_str(&peek.str);
                ASTNode::Expr(Expression::IntLiteral(buf))
            }
            TokenType::FloatLiteral => {
                buf.push_str(&peek.str);
                ASTNode::Expr(Expression::FloatLiteral(buf))
            }
            TokenType::DoubleLiteral => {
                buf.push_str(&peek.str);
                ASTNode::Expr(Expression::FloatLiteral(buf))
            }
            _ => {
                return Err(ParserError::InvalidToken(InvalidToken {
                    expected: TokenType::IntLiteral,
                    actual: Some(peek.clone()),
                }))
            }
        };
        parser.tokens.consume();
        Ok(res)
    }

    fn getPriority(&self) -> usize {
        usize::MAX
    }

    fn setPriority(&mut self, _priority: usize) {}
}

#[derive(Debug)]
struct TypeCastParsingUnit;

impl ParsingUnit<ASTNode, TokenType, VIPLParsingState> for TypeCastParsingUnit {
    fn getType(&self) -> ParsingUnitSearchType { Behind }

    fn canParse(&self, parser: &Parser<TokenType, ASTNode, VIPLParsingState>) -> bool {
        parser.isPrevExp() && parser.tokens.isPeekType(As)
    }

    fn parse(&self, parser: &mut Parser<TokenType, ASTNode, VIPLParsingState>) -> Result<ASTNode, ParserError<TokenType>> {
        let e = parser.prevPop()?.asExpr()?;
        parser.tokens.getAssert(As)?;

        let t = parser.parseDataType()?;

        Ok(ASTNode::Expr(Expression::TypeCast(Box::new(e), t)))
    }

    fn getPriority(&self) -> usize {
        todo!()
    }

    fn setPriority(&mut self, priority: usize) {
        todo!()
    }
}

pub fn parsingUnits() -> Vec<Box<dyn ParsingUnit<ASTNode, TokenType, VIPLParsingState>>> {
    vec![
        Box::new(AssignableParsingUnit),
        Box::new(StructInitParsingUnit),
        Box::new(NamespaceParsingUnit),
        Box::new(WhileParsingUnit),
        Box::new(LoopParsingUnit),
        Box::new(FunctionParsingUnit),
        Box::new(ArrayIndexingParsingUnit),
        Box::new(OneArgFunctionParsintUnit),
        Box::new(ArrayLiteralParsingUnit),
        Box::new(TypeCastParsingUnit),
        Box::new(NumericParsingUnit),
        Box::new(CharParsingUnit),
        Box::new(StringParsingUnit),
        Box::new(CallableParsingUnit),
        Box::new(BreakParsingUnit),
        Box::new(NotParsingUnit),
        Box::new(ContinueParsingUnit),
        Box::new(NamespaceImport),
        Box::new(SymbolImport),
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
        Box::new(VariableParsingUnit),
        Box::new(IfParsingUnit),
        Box::new(BoolParsingUnit),
        Box::new(ReturnParsingUnit),
        Box::new(StructParsingUnit),
        Box::new(FieldAccessParsingUnit),
        Box::new(GlobalParsingUnit),
        Box::new(ForParsingUnit),
        Box::new(LambdaParsingUnit),
        Box::new(TwoArgFunctionParsintUnit),
        Box::new(TernaryOperatorParsingUnit),
        Box::new(RepeatParsingUnit),
    ]
}
