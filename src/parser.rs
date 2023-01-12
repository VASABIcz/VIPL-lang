
use crate::{ast, DataType, OpCode, VariableMetadata};
use crate::ast::{Expression, FunctionCall, Node, Op, Statement, While};
use crate::ast::Expression::IntLiteral;
use crate::ast::Statement::VariableCreate;
use crate::lexer::{lexingUnits, SourceProvider, Token, tokenize, TokenType};
use crate::lexer::TokenType::{CCB, Colon, CRB, DoubleLiteral, FloatLiteral, Identifier, If, LongLiteral, OCB, ORB};
use crate::parser::Operation::{FunctionDef, Statement};
use crate::parser::ParsingUnitSearchType::{Ahead, Around};

struct Parser {
    parsingUnits: Vec<Box<dyn ParsingUnit>>
}

struct  ParsingU {

}

fn getParsingUnit<'a>(tokens: &mut TokenProvider, typ: ParsingUnitSearchType, parsingUnits: &'a [Box<dyn ParsingUnit>]) -> Option<&'a Box<dyn ParsingUnit>> {
    parsingUnits.iter().find(|it| {
        let parserType = it.getType();

        let canParse = match typ {
            ParsingUnitSearchType::Around => {
                parserType != ParsingUnitSearchType::Ahead
            }
            ParsingUnitSearchType::Back => {
                parserType == ParsingUnitSearchType::Back
            }
            ParsingUnitSearchType::Ahead => {
                parserType == ParsingUnitSearchType::Ahead
            }
        };

        canParse && it.canParse(tokens)
    })
}

fn parseOne(tokens: &mut TokenProvider, typ: ParsingUnitSearchType, parsingUnits: &[Box<dyn ParsingUnit>], previous: Option<Operation>) -> Option<Operation> {
    let u = getParsingUnit(tokens, typ, parsingUnits);

    if u.is_none() {
        println!("next {:?}", tokens.peekOne());
    }

    u.map(|v| v.parse(tokens, previous, parsingUnits))
}

fn parse(tokens: &mut TokenProvider, typ: ParsingUnitSearchType, parsingUnits: &[Box<dyn ParsingUnit>]) -> Vec<Operation> {
    let mut buf = vec![];

    'main: while !tokens.isDone() {
        for unit in parsingUnits.iter() {
            let parserType = unit.getType();

            let canParse = match typ {
                ParsingUnitSearchType::Around => {
                    true
                }
                ParsingUnitSearchType::Back => {
                    parserType == ParsingUnitSearchType::Back
                }
                ParsingUnitSearchType::Ahead => {
                    parserType == ParsingUnitSearchType::Ahead
                }
            };

            if canParse && unit.canParse(&tokens) {
                let res = unit.parse(tokens, None, parsingUnits);
                buf.push(res);
                continue 'main
            }
        }
        panic!("error {:?}", tokens.peekOne());
    }
    buf
}


/*
fn parse(parsingUnits: &mut [Box<dyn ParsingUnit>], mut tokens: TokenProvider, typ: ParsingUnitSearchType) -> Vec<Operation> {
    let mut buf = vec![];

    'main: while !tokens.isDone() {
        for unit in parsingUnits.iter_mut() {
            let parserType = unit.getType();

            let canParse = match typ {
                ParsingUnitSearchType::Around => {
                    true
                }
                ParsingUnitSearchType::Back => {
                    parserType == ParsingUnitSearchType::Back
                }
                ParsingUnitSearchType::Ahead => {
                    parserType == ParsingUnitSearchType::Ahead
                }
            };

            if unit.canParse(&tokens) {
                let res = unit.parse(&mut tokens, None);
                buf.push(res);
                continue 'main
            }
        }
        panic!("error {:?}", tokens.peekOne());
    }
    buf
}

 */

pub struct TokenProvider {
    tokens: Vec<Token>,
    index: usize
}

impl TokenProvider {
    fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            index: 0,
        }
    }

    fn peekOne(&self) -> &Token {
        self.tokens.get(self.index).unwrap()
    }

    fn peekIndex(&self, offset: usize) -> &Token {
        self.tokens.get(self.index + offset).unwrap()
    }

    fn consume(&mut self) {
        self.index += 1
    }

    fn getAssert(&mut self, typ: TokenType) -> &Token {
        let i = self.index;
        self.consume();
        let t = self.tokens.get(i).unwrap();
        if t.typ != typ {
            panic!("invalid token got {:?} expected {:?}", t, typ)
        }
        t
    }
    fn isPeekType(&self, typ: TokenType) -> bool {
        let t = self.peekOne();

        t.typ == typ
    }

    fn isPeekIndexType(&self, typ: TokenType, offset: usize) -> bool {
        let t = self.peekIndex(offset);

        t.typ == typ
    }

    fn getIdentifier(&mut self) -> String {
        let t = self.getAssert(TokenType::Identifier);

        t.str.clone()
    }

    fn isDone(&self) -> bool {
        self.index >= self.tokens.len()
    }
}

#[derive(Debug)]
enum Operation {
    FunctionDef(Node),
    Statement(Statement),
    Expression(Expression)
}

impl Operation {
    fn asExpr(self) -> Expression {
        match self {
            Operation::Expression(e) => e,
            _ => panic!()
        }
    }

    fn asStatement(self) -> Statement {
        match self {
            Operation::Statement(s) => s,
            _ => panic!()
        }
    }
}

#[derive(Eq, PartialEq)]
enum ParsingUnitSearchType {
    Around,
    Back,
    Ahead
}


trait ParsingUnit {
    fn getType(&self) -> ParsingUnitSearchType;

    fn canParse(&self, tokenProvider: &TokenProvider) -> bool;

    fn parse(&self, tokenProvider: &mut TokenProvider, previous: Option<Operation>, parser: &[Box<dyn ParsingUnit>]) -> Operation;
}

struct FunctionParsingUnit;

impl ParsingUnit for FunctionParsingUnit {
    fn getType(&self) -> ParsingUnitSearchType {
        ParsingUnitSearchType::Ahead
    }

    fn canParse(&self, tokenProvider: &TokenProvider) -> bool {
        tokenProvider.isPeekType(TokenType::Fn)
    }

    fn parse(&self, tokens: &mut TokenProvider, previous: Option<Operation>, parser: &[Box<dyn ParsingUnit>]) -> Operation {
        tokens.getAssert(TokenType::Fn);
        let name = tokens.getIdentifier();
        let mut args = vec![];
        let mut argCount = 0;
        let mut returnType = None;

        tokens.getAssert(TokenType::ORB);
        while !tokens.isPeekType(TokenType::CRB) {
            let argName = tokens.getIdentifier();
            tokens.getAssert(TokenType::Colon);
            let argType = tokens.getIdentifier();
            let t = DataType::fromString(&argType);
            args.push(VariableMetadata { name: argName, typ: t });
            argCount += 1
        }
        tokens.getAssert(CRB);

        if tokens.isPeekType(Colon) {
            returnType = Some(DataType::fromString(&tokens.getIdentifier()));
        }

        tokens.getAssert(TokenType::OCB);

        let mut statements = vec![];

        while !tokens.isPeekType(TokenType::CCB) {
            println!("parseeeeee {:?}", tokens.peekOne());
            match parseOne(tokens, ParsingUnitSearchType::Ahead, parser, None).unwrap() {
                Operation::Statement(s) => {
                    statements.push(s)
                }
                Operation::Expression(e) => {
                    match e {
                        Expression::FunctionCall(c) => {
                            statements.push(Statement::FunctionExpr(c))
                        }
                        e => panic!("not a statement {:?}", e),
                    }
                }
                s => panic!("not a statement {:?}", s)
            }
        }

        tokens.getAssert(TokenType::CCB);

        Operation::FunctionDef(Node::FunctionDef(crate::ast::FunctionDef {
            name,
            args,
            argCount,
            body: statements,
            returnType,
        }))
    }
}

struct StatementVarCreateParsingUnit;

impl ParsingUnit for StatementVarCreateParsingUnit {
    fn getType(&self) -> ParsingUnitSearchType {
        ParsingUnitSearchType::Ahead
    }

    fn canParse(&self, tokens: &TokenProvider) -> bool {
        tokens.isPeekType(TokenType::Identifier) && tokens.isPeekIndexType(TokenType::Equals, 1)
    }

    fn parse(&self, tokens: &mut TokenProvider, previous: Option<Operation>, parser: &[Box<dyn ParsingUnit>]) -> Operation {
        let name = tokens.getIdentifier();
        tokens.getAssert(TokenType::Equals);

        let res = parseOne(tokens, ParsingUnitSearchType::Ahead, parser, None);
        let par = getParsingUnit(tokens, Around, parser);

        let op = match par {
            None => {
                res.unwrap()
            }
            Some(p) => {
                p.parse(tokens, res, parser)
            }
        };

        match op {
            Operation::Expression(e) => {
                Operation::Statement(VariableCreate(crate::ast::VariableCreate {
                    name,
                    init: Some(e),
                }))
            }
            _ => panic!("not a statement {:?}", op)
        }
    }
}

struct CallParsingUnit;

impl ParsingUnit for CallParsingUnit {
    fn getType(&self) -> ParsingUnitSearchType {
        Ahead
    }

    fn canParse(&self, tokenProvider: &TokenProvider) -> bool {
        tokenProvider.isPeekType(Identifier) && tokenProvider.isPeekIndexType(ORB, 1)
    }

    fn parse(&self, tokens: &mut TokenProvider, previous: Option<Operation>, parser: &[Box<dyn ParsingUnit>]) -> Operation {
        if previous.is_some() {
            panic!("previous {:?} {:?}", previous, tokens.peekOne())
        }
        let name = tokens.getIdentifier();
        tokens.getAssert(TokenType::ORB);

        let mut args = vec![];

        while !tokens.isPeekType(TokenType::CRB) {
            let res = parseOne(tokens, Ahead, parser, None);
            let par = getParsingUnit(tokens, Around, parser);

            let op = match par {
                None => {
                    res.unwrap()
                }
                Some(p) => {
                    p.parse(tokens, Some(res.unwrap()), parser)
                }
            };

            match op {
                Operation::Expression(e) => {
                    args.push(e);
                    if !tokens.isPeekType(TokenType::CRB) {
                        tokens.getAssert(TokenType::Comma);
                    }
                }
                _ => panic!("not a expression")
            }
        }

        tokens.getAssert(TokenType::CRB);


        Operation::Expression(Expression::FunctionCall(FunctionCall {
            name,
            arguments: args,
        }))
    }
}

struct ArithmeticParsingUnit {
    op: Op,
    typ: TokenType
}

impl ParsingUnit for ArithmeticParsingUnit {
    fn getType(&self) -> ParsingUnitSearchType {
        Around
    }

    fn canParse(&self, tokenProvider: &TokenProvider) -> bool {
        tokenProvider.isPeekType(self.typ)
    }

    fn parse(&self, tokens: &mut TokenProvider, previous: Option<Operation>, parser: &[Box<dyn ParsingUnit>]) -> Operation {
        tokens.consume();
        let res = parseOne(tokens, Ahead, parser, None);
        let par = getParsingUnit(tokens, Around, parser);

        match par {
            None => {
                Operation::Expression(Expression::ArithmeticOp {
                    left: Box::new(previous.unwrap().asExpr()),
                    right: Box::new(res.unwrap().asExpr()),
                    op: self.op.clone(),
                })
            }
            Some(p) => {
                p.parse(tokens, res, parser)
            }
        }
    }
}

struct NumericParsingUnit;

impl ParsingUnit for NumericParsingUnit {
    fn getType(&self) -> ParsingUnitSearchType {
        Ahead
    }

    fn canParse(&self, tokenProvider: &TokenProvider) -> bool {
        let peek = tokenProvider.peekOne().typ;

        peek == TokenType::IntLiteral || peek == TokenType::LongLiteral || peek == TokenType::FloatLiteral || peek == TokenType::DoubleLiteral
    }

    fn parse(&self, tokenProvider: &mut TokenProvider, previous: Option<Operation>, parser: &[Box<dyn ParsingUnit>]) -> Operation {
        let peek = tokenProvider.peekOne();
        let res = match peek.typ {
            TokenType::IntLiteral => Operation::Expression(IntLiteral(peek.str.clone())),
            TokenType::LongLiteral => Operation::Expression(Expression::LongLiteral(peek.str.clone())),
            TokenType::FloatLiteral => Operation::Expression(Expression::FloatLiteral(peek.str.clone())),
            TokenType::DoubleLiteral => Operation::Expression(Expression::DoubleLiteral(peek.str.clone())),
            _ => panic!()
        };
        tokenProvider.consume();
        res
    }
}

struct VariableParsingUnit;

impl ParsingUnit for VariableParsingUnit {
    fn getType(&self) -> ParsingUnitSearchType {
        Ahead
    }

    fn canParse(&self, tokenProvider: &TokenProvider) -> bool {
        tokenProvider.isPeekType(Identifier)
    }

    fn parse(&self, tokenProvider: &mut TokenProvider, previous: Option<Operation>, parser: &[Box<dyn ParsingUnit>]) -> Operation {
        Operation::Expression(Expression::Variable(tokenProvider.getIdentifier()))
    }
}

struct WhileParsingUnit;

impl ParsingUnit for WhileParsingUnit {
    fn getType(&self) -> ParsingUnitSearchType {
        Ahead
    }

    fn canParse(&self, tokenProvider: &TokenProvider) -> bool {
        tokenProvider.isPeekType(TokenType::While)
    }

    fn parse(&self, tokenProvider: &mut TokenProvider, previous: Option<Operation>, parser: &[Box<dyn ParsingUnit>]) -> Operation {
        tokenProvider.getAssert(TokenType::While);
        tokenProvider.getAssert(TokenType::ORB);

        let res = parseOne(tokenProvider, Ahead, parser, None);
        let par = getParsingUnit(tokenProvider, Around, parser);

        let mut statements = vec![];

        let op = match par {
            None => {
                res.unwrap()
            }
            Some(p) => {
                p.parse(tokenProvider, Some(res.unwrap()), parser)
            }
        };

        tokenProvider.getAssert(TokenType::CRB);
        tokenProvider.getAssert(TokenType::OCB);

        while !tokenProvider.isPeekType(CCB) {
            statements.push(parseOne(tokenProvider, Ahead, parser, None).unwrap().asStatement());
        }

        tokenProvider.getAssert(TokenType::CCB);

        return Operation::Statement(Statement::While(While {
            exp: op.asExpr(),
            body: statements,
        }))
    }
}

struct IfParsingUnit;

impl ParsingUnit for IfParsingUnit {
    fn getType(&self) -> ParsingUnitSearchType {
        Ahead
    }

    fn canParse(&self, tokenProvider: &TokenProvider) -> bool {
        tokenProvider.isPeekType(TokenType::If)
    }

    fn parse(&self, tokenProvider: &mut TokenProvider, previous: Option<Operation>, parser: &[Box<dyn ParsingUnit>]) -> Operation {
        tokenProvider.getAssert(TokenType::If);
        tokenProvider.getAssert(TokenType::ORB);

        let res = parseOne(tokenProvider, Ahead, parser, None);
        let par = getParsingUnit(tokenProvider, Around, parser);

        let mut statements = vec![];

        let op = match par {
            None => {
                res.unwrap()
            }
            Some(p) => {
                p.parse(tokenProvider, Some(res.unwrap()), parser)
            }
        };

        tokenProvider.getAssert(TokenType::CRB);
        tokenProvider.getAssert(TokenType::OCB);

        while !tokenProvider.isPeekType(CCB) {
            statements.push(parseOne(tokenProvider, Ahead, parser, None).unwrap().asStatement());
        }

        tokenProvider.getAssert(TokenType::CCB);

        if !tokenProvider.isPeekType(TokenType::Else) {
            return Operation::Statement(ast::Statement::If(If {

            }))
        }
    }
}

#[test]
fn testParser() {
    let lexingUnits = lexingUnits();
    let input = "fn main() { x = -420.69 print(69*x) while (x == 1) { print(69) } if (true) { print(1) } }";

    let src = SourceProvider {
        data: input,
        index: 0,
    };

    let parsers: Vec<Box<dyn ParsingUnit>> = vec![
        Box::new(WhileParsingUnit),
        Box::new(FunctionParsingUnit),
        Box::new(StatementVarCreateParsingUnit),
        Box::new(NumericParsingUnit),
        Box::new(CallParsingUnit),
        Box::new(ArithmeticParsingUnit { op: Op::Mul, typ: TokenType::Mul }),
        Box::new(ArithmeticParsingUnit { op: Op::Div, typ: TokenType::Div }),
        Box::new(ArithmeticParsingUnit { op: Op::Add, typ: TokenType::Plus }),
        Box::new(ArithmeticParsingUnit { op: Op::Sub, typ: TokenType::Minus }),
        Box::new(ArithmeticParsingUnit { op: Op::Eq, typ: TokenType::Eq }),
        Box::new(ArithmeticParsingUnit { op: Op::Less, typ: TokenType::Less }),
        Box::new(ArithmeticParsingUnit { op: Op::Gt, typ: TokenType::Gt }),
        Box::new(VariableParsingUnit),
    ];

    let tokens = tokenize(&mut lexingUnits.into_boxed_slice(), src);

    let res = parse(&mut TokenProvider { tokens, index: 0 }, Ahead, &parsers.into_boxed_slice());
    println!("{:?}", res)
}