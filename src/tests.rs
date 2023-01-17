#[cfg(test)]
mod tests {
    use crate::ast::Op;
    use crate::codegen::bytecodeGen;
    use crate::lexer::{lexingUnits, SourceProvider, tokenize, tokenizeSource, TokenType};
    use crate::parser::ParsingUnitSearchType::Ahead;
    use crate::parser::*;
    use crate::vm::{bootStrapVM, evaluateBytecode, run, SeekableOpcodes, StackFrame};

    #[test]
    fn testParser() {
        let lexingUnits = lexingUnits();
        let input = ";0 = lol fn main() { ;-420.69 = x print(69*x) while x == 1 { print(69) } if true { test(1) } else { kys(1) }}";

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
            Box::new(ArithmeticParsingUnit {
                op: Op::Mul,
                typ: TokenType::Mul,
            }),
            Box::new(ArithmeticParsingUnit {
                op: Op::Div,
                typ: TokenType::Div,
            }),
            Box::new(ArithmeticParsingUnit {
                op: Op::Add,
                typ: TokenType::Plus,
            }),
            Box::new(ArithmeticParsingUnit {
                op: Op::Sub,
                typ: TokenType::Minus,
            }),
            Box::new(ArithmeticParsingUnit {
                op: Op::Eq,
                typ: TokenType::Eq,
            }),
            Box::new(ArithmeticParsingUnit {
                op: Op::Less,
                typ: TokenType::Less,
            }),
            Box::new(ArithmeticParsingUnit {
                op: Op::Gt,
                typ: TokenType::Gt,
            }),
            Box::new(VariableParsingUnit),
            Box::new(IfParsingUnit),
            Box::new(BoolParsingUnit),
        ];
        let tokens = tokenize(&mut lexingUnits.into_boxed_slice(), src);
        println!("tokens {:?}", &tokens);
        let res = parse(
            &mut TokenProvider { tokens, index: 0 },
            Ahead,
            &parsers.into_boxed_slice(),
        );
        println!("{:?}", &res)
    }


    #[test]
    fn basicPrint() {
        let lexingUnits = lexingUnits();
        let input = "print(69)";

        let tokens = tokenizeSource(input);
        println!("tokens {:?}", &tokens);

        let res = parseBytecode(tokens);
        println!("{:?}", &res);

        let bs = bytecodeGen(res);
        println!("{:?}", &bs.0);

        evaluateBytecode(bs.0, bs.1);
    }
}