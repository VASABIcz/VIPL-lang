use crate::vm::{StackFrame, VirtualMachine};

#[test]
fn testNumericLexingUnit() {
    let input = "5 -5 5. -5. 5.5 8.5 5f 5L 6D";

    let tokens = tokenizeSource(input).unwrap();

    assert_eq!(tokens[0].typ, TokenType::IntLiteral);
    assert_eq!(tokens[1].typ, TokenType::IntLiteral);

    assert_eq!(tokens[2].typ, TokenType::FloatLiteral);
    assert_eq!(tokens[3].typ, TokenType::FloatLiteral);
    assert_eq!(tokens[4].typ, TokenType::FloatLiteral);
    assert_eq!(tokens[5].typ, TokenType::FloatLiteral);

    assert_eq!(tokens[6].typ, TokenType::FloatLiteral);
    assert_eq!(tokens[7].typ, TokenType::LongLiteral);
    assert_eq!(tokens[8].typ, TokenType::DoubleLiteral);
}

#[test]
fn testStringLexingUnit() {
    let input = "\"UwU\" \'A\'";

    let tokens = tokenizeSource(input).unwrap();
    // println!("{:?}", &tokens);

    assert_eq!(tokens[0].typ, TokenType::StringLiteral);
    assert_eq!(tokens[1].typ, TokenType::CharLiteral);
}

#[test]
fn testOperationPriority() {
    let inp = "x = 2+3*4 assert(x, 14)";
    let tokens = tokenizeSource(inp).unwrap();
    println!("tokens {:?}", &tokens);

    let res = parseTokens(tokens).unwrap();
    println!("ast {:?}", &res);

    let bs = bytecodeGen(res).unwrap();
    println!("bytecodes {:?}", &bs.0);
    evaluateBytecode(bs.0, bs.1);
}

#[test]
fn testOperationBracketPriority() {
    let inp = "x = (2+3)*4 assert(x, 20)";
    let tokens = tokenizeSource(inp).unwrap();
    println!("tokens {:?}", &tokens);

    let res = parseTokens(tokens).unwrap();
    println!("ast {:?}", &res);

    let bs = bytecodeGen(res).unwrap();
    println!("bytecodes {:?}", &bs.0);
    evaluateBytecode(bs.0, bs.1);
}

#[test]
fn basicPrint() {
    let input = "print(69)";

    let tokens = tokenizeSource(input).unwrap();
    println!("tokens {:?}", &tokens);

    let res = parseTokens(tokens).unwrap();
    println!("{:?}", &res);

    let bs = bytecodeGen(res).unwrap();
    println!("{:?}", &bs.0);

    evaluateBytecode(bs.0, bs.1);
}

#[test]
fn testFunctionReturn() {
    let input = "fn mult(a: int, b: int): int { return a*b }  print(mult(5, 5))";

    let tokens = tokenizeSource(input).unwrap();
    println!("tokens {:?}", &tokens);

    let res = parseTokens(tokens).unwrap();
    println!("{:?}", &res);

    let bs = bytecodeGen(res).unwrap();
    println!("{:?}", &bs.0);

    evaluateBytecode(bs.0, bs.1);
}

extern "C" fn externFn(_v: &mut VirtualMachine, _l: &mut StackFrame) {
    println!("native :3")
}

#[test]
fn testFFI() {
    use crate::ffi::*;
    let vm = createVm();
    test(vm);

    dropVm(vm)
}

#[test]
fn testMakeNativeFFI() {
    use crate::ffi::*;
    let vm = createVm();

    let name = "uwu";
    unsafe { registerNative(&mut *vm, name.as_ptr(), name.len(), null(), 0, externFn); }

    let sc = "uwu()";

    unsafe { evaluate(&mut *vm, sc.as_ptr(), sc.len()) };


    dropVm(vm)
}