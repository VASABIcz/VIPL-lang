use std::ptr::null;
use std::time::Instant;

use crate::ast::{Expression, Op};
use crate::codegen::bytecodeGen;
use crate::lexer::{tokenizeSource, TokenType};
use crate::lexer::TokenType::IntLiteral;
use crate::parser::parseTokens;
use crate::std::bootStrapVM;
use crate::vm::{DataType, evaluateBytecode, MyStr, OpCode, StackFrame, Value, VariableMetadata, VirtualMachine};
use crate::vm::RawOpCode::PushInt;

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
    // println!("{:?}", &res);

    let bs = bytecodeGen(res).unwrap();
    // println!("{:?}", &bs.0);

    evaluateBytecode(bs.0, bs.1);
}

#[test]
fn testFunctionReturn() {
    let input = "fn mult(a: int, b: int): int { return a*b }  print(mult(5, 5))";

    let tokens = tokenizeSource(input).unwrap();
    println!("tokens {:?}", &tokens);

    let res = parseTokens(tokens).unwrap();
    // println!("{:?}", &res);

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

#[test]
fn testOptimization() {
    {
        let a = Expression::ArithmeticOp {
            left: box Expression::IntLiteral(String::from("4")),
            right: box Expression::IntLiteral(String::from("4")),
            op: Op::Mul,
        };
        let res = crate::optimizer::evalE(&a);

        assert_eq!(res, Some(Expression::IntLiteral(String::from("16"))));
    }

    {
        let a = Expression::ArithmeticOp {
            left: box Expression::Variable(String::from("abc")),
            right: box Expression::ArithmeticOp {
                left: box Expression::IntLiteral(String::from("4")),
                right: box Expression::IntLiteral(String::from("4")),
                op: Op::Mul,
            },
            op: Op::Add,
        };
        let res = crate::optimizer::evalE(&a);

        assert_eq!(res, Some(Expression::ArithmeticOp {
            left: box Expression::Variable(String::from("abc")),
            right: box Expression::IntLiteral(String::from("16")),
            op: Op::Add,
        }));
    }
}

#[test]
pub fn testLexingUnits() {
    let input = "fn test(x: int): int { print( x ) } test ( 25 ) ";

    let tokens = tokenizeSource(input).unwrap();
    println!("tokens {:?}", &tokens);

    let res = parseTokens(tokens).unwrap();
    println!("{:?}", &res);

    let bs = bytecodeGen(res).unwrap();

    evaluateBytecode(bs.0, bs.1);
}

#[test]
pub fn testLibLoading() {
    let mut vm = bootStrapVM();
    vm.stack.push(Value::Num(1_000_000_000));
    unsafe { vm.loadNative("/home/vasabi/CLionProjects/viplNative/cmake-build-release/libviplNative.so", String::from("test123"), None, Box::new([VariableMetadata::i(MyStr::Static(""))])); }

    let a = Instant::now();

    vm.call(MyStr::Static("test123(int)"));

    let elapsed = a.elapsed();
    println!("finished in: {:.2?}", elapsed);

    println!("{}", vm.stack.pop().unwrap().valueStr());
}

#[test]
pub fn testComiple() {
    let sc = "void call(VirtualMachine* vm, StackFrame* frame){
    long max = vm->nativeWrapper.getLocalsInt(frame, 0);
    long x;
    x=0;
    while(x < max){
        x+=1;
    }
    vm->nativeWrapper.pushInt(vm, 6969);
    return;
}";

    let vm = bootStrapVM();

    let res = crate::gccWrapper::compile(sc).unwrap();

    let ops = vec![
        OpCode::StrNew(MyStr::Runtime(res.into_boxed_str())),
        OpCode::StrNew(MyStr::Static("lool(int)")),
        OpCode::PushInt(1),
        OpCode::Call { encoded: MyStr::Static("loadNative(String, String, int)") },
        OpCode::PushInt(69),
        OpCode::Call { encoded: MyStr::Static("lool(int)") },
        OpCode::Call { encoded: MyStr::Static("print(int)") }
    ];
    crate::vm::evaluateBytecode(ops, vec![]);
    println!("{}", vm.stack.len());
}