use crate::ast::{FunctionDef, Node, Op, Statement};
use crate::OpCode;
use crate::OpCode::{FunName, LocalVarTable};
use crate::parser::Operation;

fn genFunctionDef(fun: FunctionDef, ops: &mut Vec<OpCode>, funs: &mut Vec<FunctionDef>) {
    ops.push(OpCode::FunBegin);
    ops.push(FunName { name: fun.name });

    for s in fun.body {
        match s {
            Statement::VariableCreate(_) => {}
        }
    }

    ops.push(LocalVarTable { typ: Box::new([]), argsCount: 0 });

    ops.push(OpCode::FunEnd);
}

fn bytecodeGen(mut operations: Vec<Operation>) -> Vec<OpCode> {
    let mut inlineMain = vec![];
    let mut ops = vec![];


    for op in operations {
        match op {
            Operation::FunctionDef(f) => {
                match f {
                    Node::FunctionDef(v) =>  genFunctionDef(v, &mut ops)
                }
            }
            _ => inlineMain.push(op)
        }
    }

    return ops
}