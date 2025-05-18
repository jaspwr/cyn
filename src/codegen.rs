use std::collections::{HashMap, LinkedList};

use crate::grammar::Node;
use crate::interpreter::Value;
use crate::types::Type;
use crate::bytecode::{Instruction, InstructionList, MemoryLocation, Opcode, ValueOperand};

pub struct CodegenReturn {
    type_: Type,
    code: InstructionList,
}

pub struct State {
    namespaces: HashMap<Vec<String>, NameSpace>,
    scopes: Vec<Scope>,
    stack_size: usize,
}

struct Scope {
    variables: HashMap<String, (Type, MemoryLocation)>,
}

struct NameSpace {
    functions: HashMap<String, Function>,
}

#[derive(Debug, Clone)]
struct Function {
    name: String,
    args: Vec<String>,
    body: Box<Node>,
}

#[derive(Debug, Clone)]
pub struct CodegenContext {
    pub piped: bool,
    pub piped_input: Option<MemoryLocation>,
}

pub struct CodegenError {
    message: String,
}

fn cge(err: impl ToString) -> CodegenError {
    CodegenError {
        message: err.to_string(),
    }
}

pub fn codegen(node: Node, state: &mut State, ctx: CodegenContext) -> Result<CodegenReturn, CodegenError> {
    Ok(match node {
        Node::DefinitionList(vec) => todo!(),
        Node::String(_) => todo!(),
        Node::Indentifier(_) => todo!(),
        Node::DoubleLiteral(v) => CodegenReturn {
            type_: Type::Double,
            code: Opcode::Copy(ValueOperand::Immediate(Value::Double(v))).as_inst_list(),
        },
        Node::Assignment { name, args, body, where_definitions } => todo!(),
        Node::Lambda { args, body } => todo!(),
        Node::Call(node, vec) => todo!(),
        Node::UnaryOperation(unary_operation, node) => todo!(),
        Node::BinaryOperation(binary_operation, node, node1) => todo!(),
        Node::IfThenElse { condition, then_branch, else_branch } => todo!(),
        Node::While { condition, body } => todo!(),
        Node::For { var, range, body } => todo!(),
        Node::ArrayLiteral(vec) => todo!(),
        Node::Import { qualified, path } => todo!(),
        Node::Scope(node) => todo!(),
        Node::Return(node) => todo!(),
        Node::Defer(node) => todo!(),
        Node::MarkupBlock { tag, attributes, body, siblings } => todo!(),
        Node::ObjectLiteral(vec) => todo!(),
        Node::Break => todo!(),
        Node::Continue => todo!(),
    })
}
