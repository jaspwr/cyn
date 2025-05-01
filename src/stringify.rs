use crate::grammar::{BinaryOperation, Node};

impl Node {
    pub fn stringify(&self) -> String {
        match self {
            Node::String(s) => s.clone(),
            Node::Indentifier(s) => format!("${}", s),
            Node::DoubleLiteral(d) => d.to_string(),
            Node::BinaryOperation(oper, a, b) => {
                format!("{} {} {}", a.stringify(), oper.stringify(), b.stringify())
            }
            Node::UnaryOperation(oper, a) => format!("{} {}", oper.stringify(), a.stringify()),
            Node::DefinitionList(defs) => defs
                .iter()
                .map(|d| d.stringify())
                .collect::<Vec<_>>()
                .join("\n"),
            Node::Assignment {
                name,
                args,
                body,
                where_definitions,
            } => {
                format!(
                    "{} {} = {}",
                    name.stringify(),
                    args.iter()
                        .map(|a| a.stringify())
                        .collect::<Vec<_>>()
                        .join(" "),
                    body.stringify()
                )
            }
            Node::Lambda { args, body } => {
                format!(
                    "λ {} -> {}",
                    args.iter()
                        .map(|a| a.stringify())
                        .collect::<Vec<_>>()
                        .join(" "),
                    body.stringify()
                )
            }
            Node::Call(node, args) => {
                let args = args
                    .iter()
                    .map(|a| a.stringify())
                    .collect::<Vec<_>>()
                    .join(" ");
                format!("{} {}", node.stringify(), args)
            }
            Node::ArrayLiteral(items) => {
                let items = items
                    .iter()
                    .map(|a| a.stringify())
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("[{}]", items)
            }
            Node::IfThenElse {
                condition,
                then_branch,
                else_branch,
            } => format!(
                "if {} then {} else {}",
                condition.stringify(),
                then_branch.stringify(),
                else_branch.stringify()
            ),
        }
    }
}

impl BinaryOperation {
    pub fn stringify(&self) -> String {
        match self {
            BinaryOperation::Add => "+".to_string(),
            BinaryOperation::Sub => "-".to_string(),
            BinaryOperation::Mul => "*".to_string(),
            BinaryOperation::Div => "//".to_string(),
            BinaryOperation::Pow => "**".to_string(),
            BinaryOperation::Eq => "==".to_string(),
            BinaryOperation::Lt => "<".to_string(),
            BinaryOperation::Gt => ">".to_string(),
            BinaryOperation::Lte => "<=".to_string(),
            BinaryOperation::Gte => ">=".to_string(),
            BinaryOperation::Custon(s) => s.clone(),
        }
    }
}
