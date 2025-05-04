use crate::grammar::{BinaryOperation, Node};

impl Node {
    pub fn stringify(&self) -> String {
        match self {
            Node::String(s) => s.clone(),
            Node::Indentifier(s) => format!("${}", s),
            Node::DoubleLiteral(d) => d.to_string(),
            Node::BinaryOperation(oper, a, b) => {
                format!("({} {} {})", a.stringify(), oper.stringify(), b.stringify())
            }
            Node::UnaryOperation(oper, a) => format!("({} {})", oper.stringify(), a.stringify()),
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
                format!("({} {})", node.stringify(), args)
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
                "(if {} then {} else {})",
                condition.stringify(),
                then_branch.stringify(),
                else_branch.stringify()
            ),
            Node::Import { qualified, path } => format!(
                "use{} {}",
                if *qualified { " unqualified" } else { "" },
                path.stringify()
            ),
            Node::While { condition, body } => {
                format!("(while {} do {})", condition.stringify(), body.stringify())
            }
            Node::For { var, range, body } => {
                format!(
                    "(for {} in {} do {})",
                    var.stringify(),
                    range.stringify(),
                    body.stringify()
                )
            }
            Node::Return(node) => "return ".to_string() + &node.stringify(),
            Node::Break => "break".to_string(),
            Node::Continue => "continue".to_string(),
            Node::Defer(node) => "defer ".to_string() + &node.stringify(),
            Node::Scope(node) => format!("{{ {} }}", node.stringify()),
            Node::MarkupBlock {
                tag,
                attributes,
                body,
                siblings, 
            } => {
                let attributes = attributes
                    .iter()
                    .map(|(k, v)| format!("{}={}", k, v.stringify()))
                    .collect::<Vec<_>>()
                    .join(" ");

                let siblings = siblings
                    .iter()
                    .map(|s| s.stringify())
                    .collect::<Vec<_>>()
                    .join(" ");

                if let Some(body) = body {
                    format!("<{} {}>{}</{}>{}", tag, attributes, body.stringify(), tag, siblings)
                } else {
                    format!("<{} {}/>{}", tag, attributes, siblings)
                }
            }
            Node::ObjectLiteral(vec) => {
                let items = vec
                    .iter()
                    .map(|(k, v)| format!("{}: {}", k, v.stringify()))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{{ {} }}", items)
            }
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
            BinaryOperation::And => "&&".to_string(),
            BinaryOperation::Or => "||".to_string(),
            BinaryOperation::Index => "!!".to_string(),
            BinaryOperation::Concat => "++".to_string(),
            BinaryOperation::Range => "..".to_string(),
            BinaryOperation::RangeInclusive => "..=".to_string(),
            BinaryOperation::Pipe => "|".to_string(),
            BinaryOperation::WriteFile => ">>".to_string(),
            BinaryOperation::EnvAssign => "$=".to_string(),
            BinaryOperation::SemiColon => ";".to_string(),
            BinaryOperation::Custon(s) => s.clone(),
            BinaryOperation::Assign => "=".to_string(),
            BinaryOperation::Declare => ":=".to_string(),
            BinaryOperation::AssignAnd(binary_operation) => {
                format!("{}{}", binary_operation.stringify(), "=")
            }
        }
    }
}
