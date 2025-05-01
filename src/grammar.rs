use crate::tokenizer::{Token, TokenKind};

type Tokens<'source> = Vec<Token<'source>>;
type Ast = Node;

#[derive(Debug, Clone)]
pub enum Node {
    DefinitionList(Vec<Node>),
    String(String),
    Indentifier(String),
    DoubleLiteral(f64),
    Assignment {
        name: Box<Node>,
        args: Vec<Node>,
        body: Box<Node>,
        where_definitions: Option<Box<Node>>,
    },
    Lambda {
        args: Vec<Node>,
        body: Box<Node>,
    },
    Call(Box<Node>, Vec<Node>),
    UnaryOperation(UnaryOperation, Box<Node>),
    BinaryOperation(BinaryOperation, Box<Node>, Box<Node>),
    IfThenElse {
        condition: Box<Node>,
        then_branch: Box<Node>,
        else_branch: Box<Node>,
    },
    ArrayLiteral(Vec<Node>),
}

#[derive(Debug, Clone)]
pub enum UnaryOperation {
    Negate,
}

impl UnaryOperation {
    pub fn stringify(&self) -> String {
        match self {
            UnaryOperation::Negate => "-".to_string(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum BinaryOperation {
    Add,
    Sub,
    Mul,
    Div,
    Pow,
    Eq,
    Lt,
    Gt,
    Lte,
    Gte,
    Custon(String),
}

struct ParseError {
    message: String,
    range: (usize, usize),
}

pub fn parse(ts: Tokens) -> Option<(Tokens, Ast)> {
    definition_list(ts)
}

fn definition_list(mut ts: Tokens) -> Option<(Tokens, Ast)> {
    let mut nodes = vec![];

    while let Some((new_ts, node)) = assignment(ts.clone()) {
        ts = new_ts;

        while ts
            .iter()
            .next()
            .map(|t| t.kind == TokenKind::ExpressionTerminator)
            .unwrap_or(false)
        {
            ts = ts[1..].to_vec();
        }

        nodes.push(node);
    }

    if nodes.is_empty() {
        return None;
    }

    if nodes.len() == 1 {
        return Some((ts, nodes.pop().unwrap()));
    }

    return Some((ts, Node::DefinitionList(nodes)));
}

fn assignment(ts: Tokens) -> Option<(Tokens, Ast)> {
    if let Some((mut ts, name)) = literal(ts.clone()) {
        let mut args = vec![];

        while let Some((new_ts, arg)) = literal(ts.clone()) {
            ts = new_ts;
            args.push(arg);
        }

        if peek_and_compare(&ts, "=") {
            let (mut ts, rhs) = lambda(ts[1..].to_vec())?;
            let body = Box::new(rhs);
            let mut where_definitions = None;

            if peek_and_compare(&ts, "where") {
                let (new_ts, rhs) = definition_list(ts[1..].to_vec())?;
                ts = new_ts;
                where_definitions = Some(Box::new(rhs));
            }

            return Some((
                ts,
                Node::Assignment {
                    name: Box::new(name),
                    args,
                    body,
                    where_definitions,
                },
            ));
        }
    }

    return lambda(ts);
}

fn lambda(ts: Tokens) -> Option<(Tokens, Ast)> {
    if peek_and_compare(&ts, "λ") {
        let mut ts = ts[1..].to_vec();

        let mut args = vec![];

        while let Some((new_ts, arg)) = literal(ts.clone()) {
            ts = new_ts;
            args.push(arg);
        }

        if peek_and_compare(&ts, "->") {
            let (ts, rhs) = lambda(ts[1..].to_vec())?;
            return Some((
                ts,
                Node::Lambda { args, body: Box::new(rhs) },
            ));
        }
    }

    if_then_else(ts)
}

fn if_then_else(mut ts: Tokens) -> Option<(Tokens, Ast)> {
    if peek_and_compare(&ts, "if") {
        ts = ts[1..].to_vec();

        let (mut ts, condition) = lambda(ts.clone())?;

        if !peek_and_compare(&ts, "then") {
            // TODO error
            panic!();
        }
        ts = ts[1..].to_vec();

        let (mut ts, then_branch) = lambda(ts.clone())?;

        if !peek_and_compare(&ts, "else") {
            // TODO error
            panic!();
        }
        ts = ts[1..].to_vec();

        let (ts, else_branch) = lambda(ts.clone())?;

        return Some((
            ts,
            Node::IfThenElse {
                condition: Box::new(condition),
                then_branch: Box::new(then_branch),
                else_branch: Box::new(else_branch),
            },
        ));
    } else {
        eq(ts)
    }
}


macro_rules! left_accocitive_binary_infix_operator {
    ($name:ident, $alt:ident, $next:ident, $({$comp:expr, $oper:expr}),+) => {
        fn $name(ts: Tokens) -> Option<(Tokens, Ast)> {
            let (ts, lhs) = $next(ts)?;

            $alt(lhs, ts)
        }

        fn $alt(lhs: Ast, ts: Tokens) -> Option<(Tokens, Ast)> {
            $(
                if peek_and_compare(&ts, $comp) {
                    let (ts, rhs) = $next(ts[1..].to_vec())?;

                    let this_oper_res =
                        Node::BinaryOperation($oper, Box::new(lhs), Box::new(rhs));

                    if let Some(r) = $alt(this_oper_res.clone(), ts.clone()) {
                        return Some(r);
                    }

                    return Some((ts, this_oper_res));
                }
            )+

            Some((ts, lhs))
        }
    };
}

left_accocitive_binary_infix_operator!(eq, eq_, add, 
    {"==", BinaryOperation::Eq},
    {"<", BinaryOperation::Lt},
    {">", BinaryOperation::Gt},
    {"<=", BinaryOperation::Gte},
    {"<=", BinaryOperation::Lte}
);

left_accocitive_binary_infix_operator!(add, add_, mul,
    {"+", BinaryOperation::Add},
    {"-", BinaryOperation::Sub}
);

left_accocitive_binary_infix_operator!(mul, mul_, unary_minus,
    {"*", BinaryOperation::Mul},
    {"//", BinaryOperation::Div}
);

fn unary_minus(ts: Tokens) -> Option<(Tokens, Ast)> {
    if peek_and_compare(&ts, "-") {
        let (ts, n) = pow(ts[1..].to_vec())?;
        Some((
            ts,
            Node::UnaryOperation(UnaryOperation::Negate, Box::new(n)),
        ))
    } else {
        pow(ts)
    }
}

left_accocitive_binary_infix_operator!(pow, pow_, call,
    {"**", BinaryOperation::Pow}
);

fn call(ts: Tokens) -> Option<(Tokens, Ast)> {
    let Some((ts, first)) = literal(ts.clone()) else {
        return brackets(ts);
    };

    let mut args = vec![];

    let mut fn_ts = ts.clone();
    while let Some((new_ts, arg)) = brackets(fn_ts.clone()) {
        fn_ts = new_ts;
        args.push(arg);
    }

    if !args.is_empty() {
        return Some((fn_ts, Node::Call(Box::new(first), args)));
    }

    Some((ts, first))
}

fn brackets(ts: Tokens) -> Option<(Tokens, Ast)> {
    if peek_and_compare(&ts, "(") {
        let (ts, n) = lambda(ts[1..].to_vec())?;

        if ts.iter().next()?.token == ")" {
            Some((ts[1..].to_vec(), n))
        } else {
            None
        }
    } else {
        array(ts)
    }
}

fn array(mut ts: Tokens) -> Option<(Tokens, Ast)> {
    if peek_and_compare(&ts, "[") {
        ts = ts[1..].to_vec();

        let mut items = vec![];

        while let Some((new_ts, item)) = lambda(ts.clone()) {
            ts = new_ts;

            items.push(item);

            if peek_and_compare(&ts, ",") {
                ts = ts[1..].to_vec();
            } else {
                break;
            }
        }

        if ts.iter().next()?.token == "]" {
            Some((ts[1..].to_vec(), Node::ArrayLiteral(items)))
        } else {
            // TODO error
            panic!();
        }
    } else {
        literal(ts)
    }
}

fn literal(ts: Tokens) -> Option<(Tokens, Ast)> {
    if ts.is_empty() {
        return None;
    }

    if let Some(token) = peek(&ts) {
        if token.kind == TokenKind::Indentifier {
            let mut node = Node::String(token.token.to_string());

            if token
                .token
                .chars()
                .next()
                .map(|c| c.is_numeric())
                .unwrap_or(false)
            {
                // TODO: better parsing
                if let Ok(value) = token.token.parse() {
                    node = Node::DoubleLiteral(value);
                }
            }

            if token
                .token
                .chars()
                .next()
                .map(|c| c == '$')
                .unwrap_or(false)
            {
                if token.token.len() > 1 {
                    node = Node::Indentifier(token.token[1..].to_string());
                }
            }

            return Some((ts[1..].to_vec(), node));
        }
    }

    None
}

fn peek<'source>(ts: &'source Tokens) -> Option<&'source Token<'source>> {
    ts.iter().next()
}

fn peek_and_compare<'source>(ts: &'source Tokens, token: &str) -> bool {
    peek(ts)
        .map(|t| {
            // println!("{} == {}", t.token, token);
            t.token == token
        })
        .unwrap_or(false)
}
