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
    Call(Box<Node>, Vec<Node>),
    Negate(Box<Node>),
    Add(Box<Node>, Box<Node>),
    Sub(Box<Node>, Box<Node>),
    Mul(Box<Node>, Box<Node>),
    Div(Box<Node>, Box<Node>),
    Pow(Box<Node>, Box<Node>),
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
            let (mut ts, rhs) = add(ts[1..].to_vec())?;
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

    return add(ts);
}

fn add(ts: Tokens) -> Option<(Tokens, Ast)> {
    let (ts, lhs) = mul(ts)?;

    add_(lhs, ts)
}

fn add_(lhs: Ast, ts: Tokens) -> Option<(Tokens, Ast)> {
    if peek_and_compare(&ts, "+") {
        let (ts, rhs) = mul(ts[1..].to_vec())?;

        let this_oper_res = Node::Add(Box::new(lhs), Box::new(rhs));

        if let Some(r) = add_(this_oper_res.clone(), ts.clone()) {
            return Some(r);
        }

        Some((ts, this_oper_res))
    } else if peek_and_compare(&ts, "-") {
        let (ts, rhs) = mul(ts[1..].to_vec())?;

        let this_oper_res = Node::Sub(Box::new(lhs), Box::new(rhs));

        if let Some(r) = add_(this_oper_res.clone(), ts.clone()) {
            return Some(r);
        }

        Some((ts, this_oper_res))
    } else {
        Some((ts, lhs))
    }
}

fn mul(ts: Tokens) -> Option<(Tokens, Ast)> {
    let (ts, lhs) = unary_minus(ts)?;

    mul_(lhs, ts)
}

fn mul_(lhs: Ast, ts: Tokens) -> Option<(Tokens, Ast)> {
    if peek_and_compare(&ts, "*") {
        let (ts, rhs) = unary_minus(ts[1..].to_vec())?;

        let this_oper_res = Node::Mul(Box::new(lhs), Box::new(rhs));

        if let Some(r) = mul_(this_oper_res.clone(), ts.clone()) {
            return Some(r);
        }

        Some((ts, this_oper_res))
    } else if peek_and_compare(&ts, "/") {
        let (ts, rhs) = unary_minus(ts[1..].to_vec())?;

        let this_oper_res = Node::Div(Box::new(lhs), Box::new(rhs));

        if let Some(r) = mul_(this_oper_res.clone(), ts.clone()) {
            return Some(r);
        }

        Some((ts, this_oper_res))
    } else {
        Some((ts, lhs))
    }
}

fn unary_minus(ts: Tokens) -> Option<(Tokens, Ast)> {
    if peek_and_compare(&ts, "-") {
        let (ts, n) = pow(ts[1..].to_vec())?;
        Some((ts, Node::Negate(Box::new(n))))
    } else {
        pow(ts)
    }
}

fn pow(ts: Tokens) -> Option<(Tokens, Ast)> {
    let (ts, lhs) = call(ts)?;

    pow_(lhs, ts)
}

fn pow_(lhs: Ast, ts: Tokens) -> Option<(Tokens, Ast)> {
    if peek_and_compare(&ts, "**") {
        let (ts, rhs) = call(ts[1..].to_vec())?;

        let this_oper_res = Node::Pow(Box::new(lhs), Box::new(rhs));

        if let Some(r) = pow_(this_oper_res.clone(), ts.clone()) {
            return Some(r);
        }

        Some((ts, this_oper_res))
    } else {
        Some((ts, lhs))
    }
}

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
        let (ts, n) = add(ts[1..].to_vec())?;

        if ts.iter().next()?.token == ")" {
            Some((ts[1..].to_vec(), n))
        } else {
            None
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
                node = Node::Indentifier(token.token.to_string());
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
