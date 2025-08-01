use crate::tokenizer::{Token, TokenKind};

type Tokens<'source, 'tokens> = &'tokens [Token<'source>];
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
    While {
        condition: Box<Node>,
        body: Box<Node>,
    },
    For {
        var: Box<Node>,
        range: Box<Node>,
        body: Box<Node>,
    },
    ArrayLiteral(Vec<Node>),
    Import {
        qualified: bool,
        path: Box<Node>,
    },
    Scope(Box<Node>),
    Return(Box<Node>),
    Defer(Box<Node>),
    MarkupBlock {
        tag: String,
        attributes: Vec<(String, Node)>,
        body: Option<Box<Node>>,
        siblings: Vec<Node>,
    },
    ObjectLiteral(Vec<(String, Node)>),
    Break,
    Continue,
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
    And,
    Or,
    Assign,
    AssignAnd(Box<BinaryOperation>),
    Declare,
    Index,
    Concat,
    Range,
    RangeInclusive,
    WriteFile,
    EnvAssign,
    Pipe,
    SemiColon,
    Custom(String),
}

#[derive(Debug, Clone)]
pub struct ParseError {
    pub message: String,
    pub range: (usize, usize),
}

macro_rules! left_associtive_binary_infix_operator {
    ($name:ident, $alt:ident, $next:ident, $({$comp:expr, $oper:expr}),+) => {
        fn $name<'t, 's>(ts: Tokens<'t, 's>, ctx: ParsingContext) -> Result<(Tokens<'t, 's>, Ast), ParseError> {
            let (ts, lhs) = $next(ts, ctx)?;

            $alt(lhs, ts, ctx)
        }

        fn $alt<'t, 's>(lhs: Ast, ts: Tokens<'t, 's>, ctx: ParsingContext) -> Result<(Tokens<'t, 's>, Ast), ParseError> {
            $(
                if peek_and_compare(&ts, $comp) {
                    let (ts, rhs) = $next(&ts[1..], ctx)?;

                    let this_oper_res =
                        Node::BinaryOperation($oper, Box::new(lhs), Box::new(rhs));

                    if let Ok(r) = $alt(this_oper_res.clone(), ts.clone(), ctx) {
                        return Ok(r);
                    }

                    return Ok((ts, this_oper_res));
                }
            )+

            Ok((ts, lhs))
        }
    };
}

macro_rules! right_associtive_binary_infix_operator {
    ($name:ident, $alt:ident, $next:ident, $({$comp:expr, $oper:expr}),+) => {
        fn $name<'t, 's>(ts: Tokens<'t, 's>, ctx: ParsingContext) -> Result<(Tokens<'t, 's>, Ast), ParseError> {
            let (ts, lhs) = $next(ts, ctx)?;

            let mut expr = (ts, lhs);

            while let Ok(new_expr) = $alt(expr.1.clone(), expr.0.clone(), ctx) {
                expr = new_expr;
            }

            Ok(expr)
        }

        fn $alt<'t, 's>(lhs: Ast, ts: Tokens<'t, 's>, ctx: ParsingContext) -> Result<(Tokens<'t, 's>, Ast), ParseError> {
            $(
                if peek_and_compare(&ts, $comp) {
                    let (ts, rhs) = $next(&ts[1..], ctx)?;

                    let this_oper_res =
                        Node::BinaryOperation($oper, Box::new(lhs), Box::new(rhs));

                    return Ok((ts, this_oper_res));
                }
            )+

            Err(ParseError {
                message: format!("Expected one of: {}", [$($comp.to_string()),+].join(", ")),
                range: (0, 0),
            })
        }
    };
}

macro_rules! expect_exact_token {
    ($token:expr, $ts:expr) => {
        if !peek_and_compare(&$ts, $token) {
            return Err(ParseError {
                message: format!("Expected `{}`", $token),
                range: if $ts.is_empty() { (0, 0) } else { $ts[0].range },
            });
        }
        $ts = &$ts[1..];
    };
}

pub fn parse(ts: Tokens) -> Result<Ast, ParseError> {
    let ctx = ParsingContext {
        parsing_args: false,
    };

    let (ts, ast) = definition_list(ts, ctx)?;

    if !ts.is_empty() {
        return Err(ParseError {
            message: format!("Unexpected token: {}", ts[0].token),
            range: ts[0].range,
        });
    }

    // if let Node::String(_) = ast.clone() {
    //     return Ok(Node::Call(ast.clone().into(), vec![]));
    // }

    Ok(ast)
}

#[derive(Debug, Clone, Copy)]
struct ParsingContext {
    /// If `true`, blocks parsing single words as commands without arguments.
    parsing_args: bool,
}

fn definition_list<'t, 's>(
    mut ts: Tokens<'s, 't>,
    ctx: ParsingContext,
) -> Result<(Tokens<'s, 't>, Ast), ParseError> {
    let mut nodes = vec![];

    loop {
        let (new_ts, node) = match import(ts, ctx) {
            Ok((new_ts, node)) => (new_ts, node),
            Err(e) => {
                if e.message == "Unexpected end of input" {
                    break;
                } else {
                    return Err(e);
                }
            }
        };

        ts = new_ts;

        while ts
            .iter()
            .next()
            .map(|t| t.kind == TokenKind::ExpressionTerminator)
            .unwrap_or(false)
        {
            ts = &ts[1..];
        }

        nodes.push(node);
    }

    if nodes.is_empty() {
        return Ok((ts, Node::DefinitionList(vec![])));
    }

    if nodes.len() == 1 {
        return Ok((ts, nodes.pop().unwrap()));
    }

    return Ok((ts, Node::DefinitionList(nodes)));
}

fn import<'s, 't>(
    ts: Tokens<'s, 't>,
    ctx: ParsingContext,
) -> Result<(Tokens<'s, 't>, Ast), ParseError> {
    if peek_and_compare(&ts, "use") {
        let mut ts = &ts[1..];

        let mut qualified = true;

        if peek_and_compare(ts, "unqualified") {
            ts = &ts[1..];
            qualified = false;
        }

        let (ts, path) = literal(
            ts,
            ParsingContext {
                parsing_args: true,
                ..ctx.clone()
            },
        )?;

        return Ok((
            ts,
            Node::Import {
                qualified,
                path: Box::new(path),
            },
        ));
    }

    assignment(ts, ctx)
}

fn function_assignment_name<'t, 's>(
    ts: Tokens<'t, 's>,
    ctx: ParsingContext,
) -> Option<(Tokens<'t, 's>, Ast)> {
    if let Ok((ts, name)) = literal(
        ts,
        ParsingContext {
            parsing_args: true,
            ..ctx.clone()
        },
    ) {
        return Some((ts, name));
    }

    if let Some(next) = peek(ts) {
        if next.kind == TokenKind::Operator {
            return Some((
                &ts[1..],
                Node::Indentifier(next.token.to_string()),
            ));
        }
    }

    None
}

fn assignment<'t, 's>(
    ts: Tokens<'t, 's>,
    ctx: ParsingContext,
) -> Result<(Tokens<'t, 's>, Ast), ParseError> {
    if let Some((mut ts, name)) = function_assignment_name(ts, ctx) {
        let mut args = vec![];

        while let Ok((new_ts, arg)) = literal(
            ts,
            ParsingContext {
                parsing_args: true,
                ..ctx.clone()
            },
        ) {
            ts = new_ts;
            args.push(arg);
        }

        if peek_and_compare(&ts, "=") {
            let (mut ts, rhs) = semicolon(&ts[1..], ctx)?;
            let body = Box::new(rhs);
            let mut where_definitions = None;

            if peek_and_compare(&ts, "where") {
                let (new_ts, rhs) = definition_list(&ts[1..], ctx)?;
                ts = new_ts;
                where_definitions = Some(Box::new(rhs));
            }

            return Ok((
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

    return semicolon(ts, ctx);
}

right_associtive_binary_infix_operator!(semicolon, semicolon_, lambda,
    {";", BinaryOperation::SemiColon}
);

fn lambda<'t, 's>(
    ts: Tokens<'t, 's>,
    ctx: ParsingContext,
) -> Result<(Tokens<'t, 's>, Ast), ParseError> {
    if peek_and_compare(&ts, "λ") {
        let mut ts = &ts[1..];

        let mut args = vec![];

        while let Ok((new_ts, arg)) = literal(
            ts,
            ParsingContext {
                parsing_args: true,
                ..ctx.clone()
            },
        ) {
            ts = new_ts;
            args.push(arg);
        }

        if peek_and_compare(&ts, "->") {
            let (ts, rhs) = lambda(&ts[1..], ctx)?;
            return Ok((
                ts,
                Node::Lambda {
                    args,
                    body: Box::new(rhs),
                },
            ));
        }
    }

    control_flow_statements(ts, ctx)
}

fn control_flow_statements<'t, 's>(
    ts: Tokens<'t, 's>,
    ctx: ParsingContext,
) -> Result<(Tokens<'t, 's>, Ast), ParseError> {
    if peek_and_compare(&ts, "return") {
        let (ts, value) = lambda(&ts[1..], ctx)?;
        return Ok((ts, Node::Return(Box::new(value))));
    }

    if peek_and_compare(&ts, "defer") {
        let (ts, value) = lambda(&ts[1..], ctx)?;
        return Ok((ts, Node::Defer(Box::new(value))));
    }

    if peek_and_compare(&ts, "continue") {
        return Ok((&ts[1..], Node::Continue));
    }

    if peek_and_compare(&ts, "break") {
        return Ok((&ts[1..], Node::Break));
    }

    if_then_else(ts, ctx)
}

fn if_then_else<'t, 's>(
    mut ts: Tokens<'t, 's>,
    ctx: ParsingContext,
) -> Result<(Tokens<'t, 's>, Ast), ParseError> {
    if peek_and_compare(&ts, "if") {
        ts = &ts[1..];

        let (mut ts, condition) = lambda(ts, ctx)?;

        expect_exact_token!("then", ts);

        let (mut ts, then_branch) = lambda(ts, ctx)?;

        expect_exact_token!("else", ts);

        let (mut ts, else_branch) = lambda(ts, ctx)?;

        while peek_and_compare_kind(&ts, TokenKind::ExpressionTerminator) {
            ts = &ts[1..];
        }

        return Ok((
            ts,
            Node::IfThenElse {
                condition: Box::new(condition),
                then_branch: Box::new(then_branch),
                else_branch: Box::new(else_branch),
            },
        ));
    } else {
        while_loop(ts, ctx)
    }
}

fn while_loop<'t, 's>(
    mut ts: Tokens<'t, 's>,
    ctx: ParsingContext,
) -> Result<(Tokens<'t, 's>, Ast), ParseError> {
    if peek_and_compare(&ts, "while") {
        ts = &ts[1..];

        let (mut ts, condition) = lambda(ts, ctx)?;

        expect_exact_token!("do", ts);

        let (mut ts, body) = lambda(ts, ctx)?;

        while peek_and_compare_kind(&ts, TokenKind::ExpressionTerminator) {
            ts = &ts[1..];
        }

        return Ok((
            ts,
            Node::While {
                condition: Box::new(condition),
                body: Box::new(body),
            },
        ));
    } else {
        for_loop(ts, ctx)
    }
}

fn for_loop<'t, 's>(
    mut ts: Tokens<'t, 's>,
    ctx: ParsingContext,
) -> Result<(Tokens<'t, 's>, Ast), ParseError> {
    if peek_and_compare(&ts, "for") {
        ts = &ts[1..];

        let (mut ts, var) = brackets(ts, ctx)?;

        expect_exact_token!("in", ts);

        let (mut ts, range) = lambda(ts, ctx)?;

        expect_exact_token!("do", ts);

        let (mut ts, body) = lambda(ts, ctx)?;

        while peek_and_compare_kind(&ts, TokenKind::ExpressionTerminator) {
            ts = &ts[1..];
        }

        return Ok((
            ts,
            Node::For {
                var: Box::new(var),
                range: Box::new(range),
                body: Box::new(body),
            },
        ));
    } else {
        file(ts, ctx)
    }
}

left_associtive_binary_infix_operator!(file, file_, pipe,
    {">>", BinaryOperation::WriteFile},
    {"$=", BinaryOperation::EnvAssign},
    {"=", BinaryOperation::Assign},
    {":=", BinaryOperation::Declare},
    {"+=", BinaryOperation::AssignAnd(Box::new(BinaryOperation::Add))},
    {"-=", BinaryOperation::AssignAnd(Box::new(BinaryOperation::Sub))},
    {"*=", BinaryOperation::AssignAnd(Box::new(BinaryOperation::Mul))},
    {"//=", BinaryOperation::AssignAnd(Box::new(BinaryOperation::Div))},
    {"&&=", BinaryOperation::AssignAnd(Box::new(BinaryOperation::And))},
    {"||=", BinaryOperation::AssignAnd(Box::new(BinaryOperation::Or))},
    {"++=", BinaryOperation::AssignAnd(Box::new(BinaryOperation::Concat))},
    {"**=", BinaryOperation::AssignAnd(Box::new(BinaryOperation::Pow))}
);

left_associtive_binary_infix_operator!(pipe, pipe_, bool_ops,
    {"|", BinaryOperation::Pipe}
);

right_associtive_binary_infix_operator!(bool_ops, _bool_ops, eq,
    {"&&", BinaryOperation::And},
    {"||", BinaryOperation::Or}
);

left_associtive_binary_infix_operator!(eq, eq_, custom,
    {"==", BinaryOperation::Eq},
    {"<", BinaryOperation::Lt},
    {">", BinaryOperation::Gt},
    {">=", BinaryOperation::Gte},
    {"<=", BinaryOperation::Lte}
);

fn custom<'t, 's>(
    ts: Tokens<'t, 's>,
    ctx: ParsingContext,
) -> Result<(Tokens<'t, 's>, Ast), ParseError> {
    let (ts, lhs) = concat(ts, ctx)?;
    custom_(lhs, ts, ctx)
}
fn custom_<'t, 's>(
    lhs: Ast,
    ts: Tokens<'t, 's>,
    ctx: ParsingContext,
) -> Result<(Tokens<'t, 's>, Ast), ParseError> {
    let known_ops = vec![
        "==", "<", ">", "<=", ">=", "&&", "||", "++", "//", "!!", "=", ":=", "+=", "-=", "*=",
        "//=", "&&=", "||=", "++=", "**=", "+", "-", "*", "$=", "**", "|", "..", "..=", "->", "λ",
        ",", ";", "</", ">>",
    ];

    let Some(next) = peek(ts) else {
        return Ok((ts, lhs));
    };

    if next.kind == TokenKind::Operator && !known_ops.contains(&next.token) {
        let (ts, rhs) = concat(&ts[1..], ctx)?;
        let this_oper_res = Node::BinaryOperation(
            BinaryOperation::Custom(next.token.to_string()),
            Box::new(lhs),
            Box::new(rhs),
        );
        if let Ok(r) = custom_(this_oper_res.clone(), ts, ctx) {
            return Ok(r);
        }
        return Ok((ts, this_oper_res));
    }
    Ok((ts, lhs))
}

left_associtive_binary_infix_operator!(concat, concat_, add,
    {"++", BinaryOperation::Concat}
);

left_associtive_binary_infix_operator!(add, add_, mul,
    {"+", BinaryOperation::Add},
    {"-", BinaryOperation::Sub}
);

left_associtive_binary_infix_operator!(mul, mul_, unary_minus,
    {"*", BinaryOperation::Mul},
    {"//", BinaryOperation::Div}
);

fn unary_minus<'t, 's>(
    ts: Tokens<'t, 's>,
    ctx: ParsingContext,
) -> Result<(Tokens<'t, 's>, Ast), ParseError> {
    if peek_and_compare(&ts, "-") {
        let (ts, n) = pow(&ts[1..], ctx)?;
        Ok((
            ts,
            Node::UnaryOperation(UnaryOperation::Negate, Box::new(n)),
        ))
    } else {
        pow(ts, ctx)
    }
}

left_associtive_binary_infix_operator!(pow, pow_, index,
    {"**", BinaryOperation::Pow}
);

left_associtive_binary_infix_operator!(index, index_, range,
    {"!!", BinaryOperation::Index}
);

left_associtive_binary_infix_operator!(range, _range, call,
    {"..", BinaryOperation::Range},
    {"..=", BinaryOperation::RangeInclusive}
);

fn call<'t, 's>(
    ts: Tokens<'t, 's>,
    ctx: ParsingContext,
) -> Result<(Tokens<'t, 's>, Ast), ParseError> {
    let Ok((ts, first)) = brackets(ts, ctx) else {
        return brackets(ts, ctx);
    };

    let mut args = vec![];

    let mut fn_ts = ts;
    while let Ok((new_ts, arg)) = brackets(
        fn_ts,
        ParsingContext {
            parsing_args: true,
            ..ctx.clone()
        },
    ) {
        fn_ts = new_ts;
        args.push(arg);
    }

    if !args.is_empty() {
        return Ok((fn_ts, Node::Call(Box::new(first), args)));
    }

    Ok((ts, first))
}

fn brackets<'t, 's>(
    ts: Tokens<'t, 's>,
    ctx: ParsingContext,
) -> Result<(Tokens<'t, 's>, Ast), ParseError> {
    if peek_and_compare(&ts, "(") {
        let uncolosed_error = ParseError {
            message: "Expected `)`".to_string(),
            range: ts[0].range,
        };

        let (ts, n) = lambda(
            &ts[1..],
            ParsingContext {
                parsing_args: false,
                ..ctx.clone()
            },
        )?;

        let Some(next) = ts.iter().next() else {
            return Err(uncolosed_error);
        };

        if next.token == ")" {
            Ok((&ts[1..], n))
        } else {
            Err(uncolosed_error)
        }
    } else {
        array(ts, ctx)
    }
}

fn array<'t, 's>(
    mut ts: Tokens<'t, 's>,
    ctx: ParsingContext,
) -> Result<(Tokens<'t, 's>, Ast), ParseError> {
    if peek_and_compare(&ts, "[") {
        let uncolosed_error = ParseError {
            message: "Expected `]`".to_string(),
            range: ts[0].range,
        };

        ts = &ts[1..];

        let mut items = vec![];

        while let Ok((new_ts, item)) = lambda(
            ts,
            ParsingContext {
                parsing_args: false,
                ..ctx.clone()
            },
        ) {
            ts = new_ts;

            items.push(item);

            if peek_and_compare(&ts, ",") {
                ts = &ts[1..];
            } else {
                break;
            }
        }

        let Some(next) = ts.iter().next() else {
            return Err(uncolosed_error);
        };

        if next.token == "]" {
            Ok((&ts[1..], Node::ArrayLiteral(items)))
        } else {
            Err(uncolosed_error)
        }
    } else {
        scope(ts, ctx)
    }
}

fn scope<'t, 's>(
    mut ts: Tokens<'t, 's>,
    ctx: ParsingContext,
) -> Result<(Tokens<'t, 's>, Ast), ParseError> {
    if peek_and_compare(&ts, "{") {
        if ts.len() >= 3 && ts[2].token == ":" {
            return object_literal(ts, ctx);
        }

        let uncolosed_error = ParseError {
            message: "Expected `}`".to_string(),
            range: ts[0].range,
        };

        ts = &ts[1..];

        let (ts, body) = semicolon(
            ts,
            ParsingContext {
                parsing_args: false,
                ..ctx.clone()
            },
        )?;

        let Some(next) = ts.iter().next() else {
            return Err(uncolosed_error);
        };

        if next.token == "}" {
            Ok((&ts[1..], body))
        } else {
            Err(uncolosed_error)
        }
    } else {
        markup(ts, ctx)
    }
}

fn object_literal<'t, 's>(
    mut ts: Tokens<'t, 's>,
    ctx: ParsingContext,
) -> Result<(Tokens<'t, 's>, Ast), ParseError> {
    expect_exact_token!("{", ts);

    let mut items = vec![];

    while !peek_and_compare(ts, "}") {
        if !peek_and_compare_kind(ts, TokenKind::Word) {
            return Err(ParseError {
                message: "Expected object key".to_string(),
                range: ts[0].range,
            });
        }

        let name = ts[0].token.to_string();
        ts = &ts[1..];

        expect_exact_token!(":", ts);

        let (new_ts, value) = lambda(
            ts,
            ParsingContext {
                parsing_args: true,
                ..ctx.clone()
            },
        )?;
        ts = new_ts;

        if peek_and_compare(ts, ",") {
            ts = &ts[1..];
        } else if !peek_and_compare(ts, "}") {
            return Err(ParseError {
                message: "Expected `}`".to_string(),
                range: ts[0].range,
            });
        }

        items.push((name, value));
    }

    Ok((&ts[1..], Node::ObjectLiteral(items)))
}

fn markup<'t, 's>(
    ts: Tokens<'t, 's>,
    ctx: ParsingContext,
) -> Result<(Tokens<'t, 's>, Ast), ParseError> {
    if peek_and_compare(&ts, "<") {
        let ts = &ts[1..];

        if peek_and_compare_kind(ts, TokenKind::Word) {
            let tag = ts[0].token.to_string();

            let mut ts = &ts[1..];

            let mut attributes = vec![];

            let mut self_closing = false;

            while !peek_and_compare(ts, ">") {
                if peek_and_compare(ts, "/>") {
                    self_closing = true;
                    break;
                }

                if ts.len() < 3 {
                    return Err(ParseError {
                        message: "Expected `>`".to_string(),
                        range: (0, 0),
                    });
                }

                let name = ts[0].token.to_string();
                ts = &ts[1..];

                expect_exact_token!("=", ts);

                let (new_ts, value) = brackets(
                    ts,
                    ParsingContext {
                        parsing_args: true,
                        ..ctx.clone()
                    },
                )?;

                ts = new_ts;

                attributes.push((name, value));
            }

            ts = &ts[1..];

            let mut body = None;

            if !self_closing {
                let (new_ts, body_) = lambda(
                    &ts,
                    ParsingContext {
                        parsing_args: true,
                        ..ctx.clone()
                    },
                )?;

                ts = new_ts;
                body = Some(Box::new(body_));

                while peek_and_compare_kind(ts, TokenKind::ExpressionTerminator) {
                    ts = &ts[1..];
                }

                expect_exact_token!("</", ts);
                expect_exact_token!(&tag, ts);
                expect_exact_token!(">", ts);
            }

            let mut siblings = vec![];

            while let Ok((new_ts, sibling)) = markup(ts, ctx) {
                ts = new_ts;
                siblings.push(sibling);
            }

            return Ok((
                ts,
                Node::MarkupBlock {
                    tag,
                    attributes,
                    body,
                    siblings,
                },
            ));
        }
    }

    literal(ts, ctx)
}

fn literal<'t, 's>(
    ts: Tokens<'t, 's>,
    ctx: ParsingContext,
) -> Result<(Tokens<'t, 's>, Ast), ParseError> {
    let eof_err = Err(ParseError {
        message: "Unexpected end of input".to_string(),
        range: (0, 0),
    });

    if ts.is_empty() {
        return eof_err;
    }

    if let Some(token) = peek(&ts) {
        if token.token == "true" || token.token == "false" {
            let node = Node::String(token.token.to_string());
            return Ok((&ts[1..], node));
        }

        if token.kind == TokenKind::Word || token.kind == TokenKind::QuotedString {
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
            } else {
                if !ctx.parsing_args
                    && token.token.chars().next().unwrap().is_alphabetic()
                    && token.kind != TokenKind::QuotedString
                {
                    node = Node::Call(Box::new(node), vec![]);
                }
            }

            return Ok((&ts[1..], node));
        }
    }

    return eof_err;
}

fn peek<'s, 't>(ts: Tokens<'s, 't>) -> Option<&'t Token<'s>> {
    ts.iter().next()
}

fn peek_and_compare(ts: Tokens, token: &str) -> bool {
    peek(ts)
        .map(|t| {
            // println!("{} == {}", t.token, token);
            t.token == token
        })
        .unwrap_or(false)
}

fn peek_and_compare_kind(ts: Tokens, kind: TokenKind) -> bool {
    peek(ts)
        .map(|t| {
            // println!("{} == {}", t.token, token);
            t.kind == kind
        })
        .unwrap_or(false)
}
