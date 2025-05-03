#[derive(Debug, Clone, PartialEq)]
pub struct Token<'src> {
    pub token: &'src str,
    pub range: (usize, usize),
    pub kind: TokenKind,
    pub indentation: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenKind {
    ExpressionTerminator,
    Word,
    Keyword,
    Operator,
    Literal,
    Bracket,
    QuotedString,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum CharCategory {
    None,
    EndExpression,
    AlphaNum,
    Operator,
    Bracket,
    Whitespace,
    StringLiteral,
}

fn categorize_char(c: char) -> CharCategory {
    match c {
        '+' | '*' | '/' | '%' | '=' | '!' | '&' | '|' | '^' | '~' | 'λ' | '>' | '<' | '@' | ','
        | '?' | ':' | ';' | '$' => {
            return CharCategory::Operator;
        }
        '(' | ')' | '{' | '}' | '[' | ']' => {
            return CharCategory::Bracket;
        }
        _ => {}
    }

    if c.is_alphanumeric() || c == '_' || c == '-' || c == '.' {
        return CharCategory::AlphaNum;
    } else if c.is_whitespace() {
        return CharCategory::Whitespace;
    }

    CharCategory::None
}

pub fn tokenize<'src>(source: &'src str) -> Vec<Token<'src>> {
    let mut tokens = Vec::with_capacity(5000);

    let mut last_category = CharCategory::None;
    let mut token_start = 0;

    let mut in_comment = false;

    let mut append = |i: usize, last_category: CharCategory, indentation: i32| {
        if last_category == CharCategory::StringLiteral {
            token_start += 1;
        }

        let token = &source[token_start..i];

        let kind = match last_category {
            CharCategory::AlphaNum => Some(TokenKind::Word),
            CharCategory::StringLiteral => Some(TokenKind::QuotedString),
            CharCategory::Operator => Some(TokenKind::Operator),
            CharCategory::Bracket => Some(TokenKind::Bracket),
            CharCategory::Whitespace => None,
            CharCategory::EndExpression => Some(TokenKind::ExpressionTerminator),
            CharCategory::None => None,
        };

        if kind == Some(TokenKind::ExpressionTerminator) {
            if tokens.is_empty() {
                token_start = i;
                return;
            }
        }

        if let Some(mut kind) = kind {
            match token {
                "where" | "if" | "then" | "else" | "use" | "unqualified" | "let" | "while"
                | "do" | "true" | "false" | "for" | "in" => kind = TokenKind::Keyword,
                "-" => kind = TokenKind::Operator, // minus is allowed in identifiers; convert back
                // if alone
                _ => {}
            };

            tokens.push(Token {
                token,
                range: (token_start, i),
                kind,
                indentation: indentation as usize,
            });
        }

        token_start = i;
    };

    let mut indentation = 0;
    let mut last_indentation = 0;
    let mut counting_indentation = true;
    let mut in_string_literal = false;

    for (i, c) in source.char_indices() {
        if c == '\n' || c == '\r' {
            indentation = 0;
            counting_indentation = true;
            in_comment = false;
        }

        if in_comment {
            append(i, last_category, indentation);
            last_category = CharCategory::None;
            continue;
        }

        if c == '#' {
            in_comment = true;
            append(i, last_category, indentation);
            last_category = CharCategory::None;
            continue;
        }

        if c == '"' {
            if in_string_literal {
                last_category = CharCategory::StringLiteral;
            }

            in_string_literal = !in_string_literal;
            append(i, last_category, indentation);
            last_category = CharCategory::None;
            continue;
        }

        if in_string_literal {
            continue;
        }

        if counting_indentation {
            if c.is_whitespace() {
                if c == ' ' {
                    indentation += 1;
                }

                if c == '\t' {
                    indentation += 4;
                }
            } else {
                counting_indentation = false;

                if indentation == 0 {
                    last_category = CharCategory::EndExpression;
                    append(i, last_category, indentation);
                }

                last_indentation = indentation;
            }
        }

        let mut category = categorize_char(c);

        if c == '-'
            && source.len() > i + 1
            && categorize_char(source[i + 1..].chars().next().unwrap()) == CharCategory::Operator
        {
            category = CharCategory::Operator;
        }

        // For paths
        if c == '/'
            && source.len() > i + 1
            && categorize_char(source[i + 1..].chars().next().unwrap()) == CharCategory::AlphaNum
        {
            category = CharCategory::AlphaNum;
        }

        // For range operator
        if c == '.'
            // Make sure that `..` at the start of paths still works
            && (last_category == CharCategory::AlphaNum || last_category == CharCategory::Bracket)
            && source.len() > i + 1
            && source[i + 1..].chars().next().unwrap() == '.'
        {
            category = CharCategory::Operator;
        }
        if c == '.' && last_category == CharCategory::Operator {
            category = CharCategory::Operator;
        }

        // Allow prime in identifiers. E.g. `x'` or `x''`
        if c == '\'' && last_category == CharCategory::AlphaNum {
            category = CharCategory::AlphaNum;
        }

        // Variables
        if c == '$'
            && source.len() > i + 1
            && categorize_char(source[i + 1..].chars().next().unwrap()) == CharCategory::AlphaNum
        {
            category = CharCategory::AlphaNum;
        }

        if category == last_category && last_category != CharCategory::Bracket {
            continue;
        }

        append(i, last_category, indentation);
        last_category = category;
    }

    append(source.len(), last_category, indentation);

    tokens
}
