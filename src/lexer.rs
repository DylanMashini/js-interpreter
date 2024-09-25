#[derive(Debug, PartialEq, Clone)]
pub struct Token {
    pub value: TokenValue,
    pub line: usize,
}

impl Token {
    pub fn new(value: TokenValue, line: usize) -> Token {
        Token { value, line }
    }
}

impl Into<TokenValue> for Token {
    fn into(self) -> TokenValue {
        self.value
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum TokenValue {
    // Keywords
    If,
    Else,
    Let,
    Const,
    While,
    For,
    Function,
    Return,

    Identifier(String),

    // Literals
    Number(f64),
    Boolean(bool),
    String(String),
    Null,
    Undefined,
    // Punctuators
    LBracket,
    RBracket,
    LCurlyBracket,
    RCurlyBracket,
    LParentheses,
    RParentheses,
    Dot,
    Comma,
    Colon,
    Not,
    Equal,
    Arrow,
    DoubleEqual,
    NotEqual,
    TripleEqual,
    GreaterThan,
    LessThan,
    GreaterThanEqualTo,
    LessThanEqualTo,
    Plus,
    Minus,
    Multiply,
    Divide,
    DoublePlus,
    DoubleMinus,
    Modulo,
    PlusEqual,
    MinusEqual,
    Semicolon,
    Or,
    And,
    EOL,
}

#[derive(Debug, PartialEq)]
enum State {
    Normal,
    InNumber,
    InIdentifier,
    InOperator,
    InString(char), // char represents the quote character used
}

pub struct Lexer {
    source_code: String,
    position: usize,
    state: State,
}

impl Lexer {
    pub fn new(source_code: String) -> Lexer {
        Self {
            source_code,
            position: 0,
            state: State::Normal,
        }
    }

    pub fn tokenize(&mut self) -> Vec<Token> {
        let mut tokens = Vec::new();
        let mut current_token = String::new();

        for (line_num, line) in self.source_code.lines().enumerate() {
            let _ = self.position.saturating_sub(1); // For the extra charecter chained at the end of each line
            for c in line.chars().chain(std::iter::once(' ')) {
                // Add space at end to so last token still gets properly processed
                loop {
                    match self.state {
                        State::Normal => match c {
                            '0'..='9' => {
                                current_token.push(c);
                                self.state = State::InNumber;
                            }
                            'a'..='z' | 'A'..='Z' => {
                                current_token.push(c);
                                self.state = State::InIdentifier;
                            }
                            '"' | '\'' => {
                                self.state = State::InString(c);
                            }
                            '[' => tokens.push(Token::new(TokenValue::LBracket, line_num + 1)),
                            ']' => tokens.push(Token::new(TokenValue::RBracket, line_num + 1)),
                            '{' => tokens.push(Token::new(TokenValue::LCurlyBracket, line_num + 1)),
                            '}' => tokens.push(Token::new(TokenValue::RCurlyBracket, line_num + 1)),
                            '(' => tokens.push(Token::new(TokenValue::LParentheses, line_num + 1)),
                            ')' => tokens.push(Token::new(TokenValue::RParentheses, line_num + 1)),
                            '.' => tokens.push(Token::new(TokenValue::Dot, line_num + 1)),
                            '%' => tokens.push(Token::new(TokenValue::Modulo, line_num + 1)),
                            ',' => tokens.push(Token::new(TokenValue::Comma, line_num + 1)),
                            ';' => tokens.push(Token::new(TokenValue::Semicolon, line_num + 1)),
                            ':' => tokens.push(Token::new(TokenValue::Colon, line_num + 1)),
                            '>' | '<' | '=' | '!' | '+' | '-' | '*' | '/' | '|' | '&' | '/' => {
                                current_token.push(c);
                                self.state = State::InOperator
                            }
                            ' ' | '\t' => (), // Ignore whitespace
                            _ => (),
                        },
                        State::InNumber => match c {
                            '0'..='9' | '.' => current_token.push(c),
                            _ => {
                                if let Ok(num) = current_token.parse::<f64>() {
                                    tokens.push(Token::new(TokenValue::Number(num), line_num + 1));
                                }
                                current_token.clear();
                                self.state = State::Normal;
                                continue;
                            }
                        },
                        State::InIdentifier => match c {
                            'a'..='z' | 'A'..='Z' | '0'..='9' => current_token.push(c),
                            _ => {
                                match current_token.as_str() {
                                    "if" => tokens.push(Token::new(TokenValue::If, line_num + 1)),
                                    "else" => {
                                        tokens.push(Token::new(TokenValue::Else, line_num + 1))
                                    }
                                    "let" => tokens.push(Token::new(TokenValue::Let, line_num + 1)),
                                    "const" => {
                                        tokens.push(Token::new(TokenValue::Const, line_num + 1))
                                    }
                                    "var" => tokens.push(Token::new(TokenValue::Let, line_num + 1)),
                                    "null" => {
                                        tokens.push(Token::new(TokenValue::Null, line_num + 1))
                                    }
                                    "undefined" => {
                                        tokens.push(Token::new(TokenValue::Undefined, line_num + 1))
                                    }
                                    "true" => tokens
                                        .push(Token::new(TokenValue::Boolean(true), line_num + 1)),
                                    "false" => tokens
                                        .push(Token::new(TokenValue::Boolean(false), line_num + 1)),
                                    "while" => {
                                        tokens.push(Token::new(TokenValue::While, line_num + 1))
                                    }
                                    "for" => tokens.push(Token::new(TokenValue::For, line_num + 1)),
                                    "function" => {
                                        tokens.push(Token::new(TokenValue::Function, line_num + 1))
                                    }
                                    "return" => {
                                        tokens.push(Token::new(TokenValue::Return, line_num + 1))
                                    }
                                    _ => tokens.push(Token::new(
                                        TokenValue::Identifier(current_token.clone()),
                                        line_num + 1,
                                    )),
                                }
                                current_token.clear();
                                self.state = State::Normal;
                                continue;
                            }
                        },
                        State::InString(quote_char) => {
                            if c == quote_char {
                                tokens.push(Token::new(
                                    TokenValue::String(current_token.clone()),
                                    line_num + 1,
                                ));
                                current_token.clear();
                                self.state = State::Normal;
                            } else {
                                current_token.push(c);
                            }
                        }
                        State::InOperator => match c {
                            '=' | '+' | '-' | '|' | '&' | '>' | '/' => {
                                current_token.push(c);
                            }
                            _ => {
                                match current_token.as_str() {
                                        "=" => tokens.push(Token::new(TokenValue::Equal, line_num + 1)),
                                        "==" => tokens.push(Token::new(TokenValue::DoubleEqual, line_num + 1)),
                                        "===" => tokens.push(Token::new(TokenValue::TripleEqual, line_num + 1)),
                                        ">" => tokens.push(Token::new(TokenValue::GreaterThan, line_num + 1)),
                                        "<" => tokens.push(Token::new(TokenValue::LessThan, line_num + 1)),
                                        ">=" => tokens.push(Token::new(TokenValue::GreaterThanEqualTo, line_num + 1)),
                                        "<=" => tokens.push(Token::new(TokenValue::LessThanEqualTo, line_num + 1)),
                                        "!" => tokens.push(Token::new(TokenValue::Not, line_num + 1)),
                                        "!=" | "!==" => tokens.push(Token::new(TokenValue::NotEqual, line_num + 1)),
                                        "+" => tokens.push(Token::new(TokenValue::Plus, line_num + 1)),
                                        "-" => tokens.push(Token::new(TokenValue::Minus, line_num + 1)),
                                        "++" => tokens.push(Token::new(TokenValue::DoublePlus, line_num + 1)),
                                        "--" => tokens.push(Token::new(TokenValue::DoubleMinus, line_num + 1)),
                                        "+=" => tokens.push(Token::new(TokenValue::PlusEqual, line_num + 1)),
                                        "-=" => tokens.push(Token::new(TokenValue::MinusEqual, line_num + 1)),
                                        "*" => tokens.push(Token::new(TokenValue::Multiply, line_num + 1)),
                                        "/" => tokens.push(Token::new(TokenValue::Divide, line_num + 1)),
                                        "||" => tokens.push(Token::new(TokenValue::Or, line_num + 1)),
                                        "&&" => tokens.push(Token::new(TokenValue::And, line_num + 1)),
                                        "=>" => tokens.push(Token::new(TokenValue::Arrow, line_num + 1)),
                                        "//" => {
                                            self.state = State::Normal;
                                            tokens.push(Token::new(TokenValue::EOL, line_num + 1));
                                            break;
                                        }
                                        val => panic!("Tokenization Error on line: {}\nUnknown operator token: {}", line_num + 1, val)

                                    }
                                current_token.clear();
                                self.state = State::Normal;
                                continue;
                            }
                        },
                    }
                    self.position += 1;
                    break;
                }
            }
            if tokens
                .last()
                .is_some_and(|tok| tok.value != TokenValue::EOL)
            {
                tokens.push(Token::new(TokenValue::EOL, line_num + 1));
            }
        }

        tokens
    }
}
