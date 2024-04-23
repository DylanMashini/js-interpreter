#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    // Keywords
    If,
    Else,
    Let,
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
    Not,
    Equal,
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

        for line in self.source_code.lines() {
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
                            'a'..='z' | 'A'..='Z'  => {
                                current_token.push(c);
                                self.state = State::InIdentifier;
                            }
                            '"' | '\'' => {
                                self.state = State::InString(c);
                            },
                            '[' => tokens.push(Token::LBracket),
                            ']' => tokens.push(Token::RBracket),
                            '{' => tokens.push(Token::LCurlyBracket),
                            '}' => tokens.push(Token::RCurlyBracket),
                            '(' => tokens.push(Token::LParentheses),
                            ')' => tokens.push(Token::RParentheses),
                            '.' => tokens.push(Token::Dot),
                            ',' => tokens.push(Token::Comma),
                            ';' => tokens.push(Token::Semicolon),
                            '>' | '<' | '=' | '!' | '+' | '-' | '*' | '/' | '|' | '&'  => {
                                current_token.push(c);
                                self.state = State::InOperator
                            },
                            ' ' | '\t' => (), // Ignore whitespace
                            _ => (),
                        },
                        State::InNumber => match c {
                            '0'..='9' | '.' => current_token.push(c),
                            _ => {
                                if let Ok(num) = current_token.parse::<f64>() {
                                    tokens.push(Token::Number(num));
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
                                    "if" => tokens.push(Token::If),
                                    "else" => tokens.push(Token::Else),
                                    "let" => tokens.push(Token::Let),
                                    "var" => tokens.push(Token::Let),
                                    "null" => tokens.push(Token::Null),
                                    "undefined" => tokens.push(Token::Undefined),
                                    "true" => tokens.push(Token::Boolean(true)),
                                    "false" => tokens.push(Token::Boolean(false)),
                                    "while" => tokens.push(Token::While),
                                    "for" => tokens.push(Token::For),
                                    "function" => tokens.push(Token::Function),
                                    "return" => tokens.push(Token::Return),
                                    _ => tokens.push(Token::Identifier(current_token.clone())),
                                }
                                current_token.clear();
                                self.state = State::Normal;
                                continue;
                            }
                        },
                        State::InString(quote_char) => {
                            if c == quote_char {
                                tokens.push(Token::String(current_token.clone()));
                                current_token.clear();
                                self.state = State::Normal;
                            } else {
                                current_token.push(c);
                            }
                        },
                        State::InOperator => {
                            match c {
                                '=' | '+' | '-' | '|' | '&' => {
                                    current_token.push(c);
                                }
                                _ => {
                                    match current_token.as_str() {
                                        "=" => tokens.push(Token::Equal),
                                        "==" => tokens.push(Token::DoubleEqual),
                                        "===" => tokens.push(Token::TripleEqual),
                                        ">" => tokens.push(Token::GreaterThan),
                                        "<" => tokens.push(Token::LessThan),
                                        ">=" => tokens.push(Token::GreaterThanEqualTo),
                                        "<=" => tokens.push(Token::LessThanEqualTo),
                                        "!" => tokens.push(Token::Not),
                                        "!=" => tokens.push(Token::NotEqual),
                                        "+" => tokens.push(Token::Plus),
                                        "-" => tokens.push(Token::Minus),
                                        "++" => tokens.push(Token::DoublePlus),
                                        "--" => tokens.push(Token::DoubleMinus),
                                        "*" => tokens.push(Token::Multiply),
                                        "/" => tokens.push(Token::Divide),
                                        "||" => tokens.push(Token::Or),
                                        "&&" => tokens.push(Token::And),
                                        _ => unreachable!("Match statement should be exhaustive")

                                    }
                                    current_token.clear();
                                    self.state = State::Normal;
                                    continue;
                                }
                            }
                        }
                    }
                    self.position += 1;
                    break;
                }
            }
            if tokens.last() != Some(&Token::EOL) {
                tokens.push(Token::EOL);
            }
        }

        tokens
    }

}
