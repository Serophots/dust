use std::str::Chars;

use miette::{LabeledSpan, Result};

use crate::token::{Token, TokenKind};

/// Transforms utf8 text input into an iterator of tokens
pub struct Lexer<'a> {
    /// The source code fed into this lexer
    pub source: &'a str,
    /// The remaining source code to be lexed
    remaining: Chars<'a>,

    // Internal lexer state:
    pub byte: usize,
    rest: &'a str,
    started: Started,
}

#[derive(Debug)]
enum Started {
    None,
    StringLiteral(usize),
    NumberLiteral(usize),
    SnippetComment(usize),
    LineComment {
        /// Where the `// ..` slashes start
        lower: usize,
        /// Where the actual comment string starts
        lower_literal: usize,
        doc: bool,
    },
    Identifier(usize),
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Self {
        Lexer {
            source: source,
            remaining: source.chars(),

            byte: 0,
            rest: source,
            started: Started::None,
        }
    }

    /// Pulls the next character from `self.remaining`, whilst
    /// maintaining `self.byte` and `self.rest`
    fn next_char(&mut self) -> Option<char> {
        let char = self.remaining.next()?;
        self.byte += char.len_utf8();
        self.rest = self.remaining.as_str();

        Some(char)
    }

    /// Peak at the next char without incrementing the reader
    /// and without mutating any state
    fn peak_char(&self) -> Option<char> {
        self.rest.chars().next()
    }

    /// Pull the next character and process it.
    ///
    /// NOTE: a None return does **NOT** indicate that the source has ran out,
    /// but rather just that this character was not enough to constitute a Token
    /// by itself. The caller must check whether the source has ran out.
    fn process_next_char(&mut self) -> Option<Result<Token<TokenKind<'a>>>> {
        let byte = self.byte;
        let char = self.peak_char()?;

        match self.started {
            Started::None => {
                // Consume the character
                let _ = self.next_char();

                self.started = match char {
                    '(' => return Some(Ok(Token::new(TokenKind::LeftParen, byte..self.byte))),
                    ')' => return Some(Ok(Token::new(TokenKind::RightParen, byte..self.byte))),
                    '{' => return Some(Ok(Token::new(TokenKind::LeftBrace, byte..self.byte))),
                    '}' => return Some(Ok(Token::new(TokenKind::RightBrace, byte..self.byte))),
                    ',' => return Some(Ok(Token::new(TokenKind::Comma, byte..self.byte))),
                    '-' => return Some(Ok(Token::new(TokenKind::Minus, byte..self.byte))),
                    '+' => return Some(Ok(Token::new(TokenKind::Plus, byte..self.byte))),
                    ';' => return Some(Ok(Token::new(TokenKind::Semicolon, byte..self.byte))),
                    '*' => return Some(Ok(Token::new(TokenKind::Star, byte..self.byte))),

                    '!' => match self.peak_char() {
                        Some('=') => {
                            self.next_char();
                            return Some(Ok(Token::new(TokenKind::BangEqual, byte..self.byte)));
                        }
                        _ => return Some(Ok(Token::new(TokenKind::Bang, byte..self.byte))),
                    },
                    '=' => match self.peak_char() {
                        Some('=') => {
                            self.next_char();
                            return Some(Ok(Token::new(TokenKind::EqualEqual, byte..self.byte)));
                        }
                        _ => return Some(Ok(Token::new(TokenKind::Equal, byte..self.byte))),
                    },
                    '<' => match self.peak_char() {
                        Some('=') => {
                            self.next_char();
                            return Some(Ok(Token::new(TokenKind::LesserEqual, byte..self.byte)));
                        }
                        _ => return Some(Ok(Token::new(TokenKind::Lesser, byte..self.byte))),
                    },
                    '>' => match self.peak_char() {
                        Some('=') => {
                            self.next_char();
                            return Some(Ok(Token::new(TokenKind::GreaterEqual, byte..self.byte)));
                        }
                        _ => return Some(Ok(Token::new(TokenKind::Greater, byte..self.byte))),
                    },

                    '/' => match self.peak_char() {
                        Some('/') => {
                            self.next_char();

                            if let Some('/') = self.peak_char() {
                                self.next_char();
                                Started::LineComment {
                                    lower: byte,
                                    lower_literal: self.byte,
                                    doc: true,
                                }
                            } else {
                                Started::LineComment {
                                    lower: byte,
                                    lower_literal: self.byte,
                                    doc: false,
                                }
                            }
                        }
                        Some('*') => {
                            self.next_char();
                            Started::SnippetComment(self.byte)
                        }
                        _ => return Some(Ok(Token::new(TokenKind::Slash, byte..self.byte))),
                    },

                    '&' if self.peak_char() == Some('&') => {
                        self.next_char();
                        return Some(Ok(Token::new(TokenKind::And, byte..self.byte)));
                    }

                    '|' if self.peak_char() == Some('|') => {
                        self.next_char();
                        return Some(Ok(Token::new(TokenKind::Or, byte..self.byte)));
                    }

                    '"' => Started::StringLiteral(
                        // Start the string at the next character (exclude "")
                        self.byte,
                    ),
                    '0'..='9' | '.' => Started::NumberLiteral(
                        // Start the number at this current charcater
                        byte,
                    ),
                    'a'..='z' | '_' | 'A'..='Z' => Started::Identifier(
                        // Start the identifier at this current charcater
                        byte,
                    ),

                    char if char.is_whitespace() => Started::None,

                    _ => {
                        return Some(Err(miette::miette!(
                            labels = vec![LabeledSpan::at(byte..self.byte, "this token")],
                            "unexpected token '{char}'"
                        )
                        .with_source_code(self.source.to_owned())));
                    }
                };
            }
            Started::StringLiteral(lower) => {
                // Consume the character
                let _ = self.next_char();

                match char {
                    '"' => {
                        self.started = Started::None;

                        // include the lower byte which is after the leading "
                        // exclude the upper byte which is *on* the trailing "
                        let range = lower..byte;
                        let literal = self.source.get(range.clone()).unwrap();

                        return Some(Ok(Token::new(TokenKind::StringLiteral(literal), range)));
                    }
                    _ => {}
                };
            }
            Started::NumberLiteral(lower) => match char {
                '0'..='9' | '.' => {
                    // Consume the character
                    let _ = self.next_char();
                }
                _ => {
                    // DO NOT consume the next character
                    return Some(self.finish_number_literal(lower, byte));
                }
            },
            Started::Identifier(lower) => match char {
                'a'..='z' | '_' | 'A'..='Z' => {
                    // Consume the character
                    let _ = self.next_char();
                }
                _ => {
                    // DO NOT consume the next character
                    return Some(self.finish_identifier(lower, byte));
                }
            },
            Started::LineComment {
                lower,
                lower_literal,
                doc,
            } => {
                // Consume the character
                let _ = self.next_char();

                match char {
                    '\n' => return Some(self.finish_line_comment(lower, lower_literal, doc, byte)),
                    _ => {}
                }
            }
            Started::SnippetComment(lower) => {
                // Consume the character
                let _ = self.next_char();

                match char {
                    '*' if self.peak_char() == Some('/') => {
                        // Consume the character
                        let _ = self.next_char();

                        self.started = Started::None;
                        let range = lower..byte;
                        let literal = self.source.get(range.clone()).unwrap().trim();

                        return Some(Ok(Token::new(TokenKind::Comment(literal), range)));
                    }
                    _ => {}
                }
            }
        }

        // This character was not enough to constitute a token;
        // we must process some more
        return self.process_next_char();
    }

    fn finish_number_literal(&mut self, lower: usize, byte: usize) -> Result<Token<TokenKind<'a>>> {
        self.started = Started::None;

        // include the lower byte which starts on the first digit
        // exclude the upper byte which is *on* the first character not to be a number digit
        // (i.e. we still need to process this trailing character)
        let range = lower..byte;
        let Ok(number) = self.source.get(range.clone()).unwrap().parse::<f64>() else {
            return Err(miette::miette!(
                labels = vec![LabeledSpan::at(range.clone(), "this number literal")],
                "malformed number literal"
            )
            .with_source_code(self.source.to_owned()));
        };

        return Ok(Token::new(TokenKind::NumberLiteral(number), range));
    }

    fn finish_identifier(&mut self, lower: usize, byte: usize) -> Result<Token<TokenKind<'a>>> {
        self.started = Started::None;

        // include the lower byte which begins the identifier
        // include the upper byte which sits just before the peeked byte
        let range = lower..byte;
        let identifier = self.source.get(range.clone()).unwrap();

        match identifier {
            "if" => return Ok(Token::new(TokenKind::If, range.clone())),
            "else" => return Ok(Token::new(TokenKind::Else, range.clone())),
            "true" => return Ok(Token::new(TokenKind::True, range.clone())),
            "false" => return Ok(Token::new(TokenKind::False, range.clone())),
            "while" => return Ok(Token::new(TokenKind::While, range.clone())),
            "for" => return Ok(Token::new(TokenKind::For, range.clone())),
            "fn" => return Ok(Token::new(TokenKind::Function, range.clone())),
            "nil" => return Ok(Token::new(TokenKind::Nil, range.clone())),
            "return" => {
                return Ok(Token::new(TokenKind::Return, range.clone()));
            }
            "let" => return Ok(Token::new(TokenKind::Let, range.clone())),
            _ => {
                return Ok(Token::new(TokenKind::Identifier(identifier), range.clone()));
            }
        };
    }

    fn finish_line_comment(
        &mut self,
        lower: usize,
        lower_literal: usize,
        doc: bool,
        byte: usize,
    ) -> Result<Token<TokenKind<'a>>> {
        self.started = Started::None;

        let token_range = lower..byte;
        let literal_range = lower_literal..byte;
        let literal = self.source.get(literal_range).unwrap().trim();

        if doc {
            return Ok(Token::new(TokenKind::DocComment(literal), token_range));
        } else {
            return Ok(Token::new(TokenKind::Comment(literal), token_range));
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Result<Token<TokenKind<'a>>>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.process_next_char() {
            Some(char) => Some(char),
            None => {
                // End of source

                match self.started {
                    Started::None => None,
                    Started::StringLiteral(lower) => {
                        self.started = Started::None;

                        return Some(Err(miette::miette!(
                            labels = vec![LabeledSpan::at(lower..self.byte, "this string literal")],
                            "malformed string literal"
                        )
                        .with_source_code(self.source.to_owned())));
                    }
                    Started::NumberLiteral(lower) => {
                        return Some(self.finish_number_literal(lower, self.byte));
                    }
                    Started::SnippetComment(lower) => {
                        self.started = Started::None;

                        return Some(Err(miette::miette!(
                            labels = vec![LabeledSpan::at(lower..self.byte, "this comment")],
                            "malformed comment"
                        )
                        .with_source_code(self.source.to_owned())));
                    }
                    Started::LineComment {
                        lower,
                        lower_literal,
                        doc,
                    } => {
                        return Some(self.finish_line_comment(
                            lower,
                            lower_literal,
                            doc,
                            self.byte,
                        ));
                    }
                    Started::Identifier(lower) => {
                        return Some(self.finish_identifier(lower, self.byte));
                    }
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use miette::{SourceOffset, SourceSpan};

    use crate::{
        lexer::Lexer,
        token::{Token, TokenKind},
    };

    #[test]
    fn test_lexer() {
        let lexer_test_script = include_str!("../test/lexer.rs");
        let lexer = Lexer::new(lexer_test_script);
        let tokens = lexer.map(|t| t.unwrap()).collect::<Vec<_>>();

        assert_eq!(
            tokens,
            vec![
                Token {
                    kind: TokenKind::Let,
                    src: SourceSpan::new(SourceOffset::from(0), 3)
                },
                Token {
                    kind: TokenKind::Identifier("test"),
                    src: SourceSpan::new(SourceOffset::from(4), 4)
                },
                Token {
                    kind: TokenKind::Equal,
                    src: SourceSpan::new(SourceOffset::from(9), 1)
                },
                Token {
                    kind: TokenKind::StringLiteral("beep boop"),
                    src: SourceSpan::new(SourceOffset::from(12), 9)
                },
                Token {
                    kind: TokenKind::Semicolon,
                    src: SourceSpan::new(SourceOffset::from(22), 1)
                },
                Token {
                    kind: TokenKind::If,
                    src: SourceSpan::new(SourceOffset::from(25), 2)
                },
                Token {
                    kind: TokenKind::Identifier("test"),
                    src: SourceSpan::new(SourceOffset::from(28), 4)
                },
                Token {
                    kind: TokenKind::EqualEqual,
                    src: SourceSpan::new(SourceOffset::from(33), 2)
                },
                Token {
                    kind: TokenKind::StringLiteral("beep boop"),
                    src: SourceSpan::new(SourceOffset::from(37), 9)
                },
                Token {
                    kind: TokenKind::LeftBrace,
                    src: SourceSpan::new(SourceOffset::from(48), 1)
                },
                Token {
                    kind: TokenKind::Identifier("print"),
                    src: SourceSpan::new(SourceOffset::from(54), 5)
                },
                Token {
                    kind: TokenKind::LeftParen,
                    src: SourceSpan::new(SourceOffset::from(59), 1)
                },
                Token {
                    kind: TokenKind::StringLiteral("boop beep"),
                    src: SourceSpan::new(SourceOffset::from(61), 9)
                },
                Token {
                    kind: TokenKind::RightParen,
                    src: SourceSpan::new(SourceOffset::from(71), 1)
                },
                Token {
                    kind: TokenKind::Semicolon,
                    src: SourceSpan::new(SourceOffset::from(72), 1)
                },
                Token {
                    kind: TokenKind::Comment("Bog standard comment"),
                    src: SourceSpan::new(SourceOffset::from(76), 20)
                },
                Token {
                    kind: TokenKind::Comment("whoo"),
                    src: SourceSpan::new(SourceOffset::from(103), 5)
                },
                Token {
                    kind: TokenKind::RightBrace,
                    src: SourceSpan::new(SourceOffset::from(109), 1)
                },
                Token {
                    kind: TokenKind::Else,
                    src: SourceSpan::new(SourceOffset::from(111), 4)
                },
                Token {
                    kind: TokenKind::LeftBrace,
                    src: SourceSpan::new(SourceOffset::from(116), 1)
                },
                Token {
                    kind: TokenKind::NumberLiteral(5.0),
                    src: SourceSpan::new(SourceOffset::from(122), 1)
                },
                Token {
                    kind: TokenKind::Plus,
                    src: SourceSpan::new(SourceOffset::from(124), 1)
                },
                Token {
                    kind: TokenKind::Comment("a snippet comment"),
                    src: SourceSpan::new(SourceOffset::from(128), 19)
                },
                Token {
                    kind: TokenKind::NumberLiteral(7.0),
                    src: SourceSpan::new(SourceOffset::from(150), 1)
                },
                Token {
                    kind: TokenKind::Semicolon,
                    src: SourceSpan::new(SourceOffset::from(151), 1)
                },
                Token {
                    kind: TokenKind::Identifier("print"),
                    src: SourceSpan::new(SourceOffset::from(157), 5)
                },
                Token {
                    kind: TokenKind::LeftParen,
                    src: SourceSpan::new(SourceOffset::from(162), 1)
                },
                Token {
                    kind: TokenKind::StringLiteral("foop fleep"),
                    src: SourceSpan::new(SourceOffset::from(164), 10)
                },
                Token {
                    kind: TokenKind::Comma,
                    src: SourceSpan::new(SourceOffset::from(175), 1)
                },
                Token {
                    kind: TokenKind::NumberLiteral(5.0),
                    src: SourceSpan::new(SourceOffset::from(177), 3)
                },
                Token {
                    kind: TokenKind::Slash,
                    src: SourceSpan::new(SourceOffset::from(181), 1)
                },
                Token {
                    kind: TokenKind::NumberLiteral(3.0),
                    src: SourceSpan::new(SourceOffset::from(183), 3)
                },
                Token {
                    kind: TokenKind::RightParen,
                    src: SourceSpan::new(SourceOffset::from(186), 1)
                },
                Token {
                    kind: TokenKind::Semicolon,
                    src: SourceSpan::new(SourceOffset::from(187), 1)
                },
                Token {
                    kind: TokenKind::Identifier("jeepers"),
                    src: SourceSpan::new(SourceOffset::from(193), 7)
                },
                Token {
                    kind: TokenKind::LeftParen,
                    src: SourceSpan::new(SourceOffset::from(200), 1)
                },
                Token {
                    kind: TokenKind::RightParen,
                    src: SourceSpan::new(SourceOffset::from(201), 1)
                },
                Token {
                    kind: TokenKind::Semicolon,
                    src: SourceSpan::new(SourceOffset::from(202), 1)
                },
                Token {
                    kind: TokenKind::RightBrace,
                    src: SourceSpan::new(SourceOffset::from(204), 1)
                },
                Token {
                    kind: TokenKind::DocComment("A doc comment on `jeepers`"),
                    src: SourceSpan::new(SourceOffset::from(210), 27)
                },
                Token {
                    kind: TokenKind::Function,
                    src: SourceSpan::new(SourceOffset::from(238), 2)
                },
                Token {
                    kind: TokenKind::Identifier("jeepers"),
                    src: SourceSpan::new(SourceOffset::from(241), 7)
                },
                Token {
                    kind: TokenKind::LeftParen,
                    src: SourceSpan::new(SourceOffset::from(248), 1)
                },
                Token {
                    kind: TokenKind::RightParen,
                    src: SourceSpan::new(SourceOffset::from(249), 1)
                },
                Token {
                    kind: TokenKind::LeftBrace,
                    src: SourceSpan::new(SourceOffset::from(251), 1)
                },
                Token {
                    kind: TokenKind::Identifier("print"),
                    src: SourceSpan::new(SourceOffset::from(257), 5)
                },
                Token {
                    kind: TokenKind::LeftParen,
                    src: SourceSpan::new(SourceOffset::from(262), 1)
                },
                Token {
                    kind: TokenKind::StringLiteral("jeepers"),
                    src: SourceSpan::new(SourceOffset::from(264), 7)
                },
                Token {
                    kind: TokenKind::RightParen,
                    src: SourceSpan::new(SourceOffset::from(272), 1)
                },
                Token {
                    kind: TokenKind::Semicolon,
                    src: SourceSpan::new(SourceOffset::from(273), 1)
                },
                Token {
                    kind: TokenKind::RightBrace,
                    src: SourceSpan::new(SourceOffset::from(275), 1)
                }
            ]
        );
    }

    #[test]
    fn test_trailing() {
        let tokens = Lexer::new("5").map(|t| t.unwrap()).collect::<Vec<_>>();

        assert_eq!(
            tokens,
            vec![Token {
                kind: TokenKind::NumberLiteral(5.0),
                src: SourceSpan::new(SourceOffset::from(0), 1)
            },]
        );

        let tokens = Lexer::new("// trailing comment")
            .map(|t| t.unwrap())
            .collect::<Vec<_>>();

        assert_eq!(
            tokens,
            vec![Token {
                kind: TokenKind::Comment("trailing comment"),
                src: SourceSpan::new(SourceOffset::from(2), 17)
            },]
        );

        let tokens = Lexer::new("/* trailing snippet comment").collect::<Vec<_>>();
        assert!(tokens.len() == 1);
        assert!(tokens[0].is_err());

        let tokens = Lexer::new("\"trailing string literal").collect::<Vec<_>>();
        assert!(tokens.len() == 1);
        assert!(tokens[0].is_err());

        let tokens = Lexer::new("trailing_identifier")
            .map(|t| t.unwrap())
            .collect::<Vec<_>>();

        assert_eq!(
            tokens,
            vec![Token {
                kind: TokenKind::Identifier("trailing_identifier"),
                src: SourceSpan::new(SourceOffset::from(0), 19)
            },]
        );
    }
}
