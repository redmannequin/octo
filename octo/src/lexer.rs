//! Tokenization of source code
use crate::token::{Span, Token, TokenType};

/// An iterator over the tokens of the given source
#[derive(Debug, Clone, Copy)]
pub struct Lexer<'a> {
    src: CharHandler<'a>,
    pos: usize,
}

impl<'a> Lexer<'a> {
    /// Returns a Lexer for the given source
    ///
    /// # Arguments
    ///
    /// * `src` - A string slice that holds the source
    pub fn new(src: &'a str) -> Self {
        let src = CharHandler::new(src);
        Lexer { src, pos: 0 }
    }

    /// Gets the next token in the source
    pub fn next_token(&mut self) -> Token<'a> {
        self.skip_whitespace();
        match self.curr_char() {
            Some('+') => self.create_single_char_token(TokenType::Add),
            Some('-') => self.create_single_char_token(TokenType::Sub),
            Some('/') => self.create_single_char_token(TokenType::Div),
            Some('*') => self.create_single_char_token(TokenType::Mul),

            Some('!') => self.create_single_char_token(TokenType::Not),
            Some('=') => self.create_single_char_token(TokenType::Assign),
            Some('<') => self.create_single_char_token(TokenType::LT),
            Some('>') => self.create_single_char_token(TokenType::GT),

            Some('.') => self.create_single_char_token(TokenType::Dot),

            Some(',') => self.create_single_char_token(TokenType::Comma),
            Some(';') => self.create_single_char_token(TokenType::Semicolon),
            Some(':') => self.create_single_char_token(TokenType::Colon),

            Some('(') => self.create_single_char_token(TokenType::LParam),
            Some(')') => self.create_single_char_token(TokenType::RParam),

            Some('[') => self.create_single_char_token(TokenType::LBracket),
            Some(']') => self.create_single_char_token(TokenType::RBracket),

            Some('{') => self.create_single_char_token(TokenType::LBrace),
            Some('}') => self.create_single_char_token(TokenType::RBrace),

            Some(ch) => {
                if ch.is_ascii_alphabetic() {
                    self.get_identifier()
                } else if ch.is_ascii_digit() {
                    self.get_number()
                } else {
                    unimplemented!()
                }
            }
            None => Token::new("", TokenType::EOF, Span::new(self.pos, self.pos)),
        }
    }

    /// advances source to next non whitespace char
    fn skip_whitespace(&mut self) {
        while let Some(ch) = self.curr_char() {
            if !ch.is_whitespace() {
                break;
            }
            self.next_char();
        }
    }

    /// peeks for the next char
    #[inline]
    fn peek_char(&self) -> Option<char> {
        self.src.peek()
    }

    /// gets the current char
    #[inline]
    fn curr_char(&self) -> Option<char> {
        self.src.curr()
    }

    /// advances the source by one char
    #[inline]
    fn next_char(&mut self) -> Option<char> {
        match self.src.next_char() {
            None => None,
            some => {
                self.pos += 1;
                some
            }
        }
    }

    /// initialize an identifier token from source
    #[inline]
    fn get_identifier(&mut self) -> Token<'a> {
        let s = self.pos;
        while let Some(c) = self.next_char() {
            if !(c.is_ascii_alphanumeric() || c == '_') {
                break;
            }
        }

        match self.src.span(s, self.pos) {
            "let" => self.create_span_token(s, self.pos, TokenType::Let),
            "if" => self.create_span_token(s, self.pos, TokenType::If),
            "else" => self.create_span_token(s, self.pos, TokenType::Else),
            "false" | "true" => self.create_span_token(s, self.pos, TokenType::Bool),
            "return" => self.create_span_token(s, self.pos, TokenType::Return),
            "fn" => self.create_span_token(s, self.pos, TokenType::Func),
            "impl" => self.create_span_token(s, self.pos, TokenType::Impl),
            "struct" => self.create_span_token(s, self.pos, TokenType::Struct),
            "enum" => self.create_span_token(s, self.pos, TokenType::Enum),
            _ => self.create_span_token(s, self.pos, TokenType::Identifier),
        }
    }

    /// initialize a number token from source
    #[inline]
    fn get_number(&mut self) -> Token<'a> {
        let s = self.pos;
        while let Some(c) = self.next_char() {
            if !c.is_ascii_digit() {
                break;
            }
        }
        self.create_span_token(s, self.pos, TokenType::Number)
    }

    /// initialize a token from a single char and it's token type
    #[inline]
    fn create_single_char_token(&mut self, typ: TokenType) -> Token<'a> {
        let s = self.pos;
        let e = s + 1;
        self.next_char();
        self.create_span_token(s, e, typ)
    }

    /// initialize a toke from a span of chars
    #[inline]
    fn create_span_token(&self, s: usize, e: usize, typ: TokenType) -> Token<'a> {
        Token::new(self.src.span(s, e), typ, Span::new(s, e))
    }
}

#[derive(Debug, Clone, Copy)]
struct CharHandler<'a> {
    src: &'a str,
    pos: usize,
}

impl<'a> CharHandler<'a> {
    #[inline]
    fn new(src: &'a str) -> Self {
        Self { src, pos: 0 }
    }

    #[inline]
    fn next_char(&mut self) -> Option<char> {
        self.pos += 1;
        self.src.chars().nth(self.pos)
    }

    #[inline]
    fn curr(&self) -> Option<char> {
        self.src.chars().nth(self.pos)
    }

    #[inline]
    fn peek(&self) -> Option<char> {
        self.src.chars().nth(self.pos + 1)
    }

    #[inline]
    fn span(&self, s: usize, e: usize) -> &'a str {
        &self.src[s..e]
    }
}

#[test]
fn test_new_char_handler() {
    let char_handler = CharHandler::new("test");
    assert_eq!(char_handler.curr(), Some('t'));
    assert_eq!(char_handler.peek(), Some('e'));
}

#[test]
fn test_char_handler_next_char() {
    let mut char_handler = CharHandler::new("test");
    assert_eq!(char_handler.next_char(), Some('e'));
    assert_eq!(char_handler.curr(), Some('e'));
    assert_eq!(char_handler.peek(), Some('s'));
}

#[test]
fn test_char_handler_empty_str() {
    let mut char_handler = CharHandler::new("");
    assert_eq!(char_handler.next_char(), None);
    assert_eq!(char_handler.curr(), None);
    assert_eq!(char_handler.peek(), None);
}

#[test]
fn test_lexer_ident_token() {
    let src = " test ";
    let mut lexer = Lexer::new(src);
    assert_eq!(
        lexer.next_token(),
        Token::new(&src[1..5], TokenType::Identifier, Span::new(1, 5))
    )
}

#[test]
fn test_lexer_let_stmt_token() {
    let src = " let test = 5645; ";
    let mut lexer = Lexer::new(src);
    assert_eq!(
        lexer.next_token(),
        Token::new(&src[1..4], TokenType::Let, Span::new(1, 4))
    );
    assert_eq!(
        lexer.next_token(),
        Token::new(&src[5..9], TokenType::Identifier, Span::new(5, 9))
    );
    assert_eq!(
        lexer.next_token(),
        Token::new(&src[10..11], TokenType::Assign, Span::new(10, 11))
    );
    assert_eq!(
        lexer.next_token(),
        Token::new(&src[12..16], TokenType::Number, Span::new(12, 16))
    );
    assert_eq!(
        lexer.next_token(),
        Token::new(&src[16..17], TokenType::Semicolon, Span::new(16, 17))
    );
}

#[test]
fn test_lexer_keyword_let() {
    let src = " let ";
    let mut lexer = Lexer::new(src);
    assert_eq!(
        lexer.next_token(),
        Token::new(&src[1..4], TokenType::Let, Span::new(1, 4)),
    )
}
#[test]
fn test_lexer_keyword_if() {
    let src = " if ";
    let mut lexer = Lexer::new(src);
    assert_eq!(
        lexer.next_token(),
        Token::new(&src[1..3], TokenType::If, Span::new(1, 3)),
    )
}
#[test]
fn test_lexer_keyword_else() {
    let src = " else ";
    let mut lexer = Lexer::new(src);
    assert_eq!(
        lexer.next_token(),
        Token::new(&src[1..5], TokenType::Else, Span::new(1, 5)),
    )
}
#[test]
fn test_lexer_keyword_false() {
    let src = " false ";
    let mut lexer = Lexer::new(src);
    assert_eq!(
        lexer.next_token(),
        Token::new(&src[1..6], TokenType::Bool, Span::new(1, 6)),
    )
}
#[test]
fn test_lexer_keyword_true() {
    let src = " true ";
    let mut lexer = Lexer::new(src);
    assert_eq!(
        lexer.next_token(),
        Token::new(&src[1..5], TokenType::Bool, Span::new(1, 5)),
    )
}
#[test]
fn test_lexer_keyword_return() {
    let src = " return ";
    let mut lexer = Lexer::new(src);
    assert_eq!(
        lexer.next_token(),
        Token::new(&src[1..7], TokenType::Return, Span::new(1, 7)),
    )
}
#[test]
fn test_lexer_keyword_fn() {
    let src = " fn ";
    let mut lexer = Lexer::new(src);
    assert_eq!(
        lexer.next_token(),
        Token::new(&src[1..3], TokenType::Func, Span::new(1, 3)),
    )
}
#[test]
fn test_lexer_keyword_struct() {
    let src = " struct ";
    let mut lexer = Lexer::new(src);
    assert_eq!(
        lexer.next_token(),
        Token::new(&src[1..7], TokenType::Struct, Span::new(1, 7)),
    )
}
#[test]
fn test_lexer_keyword_impl() {
    let src = " impl ";
    let mut lexer = Lexer::new(src);
    assert_eq!(
        lexer.next_token(),
        Token::new(&src[1..5], TokenType::Impl, Span::new(1, 5)),
    )
}
#[test]
fn test_lexer_keyword_enum() {
    let src = " enum ";
    let mut lexer = Lexer::new(src);
    assert_eq!(
        lexer.next_token(),
        Token::new(&src[1..5], TokenType::Enum, Span::new(1, 5)),
    )
}
