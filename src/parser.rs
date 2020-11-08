use crate::tokenizer::{Token, TokenKind, Tokenizer, Location};

#[derive(Debug, Clone)]
pub struct Error {
	pub location: Location,
	pub description: String,
}

type ParseResult<T> = Result<T, Error>;

#[derive(Debug, Clone)]
pub struct Ast {
	pub location: Location,
	pub node: Node,
}

#[derive(Debug, Clone)]
pub enum Node {
	Ident(String),
	NumLit(f32),
	StrLit(String),
	Funcall {
		fun: Box<Ast>,
		args: Box<[Ast]>,
	},
	IfExpr {
		conds: Box<[Ast]>,
		thens: Box<[Ast]>,
		r#else: Box<Ast>,
	},
}

struct Parser<'a> {
	peeked: Option<Token<'a>>,
	tokenizer: Tokenizer<'a>,
}

pub fn parse(src: &str) -> ParseResult<Ast> {
	Parser::new(src).parse_expr()
}

impl<'a> Parser<'a> {
	fn new(src: &'a str) -> Self {
		Parser {
			peeked: None,
			tokenizer: Tokenizer::new(src),
		}
	}

	fn current_location(&self) -> Location {
		self.tokenizer.location()
	}

	fn advance(&mut self) {
		if self.peeked.is_some() {
			self.peeked = None;
		} else {
			self.tokenizer.next();
		}
	}

	fn peek(&mut self) -> Option<Token<'a>> {
		if let Some(tok) = self.peeked {
			Some(tok)
		} else {
			self.peeked = self.tokenizer.next();
			self.peeked
		}
	}

	fn expect_token(&mut self, kind: TokenKind) -> ParseResult<Token<'a>> {
		if let Some(tok) = self.peek() {
			if tok.kind == kind {
				self.advance();
				Ok(tok)
			} else {
				Err(Error {
					location: tok.location,
					description: format!("Expected {:?}, but got {:?}", kind, tok.kind),
				})
			}
		} else {
			Err(Error {
				location: self.current_location(),
				description: format!("Expected {:?}, but got EOF", kind),
			})
		}
	}

	fn parse_expr(&mut self) -> ParseResult<Ast> {
		println!("\tparse_expr");
		if let Ok(fcall) = self.parse_funcall_or_delimited_expression() {
			return Ok(fcall)
		}
		self.parse_atom()
	}

	// A delimited expression is an atom or an arbitrary expression
	// delimited by parenthesis
	fn parse_delimited_expr(&mut self) -> ParseResult<Ast> {
		println!("\tparse_delimited");
		if let Ok(atom) = self.parse_atom() {
			return Ok(atom)
		}
		self.parse_paren_expr()
	}

	fn parse_paren_expr(&mut self) -> ParseResult<Ast> {
		println!("\tparse_paren_expr");
		self.expect_token(TokenKind::LParen)?;
		let expr = self.parse_expr()?;
		self.expect_token(TokenKind::RParen)?;
		Ok(expr)
	}

	// An atom is an identifier or a literal
	fn parse_atom(&mut self) -> ParseResult<Ast> {
		println!("\tparse_atom");
		// TODO: parse literals
		self.parse_ident()
	}

	fn parse_ident(&mut self) -> ParseResult<Ast> {
		println!("\tparse_ident");
		let ident = self.expect_token(TokenKind::Ident)?;
		Ok(Ast {
			location: ident.location,
			node: Node::Ident(ident.text.to_string()),
		})
	}

	// A funcall is one or more delimited expressions in sequence
	fn parse_funcall_or_delimited_expression(&mut self) -> ParseResult<Ast> {
		println!("\tparse_funcall");
		let loc = self.current_location();
		let fun = self.parse_delimited_expr()?;
		
		let mut args = Vec::new();
		while let Ok(arg) = self.parse_delimited_expr() {
			args.push(arg);	
		}
		// A funcall needs at least one argument
		if args.len() == 0 {
			return Ok(fun)
		}

		Ok(Ast {
			location: loc,
			node: Node::Funcall {
				fun: Box::new(fun),
				args: args.into(),
			}
		})
	}
}