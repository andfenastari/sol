#[derive(Copy, Clone)]
pub struct Tokenizer<'a> {
	input: &'a str,
	current: usize,
	location: Location,
}

#[derive(Debug, Copy, Clone)]
pub struct Token<'a> {
	pub kind: TokenKind,
	pub text: &'a str,
	pub location: Location,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum TokenKind {
	Ident,
	Operator,
	StrLit,
	NumLit,
	LParen,
	RParen,
	LCurly,
	RCurly,
	Len,
	Not,
	Let,
	Eq,
	In,
	If,
	Then,
	Else,
	RArrow,
	LArrow,
	Bar,
	Comma,
	Dot,
	Unknown,
}

#[derive(Debug, Copy, Clone)]
pub struct Location {
	pub index: usize,
	pub line: usize,
	pub offset: usize,
}

impl<'a> Tokenizer<'a> {
	pub fn new(input: &'a str) -> Self {
		Self {
			input,
			current: 0,
			location: Location {
				index: 0,
				line: 0,
				offset: 0,
			},
		}
	}

	pub fn location(&self) -> Location {
		self.location
	}

	fn current(&self) -> Option<char> {
		self.input.get(self.current..)?.chars().next()
	}

	fn current_text(&self) -> Option<&'a str> {
		let len = self.current()?.len_utf8();
		Some(&self.input[self.location.index..self.current+len])
	}

	fn advance(&mut self) -> Option<()> {
		let curr = self.current()?;
		if curr == '\n' {
			self.location.line += 1;
			self.location.offset = 0;
		}
		self.current += curr.len_utf8();
		Some(())
	}

	fn peek(&self) -> Option<char> {
		let curr = self.current()?;
		let len = curr.len_utf8();
		self.input.get(self.current+len..)?.chars().next()
	}

	fn emit_token(&mut self, kind: TokenKind) -> Option<Token<'a>> {
		let ret = Token {
			kind,
			text: self.current_text()?,
			location: self.location,
		};
		self.current += 1;
		self.location.index = self.current;
		self.location.offset = self.current;
		Some(ret)
	}
}

impl<'a> Iterator for Tokenizer<'a> {
	type Item = Token<'a>;

	fn next<'b>(&'b mut self) -> Option<Token<'a>> {
		// Skip whitespace
		while self.current()?.is_whitespace() {
			self.advance();
		}
		self.location.index = self.current;
		self.location.offset = self.current;

		let c = self.current()?;
		
		// Single character tokens
		match c {
			'(' => return self.emit_token(TokenKind::LParen),
			')' => return self.emit_token(TokenKind::RParen),
			'{' => return self.emit_token(TokenKind::LCurly),
			'}' => return self.emit_token(TokenKind::RCurly),
			'.' => return self.emit_token(TokenKind::Dot),
			',' => return self.emit_token(TokenKind::Comma),
			'!' => return self.emit_token(TokenKind::Not),
			'=' => return self.emit_token(TokenKind::Eq),
			'|' => return self.emit_token(TokenKind::Bar),
			'#' => return self.emit_token(TokenKind::Len),
			_ => (),
		}

		// Identifiers and reserved words
		if c.is_alphabetic() || c == '_' {
			while let Some(n) = self.peek() {
				if !n.is_alphanumeric() && n != '\'' && n != '_' { break }
				self.advance();
			}
			match self.current_text().unwrap() {
				"let"  => return self.emit_token(TokenKind::Let),
				"in"   => return self.emit_token(TokenKind::In),
				"if"   => return self.emit_token(TokenKind::If),
				"then" => return self.emit_token(TokenKind::Then),
				"else" => return self.emit_token(TokenKind::Else),
				_      => return self.emit_token(TokenKind::Ident), 
			}
		}

		// Operators
		if is_operator_char(c) {
			while let Some(n) = self.peek() {
				if !is_operator_char(n) { break }
				self.advance();
			}
			match self.current_text().unwrap() {
				"<-" => return self.emit_token(TokenKind::LArrow),
				"->" => return self.emit_token(TokenKind::RArrow),
				_    => return self.emit_token(TokenKind::Operator),
			}
		}

		// Number literals
		if c.is_digit(10) {
			// TODO: Parse hex, binary and octal literals
			while let Some(n) = self.peek() {
				if !n.is_digit(10) { break }
				self.advance();
			}
			if self.peek() == Some('.') {
				self.advance();
				while let Some(n) = self.peek() {
					if !n.is_digit(10) { break }
					self.advance();
				}
			}
			return self.emit_token(TokenKind::NumLit);
		}

		// String literals
		if c == '"' {
			while let Some(n) = self.peek() {
				self.advance();
				if n == '"' { break }
				// Escape sequence, skip next character
				if n == '\\' { self.advance(); }
			}
			return self.emit_token(TokenKind::StrLit);
		}

		self.emit_token(TokenKind::Unknown)
	}
}

fn is_operator_char(c: char) -> bool {
	if let '+'|'-'|'*'|'/'|'$'|'#'|'&'|'|'|'!'|'%'|'^'|'~'|'='|'>'|'<' = c {
		true
	} else {
		false
	}
}