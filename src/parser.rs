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
}

pub fn parse(input: &str) -> ParseResult<Ast> {
	let mut t = Tokenizer::new(input);
	parse_expr(&mut t)
}

impl Error {
	fn expected_got(l: Location, e: TokenKind, g: TokenKind) -> Error {
		Error {
			location: l,
			description: format!("Expected {:?}, got {:?}", e, g),
		}
	}
}

fn parse_expr(t: &mut Tokenizer) -> ParseResult<Ast> {
	let checkpoint = t.clone();

	if let n@Ok(_) = parse_ident(t) {
		return n;
	}

	*t = checkpoint;
	if let n@Ok(_) = parse_numlit(t) {
		return n;
	}

	*t = checkpoint;
	if let n@Ok(_) = parse_strlit(t) {
		return n;
	}

	*t = checkpoint;
	let tok = next_token(t)?;
	Err(Error {
		location: tok.location,
		description: format!("Expected expression, got {:?}", tok.kind),
	})
}

fn parse_ident(t: &mut Tokenizer) -> ParseResult<Ast> {
	let tok = next_token(t)?;
	if tok.kind != TokenKind::Ident {
		return Err(Error::expected_got(tok.location, TokenKind::Ident, tok.kind));
	}
	Ok(Ast {
		location: tok.location,
		node: Node::Ident(tok.text.to_string()),
	})
}

fn parse_numlit(t: &mut Tokenizer) -> ParseResult<Ast> {
	let tok = next_token(t)?;
	if tok.kind != TokenKind::NumLit {
		return Err(Error::expected_got(tok.location, TokenKind::NumLit, tok.kind));
	}
	if let Ok(n) = tok.text.parse::<f32>() {
		Ok(Ast {
			location: tok.location,
			node: Node::NumLit(n),
		})
	} else {
		panic!("Unreachable");
	}
}

fn parse_strlit(t: &mut Tokenizer) -> ParseResult<Ast> {
	let tok = next_token(t)?;
	if tok.kind != TokenKind::StrLit {
		return Err(Error::expected_got(tok.location, TokenKind::StrLit, tok.kind));
	}
	Ok(Ast {
		location: tok.location,
		node: Node::StrLit(tok.text[1..tok.text.len()-1].to_string()), // Remove quotes
	})
}

fn next_token<'a>(t: &mut Tokenizer<'a>) -> ParseResult<Token<'a>> {
	t.next().ok_or(Error {
		location: t.location(),
		description: "End of input".to_string(),
	})
} 
