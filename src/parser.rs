use crate::tokenizer::{Token, TokenKind, Tokenizer, Location};

pub type Result<'a, 'b, T> = std::result::Result<(T, &'b [Token<'a>]), Error>;

#[derive(Clone, Debug)]
pub struct Error {
	kind: ErrorKind,
	// If None, error occured at the end of input.
	location: Option<Location>,
}

#[derive(Clone, Debug)]
pub enum ErrorKind {
	UnexpectedEOF,
	UnexpectedNonExpression,
	UnexpectedNonBoolean,
	UnexpectedToken {
		want: TokenKind,
		got: TokenKind,
	},
}

#[derive(Clone, Debug)]
pub enum Expression {
	Number(f32),
	String(String),
	Identifier(String),
	Boolean(bool),
}

pub fn parse(input: &str) -> std::result::Result<Expression, Error> {
	let tokens: Vec<Token> = Tokenizer::new(input).collect();
	let (expr, rst) = parse_expression(&tokens)?;
	assert_eq!(rst.len(), 0);
	Ok(expr)
}

fn parse_expression<'a, 'b>(toks: &'b [Token<'a>]) -> Result<'a, 'b, Expression> {
	parse_number(toks).map(|e| (Expression::Number(e.0), e.1))
		.or_else(|_| parse_string(toks).map(|e| (Expression::String(e.0), e.1)))
		.or_else(|_| parse_identifier(toks).map(|e| (Expression::Identifier(e.0), e.1)))
		.or_else(|_| parse_boolean(toks).map(|e| (Expression::Boolean(e.0), e.1)))
		.map_err(|e| Error {
			kind: ErrorKind::UnexpectedNonExpression,
			location: e.location,
		})
}

fn parse_number<'a, 'b>(toks: &'b [Token<'a>]) -> Result<'a, 'b, f32> {
	// TODO: Parse hex, octal and binary
	expect_kind(toks, TokenKind::NumLit).map(|(tok, rst)| (tok.text.parse().unwrap(), rst))
}

fn parse_string<'a, 'b>(toks: &'b [Token<'a>]) -> Result<'a, 'b, String> {
	// TODO: Parse escape sequences
	expect_kind(toks, TokenKind::StrLit).map(|(tok, rst)| (tok.text[1..tok.text.len()-1].into(), rst))
}

fn parse_identifier<'a, 'b>(toks: &'b [Token<'a>]) -> Result<'a, 'b, String> {
	expect_kind(toks, TokenKind::Ident).map(|(tok, rst)| (tok.text.into(), rst))
}

fn parse_boolean<'a, 'b>(toks: &'b [Token<'a>]) -> Result<'a, 'b, bool> {
	let (tok, rst) = expect_token(toks)?;
	match tok.kind {
		TokenKind::True => Ok((true, rst)),
		TokenKind::False => Ok((false, rst)),
		_ => Err(Error {
			kind: ErrorKind::UnexpectedNonBoolean,
			location: Some(tok.location),
		}),
	}
}

fn expect_kind<'a, 'b>(toks: &'b [Token<'a>], kind: TokenKind) -> Result<'a, 'b, Token<'a>> {
	let (tok, rst) = expect_token(toks)?;
	if tok.kind != kind {
		Err(Error {
			kind: ErrorKind::UnexpectedToken {
				want: kind,
				got: tok.kind,
			},
			location: Some(tok.location),
		})
	} else {
		Ok((tok, rst))
	}
}

fn expect_token<'a, 'b>(toks: &'b [Token<'a>]) -> Result<'a, 'b, Token<'a>> {
	let (tok, rst) = toks.split_first().ok_or(Error {
		kind: ErrorKind::UnexpectedEOF,
		location: None,
	})?;
	Ok((*tok, rst))
}