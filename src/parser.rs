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
		conds: Box<[IfCond]>,
		thens: Box<[Ast]>,
		r#else: Box<Ast>,
	},
}

#[derive(Debug, Clone)]
pub enum IfCond {
	Expr(Box<Ast>),
	Let(LetList),
}

#[derive(Debug, Clone)]
pub enum Pattern {
	Ident(String),
	// TODO: Add table and literal patterns (and also maybe value patterns between parentheses? eg. let a = 1 let (a) = 2 in "Not reached because error")
}

#[derive(Debug, Clone)]
pub struct LetList {
	pats: Box<[Pattern]>,
	exprs: Box<[Ast]>,
}

pub fn parse(input: &str) -> ParseResult<Ast> {
	let mut t = Tokenizer::new(input);
	parse_expr(&mut t)
}

/// expr
///   = funcall
///   | letexpr
///   | ifexpr
///   | ident
///   | numlit
///   | strlit
fn parse_expr(t: &mut Tokenizer) -> ParseResult<Ast> {
	let checkpoint = t.clone();

	if let n@Ok(_) = parse_funcall(t) {
		return n
	}

	*t = checkpoint;
	if let n@Ok(_) = dbg!(parse_ifexpr(t)) {
		return n;
	}

	*t = checkpoint;
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

/// funcall = simple_expr simple_expr+
fn parse_funcall(t: &mut Tokenizer) -> ParseResult<Ast> {
	let fun = parse_simple_expr(t)?;

	let mut checkpoint = t.clone();
	let mut args = Vec::new();
	while let Ok(arg) = parse_simple_expr(t) {
		checkpoint = t.clone();
		args.push(arg);
	}
	*t = checkpoint;
	if args.len() < 1 {
		Err(Error {
			location: fun.location,
			description: "Expected 1 or more arguments to function call".to_string(),
		})
	} else {
		Ok(Ast {
			location: fun.location,
			node: Node::Funcall {
				fun: Box::new(fun),
				args: args.into_boxed_slice(),
			}
		})
	}
}

/// simple_expr
///   = ident
///   | numlit
///   | strlit
///   | LParen expr RParen
fn parse_simple_expr(t: &mut Tokenizer) -> ParseResult<Ast> {
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
	expect_token(t, TokenKind::LParen)?;
	let expr = parse_expr(t)?;
	expect_token(t, TokenKind::RParen)?;
	Ok(expr)
}

/// ifexpr = (If ifcond Then expr)+ Else expr
fn parse_ifexpr(t: &mut Tokenizer) -> ParseResult<Ast> {
	let location = t.location();
	let mut checkpoint = t.clone();
	let mut conds = Vec::new();
	let mut thens = Vec::new();

	while expect_token(t, TokenKind::If).is_ok() {
		let cond = parse_ifcond(t)?;
		expect_token(t, TokenKind::Then)?;
		let then = parse_expr(t)?;

		checkpoint = t.clone();
		conds.push(cond);
		thens.push(then);
	}
	*t = checkpoint;
	if conds.len() < 1 {
		return Err(Error {
			location,
			description: String::new(),
		})
	}

	expect_token(t, TokenKind::Else)?;
	let r#else = parse_expr(t)?;
	Ok(Ast {
		location,
		node: Node::IfExpr {
			conds: conds.into_boxed_slice(),
			thens: thens.into_boxed_slice(),
			r#else: Box::new(r#else),
		},
	})
}

/// ifcond
///   = letlist
///   | expr
fn parse_ifcond(t: &mut Tokenizer) -> ParseResult<IfCond> {
	let mut checkpoint = t.clone();

	if let Ok(lst) = parse_letlist(t) {
		checkpoint = t.clone();
		if expect_token(t, TokenKind::Then).is_ok() {
			*t = checkpoint; // Reset tokenizer because otherwise the Then token would be consumed
			return Ok(IfCond::Let(lst));
		}
	}

	*t = checkpoint;
	let expr = parse_expr(t)?;
	Ok(IfCond::Expr(Box::new(expr)))
}

/// letlist = (Let pattern Eq expr)+
fn parse_letlist(t: &mut Tokenizer) -> ParseResult<LetList> {
	let mut checkpoint = t.clone();
	let mut pats = Vec::new();
	let mut exprs = Vec::new();

	while expect_token(t, TokenKind::Let).is_ok() {
		let pat = parse_pattern(t)?;
		expect_token(t, TokenKind::Eq)?;
		let expr = parse_expr(t)?;

		checkpoint = t.clone();
		pats.push(pat);
		exprs.push(expr);
	}
	*t = checkpoint;

	if pats.len() < 1 {
		return Err(Error {
			location: t.location(),
			description: String::new(),
		})
	}

	Ok(LetList {
		pats: pats.into_boxed_slice(),
		exprs: exprs.into_boxed_slice(),
	})
}

/// pattern = ident
fn parse_pattern(t: &mut Tokenizer) -> ParseResult<Pattern> {
	let ident = expect_token(t, TokenKind::Ident)?;
	Ok(Pattern::Ident(ident.text.to_string()))
}

fn parse_letexpr_from_letlist(t: &mut Tokenizer, _lst: LetList) -> ParseResult<Ast> {
	return Err(Error {
		location: t.location(),
		description: "Unimplemented!".to_string(),
	})
}

/// ident = Ident
fn parse_ident(t: &mut Tokenizer) -> ParseResult<Ast> {
	let tok = expect_token(t, TokenKind::Ident)?;
	Ok(Ast {
		location: tok.location,
		node: Node::Ident(tok.text.to_string()),
	})
}

/// numlit = NumLit
fn parse_numlit(t: &mut Tokenizer) -> ParseResult<Ast> {
	let tok = expect_token(t, TokenKind::NumLit)?;
	if let Ok(n) = tok.text.parse::<f32>() {
		Ok(Ast {
			location: tok.location,
			node: Node::NumLit(n),
		})
	} else {
		panic!("Unreachable: Tokenizer should only accept valid number literal values");
	}
}

/// strlit = StrLit
fn parse_strlit(t: &mut Tokenizer) -> ParseResult<Ast> {
	let tok = expect_token(t, TokenKind::StrLit)?;
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

fn expect_token<'a>(t: &mut Tokenizer<'a>, kind: TokenKind) -> ParseResult<Token<'a>> {
	let tok = next_token(t)?;
	if tok.kind != kind {
		return Err(Error {
			location: tok.location,
			description: format!("Expected token {:?}, got {:?}", kind, tok.kind),
		})
	}
	Ok(tok)
}