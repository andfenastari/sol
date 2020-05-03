use std::io::BufRead;
use tokenizer::Tokenizer;
use parser::parse;

fn main() {
	//tokenizer_repl();
	parser_repl();
}

fn parser_repl() {
	let stdin = std::io::stdin();

	for line in stdin.lock().lines() {
		let l = line.unwrap();
		let ast = parse(&l);
		println!("{:#?}", ast);
	}
}

fn tokenizer_repl() {
	let stdin = std::io::stdin();
	
	for line in stdin.lock().lines() {
		let l = line.unwrap();
		let t = Tokenizer::new(&l);
		for token in t {
			println!("\t{:?}", token);
		}
	}
}

mod tokenizer;
mod parser;