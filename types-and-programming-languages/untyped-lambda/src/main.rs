use std::{iter::Peekable, slice::Iter};

fn main() {
  print_expr("(λ.1) ((λ.1) 0)");
}

fn print_expr(input: &str) {
  let ast = parse(scan(input));
  println!("{} = {:?}", input, eval(ast));
}

fn eval(ast: Term) -> Term {
  println!("{:?}", ast);
  todo!()
}

#[derive(Debug, Clone)]
enum Term {
  Variable(u64),
  Abstraction(Box<Term>),
  Application(Box<Term>, Box<Term>),
}

fn parse(tokens: Vec<Token>) -> Term {
  println!("{:?}", tokens);
  let mut iter = tokens.iter().peekable();
  parse_term(&mut iter)
}

fn parse_term(iter: &mut Peekable<Iter<Token>>) -> Term {
  let mut lhs = parse_atom(iter);
  while let Some(token) = iter.next() {
    match token {
      Token::OpenParen => {
        let rhs = parse_term(iter);
        lhs = Term::Application(Box::new(lhs), Box::new(rhs));
        iter.next();
      }
      _ => break,
    }
  }
  lhs
}

fn parse_atom(iter: &mut Peekable<Iter<Token>>) -> Term {
  match iter.next() {
    Some(Token::Lambda) => match iter.next() {
      Some(Token::Dot) => Term::Abstraction(Box::new(parse_term(iter))),
      _ => panic!(),
    },
    Some(Token::Var(i)) => Term::Variable(*i),
    Some(Token::OpenParen) => {
      let expr = parse_term(iter);
      iter.next();
      expr
    }
    _ => panic!(),
  }
}

#[derive(Debug, Clone)]
enum Token {
  Lambda,
  Dot,
  Var(u64),
  OpenParen,
  CloseParen,
}

fn scan(input: &str) -> Vec<Token> {
  let mut tokens = Vec::new();
  for ch in input.chars() {
    tokens.push(match ch {
      'λ' => Token::Lambda,
      '.' => Token::Dot,
      '(' => Token::OpenParen,
      ')' => Token::CloseParen,
      '0'..='9' => Token::Var(ch.to_digit(10).unwrap() as u64),
      ' ' => continue,
      _ => {
        panic!("unexpected {ch}");
      }
    })
  }
  tokens
}
