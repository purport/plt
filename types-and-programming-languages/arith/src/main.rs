use std::{iter::Peekable, ptr};

fn main() {
  let mut trie = TokenTrie::default();
  trie.insert("else", Token::Else);
  trie.insert("end", Token::End);
  trie.insert("false", Token::False);
  trie.insert("if", Token::If);
  trie.insert("iszero", Token::Iszero);
  trie.insert("pred", Token::Pred);
  trie.insert("succ", Token::Succ);
  trie.insert("then", Token::Then);
  trie.insert("true", Token::True);

  print_expr(&trie, "pred succ 0");
  print_expr(&trie, "iszero 0");
  print_expr(&trie, "iszero succ 0");
  print_expr(&trie, "iszero pred succ 0");
  print_expr(&trie, "iszero pred succ succ 0");
  print_expr(&trie, "iszero succ pred succ 0");
  print_expr(&trie, "pred succ succ succ 0");
  print_expr(&trie, "succ pred succ succ 0");
  print_expr(&trie, "succ succ pred succ 0");
  print_expr(&trie, "succ succ succ pred 0");
  print_expr(&trie, "succ succ succ 0");
  print_expr(
    &trie,
    "if iszero pred succ 0 then pred succ succ 0 else true",
  );
  print_expr(
    &trie,
    "if iszero succ 0 then false else if iszero pred pred succ succ 0 then iszero 0 else false",
  );

  print_expr(&trie, "if succ 0 then true else false");
  print_expr(&trie, "if 0 then true else false");
  print_expr(&trie, "succ true");
  print_expr(&trie, "pred succ true");
  print_expr(&trie, "succ pred false");
  print_expr(&trie, "succ pred iszero false");
  print_expr(&trie, "succ iszero false");
  print_expr(&trie, "iszero false");
  print_expr(&trie, "if true then succ true else false");
  print_expr(&trie, "if false then succ true else false");
}

fn print_expr(trie: &TokenTrie, input: &str) {
  let ast = parse(scan(&trie.root, input));
  println!("{} = {:?}", input, eval(ast))
}

///////////////////////////////////////////////////////////////////////////
/// Evaluation
///

fn eval(ast: Term) -> Term {
  let mut node = ast;
  while !is_normal(&node) {
    node = eval1(node);
  }
  node
}

fn eval1(ast: Term) -> Term {
  match ast {
    Term::Succ(ref subterm) if is_bad_nat(subterm) => {
      Term::Wrong(subterm.clone())
    }
    Term::Succ(subterm) => Term::Succ(Box::new(eval1(*subterm))),
    Term::Pred(subterm) => match *subterm {
      Term::Zero => Term::Zero,
      Term::Succ(ref nv1) if is_numeric(nv1) => *nv1.clone(),
      ref t if is_bad_nat(t) => Term::Wrong(Box::new(t.clone())),
      _ => Term::Pred(Box::new(eval1(*subterm))),
    },
    Term::Iszero(subterm) => match *subterm {
      Term::Zero => Term::True,
      Term::Succ(ref num) if is_numeric(num) => Term::False,
      ref t if is_bad_nat(t) => Term::Wrong(Box::new(t.clone())),
      _ => Term::Iszero(Box::new(eval1(*subterm))),
    },
    Term::If(cond, b1, b2) => match *cond {
      Term::True => *b1,
      Term::False => *b2,
      ref term if is_bad_bool(term) => Term::Wrong(Box::new(term.clone())),
      _ => Term::If(Box::new(eval1(*cond)), b1, b2),
    },
    _ => todo!(),
  }
}

fn is_numeric(num: &Term) -> bool {
  match num {
    Term::Zero => true,
    Term::Succ(t) => is_numeric(&t),
    _ => false,
  }
}

fn is_normal(node: &Term) -> bool {
  match node {
    Term::Wrong(_) => true,
    Term::Zero => true,
    Term::True => true,
    Term::False => true,
    Term::Succ(num) => is_numeric(num),
    _ => false,
  }
}

fn is_bad_bool(node: &Term) -> bool {
  match node {
    Term::Wrong(_) => true,
    _ => is_numeric(&node),
  }
}

fn is_bad_nat(node: &Term) -> bool {
  match node {
    Term::Wrong(_) => true,
    Term::True => true,
    Term::False => true,
    _ => false,
  }
}

///////////////////////////////////////////////////////////////////////////
/// Parser
///

#[derive(Debug, Clone)]
enum Term {
  Wrong(Box<Term>),
  Zero,
  True,
  False,
  Iszero(Box<Term>),
  Succ(Box<Term>),
  Pred(Box<Term>),
  If(Box<Term>, Box<Term>, Box<Term>),
}

fn parse(tokens: Vec<Token>) -> Term {
  parse_term(&mut tokens.iter().peekable())
}

fn parse_term(iter: &mut Peekable<std::slice::Iter<Token>>) -> Term {
  match iter.next() {
    Some(token) => match token {
      Token::True => Term::True,
      Token::False => Term::False,
      Token::Zero => Term::Zero,
      Token::Succ => Term::Succ(Box::new(parse_term(iter))),
      Token::Pred => Term::Pred(Box::new(parse_term(iter))),
      Token::Iszero => Term::Iszero(Box::new(parse_term(iter))),
      Token::If => parse_if(iter),
      Token::Then => unreachable!(),
      Token::Else => unreachable!(),
      Token::End => unreachable!(),
    },
    None => todo!(),
  }
}

fn parse_if(iter: &mut Peekable<std::slice::Iter<Token>>) -> Term {
  let condition = parse_term(iter);

  if let Some(Token::Then) = iter.next() {
    let branch1 = parse_term(iter);
    if let Some(Token::Else) = iter.next() {
      let branch2 = parse_term(iter);
      let result = Term::If(
        Box::new(condition),
        Box::new(branch1),
        Box::new(branch2),
      );
      if let Some(Token::End) = iter.peek() {
        iter.next();
      }
      result
    } else {
      todo!()
    }
  } else {
    todo!()
  }
}
///////////////////////////////////////////////////////////////////////////
/// Scanner
///

#[derive(Debug, Clone)]
enum Token {
  True,
  False,
  Zero,
  If,
  Then,
  Else,
  End,
  Succ,
  Pred,
  Iszero,
}

#[derive(Default)]
struct TokenTrieNode {
  children: [Option<Box<TokenTrieNode>>; 26],
  token: Option<Token>,
}

#[derive(Default)]
struct TokenTrie {
  root: TokenTrieNode,
}

impl TokenTrie {
  fn insert(&mut self, keyword: &str, tok: Token) {
    let mut current = &mut self.root;
    for k in keyword.chars() {
      let i = (k as usize) - ('a' as usize);
      current = current.children[i]
        .get_or_insert_with(|| Box::new(TokenTrieNode::default()));
    }
    current.token = Some(tok);
  }
}

fn scan(root: &TokenTrieNode, input: &str) -> Vec<Token> {
  let mut tokens = Vec::new();
  let mut current = root;
  for ch in input.chars() {
    match ch {
      'a'..='z' => {
        let i = (ch as usize) - ('a' as usize);
        current = current.children[i].as_ref().unwrap()
      }
      '0' => {
        if !ptr::eq(current, root) {
          let i = (ch as usize).saturating_sub('a' as usize);
          current = current.children[i].as_ref().unwrap()
        } else {
          tokens.push(Token::Zero)
        }
      }
      ' ' => {
        if !ptr::eq(current, root) {
          tokens.push(current.token.clone().unwrap());
          current = &root;
        }
      }
      _ => (),
    }
  }
  if current.token.is_some() {
    tokens.push(current.token.clone().unwrap());
  }
  tokens
}
