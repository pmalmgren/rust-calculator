# Three calculator parsers in Rust

This is a Rust implementation of a calculator which can do addition, subtraction, division, multiplication, and exponentials.

I have written three different top-down parsers for the calculator's grammar using these algorithms:

- Shunting yard
- Recursive descent
- Precedence climbing

I used the excellent [Parsing Expressions by Recursive Descent](https://www.engr.mun.ca/~theo/Misc/exp_parsing.htm) article as a guide. I recommend reading it along with my [blog post](https://petermalmgren.com/three-rust-parsers/) for certain implementation-specific details.

## Grammar

I used the following left-recursive grammar for this calculator:

```
S -> E0 end
E0 -> E1 | E0 "+" E1
E1 -> E2 | E1 "-" E2
E2 -> E3 | E2 "*" E3
E3 -> E4 | E3 "/" E4
E5 -> E5 | E5 "^" E6
E6 -> P
P -> "-" E0
   | "(" E0 ")"
   | v
v -> [0-9]+
```

We can get a non left recursive grammar suitable for use in a recursive descent parser by applying a transformation rule which says that any form `A -> a | A b` can be rewritten `A -> a {b}` where `{b}` means one or more `b`. Applying this rule gets rid of left recursion and makes a grammar suitable for recursive descent. For example `E0` can be transformed into `E0 -> E1 {"+" E1}`.

If you're curious about this, I highly recommend reading the [deriving precedence climbing](https://www.engr.mun.ca/~theo/Misc/exp_parsing.htm#more_climbing) section of the article I linked above. I recommend using a pen and paper to follow along.

## Evaluating

Given the following data structure for an expression:

```rust
struct Operator {
	Addition,
	Subtraction,
	Multiplication,
	Division,
	Exponent,
	Negative
}

struct Expression {
	Binary(Operator, Box<Expression>, Box<Expression>),
	Unary(Operator, Box<Expression>),
	Number(i64)
}
```

I wrote an extremely naive recursive function which parses an expression. This will probably overflow the stack for extremely large and/or nested expressions, but it's easy to understand :)

Note: This doesn't take into acccount overflows, so doing something like `2 ^ 9032` will overflow an `i64` and cause a panic.

```rust
impl Expression {
	fn eval(self) -> i64 {
		match self {
			Number(n) => *n,
			Binary(Operator::Addition, expr1, expr2) => expr1.eval() + expr2.eval(),
			Binary(Operator::Subtraction, expr1, expr2) => expr1.eval() - expr2.eval(),
			Binary(Operator::Multiplication, expr1, expr2) => expr1.eval() * expr2.eval(),
			Binary(Operator::Division, expr1, expr2) => expr1.eval() / expr2.eval(),
			Binary(Operator::Exponent, expr1, expr2) => expr1.eval().pow(expr2.eval() as u32),
			Unary(Operator::Negative, expr) => -1 * expr.eval(),
			_ => panic!("invalid")
		}
	}
}
```
