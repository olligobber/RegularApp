use nom::{
	Parser,
	Err,
	error::{
		ParseError,
		Error
	},
	IResult,
	character::complete::{
		char,
		satisfy,
		multispace0,
	},
	combinator::{
		value,
		map,
		all_consuming,
	},
	branch::alt,
	multi::{
		many0,
		many1,
		separated_list1,
	},
	sequence::{
		delimited,
		pair,
	},
};
use std::char;

#[derive(Clone)]
pub enum Regex<Char> {
	Empty,
	Epsilon,
	Character(Char),
	Concat(Box<Regex<Char>>, Box<Regex<Char>>),
	Union(Box<Regex<Char>>, Box<Regex<Char>>),
	Star(Box<Regex<Char>>),
}

use Regex::*;

impl<Char> Regex<Char>
where
	Char: Eq,
{
	pub fn parse_string(&self, string: &[Char]) -> bool {
		match self {
			Empty => { false },
			Epsilon => { string.is_empty() },
			Character(char) => {
				string.iter().all(|c| *c == *char) &&
				string.len() == 1
			},
			Concat(left, right) => {
				for i in 0..string.len()+1 {
					if
						left.parse_string(&string[..i]) &&
						right.parse_string(&string[i..])
						{ return true }
				}
				false
			},
			Union(left, right) => {
				left.parse_string(string) || right.parse_string(string)
			},
			Star(contents) => {
				for i in 0..string.len()+1 {
					if
						contents.parse_string(&string[..i]) &&
						self.parse_string(&string[i..])
						{ return true }
				}
				false
			}

		}
	}
}

impl Regex<char> {
	pub fn parse_regex(string: &str) -> Result<Regex<char>, Err<Error<&str>>> {
		fn empty_parser<'a, E: ParseError<&'a str>>()
		-> impl Parser<&'a str, Output=Regex<char>, Error=E>
		{
			value(Empty, char('∅'))
		}

		fn epsilon_parser<'a, E: ParseError<&'a str>>()
		-> impl Parser<&'a str, Output=Regex<char>, Error=E>
		{
			value(Epsilon, char('ε'))
		}

		fn character_parser<'a, E: ParseError<&'a str>>()
		-> impl Parser<&'a str, Output=Regex<char>, Error=E>
		{
			map(satisfy(|c| c.is_ascii_alphanumeric()), Character)
		}

		fn simple_parser<'a, E: ParseError<&'a str>>()
		-> impl Parser<&'a str, Output=Regex<char>, Error=E>
		{
			alt((empty_parser(), epsilon_parser(), character_parser()))
		}

		fn ws<'a, O, E: ParseError<&'a str>, F>(
			inner: F,
			) -> impl Parser<&'a str, Output = O, Error = E>
		where
			F: Parser<&'a str, Output = O, Error = E>,
		{
			delimited(multispace0, inner, multispace0)
		}

		pub fn fold1<T>(
			vec: Vec<T>, accum: fn(T, T) -> T
			) -> T
		{
			vec
				.into_iter()
				.map(Some)
				.fold(
					None,
					|x, y| {
						match (x, y) {
							(None, None) => { None }
							(None, Some(b)) => { Some(b) }
							(Some(a), None) => { Some(a) }
							(Some(a), Some(b)) => { Some(accum(a, b)) }
						}
					}
				)
				.expect("fold1 on empty vec")
		}

		fn concat_parser(string: &str) -> IResult<&str, Regex<char>>
		{
			map(
				many1(ws(star_parser)),
				|v| fold1(
					v,
					|x, y| Concat(Box::new(x), Box::new(y.clone()))
				)
			).parse(string)
		}

		fn star_parser(string: &str) -> IResult<&str, Regex<char>>
		{
			map(
				pair(alt((simple_parser(), bracketed_parser)), many0(ws(char('*')))),
				|(regex, stars)| stars.iter().rfold(
					regex,
					|x, _| Star(Box::new(x))
				)
			).parse(string)
		}

		fn union_parser(string: &str) -> IResult<&str, Regex<char>>
		{
			map(
				separated_list1(ws(char('|')), concat_parser),
				|v| fold1(
					v,
					|x, y| Union(Box::new(x), Box::new(y.clone()))
				)
			).parse(string)
		}

		fn bracket<'a, O, E: ParseError<&'a str>, F>(
			inner: F,
			) -> impl Parser<&'a str, Output = O, Error = E>
		where
			F: Parser<&'a str, Output = O, Error = E>,
		{
			// alt((
			// 	delimited(char('('), inner, char(')')),
			// 	delimited(char('['), inner, char(']')),
			// 	delimited(char('{'), inner, char('}')),
			// ))
			delimited(char('('), inner, char(')'))
		}

		fn bracketed_parser(string: &str) -> IResult<&str, Regex<char>>
		{
			bracket(ws(union_parser)).parse(string)
		}

		let (_, r) = all_consuming(union_parser).parse(string)?;
		Ok(r)
	}
}