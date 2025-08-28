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

	// fn parse_regex(string: &str) -> Regex<char> {

	// }
}