use std::collections::{HashMap, HashSet, VecDeque};
use std::hash::Hash;

pub struct Dfa<State, Char> {
	pub states: HashSet<State>,
	pub alphabet: HashSet<Char>,
	pub start_state: State,
	pub transitions: HashMap<(State, Char), State>,
	pub accepting: HashSet<State>,
}

impl<State, Char> Dfa<State, Char>
where
	State: Eq + Hash + Copy,
	Char: Eq + Hash + Copy,
{
	fn transition(&self, state: &State, char: &Char) -> &State {
		self
			.transitions
			.get(&(*state, *char))
			.expect("Invalid DFA")
	}

	pub fn validate(&self) -> bool {
		if ! self.states.contains(&self.start_state) { return false }
		for state in &self.states {
			for char in &self.alphabet {
				match self.transitions.get(&(*state, *char)) {
					None => { return false }
					Some(result) => {
						if ! self.states.contains(result) { return false }
					}
				}
			}
		}
		for state in &self.accepting {
			if !self.states.contains(state) { return false }
		}
		true
	}

	pub fn parse_string(&self, string: Box<dyn Iterator<Item=Char>>) {
		let mut state: &State = &self.start_state;
		for char in string {
			state = self.transition(state, &char);
		}
	}

	fn reachable_states(&self) -> HashSet<State> {
		let mut result: HashSet<State> = HashSet::new();
		let mut to_visit: VecDeque<State> = VecDeque::new();
		to_visit.push_front(self.start_state);
		loop {
			match to_visit.pop_front() {
				None => { return result },
				Some(state) => {
					if result.contains(&state) { continue }
					result.insert(state);
					for char in &self.alphabet {
						to_visit.push_front(*self.transition(&state, char))
					}
				}
			}
		}
	}

	pub fn is_empty(&self) -> bool {
		self
			.reachable_states()
			.iter()
			.all(|state| ! self.accepting.contains(state))
	}

	pub fn is_complete(&self) -> bool {
		self
			.reachable_states()
			.iter()
			.all(|state| self.accepting.contains(state))
	}

	pub fn complement(&self) -> Dfa<State, Char> {
		let mut non_accepting : HashSet<State> = HashSet::new();
		for state in self.states.difference(&self.accepting) {
			non_accepting.insert(*state);
		}
		Dfa
			{ states: self.states.clone()
			, alphabet: self.alphabet.clone()
			, start_state: self.start_state
			, transitions: self.transitions.clone()
			, accepting: non_accepting
			}
	}

	pub fn product<State2>(&self, other: &Dfa<State2, Char>, func: fn(bool, bool) -> bool) -> Dfa<(State, State2), Char>
	where
		State2: Eq + Hash + Copy,
	{
		if self
			.alphabet
			.symmetric_difference(&other.alphabet)
			.any(|_| true)
		{
			panic!("Cannot product DFA with different alphabets")
		}
		let mut new_states : HashSet<(State, State2)> = HashSet::new();
		for state1 in &self.states {
			for state2 in &other.states {
				new_states.insert((*state1, *state2));
			}
		}
		let mut new_transitions : HashMap<((State, State2), Char), (State, State2)> = HashMap::new();
		for (state1, state2) in &new_states {
			for char in &self.alphabet {
				new_transitions.insert(
					((*state1, *state2), *char),
					(*self.transition(state1, char), *other.transition(state2, char))
				);
			}
		}
		let mut new_accepting : HashSet<(State, State2)> = HashSet::new();
		for (state, state2) in &new_states {
			if func(self.accepting.contains(state), other.accepting.contains(state2)) {
				new_accepting.insert((*state, *state2));
			}
		}
		Dfa
			{ states: new_states
			, alphabet: self.alphabet.clone()
			, start_state: (self.start_state, other.start_state)
			, transitions: new_transitions
			, accepting: new_accepting
			}
	}

	pub fn union<State2>(&self, other: &Dfa<State2, Char>) -> Dfa<(State, State2), Char>
	where
		State2: Eq + Hash + Copy,
	{
		self.product(other, |a, b| a || b)
	}

	pub fn intersection<State2>(&self, other: &Dfa<State2, Char>) -> Dfa<(State, State2), Char>
	where
		State2: Eq + Hash + Copy,
	{
		self.product(other, |a, b| a && b)
	}

	pub fn difference<State2>(&self, other: &Dfa<State2, Char>) -> Dfa<(State, State2), Char>
	where
		State2: Eq + Hash + Copy,
	{
		self.product(other, |a, b| a & !b)
	}

	pub fn symmetric_difference<State2>(&self, other: &Dfa<State2, Char>) -> Dfa<(State, State2), Char>
	where
		State2: Eq + Hash + Copy,
	{
		self.product(other, |a, b| a != b)
	}

	pub fn equivalent<State2>(&self, other: &Dfa<State2, Char>) -> bool
	where
		State2: Eq + Hash + Copy,
	{
		self.symmetric_difference(other).is_empty()
	}

	pub fn empty(alphabet: HashSet<Char>) -> Dfa<(), Char> {
		let mut states : HashSet<()> = HashSet::new();
		states.insert(());
		let mut transitions : HashMap<((), Char), ()> = HashMap::new();
		for char in &alphabet {
			transitions.insert(((), *char), ());
		}
		Dfa
			{ states
			, alphabet
			, start_state: ()
			, transitions
			, accepting: HashSet::new()
			}
	}

	pub fn complete(alphabet: HashSet<Char>) -> Dfa<(), Char> {
		let mut states : HashSet<()> = HashSet::new();
		states.insert(());
		let mut transitions : HashMap<((), Char), ()> = HashMap::new();
		for char in &alphabet {
			transitions.insert(((), *char), ());
		}
		Dfa
			{ states: states.clone()
			, alphabet
			, start_state: ()
			, transitions
			, accepting: states
			}
	}

}