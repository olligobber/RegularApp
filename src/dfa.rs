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
	State: Eq + Hash + Clone,
	Char: Eq + Hash + Clone,
{
	// Transition from one state to the next given a character
	fn transition(&self, state: &State, char: &Char) -> &State {
		self
			.transitions
			.get(&(state.clone(), char.clone()))
			.expect("Invalid DFA")
	}

	// Check the way a DFA is stored is valid
	pub fn validate(&self) -> bool {
		if ! self.states.contains(&self.start_state) { return false }
		for state in &self.states {
			for char in &self.alphabet {
				match self.transitions.get(&(state.clone(), char.clone())) {
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

	// Check if a DFA accepts a string
	pub fn parse_string(&self, string: Box<dyn Iterator<Item=Char>>) {
		let mut state: &State = &self.start_state;
		for char in string {
			state = self.transition(state, &char);
		}
	}

	// Find all states that can be reached from the start state by any string
	fn reachable_states(&self) -> HashSet<State> {
		let mut result: HashSet<State> = HashSet::new();
		let mut to_visit: VecDeque<&State> = VecDeque::new();
		to_visit.push_front(&self.start_state);
		loop {
			match to_visit.pop_front() {
				None => { return result },
				Some(state) => {
					if result.contains(state) { continue }
					result.insert(state.clone());
					for char in &self.alphabet {
						to_visit.push_front(self.transition(state, char));
					}
				}
			}
		}
	}

	// Check if a DFA recognises no strings
	pub fn is_empty(&self) -> bool {
		self
			.reachable_states()
			.iter()
			.all(|state| ! self.accepting.contains(state))
	}

	// Check if a DFA recognises all strings
	pub fn is_complete(&self) -> bool {
		self
			.reachable_states()
			.iter()
			.all(|state| self.accepting.contains(state))
	}

	// Get a DFA that recognises all strings that this one does not recognise
	pub fn complement(&self) -> Dfa<State, Char> {
		let mut non_accepting : HashSet<State> = HashSet::new();
		for state in self.states.difference(&self.accepting) {
			non_accepting.insert(state.clone());
		}
		Dfa
			{ states: self.states.clone()
			, alphabet: self.alphabet.clone()
			, start_state: self.start_state.clone()
			, transitions: self.transitions.clone()
			, accepting: non_accepting
			}
	}

	// Check if this DFA recognises the same language as another
	pub fn equivalent<State2>(&self, other: &Dfa<State2, Char>) -> bool
	where
		State2: Eq + Hash + Clone,
	{
		Dfa::symmetric_difference(self, other).is_empty()
	}

	pub fn relabel_states(&self) -> Dfa<u64, Char> {
		let mut map_to_new : HashMap<State, u64> = HashMap::new();
		// let mut map_to_old : HashMap<u64, State> = HashMap::new();
		let mut states : HashSet<u64> = HashSet::new();
		for (i, state) in (0_u64..).zip(self.reachable_states().into_iter()) {
			map_to_new.insert(state.clone(), i);
			// map_to_old.insert(i, state);
			states.insert(i);
		}
		let mut transitions: HashMap<(u64, Char), u64> = HashMap::new();
		for ((input, char), output) in &self.transitions {
			match map_to_new.get(input) {
				None => {}
				Some(new_input) => {
					transitions.insert(
						(*new_input, char.clone()),
						*map_to_new.get(output).expect("Transition from reachable to unreachable state")
					);
				}
			}
		}
		let mut accepting: HashSet<u64> = HashSet::new();
		for state in &self.accepting {
			match map_to_new.get(state) {
				None => {}
				Some(new_state) => {
					accepting.insert(*new_state);
				}
			}
		}
		Dfa
			{ states
			, alphabet: self.alphabet.clone()
			, start_state: *map_to_new.get(&self.start_state).expect("Start state is not reachable")
			, transitions
			, accepting
		}
	}
}

impl<Char> Dfa<(), Char>{
	// Construct a DFA that recognises no strings
	pub fn empty(alphabet: HashSet<Char>) -> Dfa<(), Char>
	where
		Char: Eq + Hash + Clone,
	{
		let mut states : HashSet<()> = HashSet::new();
		states.insert(());
		let mut transitions : HashMap<((), Char), ()> = HashMap::new();
		for char in &alphabet {
			transitions.insert(((), char.clone()), ());
		}
		Dfa
			{ states
			, alphabet
			, start_state: ()
			, transitions
			, accepting: HashSet::new()
			}
	}

	// Construct a DFA that recognises every string
	pub fn complete(alphabet: HashSet<Char>) -> Dfa<(), Char>
	where
		Char: Eq + Hash + Clone,
	{
		let mut states : HashSet<()> = HashSet::new();
		states.insert(());
		let mut transitions : HashMap<((), Char), ()> = HashMap::new();
		for char in &alphabet {
			transitions.insert(((), char.clone()), ());
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

impl<State1, State2, Char> Dfa<(State1, State2), Char>
where
	State1: Eq + Hash + Clone,
	State2: Eq + Hash + Clone,
	Char: Eq + Hash + Clone,
{
	// Creates a DFA that simulates two other DFAs and accepts a string using
	// a function and whether the two DFAs accept
	// product(a, b, f) accepts a string if f(a accepts, b accepts)
	pub fn product
		(first: &Dfa<State1, Char>, second: &Dfa<State2, Char>, func: fn(bool, bool) -> bool)
		-> Dfa<(State1, State2), Char> {
		assert!(first.alphabet == second.alphabet, "Cannot product DFA with different alphabets");
		let mut new_states : HashSet<(State1, State2)> = HashSet::new();
		for state1 in &first.states {
			for state2 in &second.states {
				new_states.insert((state1.clone(), state2.clone()));
			}
		}
		let mut new_transitions : HashMap<((State1, State2), Char), (State1, State2)> = HashMap::new();
		for (state1, state2) in &new_states {
			for char in &first.alphabet {
				new_transitions.insert(
					((state1.clone(), state2.clone()), char.clone()),
					( first.transition(state1, char).clone()
					, second.transition(state2, char).clone()
					)
				);
			}
		}
		let mut new_accepting : HashSet<(State1, State2)> = HashSet::new();
		for (state, state2) in &new_states {
			if func(first.accepting.contains(state), second.accepting.contains(state2)) {
				new_accepting.insert((state.clone(), state2.clone()));
			}
		}
		Dfa
			{ states: new_states
			, alphabet: first.alphabet.clone()
			, start_state: (first.start_state.clone(), second.start_state.clone())
			, transitions: new_transitions
			, accepting: new_accepting
			}
	}

	// Construct a DFA that accepts strings that at least one of two DFAs accepts
	pub fn union
		(first: &Dfa<State1, Char>, second: &Dfa<State2, Char>) -> Dfa<(State1, State2), Char>
	{
		Dfa::product(first, second, |a, b| a || b)
	}

	// Construct a DFA that accepts strings that both of two DFAs accepts
	pub fn intersection
		(first: &Dfa<State1, Char>, second: &Dfa<State2, Char>) -> Dfa<(State1, State2), Char>
	{
		Dfa::product(first, second, |a, b| a && b)
	}

	// Construct a DFA that accepts strings that the first DFA accepts and the second doesn't
	pub fn difference
		(first: &Dfa<State1, Char>, second: &Dfa<State2, Char>) -> Dfa<(State1, State2), Char>
	{
		Dfa::product(first, second, |a, b| a & !b)
	}

	// Construct a DFA that accepts strings that exactly one of two DFAs accepts
	pub fn symmetric_difference
		(first: &Dfa<State1, Char>, second: &Dfa<State2, Char>) -> Dfa<(State1, State2), Char>
	{
		Dfa::product(first, second, |a, b| a != b)
	}
}