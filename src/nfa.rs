use std::collections::btree_set::Union;
use std::collections::{HashMap, HashSet, VecDeque};
use std::hash::Hash;
use map_macro::hash_map;

pub struct Nfa<State, Char> {
	states: HashSet<State>,
	alphabet: HashSet<Char>,
	start_state: State,
	transitions: HashMap<(State, Char), HashSet<State>>,
	epsilon_transitions: HashMap<State, HashSet<State>>,
	accepting: HashSet<State>,
}

#[derive(PartialEq, Eq, Hash, Clone, Copy)]
pub enum ConcatState<State1, State2> {
	Left(State1),
	Right(State2),
}

#[derive(PartialEq, Eq, Hash, Clone, Copy)]
pub enum UnionState<State1, State2> {
	Start,
	First(State1),
	Second(State2),
}

#[derive(PartialEq, Eq, Hash, Clone, Copy)]
pub enum StarState<State> {
	Start,
	Old(State),
}

impl<State, Char> Nfa<State, Char>
where
	State: Eq + Hash + Clone + 'static,
	Char: Eq + Hash + Clone,
{
	// Follow all transitions labelled by a character
	fn transition(&self, state: &State, char: &Char) -> Option<&HashSet<State>> {
		self
			.transitions
			.get(&(state.clone(), char.clone()))
	}

	// Check the NFA representation is valid
	pub fn validate(&self) -> bool {
		if !self.states.contains(&self.start_state) { return false }
		for state in &self.states {
			for char in &self.alphabet {
				match self.transition(state, char) {
					None => {}
					Some(result) => {
						for out_state in result {
							if !self.states.contains(out_state) { return false }
						}
					}
				}
			}
		}
		for state in &self.states {
			match self.epsilon_transitions.get(state) {
				None => {}
				Some(result) => {
					for out_state in result {
						if !self.states.contains(out_state) { return false }
					}
				}
			}
		}
		self
			.accepting
			.iter()
			.all(|state| self.states.contains(state))
	}

	// Find all states that can be reached using any number of epsilon transitions
	fn epsilon_closure(&self, start: Box<dyn Iterator<Item=State>>) -> HashSet<State> {
		let mut result : HashSet<State> = HashSet::new();
		let mut to_visit : VecDeque<State> = VecDeque::new();
		for state in start {
			to_visit.push_front(state);
		}
		loop {
			match to_visit.pop_front() {
				None => { return result }
				Some(state) => {
					if result.contains(&state) { continue }
					result.insert(state.clone());
					match self.epsilon_transitions.get(&state) {
						None => {}
						Some(neighbours) => {
							for neighbour in neighbours {
								to_visit.push_front(neighbour.clone());
							}
						}
					}
				}
			}
		}
	}

	// Find all states that can be reached by any number of transitions from the start state
	fn reachable_states(&self) -> HashSet<State> {
		let mut result : HashSet<State> = HashSet::new();
		let mut to_visit : VecDeque<State> = VecDeque::new();
		to_visit.push_front(self.start_state.clone());
		loop {
			match to_visit.pop_front() {
				None => { return result }
				Some(state) => {
					if result.contains(&state) { continue }
					result.insert(state.clone());
					for char in &self.alphabet {
						match self.transition(&state.clone(), char) {
							None => {}
							Some(neighbours) => {
								for neighbour in neighbours {
									to_visit.push_front(neighbour.clone());
								}
							}
						}
					}
					match self.epsilon_transitions.get(&state) {
						None => {}
						Some(neighbours) => {
							for neighbour in neighbours {
								to_visit.push_front(neighbour.clone());
							}
						}
					}
				}
			}
		}
	}

	// Parse a string using an NFA
	pub fn parse_string(&self, string: Box<dyn Iterator<Item=Char>>) -> bool {
		let mut states : HashSet<State> =
			self.epsilon_closure(Box::new(vec![self.start_state.clone()].into_iter()));
		for char in string {
			let mut immediate_neighbours : HashSet<State> = HashSet::new();
			for state in &states {
				match self.transition(state, &char) {
					None => {}
					Some(neighbours) => {
						for neighbour in neighbours {
							immediate_neighbours.insert(neighbour.clone());
						}
					}
				}
			}
			states.clear();
			for state in self.epsilon_closure(Box::new(immediate_neighbours.into_iter())) {
				states.insert(state);
			}
		}
		for state in states {
			if self.accepting.contains(&state) { return true }
		}
		false
	}

	// NFA which accepts nothing
	pub fn empty(alphabet: HashSet<Char>) -> Nfa<(), Char> {
		Nfa
			{ states : HashSet::from([()])
			, alphabet
			, start_state: ()
			, transitions: HashMap::new()
			, epsilon_transitions: HashMap::new()
			, accepting: HashSet::new()
			}
	}

	// NFA which accepts the empty string only
	pub fn epsilon(alphabet: HashSet<Char>) -> Nfa<(), Char> {
		Nfa
			{ states: HashSet::from([()])
			, alphabet
			, start_state: ()
			, transitions: HashMap::new()
			, epsilon_transitions: HashMap::new()
			, accepting: HashSet::from([()])
			}
	}

	// NFA that accepts a single string, which is a single character
	pub fn character(alphabet: HashSet<Char>, char: Char) -> Nfa<bool, Char> {
		assert!(alphabet.contains(&char), "Character should be in alphabet");
		Nfa
			{ states: HashSet::from([false, true])
			, alphabet
			, start_state: false
			, transitions: hash_map! {
				(false, char) => HashSet::from([true]),
				}
			, epsilon_transitions: HashMap::new()
			, accepting: HashSet::from([true])
			}
	}

	// NFA that recognises the concatenation of two NFAs
	pub fn concatenation<State2>(&self, other : &Nfa<State2, Char>) -> Nfa<ConcatState<State, State2>, Char>
	where
		State2: Eq + Hash + Clone
	{
		assert!(self.alphabet == other.alphabet, "Alphabets must be equal!");

		type StateSet<State, State2> = HashSet<ConcatState<State, State2>>;
		type Transition<State, State2, Char> = HashMap<(ConcatState<State, State2>, Char), StateSet<State, State2>>;

		let mut states : StateSet<State, State2> = HashSet::new();
		for state in &self.states {
			states.insert(ConcatState::Left(state.clone()));
		}
		for state in &other.states {
			states.insert(ConcatState::Right(state.clone()));
		}
		let mut transitions : Transition<State, State2, Char>
			= HashMap::new();
		for ((state, char), result) in &self.transitions {
			transitions.insert(
				(ConcatState::Left(state.clone()), char.clone()),
				result
					.iter()
					.map(|s| ConcatState::Left(s.clone()))
					.collect::<StateSet<State, State2>>()
				);
		}
		for ((state, char), result) in &other.transitions {
			transitions.insert(
				(ConcatState::Right(state.clone()), char.clone()),
				result
					.iter()
					.map(|s| ConcatState::Right(s.clone()))
					.collect::<StateSet<State, State2>>()
				);
		}
		let mut epsilon_transitions : HashMap<ConcatState<State, State2>, StateSet<State, State2>>
			= HashMap::new();
		for (state, result) in &self.epsilon_transitions {
			epsilon_transitions.insert(
				ConcatState::Left(state.clone()),
				result
					.iter()
					.map(|s| ConcatState::Left(s.clone()))
					.collect::<StateSet<State, State2>>()
				);
		}
		for (state, result) in &other.epsilon_transitions {
			epsilon_transitions.insert(
				ConcatState::Right(state.clone()),
				result
					.iter()
					.map(|s| ConcatState::Right(s.clone()))
					.collect::<StateSet<State, State2>>()
				);
		}
		for state in &self.accepting {
			epsilon_transitions
				.entry(ConcatState::Left(state.clone()))
				.or_default()
				.insert(ConcatState::Right(other.start_state.clone()));
		}
		Nfa
			{ states
			, alphabet: self.alphabet.clone()
			, start_state: ConcatState::Left(self.start_state.clone())
			, transitions
			, epsilon_transitions
			, accepting:
				other
					.accepting
					.iter()
					.map(|s| ConcatState::Right(s.clone()))
					.collect::<StateSet<State, State2>>()
			}
	}

	pub fn union<State2>(&self, other : &Nfa<State2, Char>) -> Nfa<UnionState<State, State2>, Char>
	where
		State2: Eq + Hash + Clone
	{
		assert!(self.alphabet == other.alphabet, "Alphabets must be equal!");

		type StateSet<State, State2> = HashSet<UnionState<State, State2>>;
		type Transition<State, State2, Char> = HashMap<(UnionState<State, State2>, Char), StateSet<State, State2>>;

		let mut states : StateSet<State, State2> = HashSet::from([UnionState::Start]);
		for state in &self.states {
			states.insert(UnionState::First(state.clone()));
		}
		for state in &other.states {
			states.insert(UnionState::Second(state.clone()));
		}
		let mut transitions : Transition<State, State2, Char>
			= HashMap::new();
		for ((state, char), result) in &self.transitions {
			transitions.insert(
				(UnionState::First(state.clone()), char.clone()),
				result
					.iter()
					.map(|s| UnionState::First(s.clone()))
					.collect::<StateSet<State, State2>>()
				);
		}
		for ((state, char), result) in &other.transitions {
			transitions.insert(
				(UnionState::Second(state.clone()), char.clone()),
				result
					.iter()
					.map(|s| UnionState::Second(s.clone()))
					.collect::<StateSet<State, State2>>()
				);
		}
		let mut epsilon_transitions : HashMap<UnionState<State, State2>, StateSet<State, State2>>
			= hash_map!
				{ UnionState::Start => HashSet::from(
					[ UnionState::First(self.start_state.clone())
					, UnionState::Second(other.start_state.clone())
					]),
				};
		for (state, result) in &self.epsilon_transitions {
			epsilon_transitions.insert(
				UnionState::First(state.clone()),
				result
					.iter()
					.map(|s| UnionState::First(s.clone()))
					.collect::<StateSet<State, State2>>()
				);
		}
		for (state, result) in &other.epsilon_transitions {
			epsilon_transitions.insert(
				UnionState::Second(state.clone()),
				result
					.iter()
					.map(|s| UnionState::Second(s.clone()))
					.collect::<StateSet<State, State2>>()
				);
		}
		let mut accepting : StateSet<State, State2> = HashSet::new();
		for state in &self.accepting {
			accepting.insert(UnionState::First(state.clone()));
		}
		for state in &other.accepting {
			accepting.insert(UnionState::Second(state.clone()));
		}
		Nfa
			{ states
			, alphabet: self.alphabet.clone()
			, start_state: UnionState::Start
			, transitions
			, epsilon_transitions
			, accepting
			}
	}

	pub fn star(&self) -> Nfa<StarState<State>, Char> {
		let mut states : HashSet<StarState<State>> = HashSet::from([StarState::Start]);
		for state in &self.states {
			states.insert(StarState::Old(state.clone()));
		}
		let mut transitions : HashMap<(StarState<State>, Char), HashSet<StarState<State>>>
			= HashMap::new();
		for ((state, char), result) in &self.transitions {
			transitions.insert(
				(StarState::Old(state.clone()), char.clone()),
				result
					.iter()
					.map(|s| StarState::Old(s.clone()))
					.collect::<HashSet<StarState<State>>>()
				);
		}
		let mut epsilon_transitions : HashMap<StarState<State>, HashSet<StarState<State>>>
			= hash_map!
				{ StarState::Start => HashSet::from([StarState::Old(self.start_state.clone())])
				};
		for (state, result) in &self.epsilon_transitions {
			epsilon_transitions.insert(
				StarState::Old(state.clone()),
				result
					.iter()
					.map(|s| StarState::Old(s.clone()))
					.collect::<HashSet<StarState<State>>>()
				);
		}
		for state in &self.accepting {
			epsilon_transitions
				.entry(StarState::Old(state.clone()))
				.or_default()
				.insert(StarState::Old(self.start_state.clone()));
		}
		let mut accepting : HashSet<StarState<State>> = HashSet::from([StarState::Start]);
		for state in &self.accepting {
			accepting.insert(StarState::Old(state.clone()));
		}
		Nfa
			{ states
			, alphabet: self.alphabet.clone()
			, start_state: StarState::Start
			, transitions
			, epsilon_transitions
			, accepting
			}
	}

}