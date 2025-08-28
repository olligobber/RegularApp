use std::collections::{HashMap, HashSet, VecDeque};
use std::hash::Hash;
use map_macro::hash_map;

pub struct Nfa<State, Char> {
	pub states: HashSet<State>,
	pub alphabet: HashSet<Char>,
	pub start_state: State,
	pub transitions: HashMap<(State, Char), HashSet<State>>,
	pub epsilon_transitions: HashMap<State, HashSet<State>>,
	pub accepting: HashSet<State>,
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
	pub fn transition(&self, state: &State, char: &Char) -> Option<&HashSet<State>> {
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
	pub fn epsilon_closure(&self, start: Box<dyn Iterator<Item=State>>) -> HashSet<State> {
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

	// NFA for the star closure of another NFA
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

	// Relabel the reachable states using integers
	pub fn relabel_states(&self) -> Nfa<u64, Char> {
		let mut map_to_new : HashMap<State, u64> = HashMap::new();
		let mut map_to_old : HashMap<u64, State> = HashMap::new();
		let mut states : HashSet<u64> = HashSet::new();
		for (i, state) in (0_u64..).zip(self.reachable_states().into_iter()) {
			map_to_new.insert(state.clone(), i);
			map_to_old.insert(i, state);
			states.insert(i);
		}
		let mut transitions : HashMap<(u64, Char), HashSet<u64>> = HashMap::new();
		for ((state, char), target) in &self.transitions {
			match map_to_new.get(state) {
				None => {},
				Some(new_state) => {
					transitions.insert(
						(*new_state, char.clone()),
						target
							.iter()
							.map(|s| *map_to_new.get(s).expect("Invalid NFA"))
							.collect::<HashSet<u64>>()
					);
				}
			}
		}
		let mut epsilon_transitions : HashMap<u64, HashSet<u64>> = HashMap::new();
		for (state, target) in &self.epsilon_transitions {
			match map_to_new.get(state) {
				None => {},
				Some(new_state) => {
					epsilon_transitions.insert(
						*new_state,
						target
							.iter()
							.map(|s| *map_to_new.get(s).expect("Invalid NFA"))
							.collect::<HashSet<u64>>()
					);
				}
			}
		}
		let mut accepting : HashSet<u64> = HashSet::new();
		for state in &self.accepting {
			match map_to_new.get(state) {
				None => {}
				Some(new_state) => {
					accepting.insert(*new_state);
				}
			}
		}
		Nfa
			{ states
			, alphabet: self.alphabet.clone()
			, start_state: *map_to_new.get(&self.start_state).expect("Invalid NFA")
			, transitions
			, epsilon_transitions
			, accepting
			}
	}
}

// NFA which accepts nothing
pub fn empty<Char>(alphabet: HashSet<Char>) -> Nfa<(), Char> {
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
pub fn epsilon<Char>(alphabet: HashSet<Char>) -> Nfa<(), Char> {
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
pub fn character<Char>(alphabet: HashSet<Char>, char: Char) -> Nfa<bool, Char>
where
	Char: Eq + Hash,
{
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

#[derive(PartialEq, Eq, Hash, Clone, Copy)]
pub enum ConcatState<State1, State2> {
	Left(State1),
	Right(State2),
}

// NFA that recognises the concatenation of two NFAs
pub fn concatenation<State1, State2, Char>(left : &Nfa<State1, Char>, right : &Nfa<State2, Char>) -> Nfa<ConcatState<State1, State2>, Char>
where
	State1: Eq + Hash + Clone,
	State2: Eq + Hash + Clone,
	Char: Eq + Hash + Clone,
{
	assert!(left.alphabet == right.alphabet, "Alphabets must be equal!");

	type StateSet<A, B> = HashSet<ConcatState<A, B>>;
	type Transition<A, B, Char> = HashMap<(ConcatState<A, B>, Char), StateSet<A, B>>;

	let mut states : StateSet<State1, State2> = HashSet::new();
	for state in &left.states {
		states.insert(ConcatState::Left(state.clone()));
	}
	for state in &right.states {
		states.insert(ConcatState::Right(state.clone()));
	}
	let mut transitions : Transition<State1, State2, Char>
		= HashMap::new();
	for ((state, char), result) in &left.transitions {
		transitions.insert(
			(ConcatState::Left(state.clone()), char.clone()),
			result
				.iter()
				.map(|s| ConcatState::Left(s.clone()))
				.collect::<StateSet<State1, State2>>()
			);
	}
	for ((state, char), result) in &right.transitions {
		transitions.insert(
			(ConcatState::Right(state.clone()), char.clone()),
			result
				.iter()
				.map(|s| ConcatState::Right(s.clone()))
				.collect::<StateSet<State1, State2>>()
			);
	}
	let mut epsilon_transitions : HashMap<ConcatState<State1, State2>, StateSet<State1, State2>>
		= HashMap::new();
	for (state, result) in &left.epsilon_transitions {
		epsilon_transitions.insert(
			ConcatState::Left(state.clone()),
			result
				.iter()
				.map(|s| ConcatState::Left(s.clone()))
				.collect::<StateSet<State1, State2>>()
			);
	}
	for (state, result) in &right.epsilon_transitions {
		epsilon_transitions.insert(
			ConcatState::Right(state.clone()),
			result
				.iter()
				.map(|s| ConcatState::Right(s.clone()))
				.collect::<StateSet<State1, State2>>()
			);
	}
	for state in &left.accepting {
		epsilon_transitions
			.entry(ConcatState::Left(state.clone()))
			.or_default()
			.insert(ConcatState::Right(right.start_state.clone()));
	}
	Nfa
		{ states
		, alphabet: left.alphabet.clone()
		, start_state: ConcatState::Left(left.start_state.clone())
		, transitions
		, epsilon_transitions
		, accepting:
			right
				.accepting
				.iter()
				.map(|s| ConcatState::Right(s.clone()))
				.collect::<StateSet<State1, State2>>()
		}
}

#[derive(PartialEq, Eq, Hash, Clone, Copy)]
pub enum UnionState<State1, State2> {
	Start,
	First(State1),
	Second(State2),
}

// NFA that accepts anything that either NFA accepts
pub fn union<State1, State2, Char>(first : &Nfa<State1, Char>, second : &Nfa<State2, Char>) -> Nfa<UnionState<State1, State2>, Char>
where
	State1: Eq + Hash + Clone,
	State2: Eq + Hash + Clone,
	Char: Eq + Hash + Clone,
{
	assert!(first.alphabet == second.alphabet, "Alphabets must be equal!");

	type StateSet<A, B> = HashSet<UnionState<A, B>>;
	type Transition<A, B, Char> = HashMap<(UnionState<A, B>, Char), StateSet<A, B>>;

	let mut states : StateSet<State1, State2> = HashSet::from([UnionState::Start]);
	for state in &first.states {
		states.insert(UnionState::First(state.clone()));
	}
	for state in &second.states {
		states.insert(UnionState::Second(state.clone()));
	}
	let mut transitions : Transition<State1, State2, Char>
		= HashMap::new();
	for ((state, char), result) in &first.transitions {
		transitions.insert(
			(UnionState::First(state.clone()), char.clone()),
			result
				.iter()
				.map(|s| UnionState::First(s.clone()))
				.collect::<StateSet<State1, State2>>()
			);
	}
	for ((state, char), result) in &second.transitions {
		transitions.insert(
			(UnionState::Second(state.clone()), char.clone()),
			result
				.iter()
				.map(|s| UnionState::Second(s.clone()))
				.collect::<StateSet<State1, State2>>()
			);
	}
	let mut epsilon_transitions : HashMap<UnionState<State1, State2>, StateSet<State1, State2>>
		= hash_map!
			{ UnionState::Start => HashSet::from(
				[ UnionState::First(first.start_state.clone())
				, UnionState::Second(second.start_state.clone())
				]),
			};
	for (state, result) in &first.epsilon_transitions {
		epsilon_transitions.insert(
			UnionState::First(state.clone()),
			result
				.iter()
				.map(|s| UnionState::First(s.clone()))
				.collect::<StateSet<State1, State2>>()
			);
	}
	for (state, result) in &second.epsilon_transitions {
		epsilon_transitions.insert(
			UnionState::Second(state.clone()),
			result
				.iter()
				.map(|s| UnionState::Second(s.clone()))
				.collect::<StateSet<State1, State2>>()
			);
	}
	let mut accepting : StateSet<State1, State2> = HashSet::new();
	for state in &first.accepting {
		accepting.insert(UnionState::First(state.clone()));
	}
	for state in &second.accepting {
		accepting.insert(UnionState::Second(state.clone()));
	}
	Nfa
		{ states
		, alphabet: first.alphabet.clone()
		, start_state: UnionState::Start
		, transitions
		, epsilon_transitions
		, accepting
		}
}
