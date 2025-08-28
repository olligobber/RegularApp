use std::collections::{BTreeSet, HashMap, HashSet, VecDeque};
use std::hash::Hash;

use crate::dfa::Dfa;
use crate::nfa::Nfa;
use crate::regex::Regex;

pub fn dfa_to_nfa<State, Char>(dfa: &Dfa<State, Char>) -> Nfa<State, Char>
where
	State: Eq + Hash + Clone,
	Char: Eq + Hash + Clone,
{
	Nfa
		{ states : dfa.states.clone()
		, alphabet: dfa.alphabet.clone()
		, start_state: dfa.start_state.clone()
		, transitions:
			dfa
				.transitions
				.iter()
				.map(
					|(key, val)|
					(key.clone(), HashSet::from([val.clone()]))
				)
				.collect::<HashMap<(State, Char), HashSet<State>>>()
		, epsilon_transitions: HashMap::new()
		, accepting: dfa.accepting.clone()
		}
}

pub fn nfa_to_dfa<State, Char>(nfa: &Nfa<State, Char>) -> Dfa<BTreeSet<State>, Char>
where
	State: Ord + Hash + Clone + 'static,
	Char: Eq + Hash + Clone,
{
	let mut states : HashSet<BTreeSet<State>> = HashSet::new();
	let start_state : BTreeSet<State>
		= BTreeSet::from_iter(
			nfa
				.epsilon_closure(Box::new([nfa.start_state.clone()].into_iter()))
			);
	let mut transitions : HashMap<(BTreeSet<State>, Char), BTreeSet<State>>
		= HashMap::new();
	let mut accepting : HashSet<BTreeSet<State>> = HashSet::new();
	let mut to_explore : VecDeque<BTreeSet<State>>
		= VecDeque::from([start_state.clone()]);
	loop {
		match to_explore.pop_back() {
			None => { break }
			Some(state) => {
				if states.contains(&state) { continue }
				states.insert(state.clone());
				for s in &state {
					if nfa.accepting.contains(s) {
						accepting.insert(state.clone());
						break
					}
				}
				for char in &nfa.alphabet {
					let mut target: BTreeSet<State> = BTreeSet::new();
					for input in &state {
						match nfa.transition(input, char) {
							None => {}
							Some(output) => {
								target.extend(output.clone());
							}
						}
					}
					let actual_target = BTreeSet::from_iter(
						nfa.epsilon_closure(Box::new(target.into_iter()))
					);
					transitions.insert((state.clone(), char.clone()), actual_target.clone());
					to_explore.push_back(actual_target);
				}
			}
		}
	}
	Dfa
		{ states
		, alphabet: nfa.alphabet.clone()
		, start_state
		, transitions
		, accepting
		}
}

pub fn regex_to_nfa<Char>(regex: &Regex<Char>, alphabet: HashSet<Char>) -> Nfa<u64, Char>
where
	Char: Eq + Hash + Clone
{
	match regex {
		Regex::Empty => { Nfa::empty(alphabet).relabel_states() }
		Regex::Epsilon => { Nfa::epsilon(alphabet).relabel_states() }
		Regex::Character(char) =>
			{ Nfa::character(alphabet, char.clone()).relabel_states() }
		Regex::Concat(left, right) => {
			let left_nfa = regex_to_nfa(left, alphabet.clone());
			let right_nfa = regex_to_nfa(right, alphabet);
			Nfa::concatenation(&left_nfa, &right_nfa).relabel_states()
		}
		Regex::Union(left, right) => {
			let left_nfa = regex_to_nfa(left, alphabet.clone());
			let right_nfa = regex_to_nfa(right, alphabet);
			Nfa::union(&left_nfa, &right_nfa).relabel_states()
		}
		Regex::Star(contents) => {
			let contents_nfa = regex_to_nfa(contents, alphabet);
			contents_nfa.star().relabel_states()
		}
	}
}

pub fn regex_to_dfa<Char>(regex: &Regex<Char>, alphabet: HashSet<Char>) -> Dfa<u64, Char>
where
	Char: Eq + Hash + Clone
{
	nfa_to_dfa(&regex_to_nfa(regex, alphabet)).relabel_states()
}