#![allow(dead_code)]

use std::collections::HashSet;

use dioxus::prelude::*;

mod conversions;
mod dfa;
mod regex;
mod nfa;

use regex::Regex;
use conversions::regex_to_dfa;

const MAIN_CSS: Asset = asset!("/assets/main.css");

fn main() {
	dioxus::launch(App);
}

#[component]
fn App() -> Element {
	rsx! {
		document::Link { rel: "stylesheet", href: MAIN_CSS }
		RegexApp {}
	}
}

fn compare(string1: &str, string2: &str, alphabet: &str) -> String {
	Regex::parse_regex(string1).map_or_else(
		|e| format!("Error parsing Regex 1: {}", e),
		|regex1| Regex::parse_regex(string2).map_or_else(
			|e| format!("Error parsing Regex 2: {}", e),
			|regex2| regex_to_dfa(&regex1, HashSet::from_iter(alphabet.chars()))
				.map_or_else(
					|e| format!("Error processing Regex 1: character not in alphabet: {}", e),
					|dfa1| regex_to_dfa(&regex2, HashSet::from_iter(alphabet.chars()))
						.map_or_else(
							|e| format!("Error processing Regex 2: character not in alphabet: {}", e),
							|dfa2|
								if dfa1.equivalent(&dfa2) {
									format!("The regex \"{}\" and \"{}\" are equivalent", string1, string2)
								} else {
									format!("The regex \"{}\" and \"{}\" are not equivalent", string1, string2)
								}
						)
				)
			)
	)
}

fn format(string: &str) -> String {
	string
		.replace("\\epsilon", "ε")
		.replace("\\empty", "∅")
}

#[component]
fn RegexApp() -> Element {
	let mut alphabet = use_signal(String::new);
	let mut regex1 = use_signal(String::new);
	let mut regex2 = use_signal(String::new);
	let mut result = use_signal(String::new);

	rsx! {
		div {
			p { "Enter your alphabet, which may only consist of alphanumeric ascii characters. Other characters will be ignored." }
			p {
				"Σ = {{"
				input {
					oninput: move |event| async move {
						alphabet.set(event.value());
					},
					autocomplete: "off"
				}
				"}}"
			}
			p { "Enter two regex to compare. Type \\empty for the empty regex ∅, and \\epsilon for the empty string ε." }
			p {
				"Regex 1 = "
				input {
					oninput: move |event| async move {
						regex1.set(format(&event.value()));
					},
					value: {
						format!("{}",regex1.read())
					},
					autocomplete: "off"
				}
			}
			p {
				"Regex 2 = "
				input {
					oninput: move |event| async move {
						regex2.set(format(&event.value()));
					},
					value: {
						format!("{}",regex2.read())
					},
					autocomplete: "off"
				}
			}
			button {
				onclick: move |_| async move {
					result.set(compare(&regex1.read(), &regex2.read(), &alphabet.read()));
				},
				"Compare regex"
			}
			p {
				"{result.read()}"
			}
		}
	}
}
