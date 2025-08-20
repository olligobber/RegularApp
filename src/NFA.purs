module NFA (
  NFA(..),
  validateNFA,
  reachableStates,
  relabelStates,
  epsilonClosure,
  stepChar,
  parseString,
  empty,
  epsilon,
  character,
  union,
  concat,
  star
  ) where

import Prelude (
  ($), (<$), (<<<), (==), (/=), (&&), (<>), (+),
  not, unit, bind, discard, pure,
  class Ord, Unit
  )

import Data.Set (Set)
import Data.Set as S
import Data.Map as M
import Data.Maybe (Maybe(Just, Nothing))
import Data.Foldable (class Foldable, foldMap, foldl, all, length)
import Data.Traversable (sequence)
import Data.Either (Either(Right, Left))
import Data.Array ((..))
import Control.Monad.State as State

data NFA state char = NFA
  { states :: Set state
  , alphabet :: Set char
  , startState :: state
  , transitions :: Set {from :: state, to :: state, label :: Maybe char}
  , accepting :: Set state
  }

-- Check the stored NFA is valid
validateNFA :: forall state char. Ord state => Ord char =>
  NFA state char -> Boolean
validateNFA (NFA nfa) =
  validStates &&
  validAlphabet &&
  validStart &&
  validTransitions &&
  validAccepting
  where
  validStates = S.checkValid nfa.states
  validAlphabet = S.checkValid nfa.alphabet
  validStart = nfa.startState `S.member` nfa.states
  validTransitions =
    S.checkValid nfa.transitions &&
    all
      (\t ->
        t.from `S.member` nfa.states &&
        t.to `S.member` nfa.states &&
        all (_ `S.member` nfa.alphabet) t.label
      )
      nfa.transitions
  validAccepting =
    S.checkValid nfa.accepting &&
    nfa.accepting `S.subset` nfa.states

reachableStates :: forall state char. Ord state => Ord char =>
  NFA state char -> Set state
reachableStates (NFA nfa) = go $ S.singleton nfa.startState
  where
  go s = if s == next s then s else go $ next s
  next s = s <> foldMap
    (\t -> if t.from `S.member` s then S.singleton t.to else S.empty)
    nfa.transitions

-- Relabel the reachable states as integers from 1 to n
relabelStates :: forall state char. Ord state => Ord char =>
  NFA state char -> NFA Int char
relabelStates (NFA nfa) = NFA {
  alphabet: nfa.alphabet,
  states: newStates,
  startState: case nfa.startState `M.lookup` stateMap of
    Nothing -> 1 -- This should never happen but I can't prove it
    Just n -> n,
  transitions: foldMap
    (\t -> case t.from `M.lookup` stateMap of
      Nothing -> S.empty
      Just from -> case t.to `M.lookup` stateMap of
        Nothing -> S.empty -- This should never happen
        Just to -> S.singleton {from, to, label: t.label}
    )
    nfa.transitions,
  accepting: foldMap
    (\s -> case s `M.lookup` stateMap of
      Nothing -> S.empty
      Just n -> S.singleton n
    )
    nfa.accepting
}
  where
  oldStates = reachableStates (NFA nfa)
  newStates = S.fromFoldable $ 1..length oldStates
  stateMap = State.evalState (sequence $ increment <$ S.toMap oldStates) 1
  increment = do
    x <- State.get
    State.put (x+1)
    pure x

-- Find all states that can be reached by only epsilon transitions
epsilonClosure :: forall state char. Ord state => Ord char =>
  NFA state char -> Set state -> Set state
epsilonClosure (NFA nfa) set =
  if nextSet == set then set else epsilonClosure (NFA nfa) $ nextSet
  where
  nextSet = set <> foldMap
    (\t ->
      if t.from `S.member` set && t.label == Nothing then
        S.singleton t.to
      else
        S.empty
    )
    nfa.transitions

-- Find all states that can be reached by following one transition labelled by
-- a character
stepChar :: forall state char. Ord state => Ord char =>
  NFA state char -> Set state -> char -> Set state
stepChar (NFA nfa) set char = foldMap
    (\t ->
      if t.from `S.member` set && t.label == Just char then
        S.singleton t.to
      else
        S.empty
    )
    nfa.transitions

-- Check if an NFA recognises a string
parseString :: forall f state char. Foldable f => Ord state => Ord char =>
  NFA state char -> f char -> Boolean
parseString (NFA nfa) string = hasAccepting $ foldl next start string
  where
  hasAccepting set = not $ S.isEmpty $ set `S.intersection` nfa.accepting
  start = epsilonClosure (NFA nfa) $ S.singleton nfa.startState
  next set char = epsilonClosure (NFA nfa) $ stepChar (NFA nfa) set char

-- The NFA that recognises no strings
empty :: forall char. Ord char => Set char -> NFA Unit char
empty alphabet = NFA {
  states: S.singleton unit,
  alphabet,
  startState: unit,
  transitions: S.empty,
  accepting: S.empty
}

-- The NFA that recognises the empty string
epsilon :: forall char. Ord char => Set char -> NFA Unit char
epsilon alphabet = NFA {
  states: S.singleton unit,
  alphabet,
  startState: unit,
  transitions: S.empty,
  accepting: S.singleton unit
}

-- The NFA that recognises a single character
character :: forall char. Ord char =>
  Set char -> char -> Maybe (NFA Boolean char)
character alphabet char | not $ char `S.member` alphabet = Nothing
character alphabet char = Just $ NFA {
  states: S.singleton true <> S.singleton false,
  alphabet,
  startState: false,
  transitions: S.singleton {from: false, to: true, label: Just char},
  accepting: S.singleton true
}

-- Union two NFA's languages
union :: forall state1 state2 char. Ord state1 => Ord state2 => Ord char =>
  NFA state1 char -> NFA state2 char ->
  Maybe (NFA (Maybe (Either state1 state2)) char)
union (NFA first) (NFA second) | first.alphabet /= second.alphabet = Nothing
union (NFA first) (NFA second) = Just $ NFA {
  states:
    S.singleton Nothing <>
    S.map (Just <<< Left) first.states <>
    S.map (Just <<< Right) second.states,
  alphabet: first.alphabet,
  startState: Nothing,
  transitions:
    S.singleton
      {from: Nothing, to: Just $ Left first.startState, label: Nothing} <>
    S.singleton
      {from: Nothing, to: Just $ Right second.startState, label: Nothing} <>
    S.map
      (\t -> {from: Just $ Left t.from, to: Just $ Left t.to, label: t.label})
      first.transitions <>
    S.map
      (\t -> {from: Just $ Right t.from, to: Just $ Right t.to, label: t.label})
      second.transitions,
  accepting:
    S.map (Just <<< Left) first.accepting <>
    S.map (Just <<< Right) second.accepting
}

-- Concatenate the languages of two NFAs
concat :: forall state1 state2 char. Ord state1 => Ord state2 => Ord char =>
  NFA state1 char -> NFA state2 char -> Maybe (NFA (Either state1 state2) char)
concat (NFA first) (NFA second) | first.alphabet /= second.alphabet = Nothing
concat (NFA first) (NFA second) = Just $ NFA {
  states: S.map Left first.states <> S.map Right second.states,
  alphabet: first.alphabet,
  startState: Left first.startState,
  transitions:
    S.map
      (\t -> {from: Left t.from, to: Left t.to, label: t.label})
      first.transitions <>
    S.map
      (\a -> {from: Left a, to: Right second.startState, label: Nothing})
      first.accepting <>
    S.map
      (\t -> {from: Right t.from, to: Right t.to, label: t.label})
      second.transitions,
  accepting: S.map Right second.accepting
}

-- Get the star closure of the language of an NFA
star :: forall state char. Ord state => Ord char =>
  NFA state char -> NFA (Maybe state) char
star (NFA nfa) = NFA {
  states: S.singleton Nothing <> S.map Just nfa.states,
  alphabet: nfa.alphabet,
  startState: Nothing,
  transitions:
    S.singleton {from: Nothing, to: Just nfa.startState, label: Nothing} <>
    S.map
      (\t -> {from: Just t.from, to: Just t.to, label: t.label})
      nfa.transitions <>
    S.map
      (\a -> {from: Just a, to: Just nfa.startState, label: Nothing})
      nfa.accepting,
  accepting: S.singleton Nothing <> S.map Just nfa.accepting
}