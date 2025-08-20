module DFA (
  DFA(..),
  validateDFA,
  parseString,
  isEmpty,
  isComplete,
  complement,
  product,
  union,
  intersection,
  symdiff,
  equal,
  empty,
  complete
  ) where

import Prelude (
  ($), (==), (/=), (||), (&&), (<>), (<$>), (>>=),
  flip, unit,
  class Ord, Void, Unit
  )

import Data.Set (Set)
import Data.Set as S
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (Maybe(Just, Nothing))
import Data.Foldable (class Foldable, foldMap, foldl, all)

-- There is an implicit error state, Nothing, which self loops on all chars
data DFA state char = DFA
  { states :: Set state
  , alphabet :: Set char
  , startState :: Maybe state
  , transitions :: Map state (Map char state)
  , accepting :: Set state
  }

-- Check the stored DFA is valid
validateDFA :: forall state char. Ord state => Ord char =>
  DFA state char -> Boolean
validateDFA (DFA dfa) =
  validStates &&
  validAlphabet &&
  validStart &&
  validTransitions &&
  validAccepting
  where
  validStates = S.checkValid dfa.states
  validAlphabet = S.checkValid dfa.alphabet
  validStart = case dfa.startState of
    Nothing -> true
    Just state -> state `S.member` dfa.states
  validTransitions =
    M.checkValid dfa.transitions &&
    M.keys dfa.transitions `S.subset` dfa.states &&
    all (\m ->
      M.checkValid m &&
      M.keys m `S.subset` dfa.alphabet &&
      all (_ `S.member` dfa.states) m
      ) dfa.transitions
  validAccepting =
    S.checkValid dfa.accepting &&
    dfa.accepting `S.subset` dfa.states

-- Check if a DFA recognises a string
parseString :: forall f state char. Foldable f => Ord state => Ord char =>
  DFA state char -> f char -> Boolean
parseString (DFA dfa) string = accepts $ foldl move start string
  where
  accepts Nothing = false
  accepts (Just state) = state `S.member` dfa.accepting
  move state char = state >>= flip M.lookup (dfa.transitions) >>= M.lookup char
  start = dfa.startState

-- Find the set of reachable states in a DFA
reachableStates :: forall state char. Ord state => Ord char =>
  DFA state char -> Set (Maybe state)
reachableStates (DFA dfa) = go $ S.singleton dfa.startState
  where
  go s = if s == next s then s else go $ next s
  next s = s <> foldMap adjacent s
  adjacent Nothing = S.singleton Nothing
  adjacent (Just state) = case M.lookup state dfa.transitions of
    Nothing -> S.singleton Nothing
    Just m -> S.map (_ `M.lookup` m) dfa.alphabet

-- Check if the recognised language is the empty language
isEmpty :: forall state char. Ord state => Ord char => DFA state char -> Boolean
isEmpty (DFA dfa) =
  S.isEmpty $ S.intersection
    (S.map Just dfa.accepting)
    (reachableStates $ DFA dfa)

-- Check if the recognised language is the complete language
isComplete :: forall state char. Ord state => Ord char => DFA state char -> Boolean
isComplete (DFA dfa) =
  reachableStates (DFA dfa) `S.subset` S.map Just dfa.accepting

-- Make a DFA that recognises the complement language
complement :: forall state char. Ord state => Ord char =>
  DFA state char -> DFA (Maybe state) char
complement (DFA dfa) = DFA {
  states: S.insert Nothing $ S.map Just dfa.states,
  alphabet: dfa.alphabet,
  startState: Just dfa.startState,
  transitions:
    M.mapMaybeWithKey
      (\state _ -> Just $
        M.mapMaybeWithKey
        (\char _ -> Just $
          state >>= flip M.lookup dfa.transitions >>= M.lookup char
        ) $
        S.toMap dfa.alphabet
      ) $
      S.toMap $ S.insert Nothing $ S.map Just dfa.states,
  accepting:
    S.insert Nothing $ S.map Just $ dfa.states `S.difference` dfa.accepting
}

-- Apply the product construction to two DFAs,
-- using a boolean function to decide the new accept states
product :: forall state1 state2 char. Ord state1 => Ord state2 => Ord char =>
  (Boolean -> Boolean -> Boolean) -> DFA state1 char -> DFA state2 char ->
  Maybe (DFA {first :: Maybe state1, second :: Maybe state2} char)
product _ (DFA first) (DFA second) | first.alphabet /= second.alphabet = Nothing
product f (DFA first) (DFA second) = Just $ DFA {
  states: newstates,
  alphabet: first.alphabet,
  startState: Just {first: first.startState, second: second.startState},
  transitions:
    M.mapMaybeWithKey
    (\state _ -> Just $
      M.mapMaybeWithKey
        (\char _ -> Just $
          { first: state.first >>= flip M.lookup first.transitions >>= M.lookup char
          , second: state.second >>= flip M.lookup second.transitions >>= M.lookup char
          }
        ) $
        S.toMap first.alphabet
    ) $
    S.toMap newstates,
  accepting:
    S.filter (\state ->
      f
        (state.first `S.member` S.map Just first.accepting)
        (state.second `S.member` S.map Just second.accepting)
    ) newstates
  }
  where
  newstates =
    foldMap
      (\s1 -> S.map (\s2 -> {first: Just s1, second: Just s2}) second.states)
      first.states <>
    S.map (\s1 -> {first: Just s1, second: Nothing}) first.states <>
    S.map (\s2 -> {first: Nothing, second: Just s2}) second.states <>
    S.singleton {first: Nothing, second: Nothing}

-- Union of two DFAs
union :: forall state1 state2 char. Ord state1 => Ord state2 => Ord char =>
  DFA state1 char -> DFA state2 char ->
  Maybe (DFA {first :: Maybe state1, second :: Maybe state2} char)
union = product (||)

-- Intersection of two DFAs
intersection :: forall state1 state2 char. Ord state1 => Ord state2 => Ord char =>
  DFA state1 char -> DFA state2 char ->
  Maybe (DFA {first :: Maybe state1, second :: Maybe state2} char)
intersection = product (&&)

-- Symmetric difference of two DFAs
symdiff :: forall state1 state2 char. Ord state1 => Ord state2 => Ord char =>
  DFA state1 char -> DFA state2 char ->
  Maybe (DFA {first :: Maybe state1, second :: Maybe state2} char)
symdiff = product (/=)

-- Check if two DFAs recognise the same language
equal :: forall state1 state2 char. Ord state1 => Ord state2 => Ord char =>
  DFA state1 char -> DFA state2 char -> Maybe Boolean
equal first second = isEmpty <$> symdiff first second

-- DFA which recognises no strings
empty :: forall char. Set char -> DFA Void char
empty alphabet = DFA {
  states: S.empty,
  alphabet,
  startState: Nothing,
  transitions: M.empty,
  accepting: S.empty
}

-- DFA which recognises every string
complete :: forall char. Set char -> DFA Unit char
complete alphabet = DFA {
  states: S.singleton unit,
  alphabet,
  startState: Just unit,
  transitions: M.singleton unit $ S.toMap alphabet,
  accepting: S.singleton unit
}

