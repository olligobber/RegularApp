module Conversions (
  dfa2nfa,
  nfa2dfa,
  regex2nfa,
  regex2dfa
  ) where

import Prelude (($), (<$>), not, bind, pure, class Ord)
import Data.Maybe (Maybe(Just, Nothing))
import Data.List.Lazy (zipWith, replicateM)
import Data.Foldable (length, fold)
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.Set (Set)
import Data.Set as S
import Data.Map as M

import DFA (DFA(DFA))
import NFA (NFA(NFA))
import NFA as NFA
import Regex (Regex(..))

dfa2nfa :: forall state char. Ord state => Ord char =>
  DFA state char -> NFA (Maybe state) char
dfa2nfa (DFA dfa) = NFA {
  alphabet: dfa.alphabet,
  states: S.insert Nothing $ S.map Just dfa.states,
  startState: dfa.startState,
  transitions: foldMapWithIndex
    (\from map -> foldMapWithIndex
      (\char to -> S.singleton {from: Just from, to: Just to, label: Just char})
      map
    )
    dfa.transitions,
  accepting: S.map Just dfa.accepting
  }

powerSet :: forall a. Ord a => Set a -> Set (Set a)
powerSet s = S.fromFoldable $ do
  include <- replicateM (length s) [false, true]
  pure $
    fold $
    zipWith (\b x -> if b then S.singleton x else S.empty) include $
    S.toUnfoldable s

nfa2dfa :: forall state char. Ord state => Ord char =>
  NFA state char -> DFA (Set state) char
nfa2dfa (NFA nfa) = DFA {
  alphabet: nfa.alphabet,
  states: powerSet nfa.states,
  startState: Just $ NFA.epsilonClosure (NFA nfa) $ S.singleton nfa.startState,
  transitions: M.mapMaybeWithKey
    (\set _ -> Just $ M.mapMaybeWithKey
      (\char _ -> Just $
        NFA.epsilonClosure (NFA nfa) $ NFA.stepChar (NFA nfa) set char
      )
      (S.toMap nfa.alphabet)
    )
    (S.toMap $ powerSet nfa.states),
  accepting: S.filter
    (\set -> not $ S.isEmpty $ set `S.intersection` nfa.accepting)
    (powerSet nfa.states)
}

regex2nfa :: forall char. Ord char =>
  Set char -> Regex char -> Maybe (NFA Int char)
regex2nfa alphabet Empty = Just $ NFA.relabelStates $ NFA.empty alphabet
regex2nfa alphabet Epsilon = Just $ NFA.relabelStates $ NFA.epsilon alphabet
regex2nfa alphabet (Char char) =
  NFA.relabelStates <$> NFA.character alphabet char
regex2nfa alphabet (Concat left right) = do
  leftNFA <- regex2nfa alphabet left
  rightNFA <- regex2nfa alphabet right
  NFA.relabelStates <$> NFA.concat leftNFA rightNFA
regex2nfa alphabet (Union left right) = do
  leftNFA <- regex2nfa alphabet left
  rightNFA <- regex2nfa alphabet right
  NFA.relabelStates <$> NFA.union leftNFA rightNFA
regex2nfa alphabet (Star r) = do
  containedNFA <- regex2nfa alphabet r
  pure $ NFA.relabelStates $ NFA.star containedNFA

regex2dfa :: forall char. Ord char =>
  Set char -> Regex char -> Maybe (DFA (Set Int) char)
regex2dfa alphabet regex = nfa2dfa <$> regex2nfa alphabet regex