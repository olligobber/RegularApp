module Main where

import Prelude (Unit, unit, pure, bind, discard, ($), (>>>), (<>))
import Control.Monad.State.Class as State
import Data.Array (filter)
import Data.Either (Either(Left, Right))
import Data.Maybe (Maybe(Just, Nothing))
import Data.Set as Set
import Data.String.Common (replace)
import Data.String.Pattern (Pattern(Pattern), Replacement(Replacement))
import Data.String.CodeUnits (toCharArray)
import Effect (Effect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Parsing (parseErrorMessage)

import Regex (validChar, parseRegex)
import Conversions (regex2dfa)
import DFA as DFA

type State = {
  alphabetEntry :: String,
  regex1Entry :: String,
  regex2Entry :: String,
  lastOutput :: String
}

initialState :: State
initialState = {
  alphabetEntry: "",
  regex1Entry: "",
  regex2Entry: "",
  lastOutput: ""
  }

data Action =
  TypeAlphabet String |
  TypeRegex1 String |
  TypeRegex2 String |
  Compare |
  None

render :: forall m. State -> H.ComponentHTML Action () m
render state = HH.div
  [ HP.id "main" ]
  [ HH.text "Enter your alphabet, which may only consist of alphanumeric ascii characters. Other characters will be ignored."
  , HH.br_
  , HH.text "Σ = {"
  , HH.input
    [ HE.onValueInput TypeAlphabet
    ]
  , HH.text "}"
  , HH.br_
  , HH.text "Enter two regex to compare. Type \\empty for the empty regex ∅, and \\epsilon for the empty string ε."
  , HH.br_
  , HH.text "Regex 1 = "
  , HH.input
    [ HE.onValueInput TypeRegex1
    , HP.value state.regex1Entry
    ]
  , HH.br_
  , HH.text "Regex 2 = "
  , HH.input
    [ HE.onValueInput TypeRegex2
    , HP.value state.regex2Entry
    ]
  , HH.br_
  , HH.button [HE.onClick \_ -> Compare] [HH.text "Compare regex"]
  , HH.br_
  , HH.text state.lastOutput
  ]

formatRegex :: String -> String
formatRegex =
  replace (Pattern "\\epsilon") (Replacement "ε") >>>
  replace (Pattern "\\empty") (Replacement "∅")

handleAction :: forall m. Action -> H.HalogenM State Action () Unit m Unit
handleAction (TypeAlphabet s) = do
  _ <- State.modify $ _ { alphabetEntry = s }
  pure unit
handleAction (TypeRegex1 s) = do
  _ <- State.modify $ _ { regex1Entry = formatRegex s }
  pure unit
handleAction (TypeRegex2 s) = do
  _ <- State.modify $ _ { regex2Entry = formatRegex s }
  pure unit
handleAction Compare = do
  alphabet <- State.gets $
    _.alphabetEntry >>> toCharArray >>> filter validChar >>> Set.fromFoldable
  input1 <- State.gets _.regex1Entry
  case parseRegex input1 of
    Left e -> do
      _ <- State.modify $ _ { lastOutput = "Error parsing regex 1: " <> parseErrorMessage e }
      pure unit
    Right regex1 -> do
      input2 <- State.gets _.regex2Entry
      case parseRegex input2 of
        Left e -> do
          _ <- State.modify $ _ { lastOutput = "Error parsing regex 2: " <> parseErrorMessage e }
          pure unit
        Right regex2 -> case regex2dfa alphabet regex1 of
          Nothing -> do
            _ <- State.modify $ _ { lastOutput = "Error converting regex 1 to a DFA, are all the characters in it also in the alphabet?" }
            pure unit
          Just dfa1 -> case regex2dfa alphabet regex2 of
            Nothing -> do
              _ <- State.modify $ _ { lastOutput = "Error converting regex 2 to a DFA, are all the characters in it also in the alphabet?" }
              pure unit
            Just dfa2 -> case DFA.symdiff dfa1 dfa2 of
              Nothing -> do
                _ <- State.modify $ _ { lastOutput = "Error comparing DFAs, this should never happen" }
                pure unit
              Just symdiff -> if DFA.isEmpty symdiff then do
                  _ <- State.modify $ _ { lastOutput = "The regex " <> input1 <> " and " <> input2 <> " are equivalent" }
                  pure unit
                else do
                  _ <- State.modify $ _ { lastOutput = "The regex " <> input1 <> " and " <> input2 <> " recognise different languages" }
                  pure unit
handleAction None = pure unit

component :: forall q m. H.Component q Action Unit m
component = H.mkComponent
  { initialState: pure initialState
  , render
  , eval : H.mkEval $ H.defaultEval { handleAction = handleAction }
  }

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component None body
