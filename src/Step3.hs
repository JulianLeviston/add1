{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# Language OverloadedStrings #-}

module Step3 where

import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad.State.Class (MonadState, get, put, modify)
import NumberStore (Number, NumberStore, Store)
import qualified NumberStore

{--
Now, we take the stuff from Step 2.5 and generalise and reduce the operations to
a base set that deal with the effects (compositionally) and then ones that rely on
those ones (ie composite ones).


The features are:
1. a store of numbers that retains its input order
2. the ability to print out the store of numbers, and the summaries
3. the ability to input new numbers into the store
4. the ability to exit the program

One of the goals here is to provide a set of *atomic algebraic constructors* (or a simple core);
to find out what the types mean.

We can start with fuzzy types (and our process here is about iterating back and forth
between the operations and the types and clarifying)

--}

-- Effects:
-- data Command -- the type representing the possible commands the program can execute (driving input, pure processing & output of the program)
-- data State s a -- the type of the state effect that keeps the store data that the user's commands use and have effect on
-- data InputOutput a -- the type representing having an input or output effect on this algebra
-- data Program a -- represents the sequencing and execution effect required to make all other effects take effect

-- We want to say there's some type that we don't know anything, particularly

class Monad m => InputOutput m where
  inputText :: m Text -- a way to get text into this algebra from outside of it
  outputText :: Text -> m () -- a way to get text to outside of this algebra

overrideInput :: Functor f => b -> f a -> f b -- a way to control what is 'inside' a context
overrideInput = fmap . const

programBind :: Monad m => m a -> (a -> m b) -> m b -- a way to bind a function on programs into another program
programBind = (>>=)

pureProgram :: Applicative f => a -> f a
pureProgram = pure

-- Law 'programBind/inputText/outputText'
-- forall (a :: Text).
--   programBind (overrideInput a inputText) outputText = outputText a

programSequence :: Monad m => m a -> m b -> m b -- way to sequence the effects of two programs, throwing away the first result
programSequence = (>>)

-- Law 'programSequence'
-- forall (a :: Text, b :: Text).
--   programSequence (outputText a) (outputText b) = outputText (a <> b)

insertInStore :: MonadState NumberStore m => Number -> m () -- lifted version from Step2 where the State is an effect within the program
insertInStore n = modify (NumberStore.insertInStore n)

getNumberStore :: MonadState NumberStore m => m NumberStore -- a way to get the NumberStore state out of the ambient state effect of a program
getNumberStore = get

-- Law 'insertInStore/getNumberStore'
--   forall (a :: Text).
--     programSequence (insertInStore a) getNumberStore = pureProgram (singletonStore a)

-- we can then apply any of the functions from Step2 using a program map function to get the projections ("lifted" algebraic operations)

programMap :: Functor f => (a -> b) -> f a -> f b
programMap = fmap

-- Law 'programMap'
-- forall (a :: Text, b :: Text).
--   programMap (const b) (pureProgram a) = pureProgram b

data Command where
  AddNumbersToStoreCommand :: [Number] -> Command -- add a sequence of numbers to the store
  ShowSummariesCommand :: Command -- print out the summaries of the store
  ExitCommand :: Command

addNumbersToStore :: [Number] -> Command
addNumbersToStore = AddNumbersToStoreCommand

showSummaries :: Command
showSummaries = ShowSummariesCommand

exit:: Command
exit = ExitCommand

prettyPrintHelpTextForCommand :: Command -> Text -- the inverse of parsing a command (a projection on the command algebra)
prettyPrintHelpTextForCommand cmd =
  case cmd of
    AddNumbersToStoreCommand _ -> "Add 1 2 3"
    ShowSummariesCommand -> "Show"
    ExitCommand -> "Exit"

parseCommand :: Text -> Maybe Command -- try to parse a text
parseCommand txt = parsedWords >>= parseCmdFromWords
  where 
    parsedWords =
      case T.words txt of
        [] -> Nothing
        ws -> Just ws
    parseCmdFromWords wds =
      case T.unpack <$> wds of
        "Add" : args ->
          case parseNums args of
            [] -> Nothing
            ns -> Just $ AddNumbersToStoreCommand ns
        ["Show"] -> pure ShowSummariesCommand
        ["Exit"] -> pure ExitCommand
    parseNums = fmap fst . foldMap (reads :: ReadS Number)

-- Law 'parseCommand/prettyPrintHelpTextForCommand'
-- forAll (a :: Command).
--   parseCommand (prettyPrintHelpTextForCommand a) = Just a

greeting :: Text
greeting = "Welcome to the program."

printGreeting :: InputOutput m => m () -- show a greeting to the program
printGreeting = outputText greeting

textIsWithinText :: Text -> Text -> Bool -- whether the first text is within the second text
textIsWithinText = T.isInfixOf

-- Law 'TextIsWithinText'
-- forAll (a :: Text, b :: Text, c :: Text).
--   textIsWithinText b (a <> b <> c) = True

helpText :: Text -- the text explaining the commands
helpText = "You can use any of the following commands: " <> T.intercalate ", " (prettyPrintHelpTextForCommand <$> commands)
  where
    commands = [addNumbersToStore [1,2,3], showSummaries, exit]

promptText :: Text -- the text that prompts the user to type some input
promptText = helpText <> "\nCommand: "

-- Law 'helpText'
-- forAll (a :: Command).
--   textIsWithinText (prettyPrintHelpTextForCommand a) helpText = True

printHelpAndPrompt :: InputOutput m => m () -- show a prompt & help explaining the commands that can be typed
printHelpAndPrompt = outputText $ helpText <> promptText

-- Law 'helpTextPrompt'
-- outputText (helpText <> "\n" <> promptText) = printHelpAndPrompt

data IsEndStep = EndProgram | ContinueProgram
  deriving Show

endProgram :: IsEndStep
endProgram = EndProgram

continueProgram :: IsEndStep
continueProgram = ContinueProgram

-- retrieves a line of text and tries to parse it into a command, returning nothing if it can't
promptGetInputAndParseCommand :: InputOutput m =>  m (Maybe Command)
promptGetInputAndParseCommand =
  printHelpAndPrompt `programSequence`
    inputText
      `programBind` \txt -> pure (parseCommand txt)

summarizeStore :: NumberStore -> Text
summarizeStore ns =
  "The mean is " <> (T.pack . show) (NumberStore.meanStore ns) <>
  "\nThe sum is " <> (T.pack . show) (NumberStore.sumStore ns) <>
  "\nThe product is " <> (T.pack . show) (NumberStore.productStore ns) <>
  "\nThe count is " <> (T.pack . show) (NumberStore.lengthStore ns) <> "\n"

interpretCommand :: (InputOutput m, MonadState NumberStore m) => Maybe Command -> m IsEndStep -- execute a command or print an error message and loop back, finally returning whether to end or not
interpretCommand Nothing = promptGetInputAndParseCommand >>= interpretCommand -- loop back around because there was no parseable command
interpretCommand (Just ExitCommand) = pure endProgram
interpretCommand (Just cmd) =
  let commandAction = 
        case cmd of
          AddNumbersToStoreCommand ns -> sequence_ (insertInStore <$> ns)
          ShowSummariesCommand -> getNumberStore `programBind` (outputText . summarizeStore)
  in commandAction `programSequence` pure continueProgram

-- Law 'interpretCommand/addNumbersToStore'
-- forAll (ns :: [Number]).
--   interpretCommand (Just (addNumbersToStore ns)) = foldr insertInStore (pureProgram continueProgram) ns

-- Law 'interpretCommand/showSummaries'
-- forAll (ns :: [Number]).
--   interpretCommand (Just showSummaries) = programSequence (outputText) (pureProgram continueProgram) ns

loopOrEnd :: (InputOutput m, MonadState NumberStore m) => IsEndStep -> m () -- conditionally reloop the program or end it depending on Endstep
loopOrEnd EndProgram = pure ()
loopOrEnd ContinueProgram = mainLoop

-- Law 'loopOrEnd/endProgram'
--   loopOrEnd endProgram = pureProgram ()

-- Law 'loopOrEnd/mainloop'
--   loopOrEnd continueProgram = mainLoop

-- main_ is the program in the algebra; it will be interpreted into whatever context is needed
main_ :: (InputOutput m, MonadState NumberStore m) => m ()
main_ = printGreeting `programSequence` mainLoop

mainLoop :: (InputOutput m, MonadState NumberStore m) => m ()
mainLoop =
  promptGetInputAndParseCommand
    `programBind` interpretCommand
    `programBind` loopOrEnd

-- need to build the property tests around the basic laws
-- then use QuickSpec to derive some more laws
-- then refactor to abstract more, optimize and simplify
