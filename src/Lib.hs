{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts, TypeSynonymInstances, FlexibleInstances #-}
{-# Language OverloadedStrings #-}

module Lib where

import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad.State.Class (MonadState, get, put, modify)
import Control.Monad.State.Strict (StateT, runStateT, lift)
import NumberStore (Number, NumberStore, Store)
import qualified NumberStore

{--
Now, we take the stuff from Step 3 and generalise and reduce the operations to simplify


The features are:
1. a store of numbers that retains its input order
2. the ability to print out the store of numbers, and the summaries
3. the ability to input new numbers into the store
4. the ability to exit the program

--}

-- Effects:
-- data Command -- the type representing the possible commands the program can execute (driving input, pure processing & output of the program)
-- data State s a -- the type of the state effect that keeps the store data that the user's commands use and have an effect on
-- data InputOutput a -- the type representing having an input or output effect on this algebra
-- data Program a -- represents the sequencing and execution effect required to make all other effects take effect

-- We want to say there's some type that we don't know anything about, particularly

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

-- Law 'programSequence/outputText'
-- forall (a :: Text, b :: Text).
--   programSequence (outputText a) (outputText b) = outputText (a <> b)

insertInStore :: MonadState NumberStore m => Number -> m () -- lifted version from Step2 where the State is an effect within the program
insertInStore n = modify (NumberStore.insertInStore n)

getNumberStore :: MonadState NumberStore m => m NumberStore -- a way to get the NumberStore state out of the ambient state effect of a program
getNumberStore = get

-- Law 'insertInStore/getNumberStore'
--   forall (a :: Number).
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
    deriving (Eq, Show)

addNumbersToStore :: [Number] -> Command
addNumbersToStore = AddNumbersToStoreCommand

showSummaries :: Command
showSummaries = ShowSummariesCommand

exit:: Command
exit = ExitCommand

prettyPrintCommand :: Command -> Text
prettyPrintCommand cmd =
  case cmd of
    AddNumbersToStoreCommand ns -> "Add " <> T.intercalate " " (T.pack . filter (/= ' ') . show <$> ns)
    ShowSummariesCommand -> "Show"
    ExitCommand -> "Exit"

prettyPrintHelpTextForCommand :: Command -> Text
prettyPrintHelpTextForCommand (AddNumbersToStoreCommand _) =
  prettyPrintCommand (AddNumbersToStoreCommand [1, 2, 3])
prettyPrintHelpTextForCommand c =
  prettyPrintCommand c

parseCommand :: Text -> Maybe Command -- try to parse a text
parseCommand txt = parsedWords >>= parseCmdFromString
  where
    parsedWords =
      case T.unpack txt of
        "" -> Nothing
        s -> Just s
    parseCmdFromString str =
      case words str of
        "Add" : args ->
          case parseNums args of
            [] -> Nothing
            ns -> Just $ AddNumbersToStoreCommand ns
        ["Show"] -> pure ShowSummariesCommand
        ["Exit"] -> pure ExitCommand
        _ -> Nothing
    parseNums = fmap fst . foldMap (reads :: ReadS Number)

-- Law 'parseCommand/prettyPrintCommand'
-- forAll (a :: Command).
--   parseCommand (prettyPrintCommand a) = Just a

greeting :: Text
greeting = "Welcome to the program.\n"

printGreeting :: InputOutput m => m () -- show a greeting to the program
printGreeting = outputText greeting

textIsWithinText :: Text -> Text -> Bool -- whether the first text is within the second text
textIsWithinText = T.isInfixOf

-- Law 'textIsWithinText/Within'
-- forAll (a :: Text, b :: Text, c :: Text).
--   textIsWithinText b (a <> b <> c) = True

-- note: there's a glaring issue here in that this single law doesn't triangulate the semantics

helpText :: Text -- the text explaining the commands
helpText = "You can use any of the following commands: " <> T.intercalate ", " (prettyPrintHelpTextForCommand <$> commands)
  where
    commands = [addNumbersToStore [1,2,3], showSummaries, exit]

promptText :: Text -- the text that prompts the user to type some input
promptText = "Command: "

-- Law 'helpText'
-- forAll (a :: Command).
--   textIsWithinText (prettyPrintHelpTextForCommand a) helpText = True

printHelpAndPrompt :: InputOutput m => m () -- show a prompt & help explaining the commands that can be typed
printHelpAndPrompt = outputText $ helpText <> "\n" <> promptText

-- Law 'helpTextPrompt'
-- outputText (helpText <> "\n" <> promptText) = printHelpAndPrompt

data IsEndStep = EndProgram | ContinueProgram
  deriving (Eq, Show)

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
  T.intercalate "\n" sentences <> "\n"
  where
    sentences = [mean, sum, product, count]
    mean = "The mean " <> 
      if null ns
      then "cannot be shown (divide by zero)"
      else "is " <> packShow (NumberStore.meanStore ns)
    sum = "The sum is " <> packShow (NumberStore.sumStore ns)
    product = "The product is " <> packShow (NumberStore.productStore ns)
    count = "The count is " <> packShow (NumberStore.lengthStore ns)
    packShow = T.pack . show

interpretCommand :: (InputOutput m, MonadState NumberStore m) => Maybe Command -> m IsEndStep -- execute a command or print an error message and loop back, finally returning whether to end or not
interpretCommand Nothing = do  -- loop back around because there was no parseable command
  outputText "Unrecognised command. Please try again.\n"
  cmd <- promptGetInputAndParseCommand
  interpretCommand cmd
interpretCommand (Just ExitCommand) = pure endProgram
interpretCommand (Just (AddNumbersToStoreCommand ns)) =
  doThenContinueProgram $ sequence_ (insertInStore <$> ns)
interpretCommand (Just ShowSummariesCommand) =
  doThenContinueProgram $ do
    nums <- getNumberStore
    outputText . summarizeStore $ nums

doThenContinueProgram :: InputOutput m => m a -> m IsEndStep
doThenContinueProgram commandAction = commandAction `programSequence` pure continueProgram

-- Law 'interpretCommand/addNumbersToStore'
-- forAll (ns :: [Number]).
--   programSequence (interpretCommand (Just (addNumbersToStore ns))) getNumberStore = programSequence (forM_ ns insertInStore) getNumberStore

-- Law 'interpretCommand/showSummaries'
-- forAll (ns :: [Number]).
--   programSequence (forM_ ns insertInStore) (interpretCommand (Just showSummaries)) = programSequence (forM_ ns insertInStore) (programSequence (programBind getNumberStore (outputText . summarizeStore)) (pureProgram continueProgram))

loopOrEnd :: (InputOutput m, MonadState NumberStore m) => IsEndStep -> m () -- conditionally reloop the program or end it depending on Endstep
loopOrEnd EndProgram = pure ()
loopOrEnd ContinueProgram = mainLoop

-- Law 'loopOrEnd/endProgram'
-- loopOrEnd endProgram = pureProgram ()

-- Law 'loopOrEnd/mainloop'
-- loopOrEnd continueProgram = mainLoop -- note that we can't test this because it's non-deterministic ; it will never end
-- if we wanted to test this, we could, but we'd have to represent continuations in data, and it'd be a lot more work.

-- main_ is the program in the algebra; it will be interpreted into whatever context is needed
main_ :: (InputOutput m, MonadState NumberStore m) => m ()
main_ = printGreeting `programSequence` mainLoop

mainLoop :: (InputOutput m, MonadState NumberStore m) => m ()
mainLoop =
  promptGetInputAndParseCommand
    `programBind` interpretCommand
    `programBind` loopOrEnd

-- This bit is the real effects at the edge: making the program work "for real" (ie in IO).

instance InputOutput IO where
  inputText = T.pack <$> getLine
  outputText = putStr . T.unpack

instance InputOutput Program where
  inputText = lift inputText
  outputText = lift <$> outputText

type Program = StateT NumberStore IO

libMain :: IO ()
libMain = runStateT main_ NumberStore.emptyStore >> pure ()

-- On runnning, noticed some problems, which were extremely easy to fix:
-- √ Entering a non-command causes a non-exhaustive pattern match
-- √ Trying to show when there are no numbers in the store causes a "Ratio has zero denominator" exception
-- √ The help text prints twice

