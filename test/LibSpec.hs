{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverloadedStrings #-}
module LibSpec where

import Test.Hspec
import Lib
import Test.QuickCheck
import Control.Monad (forM_)
import Control.Monad.Identity (Identity)
import Control.Monad.State.Strict (StateT, runStateT, State, get, put, runState, lift)
import NumberStore (NumberStore, singletonStore, Number, lengthStore, sumIdentity)
import Data.Text (Text)
import qualified Data.Text as T
import Debug.Trace (trace)

type TestProgram = StateT NumberStore (State ([Text], [Text])) -- input, output

instance Show a => Show (TestProgram a) where
  show tp = show (runState (runStateT tp initNumberStore) initInputOutputState)
    where initNumberStore = []
          initInputOutputState = ([], [])


-- Define what "Program equality" means, within our testing context
-- We join all the input text as one and all the output text as one, and we also reverse them
-- before this, because we are using singly linked lists as stacks (with cons/prepend as push).
-- Would probably be good to adjust this sometime to make it a bit nicer, but for now this is fine.
-- For the state, we just use equality on the observed/produced part (LHS), not the internal part.
instance Eq a => Eq (TestProgram a) where
  tp1 == tp2 = compactingRunStates tp1 == compactingRunStates tp2
    where compactingRunStates tp = compactAndPrepare $ runState (runStateT tp initNumberStore) initInputOutputState
          initNumberStore = []
          initInputOutputState = ([], [])
          compactAndPrepare ((producedState, _), (xs,ys)) = (producedState, (mconcat (reverse xs), mconcat (reverse ys)))

-- implement our input & output class with respect to our testing program,
-- which is using the above alias to a pair of lists of texts to represent console input and output
instance InputOutput TestProgram where
  inputText = do
    (iss,os) <- lift get
    case iss of
      (i:is) -> lift (put (is, os)) >> pure i
      is -> lift (put (is, os)) >> pure ""
  outputText txt = do
    (is,os) <- lift get
    lift (put (is, txt:os))
    pure ()

instance Arbitrary Text where
  arbitrary = T.pack <$> (arbitrary :: Gen String)

instance Arbitrary Command where
  arbitrary =
    oneof [pure showSummaries, pure exit, addNumbersToStore <$> (listOf1 arbitrary :: Gen [Number])]

libSpec :: Spec
libSpec = do
  context "Lib" $ do
    it "Law 'programBind/inputText/outputText'; forall (a :: Text). programBind (overrideInput a inputText) outputText = outputText a" $ do
      let prop_programBind_inputText_outputText :: Text -> Bool
          prop_programBind_inputText_outputText = \a -> programBind (overrideInput a (inputText :: TestProgram Text)) outputText == outputText a
      quickCheck prop_programBind_inputText_outputText
    it "Law 'programSequence/outputText'; forall (a :: Text, b :: Text). programSequence (outputText a) (outputText b) = outputText (a <> b)" $ do
      let prop_programSequence_outputText :: Text -> Text -> Bool
          prop_programSequence_outputText = \a b -> programSequence (outputText a) (outputText b) == (outputText (a <> b) :: TestProgram ())
      quickCheck prop_programSequence_outputText
    it "Law 'insertInStore/getNumberStore'; forall (a :: Number). programSequence (insertInStore a) getNumberStore = pureProgram (singletonStore a)" $ do
      let prop_insertInStore_getNumberStore :: Number -> Bool
          prop_insertInStore_getNumberStore = \a -> programSequence (insertInStore a) getNumberStore == (pureProgram (singletonStore a) :: TestProgram NumberStore)
      quickCheck prop_insertInStore_getNumberStore
    it "Law 'programMap'; forall (a :: Text, b :: Text). programMap (const b) (pureProgram a) = pureProgram b" $ do
      let prop_programMap :: Text -> Text -> Bool
          prop_programMap = \a b -> programMap (const b) (pureProgram a) == (pureProgram b :: TestProgram Text)
      quickCheck prop_programMap
    it "Law 'parseCommand/prettyPrintCommand'; forAll (a :: Command). parseCommand (prettyPrintCommand a) = Just a" $ do
      let prop_parseCommand_prettyPrintCommand :: Command -> Bool
          prop_parseCommand_prettyPrintCommand = \a -> parseCommand (prettyPrintCommand a) == Just a
      quickCheck prop_parseCommand_prettyPrintCommand
    it "Law 'textIsWithinText'; forAll (a :: Text, b :: Text, c :: Text). textIsWithinText b (a <> b <> c) = True" $ do
      let prop_textIsWithinText :: Text -> Text -> Text -> Bool
          prop_textIsWithinText = \a b c -> textIsWithinText b (a <> b <> c)
      quickCheck prop_textIsWithinText
    it "Law 'helpText'; forAll (a :: Command). textIsWithinText (prettyPrintHelpTextForCommand a) helpText = True" $ do
      let prop_helpText :: Command -> Bool
          prop_helpText = \a -> textIsWithinText (prettyPrintHelpTextForCommand a) helpText
      quickCheck prop_helpText
    it "Law 'helpTextPrompt'; outputText (helpText <> \"\n\" <> promptText) = printHelpAndPrompt" $ do
      let prop_helpTextPrompt :: Bool
          prop_helpTextPrompt = outputText (helpText <> "\n" <> promptText) == (printHelpAndPrompt :: TestProgram ())
      quickCheck prop_helpTextPrompt
    it "Law 'interpretCommand/addNumbersToStore'; forAll (ns :: NumberStore). programSequence (interpretCommand (Just (addNumbersToStore ns))) getNumberStore = programSequence (forM_ ns insertInStore) getNumberStore" $ do
      let prop_interpretCommand_addNumbersToStore :: NumberStore -> Bool
          prop_interpretCommand_addNumbersToStore = \ns -> programSequence (interpretCommand (Just (addNumbersToStore ns))) getNumberStore == (programSequence (forM_ ns insertInStore) getNumberStore :: TestProgram NumberStore)
      quickCheck prop_interpretCommand_addNumbersToStore
    it "Law 'interpretCommand/showSummaries'; forAll (ns :: [Number]). programSequence (forM_ ns insertInStore) (interpretCommand (Just showSummaries)) = programSequence (forM_ ns insertInStore) (programSequence (programBind getNumberStore (outputText . summarizeStore)) (pureProgram continueProgram))" $ do
      let prop_interpretCommand_showSummaries :: NumberStore -> Property
          prop_interpretCommand_showSummaries = \ns -> lengthStore ns /= sumIdentity ==> programSequence (forM_ ns insertInStore) (interpretCommand (Just showSummaries)) == (programSequence (forM_ ns insertInStore) (programSequence (programBind getNumberStore (outputText . summarizeStore)) (pureProgram continueProgram)) :: TestProgram IsEndStep)
      quickCheck prop_interpretCommand_showSummaries
    it "Law 'loopOrEnd/endProgram'; loopOrEnd endProgram = pureProgram ()" $ do
      let prop_loopOrEnd_endProgram :: Bool
          prop_loopOrEnd_endProgram = loopOrEnd endProgram == (pureProgram () :: TestProgram ())
      quickCheck prop_loopOrEnd_endProgram


      