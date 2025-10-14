module Step2point5 where

import Data.Text

{--
Now, again, continue to think about what I'm trying to do... and write it down. That is, what functionality we want.


The features are:
1. a store of numbers that retains its input order
2. the ability to print out the store of numbers, and the summaries
3. the ability to input new numbers into the store
4. the ability to exit the program

So we have the "algebra" for the calculation of the summaries and the data structure
of the numbers sorted out. Next we need the shell command algebra.

Again, next we write what API/interface (opaque types and functions without definitions)
we need for the parts of this that represent the shell commands as an algebra,
and write laws relating each new function as we do, relating each to previous ones, as it seems appropriate.

One of the goals here is to provide a set of *atomic algebraic constructors* (or a simple core);
to find out what the types mean.

We can start with fuzzy types (and our process here is about iterating back and forth
between the operations and the types and clarifying)

--}

-- data Command -- the type representing the possible commands the program can execute (driving input, pure processing & output of the program)
-- data State s a -- the type of the state effect that keeps the store data that the user's commands use and have effect on
-- data InputOutput a -- the type representing having an input or output effect on this algebra
-- data Program a -- represents the sequencing and execution effect required to make all other effects take effect

-- We want to say there's some type that we don't know anything, particularly

-- type Program a

-- inputText :: Program Text -- a way to get text into this algebra from outside of it

-- outputText :: Text -> Program () -- a way to get text to outside of this algebra
 
-- overrideInput :: Text -> Program a -> Program Text -- a way to control what is 'inside' a program context

-- programBind :: Program a -> (a -> Program b) -> Program b -- a way to bind a function on programs into another program

-- pureProgram :: a -> Program a

-- Law 'programBind/inputText/outputText'
-- forall (a :: Text).
--   programBind (overrideInput a inputText) outputText = outputText a

-- programSequence :: Program a -> Program b -> Program b -- way to sequence the effects of two programs, throwing away the first result

-- Law 'programSequence'
-- forall (a :: Text, b :: Text).
--   programSequence (outputText a) (outputText b) = outputText (a <> b)

-- ... we have all the store functions where the store is "lifted" into the Program type so we need a lifting function! (liftM is that function)
-- and then the projections out of that algebra are also lifted across the Program type (fmap the relevant projection functions across the store)

-- insertInStore :: Number -> Program () -- lifted version from Step2 where the State is an effect within the program

-- getNumberStore :: Program NumberStore -- a way to get the NumberStore state out of the ambient state effect of a program

-- Law 'insertInStore/getNumberStore'
--   forall (a :: Text).
--     programSequence (insertInStore a) getNumberStore = pureProgram (singletonStore a)

-- we can then apply any of the functions from Step2 using a program map function to get the projections ("lifted" algebraic operations)

-- programMap :: (a -> b) -> Program a -> Program b

-- Law 'programMap'
-- forall (a :: Text, b :: Text).
--   programMap (const b) (pureProgram a) = pureProgram b

-- now we also want an algebra of commands, and a parser of commands, and an interpreter of commands

-- then we want to compose our program together to the main function and have a law about that


-- TODO: write more laws for the below, introducing them and connecting them up to existing laws
-- TODO: write an algebra for Command, and hook that up to this, too (should be small)
-- TODO: once those are finished, write an initial algebra, abstract then reduce the algebra
--- ... (to a core + combinators that use the core)
-- ... then; use QuickSpec to find more laws and build homomorphisms
--
-- But don't we also need to have the minimal set of operations defined?
-- Then we have finished the program, so run it :)

-- commands don't have any meaning until they're interpreted into programs

-- addNumbersToStore :: [Number] -> Command -- add a sequence of numbers to the store (with spaces between)

-- showSummaries :: Command -- print out the summaries of the store

-- exit :: Command

-- prettyPrintCommand :: Command -> Text -- the inverse of parsing a command

-- parseCommand :: Text -> Maybe Command -- try to parse a text

-- Law 'parseCommand/prettyPrintCommand'
-- forAll (a :: Command).
--   parseCommand (prettyPrintCommand a) = Just a

-- printGreeting :: Program () -- show a greeting to the program

-- textIsWithinText :: Text -> Text -> Bool -- whether the first text is within the second text

-- Law 'TextIsWithinText'
-- forAll (a :: Text, b :: Text, c :: Text).
--   textIsWithinText b (a <> b <> c) = True

-- helpText :: Text -- the text explaining the commands

-- promptText :: Text -- the text that prompts the user to type some input

-- Law 'helpText'
-- forAll (a :: Command).
--   textIsWithinText (prettyPrintCommand a) helpText = True

-- printHelpAndPrompt :: Program () -- show a prompt & help explaining the commands that can be typed

-- Law 'helpTextPrompt'
-- outputText (helpText <> "\n" <> promptText) = printHelpAndPrompt

-- interpretCommand :: Maybe Command -> Program Endstep -- execute a command or print an error message and loop back, finally returning whether to end or not

-- endProgram :: EndStep
-- continueProgram :: EndStep

-- Law 'interpretCommand/addNumbersToStore'
-- forAll (ns :: [Number]).
--   interpretCommand (Just (addNumbersToStore ns)) = foldr insertInStore (pureProgram endProgram) ns

-- So I got to this point and I started to want to have primitives for the effects individually.
-- and then build everything else off of that

-- Law 'interpretCommand/showSummaries'
-- forAll (ns :: [Number]).
--   interpretCommand (Just (addNumbersToStore ns)) = foldr insertInStore (pureProgram endProgram) ns

-- ?? finish this ??

-- loopOrEnd :: EndStep -> Program () -- conditionally reloop the program or end it depending on Endstep

-- mainLoop :: Program () -- retrieves a line of text and tries to parse it into a command, looping if it can't until it can

-- Law 'loopOrEnd/endProgram'
--   loopOrEnd endProgram = pureProgram endProgram

-- Law 'loopOrEnd/mainloop'
--   loopOrEnd continueProgram = mainLoop


-- main_ will be interpreted into whatever context is needed

-- main_ :: Program ()
-- main_ = greeting `programSequence` mainLoop

-- mainLoop :: Program ()
-- mainLoop =
--   printPrompt `sequenceProgram`
--     inputText
--       `bindProgram` parseAsCommand
--       `bindProgram` interpretCommandToProgram
--       `bindProgram` loopOrEnd

-- then we have a thing to translate to IO ()
