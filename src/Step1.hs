module Step1 where

import Prelude hiding (div)
import qualified Data.Monoid as Mo
import qualified Data.Foldable as Fo

{--
First, I think about what I'm trying to do... and write it down. That is, what functionality we want.

The features are:
1. a store of numbers that retains its input order
2. the ability to print out the store of numbers, and the summaries
3. the ability to input new numbers into the store
4. the ability to exit the program

Next we write what API/interface (opaque types and functions without definitions)
we need for the pure parts of this, and write laws relating each new function as we do,
relating it to previous ones, as it seems appropriate.

One of the goals here is to provide a set of *atomic algebraic constructors* (or a simple core);
to find out what the types mean.

We can start with fuzzy types (and our process here is about iterating back and forth
between the operations and the types and clarifying)

...

Then, we generalise. The problem with this part is that we might not realise what we can generalise to.
(Or put another way, how can we discover what generalisations are avaiable?)

Then we look at the laws and rules and think and see what we can throw away and replace with generic pieces
But where do I go to find this stuff out?
--}

data Store a --{ getStore :: [a] } -- the store of numbers that retains its input order
data Number -- = Rational -- a number
type NumberStore = Store Number
instance Functor Store
instance Applicative Store
instance Foldable Store
instance Semigroup Number
instance Monoid Number

emptyStore :: Store a
emptyStore = undefined -- []

sumStore :: Monoid a => Store a -> a
sumStore = undefined -- Mo.getSum . mconcat . fmap Mo.Sum

sumIdentity :: a
sumIdentity = undefined -- sumStore emptyStore

-- Law: "sumStore/empty"
-- sumStore emptyStore = sumIdentity

singletonStore :: a -> Store a
singletonStore = undefined -- (:[])

-- Law: "sumStore/singleton"
-- forall (a :: Number).
--   sumStore (singletonStore a) = a

insertInStore :: a -> Store a -> Store a
insertInStore = undefined -- (<>) . singletonStore

-- Law: "singleton/insertInStore"
-- forall (a :: Number).
--   singletonStore a = insertInStore a emptyStore

-- Law: "sumStore/induction"
-- forall (a :: Number, ns :: NumberStore).
--   sumStore (insertInStore a ns) = sumStore (insertInStore (sumStore ns) (singletonStore a))

-- Law: "sumStore/identity"
-- forall (ns :: NumberStore).
--   sumStore (insertInStore sumIdentity ns) = sumStore ns

productStore :: Monoid a => Store a -> a
productStore = undefined -- Mo.getProduct . mconcat . fmap Mo.Product

-- Law: "productStore/singleton"
-- forall (a :: Number).
--   productStore (singletonStore a) = a

-- Law: "productStore/induction"
-- forall (a :: Number, ns :: NumberStore).
--   productStore (insertInStore a ns) = productStore (insertInStore (productStore ns) (singletonStore a))

productIdentity :: a
productIdentity = undefined -- productStore emptyStore

-- Law: "productStore/empty"
-- productStore emptyStore = productIdentity

-- Law: "productStore/identity"
-- forall (ns :: NumberStore).
--   productStore (insertInStore productIdentity ns) = productStore ns

-- Law: "productStore/sumIdentity"
-- forall (ns :: NumberStore).
--   productStore (insertInStore sumIdentity ns) = sumIdentity

lengthStore :: Store a -> Number
lengthStore = undefined -- toRational . length

-- Law: "lengthStore/sumIdentity"
--   lengthStore emptyStore = sumIdentity

-- Law: "lengthStore/sumStore"
-- forall (a :: Number, ns :: NumberStore).
--   a = productIdentity =>
--     lengthStore ns = sumStore (fmap (const productIdentity) ns)

div :: Number -> Number -> Number
div = undefined -- (*) . recip

-- Law: "product/div"
-- forall (a :: Number, b :: Number).
--  b /= sumIdentity =>
--   div (productStore (insertInStore a (singletonStore b))) b = a

meanStore :: Store Number -> Number
meanStore = undefined -- div (sumStore store) (lengthStore store)

-- Law: "meanStore/singleton"
-- forall (a :: Number).
--   meanStore (singleton a) = a

-- Law: "meanStore/div/lengthStore"
-- forall (a :: Number, ns :: NumberStore).
--   lengthStore ns /= sumIdentity =>
--     meanStore ns = div (productStore ns) (lengthStore ns)

toList :: Foldable t => t a -> [a]
toList = Fo.toList

-- Law: "toList/insertInStore"
--   forall (ns :: NumberStore).
--     foldr insertInStore emptyStore (toList ns) = ns
