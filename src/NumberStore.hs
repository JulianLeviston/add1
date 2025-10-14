{-# LANGUAGE GADTs #-}
module NumberStore where

import Prelude hiding (div)
import qualified Data.Monoid as Mo
import qualified Data.Foldable as Fo

{--
Next, we take what we consider to be primitives and start to implement
the functionality we want from the laws we defined

Firstly we implement an initial algebra with the primitives in it.

Also, we notice that no observation functions can exist in this data type.
That is, this is the type of the algebra that represents the pure calculation;
The "processing" part of the input/processing/output triad.

Interestingly, we can defer the processing until observation time, or do it at
number insertion time; it's up to us and how we want to implement, and both
implementations should still satisfy the laws.

The 'mean' projection/observation is interesting here because if we implement
calculation at insertion time rather than observation time, we'll have to rely on
the sum and length values to find our mean (average) value.
--}

-- data Store a where
--   EmptyStore :: Store a
--   InsertInStore :: a -> Store a -> Store a

-- emptyStore :: Store a
-- emptyStore = EmptyStore

-- insertInStore :: a -> Store a -> Store a
-- insertInStore = InsertInStore

-- singletonStore :: a -> Store a
-- singletonStore = flip insertInStore emptyStore

-- We notice that most of the utility of this program is in the
-- observations; its output.

-- Now we have to implement our observations in terms of the algebra.
-- Before that, we notice that our particular algebra's "easiest implementation"
-- is actually isomorphic to a list, so we use this for our "obviously correct"
-- implementation.

type Store = []
type Number = Rational
type NumberStore = Store Number

emptyStore :: Store a
emptyStore = []

sumStore :: (Foldable t, Num a) => t a -> a
sumStore = sum

sumIdentity :: Num a => a
sumIdentity = sumStore emptyStore

-- Law: "sumStore/empty"
-- sumStore emptyStore = sumIdentity

singletonStore :: a -> Store a
singletonStore = flip insertInStore emptyStore

-- Law: "sumStore/singleton"
-- forall (a :: Number).
--   sumStore (singletonStore a) = a

insertInStore :: a -> Store a -> Store a
insertInStore = (:)

-- Law: "singleton/insertInStore"
-- forall (a :: Number).
--   singletonStore a = insertInStore a emptyStore

-- Law: "sumStore/induction"
-- forall (a :: Number, ns :: NumberStore).
--   sumStore (insertInStore a ns) = sumStore (insertInStore (sumStore ns) (singletonStore a))

-- Law: "sumStore/identity"
-- forall (ns :: NumberStore).
--   sumStore (insertInStore sumIdentity ns) = sumStore ns

productStore :: (Foldable t, Num a) => t a -> a
productStore = product

-- Law: "productStore/singleton"
-- forall (a :: Number).
--   productStore (singletonStore a) = a

-- Law: "productStore/induction"
-- forall (a :: Number, ns :: NumberStore).
--   productStore (insertInStore a ns) = productStore (insertInStore (productStore ns) (singletonStore a))

productIdentity :: Num a => a
productIdentity = productStore emptyStore

-- Law: "productStore/empty"
-- productStore emptyStore = productIdentity

-- Law: "productStore/identity"
-- forall (ns :: NumberStore).
--   productStore (insertInStore productIdentity ns) = productStore ns

-- Law: "productStore/sumIdentity"
-- forall (ns :: NumberStore).
--   productStore (insertInStore sumIdentity ns) = sumIdentity

lengthStore :: Foldable t => t a -> Number
lengthStore = toRational . length

-- Law: "lengthStore/sumIdentity"
--   lengthStore emptyStore = sumIdentity

-- Law: "lengthStore/sumStore"
-- forall (ns :: NumberStore).
--   lengthStore ns = sumStore (fmap (const productIdentity) ns)

div :: Number -> Number -> Number
div x y = x * recip y

-- Law: "product/div"
-- forall (a :: Number, b :: Number).
--  b /= sumIdentity =>
--   div (productStore (insertInStore a (singletonStore b))) b = a

meanStore :: Foldable t => t Number -> Number
meanStore store = div (sumStore store) (lengthStore store)

-- Law: "meanStore/singleton"
-- forall (a :: Number).
--   meanStore (singletonStore a) = a

-- Law: "meanStore/div/lengthStore"
-- forall (a :: Number, ns :: NumberStore).
--   lengthStore ns /= sumIdentity =>
--     meanStore ns = div (sumStore ns) (lengthStore ns)

-- toList comes from Foldable, if we need it

-- Law: "toList/insertInStore"
--   forall (ns :: NumberStore).
--     toList ns = toList (foldr insertInStore emptyStore ns)

-- The process of further generalisation continued while I checked the implementations
-- in the REPL and realised that many of them weren't general enough and could be
-- pushed higher.

-- At this point, we have a pretty general leak-free abstraction.

-- Next we need to implement an "Arbitrary" typeclass instance (for generating random values for the prop tests)
-- and then automatically generate tests for our property checks using quickspec.
-- We decided not to do that for this one simply because it only actually has two constructors in the algebra,
-- and the entire algebra is a list homomorphism, so it's already correct. We might do more for the console algebra.
-- Tho these will likely simply be the Monad laws roughly homomorphic to the State monad, which then
-- get used "in production" on IO.

-- We also have the outer layer of our program to write now we have our (pure "processing")
-- mathematical middle library layer. This will also be written in an algebra. Once we've written
-- tests for that too, then we'll be done.
