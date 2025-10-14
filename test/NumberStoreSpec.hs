module NumberStoreSpec where

import Test.Hspec
import Test.QuickCheck
import NumberStore
import Prelude hiding (div)
import qualified Data.Foldable as Fo

numberStoreSpec :: Spec
numberStoreSpec = do
  context "NumberStore" $ do
    it "Law: 'sumStore/empty'; sumStore emptyStore = sumIdentity" $ do
      let prop_sumStore_empty :: Bool
          prop_sumStore_empty = sumStore emptyStore == sumIdentity
      quickCheck prop_sumStore_empty
    it "Law: 'sumStore/singleton'; forall (a :: Number). sumStore (singletonStore a) = a" $ do
      let prop_sumStore_singleton :: Number -> Bool
          prop_sumStore_singleton = \a -> sumStore (singletonStore a) == a
      quickCheck prop_sumStore_singleton
    it "Law: 'singletonStore/insertInStore'; forall (a :: Number). singletonStore a = insertInStore a emptyStore" $ do
      let prop_singletonStore_insertInStore :: Number -> Bool
          prop_singletonStore_insertInStore = \a -> singletonStore a == insertInStore a emptyStore
      quickCheck prop_singletonStore_insertInStore
    it "Law: 'sumStore/induction'; forall (a :: Number, ns :: NumberStore). sumStore (insertInStore a ns) = sumStore (insertInStore (sumStore ns) (singletonStore a))" $ do
      let prop_sumStore_induction :: Number -> NumberStore -> Bool
          prop_sumStore_induction = \a ns -> sumStore (insertInStore a ns) == sumStore (insertInStore (sumStore ns) (singletonStore a))
      quickCheck prop_sumStore_induction
    it "Law: 'sumStore/identity'; forall (ns :: NumberStore). sumStore (insertInStore sumIdentity ns) = sumStore ns" $ do
      let prop_sumStore_identity :: NumberStore -> Bool
          prop_sumStore_identity = \ns -> sumStore (insertInStore sumIdentity ns) == sumStore ns
      quickCheck prop_sumStore_identity
    it "Law: 'productStore/singleton'; forall (a :: Number). productStore (singletonStore a) = a" $ do
      let prop_productStore_singleton :: Number -> Bool
          prop_productStore_singleton = \a -> productStore (singletonStore a) == a
      quickCheck prop_productStore_singleton
    it "Law: 'productStore/induction'; forall (a :: Number, ns :: NumberStore). productStore (insertInStore a ns) = productStore (insertInStore (productStore ns) (singletonStore a))" $ do
      let prop_productStore_induction :: Number -> NumberStore -> Bool
          prop_productStore_induction = \a ns -> productStore (insertInStore a ns) == productStore (insertInStore (productStore ns) (singletonStore a))
      quickCheck prop_productStore_induction
    it "Law: 'productStore/empty'; productStore emptyStore = productIdentity" $ do
      let prop_productStore_empty :: Bool
          prop_productStore_empty = productStore emptyStore == productIdentity
      quickCheck prop_productStore_empty
    it "Law: 'productStore/identity'; forall (ns :: NumberStore). productStore (insertInStore productIdentity ns) = productStore ns" $ do
      let prop_productStore_identity :: NumberStore -> Bool
          prop_productStore_identity = \ns -> productStore (insertInStore productIdentity ns) == productStore ns
      quickCheck prop_productStore_identity
    it "Law: 'productStore/sumIdentity'; forall (ns :: NumberStore). productStore (insertInStore sumIdentity ns) = sumIdentity" $ do
      let prop_productStore_sumIdentity :: NumberStore -> Bool
          prop_productStore_sumIdentity = \ns -> productStore (insertInStore sumIdentity ns) == sumIdentity
      quickCheck prop_productStore_sumIdentity
    it "Law: 'lengthStore/sumIdentity'; lengthStore emptyStore = sumIdentity" $ do
      let prop_lengthStore_sumIdentity :: Bool
          prop_lengthStore_sumIdentity = lengthStore emptyStore == sumIdentity
      quickCheck prop_lengthStore_sumIdentity
    it "Law: 'lengthStore/sumStore'; forall (ns :: NumberStore). lengthStore ns = sumStore (fmap (const productIdentity) ns)" $ do
      let prop_lengthStore_sumStore :: NumberStore -> Bool
          prop_lengthStore_sumStore = \ns -> lengthStore ns == sumStore (fmap (const productIdentity) ns)
      quickCheck prop_lengthStore_sumStore
    it "Law: 'product/div'; forall (a :: Number, b :: Number). b /= sumIdentity => div (productStore (insertInStore a (singletonStore b))) b = a" $ do
      let prop_productStore_div :: Number -> Number -> Property
          prop_productStore_div = \a b -> b /= sumIdentity ==> div (productStore (insertInStore a (singletonStore b))) b == a
      quickCheck prop_productStore_div
    it "Law: 'meanStore/singleton'; forall (a :: Number). meanStore (singletonStore a) = a" $ do
      let prop_meanStore_singleton :: Number -> Bool
          prop_meanStore_singleton = \a -> meanStore (singletonStore a) == a
      quickCheck prop_meanStore_singleton
    it "Law: 'meanStore/div/lengthStore'; forall (ns :: NumberStore). lengthStore ns /= sumIdentity => meanStore ns = div (sumStore ns) (lengthStore ns)" $ do
      let prop_meanStore_div_lengthStore :: NumberStore -> Property
          prop_meanStore_div_lengthStore = \ns -> lengthStore ns /= sumIdentity ==> meanStore ns == div (sumStore ns) (lengthStore ns)
      quickCheck prop_meanStore_div_lengthStore
    it "Law: 'toList/insertInStore'; forall (ns :: NumberStore). toList ns = toList (foldr insertInStore emptyStore ns)" $ do
      let prop_toList_insertInStore :: NumberStore -> Bool
          prop_toList_insertInStore = \ns -> Fo.toList ns == Fo.toList (foldr insertInStore emptyStore ns)
      quickCheck prop_toList_insertInStore

