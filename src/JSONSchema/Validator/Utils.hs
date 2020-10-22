module JSONSchema.Validator.Utils where

import           Import

import           Control.Monad (fail)
import qualified Data.HashMap.Strict as HM
import qualified Data.List.NonEmpty as NE
import           Data.Scientific (Scientific, fromFloatDigits)
import           Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as V

--------------------------------------------------
-- * QuickCheck
--------------------------------------------------

newtype NonEmpty' a = NonEmpty' { _unNonEmpty' :: NonEmpty a }

instance FromJSON a => FromJSON (NonEmpty' a) where
    parseJSON v = do
        xs <- parseJSON v
        case NE.nonEmpty xs of
            Nothing -> fail "Must have at least one item."
            Just ne -> pure (NonEmpty' ne)

instance ToJSON a => ToJSON (NonEmpty' a) where
    toJSON = toJSON . NE.toList . _unNonEmpty'


--------------------------------------------------
-- * allUniqueValues
--------------------------------------------------

allUniqueValues :: (Foldable f, Functor f) => f Value -> Bool
allUniqueValues = allUnique . fmap OrdValue

allUnique :: (Foldable f, Ord a) => f a -> Bool
allUnique xs = S.size (S.fromList (toList xs)) == length xs

-- | OrdValue's Ord instance needs benchmarking, but it allows us to
-- use our 'allUnique' function instead of O(n^2) nub, so it's probably
-- worth it.
newtype OrdValue = OrdValue { _unOrdValue :: Value } deriving Eq

instance Ord OrdValue where
    (OrdValue Null) `compare` (OrdValue Null) = EQ
    (OrdValue Null) `compare` _               = LT
    _               `compare` (OrdValue Null) = GT

    (OrdValue (Bool x)) `compare` (OrdValue (Bool y)) = x `compare` y
    (OrdValue (Bool _)) `compare` _                   = LT
    _                   `compare` (OrdValue (Bool _)) = GT

    (OrdValue (Number x)) `compare` (OrdValue (Number y)) = x `compare` y
    (OrdValue (Number _)) `compare` _                     = LT
    _                     `compare` (OrdValue (Number _)) = GT

    (OrdValue (String x)) `compare` (OrdValue (String y)) = x `compare` y
    (OrdValue (String _)) `compare` _                     = LT
    _                     `compare` (OrdValue (String _)) = GT

    (OrdValue (Array xs)) `compare` (OrdValue (Array ys)) =
        (OrdValue <$> xs) `compare` (OrdValue <$> ys)
    (OrdValue (Array _))  `compare` _                     = LT
    _                     `compare` (OrdValue (Array _))  = GT

    (OrdValue (Object x)) `compare` (OrdValue (Object y)) =
        HM.toList (OrdValue <$> x) `compare` HM.toList (OrdValue <$> y)
