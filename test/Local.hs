module Main where

import           Protolude

import           Data.Aeson
import qualified Data.List.NonEmpty as NE
import qualified System.Timeout as TO
import           Test.Hspec
import           Test.QuickCheck (Arbitrary, arbitrary, arbitraryBoundedEnum, Gen, property, sized, oneof, resize, getPositive)
import           Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as V
import           Data.Scientific (Scientific, fromFloatDigits)
import qualified Data.HashMap.Strict as HM
import qualified Data.List.NonEmpty as NE

import qualified JSONSchema.Draft4 as D4
import           JSONSchema.Draft4.Schema (Schema(..))
import qualified JSONSchema.Types as JT
import qualified Local.Failure
import qualified Local.Reference
import qualified Local.Validation
import           Shared
import JSONSchema.Validator.Draft4.Array
import JSONSchema.Validator.Draft4.Object
import JSONSchema.Validator.Draft4.Any
import JSONSchema.Validator.Utils

-- Examples
import qualified AlternateSchema
import qualified Simple
import qualified TwoStep

dir :: FilePath
dir = "JSON-Schema-Test-Suite/tests/draft4"

supplementDir :: FilePath
supplementDir = "test/supplement"


instance Arbitrary schema => Arbitrary (Items schema) where
    arbitrary = oneof [ ItemsObject <$> arbitrary
                      , ItemsArray <$> arbitrary
                      ]

instance Arbitrary schema => Arbitrary (AdditionalItems schema) where
    arbitrary = oneof [ AdditionalBool <$> arbitrary
                      , AdditionalObject <$> arbitrary
                      ]

instance Arbitrary Required where
    arbitrary = do
        x  <- arbitraryText -- Guarantee at least one element.
        xs <- (fmap.fmap) T.pack arbitrary
        pure . Required . S.fromList $ x:xs

instance Arbitrary schema => Arbitrary (Dependency schema) where
    arbitrary = oneof [ SchemaDependency <$> arbitrary
                      , PropertyDependency <$> arbitrarySetOfText
                      ]

instance Arbitrary schema => Arbitrary (AdditionalProperties schema) where
    arbitrary = oneof [ AdditionalPropertiesBool <$> arbitrary
                      , AdditionalPropertiesObject <$> arbitrary
                      ]

arbitraryText :: Gen Text
arbitraryText = T.pack <$> arbitrary

arbitraryScientific :: Gen Scientific
arbitraryScientific = (fromFloatDigits :: Double -> Scientific) <$> arbitrary

arbitraryPositiveScientific :: Gen Scientific
arbitraryPositiveScientific = (fromFloatDigits :: Double -> Scientific)
                            . getPositive
                          <$> arbitrary

newtype ArbitraryValue
    = ArbitraryValue { _unArbitraryValue :: Value }
    deriving (Eq, Show)


instance Arbitrary ArbitraryValue where
    arbitrary = ArbitraryValue <$> sized f
      where
        f :: Int -> Gen Value
        f n | n <= 1    = oneof nonRecursive
            | otherwise = oneof $
                  fmap (Array . V.fromList) (traverse (const (f (n `div` 10)))
                    =<< (arbitrary :: Gen [()]))
                : fmap (Object . HM.fromList) (traverse (const (g (n `div` 10)))
                    =<< (arbitrary :: Gen [()]))
                : nonRecursive

        g :: Int -> Gen (Text, Value)
        g n = (,) <$> arbitraryText <*> f n

        nonRecursive :: [Gen Value]
        nonRecursive =
            [ pure Null
            , Bool <$> arbitrary
            , String <$> arbitraryText
            , Number <$> arbitraryScientific
            ]

arbitraryHashMap :: Arbitrary a => Gen (HM.HashMap Text a)
arbitraryHashMap = HM.fromList . fmap (first T.pack) <$> arbitrary

arbitrarySetOfText :: Gen (Set Text)
arbitrarySetOfText = S.fromList . fmap T.pack <$> arbitrary


instance Arbitrary TypeValidator where
    arbitrary = oneof [ TypeValidatorString <$> arbitrary
                      , TypeValidatorArray <$> arbitrary
                      ]
instance Arbitrary SchemaType where
    arbitrary = arbitraryBoundedEnum


instance Arbitrary a => Arbitrary (NonEmpty' a) where
    arbitrary = do
        xs <- arbitrary
        case NE.nonEmpty xs of
            Nothing -> NonEmpty' . pure <$> arbitrary
            Just ne -> pure (NonEmpty' ne)


instance Arbitrary Schema where
    arbitrary = sized f
      where
        maybeGen :: Gen a -> Gen (Maybe a)
        maybeGen a = oneof [pure Nothing, Just <$> a]

        maybeRecurse :: Int -> Gen a -> Gen (Maybe a)
        maybeRecurse n a
            | n < 1     = pure Nothing
            | otherwise = maybeGen $ resize (n `div` 10) a

        f :: Int -> Gen Schema
        f n = do
            a  <- maybeGen arbitraryText
            b  <- maybeGen arbitraryText
            c  <- maybeGen arbitraryText
               -- NOTE: The next two fields are empty to generate cleaner
               -- schemas, but note that this means we don't test the
               -- invertability of these fields.
            d  <- pure Nothing -- _schemaDefinitions
            e  <- pure mempty -- _otherPairs

            f' <- maybeGen arbitraryPositiveScientific
            g  <- maybeGen arbitraryScientific
            h  <- arbitrary
            i  <- maybeGen arbitraryScientific
            j  <- arbitrary

            k  <- maybeGen (getPositive <$> arbitrary)
            l  <- maybeGen (getPositive <$> arbitrary)
            m  <- maybeGen arbitraryText

            n' <- maybeGen (getPositive <$> arbitrary)
            o  <- maybeGen (getPositive <$> arbitrary)
            p  <- arbitrary
            q  <- maybeRecurse n arbitrary
            r  <- maybeRecurse n arbitrary

            s  <- maybeGen (getPositive <$> arbitrary)
            t  <- maybeGen (getPositive <$> arbitrary)
            u  <- maybeGen (S.map T.pack <$> arbitrary)
            v  <- maybeRecurse n arbitraryHashMap
            w  <- maybeRecurse n arbitraryHashMap
            x  <- maybeRecurse n arbitraryHashMap
            y  <- maybeRecurse n arbitrary

            z  <- maybeRecurse n ( fmap _unArbitraryValue . _unNonEmpty'
                               <$> arbitrary)
            a2 <- arbitrary
            b2 <- maybeRecurse n (_unNonEmpty' <$> arbitrary)
            c2 <- maybeRecurse n (_unNonEmpty' <$> arbitrary)
            d2 <- maybeRecurse n (_unNonEmpty' <$> arbitrary)
            e2 <- maybeRecurse n arbitrary
            pure Schema
                { _schemaVersion              = a
                , _schemaId                   = b
                , _schemaRef                  = c
                , _schemaDefinitions          = d
                , _schemaOther                = e

                , _schemaMultipleOf           = f'
                , _schemaMaximum              = g
                , _schemaExclusiveMaximum     = h
                , _schemaMinimum              = i
                , _schemaExclusiveMinimum     = j

                , _schemaMaxLength            = k
                , _schemaMinLength            = l
                , _schemaPattern              = m

                , _schemaMaxItems             = n'
                , _schemaMinItems             = o
                , _schemaUniqueItems          = p
                , _schemaItems                = q
                , _schemaAdditionalItems      = r

                , _schemaMaxProperties        = s
                , _schemaMinProperties        = t
                , _schemaRequired             = u
                , _schemaDependencies         = v
                , _schemaProperties           = w
                , _schemaPatternProperties    = x
                , _schemaAdditionalProperties = y

                , _schemaEnum                 = z
                , _schemaType                 = a2
                , _schemaAllOf                = b2
                , _schemaAnyOf                = c2
                , _schemaOneOf                = d2
                , _schemaNot                  = e2
                }


main :: IO ()
main = do

    -- Language agnostic tests
    ts <- readSchemaTests
              dir
              (\a -> not (isHTTPTest a || skipTest a))

    -- Custom supplements to the language agnostic tests
    supplementTs <- readSchemaTests supplementDir (not . isHTTPTest)

    hspec $ do
        describe "Examples" exampleTests
        describe "QuickCheck" quickCheckTests
        describe "Failure" Local.Failure.spec
        describe "JSONSchema.Validator.Reference" Local.Reference.spec
        describe "Supplementary validation tests written in Haskell"
            Local.Validation.spec

        describe "Supplementary tests written in JSON (using the record based schema)"
            (traverse_ (toTest (fmap timeout . validate)) supplementTs)
        describe "Supplementary tests written in JSON (using the 'Value' based schema)"
            (traverse_ (toTest (fmap timeout . validateExample)) supplementTs)

        describe "Language agnostic tests (using the record based schema)"
            (traverse_ (toTest validate) ts)
        describe "Language agnostic tests (using the 'Value' based schema)"
            (traverse_ (toTest validateExample) ts)
  where
    timeout :: Expectation -> Expectation
    timeout f = do
        res <- TO.timeout 3000000 f
        case res of
            Nothing -> expectationFailure "timeout expired"
            Just a  -> pure a

    validate :: HasCallStack => D4.Schema -> SchemaTestCase -> Expectation
    validate s sc = do
        res <- D4.fetchHTTPAndValidate (D4.SchemaWithURI s Nothing) (_scData sc)
        let failures = case res of
                           Right ()           -> mempty
                           Left (D4.HVData a) -> NE.toList (D4._invalidFailures a)
                           other              -> panic ("Local.validate error: "
                                                       <> show other)
        assertResult sc failures

    validateExample :: HasCallStack => JT.Schema -> SchemaTestCase -> Expectation
    validateExample s sc = do
        res <- AlternateSchema.referencesViaHTTP (D4.SchemaWithURI s Nothing)
        case res of
            Left e          -> panic ("Local.validateExample error: " <> show e)
            Right schemaMap -> do
                let failures = AlternateSchema.validate
                                   schemaMap
                                   (D4.SchemaWithURI s Nothing)
                                   (_scData sc)
                assertResult sc failures

quickCheckTests :: Spec
quickCheckTests =
    it "schemas invert through JSON without change" $ do
        property invertSchema
  where
    invertSchema :: D4.Schema -> Bool
    invertSchema a = Just a == decode (encode a)

exampleTests :: Spec
exampleTests = do
    it "Simple.example compiles successfully" Simple.example
    it "TwoStep.example compiles successfully" TwoStep.example
