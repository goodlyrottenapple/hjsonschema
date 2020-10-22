module JSONSchema.Validator.Types where

import           Import

-- import           Data.Profunctor (Profunctor(..))

data Validator schema val err = Validator
    { _embedded :: val -> ([schema], [schema])
      -- ^ The first list is embedded schemas that validate the same piece
      -- of data this schema validates (such as schemas embedded in 'allOf').
      -- The second is embedded schemas that validate a subset of that data
      -- (such as the schemas embedded in 'items').
      --
      -- This is done to allow static detection of loops, though this isn't
      -- implemented yet (though the Draft4 code does do live loop detection
      -- during validation).
    , _validate :: val -> Value -> [err]
    } deriving Functor

lmap :: (a -> b) ->  Validator s b c ->  Validator s a c 
lmap f (Validator a b) = Validator (flip (.) f a) (flip (.) f b)

rmap :: (b -> c) ->  Validator s a b -> Validator s a c 
rmap = fmap

dimap :: (a -> b) -> (c -> d) -> Validator s b c -> Validator s a d 
dimap f g = lmap f . rmap g
