module SequentialPatch where

import Control.Applicative ((<|>))
import Data.Maybe
import Data.Patch

newtype SequentialPatch a = SequentialPatch [a]

instance Semigroup (SequentialPatch a) where
  SequentialPatch a <> SequentialPatch b = SequentialPatch (a <> b)

singleton :: a -> SequentialPatch a
singleton a = SequentialPatch [a]

instance (Patch a) => Patch (SequentialPatch a) where
  type PatchTarget (SequentialPatch a) = PatchTarget a
  apply (SequentialPatch []) target = Nothing
  apply (SequentialPatch (p : ps)) target =
    let target' = apply (SequentialPatch ps) target
     in apply p (fromMaybe target target') <|> target'