module Util (spanM, flattenOn) where

import Data.Bifunctor (Bifunctor (first, second))
import Data.Bool (bool)

spanM :: (Monad m) => (a -> m Bool) -> [a] -> m ([a], [a])
spanM _ [] = pure ([], [])
spanM f (x : xs) = do
  p <- f x
  bool second first p (x :) <$> spanM f xs

flattenOn :: (a -> Bool) -> [a] -> [[a]]
flattenOn _ [] = []
flattenOn f (r : remaining) =
  let (grp, rest) = break f remaining
   in (r : grp) : flattenOn f rest