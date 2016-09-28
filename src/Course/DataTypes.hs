{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses, FlexibleInstances#-}
module Course.DataTypes where

import Data.Monoid hiding ((<>))
{- This is the haskell code for purely functional data types -}
-- Queues
-- the queue has two part to record the used part `runF` and unused part `runR`, Queue [1,2,3] [6,5,4] is an example, watch out the right part is reverse

data Queue a = Queue {runF :: [a]
                     ,runR :: [a]}
qhead :: Queue a -> a
qhead (Queue (a : _) _) = a
qhead (Queue [] _) = error "right part is empty"
qtail :: Queue a -> Queue a
qtail (Queue [] _) = error "right part is empty"
qtail (Queue [_] b) = Queue b []
qtail (Queue (_ : a) b) = Queue a b
qsnoc :: Queue a -> a -> Queue a
qsnoc (Queue [] _) x = Queue [x] []
qsnoc (Queue r l) x = Queue r (x : l) -- qsnoc is `cons` spelled backward

-- 2~3 finger tree
-- type Size = Int
-- data Tree v a = Leaf v a | Branch v (Tree v a) (Tree v a) deriving Show
-- tag :: Tree v a -> v
-- tag (Leaf v a) = v
-- tag (Branch v _ _) = v
-- instance Monoid Size where
--   mempty = 0
--   mappend = (+)
-- class Monoid v => Measured a v where
--   measure :: a -> v
-- instance Measured a Size where
--   measure _ = 1
-- instance Measured a v => Measured (Tree v a) v where
--   measure = tag
-- x <> y = tag x `mappend` tag y
-- leaf :: Measured a v => a -> Tree v a
-- leaf a = Leaf (measure a) a
-- branch :: (Monoid v) => Tree v a -> Tree v a -> Tree v a
-- branch x y = Branch (x <> y) x y
