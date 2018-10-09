{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Exploration1 where

import Data.Typeable
import Data.Data
import Control.Monad
import qualified Data.Map as M


data Nat = Succ Nat | Zero deriving (Show, Read, Typeable, Data)

zero = Zero
one  = Succ zero
two  = Succ one
three = Succ two

data List a = Cons a (List a) | Empty deriving (Show, Read, Typeable, Data)


l1 = Cons "x" (Cons "y" (Cons "z" Empty))

l2 = Cons one (Cons two (Cons three Empty))


construct :: DataType -> IO ()
construct dt = do
  putStrLn $ "Constructing " ++ dataTypeName dt
  printConstructors dt


printConstructors :: DataType -> IO ()
printConstructors dt = do
  let cns = map typeOf $ dataTypeConstrs dt
  putStrLn $ "Constructors for " ++ dataTypeName dt ++ " are: "
  forM_ cns print

numbering :: [a] -> [(Int, a)]
numbering = zip [1..]


data Node a t = Terminal a
              | Nonterminal a (Arguments a t)
              | Hole t
              deriving Show

nodeName :: Node a t -> a
nodeName (Terminal n) = n
nodeName (Nonterminal n _) = n
nodeName _ = error "Holes dont have grammar names"

plusExample :: Node String String
plusExample = Nonterminal "Plus" $ toArgs [
  Terminal "Num 5",
  Hole "Expr"
                                 ]


newtype Arguments a t = Args {unArgs :: M.Map ArgID (Node a t)} deriving (Show)

newtype ArgID = AId Int deriving (Show, Eq, Ord)

aIDs :: [ArgID]
aIDs = map AId [1..]

toArgs :: [Node a t] -> Arguments a t
toArgs xs = Args $ M.fromList (zip aIDs xs)

-- rewrites takes a type from an open and returns nodes that could replace it
-- principally these should be either terminals or non-terminals with opens inside them
newtype Rewrites a t = RW {unRW :: M.Map t [Node a t]}

getRewritesFor :: (Ord t) => Rewrites a t -> t -> [Node a t]
getRewritesFor (RW rws) x = rws M.! x

getByName :: (Ord t, Eq a) => Rewrites a t -> t -> a -> Node a t
getByName rw t n = find n $ getRewritesFor rw t
  where find :: (Eq a) => a -> [Node a t] -> Node a t
        find n = foldr (\node b -> if nodeName node == n then node else b) undefined


refine :: (Show a, Show t, Ord t, Read a, Eq a) => Rewrites a t -> Node a t -> IO (Node a t)
refine _ (Terminal x) = return $ Terminal x
refine r (Nonterminal y args) = do
  -- this must refine each arg if they are holes or refine the nonterms if they have hole etc
  xs <- mapM (refine r) (unArgs args)
  return $ (Nonterminal y (Args xs))
refine r (Hole holeType) = do
  putStrLn "This hole may be filled with: "
  printOptions (getRewritesFor r holeType)
  getChoice r holeType

getChoice :: (Read a, Ord t, Eq a) => Rewrites a t -> t -> IO (Node a t)
getChoice r t = do
  putStrLn "Which rewrite should be performed?"
  id <- readLn
  return $ getByName r t id

printOptions :: (Show a, Show t) => [Node a t] -> IO ()
printOptions = mapM_ print

-- * Arithmetic Examples

test1 :: Node String String
test1 = Nonterminal "Plus" (toArgs [(Hole "Expr"), (Hole "Expr")])

rws :: Rewrites String String
rws = RW $ M.fromList [rw1, rw2]

rw1 :: (String, [Node String String])
rw1 = ("Expr", [ Nonterminal "Plus" twoExp
               , Nonterminal "Minus" twoExp
               , Nonterminal "Mult" twoExp
               , Nonterminal "Num" (toArgs [Hole "Nat"])
               ])
  where twoExp :: Arguments String String
        twoExp = toArgs [oExp, oExp]
        oExp = Hole "Expr"


rw2 :: (String, [Node String String])
rw2 = ("Nat", [Terminal "Zero", Nonterminal "Succ" (toArgs [Hole "Nat"])])
