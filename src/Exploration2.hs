{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Exploration2 where

import           Control.Monad
import           Data.Data
import qualified Data.Map      as M
import           Data.Typeable
import           Generics.SYB

data HoleIdentifier = HId String deriving (Data, Typeable, Eq, Ord)

instance Show HoleIdentifier where
  show (HId str) = concat ["?",str]

data Expr = ExprHole HoleIdentifier -- added for HDD
          | Plus Expr Expr
          | Mult Expr Expr
          | Numb Nat
          deriving (Data, Typeable, Eq, Show)

eH :: String -> Expr
eH str = ExprHole $ HId str

data Nat = NatHole HoleIdentifier
         | Z
         | S Nat
         deriving (Eq, Ord, Show, Data, Typeable)

zero, one, two, three, four, five :: Nat
zero  = Z
one   = S zero
two   = S one
three = S two
four  = S three
five  = S four

e1 :: Expr
e1 = Plus (Numb one) (Mult (Numb two) (Numb three))

ehole :: Expr
ehole = Plus (Numb one) (eH "hole")

{-
All that needs to be done from here(given the definitions above) is a function
to replace a named hole with some term that fits that hole's type in the AST.

This is just a simple SYB application. Now I just need to find the function that
fits this mold.
-}

switchExprHole :: HoleIdentifier -> Expr -> Expr -> Expr
switchExprHole h1 exp' exp@(ExprHole h2)
  | h1 == h2 = exp'
  | otherwise = exp
switchExprHole _ _ exp = exp

replaceExprHole :: HoleIdentifier -> Expr -> Expr -> Expr
replaceExprHole h1 exp' = everywhere (mkT $ switchExprHole h1 exp')


{-
IMPORTANT NOTE: Things get really nice when the holes are the first constructor
                in the ADT. This lets the empty and constrs functions from SYB
                to be used to list legal syntactic hole fits with the nonterminal
                positions just filled with holes.

                The hole ids for the selected expression will need to be changed
                out with some unclaimed name so that it can be selected uniquely.
                This will end up needing to be some kind of environment monad that
                handles the namespace.
-}

switchNatHole :: HoleIdentifier -> Nat -> Nat -> Nat
switchNatHole h1 exp' exp@(NatHole h2)
  | h1 == h2 = exp'
  | otherwise = exp
switchNatHole _ _ exp = exp

replaceNatHole :: HoleIdentifier -> Nat -> Nat -> Nat
replaceNatHole h1 exp' = everywhere $ mkT $ switchNatHole h1 exp'


{-
The repetition of the switch/replace construct makes me think to write a typeclass
for datatypes with holes.


first draft:
-}

class Data a => Holed a where
  getHoleID :: a -> Maybe HoleIdentifier

switchHole :: Holed a => HoleIdentifier -> a -> a -> a
switchHole h1 exp' exp = case getHoleID exp of
                           Just h2 -> if h1 == h2 then exp' else exp
                           Nothing -> exp

replaceHole :: (Holed a) => HoleIdentifier -> a -> a -> a
replaceHole id exp' = everywhere $ mkT $ switchHole id exp'

instance Holed Expr where
  getHoleID (ExprHole id) = Just id
  getHoleID _             = Nothing

instance Holed Nat where
  getHoleID (NatHole id) = Just id
  getHoleID _            = Nothing

{-
Now that this is defined we should be able to use rankntypes to generalize functions
for:  1.) Listing all hole names in unfinished terms including co-induction
      2.) Construct terms monadicly from the ground up using holes.


* (Rough Algorithm for HDD)

1.) list holes in the term(include their type using the functions in Data.Data)
2.) select hole to fill(by name)
3.) from the holes AST type, list the possible hole fits
4.) programmer selects hole fit from list
5.) hole is filled
6.) if holes are still present then goto (1.)

-}
