{-# LANGUAGE FunctionalDependencies #-}

module Wingfield.Type
  ( View (..)
  , Set (..)
  , Modify (..)
  ) where

--------------------------------------------------------------------------------

class View whole part optic | optic whole -> part
  where
    view :: optic -> whole -> part

--------------------------------------------------------------------------------

{- |

The reason we have both @whole@ and @whole'@ type parameters is because, when
we're setting on a @whole@ that has type parameters, those parameters might
change during the update, leading to a different /whole/ afterward. This is
called a "polymorphic update."

The canonical example for this is setting part of a tuple. In the following
example, we change the first field from 'Integer' to 'String', thus
@whole = (Integer, Char)@ and @whole' = (String, Char)@.

>>> import Wingfield.Tuple
>>> set Tuple_1 "ab" (4, 'c')
("ab",'c')

-}
class Set whole whole' part' optic | optic whole part' -> whole'
  where
    set :: optic -> part' -> whole -> whole'

--------------------------------------------------------------------------------

class Modify whole whole' part part' optic | optic whole part' -> part whole'
  where
    modify :: optic -> (part -> part') -> (whole -> whole')
