{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Wingfield.Tuple
  ( Tuple_1 (..)
  , Tuple_2 (..)
  , Tuple_3 (..)
  ) where

import Wingfield.Type

import Data.Functor.Identity (Identity (..))

--------------------------------------------------------------------------------

data Tuple_1 = Tuple_1

instance View (Identity a) a Tuple_1
  where
    view Tuple_1 (Identity a) = a

instance View (a, b) a Tuple_1
  where
    view Tuple_1 (a, _b) = a

instance View (a, b, c) a Tuple_1
  where
    view Tuple_1 (a, _b, _c) = a

{- |

>>> set Tuple_1 5 (Identity 4)
Identity 5

>>> set Tuple_1 "ab" (Identity 4)
Identity "ab"

-}
instance Set (Identity a) (Identity a') a' Tuple_1
  where
    set Tuple_1 a' _a = Identity a'

{- |

>>> set Tuple_1 6 (3, 4)
(6,4)

>>> set Tuple_1 "ab" (4, 'c')
("ab",'c')

-}
instance Set (a, b) (a', b) a' Tuple_1
  where
    set Tuple_1 a' (_a, b) = (a', b)

instance Set (a, b, c) (a', b, c) a' Tuple_1
  where
    set Tuple_1 a' (_a, b, c) = (a', b, c)

--------------------------------------------------------------------------------

data Tuple_2 = Tuple_2

instance View (a, b) b Tuple_2
  where
    view Tuple_2 (_a, b) = b

instance View (a, b, c) b Tuple_2
  where
    view Tuple_2 (_a, b, _c) = b

instance Set (a, b) (a, b') b' Tuple_2
  where
    set Tuple_2 b' (a, _b) = (a, b')

instance Set (a, b, c) (a, b', c) b' Tuple_2
  where
    set Tuple_2 b' (a, _b, c) = (a, b', c)

--------------------------------------------------------------------------------

data Tuple_3 = Tuple_3

{- |

>>> view Tuple_3 (4, 5, 6)
6

-}
instance View (a, b, c) c Tuple_3
  where
    view Tuple_3 (_a, _b, c) = c

instance Set (a, b, c) (a, b, c') c' Tuple_3
  where
    set Tuple_3 c' (a, b, _c) = (a, b, c')
