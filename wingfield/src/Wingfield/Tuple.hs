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

{- |

>>> modify Tuple_1 (+ 5) (Identity 4)
Identity 9

>>> modify Tuple_1 show (Identity 4)
Identity "4"

-}
instance Modify (Identity a) (Identity b) a b Tuple_1
  where
    modify Tuple_1 f (Identity a) = Identity (f a)

{- |

>>> modify Tuple_1 (+ 5) (3, 4)
(8,4)

>>> modify Tuple_1 show (3, 4)
("3",4)

-}
instance Modify (a, b) (a', b) a a' Tuple_1
  where
    modify Tuple_1 f (a, b) = (f a, b)

instance Modify (a, b, c) (a', b, c) a a' Tuple_1
  where
    modify Tuple_1 f (a, b, c) = (f a, b, c)

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

instance Modify (a, b) (a, b') b b' Tuple_2
  where
    modify Tuple_2 f (a, b) = (a, f b)

instance Modify (a, b, c) (a, b', c) b b' Tuple_2
  where
    modify Tuple_2 f (a, b, c) = (a, f b, c)

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

{- |

>>> modify Tuple_3 (* 2) (1, 2, 3)
(1,2,6)

>>> modify Tuple_3 show (1, 'x', 3)
(1,'x',"3")

-}
instance Modify (a, b, c) (a, b, c') c c' Tuple_3
  where
    modify Tuple_3 f (a, b, c) = (a, b, f c)
