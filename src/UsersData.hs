module UsersData
  ( UsersData
  ) where

import Data.Map (Map)

type UserId = Int

type IsAskedForRepetitions = Bool

type RepetitionsNum = Int

-- Maybe newtype is better?
type UsersData a = Map a (IsAskedForRepetitions, RepetitionsNum)
