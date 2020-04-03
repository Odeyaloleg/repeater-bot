module UsersData
  ( UsersData
  ) where

import Data.Map ( Map )

type UserId                = Int
type IsAskedForRepetitions = Bool
type RepetitionsNum        = Int

type UsersData = Map UserId (IsAskedForRepetitions, RepetitionsNum)