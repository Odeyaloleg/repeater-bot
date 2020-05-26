module UsersData
  ( UsersData
  ) where

import Data.Map (Map)

type UserId = Int

type IsAskedForRepetitions = Bool

type RepetitionsNum = Int

-- Using polymorphic parameter as an identificator for channels\private chats.
type UsersData a = Map a (IsAskedForRepetitions, RepetitionsNum)
