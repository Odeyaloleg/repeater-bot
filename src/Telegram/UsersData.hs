module Telegram.UsersData
  ( UsersData ( UsersData, accessUsersData )
  ) where

import qualified Data.Map as M ( Map )

type UserId                = Int
type IsAskedForRepetitions = Bool
type RepetitionsNum        = Int

data UsersData = UsersData { accessUsersData :: (M.Map UserId (IsAskedForRepetitions, RepetitionsNum)) }