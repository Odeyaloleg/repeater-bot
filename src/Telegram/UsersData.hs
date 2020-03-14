module Telegram.UsersData
  ( UsersData ( UsersData )
  ) where

import qualified Data.Map as M ( Map )

type UserId                = Int
type IsAskedForRepetitions = Bool
type RepetitionsNum        = Int

data UsersData = UsersData (M.Map UserId (IsAskedForRepetitions, RepetitionsNum))