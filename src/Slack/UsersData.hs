module Slack.UsersData
  ( UsersData
  ) where

import qualified Data.Map as M ( Map )

type UserId                = String
type IsAskedForRepetitions = Bool
type RepetitionsNum        = Int

type UsersData = M.Map UserId (IsAskedForRepetitions, RepetitionsNum)