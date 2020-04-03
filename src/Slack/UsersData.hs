module Slack.UsersData
  ( UsersData
  ) where

import Data.Map ( Map )

type UserId                = String
type IsAskedForRepetitions = Bool
type RepetitionsNum        = Int

type UsersData = Map UserId (IsAskedForRepetitions, RepetitionsNum)