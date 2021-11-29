module Edtypes where

data EDirection = ELeft | ERight deriving (Eq, Ord)


newline = '\n'

type EditStopFn = String -> String -> EditStopChoice
data EditStopChoice = EdGo EDirection EditStopFn | EdStop
type IsSelect = Bool


