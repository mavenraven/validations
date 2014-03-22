module Validations.Types.Checker
    ( Checker
    , MonadicChecker
    )
    where

type Checker error a b = a -> Either error b
type MonadicChecker error monad a b = a -> monad (Either error b)
