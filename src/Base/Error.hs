module Base.Error where

data Err a = AnyErr
   | SpecificErr a
   deriving (Show, Eq) 