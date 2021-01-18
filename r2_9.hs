import Data.Maybe

maybeMaybeMaybeMaybeMaybeMaybeMaybeOrNot :: Maybe (Maybe (Maybe (Maybe (Maybe (Maybe a))))) -> a -> a
maybeMaybeMaybeMaybeMaybeMaybeMaybeOrNot c x = fromMaybe x unf where
  helper :: Maybe(Maybe c) -> Maybe c
  helper Nothing = Nothing
  helper (Just c) = c

  unf = (helper . helper. helper. helper . helper) c
