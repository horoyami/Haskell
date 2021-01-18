import Data.Maybe

maybeMaybeMaybeMaybeMaybeMaybeMaybeOrNot :: Maybe (Maybe (Maybe (Maybe (Maybe (Maybe a))))) -> a -> a
maybeMaybeMaybeMaybeMaybeMaybeMaybeOrNot c x = fromMaybe x unf where
  helper :: Maybe(Maybe d) -> Maybe d
  helper Nothing = Nothing
  helper (Just d) = d

  unf = (helper . helper. helper. helper . helper) c
