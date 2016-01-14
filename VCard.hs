module VCard where
import Control.Monad

data Context = Home | Mobile | Business
    deriving (Eq, Show)

type Phone = String

onePersonalPhone :: [(Context, Phone)] -> Maybe Phone
onePersonalPhone ps = case lookup Home ps of
                        Nothing -> lookup Business ps
                        Just n -> Just n

allBusinessPhones :: [(Context, Phone)] -> [Phone]
allBusinessPhones ps = map snd numbers
  where numbers = case filter(contextIs Business) ps of
                        [] -> filter (contextIs Mobile) ps
                        ns -> ns

contextIs a (b, _) = a == b

oneBusinessPhone :: [(Context, Phone)] -> Maybe Phone
oneBusinessPhone ps = lookup Business ps `mplus` lookup Mobile ps

allPersonalPhones :: [(Context, Phone)] -> [Phone]
allPersonalPhones ps = map snd $ filter (contextIs Home) ps `mplus`
                                 filter (contextIs Mobile) ps

x `zeroMod` n = guard ((x `mod` n) == 0) >> return x
