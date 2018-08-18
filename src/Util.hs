module Util where

import           Data.Time.Clock
import           Data.Convertible               ( convert )
import           Data.ByteString.Char8          ( pack, ByteString )
import           System.Posix.Types             ( EpochTime )

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither e = maybe (Left e) Right

utcToEpoch :: UTCTime -> EpochTime
utcToEpoch = convert

epochToUTC :: EpochTime -> UTCTime
epochToUTC = convert

epochToBS :: EpochTime -> ByteString
epochToBS t = pack $ show t

utcToBS :: UTCTime -> ByteString
utcToBS = epochToBS . utcToEpoch

days :: NominalDiffTime -> NominalDiffTime
days n = n * 86400

