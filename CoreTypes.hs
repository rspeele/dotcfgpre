module CoreTypes
    ( ByteString
    , KeyName
    , AliasName
    ) where
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B

type KeyName = ByteString
type AliasName = ByteString

