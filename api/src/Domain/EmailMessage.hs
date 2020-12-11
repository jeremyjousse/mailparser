module Domain.EmailMessage where

import qualified Domain.Gmail.Types as GMT
import GHC.Generics

data EmailMessageSender = EamilMessageSender
  { name :: String,
    email :: String,
    mxRecord :: Maybe String
  }
  deriving (Generic, Show)

extractSenderFromHeader :: [GMT.GmailMessageHeader] -> Maybe GMT.GmailMessageHeader
extractSenderFromHeader headers = case filtered of
  (x : _) -> Just x
  [] -> Nothing
  where
    filtered = filter (\header -> "From" == GMT.name header) headers

extractSenderNameFromSender :: String -> String
extractSenderNameFromSender = undefined

extractSenderEmailFromSender :: String -> String
extractSenderEmailFromSender = undefined

-- Use the hackage lib.
extractSenderMxRecordFromSender :: String -> String
extractSenderMxRecordFromSender = undefined
