module Domain.EmailMessage where

import qualified Domain.Gmail.Types as GMT
import GHC.Generics

data EmailMessageSender = EamilMessageSender
  { name :: String,
    email :: String,
    mxRecord :: Maybe String
  }
  deriving (Generic, Show)

extractSenderFromHeaders :: [GMT.GmailMessageHeader] -> Maybe GMT.GmailMessageHeader
extractSenderFromHeaders headers = case filtered of
  (x : _) -> Just x
  [] -> Nothing
  where
    filtered = filter (\header -> "From" == GMT.name header) headers

-- case filteredHeaders of
--     (x:xs) -> return Just x
--     _ -> return Nothing

extractSenderNameFromSender :: String -> String
extractSenderNameFromSender = undefined

extractSenderEmailFromSender :: String -> String
extractSenderEmailFromSender = undefined

extractSenderMxRecordFromSender :: String -> String
extractSenderMxRecordFromSender = undefined
