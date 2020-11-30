module Domain.EmailMessageSpec where

-- import Data.Aeson as Aeson
-- import Data.Aeson.QQ
-- import Hedgehog
-- import Hedgehog.Gen as Gen
-- import qualified Hedgehog.Range as Range
import Domain.EmailMessage
import Domain.Gmail.Types
import Test.Tasty.HUnit

-- import Test.Tasty.Hedgehog

test_extractSenderFromHeaders =
  testCase "It extract the From header when exists" $ do
    extractSenderFromHeaders headersWithFrom @?= Just fromHeader

test_extractSenderFromHeaders2 =
  testCase "It returns a Nothing when no From header is present" $ do
    extractSenderFromHeaders headersWithOutFrom @?= Nothing

fromHeader :: GmailMessageHeader
fromHeader = GmailMessageHeader
  { name = "From",
    value = "Salewa <news@cm.salewa.com>"
  }

headersWithOutFrom :: [GmailMessageHeader]
headersWithOutFrom = [GmailMessageHeader
    { name = "Delivered-To"
    , value = "hubmessage.mailing@gmail.com"
  }]

headersWithFrom :: [GmailMessageHeader]
headersWithFrom = headersWithOutFrom ++ [fromHeader]

