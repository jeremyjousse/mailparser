module Domain.EmailMessageSpec where

import Domain.EmailMessage
import Domain.Gmail.Types
import Generator.QuickCheckGenerator
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

test_extractSenderFromHeaders =
  testCase "It extract the From header when exists" $ do
    extractSenderFromHeader headersWithFrom @?= Just fromHeader

test_extractSenderFromHeadersUsingQuickCheck =
  testProperty "It extract the From header from QuickCheck gen when exists" $ \myGmailMessageHeaders ->
    extractSenderFromHeader ([fromHeader] ++ myGmailMessageHeaders) === Just fromHeader

test_doNotextractSenderFromHeadersUsingQuickCheck =
  testProperty "It extract the From header from QuickCheck gen when exists" $
    forAll gmailMessageHeadersWithoutFrom $ \myGmailMessageHeaders ->
      extractSenderFromHeader myGmailMessageHeaders === Nothing

test_extractSenderFromHeaders2 =
  testCase "It returns a Nothing when no From header is present" $ do
    extractSenderFromHeader headersWithOutFrom @?= Nothing

-- test invariance : no from = Nothing

-- I do not know input, but accoding to output, I can say about the input

fromHeader :: GmailMessageHeader
fromHeader =
  GmailMessageHeader
    { name = "From",
      value = "Salewa <news@cm.salewa.com>"
    }

headersWithOutFrom :: [GmailMessageHeader]
headersWithOutFrom =
  [ GmailMessageHeader
      { name = "Delivered-To",
        value = "hubmessage.mailing@gmail.com"
      },
    GmailMessageHeader
      { name = "X-Google-Smtp-Source",
        value = "ABdhPJyyDS/+4ve4GWdwPg5WoygAj/ujq8ScYrPs4M7tKQYpH0o05a+m2bPZzZB+Fnd0DycZOmPM"
      }
  ]

headersWithFrom :: [GmailMessageHeader]
headersWithFrom = headersWithOutFrom ++ [fromHeader]
