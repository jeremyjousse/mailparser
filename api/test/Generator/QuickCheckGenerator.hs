module Generator.QuickCheckGenerator where

import Domain.Gmail.Types
import Test.QuickCheck

instance Arbitrary GmailMessageHeader where
  arbitrary = do
    -- use oneof :: [Gen a] -> Gen aSource when we have generators for each name
    -- use frequency :: [(Int, Gen a)] -> Gen aSource if you want a frequency on
    randomName <- elements ["From", "Received", "Delivered-To", "Return-Path"]
    randomValue <- arbitrary
    pure GmailMessageHeader {name = randomName, value = randomValue}

gmailMessageHeadersWithoutFrom :: Gen [GmailMessageHeader]
gmailMessageHeadersWithoutFrom = listOf $ do
  randomName <- elements ["Received", "Delivered-To", "Return-Path"]
  randomValue <- arbitrary
  pure GmailMessageHeader {name = randomName, value = randomValue}
