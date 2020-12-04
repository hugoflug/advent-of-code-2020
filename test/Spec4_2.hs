module Spec4_2 where

import AOC4_2
import Test.Hspec

test = do
  specify "validHgt" $ do
    validHgt "60in" `shouldBe` True
    validHgt "190cm" `shouldBe` True
    validHgt "190in" `shouldBe` False
    validHgt "190" `shouldBe` False
  specify "validByr" $ do
    validByr "2002" `shouldBe` True
    validByr "2003" `shouldBe` False
  specify "validHcl" $ do
    validHcl "#123abc" `shouldBe` True
    validHcl "#123abz" `shouldBe` False
    validHcl "123abc" `shouldBe` False
  specify "validEcl" $ do
    validEcl "brn" `shouldBe` True
    validEcl "wat" `shouldBe` False
  specify "validPid" $ do
    validPid "000000001" `shouldBe` True
    validPid "0123456789" `shouldBe` False
