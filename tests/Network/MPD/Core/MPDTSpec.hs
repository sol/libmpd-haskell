module Network.MPD.Core.MPDTSpec (main, spec) where

import           Test.Hspec

import           Network.MPD.Core.MPDT

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "parseHost" $ do
    it "separates password from hostname" $ do
      parseHost "pw@host" `shouldBe` ("host", "pw")

    it "returns hostname if no password is given" $ do
      parseHost "host" `shouldBe` ("host", "")

    it "handles pw@ correct" $ do
      parseHost "pw@" `shouldBe` ("", "pw")

    it "handles @host correct" $ do
      parseHost "@host" `shouldBe` ("host", "")
