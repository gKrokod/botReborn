import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec $ do
  describe "Prelude.head" $ do
    it "returns the first element of a list" $ do
      head [23 ..] `shouldBe` (23 :: Int)

    it "returns the first element of an *arbitrary* list" $
      property $ \x xs -> head (x:xs) == (x :: Int)

    it "throws an exception if used with an empty list" $ do
      func ([]) `shouldBe` Nothing
  describe "NEW***************" $ do
    it "returns the first element of a list" $ do
      head [23 ..] `shouldBe` (23 :: Int)

    it "returns the first element of an *arbitrary* list" $
      property $ \x xs -> head (x:xs) == (x :: Int)

    it "throws an exception if used with an empty list" $ do
      func ([]) `shouldBe` Nothing

-- spec :: Spec
-- spec = 
--   describe "try" $ do
--     it "first try" $
--       func [] `shouldBe` Nothing
--     it "second" $
--       func [1] `shouldBe` (Just 1)
--
func :: [Int] -> Maybe Int
func [] = Nothing
func (x:xs) = Just x
