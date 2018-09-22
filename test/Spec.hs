import Tldr
import Test.Tasty
import Test.Tasty.Golden (goldenVsFile)
import System.IO (withBinaryFile, IOMode(..))

tests :: TestTree
tests = testGroup "tldr Tests" [goldenTests]

goldenTests :: TestTree
goldenTests = testGroup "Golden tests" [gtests]

renderPageToFile :: FilePath -> FilePath -> IO ()
renderPageToFile mdfile opfile = do
  withBinaryFile opfile WriteMode (\handle -> renderPage mdfile handle)

gtests :: TestTree
gtests = testGroup "(render test)" 
         [
          goldenVsFile 
          "ls test" 
          "test/data/ls.golden"
          "test/data/ls.output"
          (renderPageToFile "test/data/ls.md" "test/data/ls.output")
         ]

main :: IO ()
main = defaultMain tests
