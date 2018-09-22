import Tldr
import Test.Tasty
import Test.Tasty.Golden (goldenVsFile)
import System.IO (withBinaryFile, IOMode(..))
import Data.Monoid ((<>))

tests :: TestTree
tests = testGroup "tldr Tests" [goldenTests]

goldenTests :: TestTree
goldenTests = testGroup "Golden tests" [gtests]

renderPageToFile :: FilePath -> FilePath -> IO ()
renderPageToFile mdfile opfile = do
  withBinaryFile opfile WriteMode (\handle -> renderPage mdfile handle)

commandTest :: String -> TestTree
commandTest str = goldenVsFile (str <> " test") (golden str) (output str) (renderPageToFile (md str) (output str))
    where
      prefix = "test/data/"
      golden cmd = prefix <> cmd <> ".golden"
      output cmd = prefix <> cmd <> ".output"
      md cmd = prefix <> cmd <> ".md"

gtests :: TestTree
gtests = testGroup "(render test)" 
         [
          commandTest "ls"
         ]

main :: IO ()
main = defaultMain tests
