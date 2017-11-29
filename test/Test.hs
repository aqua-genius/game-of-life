import Test.Tasty
import qualified Game.CoreTest
import qualified Game.ReadTest

main = defaultMain tests

tests = testGroup "Tests" [Game.CoreTest.tests, Game.ReadTest.tests]
