import HW05Tests
import Testing

main :: IO ()
main = do
    print (testResults)

testResults :: [Failure]
-- testResults = runTests allTests
testResults = []