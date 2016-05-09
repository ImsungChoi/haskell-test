import HW04Tests
import Testing

main :: IO ()
main = do
    print (testResults)

testResults :: [Failure]
testResults = runTests allTests