import Data.List (isPrefixOf, isSuffixOf)

isSpace' :: Char -> Bool
isSpace' char = char == ' '

text :: String
text = "<user1> Howdy, how are ya doing ?\n<user2> I am doing great."

spacesInText :: [Char] -> [Char]
spacesInText = filter isSpace'

countSpaces :: String -> Int
countSpaces s = length $ filter isSpace' s

separateLines :: String -> [String]
separateLines = lines

separateWords :: String -> [String]
separateWords = words

startwithBrackets :: String -> Bool
startwithBrackets word = "<" `isPrefixOf` word

endswithBrackets :: String -> Bool
endswithBrackets word = ">" `isSuffixOf` word

hasContentBetweenBrackets :: String -> Bool
hasContentBetweenBrackets word = length word >= 4

findUserName :: String -> Bool
findUserName word = startwithBrackets word && endswithBrackets word && hasContentBetweenBrackets word

matchingUserName :: String -> [String]
matchingUserName s = filter findUserName (separateWords s)

matchingNonUserName :: String -> [String]
matchingNonUserName s = filter (not . findUserName) (separateWords s)

-- Type definition for our result: (line, usernames, count of non-username words)
type LineAnalysis = (String, [String], Int)

-- Analyze each line in the text
analyzeLine :: String -> LineAnalysis
analyzeLine line = 
    let usernames = matchingUserName line
        nonUserNameWords = matchingNonUserName line
        nonUserNameCount = length nonUserNameWords
    in (line, usernames, nonUserNameCount)

analyzeText :: String -> [LineAnalysis]
analyzeText text = map analyzeLine (separateLines text)

-- Pretty print the analysis results
printTextAnalysis :: [LineAnalysis] -> IO ()
printTextAnalysis = mapM_ printLineAnalysis
  where
    printLineAnalysis :: LineAnalysis -> IO ()
    printLineAnalysis (line, usernames, count) = do
      putStrLn $ "Line: " ++ line
      putStrLn $ "  Usernames found: " ++ show usernames
      putStrLn $ "  Non-username word count: " ++ show count
      putStrLn ""

-- Example function to run the analysis
runTextAnalysis :: String -> IO ()
runTextAnalysis text = printTextAnalysis (analyzeText text)





