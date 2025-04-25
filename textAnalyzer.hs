import Data.List (isPrefixOf, isSuffixOf, sortBy)
import Data.Map (Map, empty, insertWith, toList)
import Data.Ord (comparing)
import Data.Maybe (fromMaybe)

-- Function to extract the username from a line, if present
extractUsername :: String -> Maybe String
extractUsername line =
  case words line of
    (word : _) | "<" `isPrefixOf` word && ">" `isSuffixOf` word -> Just word
    _ -> Nothing

-- Function to extract the words (excluding the username) from a line
extractWords :: String -> [String]
extractWords line =
  case words line of
    (word : rest) | "<" `isPrefixOf` word && ">" `isSuffixOf` word -> rest
    allWords -> allWords

-- Analyzes the text to count words per attributed username
analyzeTextSimilar :: String -> [String]
analyzeTextSimilar s =
  let textLines = lines s  -- Renamed to avoid shadowing built-in function
      (wordCounts, _) = foldl processLine (empty, Nothing) textLines
      sortedWordCounts = sortBy (comparing (negate . snd)) (toList wordCounts)
  in map fst sortedWordCounts
  where
    processLine :: (Map String Int, Maybe String) -> String -> (Map String Int, Maybe String)
    processLine (currentCounts, currentUsername) line =
      let newUsername = extractUsername line
          attributedUsername = case newUsername of
                                Just name -> Just name
                                Nothing -> currentUsername
          wordsOnLine = extractWords line
          wordCount = length wordsOnLine
          updatedCounts = case attributedUsername of
                            Just username -> insertWith (+) username wordCount currentCounts
                            Nothing -> currentCounts  -- Ignore lines with no attributed username
      in (updatedCounts, attributedUsername)

main :: IO ()
main = do
  let inputText = "<user1> this is some chat words\n<user2> the sky is blue\nThis line is still attributed to the user above haha\n<user1> more chat from me! 38gad81"
  print (analyzeTextSimilar inputText)