axiom :: String
axiom = "F"

rules :: [(Char, String)]
rules = [
    ('F', "F+F-F")
    , ('-', "-")
    , ('+', "+")
    ]

applyRules :: String -> String 
applyRules = concatMap (\c -> case lookup c rules of
    Just r -> r
    Nothing -> [c]
    )

generateLSystem :: Int -> String 
generateLSystem 0 = axiom
generateLSystem n = applyRules (generateLSystem (n-1))