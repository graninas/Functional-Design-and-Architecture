module Terminal where


data Terminal
  = ReadLine (String -> [Terminal])
  | PrintLine String


terminal :: [Terminal]
terminal =
  [ ReadLine (\line1 -> [PrintLine line1])
  , ReadLine (\line2 -> [PrintLine line2])
  ]

