data PieceColor = Black | White
data PieceType = Pawn | Tower | Knight | Bishop | King | Queen

initial_board =
  [ [Just (Black, Tower), Just (Black, Knight), Just (Black, Bishop), Just (Black, Queen), Just (Black, King), Just (Black, Bishop), Just (Black, Knight), Just (Black, Tower)]
  , [Just (Black, Pawn), Just (Black, Pawn), Just (Black, Pawn), Just (Black, Pawn), Just (Black, Pawn), Just (Black, Pawn), Just (Black, Pawn), Just (Black, Pawn)]
  , [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
  , [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
  , [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
  , [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
  , [Just (White, Pawn), Just (White, Pawn), Just (White, Pawn), Just (White, Pawn), Just (White, Pawn), Just (White, Pawn), Just (White, Pawn), Just (White, Pawn)]
  , [Just (White, Tower), Just (White, Knight), Just (White, Bishop), Just (White, Queen), Just (White, King), Just (White, Bishop), Just (White, Knight), Just (White, Tower)] ]


piecetype_to_string Pawn = "p"
piecetype_to_string Tower = "T"
piecetype_to_string Knight = "N"
piecetype_to_string Bishop = "B"
piecetype_to_string King = "K"
piecetype_to_string Queen = "Q"

piece_to_string Nothing = "  "
piece_to_string (Just (Black, p)) = "b" ++ piecetype_to_string p
piece_to_string (Just (White, p)) = "w" ++ piecetype_to_string p

row_to_string row = foldl (\str x -> str ++ " " ++ piece_to_string x ++ " |" ) "|" row

display board = do
  putStrLn "    A    B    C    D    E    F    G    H   "
  mapM_ (\(n, x) -> putStrLn (
      (show (8 - n) ++ " " ++ (row_to_string x)) ))
      (zip [0..] board)

display_board_example = do
  putStrLn "| wT | wN | wB | wQ | wK | wB | wN | wT |"
  putStrLn "| wp | wp | wp | wp | wp | wp | wp | wp |"
  putStrLn "|    |    |    |    |    |    |    |    |"
  putStrLn "|    |    |    |    |    |    |    |    |"
  putStrLn "|    |    |    |    |    |    |    |    |"
  putStrLn "|    |    |    |    |    |    |    |    |"
  putStrLn "| wT | wN | wB | wQ | wK | wB | wN | wT |"
  putStrLn "| wp | wp | wp | wp | wp | wp | wp | wp |"
