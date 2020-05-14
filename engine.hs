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

piece_to_string Nothing = " "
piece_to_string (Just (Black, Pawn)) = "♟"
piece_to_string (Just (Black, Tower)) = "♜"
piece_to_string (Just (Black, Knight)) = "♞"
piece_to_string (Just (Black, Bishop)) = "♝"
piece_to_string (Just (Black, King)) = "♚"
piece_to_string (Just (Black, Queen)) = "♛"
piece_to_string (Just (White, Pawn)) = "♙"
piece_to_string (Just (White, Tower)) = "♖"
piece_to_string (Just (White, Knight)) = "♘"
piece_to_string (Just (White, Bishop)) = "♗"
piece_to_string (Just (White, King)) = "♔"
piece_to_string (Just (White, Queen)) = "♕"

row_to_string row = foldl (\str x -> str ++ " " ++ piece_to_string x ++ " |" ) "|" row

-- Display Board Output:
--
--     A    B    C    D    E    F    G    H
-- 8 | bT | bN | bB | bQ | bK | bB | bN | bT |
-- 7 | bp | bp | bp | bp | bp | bp | bp | bp |
-- 6 |    |    |    |    |    |    |    |    |
-- 5 |    |    |    |    |    |    |    |    |
-- 4 |    |    |    |    |    |    |    |    |
-- 3 |    |    |    |    |    |    |    |    |
-- 2 | wp | wp | wp | wp | wp | wp | wp | wp |
-- 1 | wT | wN | wB | wQ | wK | wB | wN | wT |

display board = do
  putStrLn "    a   b   c   d   e   f   g   h  "
  mapM_ (\(n, x) -> putStrLn (
      (show (8 - n) ++ " " ++ (row_to_string x)) ))
      (zip [0..] board)
