import System.IO
type Posn = (Int, Int)
type RState = (Posn, Posn)

parse_one_line x = (head(words x), (read ((words x)!!1) :: Int))
parse_multi (a, b)
 | b > 1 = replicate b a
 | otherwise = [a]

parse xs = foldr ((++) . parse_multi . parse_one_line) [] xs

check_unrolla = [parse_multi ("U", 5) == ["U","U","U","U","U"]]

check_unrollb = [parse ["U 2", "R 3"] == ["U","U","R","R","R"]]

check_parse = [ parse_one_line "L 27" == ("L",27), parse_one_line "U 8" == ("U",8) ]

delta a
 | a == "U" = (0, 1)
 | a == "D" = (0, -1) 
 | a == "L" = (-1, 0)
 | otherwise = (1, 0)

check_delta = [ delta "U" == (0,1) ]

touching :: Posn -> Posn -> Bool
touching (hx, hy) (tx, ty)
 | abs(hx - tx) <= 1 && abs(hy - ty) <= 1 = True
 | otherwise = False

move :: Posn -> Posn -> Posn
move (hx, hy) (x, y) = (hx + x, hy + y)
check_move = [ move (2,7) (10,-1) == (12,6) ]

new_tailH :: Posn -> Posn -> Posn
new_tailH (hx, hy) (tx, ty)
 | hx == tx = (tx, (hy - ty) `quot` 2 + ty) --same col
 | hy == ty = ((hx - tx) `quot` 2 + tx, ty) --same row
 | hx > tx && hy > ty = (tx + 1, ty + 1) --q3
 | hx > tx && hy < ty = (tx + 1, ty - 1) --q2
 | hx < tx && hy > ty = (tx - 1, ty + 1) --q4
 | otherwise          = (tx - 1, ty - 1) --q1

new_tail h t
 | touching h t = t
 | otherwise    = new_tailH h t

check_tail = [ new_tail (2,7) (3,9) == (2, 8) , new_tail (4,2) (5,3) == (5, 3) ]

moveCombined (h, t) m = (hs, new_tail hs t)
  where hs = move h m

-- move, newtail, repeat (scanl)
update :: RState -> [(Int, Int)] -> [RState]
update (h, t) m = scanl (\x y -> moveCombined x y) (h, t) m

takeout (a, b) = b
remove [] = []
remove (x:xs)
 | x `elem` xs = remove xs
 | otherwise = x:(remove xs)

uniq_taila = [remove [(0, 0), (0, 0), (2, 0), (3, 5), (3, 5)] == [(0, 0), (2, 0), (3, 5)]]
uniq_tailb = [remove [(0, 0), (2, 0), (3, 5), (3, 2)] == [(0, 0), (2, 0), (3, 5), (3, 2)]]

--good luck
final1 x1 = length (remove (map takeout (update ((0, 0), (0, 0)) (map delta (parse x1)))))

final2H x1 = (map takeout (update ((0, 0), (0, 0)) (map delta (parse x1))))
final2k2 x1 = scanl (\x y -> new_tail y x) (0, 0) (final2H x1)
final2k3 x1 = scanl (\x y -> new_tail y x) (0, 0) (final2k2 x1)
final2k4 x1 = scanl (\x y -> new_tail y x) (0, 0) (final2k3 x1)
final2k5 x1 = scanl (\x y -> new_tail y x) (0, 0) (final2k4 x1)
final2k6 x1 = scanl (\x y -> new_tail y x) (0, 0) (final2k5 x1)
final2k7 x1 = scanl (\x y -> new_tail y x) (0, 0) (final2k6 x1)
final2k8 x1 = scanl (\x y -> new_tail y x) (0, 0) (final2k7 x1)
final2k9 x1 = scanl (\x y -> new_tail y x) (0, 0) (final2k8 x1)
final2 x1 = length(remove(final2k9 x1))

main = do
  x <- readFile "input.txt"
  let x1 = lines x
  print(final1 x1)
  print(final2 x1)
