import Prelude
import System.Environment ( getArgs )
import Data.List
import Helpers

-- The main method that will be used for testing / command line access
main = do
 args <- getArgs
 filename <- readFile (head args)
 (map,turns) <- readMapFile filename
 print "Result"
 printMap (oneVehicleOneTurn map (head turns))

-- YOUR CODE SHOULD COME AFTER THIS POINT
oneVehicleOneTurn :: [[Char]] -> Char -> [[Char]]
oneVehicleOneTurn map turn
              | ( (turn == 'n') && (north_north == 0) ) = map
              | ( (turn == 's') && (south_south == 0) ) = map
              | ( (turn == 'w') && (west_west == 0) ) = map
              | ( (turn == 'e') && (east_east == 0) ) = map
              | otherwise = oneVehicleOneTurn_helper map turn vehicle
              where north_north = continue_boolean map (i-1, j)
                    south_south = continue_boolean map (i+1, j)
                    west_west = continue_boolean map (i, j-1)
                    east_east = continue_boolean map (i, j+1)
                    (i,j) = head_3
                    (head_3 : tail_3) = find_vehicle_2D map vehicle
                    vehicle = '1'

oneVehicleOneTurn_helper :: [[Char]] -> Char -> Char -> [[Char]]
oneVehicleOneTurn_helper [] _ _  = []
oneVehicleOneTurn_helper map turn vehicle
        | (turn == 'd' || turn == 'b' || turn == 'c' || turn == 'o') = map
        | otherwise = oneVehicleOneTurn_helper updated_map updated_turn vehicle
        where 
          updated_map  = exchange_element map position position_next vehicle
          position  = head_1
          (head_1 : tail_1) = find_vehicle_2D map vehicle
          position_next  = head_2
          (head_2 : tail_2) = next_move position turn
          updated_turn = next_turn updated_map turn vehicle total_moves
          total_moves = possible_moves updated_map position_next
        
possible_moves :: [[Char]] -> (Int,Int) -> Int
possible_moves [] _ = 0
possible_moves map (i,j)
        | ( i < 0 || i >= rows) = 0
        | ( j < 0 || j >= columns) = 0
        | otherwise = (continue_boolean map north_position) + (continue_boolean map south_position) + (continue_boolean map west_position) + (continue_boolean map east_position)
        where north_position = (i - 1, j)
              south_position = (i + 1, j)
              west_position = (i , j - 1)
              east_position = (i , j + 1)
              rows = total_rows map
              columns = total_columns map

exchange_element :: [[Char]] -> (Int,Int) -> (Int,Int) -> Char -> [[Char]]
-- should psotion be considered as a list?
exchange_element [] _ _ _ = []
exchange_element map position new_position vehicle = exchange_element_helper_2D new_map new_position vehicle 0
        where new_map = exchange_element_helper_2D map position '-' 0

exchange_element_helper_2D :: [[Char]] -> (Int,Int) -> Char -> Int -> [[Char]]
exchange_element_helper_2D (row : rows) (i, j) char_new row_temp
-- [(exchange_element_helper_1D):rows] bc the result should be [[]] instead of CHAr like in find_element
          | (i == row_temp) = (exchange_element_helper_1D row j char_new 0):rows
          | otherwise = (row:(exchange_element_helper_2D rows (i,j) char_new (row_temp + 1)))

exchange_element_helper_1D :: [Char] -> Int -> Char -> Int -> [Char]
exchange_element_helper_1D (head : tail) index char_new index_temp 
          | (index == index_temp) = (char_new : tail)
          | otherwise = head : (exchange_element_helper_1D tail index char_new (index_temp + 1))


next_move :: (Int,Int) -> Char -> [(Int,Int)]
next_move (i,j) turn
                | (turn == 'n') = [(i-1,j)]
                | (turn == 's') = [(i+1,j)]
                | (turn == 'w') = [(i,j-1)]
                | (turn == 'e') = [(i,j+1)]

next_turn :: [[Char]] -> Char -> Char -> Int -> Char
next_turn map turn vehicle total_moves
                | (total_moves == 0) = 'o'
                | (total_moves == 1) = 'd'
                | (total_moves == 4) = 'b'
                | (total_moves == 3) = 'c'
                | (total_moves == 2) = next_turn_helper map turn vehicle

next_turn_helper :: [[Char]] -> Char -> Char -> Char
next_turn_helper map turn vehicle
                | ( (turn == 'n') && (north_north == 1)) = 'n'
                | ( (turn == 'n') && (north_west == 1)) = 'w'
                | ( (turn == 'n') && (north_east == 1)) = 'e'
                | ( (turn == 's') && (south_south == 1)) = 's'
                | ( (turn == 's') && (south_west == 1)) = 'w'
                | ( (turn == 's') && (south_east == 1)) = 'e'
                | ( (turn == 'w') && (west_west == 1)) = 'w'
                | ( (turn == 'w') && (west_north == 1)) = 'n'
                | ( (turn == 'w') && (west_south == 1)) = 's'
                | ( (turn == 'e') && (east_east == 1)) = 'e'
                | ( (turn == 'e') && (east_north == 1)) = 'n'
                | ( (turn == 'e') && (east_south == 1)) = 's'
                where north_north = continue_boolean map (i-1, j)
                      north_west = continue_boolean map (i, j-1)
                      north_east = continue_boolean map (i, j+1)
                      south_south = continue_boolean map (i+1, j)
                      south_west = continue_boolean map (i, j-1)
                      south_east = continue_boolean map (i, j+1)
                      west_west = continue_boolean map (i, j-1)
                      west_north = continue_boolean map (i-1, j)
                      west_south = continue_boolean map (i+1, j)
                      east_east = continue_boolean map (i, j+1)
                      east_north = continue_boolean map (i-1, j)
                      east_south = continue_boolean map (i+1, j)
                      (i,j) = head
                      (head : tail) = find_vehicle_2D map vehicle

continue_boolean :: [[Char]] -> (Int,Int) -> Int
continue_boolean map (i,j)
                     | ((find_element map (i,j) 0) == '-') = 1
                     | otherwise = 0

find_element :: [[Char]] -> (Int,Int) -> Int -> Char
find_element (row:rows) (i,j) row_temp
        | (i == row_temp) = find_element_helper row j 0
        | otherwise = find_element rows (i,j) (row_temp + 1)

find_element_helper :: [Char] -> Int -> Int -> Char
find_element_helper (head : tail) index index_temp
        | (index == index_temp) = head
        | otherwise = find_element_helper tail index (index_temp + 1)

find_vehicle_ID :: Eq e => [e] -> e -> [Int]
find_vehicle_ID row element = find_vehicle_ID_helper row element 0

find_vehicle_ID_helper :: Eq e => [e] -> e -> Int -> [Int]
find_vehicle_ID_helper [] _ _ = []
find_vehicle_ID_helper (head : tail) element index
        | (head == element) = index : (find_vehicle_ID_helper tail element (index + 1))
        | otherwise = find_vehicle_ID_helper tail element (index + 1)

find_vehicle_2D :: Eq e => [[e]] -> e -> [(Int,Int)]
find_vehicle_2D map element = find_vehicle_2D_helper map element 0

find_vehicle_2D_helper :: Eq e => [[e]] -> e -> Int -> [(Int, Int)]
find_vehicle_2D_helper [] _ _ = []
find_vehicle_2D_helper (row:rows) element row_index = [(row_index,column) | column <- (find_vehicle_ID row element)] ++ (find_vehicle_2D_helper rows element (row_index + 1))

total_rows :: [[Char]] -> Int
total_rows [] = 0
total_rows (row:rows) = 1 + (total_rows rows)

total_columns :: [[Char]] -> Int
total_columns [] = 0
total_columns (row:rows) = length row