data Direction = North
               | South
               | East
               | West

turn_clockwise : Direction -> Direction
turn_clockwise North = East
turn_clockwise South = West
turn_clockwise East = South
turn_clockwise West = North
