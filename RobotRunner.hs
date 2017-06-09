import Data.Monoid as M
import Data.Monoid.Endo as E

data Position = Position { x :: Int, y :: Int }

data Direction = North | South | East | West

data Robot = Robot { pos :: Position, dir :: Direction }

type RobotRunner = Endo (Maybe Robot)

move :: RobotRunner
move = M.Endo (fmap (\robot ->
      case (pos robot, dir robot) of
        (Position x y, North) -> Robot (Position x (y + 1)) North
        (Position x y, South) -> Robot (Position x (y -1)) South
        (Position x y, East) -> Robot (Position (x + 1) y) East
        (Position x y, West) -> Robot (Position (x - 1) y) West
        ))

left :: RobotRunner
left = M.Endo (fmap (\robot ->
  case (pos robot, dir robot) of
      (p, North) -> Robot p West
      (p, West) -> Robot p South
      (p, South) -> Robot p East
      (p, East) -> Robot p North
    ))

right :: RobotRunner
right = M.Endo (fmap (\robot ->
  case (dir robot) of
    North -> Robot (pos robot) East
    East -> Robot (pos robot) South
    South -> Robot (pos robot) West
    West -> Robot (pos robot) North
))
