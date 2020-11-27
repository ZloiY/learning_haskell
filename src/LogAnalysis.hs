module LogAnalysis
  ( parseMessage
  , parse
  , createMessageTree
  ) where

import Log

parseMessage :: String -> LogMessage
parseMessage = aggregateMessage (LogMessage Info 0 "") . words
  where aggregateMessage _ ("E":a:xs)                      = aggregateMessage (LogMessage (Error (read a)) 0 "") xs
        aggregateMessage _ ("W":xs)                        = aggregateMessage (LogMessage Warning 0 "") xs
        aggregateMessage _ ("I":xs)                        = aggregateMessage (LogMessage Info 0 "") xs
        aggregateMessage (LogMessage Info _ _) (x:xs)      = LogMessage Info (read x) (unwords xs)
        aggregateMessage (LogMessage Warning _ _) (x:xs)   = LogMessage Warning (read x) (unwords xs)
        aggregateMessage (LogMessage (Error e) _ _) (x:xs) = LogMessage (Error e) (read x) (unwords xs)

parse :: String -> [LogMessage]
parse log = map parseMessage (lines log)

createMessageTree :: [LogMessage] -> MessageTree
createMessageTree = foldl findPlace Leaf
  where 
    findPlace (Node left message1@(LogMessage _ b1 _) right) message@(LogMessage _ b _)
      | b > b1    = Node left message1 (findPlace right message)
      | otherwise = Node (findPlace left message) message1 right
    findPlace Leaf message = Node Leaf message Leaf

