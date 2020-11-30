module LogAnalysis
  ( parseMessage
  , parse
  , buildMessageTree
  , insertMessage
  , inOrder
  , whatWentWrong
  ) where

import Log

parseMessage :: String -> LogMessage
parseMessage = aggregateMessage . words
  where aggregateMessage ("E":e:t:msg) = LogMessage (Error (read e)) (read t) (unwords msg)
        aggregateMessage ("W":t:msg)   = LogMessage Warning (read t) (unwords msg)
        aggregateMessage ("I":t:msg)   = LogMessage Info (read t) (unwords msg)
        aggregateMessage msg           = Unknown (unwords msg)

parse :: String -> [LogMessage]
parse log = map parseMessage (lines log)

insertMessage :: LogMessage -> MessageTree -> MessageTree
insertMessage message@(LogMessage _ t _) (Node left message1@(LogMessage _ t1 _) right)
  | t > t1    = Node left message1 (insertMessage message right)
  | otherwise = Node (insertMessage message left) message1 right
insertMessage message Leaf            = Node Leaf message Leaf
insertMessage (Unknown _) messageTree = messageTree

buildMessageTree :: [LogMessage] -> MessageTree
buildMessageTree = foldl (flip insertMessage) Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf                      = []
inOrder (Node left message right) = inOrder left ++ [message] ++ inOrder right

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = foldl errors [] . inOrder . buildMessageTree
  where
    errors messages (LogMessage (Error e) _ message)
      | e >= 50   = messages ++ [message]
      | otherwise = messages
    errors messages _ = messages