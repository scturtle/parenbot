{-# LANGUAGE OverloadedStrings #-}
import Network
import System.IO
import Data.Functor ((<$>))
import Control.Monad (forever,unless)
import Control.Exception (bracket)
import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text.IO as TI
import qualified Data.Text.Encoding as E

channel :: String
channel = "#haskell-cn"

main :: IO ()
main = bracket connect hClose listen
  where connect = do
          h <- connectTo "irc.freenode.org" (PortNumber 6667)
          hSetBuffering h NoBuffering
          send h "NICK" "parenbot"
          send h "USER" "parenbot 0 * :parenbot"
          send h "JOIN" (T.pack channel)
          return h

send :: Handle -> T.Text -> T.Text -> IO ()
send h s t = do
  let rep = T.concat [s, " ", t]
  B8.hPutStrLn h $ E.encodeUtf8 rep
  B8.putStrLn $ E.encodeUtf8 rep

listen :: Handle -> IO ()
listen h = forever $ do
    msg <- T.init . E.decodeUtf8 <$> B.hGetLine h
    TI.putStrLn msg
    if ping msg
      then pong msg
      else let s = T.dropWhile (`notElem` ("()（）" :: String))
                  . T.reverse $ clean msg
           in  unless (T.null s) $
                 case T.head s of
                   '('  -> send' $ replicate (countParen '(' s)  ')'
                   '（' -> send' $ replicate (countParen '（' s) '）'
                   _    -> return ()
  where ping x = "PING :" `T.isPrefixOf` x
        pong x = send h "PONG" (T.cons ':' $ T.drop 6 x)
        clean = T.drop 1 . T.dropWhile (/= ':') . T.drop 1
        face = "○(￣□￣○)"
        countParen c = T.length . T.takeWhile (== c)
        send' s = send h "PRIVMSG" (T.pack $ channel ++ s ++ face)
