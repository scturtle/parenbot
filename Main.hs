{-# LANGUAGE OverloadedStrings #-}
import Network
import System.IO
import System.Environment
import Data.List (isPrefixOf)
import Control.Exception (bracket)
import Control.Monad.Reader
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text.Encoding as E

data Config = Config { channel :: String , handle :: Handle}
type Env = ReaderT Config IO

main :: IO ()
main = do
    args <- getArgs
    let chan = if null args then "#test" else "#" ++ head args
    bracket (connect chan) (hClose . handle) (runReaderT run)
  where connect chan = do
          h <- connectTo "irc.freenode.org" (PortNumber 6667)
          hSetBuffering h NoBuffering
          return $ Config chan  h
        run = do
          chan <- asks channel
          send "NICK" "parenbot"
          send "USER" "parenbot 0 * :parenbot"
          send "JOIN" chan
          listen

send :: String -> String -> Env ()
send cmd s = do
  h <- asks handle
  let reply = cmd ++ " " ++ s
  liftIO . B8.hPutStrLn h . E.encodeUtf8 $ T.pack reply
  liftIO . B8.hPutStrLn stderr . E.encodeUtf8 $ T.pack reply

sendPrivmsg :: String -> String -> Env ()
sendPrivmsg to s = send "PRIVMSG" $ to ++ " :" ++ s

listen :: Env ()
listen = forever $ do
    h <- asks handle
    rawmsg <-liftIO $ B.hGetLine h
    let msg = T.unpack . T.init . E.decodeUtf8 $ rawmsg
        sender = takeWhile (/= '!') . drop 1 $ msg
        receiver = words msg !! 2
        to = if receiver == "parenbot" then sender else receiver
        text = drop 1 . dropWhile (/= ':') . drop 1 $ msg
    liftIO $ hPutStrLn stderr msg
    if "PING :" `isPrefixOf` msg
      then send "PONG" (':' : drop 6 msg)
      else when (head msg == ':' && (words msg !! 1) == "PRIVMSG") $
             maybe (return ()) (sendPrivmsg to . (++ face)) (matchParen text)
  where face = "○(￣□￣○)"

parenList :: String
parenList = "<>()[]{}（）［］｛｝⦅⦆〚〛⦃⦄“”‘’‹›«»"
          ++ "「」〈〉《》【】〔〕⦗⦘『』〖〗〘〙"
          ++ "｢｣⟦⟧⟨⟩⟪⟫⟮⟯⟬⟭⌈⌉⌊⌋⦇⦈⦉⦊❛❜❝❞❨❩❪❫❴❵❬❭❮❯❰❱"
          ++ "❲❳⏜⏝⎴⎵⏞⏟⏠⏡﹁﹂﹃﹄︹︺︻︼︗︘︿﹀︽︾﹇﹈︷︸"

parlenMap :: M.Map Char Char
parlenMap = M.fromList $ parseList parenList
  where parseList [] = []
        parseList (l:r:xs) = (l,r) : parseList xs
        parseList _ = error "unmatched parenList"

matchParen :: String -> Maybe String
matchParen input = toMaybe $ match input []
  where match [] output = output
        match (x:xs) (y:ys)
          | x == y = match xs ys
        match (x:xs) output
          | x `M.member` parlenMap = match xs $ parlenMap M.! x : output
          | otherwise = match xs output
        toMaybe [] = Nothing
        toMaybe s = Just s
