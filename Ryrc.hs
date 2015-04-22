import Data.List
import Network
import System.IO
import System.Exit
import System.Time
import Control.Arrow
import Control.Monad.Reader
import Control.Exception
import Text.Printf
import Data.Units

server = "irc.freenode.org"
port   = 6667
chan   = "#whatup"
nick   = "ryrc"
 
-- The 'Net' monad, a wrapper over IO, carrying the bot's immutable state.
type Net = ReaderT Bot IO
data Bot = Bot { socket :: Handle, starttime :: ClockTime }
 
-- Set up actions to run on start and end, and run the main loop
main :: IO ()
main = bracket connect disconnect loop
  where
    disconnect = hClose . socket
    loop st    = runReaderT run st
 
-- Connect to the server and return the initial bot state
connect :: IO Bot
connect = notify $ do
    t <- getClockTime
    h <- connectTo server (PortNumber (fromIntegral port))
    hSetBuffering h NoBuffering
    return (Bot h t)
  where
    notify a = bracket_
        (printf "Connecting to %s ... " server >> hFlush stdout)
        (putStrLn "done.")
        a
 
-- Join a channel, and start processing commands
run :: Net ()
run = do
    write "NICK" nick
    write "USER" (nick++" 0 * :ryrc bot")
    write "JOIN" chan
    asks socket >>= listen
 
-- Process each line from the server
listen :: Handle -> Net ()
listen h = forever $ do
    s <- init `fmap` io (hGetLine h)
    io (putStrLn s)
    if ping s then pong s else eval (clean s)
  where
    forever a = a >> forever a
    clean     = drop 1 . dropWhile (/= ':') . drop 1
    ping x    = "PING :" `isPrefixOf` x
    pong x    = write "PONG" (':' : drop 6 x)
 
-- Dispatch a command
eval :: String -> Net ()
eval     "!quit"                      = write "QUIT" ":Exiting" >> io (exitWith ExitSuccess)
eval x | "!id " `isPrefixOf`        x = privmsg (drop 4 x)
eval     "!uptime"                    = uptime >>= privmsg
eval x | "!convert -v" `isPrefixOf` x = privmsg (conv' (drop 11 x))
eval x | "!convert " `isPrefixOf`   x = privmsg (conv (drop 9 x))
eval x | "!power " `isPrefixOf`     x = privmsg (pow (drop 7 x))
eval     _                            = return () -- ignore everything else
 
-- Send a privmsg to the current chan + server
privmsg :: String -> Net ()
privmsg s = write "PRIVMSG" (chan ++ " :" ++ s)
 
-- Send a message out to the server we're currently connected to
write :: String -> String -> Net ()
write s t = do
    h <- asks socket
    io $ hPrintf h "%s %s\r\n" s t
    io $ printf    "> %s %s\n" s t
 
io :: IO a -> Net a
io = liftIO

conv :: String -> String
conv x =  case getPrefix x of
          Left msg -> msg
          Right prefix -> reportMeasurement (convert (strToMeasurement x) prefix)

conv' :: String -> String
conv' x =  case getPrefix x of
          Left msg -> msg
          Right prefix -> reportMeasurement' (convert (strToMeasurement x) prefix)

pow :: String -> String
pow = parsePower'

-- Provide binding for uptime
uptime :: Net String
uptime = do
    now <- io getClockTime
    zero <- asks starttime
    return . pretty $ diffClockTimes now zero

-- taken from haskell.org on to pretty print time values
pretty :: TimeDiff -> String
pretty td =
    unwords $ map (uncurry (++) . first show) $
    if null diffs then [(0,"s")] else diffs
    where merge (tot,acc) (sec,typ) = let (sec',tot') = divMod tot sec 
                                      in (tot',(sec',typ):acc)
          metrics = [(86400,"d"),(3600,"h"),(60,"m"),(1,"s")]
          diffs = filter ((/= 0) . fst) $ reverse $ snd $
                  foldl' merge (tdSec td,[]) metrics
