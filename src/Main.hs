module Main where

import Control.Concurrent
import qualified Control.Concurrent.Chan as Chan
import Control.Exception
import Control.Monad
import Data.Functor
import Data.List (intersperse)
import Data.Time
import Data.IORef
import Prelude hiding (catch)
import System.Random

import Paths
import GenLists

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core hiding (text)

{-----------------------------------------------------------------------------
    Chat
------------------------------------------------------------------------------}

maxLine = 43

main :: IO ()
main = do
    static   <- getStaticDir
    messages <- Chan.newChan
    startGUI defaultConfig
        { jsCustomHTML     = Just "chat.html"
        , jsStatic         = Just static
        , jsCallBufferMode = BufferRun
        } $ setup messages

type Message = (UTCTime, String, String)

setup :: Chan Message -> Window -> UI ()
setup globalMsgs window = do
    msgs <- liftIO $ Chan.dupChan globalMsgs

    return window # set title "Chat"
    
    (nickRef, nickname) <- mkNickname
    messageArea         <- mkMessageArea msgs nickRef

    getBody window #+
        [ UI.div #. "header"   #+ [string "Threepenny Chat"]
        , UI.div #. "gradient"
        , viewSource
        , element nickname
        , element messageArea
        ]
    
    messageReceiver <- liftIO $ forkIO $ receiveMessages window msgs messageArea

    on UI.disconnect window $ const $ liftIO $ do
        killThread messageReceiver
        now   <- getCurrentTime
        nick  <- readIORef nickRef
        Chan.writeChan msgs (now,nick,"( left the conversation )")


receiveMessages w msgs messageArea = do
    messages <- Chan.getChanContents msgs
    forM_ messages $ \msg -> do
        runUI w $ do
          element messageArea #+ [mkMessage msg]
          UI.scrollToBottom messageArea
          flushCallBuffer -- make sure that JavaScript functions are executed

mkMessageArea :: Chan Message -> IORef String -> UI Element
mkMessageArea msgs nickname = do
    input <- UI.textarea #. "send-textarea"
    
    on UI.sendValue input $ \content -> do
        element input # set value ""
        when (not (null content)) $ liftIO $ do
            now  <- getCurrentTime
            nick <- readIORef nickname
            when (not (null nick)) $
                Chan.writeChan msgs (now,nick,content)

    UI.div #. "message-area" #+ [UI.div #. "send-area" #+ [element input]]


mkNickname :: UI (IORef String, Element)
mkNickname = do
    input  <- UI.input #. "name-input"
    el     <- UI.div   #. "name-area"  #+
                [ UI.span  #. "name-label" #+ [string "Your name "]
                , element input
                ]
    UI.setFocus input
    
    nick <- liftIO $ newIORef ""
    on UI.keyup input $ \_ -> liftIO . writeIORef nick =<< get value input
    return (nick,el)

mkMessage :: Message -> UI Element
mkMessage (timestamp, nick, content) = do
    let content1 = convert content
    UI.div #. "message" #+
      [UI.pre #+ 
        [ UI.div #. "timestamp" #+ [string $ show timestamp]
        , UI.div #. "name"      #+ [string $ nick ++ " says:"]
        , UI.div #. "content"   #+ [string content1]
        ]
      ]

convert content = unlines . intersperse "\n" $
                  map ( unlines . wrap maxLine . unwords . transform . words) (lines content)
  where transform ls = init ls ++ [showList . genList . read . last $ ls]
        genList n = rndPermute (mkStdGen n) n
        showList ls = "[" ++ (concat $ intersperse ", " (map show ls)) ++ "]"
        wrap :: Int -> String -> [String]
        wrap maxWidth text = reverse (lastLine : accLines)
          where (accLines, lastLine) = foldl handleWord ([], "") (words text)
                handleWord (acc, line) word
                  -- 'word' fits fine; append to 'line' and continue.
                  | length line + length word <= maxWidth = (acc, word `append` line)
                  -- 'word' doesn't fit anyway; split awkwardly.
                  | length word > maxWidth                =
                    let (line', extra) = splitAt maxWidth (word `append` line)
                    in  (line' : acc, extra)
                  -- 'line' is full; start with 'word' and continue.
                  | otherwise                             = (line : acc, word)
                append word ""   = word
                append word line = line ++ " " ++  word

viewSource :: UI Element
viewSource =
    UI.anchor #. "view-source" # set UI.href url #+ [string "View source code"]
    where
    url = samplesURL ++ "Chat.hs"
