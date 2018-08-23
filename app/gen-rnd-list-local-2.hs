module Main where

import Control.Concurrent
import qualified Control.Concurrent.Chan as Chan
import Control.Exception
import Control.Monad
import Data.Functor
import Data.List (intersperse, sort)
-- import Data.List.Extra (replace)
import Data.Time
import Data.IORef
import Prelude hiding (catch)
import System.Random

import Paths
import GenLists

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core hiding (text)

{-----------------------------------------------------------------------------
    Lucky numbers 
------------------------------------------------------------------------------}

maxLine = 43

main :: IO ()
main = do
    static   <- getStaticDir
    startGUI defaultConfig
        { jsCustomHTML     = Just "nums.html"
--        , jsPort           = Just 80
        , jsStatic         = Just static
        , jsCallBufferMode = BufferRun
        } $ setup 

type Message = (UTCTime, String, String)

setup :: Window -> UI ()
setup window = do

    return window # set title "Numbers"
    
    inputArea           <- UI.textarea #. "send-textarea" 
    
    sendBtn             <- UI.button #. "button" # set UI.text "Отправить"
    messageArea         <-  UI.div #. "message-area" 
                            #+ [UI.div #. "send-area" #+ [element inputArea]
                                                      #+ [element sendBtn]]  

    getBody window #+
        [ UI.div #. "header"   #+ [string "Lucky numbers"]
        , element messageArea
        ]
    on UI.click sendBtn $ \ _ -> do
      content <- get value inputArea
      mkWindow window content

    return ()


mkWindow :: Window -> String -> UI ()
mkWindow w content = do
    when (not (null content)) $ do
        now  <- liftIO getCurrentTime
        msgArea <- mkMessage (now,"nick",content)
        bodyArea <- UI.div #. "message-area" #+ [element msgArea]
        getBody w # set children [bodyArea]
        return ()

mkMessage :: Message -> UI Element
mkMessage (timestamp, nick, content) = do
    seed <- liftIO $ newStdGen
    let content1 = convert seed content
    UI.div #. "message" #+
      [UI.pre #+ 
        [ UI.div #. "timestamp" #+ [string $ show timestamp]
      --  , UI.div #. "name"      #+ [string $ nick ++ " says:"]
        , UI.div #. "content"   #+ [string content1]
        ]
      ]

-- for extra new lines: replace "#n" "\n" ls

convert seed content = unlines . intersperseN 2 "\n" $
                  map ( unlines . wrap maxLine . unwords) . transform seed
                  . separate . map  words $ filter (not . null) $ lines content
  where separate :: [[String]] -> [([String], Int)]
        separate = map (\ls -> (init ls, read $ last ls))
        transform seed ls = 
          let sumN = sum . snd . unzip $ ls
              listN = rndPermute seed sumN
              fChop ((ls, n) : lss) bigLst =
                 (ls ++ [showList . sort . take n $ bigLst])
                 : ["Всего номеров: ", show n]
                 : fChop lss (drop n bigLst)
              fChop _ [] = []
              fChop [] _ = []
          in
              fChop ls listN ++ [["Сумма: ", show sumN]]
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

intersperseN :: Int -> a -> [a] -> [a]
intersperseN _ _ [] = []
intersperseN n e ls = take n ls ++ [e] ++ intersperseN n e (drop n ls)

{-
viewSource :: UI Element
viewSource =
    UI.anchor #. "view-source" # set UI.href url #+ [string "View source code"]
    where
    url = samplesURL ++ "Chat.hs"
-}
