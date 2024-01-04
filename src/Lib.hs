-- {-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE RecordWildCards #-}
-- {-# LANGUAGE TemplateHaskell #-}

module Lib () where

-- import Brick
-- import Brick.BChan
-- import Brick.Widgets.Border
-- import Control.Concurrent
-- import Control.Monad
-- import Control.Monad.IO.Class
-- import Graphics.Vty
-- import Graphics.Vty.CrossPlatform
-- import Lens.Micro
-- import Lens.Micro.Mtl as L
-- import Lens.Micro.TH
-- import System.Directory (listDirectory)

-- data AppState = AppState
--   { _names :: [String],
--     _selected :: Integer,
--     _counter :: Integer,
--     _lastEvent :: Maybe String
--   }
--   deriving (Eq, Show)

-- makeLenses ''AppState

-- blueBg :: AttrName
-- blueBg = attrName "blueBg"

-- row :: Integer -> String -> Widget ()
-- row index name = str (show index) <+> padLeft (Pad padding) (str name)
--   where
--     padding = 3 - length (show index)

-- rows :: Integer -> [String] -> Widget ()
-- rows selected names = vBox $ zipWith render [0 ..] names
--   where
--     render index name = attrs index $ row index name
--     attrs index = if index == selected then withAttr blueBg else id

-- app :: App AppState () ()
-- app = App {..}
--   where
--     appDraw state =
--       [border $ rows (state ^. selected) (state ^. names)]

--     appChooseCursor = neverShowCursor

--     appHandleEvent (AppEvent event) = do
--       lastEvent .= Just ("Custom Event:" <> show event)

--       state <- view counter <$> get
--       when (state > 10) $ do
--         halt
--     appHandleEvent (VtyEvent (EvKey KUp _)) = do
--       len <- toInteger . length <$> use names
--       modifying selected (\s -> clamp 0 len (s - 1))
--     appHandleEvent (VtyEvent (EvKey KDown _)) = do
--       len <- toInteger . length <$> use names
--       modifying selected (\s -> clamp 0 (len - 1) (s + 1))
--     appHandleEvent (VtyEvent (EvKey KEnter _)) = do
--       index <- use selected
--       dir <- (!! fromInteger index) <$> use names
--       newNames <- liftIO $ listDirectory $ "./" <> dir
--       names .= newNames
--       modifying selected (\s -> clamp 0 (toInteger $ length newNames) (s + 1))
--     appHandleEvent (VtyEvent (EvKey (KChar 'q') _)) = halt
--     appHandleEvent e = do
--       liftIO $ print e
--       counter += 1
--       state <- view counter <$> get
--       when (state > 10) $ do
--         halt

--     appStartEvent = do
--       files <- liftIO $ listDirectory "."
--       names .= files

--     appAttrMap = const $ attrMap defAttr [(blueBg, bg blue), (attrName "borderAttr", bg blue)]

-- main :: IO ()
-- main = do
--   eventChan <- newBChan 1024

--   _ <- forkIO $ do
--     threadDelay 3000000
--     writeBChan eventChan ()

--   let buildVty = mkVty defaultConfig
--   let initialState = AppState [] 0 0 Nothing
--   initialVty <- buildVty
--   _ <- customMain initialVty buildVty (Just eventChan) app initialState

--   return ()
