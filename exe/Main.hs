{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

module Main where

--oprc imports
import Env
import MoveCosts
import qualified SampleVals as SV

--diagrams imports
--import qualified Diagrams.Prelude as D
--import qualified Diagrams.Backend.SVG.CmdLine as BE

--
import Control.Monad.Trans (liftIO)
import Graphics.UI.Gtk
import Diagrams.Backend.Cairo
import Diagrams.Backend.Cairo.Gtk
import Diagrams.Prelude

--myCircle :: D.Diagram BE.B
--myCircle = D.circle 1

--main :: IO ()
--main = do
  --putStrLn "Starting Environment"
  --putStrLn $ show SV.pat

--main = BE.mainWith myCircle

main :: IO ()
main = do
  initGUI
  window <- windowNew
  canvas <- drawingAreaNew
  canvas `on` sizeRequest $ return (Requisition 256 256)
  set window [ containerBorderWidth := 10,
               containerChild := canvas ]
  canvas `on` exposeEvent $ renderFigure
  onDestroy window mainQuit
  widgetShowAll window
  mainGUI


renderFigure :: EventM EExpose Bool
renderFigure = do
   win <- eventWindow
   liftIO $ renderToGtk win $ toGtkCoords figure
   return True


figure :: Diagram Cairo R2
figure =  unitCircle # scaleX 0.5 # rotateBy (1/6) # scale 50 # fc red  
