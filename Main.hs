{-# LANGUAGE OverloadedStrings #-}
import Reflex
import Reflex.Dom
import Data.Text as DT
import Data.Maybe
import Data.Map
import Text.Read

svgns :: Maybe Text
svgns = (Just "http://www.w3.org/2000/svg")

showError :: Maybe a -> String
showError Nothing = "invalid input"
showError _ = ""

toIntMap :: Maybe Int -> Map Int Int
toIntMap  = 
    fromList
    .fmap (\i -> ((,) 0 (fromMaybe 0 i))) -- Change "Just Int" to "Int"
    .Prelude.filter isJust -- Get rid of Nothing ( an invalid Int)
    .(:[]) -- Put the Maybe Int into a (one item) list 

toMaybeInt  = 
    (readMaybe::(String -> Maybe Int))  -- String to Maybe Int
    .unpack  -- Text to String

circleAttrs :: Int -> Map Text Text
circleAttrs r =
    fromList [ ( "cx",    "0")
             , ( "cy",    "0")
             , ( "r",     pack $ show r)
             , ( "style", "fill:green")
             ] 

showCircle :: MonadWidget t m => Int -> Dynamic t Int -> m ()
showCircle _ dInt = do
    elStopPropagationNS svgns "g" Mousemove $ 
        elDynAttrNS' svgns "circle" (circleAttrs <$> dInt) $ return ()
    return ()

view :: MonadWidget t m => m ()
view = do 
    ti <- el "div" $ textInput def
    let dString = value ti
        dMaybeInt = fmap toMaybeInt dString
        dIntMap = fmap toIntMap dMaybeInt
        
        dAttrs = constDyn $ 
                        fromList [ ("width" , "600")
                                 , ("height", "400")
                                 , ("style" , "border:solid; margin:8em")
                                 ]

    dynText $ fmap (pack.showError) dMaybeInt
    elDynAttrNS' svgns "svg" dAttrs $ listWithKey dIntMap showCircle

    return ()

main = mainWidget view
