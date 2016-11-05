{-# LANGUAGE OverloadedStrings #-}
import Reflex
import Reflex.Dom
import Data.Text (Text, pack, unpack) 
import Data.Map (Map, fromList, empty)
import Text.Read (readMaybe)

width = 600
height = 500

type Point = (Float,Float)
type Segment = (Point,Point)

data Ellipse = Ellipse {a :: Float, b :: Float, n :: Float}

toFloat :: Text -> Maybe Float
toFloat  = readMaybe.unpack  

toEllipse :: Maybe Float -> Maybe Float -> Maybe Float -> Maybe Ellipse
toEllipse (Just a) (Just b) (Just n) = 
    if a < 1.0 || b <= 1.0 || n <= 0.0  -- not all floats are valid
    then Nothing 
    else Just $ Ellipse a b n

toEllipse _ _ _ = Nothing

svgns :: Maybe Text
svgns = Just "http://www.w3.org/2000/svg"

showError :: Maybe a -> String
showError Nothing = "invalid input"
showError _ = ""

reflect45 pts  =  pts ++ fmap (\(x,y) -> ( y,  x)) (reverse pts)
rotate90  pts  =  pts ++ fmap (\(x,y) -> ( y, -x)) pts
rotate180 pts  =  pts ++ fmap (\(x,y) -> (-x, -y)) pts
scale a b pts  =  fmap (\(x,y) -> ( a*x, b*y )) pts
segments  pts  =  zip pts $ tail pts

getOctant :: Maybe Ellipse -> Map Int ((Float,Float),(Float,Float))
getOctant (Just (Ellipse a b n)) =
    let f p = (1 - p**n)**(1/n)
        dp = iterate (\v -> v*0.9) 1.0
        ip = map (\p -> 1.0 -p) dp
        points s = 
            if (n > 1.0) 
            then (\p -> zip p (map f p)) ip
            else (\p -> zip (map f p) p) dp

    in fromList $  -- changes list to map (for listWithKey)
       zip [0..] $ -- annotates segments with index
       segments $  -- changes points to line segments
       scale a b $ 
       rotate180 $ -- doubles the point count
       rotate90 $  -- doubles the point count
       reflect45 $ -- doubles the point count
       takeWhile (\(x,y) -> x < y ) $ -- stop at 45 degree line
       points 0.9

getOctant Nothing = empty

lineAttrs :: Segment -> Map Text Text
lineAttrs ((x1,y1), (x2,y2)) =
    fromList [ ( "x1",    pack $ show (width/2+x1))
             , ( "y1",    pack $ show (height/2+y1))
             , ( "x2",    pack $ show (width/2+x2))
             , ( "y2",    pack $ show (height/2+y2))
             , ( "style", "stroke:red;stroke-width:2")
             ]    
         
showLine :: MonadWidget t m => Int -> Dynamic t Segment -> m ()
showLine _ dSegment = do
    elDynAttrNS' svgns "line" (lineAttrs <$> dSegment) $ return ()
    return ()

main = mainWidget $ do
    ta <- el "div" $ do
        text "a: "
        textInput def { _textInputConfig_initialValue = "200"}

    tb <- el "div" $ do
        text "b: "
        textInput def { _textInputConfig_initialValue = "200"}

    tn <- el "div" $ do
        text "n: "
        textInput def { _textInputConfig_initialValue = "2.5"}
    let 
        ab = zipDynWith toEllipse (fmap toFloat $ value ta) (fmap toFloat $ value tb)
        dEllipse = zipDynWith ($) ab (fmap toFloat $ value tn)
        dLines = fmap getOctant dEllipse -- a collection of lines
        
        dAttrs = constDyn $ fromList 
                     [ ("width" , pack $ show width)
                     , ("height", pack $ show height)
                     ]
        alertStyle = "style" =: "color:red" 
    elAttr "div" alertStyle $ dynText $ fmap (pack.showError) dEllipse
    elDynAttrNS' svgns "svg" dAttrs $ listWithKey dLines showLine
    return ()
