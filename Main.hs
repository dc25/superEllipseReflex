{-# LANGUAGE OverloadedStrings #-}
import Reflex
import Reflex.Dom
import Data.Text (Text, pack,unpack) 
import Data.Map (Map, fromList, empty)
import Text.Read (readMaybe)

width = 600
height = 400

type Point = (Float,Float)
type Segment = (Point,Point)

data Ellipse = Ellipse {a :: Float, b :: Float, n :: Float}

toFloat :: Text -> Maybe Float
toFloat  = readMaybe.unpack  

toEllipse :: Maybe Float -> Maybe Float -> Maybe Float -> Maybe Ellipse
toEllipse (Just a) (Just b) (Just n) = Just $ Ellipse a b n
toEllipse _ _ _ = Nothing

svgns :: Maybe Text
svgns = Just "http://www.w3.org/2000/svg"

showError :: Maybe a -> String
showError Nothing = "invalid input"
showError _ = ""

lineAttrs :: Segment -> Map Text Text
lineAttrs ((x1,y1), (x2,y2)) =
    fromList [ ( "x1",    pack $ show (width/2+x1))
             , ( "y1",    pack $ show (height/2+y1))
             , ( "x2",    pack $ show (width/2+x2))
             , ( "y2",    pack $ show (height/2+y2))
             , ( "style", "stroke:red;stroke-width:2")
             ]    

reflect45 pts  =  pts ++ fmap (\(x,y) -> ( y,  x)) (reverse pts)
rotate90  pts  =  pts ++ fmap (\(x,y) -> ( y, -x)) pts
rotate180 pts  =  pts ++ fmap (\(x,y) -> (-x, -y)) pts
scale a b pts  =  fmap (\(x,y) -> ( a*x, b*y )) pts
segments  pts  =  zip pts $ tail pts

getOctant :: Maybe Ellipse -> Map Int ((Float,Float),(Float,Float))
getOctant (Just (Ellipse a b n)) =
    let f p = (1 - p**n)**(1/n)
        points s = 
            if (n > 1.0) 
            then (\p -> zip p (map f p)) $ iterate (\v -> v+s) 0.0  
            else (\p -> zip (map f p) p) $ iterate (\v -> v-s) 1.0

    in fromList $  -- change list to map (for listWithKey)
       zip [0..] $ -- annotate segments with index
       segments $  -- change points to line segments
       scale a b $ 
       rotate180 $ -- doubles the point count
       rotate90 $  -- doubles the point count
       reflect45 $ -- doubles the point count
       takeWhile (\(x,y) -> x < y ) $ -- until crossing 45 degrees
       points 0.05

getOctant Nothing = empty

showCircle :: MonadWidget t m => Int -> Dynamic t Segment -> m ()
showCircle _ dSegment = do
    elDynAttrNS' svgns "line" (lineAttrs <$> dSegment) $ return ()
    return ()

view :: MonadWidget t m => m ()
view = do 
    ta <- el "div" $ textInput def
    tb <- el "div" $ textInput def
    tn <- el "div" $ textInput def
    let 
        ab = zipDynWith toEllipse (fmap toFloat $ value ta) (fmap toFloat $ value tb)
        ellipse = zipDynWith ($) ab (fmap toFloat $ value tn)
        dMap = fmap getOctant ellipse 
        
        dAttrs = constDyn $ fromList 
                     [ ("width" , pack $ show width)
                     , ("height", pack $ show height)
                     ]

    dynText $ fmap (pack.showError) ellipse
    elDynAttrNS' svgns "svg" dAttrs $ listWithKey dMap showCircle
    return ()

main = mainWidget view
