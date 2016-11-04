{-# LANGUAGE OverloadedStrings #-}
import Reflex
import Reflex.Dom
import Data.Text (Text, pack,unpack) 
import Data.Maybe
import Data.Map (Map, fromList, empty)
import Text.Read

type Point = (Float,Float)
type Segment = (Point,Point)

data Ellipse = Ellipse { a :: Float
                       , b :: Float
                       , n :: Float
                       }

toMaybeFloat :: Text -> Maybe Float
toMaybeFloat  = 
    (readMaybe::(String -> Maybe Float))  -- String to Maybe Int
    .unpack  -- Text to String

toMaybeEllipse :: Maybe Float -> Maybe Float -> Maybe Float -> Maybe Ellipse
toMaybeEllipse Nothing _ _ = Nothing
toMaybeEllipse _ Nothing _ = Nothing
toMaybeEllipse _ _ Nothing = Nothing
toMaybeEllipse (Just a) (Just b) (Just n) = Just $ Ellipse a b n

toEllipse :: Text -> Text -> Text -> Maybe Ellipse
toEllipse tA tB tN =
    toMaybeEllipse (toMaybeFloat tA) (toMaybeFloat tB) (toMaybeFloat tN)

svgns :: Maybe Text
svgns = Just "http://www.w3.org/2000/svg"

showError :: Maybe a -> String
showError Nothing = "invalid input"
showError _ = ""

lineAttrs :: Segment -> Map Text Text
lineAttrs ((x1,y1), (x2,y2)) =
    fromList [ ( "x1",    pack $ show (150+x1))
             , ( "y1",    pack $ show (150+y1))
             , ( "x2",    pack $ show (150+x2))
             , ( "y2",    pack $ show (150+y2))
             , ( "style", "stroke:red;stroke-width:2")
             ]    

filter2 :: Ord a => (a -> a -> Bool) -> [a] -> [a]
filter2 f (x0:x1:xs) = 
    let unfiltered = filter2 f (x1:head xs:tail xs) 
    in if f x0 x1 
       then x0 : unfiltered
       else unfiltered


getOctant :: Maybe Ellipse -> Map Int ((Float,Float),(Float,Float))
getOctant (Just (Ellipse a b n)) =
    let 
        stepSize = 0.05
        params = if n > 1.0 
                 then iterate (+stepSize) 0.0 
                 else iterate (\v -> v-stepSize) 1.0
        getPoint p = (p, (1 - p**n)**(1/n))
        points = fmap getPoint params
        segments = zip points $ tail points
        eighth = takeWhile (\((x0,y0),(x1,y1)) -> abs (x1-x0) > abs (y1-y0)) segments
        quarter pts = pts++fmap (\((x0,y0),(x1,y1)) -> ((y0,x0),(y1,x1))) pts
        half pts = pts ++ fmap (\((x0,y0),(x1,y1)) -> ((-x0,y0),(-x1,y1))) pts
        whole pts = pts ++ fmap (\((x0,y0),(x1,y1)) -> ((x0,-y0),(x1,-y1))) pts
        scaled pts = fmap (\((x0,y0),(x1,y1)) -> ((a*x0,b*y0),(a*x1,b*y1))) pts
    in fromList $ zip [0..] $ scaled $ whole $ half $ quarter $ eighth

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
        ab = zipDynWith toEllipse (value ta) (value tb)
        abn = zipDynWith ($) ab (value tn)

        dMap = fmap getOctant abn
        
        dAttrs = constDyn $ 
                        fromList [ ("width" , "600")
                                 , ("height", "400")
                                 , ("style" , "border:solid; margin:8em")
                                 ]
    dynText $ fmap (pack.showError) abn
    elDynAttrNS' svgns "svg" dAttrs $ listWithKey dMap showCircle
    return ()

main = mainWidget view
