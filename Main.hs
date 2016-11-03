{-# LANGUAGE OverloadedStrings #-}
import Reflex
import Reflex.Dom
import Data.Text (Text, pack,unpack) 
import Data.Maybe
import Data.Map (Map, fromList)
import Text.Read

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
svgns = (Just "http://www.w3.org/2000/svg")

showError :: Maybe a -> String
showError Nothing = "invalid input"
showError _ = ""

toMap :: Maybe Ellipse -> Map Int Ellipse
toMap  = 
    fromList
    .fmap (\e -> ((,) 0 (fromMaybe (Ellipse 0.0 0.0 0.0) e))) 
    .filter isJust -- Get rid of Nothing 
    .(:[]) -- Put the Maybe into a (one item) list 

lineAttrs :: (Float,Float) -> Ellipse -> Map Text Text
lineAttrs (p0, p1) e =
    let
        evalPoint p (Ellipse a b n) = 
            let x = p*a
                y = ((1 - (x/a) ** n) ** (1/n) ) * b
            in (x,y)
    
        (x1,y1) = evalPoint p0 e
        (x2,y2) = evalPoint p1 e
    in fromList [ ( "x1",    pack $ show (x1*200.0))
                , ( "y1",    pack $ show (y1*200.0))
                , ( "x2",    pack $ show (x2*200.0))
                , ( "y2",    pack $ show (y2*200.0))
                , ( "style", "stroke:red;stroke-width:2")
                ]    

showCircle :: MonadWidget t m => Int -> Dynamic t Ellipse -> m ()
showCircle _ dEllipse = do
    let ps = take 201 $ iterate (+0.005) 0.0
        ps2 = zip ps $ tail ps
    elStopPropagationNS svgns "g" Mousemove $ 
        mapM_ (\p -> (elDynAttrNS' svgns "line" (lineAttrs p <$> dEllipse) $ return ())) ps2

view :: MonadWidget t m => m ()
view = do 
    ta <- (el "div" $ textInput def)
    tb <- (el "div" $ textInput def)
    tn <- (el "div" $ textInput def)
    let 
        ab = zipDynWith toEllipse (value ta) (value tb)
        abn = zipDynWith ($) ab (value tn)

        dMap = fmap toMap abn
        
        dAttrs = constDyn $ 
                        fromList [ ("width" , "600")
                                 , ("height", "400")
                                 , ("style" , "border:solid; margin:8em")
                                 ]

    dynText $ fmap (pack.showError) abn
    elDynAttrNS' svgns "svg" dAttrs $ listWithKey dMap showCircle

    return ()

main = mainWidget view
