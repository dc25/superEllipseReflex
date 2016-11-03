{-# LANGUAGE OverloadedStrings #-}
import Reflex
import Reflex.Dom
import Data.Text as DT
import Data.Maybe
import Data.Map
import Text.Read

data Ellipse = Ellipse { a :: Float
                       , b :: Float
                       , n :: Float
                       }

toMaybeEllipse :: Maybe Float -> Maybe Float -> Maybe Float -> Maybe Ellipse
toMaybeEllipse Nothing _ _ = Nothing
toMaybeEllipse _ Nothing _ = Nothing
toMaybeEllipse _ _ Nothing = Nothing
toMaybeEllipse (Just a) (Just b) (Just n) = Just $ Ellipse a b n

svgns :: Maybe Text
svgns = (Just "http://www.w3.org/2000/svg")

showError :: Maybe a -> String
showError Nothing = "invalid input"
showError _ = ""

toMap :: Maybe Ellipse -> Map Int Ellipse
toMap  = 
    fromList
    .fmap (\e -> ((,) 0 (fromMaybe (Ellipse 0.0 0.0 0.0) e))) 
    .Prelude.filter isJust -- Get rid of Nothing 
    .(:[]) -- Put the Maybe into a (one item) list 

toMaybeFloat :: Text -> Maybe Float
toMaybeFloat  = 
    (readMaybe::(String -> Maybe Float))  -- String to Maybe Int
    .unpack  -- Text to String

circleAttrs :: Ellipse -> Map Text Text
circleAttrs (Ellipse a b n) =
    fromList [ ( "cx",    "0")
             , ( "cy",    "0")
             , ( "r",     pack $ show a)
             , ( "style", "fill:green")
             ] 

showCircle :: MonadWidget t m => Int -> Dynamic t Ellipse -> m ()
showCircle _ dEllipse = do
    elStopPropagationNS svgns "g" Mousemove $ 
        elDynAttrNS' svgns "circle" (circleAttrs <$> dEllipse) $ return ()
    return ()

view :: MonadWidget t m => m ()
view = do 
    inputA <- el "div" $ textInput def
    inputB <- el "div" $ textInput def
    inputN <- el "div" $ textInput def
    let dMaybeA = fmap toMaybeFloat $ value inputA
        dMaybeB = fmap toMaybeFloat $ value inputB
        dMaybeN = fmap toMaybeFloat $ value inputN

        dMaybeAB = zipDynWith toMaybeEllipse dMaybeA dMaybeB 
        dMaybeABN = zipDynWith ($) dMaybeAB dMaybeN 

        dMap = fmap toMap dMaybeABN
        
        dAttrs = constDyn $ 
                        fromList [ ("width" , "600")
                                 , ("height", "400")
                                 , ("style" , "border:solid; margin:8em")
                                 ]

    dynText $ fmap (pack.showError) dMaybeABN
    elDynAttrNS' svgns "svg" dAttrs $ listWithKey dMap showCircle

    return ()

main = mainWidget view
