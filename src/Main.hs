{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

import Reflex.Dom
import qualified Data.Map as M
import Data.List (nub)
import Data.Maybe (fromMaybe)
import Safe (readMay)
import Control.Monad (forM_)
import Data.Monoid ((<>))


data Specialty = Specialty {unSpecialty :: String} deriving (Eq, Read, Show, Ord)

newtype City  = City String deriving Eq
newtype State = State String deriving Eq

data Pair = Pair {
    pSpecialty  :: Specialty
  , pCluster    :: Cluster
  , pS1MinScore :: Int
  , pS1MaxScore :: Int
  , pS2MinScore :: Int
  , pH1Bprob    :: Double
  , pJ1prob     :: Double
  , pNInterviews :: Int
  }

{- type Clusters = M.Map Specialty Cluster -}

data Cluster = Cluster {
    cId       :: Int
  , cPrograms :: [Program]
  }

data Program = Program{
    pName   :: String
  , pCity   :: City
  , pState  :: State
  , pMinYrs :: Int
  , pLink   :: String
  } deriving Eq

data In = In {
    iScore      :: Int
  , iSpecialty  :: Specialty
  , iIntl       :: Bool
  }


cluster1 = Cluster 1 [Program "some program" (City "New York") (State "NY") 3 "http://some.where.com"]

pairs :: [Pair]
pairs = [Pair {
      pSpecialty  = Specialty "CARDIOLOGY"
    , pCluster    = cluster1
    , pS1MinScore = 100
    , pS1MaxScore = 200
    , pS2MinScore = 100
    , pH1Bprob    = 0.1
    , pJ1prob     = 0.6
    , pNInterviews = 20
    } 
  , Pair {
      pSpecialty  = Specialty "PSYCHIATRY"
    , pCluster    = cluster1
    , pS1MinScore = 110
    , pS1MaxScore = 210
    , pS2MinScore = 200
    , pH1Bprob    = 0.5
    , pJ1prob     = 0.9
    , pNInterviews = 30
    } 
  ]

specialties :: [Pair] -> [Specialty] 
specialties = nub . map pSpecialty 

specialtiesMap :: [Pair] -> M.Map Specialty String
specialtiesMap = M.fromList . map (\s@(Specialty t) -> (s, t)) . specialties



stylesheet :: MonadWidget t m => m ()
stylesheet = elAttr "link" ("rel" =: "stylesheet" <> "href" =: "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/css/bootstrap.min.css") $ return ()

main = mainWidgetWithHead stylesheet $ do

  input <- inputForm $ do
    score <- textField "Score"
    maybeScore <- mapDyn readMay score 

    specialty <- selectorField "Specialty" (head $ specialties pairs) (constDyn $ specialtiesMap pairs)

    international <- selectorField "International" False (constDyn $ M.fromList [(False, "No"), (True, "Yes")])


    combineDyn (\maybeScore (specialty, international) -> In (fromMaybe 0 maybeScore) specialty international) maybeScore 
     =<< combineDyn (\x y -> (x,y)) specialty international

    {- elAttr "div" ("style" =: s) $ text "Othello" -}
    {- setup -}
    {- where -}
      {- s = "font-size: 50px; margin-left: 155px; font-family: Helvetica; color: steelblue" -}


  {- submitEvent <- button "Submit" -}

  {- el "div" . widgetHold blank . ffor submitEvent $ \_ -> -}
  {- el "div" . dynText =<< mapDyn (show . iScore) input -}

  dyn =<< mapDyn (renderPrograms . calculate) input

  return ()


inputForm = elAttr "form" ("style" =: "width: 600px;")

textField label = do
  elAttr "div" ("class" =: "form-group") $ do
    elAttr "label" ("for" =: "") $ text label
    _textInput_value <$> (textInput $ def & textInputConfig_attributes .~ constDyn ("class" =: "form-control") )


selectorField label z mp = do
  elAttr "div" ("class" =: "form-group") $ do
    elAttr "label" ("for" =: "") $ text label
    _dropdown_value <$> dropdown z mp (def & dropdownConfig_attributes .~ constDyn ("class" =: "form-control") )



{- renderPrograms :: (Reflex t, MonadHold t m) => [Program] -> m () -}
renderPrograms ps =
  {- elAttr "table" ("style" =: "border-width: 1; border-style: solid") $ do -}
  elAttr "table" ("class" =: "table") $ do
    el "tr" $ do
      el "th" $ text "Program name"
      el "th" $ text "Link"
    forM_ ps $ \p ->
      el "tr" $ do
        el "td" . text $ pName p
        el "td" . text $ pLink p


calculate :: In -> [Program]
calculate input = nub . concatMap (cPrograms . pCluster) . filterByS1Score . filterBySpecialty $ pairs
  where
    filterByS1Score = filter (\p -> pS1MinScore p <= score && pS1MaxScore p >= score )
    filterBySpecialty = filter ((== specialty) . pSpecialty)

    score = iScore input
    specialty = iSpecialty input
    intl = iIntl input
