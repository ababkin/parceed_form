{-# LANGUAGE JavaScriptFFI      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

import Reflex.Dom
import qualified Data.Map as M
import Data.List (nub, foldl')
import Data.Maybe (fromMaybe, fromJust)
import Safe (readMay)
import Control.Monad (forM_)
import Data.Monoid ((<>))
import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON),
  Value (..), object, (.:), encode, decode, (.:?))
import Data.Aeson.Types (typeMismatch, Parser, Object)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy.Char8 as LBS

import qualified GHCJS.Types    as T
import qualified GHCJS.DOM.Types    as DT
import qualified GHCJS.Foreign  as F

foreign import javascript unsafe "clusterData()" clusterData :: IO (T.JSString)
foreign import javascript unsafe "programData()" programData :: IO (T.JSString)


data ClusterDatum = ClusterDatum {
    cdSpecialty :: String
  , cdCluster   :: Int
  , cdIndicator :: String
  , cdAvgValue  :: Double
  }

instance FromJSON ClusterDatum where
  parseJSON (Object o) = ClusterDatum
    <$> o .: "specialty"
    <*> (read <$> o .: "cluster")
    <*> o .: "indicator"
    <*> (read <$> o .: "avgValue")
  parseJSON o = typeMismatch "ClusterDatum" o


data ProgramDatum = ProgramDatum {
    pdId                 :: String
  , pdSchoolName         :: String
  , pdSpecialty          :: String
  , pdCity               :: String
  , pdState              :: String
  , pdResidencySpecialty :: String
  , pdPrevGMEYears       :: Maybe Int
  , pdCluster            :: Maybe Int
  , pdLink               :: String
  }

instance FromJSON ProgramDatum where
  parseJSON (Object o) = ProgramDatum
    <$> o .: "id"
    <*> o .: "schoolName"
    <*> o .: "specialty"
    <*> o .: "city"
    <*> o .: "state"
    <*> o .: "residencySpecialty"
    <*> (readMay <$> o .: "prevGMEYears")
    <*> (readMay <$> o .: "cluster")
    <*> o .: "link"
  parseJSON o = typeMismatch "ProgramDatum" o


data Specialty = Specialty {unSpecialty :: String} deriving (Eq, Read, Show, Ord)

newtype City  = City String deriving Eq
newtype State = State String deriving Eq

data Pair = Pair {
    pSpecialty    :: Specialty
  , pCluster      :: Cluster
  , pS1MinScore   :: Double
  , pS1MaxScore   :: Double
  , pS2MinScore   :: Double
  , pH1Bprob      :: Double
  , pJ1prob       :: Double
  , pNInterviews  :: Double
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
  , pMinYrs :: Maybe Int
  , pLink   :: String
  } deriving Eq

data In = In {
    iScore      :: Int
  , iSpecialty  :: Specialty
  , iIntl       :: Bool
  }



dataToPairs :: [ProgramDatum] -> [ClusterDatum] -> [Pair]
dataToPairs pds = M.elems . M.mapWithKey makePair . cdMap
  where
    makePair (cdSpecialty, cdCluster) vs = fromJust $ Pair 
      <$> pure (Specialty cdSpecialty)
      <*> pure (Cluster cdCluster $ programs cdCluster pds)
      <*> lookup "Average Step 1 score min" vs
      <*> lookup "Average Step 1 score max" vs
      <*> lookup "Step 2 Minimum Score" vs
      <*> lookup "H1-B visa" vs
      <*> lookup "J-1 visa" vs
      <*> lookup "Interviews conducted last year for first year positions" vs


    cdMap :: [ClusterDatum] -> M.Map (String, Int) [(String, Double)]
    cdMap = foldl' accum M.empty 
    
    accum mp ClusterDatum{cdSpecialty, cdCluster, cdIndicator, cdAvgValue} = 
      M.unionWith (++) mp $ M.singleton (cdSpecialty, cdCluster) [(cdIndicator, cdAvgValue)]


programs :: Int -> [ProgramDatum] -> [Program]
programs cl = map fromDatum . filter ((== Just cl) . pdCluster)
  where
    fromDatum :: ProgramDatum -> Program
    fromDatum ProgramDatum{pdSchoolName, pdCity, pdState, pdPrevGMEYears, pdLink} =
      Program pdSchoolName (City pdCity) (State pdState) pdPrevGMEYears pdLink



{- cluster1 = Cluster 1 [Program "some program" (City "New York") (State "NY") 3 "http://some.where.com"] -}

{- pairs :: [Pair] -}
{- pairs = [Pair { -}
      {- pSpecialty  = Specialty "CARDIOLOGY" -}
    {- , pCluster    = cluster1 -}
    {- , pS1MinScore = 100 -}
    {- , pS1MaxScore = 200 -}
    {- , pS2MinScore = 100 -}
    {- , pH1Bprob    = 0.1 -}
    {- , pJ1prob     = 0.6 -}
    {- , pNInterviews = 20 -}
    {- }  -}
  {- , Pair { -}
      {- pSpecialty  = Specialty "PSYCHIATRY" -}
    {- , pCluster    = cluster1 -}
    {- , pS1MinScore = 110 -}
    {- , pS1MaxScore = 210 -}
    {- , pS2MinScore = 200 -}
    {- , pH1Bprob    = 0.5 -}
    {- , pJ1prob     = 0.9 -}
    {- , pNInterviews = 30 -}
    {- }  -}
  {- ] -}

specialties :: [Pair] -> [Specialty] 
specialties = nub . map pSpecialty 

specialtiesMap :: [Pair] -> M.Map Specialty String
specialtiesMap = M.fromList . map (\s@(Specialty t) -> (s, t)) . specialties



stylesheet :: MonadWidget t m => m ()
stylesheet = elAttr "link" ("rel" =: "stylesheet" <> "href" =: "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/css/bootstrap.min.css") $ return ()

main = mainWidgetWithHead stylesheet $ do

  
  pairs <- liftIO $ dataToPairs 
                      <$> (fromJS <$> programData) 
                      <*> (fromJS <$> clusterData)

  {- programs <- fromJS <$> programData -}

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

  dyn =<< mapDyn (renderPrograms . calculate pairs) input

  return ()

fromJS :: FromJSON a => T.JSString -> a
fromJS = fromJust . decode . LBS.pack . DT.fromJSString



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


calculate :: [Pair] -> In -> [Program]
calculate pairs input = nub . concatMap (cPrograms . pCluster) . filterByS1Score . filterBySpecialty $ pairs
  where
    filterByS1Score = filter (\p -> pS1MinScore p <= fromIntegral score && pS1MaxScore p >= fromIntegral score )
    filterBySpecialty = filter ((== specialty) . pSpecialty)

    score = iScore input
    specialty = iSpecialty input
    intl = iIntl input
