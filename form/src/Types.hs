{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

module Types where

import qualified Data.Map as M
import Data.Maybe (fromMaybe, fromJust)
import Safe (readMay)
import Control.Monad (forM_)
import Data.Monoid ((<>))
import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON),
  Value (..), object, (.:), encode, decode, (.:?))
import Data.Aeson.Types (typeMismatch, Parser, Object)
import qualified Data.ByteString.Lazy.Char8 as LBS


data ClusterDatum = ClusterDatum {
    cdSpecialty :: !String
  , cdCluster   :: !Int
  , cdIndicator :: !String
  , cdAvgValue  :: !Double
  }

instance FromJSON ClusterDatum where
  parseJSON (Object o) = ClusterDatum
    <$> o .: "specialty"
    <*> (read <$> o .: "cluster")
    <*> o .: "indicator"
    <*> (read <$> o .: "avgValue")
  parseJSON o = typeMismatch "ClusterDatum" o


data ProgramDatum = ProgramDatum {
    pdId                 :: !String
  , pdSchoolName         :: !String
  , pdSpecialty          :: !String
  , pdCity               :: !String
  , pdState              :: !String
  , pdResidencySpecialty :: !String
  , pdPrevGMEYears       :: !(Maybe Int)
  , pdCluster            :: !(Maybe Int)
  , pdLink               :: !String
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


data Specialty = Specialty {unSpecialty :: !String} deriving (Eq, Read, Show, Ord)

newtype City  = City {unCity :: String} deriving (Eq, Ord)
newtype State = State {unState :: String} deriving (Eq, Ord, Read, Show)
newtype Region = Region {unRegion :: String} deriving (Eq, Ord, Read, Show)

data Pair = Pair {
    pSpecialty    :: !Specialty
  , pCluster      :: !Cluster
  , pS1MinScore   :: !Double
  , pS1MaxScore   :: !Double
  , pS2MinScore   :: !Double
  , pH1Bprob      :: !Double
  , pJ1prob       :: !Double
  , pNInterviews  :: !Double
  } deriving Eq

instance Ord Pair where
  Pair{pNInterviews = p1} `compare` Pair{pNInterviews = p2} = p1 `compare` p2 


data Cluster = Cluster {
    cId       :: !Int
  , cPrograms :: ![Program]
  } deriving Eq

data Program = Program{
    pName   :: !String
  , pCity   :: !City
  , pState  :: !State
  , pMinYrs :: !(Maybe Int)
  , pLink   :: !String
  , pResidencySpecialty :: !Specialty
  } deriving Eq

instance Ord Program where
  Program{pState = ps1} `compare` Program{pState = ps2} = ps1 `compare` ps2

data In = In {
    iScore1     :: Int
  , iScore2     :: Int
  , iSpecialty  :: Specialty
  , iIntl       :: Bool
  , iExp        :: Int
  }


