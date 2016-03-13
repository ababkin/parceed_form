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
    <*> (fromJust . readMay <$> o .: "cluster")
    <*> o .: "indicator"
    <*> (fromJust . readMay  <$> o .: "avgValue")
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
  , pIMGProb      :: !Double
  , pNInterviews  :: !Double
  } deriving Eq

newtype PairSortableByInterviews = PairSortableByInterviews {unPairSortableByInterviews :: Pair} deriving Eq
instance Ord PairSortableByInterviews where
  PairSortableByInterviews Pair{pNInterviews = p1} `compare` PairSortableByInterviews Pair{pNInterviews = p2} = p1 `compare` p2 


newtype PairSortableByIMGProb = PairSortableByIMGProb {unPairSortableByIMGProb :: Pair} deriving Eq
instance Ord PairSortableByIMGProb where
  PairSortableByIMGProb Pair{pIMGProb = p1} `compare` PairSortableByIMGProb Pair{pIMGProb = p2} = p1 `compare` p2 

newtype PairSortableByS1MinScore = PairSortableByS1MinScore {unPairSortableByS1MinScore :: Pair} deriving Eq
instance Ord PairSortableByS1MinScore where
  PairSortableByS1MinScore Pair{pS1MinScore = p1} `compare` PairSortableByS1MinScore Pair{pS1MinScore = p2} = p1 `compare` p2 

newtype PairSortableByS1MaxScore = PairSortableByS1MaxScore {unPairSortableByS1MaxScore :: Pair} deriving Eq
instance Ord PairSortableByS1MaxScore where
  PairSortableByS1MaxScore Pair{pS1MinScore = p1} `compare` PairSortableByS1MaxScore Pair{pS1MinScore = p2} = p1 `compare` p2 


newtype PairSortableByS2MinScore = PairSortableByS2MinScore {unPairSortableByS2MinScore :: Pair} deriving Eq
instance Ord PairSortableByS2MinScore where
  PairSortableByS2MinScore Pair{pS1MinScore = p1} `compare` PairSortableByS2MinScore Pair{pS1MinScore = p2} = p1 `compare` p2 


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
  }


