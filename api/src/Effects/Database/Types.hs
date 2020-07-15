{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Effects.Database.Types where

import           Data.Aeson
import           Data.Text
import           Database.Persist               ( )
import           Database.Persist.Sql           ( Migration
                                                , addMigration
                                                )
import           Database.Persist.TH

-- import qualified Model.Brand as Brand

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
  Brand
      name Text
      url Text
      deriving Show
  Sender
      from Text
      returnPath Text
      brandId BrandId
      deriving Show
  |]
