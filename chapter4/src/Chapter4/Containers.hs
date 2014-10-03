{-# LANGUAGE LambdaCase #-}

module Chapter4.Containers where

import qualified Data.Set as S
import qualified Data.Map as M

data ClientKind = GovOrgKind | CompanyKind | IndividualKind

data Client i = GovOrg  { clientId :: i, clientName :: String }
              | Company { clientId :: i, clientName :: String
                         , person :: Person, duty :: String }
              | Individual { clientId :: i, person :: Person }
              deriving Show

data Person = Person { firstName :: String, lastName  :: String }
              deriving Show

countClient :: M ClientKind (Set (Client Integer)) -> Client Integer -> M ClientKind (Set (Client Integer))
countClient results client = map (\GovOrg)


classifyClients :: [Client Integer] -> M ClientKind (Set (Client Integer))
classifyClients clients = foldl initValue clients
                            where initValue = M.fromList [(GovOrgKind, S.empty),
                              (CompanyKind, S.empty), (IndividualKind, S.empty)]

