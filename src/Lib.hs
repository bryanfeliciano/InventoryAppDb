module Lib where

import Control.Applicative
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Data.Time

data Gun = Gun {
    gunId :: Int ,
    name :: String ,
    description :: String ,
    lastReturned :: Day ,
    timesBorrowed :: Int
}

data User = User {
    userId :: Int ,
    userName :: String 
}

instance Show User where
    show user = mconcat [ show $ userId user , ".)" , userName user]

instance Show Gun where
    show gun = mconcat [show $ gunId gun , ".)", name gun , "\n description:", description gun , "\n last returned:", show $ lastReturned gun , "\n times borrowed: ", show $ timesBorrowed gun , "\n"]

addUser :: String -> IO ()
addUser username =  do  
    conn <- open "guns.db"
    execute conn "INSERT INTO users (username) VALUES (?)" (Only username)
    print "user added"
    close conn

withConn :: String -> (Connection -> IO()) -> IO()
withConn dbName action = do
    conn <- open dbName
    action conn
    close conn

checkout :: Int -> Int -> IO()
checkout userId gunId = withConn "guns.db" $ \conn-> do
    execute conn "INSERT INTO checkedout (user_id,gun_id) VALUES (?,?)" (userId,gunId)

-- class FromRow a where
--     fromRow :: RowParser a 

instance FromRow User where
    fromRow = User <$> field
                   <*> field

instance FromRow Gun where
    fromRow = Gun <$> field
                  <*> field
                  <*> field
                  <*> field
                  <*> field

-- query :: (FromRow r, ToRow q) => Connection -> Query -> q -> IO [r]
-- query_ :: FromRow r => Connection -> Query -> IO [r]