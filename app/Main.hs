module Main where

import Control.Applicative
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Data.Time

addUser :: String -> IO ()
addUser username =  do  
                    conn <- open "guns.db"
                    execute conn "INSERT INTO users (username) VALUES (?)" (Only username)
                    print "user added"
                    close conn
-- takes in the db value , and an IO action
withConn :: String -> (Connection -> IO ()) -> IO ()
withConn dbName operation = do
                            conn <- open dbName
                            operation conn 
                            close conn

addUserOperation ::  String -> Connection -> IO () 
addUserOperation user conn = do
        execute conn "INSERT INTO users (username) VALUES (?)" (Only user)
        putStrLn "user added"       

{-
How to use:
> withConn "guns.db" (addUserOperation "Zezima")
user added
-- here ^ you are creating a partial function, and using complicated operations, let us simplify
-}        

addUserNew :: String -> IO ()
addUserNew user = withConn "guns.db" $
                        (\ conn ->  do 
                                    execute conn  "INSERT INTO users (username) VALUES (?)" (Only user)
                                    putStrLn "added user"
                                    )

-- lambdas are tedious, so we create,
executeWrapper :: ToRow q =>  Query -> q -> String -> (Connection -> IO ())
executeWrapper sqlStml tuples successMsg =
            (\ conn ->  do 
                        execute conn sqlStml tuples
                        putStrLn successMsg
            )
      
-- now,
addUserNew2 :: String -> IO ()
addUserNew2 user = withConn "guns.db" $
                  executeWrapper "INSERT INTO users (username) VALUES (?)" (Only user) "added the user"


checkoutMy :: Int -> Int -> IO ()
checkoutMy userId gunId = withConn "guns.db" $
        executeWrapper  "INSERT INTO checkedout (user_id,gun_id) VALUES (?,?)" 
                        (userId,gunId) 
                        "Checked out the gun"

                            

checkout :: Int -> Int -> IO ()
checkout userId gunId = withConn "guns.db" $
                         \conn -> do
                           execute conn
                             "INSERT INTO checkedout (user_id,gun_id) VALUES (?,?)"
                             (userId,gunId)

                          

--------------------------------------------
--------------------------------------------
--------------------------------------------
--------------------------------------------

-- Reading from DB, and
-- Converting rows into Haskell types.

data Gun = Gun
 { gunId :: Int
 , name :: String
 , description :: String
 , lastReturned :: Day
 , timesBorrowed :: Int
 }

data User = User
 { userId :: Int
 , userName :: String
 }

instance Show User where
   show user = mconcat [ show $ userId user
                       , ".)  "
                       , userName user]

instance Show Gun where
   show gun = mconcat [ show $ gunId gun
                       , ".) "
                       , name gun
                       , "\n description: "
                       , description gun
                       , "\n last returned: "
                       , show $ lastReturned gun
                       , "\n times borrowed: "
                       , show $ timesBorrowed gun
                       , "\n"]

                       
instance FromRow User where
   fromRow = User <$> field
                  <*> field

instance FromRow Gun where
   fromRow = Gun <$> field
                  <*> field
                  <*> field
                  <*> field
                  <*> field


printUsers :: IO ()
printUsers = do 
        withConn "guns.db" $
                (\ conn ->
                           do
                           values <- query_ conn "SELECT * FROM users"  :: IO [User]    
                           mapM_ print values          
                )            


-- can execute all queries returning a gun
printgunQuery :: Query -> IO ()
printgunQuery query = withConn "guns.db" $
                        ( \ conn -> 
                                    do
                                    values <- query_ conn query :: IO [Gun]
                                    mapM_ print values)
                               
-- to use the above,
printguns :: IO ()
printguns =  printgunQuery "SELECT * FROM guns;"

printAvailable :: IO ()
printAvailable = printgunQuery $ mconcat [ "select * from guns "
                                          , "where id not in "
                                          , "(select gun_id from checkedout);"]

printCheckedout :: IO ()
printCheckedout = printgunQuery $ 
                mconcat [ "select * from guns ", 
                                "where id in ", 
                                "(select gun_id from checkedout);"]                                          

------------------------------------------
------------------------------------------
------------------------------------------
-- Update the DB  (important) ------------


-- first obtain the gun
selectgun :: Connection -> Int -> IO (Maybe Gun)
selectgun conn gunId = do
   resp <- query conn
           "SELECT * FROM guns WHERE id = (?)"
           (Only gunId) :: IO [Gun]
   return $ firstOrNothing resp

firstOrNothing :: [a] -> Maybe a
firstOrNothing [] = Nothing
firstOrNothing (x:_) = Just x


-- then update the gun (the Haskell type)
updategun :: Gun -> Day -> Gun
updategun gun date = gun
   { lastReturned = date
   , timesBorrowed = 1 + timesBorrowed gun
   }


-- then put this new gun values in the DB,
updateOrWarn :: Maybe Gun -> IO ()
updateOrWarn Nothing = print "id not found"
updateOrWarn (Just gun) =  withConn "guns.db" $
                            \conn -> do
                              let q = mconcat ["UPDATE gunS SET  "
                                              ,"lastReturned = ?,"
                                              ," timesBorrowed = ? "
                                              ,"WHERE ID = ?;"]

                              execute conn q (lastReturned gun
                                             , timesBorrowed gun
                                             , gunId gun)
                              print "gun updated"


---------
-- putting it all together

{-
 updategunTable, takes a gunId, fetches the current date, 
 and then performs the necessary steps to update the gun in the table.
-}

updategunTable gunId = do
                         conn <- open "guns.db"
                         chosengun <- selectgun conn gunId
                         currentDate <-  utctDay <$> getCurrentTime  
                         let newgun = (fmap updategun chosengun) <*> (pure currentDate) -- we need a Maybe type for date, don't use Just, use pure to convert

                         --let newgun = updategun removeMaybeContextgun currentDate
                         -- nowt hat we have our updated gun,
                         -- we can use, updateOrWarn ::  Maybe gun -> IO ()

                         -- newgun is a Maybegun, and updateOrWarn takes just that as a parameter
                         updateOrWarn newgun
                         close conn



-------------------------------
-------------------------------
-------------------------------
-- Deleting records -------------
checkin :: Int -> IO ()
checkin gunId =  withConn "guns.db" $
                     \conn -> do
                       execute conn
                         "DELETE FROM checkedout WHERE gun_id = (?);"
                         (Only gunId)

checkinAndUpdate :: Int -> IO ()
checkinAndUpdate gunId = do
   checkin gunId
   updategunTable gunId
-- why? so that it can update that the gun was borrowed, it deletes the gun from the checkout entries   

------------------------------------------
------------------------------------------
------------------------------------------
--- Creating a User Interface of sorts ---




promptAndAddUser :: IO ()
promptAndAddUser = do
   print "Enter new user name"
   userName <- getLine
   addUser userName

promptAndCheckout :: IO ()
promptAndCheckout = do
   print "Enter the id of the user"
   userId <- pure read <*> getLine
   print "Enter the id of the gun"
   gunId <- pure read <*> getLine
   checkout userId gunId

promptAndCheckin :: IO ()
promptAndCheckin = do
   print "enter the id of gun"
   gunId <- pure read <*> getLine
   checkinAndUpdate gunId


performCommand :: String -> IO ()
performCommand command
   | command == "users" = printUsers >> main
   | command == "guns" = printguns >> main
   | command == "adduser" = promptAndAddUser >> main
   | command == "checkout" = promptAndCheckout >> main
   | command == "checkin" = promptAndCheckin >> main
   | command == "in" = printAvailable >> main
   | command == "out" = printCheckedout >> main
   | command == "quit" = print "bye!"
   | otherwise = print "Sorry command not found" >> main





main :: IO ()
main = do
   print "Enter a command"
   command <- getLine
   performCommand command