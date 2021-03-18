{-# LANGUAGE TypeFamilies, FlexibleInstances, FlexibleContexts #-}

module Bead.Persistence.SQL.User where

import           Control.Applicative
import           Data.ByteString (ByteString)
import qualified Data.Function as Fun (on)
import           Data.List (groupBy, sortBy)
import           Data.Maybe
import qualified Data.Text as Text

import           Database.Persist.Sql
import           Database.Esqueleto (select, exists, from, on, where_, orderBy, asc, limit, InnerJoin(InnerJoin), val, (^.), Value(unValue))
import qualified Database.Esqueleto as Esq

import qualified Bead.Domain.Entities as Domain
import qualified Bead.Domain.Relationships as Domain
import           Bead.Persistence.SQL.Class
import           Bead.Persistence.SQL.Entities
import qualified Bead.Persistence.SQL.FileSystem as FS
import           Bead.Persistence.SQL.JSON

-- * User persistence

-- Save the current user, if the timezone and the role is stored in the database, otherwise
-- do nothing
saveUser :: Domain.User -> Persist ()
saveUser = void . insert . fromDomainValue

-- Calculates the personal information about the user
personalInfo :: Domain.Username -> Persist Domain.PersonalInfo
personalInfo username = do
  user <- loadUser username
  return $! Domain.withUser user $ \role _username _email name timezone language uid ->
    Domain.PersonalInfo (role, name, timezone, language, uid)

-- Select users who satiesfies the given predicate
filterUsers :: (Domain.User -> Bool) -> Persist [Domain.User]
filterUsers pred = do
  users <- selectList [] []
  return $! filter pred $ map (toDomainValue . entityVal) users

allAdministrators :: Persist [Domain.User]
allAdministrators = do
  admins <- select $ from $ \u -> do
    where_ (u ^. UserRole Esq.==. val "\"GroupAdmin\"" Esq.||. u ^. UserRole Esq.==. val "\"CourseAdmin\"")
    return u
  return $ sortBy (Domain.compareHun `Fun.on` Domain.u_name) $ map (toDomainValue . entityVal) admins

  where
    compareUser :: Domain.User -> Domain.User -> Ordering
    compareUser u1 u2 = case compare (Domain.u_name u1) (Domain.u_name u2) of
                          LT -> LT
                          EQ -> compare (Domain.u_username u1) (Domain.u_username u2)
                          GT -> GT

-- Loads the user information for the given username, supposing that the user
-- exists in the database
loadUser :: Domain.Username -> Persist Domain.User
loadUser = Domain.usernameCata $ \username -> do
  mUserEnt <- getBy . UniqueUsername $ Text.pack username
  case mUserEnt of
    Nothing -> persistError "loadUser" $ "Username is not found: " ++ username
    Just (Entity _entityKey userEnt) -> return $! toDomainValue userEnt

-- Updates the user information
updateUser :: Domain.User -> Persist ()
updateUser user = do
  userId <- entityKey <$> (getByUsername $ Domain.u_username user)
  update userId $ Domain.withUser user $ \role username email name timezone language uid ->
    [ UserRole     =. (encodeRole role)
    , UserUsername =. (Domain.usernameCata Text.pack username)
    , UserEmail    =. (Domain.emailCata Text.pack email)
    , UserName     =. (Text.pack name)
    , UserTimeZone =. (encodeTimeZone timezone)
    , UserLanguage =. (Domain.languageCata Text.pack language)
    , UserUid      =. (Domain.uid Text.pack uid)
    ]

-- Checks if the user is already in the database
doesUserExist :: Domain.Username -> Persist Bool
doesUserExist = Domain.usernameCata $ \username -> do
  mUserEnt <- getBy . UniqueUsername $ Text.pack username
  return $! maybe False (const True) mUserEnt

-- Creates a description from the given username
userDescription :: Domain.Username -> Persist Domain.UserDesc
userDescription username = do
  user <- loadUser username
  return $! Domain.withUser user $ \_role username _email name _timezone _language ->
    Domain.UserDesc username name

uidToUsername :: Domain.Uid -> Persist Domain.Username
uidToUsername uid = do
  users <- select $ from $ \u -> do
    where_ (u ^. UserUid Esq.==. val (Domain.uid Text.pack uid))
    limit 1
    return $ u ^. UserUsername
  case users of
    [] -> persistError "uidToUsername" $ "user is not found: " ++ show uid
    [user] -> return . Domain.Username . Text.unpack . unValue $ user
    _ -> persistError "uidToUsername" $ "impossible: multiple users are found: " ++ show uid

-- Lists all the submission keys for the submissions that submitted by the user
-- for the given assignment
userSubmissions :: Domain.Username -> Domain.AssignmentKey -> Persist [Domain.SubmissionKey]
userSubmissions username key =
  withUser
    username
    (persistError "userSubmissions" $ "user is not found: " ++ show username)
    (\userEnt ->
       map (toDomainKey . userSubmissionOfAssignmentSubmission . entityVal) <$>
         selectList
           [ UserSubmissionOfAssignmentUser       ==. entityKey userEnt
           , UserSubmissionOfAssignmentAssignment ==. toEntityKey key
           ] [])

-- Lists all the courses that are administrated by the user
administratedCourses :: Domain.Username -> Persist [(Domain.CourseKey, Domain.Course)]
administratedCourses username =
  withUser
    username
    (persistError "administratedCourse" $ "user is not found: " ++ show username)
    (\userEnt -> do
       cks <- map (adminsOfCourseCourse . entityVal) <$>
                selectList [AdminsOfCourseAdmin ==. entityKey userEnt] []
       catMaybes <$> mapM getWithKey cks)
  where
    getWithKey k = do
      mVal <- get k
      return $ fmap (\x -> (toDomainKey k,toDomainValue x)) mVal

-- Lists all the groups that are administrated by the user
administratedGroups :: Domain.Username -> Persist [(Domain.CourseKey, Domain.Course, [(Domain.GroupKey, Domain.Group)])]
administratedGroups username = do
  groups <- select $ from $ \(u `InnerJoin` ag `InnerJoin` g `InnerJoin` gc `InnerJoin` c) -> do
    on (u ^. UserId Esq.==. ag ^. AdminsOfGroupAdmin Esq.&&.
        ag ^. AdminsOfGroupGroup Esq.==. g ^. GroupId Esq.&&.
        g ^. GroupId Esq.==. gc ^. GroupsOfCourseGroup Esq.&&.
        gc ^. GroupsOfCourseCourse Esq.==. c ^. CourseId)
    where_ (u ^. UserUsername Esq.==. val (Domain.usernameCata Text.pack username))
    orderBy [ asc (c ^. CourseId) ]
    return (c, g)
  return . extract . groupBy ((==) `Fun.on` (entityKey . fst)) $ groups

      where
        extract :: [[(Entity Course, Entity Group)]] -> [(Domain.CourseKey, Domain.Course, [(Domain.GroupKey, Domain.Group)])]
        extract = map (\grps@((c,_):_) -> (toDomainKey (entityKey c), toDomainValue (entityVal c), [(toDomainKey (entityKey g), toDomainValue (entityVal g)) | (_, g) <- grps]))


-- Returns True if the given user administrates the given group.
isAdminOfGroup :: Domain.Username -> Domain.GroupKey -> Persist Bool
isAdminOfGroup username groupKey = do
  records <- select $ from $ \(ag `InnerJoin` u) -> do
    on (ag ^. AdminsOfGroupAdmin Esq.==. u ^. UserId)
    where_ (ag ^. AdminsOfGroupGroup Esq.==. val (fromDomainKey groupKey) Esq.&&.
            u ^. UserUsername Esq.==. val (Domain.usernameCata Text.pack username))
    limit 1
  return $ not $ null records

-- * Users file upload

-- | Copies the given file with the given filename to the users data directory
copyFile :: Domain.Username -> FilePath -> Domain.UsersFile FilePath -> Persist ()
copyFile = FS.copyUsersFile

-- | Saves a file with the given filename and contents in the users data directory
saveFile :: Domain.Username -> FilePath -> Domain.UsersFile ByteString -> Persist ()
saveFile = FS.saveUsersFile

-- | Lists user's all files.
listFiles :: Domain.Username -> Persist [(Domain.UsersFile FilePath, Domain.FileInfo)]
listFiles = FS.listFiles

-- | Gets the path for the user's file
getFile :: Domain.Username -> Domain.UsersFile FilePath -> Persist FilePath
getFile = FS.getFile

-- Select all the existing usernames for the given user id list
usernames :: [UserId] -> Persist [Domain.Username]
usernames userIds = catMaybes <$> (mapM toUsername userIds)
  where
    toUsername e = do
      mUser <- get e
      return $! (Domain.Username . Text.unpack . userUsername <$> mUser)

-- TODO: Write user unit tests
