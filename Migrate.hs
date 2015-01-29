{-# LANGUAGE OverloadedStrings #-} 
module Main where

import Control.Exception (bracket)
import Data.Acid
import Data.Acid.Local
import Data.IxSet ((@=), getOne)
import qualified Data.IxSet as IxSet
import Data.Map (Map)
import Data.Monoid ((<>))
import qualified Data.Set as Set
import Data.Text (Text, pack)
import Data.Text.IO as T
import Happstack.Auth.Core.Auth as Auth
import Happstack.Auth.Core.Profile as Profile
import Happstack.Authenticate.Core as Authenticate
import Happstack.Authenticate.Password.Core as Password
import Happstack.Authenticate.OpenId.Core as OpenId
import Web.Authenticate.OpenId (Identifier)

main :: IO ()
main =
  bracket (openLocalStateFrom "_state/auth"    initialAuthState)                       closeAcidState $ \auth ->
  bracket (openLocalStateFrom "_state/profile" initialProfileState)                    closeAcidState $ \profile ->
  bracket (openLocalStateFrom "_new_state/authenticate" initialAuthenticateState)           closeAcidState $ \authenticateState ->
  bracket (openLocalStateFrom "_new_state/authenticate/password" initialPasswordState) closeAcidState $ \passwordState ->
  bracket (openLocalStateFrom "_new_state/authenticate/openid" initialOpenIdState)     closeAcidState $ \openIdState ->
    do as <- query auth    AskAuthState
       ps <- query profile GetProfileState
       let users = map (mkUser as (authUserMap ps)) (IxSet.toList $ profiles ps)
       mapM_ (insertUser authenticateState passwordState openIdState) users
       return ()
  where
    insertUser :: AcidState AuthenticateState
               -> AcidState PasswordState
               -> AcidState OpenIdState
               -> Either Text (User, Either Password.HashedPass Identifier)
               -> IO ()
    insertUser authenticateState passwordState _openIdState (Right (user, Left pw)) =
      do T.putStrLn $ _unUsername $ _username user
         update authenticateState (UpdateUser user)
    insertUser authenticateState _passwordState openIdState (Right (user, Right ident)) =
      do T.putStrLn $ _unUsername $ _username user
         update authenticateState (UpdateUser user)
    insertUser _ _ _ (Left err) = T.putStrLn err
    mkUser :: AuthState -> Map AuthId Profile.UserId -> Profile -> Either Text (User, Either Password.HashedPass Identifier)
    mkUser authState auMap (Profile u@(Profile.UserId i) auths nickName) =
      case Set.toList auths of
        [authId] ->
          case getOne $ (authMaps authState) @= authId of
            (Just (AuthMap authMethod _)) ->
              case authMethod of
                (AuthUserPassId upid) ->
                  case getOne $ (userPasses authState) @= upid of
                    (Just (Auth.UserPass (Auth.UserName name) (Auth.HashedPass password) _)) ->
                      let user = User { _userId   = Authenticate.UserId i
                                      , _username = Authenticate.Username name
                                      , _email    = Nothing
                                      }
                      in Right (user, Left (Password.HashedPass password))
                    Nothing -> Left $ "Missing " <> (pack $ show upid)
                (AuthIdentifier identifier) ->
                  do let nickName' = if nickName == "Anonymous" then "Anonymous" <> (pack $ show i) else nickName
                         user = User { _userId   = Authenticate.UserId i
                                     , _username = Username $ nickName'
                                     , _email    = Nothing
                                     }
                      in Right (user, Right identifier)
                _ -> Left $ "Unsupported method -- skipping user. " <> (pack $ show authMethod)
            Nothing -> Left $ "Not Found: " <> (pack $ show authId)
        _ -> Left $ "Multiple auths found, not sure which to pick."
