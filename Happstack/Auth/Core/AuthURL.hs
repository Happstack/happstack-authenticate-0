{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, FlexibleInstances, OverloadedStrings #-}
module Happstack.Auth.Core.AuthURL where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (msum)
import Data.Data (Data, Typeable)
import Data.Text (unpack)
import Test.QuickCheck (Arbitrary(..), Property, property, oneof)
import Web.Routes (PathInfo(..), pathInfoInverse_prop, segment)

data OpenIdProvider
    = Google
    | Yahoo
    | Myspace
    | LiveJournal
    | Generic
      deriving (Eq, Ord, Read, Show, Data, Typeable, Enum, Bounded)

instance PathInfo OpenIdProvider where
    toPathSegments Google      = ["google"]
    toPathSegments Yahoo       = ["yahoo"]
    toPathSegments Myspace     = ["myspace"]
    toPathSegments LiveJournal = ["livejournal"]
    toPathSegments Generic     = ["generic"]
    fromPathSegments =
        msum [ do segment "google"
                  return Google
             , do segment "yahoo"
                  return Yahoo
             , do segment "myspace"
                  return Myspace
             , do segment "livejournal"
                  return LiveJournal
             , do segment "generic"
                  return Generic
             ]

instance Arbitrary OpenIdProvider where
    arbitrary = oneof $ map return [ minBound .. maxBound ]

data AuthMode
    = LoginMode
    | AddIdentifierMode
      deriving (Eq, Ord, Read, Show, Data, Typeable)

instance PathInfo AuthMode where
    toPathSegments LoginMode         = ["login"]
    toPathSegments AddIdentifierMode = ["add_identifier"]
    fromPathSegments =
        msum [ do segment "login"
                  return LoginMode
             , do segment "add_identifier"
                  return AddIdentifierMode
             ]


instance Arbitrary AuthMode where
    arbitrary = oneof [ return LoginMode
                      , return AddIdentifierMode
                      ]

data AuthURL
    = A_Login
    | A_AddAuth
    | A_Logout
    | A_Signup
    | A_Local
    | A_CreateAccount
    | A_ChangePassword
    | A_OpenId OpenIdURL
    | A_OpenIdProvider AuthMode OpenIdProvider
    | A_Facebook AuthMode
    | A_FacebookRedirect AuthMode
      deriving (Eq, Ord, Read, Show, Data, Typeable)

data OpenIdURL
    = O_OpenId AuthMode
    | O_Connect AuthMode
      deriving (Eq, Ord, Read, Show, Data, Typeable)

instance Arbitrary OpenIdURL where
    arbitrary = oneof [ O_OpenId <$> arbitrary
                      , O_Connect <$> arbitrary
                      ]

instance Arbitrary AuthURL where
    arbitrary = oneof [ return A_Login
                      , return A_AddAuth
                      , return A_Logout
                      , return A_Signup
                      , return A_Local
                      , return A_CreateAccount
                      , return A_ChangePassword
                      , A_OpenId <$> arbitrary
                      , A_OpenIdProvider <$> arbitrary <*> arbitrary
                      , A_Facebook <$> arbitrary
                      , A_FacebookRedirect <$> arbitrary
                      ]

instance PathInfo OpenIdURL where
    toPathSegments (O_OpenId authMode)            = "openid_return" : toPathSegments authMode
    toPathSegments (O_Connect authMode)           = "connect" : toPathSegments authMode

    fromPathSegments =
        msum [ do segment "openid_return"
                  mode <- fromPathSegments
                  return (O_OpenId mode)
             , do segment "connect"
                  authMode <- fromPathSegments
                  return (O_Connect authMode)
             ]

instance PathInfo AuthURL where
    toPathSegments A_Login          = ["login"]
    toPathSegments A_Logout         = ["logout"]
    toPathSegments A_Local          = ["local"]
    toPathSegments A_CreateAccount  = ["create"]
    toPathSegments A_ChangePassword = ["change_password"]
    toPathSegments A_AddAuth        = ["add_auth"]
    toPathSegments A_Signup         = ["signup"]
    toPathSegments (A_OpenId o)     = "openid" : toPathSegments o
    toPathSegments (A_OpenIdProvider authMode provider) = "provider" : toPathSegments authMode ++ toPathSegments provider
    toPathSegments (A_Facebook authMode)         = "facebook"          : toPathSegments authMode
    toPathSegments (A_FacebookRedirect authMode) = "facebook-redirect" : toPathSegments authMode

    fromPathSegments =
        msum [ do segment "login"
                  return A_Login
             , do segment "logout"
                  return A_Logout
             , do segment "local"
                  return A_Local
             , do segment "signup"
                  return A_Signup
             , do segment "create"
                  return A_CreateAccount
             , do segment "change_password"
                  return A_ChangePassword
             , do segment "openid"
                  A_OpenId <$> fromPathSegments
             , do segment "add_auth"
                  return A_AddAuth
             , do segment "provider"
                  authMode <- fromPathSegments
                  provider <- fromPathSegments
                  return (A_OpenIdProvider authMode provider)
             , do segment "facebook"
                  authMode <- fromPathSegments
                  return (A_Facebook authMode)
             , do segment "facebook-redirect"
                  authMode <- fromPathSegments
                  return (A_FacebookRedirect authMode)
             ]

authUrlInverse :: Property
authUrlInverse =
    property (pathInfoInverse_prop :: AuthURL -> Bool)
