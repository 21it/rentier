{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Yesod.Auth.Firebase
  ( authFirebase,
    FirebaseSettings (..),
    YesodAuthFirebase (..),
  )
where

import Control.Lens (_Just)
import Control.Monad.Trans.Except (runExceptT)
import qualified Crypto.JOSE.JWK as JWK
import Crypto.JOSE.JWK.Store (VerificationKeyStore (..))
import qualified Crypto.JOSE.Types as JTypes
import qualified Crypto.JWT as JWT
import Crypto.PubKey.RSA.Types (PublicKey (..))
import Data.Aeson
import Data.ByteString
import qualified Data.ByteString.Char8
import qualified Data.ByteString.Lazy.Char8
import Data.EitherR
import Data.HashMap.Strict
import Data.Map.Strict
import Data.Maybe (catMaybes)
import qualified Data.PEM as Pem
import Data.Text
import Data.Time.Clock
import qualified Data.X509 as X509
import Import.External
import Network.HTTP.Simple
import Safe
import Yesod.Auth
  ( AuthHandler,
    AuthPlugin (..),
    AuthRoute,
    Creds (..),
    Route (..),
    YesodAuth,
    loginErrorMessageI,
    setCredsRedirect,
  )
import qualified Yesod.Auth.Message as Msg
import Yesod.Core

data FirebaseSettings
  = FirebaseSettings
      { apiKey :: Text,
        projectId :: Text,
        msgSenderId :: Text
      }

class (YesodAuth site) => YesodAuthFirebase site where
  -- | JWT AUD for Firebase auth validators
  jwtAud :: AuthHandler site Text

newtype GoogleCertStorage = GoogleCertStorage String

instance (MonadIO m, JWT.HasKid h) => VerificationKeyStore m (h p) JWT.ClaimsSet GoogleCertStorage where
  getVerificationKeys h _ (GoogleCertStorage keyURL) =
    liftIO
      $ fmap catMaybes . traverse findKey
      $ catMaybes
        [preview (JWT.kid . _Just . JWT.param) h]
    where
      bytesToCert :: ByteString -> Either Text X509.Certificate
      bytesToCert s = do
        pems <-
          fmapL Data.Text.pack $
            Pem.pemParseBS s
        pem <-
          note "No pem found" $
            headMay pems
        signedExactCert <-
          fmapL Data.Text.pack
            $ X509.decodeSignedCertificate
            $ Pem.pemContent pem
        return $ X509.signedObject $ X509.getSigned signedExactCert
      certToJwk :: X509.Certificate -> Either Text JWT.JWK
      certToJwk cert = do
        (n, e) <-
          note "Unexpected cert format" $ getRSAKey $
            X509.certPubKey cert
        let jwk =
              JWK.fromKeyMaterial $ JWK.RSAKeyMaterial $
                JWK.RSAKeyParameters
                  (JTypes.Base64Integer n)
                  (JTypes.Base64Integer e)
                  Nothing
         in return $ jwk & JWK.jwkKeyOps .~ Just [JWK.Verify]
        where
          getRSAKey (X509.PubKeyRSA (PublicKey _ n e)) = Just (n, e)
          getRSAKey _ = Nothing
      findKey :: Text -> IO (Maybe JWT.JWK)
      findKey kidValue = do
        request <- parseRequest keyURL
        response <- httpJSONEither request :: IO (Response (Either JSONException (Map Text Text)))
        return $ do
          dict <-
            rightToMaybe $
              getResponseBody response
          raw <- Data.Map.Strict.lookup kidValue dict
          cert <-
            rightToMaybe
              $ bytesToCert
              $ Data.ByteString.Char8.pack
              $ Data.Text.unpack raw
          rightToMaybe $ certToJwk cert

loginR :: AuthRoute
loginR = PluginR "firebase" ["login"]

getLoginR :: YesodAuthFirebase site => AuthHandler site TypedContent
getLoginR = do
  mtoken <- lookupGetParam "token"
  case mtoken of
    Nothing -> authFailed
    Just token ->
      case defcodeJWT token of
        Left _ -> authFailed
        Right jwt -> do
          jwtAudV <- jwtAud
          case preview JWT.stringOrUri jwtAudV of
            Nothing -> authFailed
            Just aud -> do
              now <- liftIO getCurrentTime
              let settings = JWT.defaultJWTValidationSettings (== aud)
              eclaims <-
                runExceptT $
                  JWT.verifyClaimsAt
                    (settings & JWT.jwtValidationSettingsCheckIssuedAt .~ False)
                    (GoogleCertStorage "https://www.googleapis.com/robot/v1/metadata/x509/securetoken@system.gserviceaccount.com")
                    now
                    jwt
              case eclaims of
                Left (_ :: JWT.JWTError) -> authFailed
                Right claims ->
                  case phoneFromClaims claims of
                    Nothing -> authFailed
                    Just phone -> setCredsRedirect (Creds "firebase" phone [])
  where
    authFailed = loginErrorMessageI LoginR Msg.AuthError

phoneFromClaims :: JWT.ClaimsSet -> Maybe Text
phoneFromClaims claims =
  case toJSON claims of
    Object hm ->
      case Data.HashMap.Strict.lookup "phone_number" hm of
        Just (String phone) -> Just phone
        _ -> Nothing
    _ ->
      Nothing

defcodeJWT :: Text -> Either JWT.JWTError JWT.SignedJWT
defcodeJWT token =
  JWT.decodeCompact $ Data.ByteString.Lazy.Char8.pack $ Data.Text.unpack token

authFirebase :: (YesodAuthFirebase m, RenderMessage m msg) => msg -> msg -> FirebaseSettings -> AuthPlugin m
authFirebase msgIso639v1 msgIso3166v1 firebaseSettings =
  AuthPlugin "firebase" dispatch loginWidget
  where
    dispatch "GET" ["login"] = getLoginR >>= sendResponse
    dispatch _ _ = notFound
    projectIdV = projectId firebaseSettings
    loginWidget toMaster =
      [whamlet|
        <div id="firebaseui-auth-container">
        <script src="https://www.gstatic.com/firebasejs/5.7.2/firebase-app.js">
        <script src="https://www.gstatic.com/firebasejs/5.7.2/firebase-auth.js">
        <script>
          /* Initialize Firebase */
          var config = {
            apiKey: "#{apiKey firebaseSettings}",
            authDomain: "#{projectIdV}.firebaseapp.com",
            databaseURL: "https://#{projectIdV}.firebaseio.com",
            projectId: "#{projectIdV}",
            storageBucket: "#{projectIdV}.appspot.com",
            messagingSenderId: "#{msgSenderId firebaseSettings}"
          };
          firebase.initializeApp(config);
        <script src="https://cdn.firebase.com/libs/firebaseui/3.5.1/firebaseui.js">
        <script src="https://www.gstatic.com/firebasejs/ui/3.5.1/firebase-ui-auth___{msgIso639v1}.js">
        <link type="text/css" rel="stylesheet" href="https://cdn.firebase.com/libs/firebaseui/3.5.1/firebaseui.css">
        <script type="text/javascript">
          /* FirebaseUI config. */
          var uiConfig = {
            callbacks: {signInSuccessWithAuthResult: function(){return false}},
            signInOptions: [
              {
                provider: firebase.auth.PhoneAuthProvider.PROVIDER_ID,
                defaultCountry: "_{msgIso3166v1}"
              }
            ],
            tosUrl: 'http://localhost:3000',
            privacyPolicyUrl: 'http://localhost:3000'
          };

          const maybeStartUI = function(){
            let started = false;
            return function(){
              if (!started) {
                /* Initialize the FirebaseUI Widget using Firebase. */
                var ui = new firebaseui.auth.AuthUI(firebase.auth());
                /* The start method will wait until the DOM is loaded. */
                ui.start('#firebaseui-auth-container', uiConfig);
                started = true;
              }
            };
          }();

          firebase.auth().onAuthStateChanged(function(user){
            let token = "redefineme";
            if (user) {
              user.getIdToken(true).then(function(idToken){
                token = idToken;
                return firebase.auth().signOut();
              }).then(function(){
                window.location.assign("@{toMaster loginR}?token=" + token);
              }).catch(function(error) {
                console.log("Firebase login error", error);
                maybeStartUI();
              });
            } else {
              maybeStartUI();
            }
          });
        |]
