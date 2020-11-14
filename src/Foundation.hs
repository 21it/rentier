{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Foundation where

import Control.Monad.Logger (LogSource)
-- Used only when in "auth-dummy-login" setting is enabled.

import qualified Data.CaseInsensitive as CI
import qualified Data.Text.Encoding as TE
import Database.Persist.Sql (ConnectionPool, runSqlPool)
import Import.NoFoundation
-- import           Yesod.Auth.GoogleEmail2

import qualified Rentier.Currency
import Rentier.Data.Type
import qualified Rentier.Language
import Rentier.Permission
import Rentier.Time
import Rentier.UserRole
import Text.Hamlet (hamletFile)
import Text.Jasmine (minifym)
import qualified Text.Read
import Yesod.Auth.Dummy
import Yesod.Auth.Firebase
import qualified Yesod.Auth.Message as AuthMsg
import Yesod.Core.Types (Logger)
import qualified Yesod.Core.Unsafe as Unsafe
import Yesod.Default.Util (addStaticContentExternal)

newtype PropertiesMultipleSelection = PropertiesMultipleSelection [(PropertyId, Int)] deriving (Show, Read, Eq)

instance PathPiece PropertiesMultipleSelection where
  fromPathPiece :: Text -> Maybe PropertiesMultipleSelection
  fromPathPiece = Text.Read.readMaybe . unpack
  toPathPiece :: PropertiesMultipleSelection -> Text
  toPathPiece = pack . show

-- | The foundation datatype for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App
  = App
      { appSettings :: AppSettings,
        -- | Settings for static file serving.
        appStatic :: Static,
        -- | Database connection pool.
        appConnPool :: ConnectionPool,
        appHttpManager :: Manager,
        appLogger :: Logger,
        --, gmailClientId       :: Text
        --, gmailClientSecret   :: Text
        firebaseApiKey :: Text,
        firebaseProjectId :: Text,
        firebaseMsgSenderId :: Text
      }

mkMessage "App" "messages" "en"

data MenuItem
  = MenuItem
      { menuItemLabel :: AppMessage,
        menuItemRoute :: Route App,
        menuItemAccessCallback :: Bool,
        menuItemActiveCallback :: Bool,
        menuItemNoReferrer :: Bool
      }

data MenuTypes
  = NavbarLeft MenuItem
  | NavbarRight MenuItem

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/routing-and-handlers
--
-- Note that this is really half the story; in Application.hs, mkYesodDispatch
-- generates the rest of the code. Please see the following documentation
-- for an explanation for this split:
-- http://www.yesodweb.com/book/scaffolding-and-the-site-template#scaffolding-and-the-site-template_foundation_and_application_modules
--
-- This function also generates the following type synonyms:
-- type Handler = HandlerT App IO
-- type Widget = WidgetT App IO ()
mkYesodData "App" $(parseRoutesFile "config/routes")

-- | A convenient synonym for creating forms.
type Form x = Html -> MForm (HandlerFor App) (FormResult x, Widget)

-- | A convenient synonym for database access functions.
type DB a =
  forall (m :: * -> *).
  (MonadIO m) =>
  ReaderT SqlBackend m a

instance YesodAuthFirebase App where
  jwtAud :: AuthHandler App Text
  jwtAud = do
    app <- getYesod
    return (firebaseProjectId app)

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
  -- Controls the base of generated URLs. For more information on modifying,
  -- see: https://github.com/yesodweb/yesod/wiki/Overriding-approot
  approot :: Approot App
  approot = ApprootRequest $ \app req ->
    fromMaybe (getApprootText guessApproot app req) (appRoot $ appSettings app)

  -- Store session data on the client in encrypted cookies,
  -- default session idle timeout is 120 minutes
  makeSessionBackend :: App -> IO (Maybe SessionBackend)
  makeSessionBackend _ =
    Just
      <$> defaultClientSessionBackend
        20160 -- timeout in minutes
        "config/client_session_key.aes"

  -- Yesod Middleware allows you to run code before and after each handler function.
  -- The defaultYesodMiddleware adds the response header "Vary: Accept, Accept-Language" and performs authorization checks.
  -- Some users may also want to add the defaultCsrfMiddleware, which:
  --   a) Sets a cookie with a CSRF token in it.
  --   b) Validates that incoming write requests include that token in either a header or POST parameter.
  -- To add it, chain it together with the defaultMiddleware: yesodMiddleware = defaultYesodMiddleware . defaultCsrfMiddleware
  -- For details, see the CSRF documentation in the Yesod.Core.Handler module of the yesod-core package.
  yesodMiddleware :: ToTypedContent res => Handler res -> Handler res
  yesodMiddleware = defaultYesodMiddleware

  defaultLayout :: Widget -> Handler Html
  defaultLayout widget = do
    master <- getYesod
    mmsg <- getMessage
    muser <- maybeAuth
    muser_role <- lookupUserRole
    mcurrentRoute <- getCurrentRoute
    -- Get the breadcrumbs, as defined in the YesodBreadcrumbs instance.
    (title, parents) <- breadcrumbs
    -- Define the menu items of the header.
    let genericMenuItems =
          [ NavbarLeft
              MenuItem
                { menuItemLabel = MsgHome,
                  menuItemRoute = HomeR,
                  menuItemAccessCallback = True,
                  menuItemActiveCallback = mcurrentRoute == Just HomeR,
                  menuItemNoReferrer = False
                },
            NavbarLeft
              MenuItem
                { menuItemLabel = MsgProfile,
                  menuItemRoute = ProfileR,
                  menuItemAccessCallback = isJust muser,
                  menuItemActiveCallback = mcurrentRoute == Just ProfileR,
                  menuItemNoReferrer = False
                },
            NavbarLeft
              MenuItem
                { menuItemLabel = MsgForMerchant,
                  menuItemRoute = MerchantCreateR,
                  menuItemAccessCallback = isNothing muser_role,
                  menuItemActiveCallback = mcurrentRoute == Just MerchantCreateR,
                  menuItemNoReferrer = False
                },
            NavbarRight
              MenuItem
                { menuItemLabel = MsgLogin,
                  menuItemRoute = AuthR LoginR,
                  menuItemAccessCallback = isNothing muser,
                  menuItemActiveCallback = mcurrentRoute == Just (AuthR LoginR),
                  menuItemNoReferrer = True
                },
            NavbarRight
              MenuItem
                { menuItemLabel = MsgLogout,
                  menuItemRoute = SignoutR,
                  menuItemAccessCallback = isJust muser,
                  menuItemActiveCallback = False,
                  menuItemNoReferrer = True
                }
          ]
    let specificMenuItems =
          case muser_role of
            Just (UserRoleMerchant merchantId) ->
              [ NavbarLeft
                  MenuItem
                    { menuItemLabel = MsgOrganizations,
                      menuItemRoute = OrganizationListR merchantId,
                      menuItemAccessCallback = True,
                      menuItemNoReferrer = False,
                      menuItemActiveCallback = case mcurrentRoute of
                        Just OrganizationListR {} -> True
                        Just OrganizationCreateR {} -> True
                        Just OrganizationUpdateR {} -> True
                        Just OrganizationScheduleCreateR {} -> True
                        Just OrganizationScheduleUpdateR {} -> True
                        Just PropertyCreateR {} -> True
                        Just PropertyUpdateR {} -> True
                        _ -> False
                    },
                NavbarLeft
                  MenuItem
                    { menuItemLabel = MsgEmployees,
                      menuItemRoute = EmployeeListR merchantId,
                      menuItemAccessCallback = True,
                      menuItemNoReferrer = False,
                      menuItemActiveCallback = case mcurrentRoute of
                        Just EmployeeListR {} -> True
                        Just EmployeeCreateR {} -> True
                        _ -> False
                    }
              ]
            _ ->
              []
    let menuItems = genericMenuItems ++ specificMenuItems
    let navbarLeftMenuItems = [x | NavbarLeft x <- menuItems]
    let navbarRightMenuItems = [x | NavbarRight x <- menuItems]
    let navbarLeftFilteredMenuItems = [x | x <- navbarLeftMenuItems, menuItemAccessCallback x]
    let navbarRightFilteredMenuItems = [x | x <- navbarRightMenuItems, menuItemAccessCallback x]
    -- We break up the default layout into two components:
    -- default-layout is the contents of the body tag, and
    -- default-layout-wrapper is the entire page. Since the final
    -- value passed to hamletToRepHtml cannot be a widget, this allows
    -- you to use normal widget features in default-layout.

    pc <- widgetToPageContent $ do
      addStylesheet $ StaticR css_bootstrap_css
      addStylesheet $ StaticR css_app_css
      $(widgetFile "default-layout")
    withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

  -- The page to be redirected to when authentication is required.
  authRoute :: App -> Maybe (Route App)
  authRoute _ = Just $ AuthR LoginR

  isAuthorized :: Route App -> Bool -> Handler AuthResult
  isAuthorized route isWrite = do
    muser <- maybeAuth
    mrole <- lookupUserRole
    runDB $ isAuthorizedTo muser mrole (permissionsRequiredFor route isWrite)
    where
      isAuthorizedTo ::
        Maybe (Entity User) ->
        Maybe UserRole ->
        [Permission] ->
        YesodDB App AuthResult
      isAuthorizedTo _ _ [] = return Authorized
      isAuthorizedTo Nothing _ (_ : _) = return AuthenticationRequired
      isAuthorizedTo (Just user) mrole (permission : otherPermissions) = do
        result <- hasPermissionTo user mrole permission
        case result of
          Authorized -> isAuthorizedTo (Just user) mrole otherPermissions
          _ -> return result -- unauthorized
      permissionsRequiredFor :: Route App -> Bool -> [Permission]
      -- service Routes not requiring permissions
      permissionsRequiredFor FaviconR _ = []
      permissionsRequiredFor RobotsR _ = []
      permissionsRequiredFor (StaticR _) _ = []
      permissionsRequiredFor (LanguageR _) _ = []
      permissionsRequiredFor (AuthR _) _ = []
      permissionsRequiredFor SignoutR _ = []
      -- public Routes not requiring permissions
      permissionsRequiredFor HomeR _ = []
      -- private routes are requiring permissions
      permissionsRequiredFor ProfileR _ =
        [ ViewProfile
        ]
      permissionsRequiredFor EmployeeLoginR _ =
        [ ViewProfile
        ]
      permissionsRequiredFor MerchantCreateR _ =
        [ ViewProfile
        ]
      permissionsRequiredFor (OrganizationListR merchantId) _ =
        [ UserRoleIs (UserRoleMerchant merchantId),
          ManageMerchant merchantId
        ]
      permissionsRequiredFor (OrganizationCreateR merchantId) _ =
        [ UserRoleIs (UserRoleMerchant merchantId),
          ManageMerchant merchantId
        ]
      permissionsRequiredFor (OrganizationUpdateR merchantId organizationId) _ =
        [ UserRoleIs (UserRoleMerchant merchantId),
          ManageMerchant merchantId,
          ManageOrganization merchantId organizationId
        ]
      permissionsRequiredFor (OrganizationScheduleCreateR merchantId organizationId) _ =
        [ UserRoleIs (UserRoleMerchant merchantId),
          ManageMerchant merchantId,
          ManageOrganization merchantId organizationId
        ]
      permissionsRequiredFor (OrganizationScheduleUpdateR merchantId organizationId organizationScheduleId) _ =
        [ UserRoleIs (UserRoleMerchant merchantId),
          ManageMerchant merchantId,
          ManageOrganization merchantId organizationId,
          ManageOrganizationSchedule organizationId organizationScheduleId
        ]
      permissionsRequiredFor (OrganizationScheduleDeleteR merchantId organizationId organizationScheduleId) _ =
        [ UserRoleIs (UserRoleMerchant merchantId),
          ManageMerchant merchantId,
          ManageOrganization merchantId organizationId,
          ManageOrganizationSchedule organizationId organizationScheduleId
        ]
      permissionsRequiredFor (PropertyCreateR merchantId organizationId) _ =
        [ UserRoleIs (UserRoleMerchant merchantId),
          ManageMerchant merchantId,
          ManageOrganization merchantId organizationId
        ]
      permissionsRequiredFor (PropertyUpdateR merchantId organizationId propertyId) _ =
        [ UserRoleIs (UserRoleMerchant merchantId),
          ManageMerchant merchantId,
          ManageOrganization merchantId organizationId,
          ManageProperty organizationId propertyId
        ]
      permissionsRequiredFor (EmployeeListR merchantId) _ =
        [ UserRoleIs (UserRoleMerchant merchantId),
          ManageMerchant merchantId
        ]
      permissionsRequiredFor (EmployeeCreateR merchantId) _ =
        [ UserRoleIs (UserRoleMerchant merchantId),
          ManageMerchant merchantId
        ]
      permissionsRequiredFor (IframePropertyScheduleR _) _ =
        []
      permissionsRequiredFor IframeSessionCreateR {} _ =
        []
      permissionsRequiredFor IframeSessionConfirmR {} _ =
        []
      hasPermissionTo ::
        Entity User ->
        Maybe UserRole ->
        Permission ->
        YesodDB App AuthResult
      hasPermissionTo Entity {} _ ViewProfile = return Authorized
      hasPermissionTo Entity {} mrole (UserRoleIs r) =
        return $ case mrole of
          Just x | x == r -> Authorized
          _ -> Unauthorized ""
      hasPermissionTo Entity {entityKey = userId} _ (ManageMerchant merchantId) = do
        maybeMerchant <- getBy $ UniqueMerchant userId
        return $ case maybeMerchant of
          Just Entity {entityKey = userMerchantId}
            | merchantId == userMerchantId ->
              Authorized
          Just Entity {} ->
            Unauthorized ""
          Nothing ->
            Unauthorized ""
      hasPermissionTo Entity {} _ (ManageOrganization merchantId organizationId) = do
        maybeOrg <- get organizationId
        return $ case maybeOrg of
          Just org
            | organizationMerchantId org == merchantId ->
              Authorized
          Just Organization {} ->
            Unauthorized ""
          Nothing ->
            Unauthorized ""
      hasPermissionTo Entity {} _ (ManageOrganizationSchedule organizationId organizationScheduleId) = do
        mSchedule <- get organizationScheduleId
        return $ case mSchedule of
          Just schedule
            | organizationScheduleOrganizationId schedule == organizationId ->
              Authorized
          Just OrganizationSchedule {} ->
            Unauthorized ""
          Nothing ->
            Unauthorized ""
      hasPermissionTo Entity {} _ (ManageProperty organizationId propertyId) = do
        mm <- get propertyId
        return $ case mm of
          Just m
            | propertyOrganizationId m == organizationId ->
              Authorized
          Just Property {} ->
            Unauthorized ""
          Nothing ->
            Unauthorized ""

  -- This function creates static content files in the static folder
  -- and names them based on a hash of their content. This allows
  -- expiration dates to be set far in the future without worry of
  -- users receiving stale content.
  addStaticContent ::
    -- | The file extension
    Text ->
    -- | The MIME content type
    Text ->
    -- | The contents of the file
    LByteString ->
    Handler (Maybe (Either Text (Route App, [(Text, Text)])))
  addStaticContent ext mime content = do
    master <- getYesod
    let staticDir = appStaticDir $ appSettings master
    addStaticContentExternal
      minifym
      genFileName
      staticDir
      (StaticR . flip StaticRoute [])
      ext
      mime
      content
    where
      -- Generate a unique filename based on the content itself
      genFileName lbs = "autogen-" ++ base64md5 lbs

  -- What messages should be logged. The following includes all messages when
  -- in development, and warnings and errors in production.
  shouldLogIO :: App -> LogSource -> LogLevel -> IO Bool
  shouldLogIO app _source level =
    return $
      appShouldLogAll (appSettings app)
        || level == LevelWarn
        || level == LevelError

  makeLogger :: App -> IO Logger
  makeLogger = return . appLogger

-- Define breadcrumbs.
instance YesodBreadcrumbs App where
  -- Takes the route that the user is currently on, and returns a tuple
  -- of the 'Text' that you want the label to display, and a previous
  -- breadcrumb route.
  breadcrumb ::
    -- | The route the user is visiting currently.
    Route App ->
    Handler (Text, Maybe (Route App))
  breadcrumb r = do
    render <- getMessageRender :: Handler (AppMessage -> Text)
    msg <- getMsg r
    let parent = getParent r
    return (render msg, parent)
    where
      getMsg :: Route App -> Handler AppMessage
      getMsg r0 =
        case r0 of
          StaticR _ -> return $ MsgProxy ""
          FaviconR -> return $ MsgProxy ""
          RobotsR -> return $ MsgProxy ""
          LanguageR _ -> return $ MsgProxy ""
          EmployeeLoginR -> return $ MsgProxy ""
          HomeR -> return MsgHomeRBreadcrumb
          AuthR _ -> return MsgAuthRBreadcrumb
          SignoutR -> return MsgSignoutRBreadcrumb
          ProfileR -> return MsgProfileRBreadcrumb
          MerchantCreateR -> return MsgMerchantCreateRBreadcrumb
          OrganizationListR _ -> return MsgOrganizationListRBreadcrumb
          OrganizationCreateR _ -> return MsgCreateRBreadcrumb
          OrganizationUpdateR _ organizationId -> do
            mx <- runDB $ get organizationId
            return $ case mx of
              Just Organization {organizationName = n} -> MsgProxy n
              Nothing -> MsgCreateRBreadcrumb
          OrganizationScheduleCreateR _ _ ->
            return MsgOrganizationScheduleCreateRBreadcrumb
          OrganizationScheduleUpdateR {} ->
            return MsgOrganizationScheduleUpdateRBreadcrumb
          OrganizationScheduleDeleteR {} ->
            return MsgOrganizationScheduleDeleteRBreadcrumb
          PropertyCreateR _ _ ->
            return MsgPropertyCreateRBreadcrumb
          PropertyUpdateR _ _ propertyId -> do
            mx <- runDB $ get propertyId
            return $ case mx of
              Just Property {propertyName = n} -> MsgProxy n
              Nothing -> MsgPropertyCreateRBreadcrumb
          EmployeeListR _ ->
            return MsgEmployeeListRBreadcrumb
          EmployeeCreateR _ ->
            return MsgCreateRBreadcrumb
          IframePropertyScheduleR {} ->
            return MsgIframePropertyScheduleRBreadcrumb
          IframeSessionCreateR {} ->
            return MsgIframeSessionCreateRBreadcrumb
          IframeSessionConfirmR {} ->
            return MsgIframeSessionConfirmRBreadcrumb
      getParent :: Route App -> Maybe (Route App)
      getParent r0 =
        case r0 of
          StaticR _ -> Nothing
          FaviconR -> Nothing
          RobotsR -> Nothing
          LanguageR _ -> Nothing
          EmployeeLoginR -> Nothing
          HomeR -> Nothing
          AuthR _ -> Just HomeR
          SignoutR -> Just HomeR
          ProfileR -> Just HomeR
          MerchantCreateR -> Just HomeR
          OrganizationListR _ -> Just ProfileR
          OrganizationCreateR merchantId -> Just $ OrganizationListR merchantId
          OrganizationUpdateR merchantId _ -> Just $ OrganizationListR merchantId
          OrganizationScheduleCreateR merchantId organizationId ->
            Just $ OrganizationUpdateR merchantId organizationId
          OrganizationScheduleUpdateR merchantId organizationId _ ->
            Just $ OrganizationUpdateR merchantId organizationId
          OrganizationScheduleDeleteR merchantId organizationId _ ->
            Just $ OrganizationUpdateR merchantId organizationId
          PropertyCreateR merchantId organizationId ->
            Just $ OrganizationUpdateR merchantId organizationId
          PropertyUpdateR merchantId organizationId _ ->
            Just $ OrganizationUpdateR merchantId organizationId
          EmployeeListR _ ->
            Just ProfileR
          EmployeeCreateR merchantId ->
            Just $ EmployeeListR merchantId
          IframePropertyScheduleR {} ->
            Nothing
          IframeSessionCreateR propertyId _ _ ->
            Just $ IframePropertyScheduleR propertyId
          IframeSessionConfirmR propertyId startsAt endsAt _ _ ->
            Just $ IframeSessionCreateR propertyId startsAt endsAt

-- How to run database actions.
instance YesodPersist App where
  type YesodPersistBackend App = SqlBackend
  runDB :: SqlPersistT Handler a -> Handler a
  runDB action = do
    master <- getYesod
    runSqlPool action $ appConnPool master

instance YesodPersistRunner App where
  getDBRunner :: Handler (DBRunner App, Handler ())
  getDBRunner = defaultGetDBRunner appConnPool

instance YesodAuth App where
  type AuthId App = UserId

  -- Where to send a user after successful login
  loginDest :: App -> Route App
  loginDest _ = ProfileR

  -- Where to send a user after logout
  logoutDest :: App -> Route App
  logoutDest _ = HomeR

  -- Override the above two destinations when a Referer: header is present
  redirectToReferer :: App -> Bool
  redirectToReferer _ = True

  authenticate ::
    (MonadHandler m, HandlerSite m ~ App) =>
    Creds App ->
    m (AuthenticationResult App)
  authenticate creds = liftHandler $ runDB $ do
    x <- getBy $ UniqueUser $ credsIdent creds
    case x of
      Just (Entity uid _) ->
        return $ Authenticated uid
      Nothing -> do
        userId <-
          insert400
            User
              { userIdent = credsIdent creds,
                userFirstName = Nothing,
                userLastName = Nothing
              }
        return $ Authenticated userId

  -- You can add other plugins like Google Email, email or OAuth here
  authPlugins :: App -> [AuthPlugin App]
  authPlugins app =
    -- [firebaseAuthPlugin, gmailAuthPlugin] ++ extraAuthPlugins
    firebaseAuthPlugin : extraAuthPlugins
    where
      firebaseAuthPlugin =
        authFirebase
          MsgIso639v1
          MsgIso3166v1
          FirebaseSettings
            { apiKey = firebaseApiKey app,
              projectId = firebaseProjectId app,
              msgSenderId = firebaseMsgSenderId app
            }
      -- gmailAuthPlugin = authGoogleEmail (gmailClientId app) (gmailClientSecret app)
      -- Enable authDummy login if enabled.
      extraAuthPlugins = [authDummy | appAuthDummyLogin (appSettings app)]

  renderAuthMessage ::
    master ->
    -- | languages
    [Text] ->
    AuthMsg.AuthMessage ->
    Text
  renderAuthMessage _ langStrings msg =
    case msg of
      AuthMsg.LoginTitle -> baseMsgText ++ " | Rentier"
      AuthMsg.LogoutTitle -> baseMsgText ++ " | Rentier"
      _ -> baseMsgText
    where
      baseMsgText =
        case mapMaybe (\x -> fromPathPiece x :: Maybe Rentier.Language.Code) langStrings of
          [] -> AuthMsg.defaultMessage msg
          (Rentier.Language.En : _) -> AuthMsg.englishMessage msg
          (Rentier.Language.Ru : _) -> AuthMsg.russianMessage msg

-- loginHandler :: AuthHandler App Html
-- loginHandler = do
--     ma <- liftHandler maybeAuth
--     when (isJust ma) $
--         liftHandler $ redirect (if isVerified ma then HomeR else VerifyR)
--     when (isNothing ma) $
--         liftHandler $ redirect $ AuthR Yesod.Auth.GoogleEmail2.forwardUrl
--     defaultLoginHandler

instance YesodAuthPersist App

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
  renderMessage :: App -> [Lang] -> FormMessage -> Text
  renderMessage _ _ = defaultFormMessage

-- Useful when writing code that is re-usable outside of the Handler context.
-- An example is background jobs that send email.
-- This can also be useful for writing code that works across multiple Yesod applications.
instance HasHttpManager App where
  getHttpManager :: App -> Manager
  getHttpManager = appHttpManager

unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger

-- Note: Some functionality previously present in the scaffolding has been
-- moved to documentation in the Wiki. Following are some hopefully helpful
-- links:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email
-- https://github.com/yesodweb/yesod/wiki/Serve-static-files-from-a-separate-domain
-- https://github.com/yesodweb/yesod/wiki/i18n-messages-in-the-scaffolding

deleteUserRole :: Handler ()
deleteUserRole =
  deleteSession (pack $ show UserRole)

setUserRole :: UserRole -> Handler ()
setUserRole userRole =
  setSession (pack $ show UserRole) (pack $ show userRole)

lookupUserRole :: Handler (Maybe UserRole)
lookupUserRole = do
  mval <- lookupSession (pack $ show UserRole)
  return $ case mval of
    Just val -> Text.Read.readMaybe $ unpack val
    Nothing -> Nothing

noLayout :: Widget -> Handler Html
noLayout widget = do
  master <- getYesod
  (title, parents) <- breadcrumbs
  pc <- widgetToPageContent $ do
    addStylesheet $ StaticR css_bootstrap_css
    addStylesheet $ StaticR css_app_css
    $(widgetFile "no-layout")
  withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")
