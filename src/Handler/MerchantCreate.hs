{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE TypeFamilies          #-}

module Handler.MerchantCreate where

import           Import
import           Rentier.Rational      ()
import           Rentier.SimpleForm
import           Rentier.UserRole
import           Yesod.Form.Bootstrap3

getMerchantCreateR :: Handler Html
getMerchantCreateR = do
  (userId, user) <- requireAuthPair
  mm <- runDB $ getBy $ UniqueMerchant userId
  case mm of
    Nothing -> do
      (formWidget, formEnctype) <- generateFormPost $
                                   renderBootstrap3 BootstrapBasicForm
                                   (aForm userId)
      renderSimpleForm formWidget formEnctype (formSettings user)
    Just Entity{entityKey = merchantId} -> do
      setUserRole $ UserRoleMerchant merchantId
      redirect ProfileR

postMerchantCreateR :: Handler Html
postMerchantCreateR = do
  (userId, user) <- requireAuthPair
  mm <- runDB $ getBy $ UniqueMerchant userId
  case mm of
    Nothing -> do
      ((formResult, formWidget), formEnctype) <- runFormPost $
                                                 renderBootstrap3 BootstrapBasicForm
                                                 (aForm userId)
      case formResult of
          FormSuccess form -> do
            merchantId <- runDB $ insert400 Merchant{
              merchantUserId = userId,
              merchantBaseCurrencyCode = merchantBaseCurrencyCode form,
              merchantBalance = 0,
              merchantInitialized = False,
              merchantActive = True,
              merchantExists = True
            }
            setMessageI MsgMerchantCreated
            setUserRole $ UserRoleMerchant merchantId
            redirect ProfileR
          _ ->
            renderSimpleForm formWidget formEnctype (formSettings user)
    Just Entity{entityKey = merchantId} -> do
      setUserRole $ UserRoleMerchant merchantId
      redirect ProfileR

aForm :: UserId -> AForm Handler Merchant
aForm userId = Merchant
    <$> areq hiddenField (bfs MsgNothing) (Just userId)
    <*> areq (selectField optionsEnum) (bfs MsgCurrencyCodeSelect) Nothing
    <*> areq hiddenField (bfs MsgNothing) (Just 0)
    <*> areq hiddenField (bfs MsgNothing) (Just False)
    <*> areq hiddenField (bfs MsgNothing) (Just True)
    <*> areq hiddenField (bfs MsgNothing) (Just True)

formSettings :: User -> SimpleFormSettings
formSettings user =
  SimpleFormSettings{
    formRoute = MerchantCreateR,
    formMsgSubmit = MsgMerchantCreate,
    formPageTitle = MsgMerchantCreateRTitle $ userIdent user,
    formLayout = defaultLayout
  }
