module Lib where 

import Prelude
import Type.Row

import Control.Monad.Error.Class (class MonadError, class MonadThrow, catchError, throwError, try)
import Control.Monad.Except (ExceptT(..), runExcept, runExceptT)
import Control.Monad.Except.Checked (ExceptV, handleError, safe)
import Data.Either (Either(..))
import Data.Variant (class VariantMatchCases, SProxy(..), Variant, inj)
import Effect (Effect)
import Effect.Class.Console (log, logShow)
import Effect.Exception (Error)
import Type.Equality (class TypeEquals)
import Unsafe.Coerce (unsafeCoerce)

newtype SId r = SId String  
newtype SOQL r = SOQL String 
newtype SObject t r = SObject t 
type Account = { name :: String }
data QueryEndpoint r  = Query (SOQL r) | QueryExplain (SOQL r)
data SObjectEndpoint t r = Retrieve (SObject t r) | Update (SObject t r) | Delete (SObject t r) | Insert (SId r) 
data Connection = Connection 

type QueryError r = (QueryServerError + QueryParseError + r)
type QueryServerError r = (queryServerError :: String | r)
type QueryParseError r = (queryParseError :: String | r)

type SObjectError r = (SObjectServerError + SObjectParseError + r)
type SObjectServerError r = (sobjectServerError :: String | r)
type SObjectParseError r = (sobjectServerError :: String | r)

type RequestError api r = (RequestServerError + RequestParseError + r)
type RequestServerError r = (requestServerError :: String | r)
type RequestParseError r = (requestParseError :: String | r)

networktError ∷ ∀ r. String → Variant (NetworkError + r)
networktError = inj (SProxy ∷ SProxy "error")

requestServerError ∷ ∀ r api. String → Variant (RequestError api + r)
requestServerError = inj (SProxy ∷ SProxy "requestServerError")

requestParseError ∷ ∀ r api. String → Variant (RequestError api + r)
requestParseError = inj (SProxy ∷ SProxy "requestParseError")


type NetworkError r = (error :: String | r) 
-- type NetworkError 
--     = { message     :: String 
--       , errorCode   :: String 
--       , statusCode  :: Int
--       , fields      :: Array String
--       }

class Monad m <= HasNetwork m sfapi where 
    request :: forall result rowlist r excHandled excOut. 
        RowToList (NetworkError + r) rowlist 
        => VariantMatchCases rowlist excHandled (Either (Variant excOut) result)
        => Union excHandled excOut (NetworkError + r)
        => HasEndpoint sfapi result
        => sfapi 
        -> m (Either (Variant (NetworkError + r)) result)

class HasEndpoint sfapi result | sfapi -> result where 
    baseUrl :: Connection -> sfapi -> String  

-- instance hasExceptTNetwork 
--     :: ( TypeEquals (Variant (RequestError sfapi r)) err
--        , HasNetwork m sfapi
--        , HasEndpoint sfapi result
--        )
--      => HasNetwork (ExceptT err m) sfapi where 
--     request sfapi =  do 
--         eitherResult <- runExceptT $ ExceptT $ request sfapi 
    
--         case eitherResult of 
--             Left e -> ((ExceptT $ pure $ Left $ requestServerError "server error") :: ExceptT err m result) 
--             Right result -> ExceptT $ pure $ Right result


instance hasQueryEndpoint :: HasEndpoint (QueryEndpoint r) r where
    baseUrl connection _ = "https:://test.salesforce.com/service/data/v45.0/query?q=" 

instance hasSObjectEndpoint :: HasEndpoint (SObjectEndpoint t r) r where 
    baseUrl connection _ = "https:://test.salesforce.com/service/data/v45.0/sobjects/" 


instance hasQueryNetwork :: HasNetwork Effect (QueryEndpoint r) where
    request sfapi = do
        let url = baseUrl Connection sfapi
        -- pure $ Right responseFromSFDC
        pure $ Left $ networktError "error"

instance hasSObjectNetwork :: HasNetwork Effect (SObjectEndpoint t r) where
    request sfapi = do
        let url = baseUrl Connection sfapi
        pure $ Right responseFromSFDCSObject

responseFromSFDC :: forall r. r 
responseFromSFDC = unsafeCoerce $ { name: "SFDC Account" }

responseFromSFDCSObject :: forall r. r  
responseFromSFDCSObject = unsafeCoerce $ {}


main :: Effect Unit
main = do 
    void $ request $ Query queryAccount
 

queryAccount :: SOQL Account 
queryAccount = SOQL "Select name from Account"

updateAccount :: SObject Account Unit
updateAccount = SObject { name: "Account" }  

-- f :: RowToList _ _ 
--         => VariantMatchCases _ _ (Effect (Either _ Account))
--         => Union _ _ _
--         => HasEndpoint (QueryEndpoint Account) Account
--         => (QueryEndpoint Account)
--         -> Effect (Either _ Account)
-- f = request $ Query queryAccount

-- request' :: forall result sfapi m r. 
--     HasEndpoint sfapi result
--     => HasNetwork m sfapi 
--     => sfapi 
--     -> ExceptV (RequestError sfapi r) m result
-- request' api = do 
--     eitherResult <- request api 
    
--     case eitherResult of 
--         Left e -> ExceptT $ pure $ Left $ requestServerError "server error"
--         Right result -> ExceptT $ pure $ Right result



