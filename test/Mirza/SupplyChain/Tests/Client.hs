module Mirza.SupplyChain.Tests.Client where

import           Mirza.SupplyChain.Tests.Settings

import           Control.Concurrent               (ThreadId, forkIO, killThread)
import           System.IO.Unsafe                 (unsafePerformIO)

import qualified Network.HTTP.Client              as C
import           Network.Socket
import qualified Network.Wai                      as Wai
import           Network.Wai.Handler.Warp

import           Servant.API.BasicAuth
import           Servant.Client

import           Data.Bifunctor
import           Data.Either                      (isLeft, isRight)
import           Data.Maybe                       (fromJust)

import           Data.Text.Encoding               (encodeUtf8)

import           Test.Tasty.Hspec

import           Mirza.SupplyChain.Main           (ServerOptions (..),
                                                   initApplication,
                                                   initSCSContext)
import           Mirza.SupplyChain.Types

import           Data.GS1.EPC                     (GS1CompanyPrefix (..))

import           Mirza.SupplyChain.Client.Servant

import           Katip                            (Severity (DebugS))
import           Mirza.SupplyChain.Tests.Dummies

import           Text.Email.Validate              (emailAddress, toByteString)


-- Cribbed from https://github.com/haskell-servant/servant/blob/master/servant-client/test/Servant/ClientSpec.hs

-- === Servant Client tests

userABC :: NewUser
userABC = NewUser
  { newUserPhoneNumber = "0400 111 222"
  , newUserEmailAddress = fromJust . emailAddress $ "abc@example.com"
  , newUserFirstName = "Biz Johnny"
  , newUserLastName = "Smith Biz"
  , newUserCompany = GS1CompanyPrefix "something"
  , newUserPassword = "re4lly$ecret14!"}

authABC :: BasicAuthData
authABC = BasicAuthData
  (toByteString . newUserEmailAddress $ userABC)
  (encodeUtf8   . newUserPassword     $ userABC)

runApp :: IO (ThreadId, BaseUrl)
runApp = do
  let so = (ServerOptions Dev False testDbConnStr "127.0.0.1" 8000 14 8 1 DebugS)
  ctx <- initSCSContext so
  startWaiApp =<< initApplication so ctx

clientSpec :: Spec
clientSpec =
  beforeAll runApp $
  afterAll endWaiApp $ do
    describe "SupplyChain.Client new user" $ do
      it "Can create a new user" $ \(_,baseurl) -> do
        res <- first show <$> runClient (newUser userABC) baseurl
        res `shouldSatisfy` isRight

      it "Can't reuse email address" $ \(_,baseurl) -> do
        res <- first show <$> runClient (newUser userABC) baseurl
        res `shouldSatisfy` isLeft

    describe "BasicAuth" $ do
      it "Should be able to authenticate" $ \(_,baseurl) -> do
        res <- first show <$> runClient (contactsInfo authABC) baseurl
        res `shouldBe` Right []

      it "Should fail to authenticate with unknown user" $ \(_,baseurl) -> do
        res <- first show <$> runClient
                (contactsInfo (BasicAuthData "xyz@example.com" "notagoodpassword"))
                baseurl
        res `shouldSatisfy` isLeft
    describe "Events can be inserted by the new user" $ do
      -- TODO: Events need their EventId returned to user
      it "Can insert Object events" $ \(_,baseurl) -> do
        res <- first show <$> runClient
                (insertObjectEvent authABC dummyObject)
                baseurl
        print res
        res `shouldSatisfy` isRight
      it "Can insert Aggregation events" $ \(_,baseurl) -> do
        res <- first show <$> runClient
                (insertAggEvent authABC dummyAggregation)
                baseurl
        print res
        res `shouldSatisfy` isRight
      it "Can insert Transaction events" $ \(_,baseurl) -> do
        res <- first show <$> runClient
                (insertTransactEvent authABC dummyTransaction)
                baseurl
        print res
        res `shouldSatisfy` isRight
      it "Can insert Transformation events" $ \(_,baseurl) -> do
        res <- first show <$> runClient
                (insertTransfEvent authABC dummyTransformation)
                baseurl
        print res
        res `shouldSatisfy` isRight
      xit "Can retrieve all submitted events" $ \(_,_baseurl) ->
        pending
        --   res@(Right evs) <- first show <$> runClient
        --           (eventList authABC uid) -- uid of userABC
        --           baseurl
        --   print res
        --   res `shouldSatisfy` isRight

{-
Check Provenance of a labelEPC
where I've used head, you need to use map to actually do it for all elements in the list. I've just done one element for illustrative purposes.
eventList ← listEvents <labelEPC>
let event = head eventList
eventInfo ← eventInfo(eventID)
(sig, uid) = head (signatures eventInfo)
publicKey ← getPublicKey uid
assert $ decrypt(sig, publicKey) == (joseText eventInfo)

Insert an ObjectEvent
Create an ObjectEvent
add it via the API
sign it
Insert an AggregationEvent
Create an aggregation event
add it
sign it

Insert an TransformationEvent
Create an transformation event
add it
sign it

Sign AND countersign a TransactionEvent
(eventID, joseTxt) ← insertTransactionEvent transactionEvent
signedEvent = sign(joseTxt, privKey)
sign(signedEvent)
addUserToEvent(user2ID, eventID)
.. then user2 does the same thing with their priv key, and sends it using the "event/sign" api call.

Check for tampering by comparing to Blockchain hash
eventInfo ← eventInfo(eventID)
joseTxt = joseText eventInfo
expectedHash = hash joseText
blockchainID = blockchainID eventInfo
bcHash = getBlockchainHash(blockchainID)
assert (bcHash == expectedHash)
Get all events that relate to a labelEPC
eventList ← listEvents <labelEPC>
subEvents eventList = [e | e ← eventList, if
(eventType e == aggregationEvent || eventType e == transformationEvent)
then (map subEvents $ map listEvents (getSubEPCs e)]
Keys
add, get, getInfo public key
revoke public key
..these will be moved into the registery soon.
Contacts
Add, remove and search for contacts.


-}




    -- describe "Signatures" $ do
    --   it "Can "

-- Plumbing

startWaiApp :: Wai.Application -> IO (ThreadId, BaseUrl)
startWaiApp app = do
    (prt, sock) <- openTestSocket
    let settings = setPort prt defaultSettings
    thread <- forkIO $ runSettingsSocket settings sock app
    return (thread, BaseUrl Http "localhost" prt "")


endWaiApp :: (ThreadId, BaseUrl) -> IO ()
endWaiApp (thread, _) = killThread thread

openTestSocket :: IO (Port, Socket)
openTestSocket = do
  s <- socket AF_INET Stream defaultProtocol
  localhost <- inet_addr "127.0.0.1"
  bind s (SockAddrInet aNY_PORT localhost)
  listen s 1
  prt <- socketPort s
  return (fromIntegral prt, s)



{-# NOINLINE manager' #-}
manager' :: C.Manager
manager' = unsafePerformIO $ C.newManager C.defaultManagerSettings

runClient :: ClientM a -> BaseUrl -> IO (Either ServantError a)
runClient x baseUrl' = runClientM x (mkClientEnv manager' baseUrl')


-- defaultEnv :: IO Env
-- defaultEnv = (\conn -> Env Dev conn Scrypt.defaultParams) <$> defaultPool

-- defaultPool :: IO (Pool Connection)
-- defaultPool = Pool.createPool (connectPostgreSQL testDbNameConnStr) close
--                 1 -- Number of "sub-pools",
--                 60 -- How long in seconds to keep a connection open for reuse
--                 10 -- Max number of connections to have open at any one time

