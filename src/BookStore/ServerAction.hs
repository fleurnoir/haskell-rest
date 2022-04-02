module BookStore.ServerAction where
import BookStore.EitherTransform
import Happstack.Server

type ServerAction a = EitherT Response (ServerPartT IO) a

throw :: ServerAction Response -> ServerAction a
throw responseAction = do 
    response <- responseAction 
    EitherT $ return (Left response)

runServerAction :: ServerAction Response -> ServerPartT IO Response
runServerAction action = do
    either <- runEitherT action
    case either of
        Left x -> return x
        Right x -> return x