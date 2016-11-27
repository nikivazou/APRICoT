module Handler.Upload where

import Import
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Data.Conduit.Binary

import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)

data FileForm = FileForm
    { fileInfo :: FileInfo
    , title    :: Text
    , author   :: Text
    , abstract :: Textarea
    }

postUploadR :: Handler Html
postUploadR = do
    ((result, _), _) <- runFormPost uploadForm
    case result of
        FormSuccess (FileForm fi title author abstract) -> do
            fileBytes <- runResourceT $ fileSource fi $$ sinkLbs
            (uid, _) <- requireAuthPair
            paperId <- runDB $ insert $ Paper uid (fileName fi)
                            title (unTextarea abstract)
                                 (S.pack . L.unpack $ fileBytes)
            runDB $ insert_ $ AuthorToPaper author paperId
            setMessage "PDF saved"
            redirect ProfileR 
        _ -> do
            setMessage "Something went wrong"
            redirect UploadR 

getUploadR :: Handler Html
getUploadR = do
    (formWidget, formEnctype) <- generateFormPost uploadForm
    defaultLayout $ do
        $(widgetFile "upload")

uploadForm :: Form FileForm
uploadForm = renderBootstrap3 BootstrapBasicForm $ FileForm
    <$> fileAFormReq "Choose a file"
    <*> areq textField "Paper Title" Nothing
    <*> areq textField "Author" Nothing
    <*> areq textareaField "Abstract" Nothing
