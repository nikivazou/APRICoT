module Handler.Profile where

import Import

getProfileR :: Handler Html
getProfileR = do
    (_, user) <- requireAuthPair
    papers <- getPapers
    authorLists <- mapM getAuthorList papers
    let authorsAndPapers = zip authorLists papers
    defaultLayout $ do
        setTitle . toHtml $ userUsername user <> "'s User page"
        $(widgetFile "profile")

getPapers :: Handler [Entity Paper]
getPapers = do
    (uid, _) <- requireAuthPair
    papers <- runDB $ selectList [PaperOwner ==. uid] []
    return papers

getAuthorList :: Entity Paper -> Handler [Entity AuthorToPaper]
getAuthorList (Entity paperId _) = runDB $ selectList [AuthorToPaperPaper ==. paperId] []
