module Security where 



import Tagged 
import Prelude hiding (print)

showPaperUnSafe :: World -> PaperId -> World
showPaperUnSafe w pid = 
  print w (getCurrentUser w) (getPaperAuthors w pid >>= (\authors -> return (show authors)))


showPaperUnsoundSafe :: World -> PaperId -> World
showPaperUnsoundSafe w pid = 
  let out = do authors <- getPaperAuthors w pid 
               return (show authors) 
  in print w (getCurrentUser w) out 
