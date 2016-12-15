module Security where 


{-@ LIQUID "--no-pattern-inline" @-}

import Tagged 
import Prelude hiding (print)

showPaperSafeLifty :: World -> PaperId -> World
showPaperSafeLifty w pid = 
  let u = getCurrentUser w 
      out = do title   <- getPaperTitle w pid
               chair   <- getChair
               user    <- u 
               authors <- if user == chair then getPaperAuthors w pid else return []
               return (title ++ ":_" ++ show authors) 
  in print w u out 


showPaperSafe :: World -> PaperId -> World
showPaperSafe w pid = 
  let u = getCurrentUser w 
      out = do title   <- getPaperTitle w pid
               chair   <- getChair
               authors <- whenUserIsChair w $ getPaperAuthors w pid 
               return (title ++ ":_" ++ show authors) 
  in print w u out 


showPaperUnSafe :: World -> PaperId -> World
showPaperUnSafe w pid = 
  let u = getCurrentUser w 
      out = do title   <- getPaperTitle w pid
               authors <- getPaperAuthors w pid 
               return (title ++ ":_" ++ show authors) 
  in print w u out 
