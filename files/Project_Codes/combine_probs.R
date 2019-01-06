combine_probs <- function(matchdat){
  matchdat=matchdat[,Match_Result:=NULL]
  combined=merge(matchdat,predictions_gbm$predictions,by='matchId')
  combined=combined[,H1:= (Home.y+Home_Prob)/2]
  combined=combined[,T1:= (Tie+Tie_Prob)/2]
  combined=combined[,A1:= (Away.y+Away_Prob)/2]
  
  combined2=merge(matchdat,predictions_gbm$predictions,by='matchId')
  combined2=combined2[,H1:= ifelse(Tie_Prob>Tie,Home.y,Home_Prob)]
  combined2=combined2[,T1:= ifelse(Tie_Prob>Tie,Tie,Tie_Prob)]
  combined2=combined2[,A1:= ifelse(Tie_Prob>Tie,Away.y,Away_Prob)]

  return(combined2)
}