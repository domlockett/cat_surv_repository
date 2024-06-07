#' Minimizes Bias of Multiple Answer Profiles
#'
#' Generating all possible combinations of length n from a response profile to determine
#'
#' @param catObj An object of class \code{Cat}
#' @param thetas A vector of numerics representing the true position on the latent trait.
#' @param ans_profiles A dataframe of values representing the full answer profile of multiple respondents.
#' @param n A numeric indicating the length the combinations of all subsetted answer profile should be.
#'
#' @details ....
#'
#' @return The best theta estimate for each respondent given the 
#'  
#' @author Haley Acevedo, Ryden Butler, Josh W. Cutler, Matt Malis, Jacob M. Montgomery, Tom Wilkinson, Erin Rossiter, Min Hee Seo, Alex Weil 
#' 
#'
#' @importFrom utils combn
#'
#' 
#' @name allOrac
#' 
#' @examples 
#' 
#'  ## Loading ltm Cat object
#' data(grm_cat)
#'    
#' ## load sample data
#'  
#'  
#'sr<-apply(thetaValue,2, simulateRespondents_test, cat=grm_cat, n= 10)
#'thetas<-estimateThetas(grm_cat, sr)

#' ## Create a threshold for the estimation
#' 
#'
#' 
#' ## Run 
#' 
#'allOrac(grm_cat, thetas,sr, n=3)
#' 
#' @author Haley Acevedo, Ryden Butler, Josh W. Cutler, Matt Malis, Jacob M. Montgomery, Tom Wilkinson, Erin Rossiter, Min Hee Seo, Alex Weil, Jaerin Kim, Dominique Lockett 
#' 
#' @importFrom utils combn
#' @importFrom plyr adply
#' 
#' @name allOrac
NULL

allOrac<- function(catObj, thetas, ans_profiles, n){
  orac<-rep(NA, nrow(ans_profiles))
  for (i in 1:nrow(ans_profiles)){
      
 orac[i]<-tryCatch({ 
   oracle(catObj=catObj,theta=thetas[i],ans_profiles=ans_profiles[i,],n=n)$theta_est
    
   },
   error = function(cond) {
     message(cond)
     return(NA)
   }
 )
  }

  store<-as.data.frame(orac)
  colnames(store)<-"theta_Est"
  

  return(store)
}
theta<-rep(-4:5,each=100)
try<-allOrac(grmList[[1]], thetaValue, respProf[1,],4)
estimateThetas(grmList[[1]], respProf[1,] )
