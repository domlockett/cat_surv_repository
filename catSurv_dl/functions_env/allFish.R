#' A function that finds total information that can be gained via answered items .
#'
#' Takes in a a \code{Cat} object, a set of respondents, and their corresponding \code{theta} values, to calculate the amount of information which can be extracted given an adaptive battery  
#'
#' 
#' @param catObjs A list of \code{Cat} objects of the same class.
#' @param theta A vector of numerics representing the true value of theta.
#' @param resp One or many full response profiles.
#' 
#' @details The function takes a \code{Cat} object, \code{theta}, and response profiles. 
#' The user defines the selection type, estimation type, etc. so that the questions can be applied adaptively
#' These adaptive profiles are then used to calculate the total inforamtion gained for a respondent for all answered
#' items, conditioned on \code{theta}.
#' 
#' @return The function \code{allFish} returns a dataframe.  Each \code{Cat} object returns a column and each respondent returns a row.
#' 
#' 
#' 
#' @seealso \code{\link{Cat-class}}, \code{\link{fisherTestInfo}}, \code{\link{selectItem}}, \code{\link{store}}
#' 
#' @examples 
#' 
#'  ## Loading ltm Cat object
#' data(grm_cat)
#'    
#' ## load sample data
#'  
#' respondents<-apply(thetaValue[1:3], 2, simulateRespondents, cat=grm_cat, n=10)
#' 
#' #Choose your \code{cat} object and the types of estimation you would like to use
#'  grm_MAP<-grm_cat
#'  grm_MAP@estimation<-"MAP"
#'  grm_EAP<-grm_cat
#'  grm_EAP@estimation<-"EAP"
#' 
#' 
#' grmList<-list(grm_MAP, grm_EAP)
#' ## Create a threshold for the estimation
#' 
#' ## You must set some sort of CheckStopRule
#' grm_cat@lengthThreshold<-4
#' 
#' ## Run 
#' 
#' allFish(catObjs=grmList, resp=respondents)
#' @author Haley Acevedo, Ryden Butler, Josh W. Cutler, Matt Malis, Jacob M. Montgomery, Tom Wilkinson, Erin Rossiter, Min Hee Seo, Alex Weil, Jaerin Kim, Dominique Lockett 
#' 
#' @rdname allFish
#' 
#' @export
allFish <- function(catObjs, resp){
  UseMethod("allFish", catObj)
}


allFish<-function(catObjs=list(),theta,resp){
  
  #the multiple cat objects must be of the same class and be in list form
  store<-NULL
  obj<-NULL
  #loop because we want to go through all catObjs
  ## if you want to do single catObj make sure to put it in a list(catObj)
  for (i in 1:length(catObjs)){
    #create a dummy object so we can collect the model type of every catObj
    obj<-c(obj, catObjs[[i]]@model)
    #if there are more than one types of catObj (ltm, grm), stop
    if (length(unique(obj))!=1) { #USE THE @MODEL SECTION
      stop("List of Cat objects must be of the same type*")
    }
    #make sure answer profile is appropriate for catObj grm@answers = 18 so make sure battery has 18 questions
    if(length(resp) != length(catObjs[[i]]@answers)){
      stop("Response profile is not compatible with Cat object.")
    } 
    #two selection types must have the first answer, cannot generate the first answer
    if(catObjs[[i]]@selection=="KL" | catObjs[[i]]@selection=="MFII"){
      #lots going on here; go over each respondent and return  each theta value
      #we do this adaptively by selecting the next question  and grabbing it from the answer profile
      
      store<-(rbind(store,apply(resp,1,  function(catObj, answers=c()){
        #throw a tryCatch in here because there are troubles with MAP
        
        fish <- tryCatch({   
          
          #Bc they cant generate 1st answer we resort to EPV 
          orig<-catObjs[[i]]@selection
          tempcatObj<-catObjs[[i]]
          setSelection(tempcatObj) <-"EPV"
          #here we pick the first item that "EPV" chooses
          while ( answers[selectItem(tempcatObj)$next_item]== -1){
            tempcatObj<-storeAnswer(catObj=tempcatObj,item=selectItem(tempcatObj)$next_item, answers[selectItem(tempcatObj)$next_item])
          }
          tempcatObj<-storeAnswer(catObj=tempcatObj,item=selectItem(tempcatObj)$next_item, answers[selectItem(tempcatObj)$next_item])
          # after first question we flip back to KL and continue as usual
          
          setSelection(tempcatObj) <-orig
          cat<-tempcatObj
          # Look to checkStopRules and identify the question selection and grab the corresponding answer
          while (checkStopRules(cat)==F) {
            cat<-storeAnswer(cat, item=selectItem(cat)$next_item, answers[selectItem(cat)$next_item])}
          
          #used estimateTheta here probably could have went with estimateThetas, idk
          fisherTestInfo(cat, theta[i])
        }, 
        
        #back to tryCatch there are lots of potential errors so we just nip it in the bud and return an error message and NA's
        error = function(cond) {
          message(cond)
          return(NA)
        }
        )
        return(fish)
      }, catObj=catObjs[[i]])))
      
      
      # colnames(store)<-  sapply(catObjs, function(x) catObjs@estimation))
      
      
    }else
      #apply theta basic to a set or respondents and return the values for every catObj provided by the user.
      store<-(rbind(store,apply(resp,1,  function(catObj, answers=c()){
        if(length(resp) != length(catObj@answers)){
          stop("Response profile is not compatible with Cat object.")
        }
        
        
        fish <- tryCatch(
          {
            cat<-storeAnswer(catObj,item=selectItem(catObj)$next_item, answers[selectItem(catObj)$next_item])
            while (checkStopRules(cat)==F) {
              cat<-storeAnswer(cat, item=selectItem(cat)$next_item, answers[selectItem(cat)$next_item])
            }
            ## no return statement needed
            fisherTestInfo(cat,theta[i])
          }, 
          error = function(cond) {
            message(cond)
            return(NA)
          }
        )
        return(fish)
      }, catObj=catObjs[[i]])))
    
  }
  store<-t(store)
  colnames(store)<- paste("fish", 1:ncol(store))
  return(store)
}
allFish(grmList, thetaValue[c(1,23,654,789,323,645)], respProf[c(1,23,654,789,323,645),])
