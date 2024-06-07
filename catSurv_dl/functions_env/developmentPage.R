##Plot theta estimates using simThetas, oracle, allEst
library(plyr)
library(catSurv)
load("C:/Users/dl0ck/OneDrive/Documents/Summer 2018/Montgomery/catSurv_dl/environments with functions.RData")
grm_cat@lengthThreshold<-4
thetaValue<-data.frame(1)
sr<-apply(as.data.frame(1:5),1, simulateRespondents_test, cat=grm_cat, n= 10)
sr<-ldply(sr, rbind)
sr<-sr[,-1]
grm<-grm_cat


?oracle
th<-estimateThetas(grm_EAP, responses=sr)


realThetas<-rep(1, 10)
  grm[[1]]
allEst(grm,sr)
allOrac<-function(catObj,theta,resp, n){
  oracle<-rep(NA, nrow(resp))
  for (i in 1:nrow(resp)){
    oracle[i]<-oracle_test(catObj=catObj,theta=theta[i],x=resp[i,],n=n)$theta_est
  }
  return(as.data.frame(oracle))
  }

orac_results<-allOrac(resp=sr, catObj=grm_cat,theta=realThetas, n=5)

oracle_test(catObj=grm_cat,theta=th[1],x=sr[1,], n=2)

cat_results<-allEst(grm,sr)

cbind(th[1], orac_results)




plot(th, cat_results[,1], pch=19)

points(th, orac_results$oracle, col='green')

?nextItem


## unit test..? 1 case when we no positively it is correct
## write a summary function that evaluate()** that takes argument and
##fix collors
## use actual respondents from nfc waling them through all est
e

sr<-apply(thetaValue,2, simulateRespondents_test, cat=grm_cat, n= 10)
but<-allEst(grm_cat,sr$X1)


dim(nfc)
?grm_cat



#############################
# Here we are going to start using tryCatch() to stop allEst from breaking computers:

r<-allEst(list(grm_MAP), respProf[1:1000,])

grm_MAP<-grm_cat
grm_MAP@estimation<-"MAP"
grm_WLE<-grm_cat
grm_WLE@estimation<-"WLE"

grm<-list(grm_MAP, grm_EAP)