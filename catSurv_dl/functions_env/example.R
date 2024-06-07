library(ggplot2)
library(catSurv)
library(scales)
library(dplyr)
library(plyr)
library(latex2exp)
#load("~/Summer 2018/Montgomery/catSurv_dl/troubleshooting.RData")

## Plotting data

# Set a threshold 
grm_cat@lengthThreshold<-3


grm_cat@lengthThreshold
# Define answer profiles 

#first 100 rows -4, 2nd 100 -3, etc.
 # thetaValue<-as.data.frame(rep(-4:5, each=10))
 # respprof<-apply(thetaValue,1,  simulateRespondents, cat=grm_cat, n=10)
 # respprof<-ldply(respprof, rbind)
data(nfc)
respprof<-nfc

# Various cat methods
# First set changes selection, lets see more varition below:
# grm_KL<-grm_cat
# setSelection(grm_KL) <- "KL"
# 
# grm_EPV<-grm_cat
# setSelection(grm_EPV) <- "EPV"
# 
# grm_MFI<-grm_cat
# setSelection(grm_MFI) <- "MFI"
# 
# grm_MEI<-grm_cat
# setSelection(grm_MEI) <- "MEI"
# 
# grm_LKL<-grm_cat
# setSelection(grm_LKL) <- "LKL"
# 
# grm_PKL<-grm_cat
# setSelection(grm_PKL) <- "PKL"
# 
# grm_MLWI<-grm_cat
# setSelection(grm_MLWI) <- "MLWI"
# 
# grm_MFII<-grm_cat
# setSelection(grm_MFII) <- "MFII"
# 
# 
# grm_RAND<-grm_cat
# setSelection(grm_RAND) <- "RANDOM"
# 
# grmList<-list(grm_KL,grm_EPV,grm_MFI,grm_MEI,grm_LKL,grm_PKL,grm_MLWI,grm_MFII,grm_RAND)
# #Looking at different estimation methods [not WLE]:

grm_MAP<-grm_cat
grm_MAP@estimation<-"MAP"
grm_EAP<-grm_cat
grm_EAP@estimation<-"EAP"
grmList<- list(grm_MAP, grm_EAP )
#define fixed battery by the highest level of discrimination
respProf<-respprof
#respprof<-respProf
names(grmList[[1]]@discrimination)<-1:length(grmList[[1]]@discrimination)
dif<-as.integer(names(grmList[[1]]@discrimination[order(grmList[[1]]@discrimination)[1:3]]))
dif
respprof[1,]
respprof[,c(1,2,3,4,5,6,7,8,9,11,12,13,16,17,18)]<--1
fixed<-estimateThetas(grmList[[1]], respprof)
fixed[1]
#find any empty profiles:

empty<-function(resp){
  for (i in 1:nrow(resp)){
    if (sum(is.na(resp[i,]))==18){
      print(i)
    }
  }
}


#Define true theta of each respondent
thetaValue<-estimateThetas(grmList[[1]],respProf)

# so now we dropped the empty profile:

empty(respProf)
thetaValue<-thetaValue[-804]
fixed<-fixed[-804]
respProf<-respProf[-804,]
respprof<-respprof[-804,]

##now let's change all na responses to -1

respProf[is.na(respProf)]<- -1
sum(is.na(respProf))

#find the average estimates of the oracle function for these respondents
orac<-allOrac(grmList[[1]], thetaValue, respProf, n=3)

allOrac(grmList[[1]], thetaValue[1:5], respProf[1:5,], n=3)
allEst(list(grm_cat), respProf[1,])
estimateTheta(grm_cat)
grm_cat@answers<-as.numeric(respProf[1,])
#oracAvg<- tapply(orac[,1], orac[,2], mean)
#Now let's get our estimates for these thousand observations
 
##########################################
estimates<-allEst(grmList, respProf)
estimates[1,]

#run the function which finds blank responses and remove them from all estimates


#put all the estimates together
estimates<-cbind(estimates, orac, fixed)

#colnames(estimates)<-c("KL", "EPV", "MFI", "MEI", "LKL", 'PKL', 'MLWI', 'MFII', 'RANDOM', "Oracle", "Fixed")
estimates[1,]
#Calculate the bias for each cat Object
bias<-apply(estimates, 2, function(x){abs(thetaValue-x)})
bias[1,]
#attach the thetas to the corresponding respondent to find average by theta
bias<-as.data.frame(cbind(bias, thetaValue))
colnames(bias)<-c("MAP", 'EAP', 'Oracle',"Fixed","Theta")
#colnames(bias)<-c("KL", "EPV", "MFI", "MEI", "LKL", 'PKL', 'MLWI', 'MFII', 'RANDOM', "Oracle", "Fixed", 'Theta')
bias[1,]

######################################
#set a color and line type schemes
linecol<- c( "#339966", "#0066CC",
             "#ffcccc", "#660066", "#CC3300", 
             "#003333", "#660000", "#666666", "#99cccc", "#cc9900")
ltys<-c(2:11)
#Looking at different selection types:
#use par to split up graph view
# par(mar=c(4,3,2,9),mgp=c(1.5,0,0))
# #generic plot page
# plot(bias$Theta,y=c(1:length(thetaValue)), yaxt="n",tck=F, xlab= expression(theta), ylab="Bias", ylim=c(0,2), col="white", lty=1, lwd=1)
# #add oracle
# lines(lowess(bias$Theta, bias$Oracle),col="#003366")
# #make an axis to jacob's aesthetic preferences
# axis(2, at=seq(0,2, by=.4),labels=seq(0,2, by=.4), col.axis="black", las=2, tick=F)
# #use apply function to plot all of the biases and the distribution of thetas
# sapply(1:ncol(bias[,c(1:9,11)]), function(i){lines(lowess(bias$Theta, bias[,c(1:9,11)][,i]),col=alpha(linecol[i], .4),lty=ltys[i], lwd=2)})
# sapply(1:length(quantile(bias$Theta, c(0.95, 0.9, 0.1, 0.05))), function(i){abline(v=quantile(bias$Theta, c(0.95, 0.9, 0.1, 0.05))[i])})
# #add a legend
# legend("left",  inset=c(1,1.2), xpd=TRUE, bty="n", col = c("#003366", linecol),legend=c( "Oracle","KL", "EPV", "MFI", "MEI", "LKL", 'PKL', 'MLWI', 'MFII', 'RANDOM', "Fixed"), lty=c(1,ltys))
# title(main=TeX("Simulated Respondents' distance from $\\theta$ w/ varied Selection methods"),sub= "Vertical lines represent quantiles at 0.95, 0.9, 0.1, 0.05" , cex.sub=.7, cex.main=.8)
# #


#different estimation methods:
par(mar=c(4,3,2,9),mgp=c(1.5,0,0))
plot(bias$Theta,y=c(1:length(thetaValue)), yaxt="n",tck=F, xlab= expression(theta), ylab="Bias", ylim=c(0,1), col="white", lty=1, lwd=1)
axis(2, at=seq(0,1, by=.2),labels=seq(0,1, by=.2), col.axis="black", las=2, tick=F)

lines(lowess(bias$Theta, bias[,1]),col=alpha(linecol[2], .4),lty=ltys[2], lwd=2)
lines(lowess(bias$Theta, bias[,2]),col=alpha(linecol[4], .4),lty=ltys[4], lwd=2)
lines(lowess(bias$Theta, bias[,3]),col=alpha(linecol[5], .4),lty=ltys[5], lwd=2)
lines(lowess(bias$Theta, bias[,4]),col=alpha(linecol[6], .4),lty=ltys[6], lwd=2)
sapply(1:length(quantile(bias$Theta, c(0.95, 0.9, 0.1, 0.05))), function(i){abline(v=quantile(bias$Theta, c(0.95, 0.9, 0.1, 0.05))[i])})

legend("left",  inset=c(1,1.2), xpd=TRUE, bty="n", col = linecol[c(2,4,5,6)],legend=c( "MAP", 'EAP', 'Oracle', 'Fixed'), lty=ltys[c(2,4,5,6)])

title(main="Simulated Respondents' distance from $\\theta$ w/ varied Estimation methods",sub= "Vertical lines represent quantiles at 0.95, 0.9, 0.1, 0.05" , cex.sub=.7, cex.main=,8)

#######################################

#now we want to know the average information instead of Bias
# allEst should also return the fisherInfo data
#gotta remove the missing profile from respprof before doing fish
fish<- allFish(grmList, thetaValue,respProf)
fish<- cbind(fish, thetaValue)


fish<-as.data.frame(fish)

#fishAvg<-apply(fish[,1:2], 2,function(x){tapply(x, fish[,3], mean, na.rm=T)} )
colnames(fish)<-c("MAP", 'EAP','Theta')
# colnames(fish)<-c("KL", "EPV", "MFI", "MEI", "LKL", 'PKL', 'MLWI', 'MFII', 'RANDOM',  "Theta")
fish[1,]

#for selection:
# par(mar=c(4,3,2,9),mgp=c(1.5,0,0))
# #generic plot page
# plot(fish$Theta,y=c(1:length(thetaValue)), yaxt="n", ylab="Fish_test",xlab=TeX("$\\theta$"), tck=F,ylim=c(0,1), col="white", lty=1, lwd=1)
# #add oracle
# #make an axis to jacob's aesthetic preferences
# axis(2, at=seq(0,1, by=.2),labels=seq(0,1, by=.2), col.axis="black", las=2, tick=F)
# #use apply function to plot all of the biases and the distribution of thetas
# sapply(1:ncol(fish[,c(1:9)]), function(i){lines(lowess(fish$Theta, fish[,c(1:9)][,i]),col=alpha(linecol[i], .4),lty=ltys[i], lwd=2)})
# sapply(1:length(quantile(fish$Theta, c(0.95, 0.9, 0.1, 0.05))), function(i){abline(v=quantile(fish$Theta, c(0.95, 0.9, 0.1, 0.05))[i])})
# #add a legend
# legend("left",  inset=c(1,1.2), xpd=TRUE, bty="n", col = c(linecol[c(1:9)]),legend=c( colnames(fish[1:9])), lty=c(ltys[c(1:9)]))
# title(main=TeX("Simulated Respondents' Fisher info w/ varied Selection methods"),sub= "Vertical lines represent quantiles at 0.95, 0.9, 0.1, 0.05" , cex.sub=.7, cex.main=.8)
# # 


#for estimation:
par(mar=c(4,3,2,9),mgp=c(1.5,0,0))
plot(fish$Theta,y=c(1:length(thetaValue)), yaxt="n",tck=F, xlab= expression(theta), ylab="Fisher_test", ylim=c(0,1), col="white", lty=1, lwd=1)
axis(2, at=seq(0,1, by=.4),labels=seq(0,1, by=.4), col.axis="black", las=2, tick=F)

lines(lowess(fish$Theta, fish[,1]),col=alpha(linecol[2], .4),lty=ltys[2], lwd=2)
lines(lowess(fish$Theta, fish[,2]),col=alpha(linecol[4], .4),lty=ltys[4], lwd=2)
sapply(1:length(quantile(fish$Theta, c(0.95, 0.9, 0.1, 0.05))), function(i){abline(v=quantile(fish$Theta, c(0.95, 0.9, 0.1, 0.05))[i])})
legend("left",  inset=c(1,1.2), xpd=TRUE, bty="n", col = linecol[c(2,4)],legend=c( "MAP", 'EAP'), lty=ltys[c(2,4)])
title(main=TeX("Simulated Respondents' Fisher info w/ varied Estimation methods"),sub= "Vertical lines represent quantiles at 0.95, 0.9, 0.1, 0.05" , cex.sub=.7, cex.main=.8)
?title
## Unit test for allEst

rm(grm_TRUE)


grm<-list( grm_cat, ltm_cat)
grm[[1]]@selection
allEst(list(grm_cat), respProf[1,])
grm_cat@lengthThreshold
length(grm)
cat<-grm_cat
cat<-grm_cat@answers<-rep(NA, 18)
cat<-storeAnswer(grm_cat, item=selectItem(grm_cat)$next_item, as.numeric(respProf[1,])[selectItem(grm_cat)$next_item])
cat<-storeAnswer(cat, item=selectItem(cat)$next_item, as.numeric(respProf[1,])[selectItem(cat)$next_item])
cat<-storeAnswer(cat, item=selectItem(cat)$next_item, as.numeric(respProf[1,])[selectItem(cat)$next_item])
estimateTheta(cat)
cat@answers
