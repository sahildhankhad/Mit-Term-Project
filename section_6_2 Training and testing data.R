###############################################################
# This code covers the code presented in 
# Section 6.2 Logistic Regression And we findout the training error and testing error
###############################################################

###############################################################
# we will divide our dataset into 80% of training data and 20% of our testing data
###############################################################
#load the file
churn_input = as.data.frame(  read.csv("/Users/sdhankhad/Desktop/churn.csv")   )
head(churn_input)
# now i'm spreating Traning and testing dataset with 80% and 20%
dt=sort(sample(nrow(churn_input),nrow(churn_input)*.8))
traindataset<-churn_input[dt,]
testdataset<-churn_input[-dt,]
sum(churn_input$Churned)



Churn_logistic1 <- glm (Churned~Age + Married + Cust_years + Churned_contacts, 
                        data=traindataset, family=binomial(link="logit"))
summary(Churn_logistic1)

Churn_logistic2 <- glm (Churned~Age + Married +  Churned_contacts,
                        data=traindataset, family=binomial(link="logit"))
summary(Churn_logistic2)

Churn_logistic3 <- glm (Churned~Age + Churned_contacts, 
                        data=traindataset, family=binomial(link="logit"))
summary(Churn_logistic3)


# Deviance and the Log Likelihood Ratio Test

# Using the residual deviances from Churn_logistics2 and Churn_logistic3
# determine the signficance of the computed test statistic
summary(Churn_logistic2)
pchisq(.9 , 1, lower=FALSE) 

# Receiver Operating Characteristic (ROC) Curve

install.packages("ROCR")     #install, if necessary
library(ROCR)

pred = predict(Churn_logistic3, type="response")
predObj = prediction(pred, traindataset$Churned )

rocObj = performance(predObj, measure="tpr", x.measure="fpr")
aucObj = performance(predObj, measure="auc")  

plot(rocObj, main = paste("Area under the curve:", round(aucObj@y.values[[1]] ,4))) 

# extract the alpha(threshold), FPR, and TPR values from rocObj
alpha <- round(as.numeric(unlist(rocObj@alpha.values)),4)
fpr <- round(as.numeric(unlist(rocObj@x.values)),4)
tpr <- round(as.numeric(unlist(rocObj@y.values)),4)

# adjust margins and plot TPR and FPR
par(mar = c(5,5,2,5))
plot(alpha,tpr, xlab="Threshold", xlim=c(0,1), ylab="True positive rate", type="l")
par(new="True")
plot(alpha,fpr, xlab="", ylab="", axes=F, xlim=c(0,1), type="l" )
axis(side=4)
mtext(side=4, line=3, "False positive rate")
text(0.18,0.18,"FPR")
text(0.58,0.58,"TPR")

i <- which(round(alpha,2) == .5)
paste("Threshold=" , (alpha[i]) , " TPR=" , tpr[i] , " FPR=" , fpr[i])

i <- which(round(alpha,2) == .15)
paste("Threshold=" , (alpha[i]) , " TPR=" , tpr[i] , " FPR=" , fpr[i])

# now we are going to use our testing data and find the thrsold for that 

Churn_logistic1 <- glm (Churned~Age + Married + Cust_years + Churned_contacts, 
                        data=testdataset, family=binomial(link="logit"))
summary(Churn_logistic1)

Churn_logistic2 <- glm (Churned~Age + Married +  Churned_contacts,
                        data=testdataset, family=binomial(link="logit"))
summary(Churn_logistic2)

Churn_logistic3 <- glm (Churned~Age + Churned_contacts, 
                        data=testdataset, family=binomial(link="logit"))
summary(Churn_logistic3)


# Deviance and the Log Likelihood Ratio Test

# Using the residual deviances from Churn_logistics2 and Churn_logistic3
# determine the signficance of the computed test statistic
summary(Churn_logistic2)
pchisq(.9 , 1, lower=FALSE) 

# Receiver Operating Characteristic (ROC) Curve

install.packages("ROCR")     #install, if necessary
library(ROCR)

pred = predict(Churn_logistic3, type="response")
predObj = prediction(pred, testdataset$Churned )

rocObj = performance(predObj, measure="tpr", x.measure="fpr")
aucObj = performance(predObj, measure="auc")  

plot(rocObj, main = paste("Area under the curve:", round(aucObj@y.values[[1]] ,4))) 

# extract the alpha(threshold), FPR, and TPR values from rocObj
alpha <- round(as.numeric(unlist(rocObj@alpha.values)),4)
fpr <- round(as.numeric(unlist(rocObj@x.values)),4)
tpr <- round(as.numeric(unlist(rocObj@y.values)),4)

# adjust margins and plot TPR and FPR
par(mar = c(5,5,2,5))
plot(alpha,tpr, xlab="Threshold", xlim=c(0,1), ylab="True positive rate", type="l")
par(new="True")
plot(alpha,fpr, xlab="", ylab="", axes=F, xlim=c(0,1), type="l" )
axis(side=4)
mtext(side=4, line=3, "False positive rate")
text(0.18,0.18,"FPR")
text(0.58,0.58,"TPR")

i <- which(round(alpha,2) == .5)
paste("Threshold=" , (alpha[i]) , " TPR=" , tpr[i] , " FPR=" , fpr[i])

i <- which(round(alpha,2) == .15)
paste("Threshold=" , (alpha[i]) , " TPR=" , tpr[i] , " FPR=" , fpr[i])






