#effects of multicollinearity on reg coeff

### Find Total Sum of Squares
SST_m <- sum((Y-mean(Y))^2)
SST_m #12631172689
#Model includes X1,X2,X3
Cp.modelX89 <- lm(Y ~ X8+X9)
Cp.modelX4 <- lm(Y ~ X4)
Cp.modelX8 <- lm(Y ~ X8)
Cp.modelX9 <- lm(Y~ X9)
summary(Cp.modelX89)
anova(Cp.modelX4)
summary(Cp.modelX4)

Cp.modelX489 <- lm(Y~X4+X8+X9)
# Effect on coefficients for multicollinearity
summary(Cp.modelX9)$coefficients
b8_X8<-summary(Cp.modelX8)$coefficients[2]
b8_X8
summary(Cp.modelX89)$coefficients
b8_X8X9<-summary(Cp.modelX89)$coefficients[2]
b8_X8X9
b9_X9<-summary(Cp.modelX9)$coefficients[2]
b9_X8X9<-summary(Cp.modelX89)$coefficients[3]
b8 <- rbind(b8_X8,"N/A",b8_X8X9)
b9 <- rbind("N/A",b9_X9,b9_X8X9)
b_table<-cbind(b8,b9)
colnames(b_table)<- cbind("b8","b9")
rownames(b_table)<- rbind("X8","X9","X8,X9")
print(b_table,quote=FALSE)

# standard deviation for multicollinearity
sb8_X8<-summary(Cp.modelX8)$coefficients[2,2]
sb8_X8X9<-summary(Cp.modelX89)$coefficients[2,2]
sb9_X9<-summary(Cp.modelX9)$coefficients[2,2]
sb9_X8X9<-summary(Cp.modelX89)$coefficients[3,2]
sb8 <- rbind(sb8_X8,"N/A",sb8_X8X9)
sb9 <- rbind("N/A",sb9_X9,sb9_X8X9)
sb_table<-cbind(sb8,sb9)
colnames(sb_table)<- cbind("s(b8)","s(b9)")
rownames(sb_table)<- rbind("X8","X9","X8,X9")
print(sb_table,quote=FALSE)



# Effect on MSE for multicollinearity
SSE_X8<-sum((Cp[1]-predict(Cp.modelX8))^2)
SSE_X8
SSE_X8X9<-sum((Cp[1]-predict(Cp.modelX89))^2)
SSE_X8X9
R8_X8<-1-SSE_X8/SST
R8_X8
R8_X8X9<-1-SSE_X8X9/SST

MSE_X8<-SSE_X8/(n-2)
MSE_X8X9<-SSE_X8X9/(n-3)

R8a_X8<-1-MSE_X8*(n-1)/SST
R8a_X8X9<-1-MSE_X8X9*(n-1)/SST

MSE_table<-rbind(MSE_X8,MSE_X8X9)
R8_table<-rbind(R8_X8,R8_X8X9)
R8a_table<-rbind(R8a_X8,R8a_X8X9)
SSE_table<-rbind(SSE_X8,SSE_X8X9)
MStable<-cbind(SSE_table,R8_table,MSE_table,R8a_table)
colnames(MStable)<- cbind("SSE","R^2","MSE","Adjusted R^2")
rownames(MStable)<- rbind("X8","X8,X9") #adjustd R2 in negative not good STOP
print(MStable,quote=FALSE)

#Alternatively you can uselibrary(dvmisc)
#get_mse(Iris.modelX2, var.estimate = FALSE)
# Effect on fitted values for multicollinearity
X8h<-31
X9h<-25
Yhat_X8h<-Cp.modelX8$coefficients[1]+Cp.modelX8$coefficients[2]* X8h
Yhat_X89h<-Cp.modelX89$coefficients[1]+Cp.modelX89$coefficients[2]* X8h+Cp.modelX89$coefficients[3]* X9h
Yhat_table<-rbind(Yhat_X8h,Yhat_X89h)
colnames(Yhat_table)<- cbind("Y_hat")
rownames(Yhat_table)<- rbind("X8_h","X8_h,X9_h")
print(Yhat_table,quote=FALSE)


# Effect on t test beta_2=0 or and beta_3=0
pvaluebeta8_X8<-summary(Cp.modelX8)$coefficients[2,4]
pvaluebeta9_X9<-summary(Cp.modelX9)$coefficients[2,4]
pvaluebeta8_X8X9<-summary(Cp.modelX89)$coefficients[2,4]
pvaluebeta9_X8X9<-summary(Cp.modelX89)$coefficients[3,4]
pvaluebeta9_X4X8X9<-summary(Cp.modelX489)$coefficients[2,4]
beta8 <- rbind(pvaluebeta8_X8,"N/A",pvaluebeta8_X8X9,pvaluebeta9_X4X8X9)
beta9 <- rbind("N/A",pvaluebeta9_X9,pvaluebeta9_X8X9,pvaluebeta9_X4X8X9)
beta_table<-cbind(beta8,beta9)
colnames(beta_table)<- cbind("p value for beta_8","p value for beta_9")
rownames(beta_table)<- rbind("X8","X9","X8,X9","X4,X8,X9")
print(beta_table,quote=FALSE)

