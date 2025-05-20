Cp <- read.csv("F:/Uni classes/Linear regression/Project/Carprice.csv", header=TRUE)

attach(Cp)
names(Cp)
n <- length(Cp$price)
cormatrix<-cor(Cp)
cormatrix
Cp$price<-(Cp$price-mean(Cp$price))/sqrt((n-1)*var(Cp$price))
Cp$wheelbase<-(Cp$wheelbase-mean(Cp$wheelbase))/sqrt((n-1)*var(Cp$wheelbase))
Cp$carlength<-(Cp$carlength-mean(Cp$carlength))/sqrt((n-1)*var(Cp$carlength))
Cp$carwidth<-(Cp$carwidth-mean(Cp$carwidth))/sqrt((n-1)*var(Cp$carwidth))
Cp$curbweight <-(Cp$curbweight -mean(Cp$curbweight ))/sqrt((n-1)*var(Cp$curbweight))
Cp$enginesize <-(Cp$enginesize-mean(Cp$enginesize))/sqrt((n-1)*var(Cp$enginesize))
Cp$boreratio <-(Cp$boreratio-mean(Cp$boreratio))/sqrt((n-1)*var(Cp$boreratio))
Cp$horsepower <-(Cp$horsepower-mean(Cp$horsepower))/sqrt((n-1)*var(Cp$horsepower))
Cp$citympg <-(Cp$citympg-mean(Cp$citympg))/sqrt((n-1)*var(Cp$citympg))
Cp$highwaympg <-(Cp$highwaympg-mean(Cp$highwaympg))/sqrt((n-1)*var(Cp$highwaympg))

Y <- Cp$price
X1 <- Cp$wheelbase
X2 <- Cp$carlength
X3 <- Cp$carwidth
X4 <- Cp$curbweight
X5 <- Cp$enginesize
X6 <- Cp$boreratio
X7 <- Cp$horsepower
X8 <- Cp$citympg
X9 <- Cp$highwaympg

library("ggplot2")                     
library("GGally") 
ggpairs(Cp, columns=c('wheelbase','carlength','carwidth','curbweight','enginesize','boreratio','horsepower',
                      'citympg','highwaympg','price'))


# Form the design matrix X (n rows, 4 Cols)
X <- as.matrix(cbind(X1,X2,X3,X4,X5,X6,X7,X8,X9)) 

# Form the Y-vector (n rows, 1 col)
Y <- as.matrix(Y,ncol=1) 
Y
# Notes: t(X) = transpose of X, %*% = matrix multiplication, solve(A) = A^(-1) 
XtX <- t(X) %*% X 
XtX
XtY <- t(X) %*% Y
XtY


#correlation matrix
cormatrixN <- cor(Cp[c('wheelbase','carlength','carwidth','curbweight','enginesize','boreratio','horsepower',
                       'citympg','highwaympg','price')])
cormatrixN

