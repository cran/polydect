library(polydect)
############ checking noise level estimation procedure #################

set.seed(1)
n=100
sigma=0.25
## X is the design points ##
X<-c(1:n)/n

## Y is the observed data ##
NS<-rnorm(n,0,sigma)
Y<-c(10-30*X[1:floor(n/3)],-360*(X[(floor(n/3)+1):floor(2*n/3)]-0.5)^2+11,exp(7.5*(X[(floor(2*n/3)+1):(n)]-2/3))-1)+NS

## x is the set of positions that we are interested in estimation ##
x<-c(floor(0.1*n):ceiling(0.9*n))/n

sigma.est<-sqrt(sig.est(x,X,Y,0.05))
sigma.est

