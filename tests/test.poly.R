########## checking local polynomial detector combined with variance estimation and modification precedure ##########
library(polydect)
set.seed(8054)
n=100

## X is the design points ##
X<-c(1:n)/n

## Y is the observed data ##
sigma=0.1 #specify the noise level
NS<-rnorm(n,0,sigma)   #generate the noise
Y<-c(2/3-2*X[1:floor(n/3)],1+0*X[(floor(n/3)+1):floor(2*n/3)],-2*(X[(floor(2*n/3)+1):(n)]-2/3)*(X[(floor(2*n/3)+1):n]-2))+NS

## look at the data ##
plot(X,Y)

## x is the set of positions that we are interested in estimation ##
x<-X[floor(0.1*n):ceiling(0.9*n)]

## specify bandwidth ##
h0=0.04

## Since the data does not look to have large slope or curvature, we use kernel detector, which has order 0.

polydect(h0,X,Y,x,0)

