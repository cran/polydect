############ checking bandwidth selection procedure combined with all the other functions################# 
library(polydect)
data(DJIA)
attach(DJIA)
n=length(Open)

## X is the design points ##
X<-c(1:n)/n

## Y is the observed data ##
Y<-Open

## x is the set of positions that we are interested in estimation ##
x<-X[floor(0.1*n):ceiling(0.9*n)]

## specify bandwidth ##
h=0.10
h2=0.13

B=100

set.seed(1)
## Since the curvature of the data is kind of large, we use quadratic detector
order=2
bds(h,B,h2,X,Y,x,order)
