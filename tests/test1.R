############ checking local linear detector combined with modification procedure#################

library(polydect)

set.seed(8054)

n=100 ## number of design points

h1=0.16 ##threshold value

zalf<-qnorm(0.999995)  ##significance level

sigma=0.1  ##noise level

x<-c(floor(0.1*n):ceiling(0.9*n))/n
y<-numeric(length(x))

#####generate data#####
X<-c(1:n)/n
NS<-rnorm(n,0,sigma)
Y<-c(10-30*X[1:floor(n/3)],-360*(X[(floor(n/3)+1):floor(2*n/3)]-0.5)^2+11,exp(7.5*(X[(floor(2*n/3)+1):(n)]-2/3))-1)+NS

tot=0
pos<-c()
y<-m.det1(h1,x,X,Y)
y<-abs(y)
C<-sqrt(sigma^2*4.49/n/h1)*zalf

tot<-det.jmp(x,y,h1,C,n)[[1]]
pos<-det.jmp(x,y,h1,C,n)[[2]]
tot
pos
