library(polydect)

############ checking local kernel detector combined with modification procedure#################

set.seed(8054)

n=100 ## number of design points

h0=0.04 ##threshold value

zalf<-qnorm(0.99995)  ##significance level

sigma=0.1  ##noise level

x<-c(floor(0.1*n):ceiling(0.9*n))/n
y<-numeric(length(x))

#####generate data#####
X<-c(1:n)/n
NS<-rnorm(n,0,sigma)
Y<-c(2/3-2*X[1:floor(n/3)],1+0*X[(floor(n/3)+1):floor(2*n/3)],-2*(X[(floor(2*n/3)+1):(n)]-2/3)*(X[(floor(2*n/3)
	+1):n]-2))+NS

tot=0
pos<-c()
y<-m.det0(h0,x,X,Y)
y<-abs(y)
C<-sqrt(sigma^2*2.4/n/h0)*zalf

tot<-det.jmp(x,y,h0,C,n)[[1]]
pos<-det.jmp(x,y,h0,C,n)[[2]]
tot
pos
