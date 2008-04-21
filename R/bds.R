 bds<-function(h,B,h2,X,Y,x,order)
{
	stopifnot(is.numeric(h))
	stopifnot(is.numeric(B))
	stopifnot(is.numeric(X))
	stopifnot(is.numeric(Y))
	stopifnot(is.numeric(h2))
	stopifnot(is.numeric(x))
	stopifnot(is.numeric(order))
	stopifnot(order-floor(order)==0)
	stopifnot(B-floor(B)==0)
       stopifnot(order>=0)
	stopifnot(order<=3)
	stopifnot(h>0)
	stopifnot(B>0)
	stopifnot(h2>0)
	stopifnot(length(X)==length(Y))
	y<-numeric(length(x))
	n=length(X)
 if (order==0) {
 #######calculate pos and mag########

 	tot=0
 	pos<-c()
	y<-m.det0(h,x,X,Y)
	sigma<-sqrt(sig.est(x,X,Y,h))
 	C<-sqrt(sigma^2*2.4/n/h)*qnorm(0.99995)
 	o<-det.jmp(x,abs(y),h,C,n)
	pos<-o[[1]]
	mag<-o[[2]]

 ########Calculate continuous parts#########
if (length(pos)>0) {
z<-c()
z=Y
w<-c(1:n)/n	
if (length(pos>=1)) {for (i in 1:length(pos)) z<-z-mag[i]*I(w>pos[i])}

#########Estimate the continuous part and residual##########

zhat<-c()
zhat<-lest(h2,x,X,z)
epsil<-c()
epsil<-z[floor(0.1*n):ceiling(0.9*n)]-zhat

######### Get the estimated original data (middle part)###############

ey<-rep(0,length(Y))
ey[floor(0.1*n):ceiling(0.9*n)]<-zhat
for (i in 1:length(pos)) ey=ey+mag[i]*I(w>pos[i])
ey[1:floor(0.1*n)-1]=Y[1:floor(0.1*n)-1]
ey[ceiling(0.9*n)+1:n]=Y[ceiling(0.9*n)+1:n]
ey<-ey[1:n]

 #######bootstrap resample########
 
 	totds=0
	num=0
	Ys<-c()
 	for(i in 1:B)
		{
 		est.pos<-c()
        	sindex<-sample(1:length(epsil),length(epsil),replace=TRUE)
		noise<-rep(0,length(Y))
		noise[floor(0.1*n):ceiling(0.9*n)]<-epsil[sindex]
 		Ys<-ey+noise   
 	y<-m.det0(h,x,X,Ys)
 	est.pos<-det.jmp(x,abs(y),h,C,n)[[1]]
	totds<-totds+abs(HD(pos,est.pos))
 		}  
	}
        else totds=1
 }
 else if (order==1) {
 #######calculate pos and mag########

 	tot=0
 	pos<-c()
	y<-m.det1(h,x,X,Y)
	sigma<-sqrt(sig.est(x,X,Y,h))
 	C<-sqrt(sigma^2*4.49/n/h)*qnorm(0.999995)
 	o<-det.jmp(x,abs(y),h,C,n)
	pos<-o[[1]]
	mag<-o[[2]]

 ########Calculate continuous parts#########
if (length(pos)>0) {
z<-c()
z=Y
w<-c(1:n)/n	
if (length(pos>=1)) {for (i in 1:length(pos)) z<-z-mag[i]*I(w>pos[i])}

#########Estimate the continuous part and residual##########

zhat<-c()
zhat<-lest(h2,x,X,z)
epsil<-c()
epsil<-z[floor(0.1*n):ceiling(0.9*n)]-zhat

######### Get the estimated original data (middle part)###############

ey<-rep(0,length(Y))
ey[floor(0.1*n):ceiling(0.9*n)]<-zhat
for (i in 1:length(pos)) ey=ey+mag[i]*I(w>pos[i])
ey[1:floor(0.1*n)-1]=Y[1:floor(0.1*n)-1]
ey[ceiling(0.9*n)+1:n]=Y[ceiling(0.9*n)+1:n]
ey<-ey[1:n]

 #######bootstrap resample########
 
 	totds=0
	num=0
	Ys<-c()
 	for(i in 1:B)
		{
 		est.pos<-c()
       	sindex<-sample(1:length(epsil),length(epsil),replace=TRUE)
		noise<-rep(0,length(Y))
		noise[floor(0.1*n):ceiling(0.9*n)]<-epsil[sindex]
 		Ys<-ey+noise   
 	y<-m.det1(h,x,X,Ys)
 	est.pos<-det.jmp(x,abs(y),h,C,n)[[1]]
	totds<-totds+abs(HD(pos,est.pos))
 		}  
	}
        else totds=1
}
else if (order==2)
{
 #######calculate pos and mag########

 	tot=0
 	pos<-c()
	y<-m.det1(h,x,X,Y)
	sigma<-sqrt(sig.est(x,X,Y,h))
 	C<-sqrt(2*sigma^2*4.44178/n/h)*qnorm(0.999999995)
 	o<-det.jmp(x,abs(y),h,C,n)
	pos<-o[[1]]
	mag<-o[[2]]

 ########Calculate continuous parts#########
if (length(pos)>0) {
z<-c()
z=Y
w<-c(1:n)/n	
if (length(pos>=1)) {for (i in 1:length(pos)) z<-z-mag[i]*I(w>pos[i])}

#########Estimate the continuous part and residual##########

zhat<-c()
zhat<-lest(h2,x,X,z)
epsil<-c()
epsil<-z[floor(0.1*n):ceiling(0.9*n)]-zhat

######### Get the estimated original data (middle part)###############

ey<-rep(0,length(Y))
ey[floor(0.1*n):ceiling(0.9*n)]<-zhat
for (i in 1:length(pos)) ey=ey+mag[i]*I(w>pos[i])
ey[1:floor(0.1*n)-1]=Y[1:floor(0.1*n)-1]
ey[ceiling(0.9*n)+1:n]=Y[ceiling(0.9*n)+1:n]
ey<-ey[1:n]

 #######bootstrap resample########
 
 	totds=0
	num=0
	Ys<-c()
 	for(i in 1:B)
		{
 		est.pos<-c()
       	sindex<-sample(1:length(epsil),length(epsil),replace=TRUE)
		noise<-rep(0,length(Y))
		noise[floor(0.1*n):ceiling(0.9*n)]<-epsil[sindex]
 		Ys<-ey+noise   
 	y<-m.det2(h,x,X,Ys)
 	est.pos<-det.jmp(x,abs(y),h,C,n)[[1]]
	totds<-totds+abs(HD(pos,est.pos))
 		}  
	}
        else totds=1
}
else 
{
 #######calculate pos and mag########

 	tot=0
 	pos<-c()
	y<-m.det3(h,x,X,Y)
	sigma<-sqrt(sig.est(x,X,Y,h))
 	C<-sqrt(sigma^2*22.85648/n/h)*qnorm(0.999999995)
 	o<-det.jmp(x,abs(y),h,C,n)
	pos<-o[[1]]
	mag<-o[[2]]

 ########Calculate continuous parts#########
if (length(pos)>0) {
z<-c()
z=Y
w<-c(1:n)/n	
if (length(pos>=1)) {for (i in 1:length(pos)) z<-z-mag[i]*I(w>pos[i])}

#########Estimate the continuous part and residual##########

zhat<-c()
zhat<-lest(h2,x,X,z)
epsil<-c()
epsil<-z[floor(0.1*n):ceiling(0.9*n)]-zhat

######### Get the estimated original data (middle part)###############

ey<-rep(0,length(Y))
ey[floor(0.1*n):ceiling(0.9*n)]<-zhat
for (i in 1:length(pos)) ey=ey+mag[i]*I(w>pos[i])
ey[1:floor(0.1*n)-1]=Y[1:floor(0.1*n)-1]
ey[ceiling(0.9*n)+1:n]=Y[ceiling(0.9*n)+1:n]
ey<-ey[1:n]

 #######bootstrap resample########
 
 	totds=0
	num=0
	Ys<-c()
 	for(i in 1:B)
		{
 		est.pos<-c()
       	sindex<-sample(1:length(epsil),length(epsil),replace=TRUE)
		noise<-rep(0,length(Y))
		noise[floor(0.1*n):ceiling(0.9*n)]<-epsil[sindex]
 		Ys<-ey+noise   
 	y<-m.det3(h,x,X,Ys)
 	est.pos<-det.jmp(x,abs(y),h,C,n)[[1]]
	totds<-totds+abs(HD(pos,est.pos))
 		}  
	}
        else totds=1
}
 	return(totds/B)
        }

##############################################################################
######  estimate func. by local linear kernel esitimator  ####################
##############################################################################
#kernel function: k(x)=(12/11)*(1-x^2)*I(-0.5<x<0.5)
k<-function(u){
y<-numeric(length(u))
y[abs(u)<0.5]<-(12/11)*(1-(u[abs(u)<0.5])^2)
y[abs(u)>=0.5]<-0
y
}

k.01<-function(h.lest,x,X){y<-sum(k((X-x)/h.lest));y;}
k.11<-function(h.lest,x,X){y<-sum(k((X-x)/h.lest)*(X-x));y;}
k.21<-function(h.lest,x,X){y<-sum(k((X-x)/h.lest)*((X-x)^2));y;}

lest<-function(h.lest,x,X,Y){  
m<-c()
for (i in 1:length(x))       m[i]<-sum(Y*k((X-x[i])/h.lest)*(k.21(h.lest,x[i],X)-k.11(h.lest,x[i],X)*(X-x[i])))/(k.01(h.lest,x[i],X)*k.21(h.lest,x[i],X)-(k.11(h.lest,x[i],X))^2)
m
}


