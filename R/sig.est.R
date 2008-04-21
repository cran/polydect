sig.est<-function(x,X,Y,h1) 
	{
	stopifnot(is.numeric(h1))
	stopifnot(is.numeric(x))
	stopifnot(is.numeric(X))
	stopifnot(is.numeric(Y))
	stopifnot(h1>0)
	stopifnot(length(X)==length(Y))
	n=length(X)
	MSE.l<-rep(0,length(x))
	MSE.r<-rep(0,length(x))
	MSE<-c()
	Y1<-Y[floor(0.1*n):ceiling(0.9*n)]-m.det11(h1,x,X,Y)[[1]]
	Y2<-Y[floor(0.1*n):ceiling(0.9*n)]-m.det11(h1,x,X,Y)[[2]]
	for (i in 1:length(x))
		{
		MSE.l[i]<-MSE.l[i]+sum((Y1)^2*k1((X[floor(0.1*n):ceiling(0.9*n)]-x[i])/h1))/sum(k1((X[floor(0.1*n):ceiling(0.9*n)]-x[i])/h1))
		MSE.r[i]<-MSE.r[i]+sum((Y2)^2*k2((X[floor(0.1*n):ceiling(0.9*n)]-x[i])/h1))/sum(k2((X[floor(0.1*n):ceiling(0.9*n)]-x[i])/h1))
		MSE[i]=min(MSE.l[i],MSE.r[i])
		}
	if(length(x)>=1) MSE=sum(MSE[2:(length(MSE)-1)])/(length(MSE)-2)
	MSE
	}

m.det11<-function(h.det1,x,X,Y){
stopifnot(is.numeric(h.det1))
stopifnot(is.numeric(x))
stopifnot(is.numeric(X))
stopifnot(is.numeric(Y))
stopifnot(h.det1>0)
stopifnot(length(X)==length(Y))
m<-c()
left.sided<-c()
right.sided<-c()
for (i in 1:length(x)) 
	{
	left.sided[i]<-sum(Y*k1((X-x[i])/h.det1)*(k1.21(h.det1,x[i],X)-k1.11(h.det1,x[i],X)*(X-x[i])))/(k1.01(h.det1,x[i],X)*k1.21(h.det1,x[i],X)-(k1.11(h.det1,x[i],X))^2)
	right.sided[i]<-sum(Y*k2((X-x[i])/h.det1)*(k2.21(h.det1,x[i],X)-k2.11(h.det1,x[i],X)*(X-x[i])))/(k2.01(h.det1,x[i],X)*k2.21(h.det1,x[i],X)-(k2.11(h.det1,x[i],X))^2)
	m[i]<-left.sided[i]-right.sided[i]
	}
return(list(left.sided,right.sided,m))
}

# k_1(x)=1.5*(1-x^2)*I(0,1)       
k1<-function(u){
y<-numeric(length(u))
y[(abs(u-0.5))<0.5]<-1.5*(1-(u[(abs(u-0.5))<0.5])^2)
y[(abs(u-0.5))>=0.5]<-0
y
}

# k_2(x)=K_1(-x)          
k2<-function(u){
y<-numeric(length(u))
y[(abs(u+0.5))<0.5]<-1.5*(1-(u[(abs(u+0.5))<0.5])^2)
y[(abs(u+0.5))>=0.5]<-0
y
}

k1.01<-function(h.det1,x,X){   
y<-sum(k1((X-x)/h.det1))
y
}


k1.11<-function(h.det1,x,X){
y<-sum(k1((X-x)/h.det1)*(X-x))
y
}

k1.21<-function(h.det1,x,X){
y<-sum(k1((X-x)/h.det1)*((X-x)^2))
y
}

k2.01<-function(h.det1,x,X){
y<-sum(k2((X-x)/h.det1))
y
}


k2.11<-function(h.det1,x,X){
y<-sum(k2((X-x)/h.det1)*(X-x))
y
}

k2.21<-function(h.det1,x,X){
y<-sum(k2((X-x)/h.det1)*((X-x)^2))
y
}

