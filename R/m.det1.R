# M_DLK               
m.det1<-function(h.det1,x,X,Y){
stopifnot(is.numeric(h.det1))
stopifnot(is.numeric(x))
stopifnot(is.numeric(X))
stopifnot(is.numeric(Y))
stopifnot(h.det1>0)
stopifnot(length(X)==length(Y))
m<-c()
for (i in 1:length(x)) 
	{
	left.sided<-sum(Y*k1((X-x[i])/h.det1)*(k1.21(h.det1,x[i],X)-k1.11(h.det1,x[i],X)*(X-x[i])))/(k1.01(h.det1,x[i],X)*k1.21(h.det1,x[i],X)-(k1.11(h.det1,x[i],X))^2)
	right.sided<-sum(Y*k2((X-x[i])/h.det1)*(k2.21(h.det1,x[i],X)-k2.11(h.det1,x[i],X)*(X-x[i])))/(k2.01(h.det1,x[i],X)*k2.21(h.det1,x[i],X)-(k2.11(h.det1,x[i],X))^2)
	m[i]<-left.sided-right.sided
	}
m
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

k1.01<-function(h.det1,x,X){   #Check: ok
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

