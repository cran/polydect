#M_DKE                   
m.det0<-function(h.det0,x,X,Y){
stopifnot(is.numeric(h.det0))
stopifnot(is.numeric(x))
stopifnot(is.numeric(X))
stopifnot(is.numeric(Y))
stopifnot(h.det0>0)
stopifnot(length(X)==length(Y))
n=length(X)
m<-c()
for (i in 1:length(x))  m[i]<-(sum(Y*k1((X-x[i])/h.det0))-sum(Y*k2((X-x[i])/h.det0)))*(1/(h.det0*n))
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



