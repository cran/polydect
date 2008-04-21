# M_DQK
m.det2<-function(h.det2,x,X,Y){
stopifnot(is.numeric(h.det2))
stopifnot(is.numeric(x))
stopifnot(is.numeric(X))
stopifnot(is.numeric(Y))
stopifnot(h.det2>0)
stopifnot(length(X)==length(Y))
m<-c()
for (i in 1:length(x)){
W<-matrix(c(k1.011(h.det2,x[i],X),k1.111(h.det2,x[i],X),k1.211(h.det2,x[i],X),
            k1.111(h.det2,x[i],X),k1.211(h.det2,x[i],X),k1.311(h.det2,x[i],X),
            k1.211(h.det2,x[i],X),k1.311(h.det2,x[i],X),k1.411(h.det2,x[i],X)), nrow=3)

V<-matrix(c(k2.011(h.det2,x[i],X),k2.111(h.det2,x[i],X),k2.211(h.det2,x[i],X),
            k2.111(h.det2,x[i],X),k2.211(h.det2,x[i],X),k2.311(h.det2,x[i],X), 
            k2.211(h.det2,x[i],X),k2.311(h.det2,x[i],X),k2.411(h.det2,x[i],X)), nrow=3)

left.sided<-sum(Y*k1((X-x[i])/h.det2)*(w0(h.det2,x[i],X)+w1(h.det2,x[i],X)*(X-x[i])+w2(h.det2,x[i],X)*(X-x[i])^2))/det(W)

right.sided<-sum(Y*k2((X-x[i])/h.det2)*(v0(h.det2,x[i],X)+v1(h.det2,x[i],X)*(X-x[i])+v2(h.det2,x[i],X)*(X-x[i])^2))/det(V)

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


k1.011<-function(h.det2,x,X){
y<-sum(k1((X-x)/h.det2))
y
}


k1.111<-function(h.det2,x,X){
y<-sum(k1((X-x)/h.det2)*(X-x))
y
}

k1.211<-function(h.det2,x,X){
y<-sum(k1((X-x)/h.det2)*((X-x)^2))
y
}

k1.311<-function(h.det2,x,X){
y<-sum(k1((X-x)/h.det2)*((X-x)^3))
y
}


k1.411<-function(h.det2,x,X){
y<-sum(k1((X-x)/h.det2)*((X-x)^4))
y
}


k2.011<-function(h.det2,x,X){
y<-sum(k2((X-x)/h.det2))
y
}


k2.111<-function(h.det2,x,X){
y<-sum(k2((X-x)/h.det2)*(X-x))
y
}

k2.211<-function(h.det2,x,X){
y<-sum(k2((X-x)/h.det2)*((X-x)^2))
y
}

k2.311<-function(h.det2,x,X){
y<-sum(k2((X-x)/h.det2)*((X-x)^3))
y
}

k2.411<-function(h.det2,x,X){
y<-sum(k2((X-x)/h.det2)*((X-x)^4))
y
}

#k2.511<-function(h.det2,x,X){
#y<-sum(k2((X-x)/h.det2)*((X-x)^5))
#y
#}

#k2.611<-function(h.det2,x,X){
#y<-sum(k2((X-x)/h.det2)*((X-x)^6))
#y
#}

w0<-function(h.det2,x,X){
A<-matrix(c(k1.211(h.det2,x,X),k1.311(h.det2,x,X),k1.311(h.det2,x,X),k1.411(h.det2,x,X)), nrow=2)
w<-det(A)
w
}

w1<-function(h.det2,x,X){
A<-matrix(c(k1.111(h.det2,x,X),k1.311(h.det2,x,X),k1.211(h.det2,x,X),k1.411(h.det2,x,X)), nrow=2)
w<-det(A)*(-1)
w
}

w2<-function(h.det2,x,X){
A<-matrix(c(k1.111(h.det2,x,X),k1.211(h.det2,x,X),k1.211(h.det2,x,X),k1.311(h.det2,x,X)), nrow=2)
w<-det(A)
w
}

v0<-function(h.det2,x,X){
A<-matrix(c(k2.211(h.det2,x,X),k2.311(h.det2,x,X),k2.311(h.det2,x,X),k2.411(h.det2,x,X)), nrow=2)
v<-det(A)
v
}

v1<-function(h.det2,x,X){
A<-matrix(c(k2.111(h.det2,x,X),k2.311(h.det2,x,X),k2.211(h.det2,x,X),k2.411(h.det2,x,X)), nrow=2)
v<-det(A)*(-1)
v
}

v2<-function(h.det2,x,X){
A<-matrix(c(k2.111(h.det2,x,X),k2.211(h.det2,x,X),k2.211(h.det2,x,X),k2.311(h.det2,x,X)), nrow=2)
v<-det(A)
v
}
