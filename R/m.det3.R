m.det3<-function(h,x,X,Y){
stopifnot(is.numeric(h))
stopifnot(is.numeric(x))
stopifnot(is.numeric(X))
stopifnot(is.numeric(Y))
stopifnot(h>0)
stopifnot(length(X)==length(Y))
m<-c()
for (i in 1:length(x)){
W3<-matrix(c(k3.011(x[i],X,h),k3.111(x[i],X,h),k3.211(x[i],X,h),k3.311(x[i],X,h),k3.111(x[i],X,h),k3.211(x[i],X,h),k3.311(x[i],X,h),k3.411(x[i],X,h),k3.211(x[i],X,h),k3.311(x[i],X,h),k3.411(x[i],X,h),k3.511(x[i],X,h),k3.311(x[i],X,h),k3.411(x[i],X,h),k3.511(x[i],X,h),k3.611(x[i],X,h)),
 nrow=4)

v3<-matrix(c(k4.011(x[i],X,h),k4.111(x[i],X,h),k4.211(x[i],X,h),k4.311(x[i],X,h),k4.111(x[i],X,h),k4.211(x[i],X,h),k4.311(x[i],X,h),k4.411(x[i],X,h),k4.211(x[i],X,h),k4.311(x[i],X,h),k4.411(x[i],X,h),k4.511(x[i],X,h),k4.311(x[i],X,h),k4.411(x[i],X,h),k4.511(x[i],X,h),k4.611(x[i],X,h)),
 nrow=4)


left.sided<-sum(Y*k1((X-x[i])/h)*(w30(x[i],X,h)+w31(x[i],X,h)*(X-x[i])+w32(x[i],X,h)*(X-x[i])^2+w33(x[i],X,h)*(X-x[i])^3))/det(W3)

right.sided<-sum(Y*k2((X-x[i])/h)*(v30(x[i],X,h)+v31(x[i],X,h)*(X-x[i])+v32(x[i],X,h)*(X-x[i])^2+v33(x[i],X,h)*(X-x[i])^3))/det(v3)
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


k3.011<-function(x,X,h){
y<-sum(k1((X-x)/h))
y
}


k3.111<-function(x,X,h){
y<-sum(k1((X-x)/h)*(X-x))
y
}

k3.211<-function(x,X,h){
y<-sum(k1((X-x)/h)*((X-x)^2))
y
}

k3.311<-function(x,X,h){
y<-sum(k1((X-x)/h)*((X-x)^3))
y
}


k3.411<-function(x,X,h){
y<-sum(k1((X-x)/h)*((X-x)^4))
y
}

k3.511<-function(x,X,h){
y<-sum(k1((X-x)/h)*((X-x)^5))
y
}

k3.611<-function(x,X,h){
y<-sum(k1((X-x)/h)*((X-x)^6))
y
}



k4.011<-function(x,X,h){
y<-sum(k2((X-x)/h))
y
}


k4.111<-function(x,X,h){
y<-sum(k2((X-x)/h)*(X-x))
y
}

k4.211<-function(x,X,h){
y<-sum(k2((X-x)/h)*((X-x)^2))
y
}

k4.311<-function(x,X,h){
y<-sum(k2((X-x)/h)*((X-x)^3))
y
}

k4.411<-function(x,X,h){
y<-sum(k2((X-x)/h)*((X-x)^4))
y
}

k4.511<-function(x,X,h){
y<-sum(k2((X-x)/h)*((X-x)^5))
y
}

k4.611<-function(x,X,h){
y<-sum(k2((X-x)/h)*((X-x)^6))
y
}

w30<-function(x,X,h){
A<-matrix(c(k3.211(x,X,h),k3.311(x,X,h),k3.411(x,X,h),k3.311(x,X,h),k3.411(x,X,h),k3.511(x,X,h),k3.411(x,X,h),k3.511(x,X,h),k3.611(x,X,h)), nrow=3)
w<-det(A)
w
}

w31<-function(x,X,h){
A<-matrix(c(k3.111(x,X,h),k3.211(x,X,h),k3.311(x,X,h),k3.311(x,X,h),k3.411(x,X,h),k3.511(x,X,h),k3.411(x,X,h),k3.511(x,X,h),k3.611(x,X,h)), nrow=3)
w<-det(A)*(-1)
w
}

w32<-function(x,X,h){
A<-matrix(c(k3.111(x,X,h),k3.211(x,X,h),k3.311(x,X,h),k3.211(x,X,h),k3.311(x,X,h),k3.411(x,X,h),k3.411(x,X,h),k3.511(x,X,h),k3.611(x,X,h)), nrow=3)
w<-det(A)
w
}

w33<-function(x,X,h){
A<-matrix(c(k3.111(x,X,h),k3.211(x,X,h),k3.311(x,X,h),k3.211(x,X,h),k3.311(x,X,h),k3.411(x,X,h),k3.311(x,X,h),k3.411(x,X,h),k3.511(x,X,h)), nrow=3)
w<-det(A)*(-1)
w
}

v30<-function(x,X,h){
A<-matrix(c(k4.211(x,X,h),k4.311(x,X,h),k4.411(x,X,h),k4.311(x,X,h),k4.411(x,X,h),k4.511(x,X,h),k4.411(x,X,h),k4.511(x,X,h),k4.611(x,X,h)), nrow=3)
w<-det(A)
w
}

v31<-function(x,X,h){
A<-matrix(c(k4.111(x,X,h),k4.211(x,X,h),k4.311(x,X,h),k4.311(x,X,h),k4.411(x,X,h),k4.511(x,X,h),k4.411(x,X,h),k4.511(x,X,h),k4.611(x,X,h)), nrow=3)
w<-det(A)*(-1)
w
}

v32<-function(x,X,h){
A<-matrix(c(k4.111(x,X,h),k4.211(x,X,h),k4.311(x,X,h),k4.211(x,X,h),k4.311(x,X,h),k4.411(x,X,h),k4.411(x,X,h),k4.511(x,X,h),k4.611(x,X,h)), nrow=3)
w<-det(A)
w
}

v33<-function(x,X,h){
A<-matrix(c(k4.111(x,X,h),k4.211(x,X,h),k4.311(x,X,h),k4.211(x,X,h),k4.311(x,X,h),k4.411(x,X,h),k4.311(x,X,h),k4.411(x,X,h),k4.511(x,X,h)), nrow=3)
w<-det(A)*(-1)
w
}


