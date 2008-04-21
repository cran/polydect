HD<-function(X,Y){
hd=0
n1<-length(X)
n2<-length(Y)
d1<-c()
d2<-c()
if(n1*n2==0) hd<- -1
else{
stopifnot(is.numeric(X))
stopifnot(is.numeric(Y))
for (i in 1:n1)	d1[i]<-min(abs(X[i]-Y))	
for (i in 1:n2)     d2[i]<-min(abs(Y[i]-X))
hd=max(max(d1),max(d2))
}
hd
}
