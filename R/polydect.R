polydect<-function(h,X,Y,x,order)
{
	stopifnot(is.numeric(h))
	stopifnot(is.numeric(X))
	stopifnot(is.numeric(Y))
	stopifnot(is.numeric(x))
	stopifnot(is.numeric(order))
	stopifnot(order-floor(order)==0)
        stopifnot(order>=0)
	stopifnot(order<=3)
	stopifnot(h>0)
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
}
 else if (order==1) {
 #######calculate pos and mag########

 	tot=0
 	pos<-c()
	y<-m.det1(h,x,X,Y)
	sigma<-sqrt(sig.est(x,X,Y,h))
 	C<-sqrt(sigma^2*4.49/n/h)*qnorm(0.999995)
 	o<-det.jmp(x,abs(y),h,C,n)
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
}
 	o[[2]]
        }

