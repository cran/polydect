det.jmp<-function(x,y,h,C,n)
	{
	stopifnot(is.numeric(C))
	stopifnot(is.numeric(x))
	stopifnot(is.numeric(y))
	stopifnot(is.numeric(h))
	stopifnot(is.numeric(n))
	stopifnot(n-floor(n)==0)
	stopifnot(h>0)
	stopifnot(C>0)
	stopifnot(n>0)
	stopifnot(all(y>=0))
	stopifnot(length(x)==length(y))
	tot=0
	pos<-c()
	p=1
	q=1
	s<-floor(n*h/2)
	for (i in 1:length(x)) 
		{
		p=1
		if (y[i]>C)
			{ 
			for (j in max(i-s,1):max(i-1,1)) p=p*(y[j]<=C)
			if (p==1)
			{
			for (j in (i+1):(length(x)-s)) 
				{
				q=1
				if (y[min(j,length(x))]<=C) {for (k in min(j,length(x)):min(j+s-1,length(x))) q=q*(y[k]<=C); if (q==1) break}
				}
			tot<-tot+1;
			if (j==(i+1)) pos[tot]=(i-1)*1/n+.1 else pos[tot]=((i+j-1)/2-1)*1/n+.1;
			} 
			}
		}
	return(list(tot,pos))
	}