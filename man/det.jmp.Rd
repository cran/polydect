\name{det.jmp}
\alias{det.jmp}

\title{det.jmp}
\description{
  \code{det.jmp} suggests the jump candidates from the threshold value specified by users and the difference between left and right limit estimated by the one-sided polynomial kernel estimators. 
}

\usage{
det.jmp(x,y,h,C,n)
}

\arguments{
  \item{x}{a vector of real numbers, which indicates the locations where we have the estimated difference between left and right limits.}
  \item{y}{a vector of positive real numbers which has the same length with x. It gives the absolute value of the estimated difference at the places indicated by x.}
  \item{h}{a positive real number indicating the threshold value that we used to do the estimation.}
  \item{C}{a positive real number which acts as the threshold in selecting candidates of a jump points.}
  \item{n}{a positive integer indicating the number of design points.}

}
\details{
For a jump point on a curve, the difference between the left and right limits is expected to be big. By using a threshold value, we may find the possible jump candidates. However, the points near the true jump will have a high probablity to have a bigger difference, therefore, we need some modification procedure to make false detection as low as possible, such as combining several connected jump points into one. For details, please refer to the reference section.
}
\value{
  \item{tot}{a positive integer giving the total number of jump points.}

  \item{pos}{a vector of positive real numbers that gives the estimated positions of the jump points.}
}

\references{
Qiu, P(1994) "Estimation of the number of jumps of the jump regression fuctions", Communications in Statistics-Theory and Methods 23
}

\examples{
library(polydect)
n=100
sigma=0.1
## X is the design points ##
X<-c(1:n)/n

## Y is the observed data ##
NS<-rnorm(n,0,sigma)
Y<-c(10-30*X[1:floor(n/3)],-360*(X[(floor(n/3)+1):floor(2*n/3)]-0.5)^2+11,
exp(7.5*(X[(floor(2*n/3)+1):(n)]-2/3))-1)+NS

## x is the set of positions that we are interested in estimation ##
x<-c(floor(0.1*n):ceiling(0.9*n))/n

## specify bandwidth ##
h1=0.16

## estimation ##
y1<-numeric(length(x))
y1<-m.det1(h1,x,X,Y) ## one sided local kernel estimator ##
C=sqrt(sigma^2*4.49/n/h1)*qnorm(0.999995)
plot(x,y1)
tot<-det.jmp(x,abs(y1),h1,C,n)[[1]]
pos<-det.jmp(x,abs(y1),h1,C,n)[[2]]
}
\keyword{methods}
