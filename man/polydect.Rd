\name{polydect}
\alias{polydect}

\title{polydect}
\description{
  \code{polydect} suggests the number of possible jumps and the jump locations using the bandwidth and the polynomial detectors specified by the users. The users are allowed to choose local polynomial kernel detectors from order 0 to 3.
}

\usage{
 polydect(h,X,Y,x,order)
}

\arguments{
  \item{h}{a positive real number indicating the bandwidth value that we use to do the detection of jump locations.}
  \item{X}{a vector of real numbers which indicates the location of the design points, the points where we have observations.}
  \item{Y}{a vector of real numbers having the same length with X. It is the observed data on the design points.}
  \item{x}{a vector of real numbers, which indicates the locations where we have the estimated difference between left and right limits.}
  \item{order}{a positive integer specifies the order of local polynomial estimator used. Here the order can be taken as 0, 1, 2 or 3.}
}
\details{
As long as the bandwidth and the order of the polynomial jump detector is given, we first estimate the noise level using the sig.est function in the package. Then we use the jump detector to find the locations where the difference between the left and right limit are above the threshold value. Finally we use the function jmp.det to eliminate false detection and use one point to represent a tie.
}
\value{
The value returned is a vector of positive real numbers that gives the estimated positions of the jump points.
}

\examples{
library(polydect)
set.seed(8054)
n=100

## X is the design points ##
X<-c(1:n)/n

## Y is the observed data ##
sigma=0.1 #specify the noise level
NS<-rnorm(n,0,sigma)   #generate the noise
Y<-c(2/3-2*X[1:floor(n/3)],1+0*X[(floor(n/3)+1):floor(2*n/3)],
-2*(X[(floor(2*n/3)+1):(n)]-2/3)*(X[(floor(2*n/3)+1):n]-2))+NS

## look at the data ##
plot(X,Y)

## x is the set of positions that we are interested in estimation ##
x<-X[floor(0.1*n):ceiling(0.9*n)]

## specify bandwidth ##
h0=0.04

## Since the data does not look to have large slope or curvature, we ##
## use kernel detector, which has order 0.                           ##

polydect(h0,X,Y,x,0)
}
\keyword{methods}
 
