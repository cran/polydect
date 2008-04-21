\name{sig.est}
\alias{sig.est}

\title{sig.est}
\description{
  \code{sig.est} estimates the noise level of the data using one-sided local linear kernel estimator. The kernel function used here is the Epanechnikov kernel function. 
}

\usage{
sig.est(x,X,Y,h1)
}

\arguments{
  \item{x}{a vector of real numbers, which indicates the locations where we have the estimated difference between left and right limits.}
  \item{X}{a vector of real numbers which indicates the location of the design points, the points where we have observations.}
  \item{Y}{a vector of real numbers having the same length with X. It is the observed data on the design points.}
  \item{h1}{a positive real number, which indicates the bandwidth used for estimation.}

}
\details{
To estimate the variance, we use two one-sided local linear kernel estimator to estimate the left and the right limit of a certain point, then we will get the residuals. Since at those continuous points, both residuals will be good estimator for the noise level, while at the jump locations, only one of the two residual, the one has smaller absolute value, will be a good estimator for the residual. Therefore, we just pick up the one with smaller absolute value for calulating residual sum of squares at that point. We will average all the squares of the residuals to get an estimator for the variance.
}
\value{
The returned value is a positive real number which is the estimated variance of the noise. 
}

\examples{
data(DJIA)
attach(DJIA)
n=length(Open)
## X is the design points ##
X<-c(1:n)/n

## Y is the observed data ##
Y<-Open

## x is the set of positions that we are interested in estimation ##
x<-X[floor(0.1*n):ceiling(0.9*n)]

## specify bandwidth ##
h1=0.16

## estimation ##
variance<-sig.est(x,X,Y,h1)
variance
}
\keyword{methods}
