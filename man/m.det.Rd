\name{m.det}
\alias{m.det}
\alias{m.det0}
\alias{m.det1}
\alias{m.det2}
\alias{m.det3}

\title{m.det}
\description{
  \code{m.det0, m.det1, m.det2, m.det3} gives the estimation of the difference between the right limit and the left limit of a curve using one-sided local polynomial kernel estimators with order 0, 1, 2 and 3 respectively.
}

\usage{
m.det0(h.det0,x,X,Y)
m.det1(h.det1,x,X,Y)
m.det2(h.det2,x,X,Y)
m.det3(h,x,X,Y)
}

\arguments{
  \item{h.det0,h.det1,h.det2,h}{a positive real number, which indicates the bandwidth for estimation.}
  \item{x}{a vector of real numbers, which indicates the places that we want to do the estimation. It may be the same as design points, or it can be a subset of the design points, or other sets.}
  \item{X}{a vector of real numbers which indicates the location of the design points, the points where we have observations.}
  \item{Y}{a vector of real numbers having the same length with X. It is the observed data on the design points.}

}
\details{The estimation is done by estimating the left limit and the right limit seperately using one-sided polynomial kernel estimators. The kernel function used is the Epanechnikov kernel function. For reference, see Qiu, P. (2005) Image Processing and Jump Regression Analysis, New York: John Wiley & Sons. 

Higher order estimators will bring less bias, however, they have greater variance. And also, higher order estimators will require more computer time. Generally speaking, if the function is flat, local kernel or linear estimator is recommended. But if the function has large curvature, then higher order detector as quadratic or cubic estimator is recommended.
}
\value{
The returned value is a vector with the length as x, representing the estimated value(s) of the difference between the right limit and the left limit at each point of x, using one-sided polynomial estimators of different orders.
}

\examples{
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
h0=0.04
h1=0.16
h2=0.15
h3=0.16

## estimation ##
y0<-numeric(length(x))
y1<-numeric(length(x))
y2<-numeric(length(x))
y3<-numeric(length(x))

y0<-m.det0(h0,x,X,Y) ## one sided local kernel estimator ##
y1<-m.det1(h1,x,X,Y) ## one sided local linear kernel estimator ##
y2<-m.det2(h2,x,X,Y) ## one sided local quadratic kernel estimator ##
y3<-m.det3(h3,x,X,Y) ## one sided local cubic kernel estimator ##

## visualize the result ##
plot(x,y0)
plot(x,y1)
plot(x,y2)
plot(x,y3)
}
\keyword{methods}
