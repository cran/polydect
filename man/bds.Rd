\name{bds}
\alias{bds}

\title{bds}
\description{
  \code{bds} helps with the bandwidth selection in detecting jump positions using bootstrap with Hausdorff distance as a criteria, as long as the observations are given. It can be used for selecting bandwidth for local polynomial jump dectors with order 0, 1, 2 and 3.
}

\usage{
 bds(h,B,h2,X,Y,x,order)
}

\arguments{
  \item{h}{a positive real number indicating the bandwidth value that we used to do the detection of jump locations.}
  \item{B}{a positive integer which indicates the times of Boostrap.}
  \item{h2}{a positive real number indicating the bandwidth value that we used in estimating the function with jumps removed.}
  \item{X}{a vector of real numbers which indicates the location of the design points, the points where we have observations.}
  \item{Y}{a vector of real numbers having the same length with X. It is the observed data on the design points.}
  \item{x}{a vector of real numbers, which indicates the locations where we have the estimated difference between left and right limits.}
  \item{order}{a positive integer specifies the order of local polynomial estimator used. Here the order can be taken as 0, 1, 2 or 3.}
}
\details{
The bootstrap procedure works like this: first we estimate the jump positions and magnitudes from the original data. Once we get this estimator, we substract the jump part from the data and estimate the continuous part with a local linear estimator. And also we will get the residuals. We bootstrap the residuals, add back the jump part and redo the jump detection. Then we calculate the Hausdorff distance between this set of jump positions and the original detected ones. We repeat this procedure by B times and get the average Hausdorff distance for the particular bandwidth. And then we may continue in selecting the bandwidth which corresponding to the smallest Hausdorff distance. For more details, please refer to the reference. Here this function only returns the average Hausdorff distance, instead of directly choosing a bandwidth. This is because that sometimes, we may also look at other information besides the Hausdorff distance to decide the bandwidth. Such like the plot of the data itself and so on.
}
\value{
The value returned is a positive real number indicating the average of the Hausdorff distance between the detected and true set of jump positions, while the detection is repeated by B times.
}

\references{
Gijbels, I. and Goderniaux, A.-C.(2004), "Bandwidth selection for change point estimation is nonparametric regression", Technometrics, 46, 76-86.
}

\examples{
library(polydect)
data(DJIA)
attach(DJIA)
n=length(Open)

## X is the design points ##
X<-c(1:n)/n

## Y is the observed data ##
Y<-Open

## look at the data ##
ts.plot(Open)

## x is the set of positions that we are interested in estimation ##
x<-X[floor(0.1*n):ceiling(0.9*n)]

## specify bandwidth ##
h=0.10
h2=0.13

B=100

## Since the curvature is kind of large, we use quadratic detector
order=2
bds(h,B,h2,X,Y,x,order)
}
\keyword{methods}
