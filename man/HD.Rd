\name{HD}
\alias{HD}
\title{Hausdorff-distance}
\description{
  \code{HD} calculates the Hausdorff distance between two datasets. The elements of the two datasets should be real valued. And Hausdorff distance between two sets is defined to be \deqn{d_{\mathrm{H}}(X,Y) = \max\{\,\sup_{x \in X} \inf_{y \in Y} d(x,y),\, \sup_{y \in Y} \inf_{x \in X} d(x,y)\,\}}\\
That is, to calculate the Hausdorff distance of two sets X and Y, we first look at an element in X, and find out the minimum distance of it to an element in Y. Then we take the supremum of those distance which gives the first term in the definition. Then we do the same thing to Y and take the minimum of these two distances.
If at least one of the dataset is an empty set, this function defines the Hausdorff distance to be -1.
}

\usage{
HD(X,Y)
}

\arguments{
  \item{X}{a vector whose elements are real numbers}
  \item{Y}{a vector whose elements are real numbers}
}
\value{
The result returned is a positive real number indicating the Hausdorff distance between two non-empty sets, where the distance is defined above in the discription. If at least one of the datasets is empty, it will return -1. Since -1 does not make sense for distance, it acts kind of warning signal for empty set.
}

\examples{
X<-c(1,2,3)
Y<-c(2,3,4,9)
HD(X,Y)

# If one of the dataset is an empty set
X<-c()
Y<-c(1,2,3,4)
HD(X,Y)
}
\keyword{methods}
