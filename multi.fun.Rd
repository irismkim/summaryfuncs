\name{multi.fun}
\alias{multi.fun}
\title{multi.fun
Generates a frequency table of data.
}
\description{
Generates a frequency table of the data.
}
\usage{
multi.fun(x, digit = 3)
}
\arguments{
  \item{x}{
  The data.
}
  \item{digit}{
  The number of decimal places the user wants to round the values to.
}
}
\value{
Returns a data frame.
}
\author{
Iris Kim
}
\examples{
data = data.frame(x=c(4, 5, 1, 7, 10), y=c(10, 2, 13, 21, 12), z=c(6, 44, 32, 81, 17), a = c(1, 0, 1, 1, 0), b = c(2, 3, 1, 2, 1))
multi.fun(data$a, 3)
}
