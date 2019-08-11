\name{crosstable}
\alias{crosstable}
\title{crosstable
}
\description{
Generates a crosstable between two variables with row margins.
}
\usage{
crosstable(y, bygroup, bygroup.name=NULL, bygroup.labels=NULL, y.name=NULL, y.labels=NULL)
}
\arguments{
  \item{y}{
The first variable.
}
  \item{y.name}{
The label of the first variable in the crosstable.
}
  \item{y.labels}{
A vector of strings representing the labels of each level of the variable 'y' if it has multiple levels.
}
  \item{bygroup}{
The second variable.
}
  \item{bygroup.name}{
The label of the second variable in the crosstable.
}
  \item{bygrouplabels}{
A vector of strings representing the labels of each level of the variable 'bygroup' if it has multiple levels.
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
crosstable(y=data$a, bygroup=data$b, bygroup.name="b", y.name="a", bygroup.labels=c("one", "two", "three"), y.labels=c("ONE", "TWO"))
}
