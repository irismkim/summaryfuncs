\name{insertRow2}
\alias{insertRow2}
\title{insertRow2
}
\description{
Adds a row to a dataframe at a specific row index.
}
\usage{
insertRow2(existingDF, newrow, r)
}
\arguments{
  \item{existingDF}{
The data frame.
}
  \item{newrow}{
The new row being added to the data frame.
}
  \item{r}{
The row index which the new row is to be inserted.
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
insertRow2(data, newrow = c(100, 101, 105, 104, 110), 1)
}
