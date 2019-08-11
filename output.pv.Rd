\name{output.pv}
\alias{output.pv}
\title{output.pv
}
\description{
Returns the p-value rounded to a specific number of digits. It the p-value is less than 0.001, it is replaced with the text '< 0.001'.
}
\usage{
output.pv(pp, digit)
}
\arguments{
  \item{pp}{
The input p-value.
}
  \item{digit}{
The number of digits to round the p-value.
}
}
\value{
Returns either a numerical value or string.
}
\author{
Iris Kim
}

