\name{regTable.logistic}
\alias{regTable.logistic}
\title{regTable.logistic
}
\description{
Generates a logistic regression table.
}
\usage{
regTable.logistic(formula, y.name=NULL, y.labels=NULL, digit=3, title=NULL, caption=NULL, caption_heading=NULL, data)
}
\arguments{
  \item{formula}{
  The formula written in the form 'outcome ~ predictors'.
}
  \item{y.name}{
The name of the outcome.
}
  \item{y.labels}{
A vector of strings representing the labels of each 'y' category if the variable has multiple levels.
}
  \item{digit}{
The number of digits to round the p-value.
}
  \item{caption}{
The caption of the regression table.
}
  \item{caption_heading}{
The caption heading of the regression table.
}
  \item{title}{
The title of the regression table.
}
  \item{data}{
The data frame from which the 'formula' variables are derived from.
}
}
\details{
}
\value{
Returns a data frame.
}
\author{
Iris Kim
}
\examples{
data = data.frame(x = rep(c(0, 1), 50), y = c(500:599), z = c(300:349, 731:780))
regTable.logistic(data=data,formula=x~z, y.labels = "X vs Z")
}
