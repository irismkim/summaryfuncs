\name{summaryTable}
\alias{summaryTable}
\title{summaryTable
}
\description{
This function generates a type of summary table for the given data y. It can generate a summary table (sample size n, mean, standard deviation, min, median, max), a frequency table, or a crosstable. It also has a boxplot option, which generates a visual boxplot of the data as well.
}
\usage{
summaryTable(y, bygroup, digit = 3, table.title = NULL, caption_heading = NULL, caption = NULL, freq.tab = F, y.name = NULL, y.labels = NULL, bygroup.name = NULL, bygroup.labels = NULL, boxplot = F, boxplot.labelsize = 1, plot.title=NULL)
}
\arguments{
    \item{data}{
  The dataset.
}
  \item{y}{
  The data which is to be summarized.
}
  \item{bygroup}{
  An optional argument if the user desires to describe y by bygroup.
}
  \item{digit}{
  The number of decimal places the user wants to round the values to.
}
  \item{table.title}{
  The title of the table.
}
  \item{caption_heading}{
  The caption heading.
}
  \item{caption}{
  The caption of the table.
}
  \item{freq.tab}{
  A logical argument that determines whether the type of summary table generated will be a frequency table.
}
  \item{y.name}{
  The label of the 'y' data in the table.
}
  \item{y.labels}{
  The vector of string names for each of the rows of the 'y' data if it has multiple levels.
}
  \item{bygroup.name}{
  The label of the 'bygroup' data in the table.
}
  \item{bygroup.labels}{
  The vector of string names for each of the rows of the 'bygroup' data if it has multiple levels.
}
  \item{boxplot}{
  A logical argument that determines whether a boxplot is generated of the data.
}
  \item{boxplot.labelsize}{
  The size of the labels on the boxplot.
}
  \item{plot.title}{
  The title of the boxplot.
}
\value{
A data frame of the table is returned. It is wrapped in kable_styling() in statistical report formatting. IF the boxplot option is set to true, a boxplot is also returned. The data frame and boxplot are returned in a list.
}
\author{
Iris Kim
}
\examples{
data = data.frame(x=c(4, 5, 1, 7, 10), y=c(10, 2, 13, 21, 12), z=c(6, 44, 32, 81, 17), a = c(1, 0, 1, 1, 0), b = c(2, 3, 1, 2, 1))

# Summary Table
summaryTable(y=data$x, digit=3, table.title="Summary", y.name="X", y.labels=data$x, bygroup.name="Y", bygroup.labels=data$y)

# Frequency Table
summaryTable(y=data$x, digit=3, y.name="X", y.labels=data$x, caption_heading="Note:", caption="Caption here.", freq.tab=T)

# Crosstable
summaryTable(y=data$x, bygroup=data$y, digit=3, y.name="X", y.labels=data$x, bygroup.name="Y", bygroup.labels=data$y, caption_heading="Note:", caption="Caption here.", freq.tab=T)

# Boxplot
summaryTable(y=data$z, bygroup=data$y, digit=3, y.name="Z", y.labels=data$y, bygroup.name="Y", caption_heading="Note:", caption="Caption here.", boxplot=T, boxplot.labelsize = 1, plot.title="Plot Title")[[1]]
}
