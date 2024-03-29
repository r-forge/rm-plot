\name{add.ps}
\alias{add.ps}
\title{
add p-values from t-tests
}
\description{
Adds p-values comparing the different cells at each x-axis position with a reference cell. Uses a syntax similar to \code{raw.means.plot2}.
}
\usage{
add.ps(data, col.id, col.offset, col.x, col.value, fun.aggregate = "mean", 
ref.offset = 1, prefixes, test = "t",
alternative = c("two.sided", "less", "greater"), distribution = approximate(100000),
mu = 0, paired = FALSE, var.equal = FALSE, 
lty = 0, ...)
}
\arguments{
  \item{data}{
A \code{data.frame}
}
  \item{col.id}{
\code{character} vector specifying the id column.
}
  \item{col.offset}{
\code{character} vector specifying the offset column.
}
  \item{col.x}{
\code{character} vector specifying the x-axis column.
}
  \item{col.value}{
\code{character} vector specifying the data column.
}
  \item{fun.aggregate}{
Function or function name used for aggregating the results. Default is \code{"mean"}.
}
  \item{ref.offset}{
Scalar \code{numeric} indicating the reference level to be tested against. The default is 1 corresponding to \code{levels(factor(d[,col.offset]))[1]}.
}
  \item{prefixes}{
\code{character} vector of the indices for the p-values. If missing corresponds to \code{levels(factor(d.new[,col.offset]))[-ref.offset]}. 
}
  \item{test}{
character of length 1, which test to use. currently implemented are \code{c("t", "stats.wilcoxon", "Wilcoxon", "permutation", "median")}. \code{c("Wilcoxon", "permutation", "median")} are called via \code{afex::compare.2.vectors}.
}
  \item{alternative}{
same as in \code{\link{t.test}}
}
  \item{distribution}{
\code{distribution} argument to \pkg{coin}, see \code{\link[coin]{Distribution}} or , \code{\link[coin]{IndependenceTest}}. Defaults to \code{approximate(100000)} indicating an approximation of the excat conditional distribution with 100.000 Monte Carlo samples. One can use \code{"exact"} for small samples and if \code{paired = FALSE}.
}
  \item{mu}{
same as in \code{\link{t.test}}
}
  \item{paired}{
same as in \code{\link{t.test}}
}
  \item{var.equal}{
same as in \code{\link{t.test}}
}
  \item{lty}{
line type of axis, Default is 0 (i.e., no line).
}
  \item{\dots}{
further arguments passed to axis.
}
}
\details{
This function computes t-tests comparing the values at each x-axis position for each condition against the reference condition at and adds the p-values to the axis.

This functions uses the same syntax as \code{\link{raw.means.plot2}} and should be used in addition to it. Note that values are ordered according to the \code{col.id} so \code{paired = TRUE} should be fine.
}
\value{
axis is plotted.
}
\author{
Henrik Singmann
}


\seealso{
\code{\link{raw.means.plot}} as the accompanying main functions.
}
\examples{
\dontrun{
#The examples uses the OBrienKaiser dataset from car and needs reshape.
# This extends the examples from raw.means.plot
require(reshape)
require(car)
data(OBrienKaiser)
OBKnew <- cbind(factor(1:nrow(OBrienKaiser)), OBrienKaiser)
colnames(OBKnew)[1] <- "id"
OBK.long <- melt(OBKnew)
OBK.long[, c("measurement", "time")] <- t(vapply(strsplit(as.character(OBK.long$variable), "\\\."),  "[", c("", "")))

# For this example the position at each x-axis are within-subject comparisons!
raw.means.plot2(OBK.long, "id", "measurement", "gender", "value")
add.ps(OBK.long, "id", "measurement", "gender", "value", paired = TRUE) #reference is "fup"

raw.means.plot2(OBK.long, "id", "measurement", "gender", "value")
add.ps(OBK.long, "id", "measurement", "gender", "value", ref.offset = 2, paired = TRUE) #reference is "post"

# Use R's standard (i.e., Welch test)
raw.means.plot2(OBK.long, "id", "treatment", "gender", "value")
add.ps(OBK.long, "id", "treatment", "gender", "value", prefixes = c("p(control vs. A)", "p(control vs. B)"))

# Use standard t-test
raw.means.plot2(OBK.long, "id", "treatment", "gender", "value")
add.ps(OBK.long, "id", "treatment", "gender", "value", var.equal = TRUE, prefixes = c("p(control vs. A)", "p(control vs. B)"))

}
}

