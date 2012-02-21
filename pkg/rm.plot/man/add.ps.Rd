\name{add.ps}
\alias{add.ps}
\title{
add p-values from t-tests
}
\description{
Adds p-values comparing the different cells at each x-axis position with a reference cell. Uses a syntex similar to \code{rm.plot2}.
}
\usage{
add.ps(d, col.id, col.offset, col.x, col.d, fun.aggregate = "mean", 
ref.offset = 1, prefixes, 
alternative = c("two.sided", "less", "greater"), mu = 0, paired = FALSE, var.equal = FALSE, 
lty = 0, ...)
}
\arguments{
  \item{d}{
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
  \item{col.d}{
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
  \item{alternative}{
same as in \code{\link{t.test}}
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

This functions uses the same syntax as \code{\link{rm.plot2}} and should be used in addition to it. Note that values are ordered according to the \code{id.col} so \code{paired = TRUE} should be fine.
}
\value{
axis is plotted.
}
\author{
Henrik Singmann
}


\seealso{
\code{\link{rm.plot2}} as the accompanying function or \code{\link{rm.plot}} for the main function.
}
\examples{
\dontrun{
#The examples uses the OBrienKaiser dataset from car and needs reshape.
# This extends the example sfrom rm.plot2
require(reshape)
require(car)
data(OBrienKaiser)
OBKnew <- cbind(factor(1:nrow(OBrienKaiser)), OBrienKaiser)
colnames(OBKnew)[1] <- "id"
OBK.long <- melt(OBKnew)
OBK.long[, c("measurement", "time")] <- t(vapply(strsplit(as.character(OBK.long$variable), "\\\."),  "[", c("", "")))

# For this example the position at each x-axis are within-subject comparisons!
rm.plot2(OBK.long, "id", "measurement", "gender", "value")
add.ps(OBK.long, "id", "measurement", "gender", "value", paired = TRUE) #reference is "fup"

rm.plot2(OBK.long, "id", "measurement", "gender", "value")
add.ps(OBK.long, "id", "measurement", "gender", "value", ref.offset = 2, paired = TRUE) #reference is "post"

# Use R's standard (i.e., Welch test)
rm.plot2(OBK.long, "id", "treatment", "gender", "value")
add.ps(OBK.long, "id", "treatment", "gender", "value", prefixes = c("p(control vs. A)", "p(control vs. B)"))

# Use standard t-test
rm.plot2(OBK.long, "id", "treatment", "gender", "value")
add.ps(OBK.long, "id", "treatment", "gender", "value", var.equal = TRUE, prefixes = c("p(control vs. A)", "p(control vs. B)"))

}
}

