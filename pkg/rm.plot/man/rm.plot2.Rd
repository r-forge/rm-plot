\name{rm.plot2}
\alias{rm.plot2}
\title{
rm.plot for data with too many cells
}
\description{
This function is a convenient wrapper for rm.plot that aggregates the data per individual dataset and passes the resulting data to rm.plot.
}
\usage{
rm.plot2(d, col.id, col.offset, col.x, col.d, fun.aggregate = "mean", ...)
}
%- maybe also 'usage' for other objects documented here.
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
  \item{\dots}{
further arguments passed to \code{\link{rm.plot}}.
}
}
\details{
Uses \code{\link{aggregate}} with formula notation for aggregation.
}
\value{
nothing (a plot is created using base graphics)
}
\author{
Henrik Singmann
}

\seealso{
\code{\link{rm.plot}}
}
\examples{
\dontrun{
#The examples uses the OBrienKaiser dataset and needs reshape.
require(reshape)
data(OBrienKaiser)
OBKnew <- cbind(factor(1:nrow(OBrienKaiser)), OBrienKaiser)
colnames(OBKnew)[1] <- "id"
OBK.long <- melt(OBKnew)
OBK.long[, c("measurement", "time")] <- t(vapply(strsplit(as.character(OBK.long$variable), "\\."),  "[", c("", "")))

rm.plot2(OBK.long, "id", "measurement", "gender", "value")

rm.plot2(OBK.long, "id", "treatment", "gender", "value")

}
}

