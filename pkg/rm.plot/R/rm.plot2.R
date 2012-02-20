
rm.plot2 <- function(d, col.id, col.offset, col.x, col.d, fun.aggregate = "mean", ... ) {
	
	if(!is.data.frame(d)) stop("d must be a data.frame")
	
	columns <- c(col.id, col.offset, col.x, col.d)
	
	if (any(!(columns %in% colnames(d)))) stop("column not matching the data")
	
	formula.agg <- as.formula(paste(col.d, "~", col.id, "+", col.offset, "+", col.x))
	
	d.new <- aggregate(formula.agg, data = d, FUN = fun.aggregate)
	rm.plot(d.new, col.offset = col.offset, col.x = col.x, col.d = col.d, ...)
}
