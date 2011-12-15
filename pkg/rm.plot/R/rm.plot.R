
rm.plot <- function(d, col.offset = 2, col.x = 3,  col.d = 4, noise = 0.008, na.rm = FALSE,
				pch = 21:25, lty = 1:5, bg.b.col = "darkgrey", bg.f.col = NULL, fg.b.col = "black", fg.f.col="black", type = "o",
				xlab = "", ylab = "", ylim, max.offset = 0.2, xaxis = TRUE, x.labels, xaxt ="n", plot = TRUE,
				legend = TRUE, mar = NULL, reset.mar = TRUE, l.pos, yjust = 0.5, l.bty = "n", l.adj = c(0, 0.5),  ...) {


	# add jitter (i.e., random noise) for points with identical y-value.
	addJitter <- function (x, noise) {
		dupl <- duplicated(x)
		x[dupl] <- x[dupl] + runif(sum(dupl),-noise,noise)
		return(x)
	}


	# create 2-dimensional vector containing data points for plotting of raw data.
	create.dp <- function(lst, n.x, noise) {
		ret <- vector('list',2)
		ret[[1]] <- rep(1:n.x, sapply(lst,length))
		ret[[2]] <- unlist(sapply(lst, addJitter, noise=noise))
		return(ret)
	}

	largs <- c("fill", "border", "angle", "density", "box.lwd", "box.lty", "box.col", 
		"pt.cex", "pt.lwd", "xjust", "x.intersp", "y.intersp", "text.width", "text.col",
		"merge", "trace", "plot", "ncol", "horiz", "title", "inset", "title.col", "title.adj")
	
	dots <- list(...)
	args.to.l <- dots[names(dots) %in% largs]
	args.to.p <- dots[!(names(dots) %in% largs)]
	
	if(!is.data.frame(d)) stop("d must be a data.frame")
	
	if(TRUE %in% is.na(d[,c(col.offset, col.x)])) warning("NAs in offset or x column (this produces other warnings).")
	if(na.rm == FALSE) if(TRUE %in% is.na(d[,c(col.d)])) stop("NAs in data column. Try: na.rm = TRUE")
	
	if (!is.factor(d[,col.offset])) {
		warning(paste("Converting offset variable (column ", col.offset, ") to factor.", sep=""))
		d[,col.offset] <- factor(d[,col.offset])
	}
	if (!is.factor(d[,col.x])) {
		warning(paste("Converting x-axis variable (column ", col.offset, ") to factor.", sep=""))
		d[,col.x] <- factor(d[,col.x])
	}
	if ((length(levels(d[,col.x])) != length(unique(d[,col.x])))) {
		warning(paste("Refactoring x-axis variable (column ", col.x, ") due to length mismatch.", sep=""))
		d[,col.x] <- factor(d[,col.x])
	}
	if ((length(levels(d[,col.offset])) != length(unique(d[,col.offset])))) {
		warning(paste("Refactoring offset variable (column ", col.offset, ") due to length mismatch.", sep=""))
		d[,col.offset] <- factor(d[,col.offset])
	}
	if (missing(ylim)) {
		ylim <- c(min(d[,col.d], na.rm = na.rm),max(d[,col.d], na.rm = na.rm))
		warning(paste("ylim not specified, taken from data: ", ylim[1], " - ", ylim[2], sep=""))
	}
	if (is.null(noise)) noise <- 0
	else noise <- (ylim[2]-ylim[1]) * noise
	
	n.offset <- length(levels(d[,col.offset]))
	n.x <- length(levels(d[,col.x]))
	
	if (!(missing(x.labels))) {
	if (length(x.labels) < n.x) {
			warning("x.labels too short, taking unique(d[,col.x]) as labels at x-axis ticks")
			x.labels <- levels(d[,col.x])
		}
	}
	
	while (length(pch) < n.offset) {
		warning("pch vector too short. recycling pch vector.")
		pch <- rep(pch, 2)
	}
	while (length(lty) < n.offset) {
		warning("lty vector too short. recycling lty vector.")
		lty <- rep(lty, 2)
	}
	if (missing(x.labels)) {
		x.labels <- levels(d[,col.x])
	}
	orig.mar <- par("mar")
	if (legend == TRUE & is.null(mar)) {
		mar <- orig.mar
		max.l <- max(nchar(levels(d[,col.offset])))
		if (max.l < 3) rb <- 4.2
		else if (max.l > 2 & max.l < 5) rb <- 5
		else if (max.l > 4 & max.l < 7) rb <- 6
		else if (max.l > 6 & max.l < 9) rb <- 7
		else rb <- 8
		mar[4] <- rb + 0.1
	}
	if (!plot) mar <- c(0,0,0,0)
	if (!is.null(mar)) res.mar <- par(mar = mar)
	nd <- split(d, d[,col.offset])
	#empty plot
	if (plot) {
		do.call('plot', c(list(x = 1, y = 2, xlim = c((1-max.offset-0.2),(n.x+max.offset+0.2)), ylim = ylim, xaxt = xaxt, type ="n", xlab = xlab, ylab = ylab), args.to.p))
		#calculate offset values
		if (n.offset > 1) {
			offset.start <- max.offset - ((1-(n.offset%%2)) * (max.offset / n.offset))
			offset.dist <- max.offset / ((n.offset - (n.offset%%2)) / 2)
		}
		if (n.offset == 1) {
			offset.start <- 0
			offset.dist <- 0
		}
		#points
		for (c in 1:n.offset) {
			d.c <- nd[[c]]
			d.lst <- split(d.c[,col.d], d.c[,col.x])
			dp <- create.dp(d.lst, n.x, noise)
			x <- dp[[1]] - ((offset.start)-((c-1)*offset.dist))
			y <- dp[[2]]
			points(x,y, pch = pch[c], col = bg.b.col, bg = bg.f.col)
		}
		#lines
		for (c in 1:n.offset) {
			d.c <- nd[[c]]
			d.lst <- split(d.c[,col.d], d.c[,col.x])
			x <- 1:n.x - ((offset.start)-((c-1)*offset.dist))
			y <- sapply(d.lst, mean, na.rm = na.rm)
			lines(x, y, pch = pch[c], type = type, lty = lty[c], col = fg.b.col, bg = fg.f.col )
		}
		if (xaxis == TRUE) axis(side = 1, at = 1:n.x, labels = x.labels)
	}
	if (!plot) {
		plot(0,0, type = "n", xlim = c(0, 10), ylim = c(0,10), axes = FALSE, ylab = "", xlab = "" , mar = c(0,0,0,0))
		if (missing(l.pos)) l.pos = c(5,5)
	}
	if (legend == TRUE) {
		if (n.x == 1) {
			if (missing(l.pos)) {
				l.pos <- (n.x+0.45)
				l.pos[2] <- (ylim[2]-((ylim[2]-ylim[1])/2))
			}
			lty <- NULL
		}
		else (if (missing(l.pos)) {
			l.pos <- (n.x+max.offset+0.4)
			l.pos[2] <- (ylim[2]-((ylim[2]-ylim[1])/2))
		})
		do.call('legend', c(list(x = l.pos[1],y = l.pos[2], levels(d[,col.offset]), pch = pch, lty = lty, 
		col = fg.b.col, pt.bg = fg.f.col, yjust = yjust, bty = l.bty, adj = l.adj, xpd = TRUE), args.to.l))
	}
	if (legend == TRUE & reset.mar == TRUE) {
		par(mar = res.mar)
	}
}
