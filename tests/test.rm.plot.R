
x <- data.frame(id = 1:150, offset = rep(c("Group A", "Group B", "Group C"),each = 50), xaxis = sample(c("A", "B", "C", "D"),150, replace = TRUE), data = c(rnorm(50, 10, 5), rnorm(50, 15,6), rnorm(50, 20, 5)))

rm.plot(x)

rm.plot(x, main = "Example", ylab = "Values", xlab = "Factor", title = "Groups")

rm.plot(x, "offset", "xaxis", "data")

rm.plot(x, "xaxis", "offset", "data")

rm.plot(x, 3, 2, 4)

x2 <- data.frame(id = 1:150, offset = rep(c("Group A", "Group B", "Group C"),each = 50), xaxis = sample(c("A", "B", "C", "D"),150, replace = TRUE), data = c(rnorm(50, 10, 5), rnorm(50, 15,6), rnorm(50, 20, 5)))

layout(matrix(c(1,2,3,3), 2,2,byrow = TRUE), heights = c(7,1))
rm.plot(x, main = "Data x1", ylab = "Values", xlab = "Factor", legend = FALSE, mar = c(4,4,4,1)+0.1)
rm.plot(x2, main = "Data x2", ylab = "Values", xlab = "Factor", legend = FALSE, mar = c(4,4,4,1)+0.1)
rm.plot(x2, plot = FALSE, title = "Groups")


y <- data.frame(id = 1:300, offset = rep(1, 300), axis = sample(LETTERS[1:6],300, replace = TRUE), data = c(rnorm(100, 1), rnorm(100), rnorm(100,1)))

par(mfrow = c(2,2))

rm.plot(y, legend = FALSE)

rm.plot(y, type = "p", legend = FALSE)

rm.plot(y, type = "l", legend = FALSE)

rm.plot(y, 3, 2, x.labels = "one group only")

 


z <- data.frame (id = 1:200, offset = rep(c("C 1", "C 2"), 200), axis = sample(LETTERS[1:4], 200, replace = TRUE), data = sample(1:20, 200, replace = TRUE))

par(mfrow = c(2,2))

rm.plot(z, noise = NULL, main = "no jitter")

rm.plot(z, noise = .001, main = "smaller jitter")

rm.plot(z, main = "standard jitter")

rm.plot(z, noise = .01, main = "bigger jitter")

data(OBrienKaiser)

OBKnew <- cbind(factor(1:nrow(OBrienKaiser)), OBrienKaiser)
colnames(OBKnew)[1] <- "id"

OBK.long <- melt(OBKnew)
OBK.long[, c("measurement", "time")] <- t(vapply(strsplit(as.character(OBK.long$variable), "\\."),  "[", c("", "")))

aggregate(value ~ id + treatment + gender, data = OBK.long, mean)

aggregate(value ~ id + measurement + gender, data = OBK.long, mean)

rm.plot2(OBK.long, col.id = "id", "measurement", "gender", "value")

rm.plot2(OBK.long, col.id = "id", "treatment", "gender", "value")

aggregate(OBK.long, col.id = "id", "treatment", "gender", "value")


