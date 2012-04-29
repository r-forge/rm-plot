x <- data.frame(id = 1:150, offset = rep(c("Group A", "Group B", "Group C"),each = 50), xaxis = sample(c("A", "B", "C", "D"),150, replace = TRUE), data = c(rnorm(50, 10, 5), rnorm(50, 15,6), rnorm(50, 20, 5)))

raw.means.plot(x)

raw.means.plot(x, main = "Example", ylab = "Values", xlab = "Factor", title = "Groups")

raw.means.plot(x, "offset", "xaxis", "data")

raw.means.plot(x, "xaxis", "offset", "data")

raw.means.plot(x, 3, 2, 4)

x2 <- data.frame(id = 1:150, offset = rep(c("Group A", "Group B", "Group C"),each = 50), xaxis = sample(c("A", "B", "C", "D"),150, replace = TRUE), data = c(rnorm(50, 10, 5), rnorm(50, 15,6), rnorm(50, 20, 5)))

layout(matrix(c(1,2,3,3), 2,2,byrow = TRUE), heights = c(7,1))
raw.means.plot(x, main = "Data x1", ylab = "Values", xlab = "Factor", legend = FALSE, mar = c(4,4,4,1)+0.1)
raw.means.plot(x2, main = "Data x2", ylab = "Values", xlab = "Factor", legend = FALSE, mar = c(4,4,4,1)+0.1)
raw.means.plot(x2, plot = FALSE, title = "Groups")


y <- data.frame(id = 1:300, offset = rep(1, 300), axis = sample(LETTERS[1:6],300, replace = TRUE), data = c(rnorm(100, 1), rnorm(100), rnorm(100,1)))

par(mfrow = c(2,2))

raw.means.plot(y, legend = FALSE)

raw.means.plot(y, type = "p", legend = FALSE)

raw.means.plot(y, type = "l", legend = FALSE)

raw.means.plot(y, 3, 2, x.labels = "one group only")

z <- data.frame (id = 1:200, offset = rep(c("C 1", "C 2"), 200), axis = sample(LETTERS[1:4], 200, replace = TRUE), data = sample(1:20, 200, replace = TRUE))


# x versus y jitter

par(mfrow = c(2,2))

raw.means.plot(z, avoid.overlap = "none", main = "no-jitter")

raw.means.plot(z, main = "y-axis jitter (default)")

raw.means.plot(z, avoid.overlap = "x", main = "x-axis jitter")

raw.means.plot(z, avoid.overlap = "both", main = "both-axis jitter")


# y-axis jitter (default)

par(mfrow = c(2,2))

raw.means.plot(z, avoid.overlap = "none", main = "no jitter")

raw.means.plot(z, y.factor = 0.5, main = "smaller y-jitter")

raw.means.plot(z, main = "standard y-jitter")

raw.means.plot(z, y.factor = 2, main = "bigger y-jitter")



# x-axis jitter (default)

par(mfrow = c(2,2))

raw.means.plot(z, avoid.overlap = "none", main = "no jitter")

raw.means.plot(z, avoid.overlap = "x", x.amount = 0.025, main = "smaller x -jitter")

raw.means.plot(z, avoid.overlap = "x", main = "standard x-jitter")

raw.means.plot(z, avoid.overlap = "x", x.amount= 0.1, main = "bigger x-jitter")






\dontrun{

#The examples uses the OBrienKaiser dataset from car and requires the reshape package.
require(reshape)
require(car)
data(OBrienKaiser)
OBKnew <- cbind(factor(1:nrow(OBrienKaiser)), OBrienKaiser)
colnames(OBKnew)[1] <- "id"
OBK.long <- melt(OBKnew)
OBK.long[, c("measurement", "time")] <- t(vapply(strsplit(as.character(OBK.long$variable), "\\."),  "[", c("", "")))

raw.means.plot2(OBK.long, "id", "measurement", "gender", "value")

raw.means.plot2(OBK.long, "id", "treatment", "gender", "value")

# also use add.ps:
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