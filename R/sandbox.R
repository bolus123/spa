q1 <- 15
q2 <- 20
q3 <- 37
q4 <- 40
q5 <- 55
m <- 36


y <- seq(q1 - 1, q5 + 1, 1)
x <- seq(1, length(y), 1)

plot(x, y, type = "n", ylab = "", xlab = "", frame.plot = F, axes = F)
segments(length(x)/2 - 3, q5, length(x)/2 + 3, q5)
segments(length(x)/2 - 3, q1, length(x)/2 + 3, q1)
segments(length(x)/2 - 5, q4, length(x)/2 + 5, q4)
segments(length(x)/2 - 5, q2, length(x)/2 + 5, q2)
segments(length(x)/2 - 5, q3, length(x)/2 + 5, q3)
segments(length(x)/2 - 5, q2, length(x)/2 - 5, q4)
segments(length(x)/2 + 5, q2, length(x)/2 + 5, q4)
segments(length(x)/2, q4, length(x)/2, q5)
segments(length(x)/2, q1, length(x)/2, q2)
points(length(x)/2, m, type = "b", col = "red")
text(length(x)/2 + 10, q5, labels = c(paste("Q95 =", q5)))
text(length(x)/2 + 10, q4, labels = c(paste("Q75 =", q4)))
text(length(x)/2 + 10, q3, labels = c(paste("Q50 =", q3)))
text(length(x)/2 + 10, q2, labels = c(paste("Q25 =", q2)))
text(length(x)/2 + 10, q1, labels = c(paste("Q5 =", q1)))
text(length(x)/2 + 10, m, labels = c(paste("Mean =", m)))

L <- 3
q1 <- qgeom(0.05, 2 - (2 * pnorm(L)))
q2 <- qgeom(0.25, 2 - (2 * pnorm(L)))
q3 <- qgeom(0.5, 2 - (2 * pnorm(L)))
q4 <- qgeom(0.75, 2 - (2 * pnorm(L)))
q5 <- qgeom(0.95, 2 - (2 * pnorm(L)))
m <-  1/(2 - (2 * pnorm(L)))

y <- seq(q1 - 1, q5 + 1, 1)
x <- seq(1, length(y), 1)

box <- plot(x, y, type = "n", ylab = "", xlab = "", frame.plot = F, axes = F)
segments(length(x)/2 - 50, q5, length(x)/2 + 50, q5)
segments(length(x)/2 - 50, q1, length(x)/2 + 50, q1)
segments(length(x)/2 - 100, q4, length(x)/2 + 100, q4)
segments(length(x)/2 - 100, q2, length(x)/2 + 100, q2)
segments(length(x)/2 - 100, q3, length(x)/2 + 100, q3)
segments(length(x)/2 - 100, q2, length(x)/2 - 100, q4)
segments(length(x)/2 + 100, q2, length(x)/2 + 100, q4)
segments(length(x)/2, q4, length(x)/2, q5)
segments(length(x)/2, q1, length(x)/2, q2)
points(length(x)/2, m, type = "b", col = "red")
text(length(x)/2 + 200, q5, labels = c(paste("Q95 =", q5)))
text(length(x)/2 + 200, q4, labels = c(paste("Q75 =", q4)))
text(length(x)/2 + 200, q3, labels = c(paste("Q50 =", q3)))
text(length(x)/2 + 200, q2, labels = c(paste("Q25 =", q2)))
text(length(x)/2 + 200, q1, labels = c(paste("Q5 =", q1)))
text(length(x)/2 + 320, m, labels = c(paste("Mean =", m)))

box