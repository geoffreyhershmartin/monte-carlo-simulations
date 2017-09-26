# Torus Area Estimation
set.seed(1)
m <- 1e7

# set axes limits
x <- runif(m, min = 1, max = 4)
y <- runif(m, min = -3 , max = 4)
# z goes between +/- 1
z <- runif(m, min = -1, max = 1)

# We only need to add these conditions because the 
# conditions on x and y are taken care of in the previous lines
hits <- sum(z^2 + (sqrt(x^2 + y^2) - 3)^2 <= 1)
box = 3 * 7 * 2
#volume = hits/m*box

t <- prop.test(hits, m)
cat("Estimated integral:", t$estimate*box, "\n")
cat("95% CI: [", t$conf.int[1]*box, ", ", t$conf.int[2]*box, "]",sep = "")