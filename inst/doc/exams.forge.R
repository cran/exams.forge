## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(exams.forge)

## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----echo=FALSE---------------------------------------------------------------
suppressPackageStartupMessages({
library("extraDistr")
library("exams")
library("exams.forge")
})

## ----eval=FALSE---------------------------------------------------------------
#  library("exams")
#  library("exams.forge")
#  repeat {
#    ... # some data generation
#    if (condition_holds) break
#  }

## ----eval=FALSE---------------------------------------------------------------
#  library("exams")
#  library("exams.forge")
#  repeat {
#    x  <- sample(1:10, size=5)
#    sx <- sort(x)
#    if (x[3]!=sx[3]) break
#  }
#  x

## -----------------------------------------------------------------------------
# Generate a time series 
ts_eg <- ts_data(end = 20, trend = TRUE, trend.coeff = c(1, 0.5),
                               season = TRUE, season.coeff = c(0.2, 0.1),
                               error = TRUE, error.coeff = 0.1, digits = 2)
print(ts_eg)

## -----------------------------------------------------------------------------
ts <- ts_data(12, trend.coeff= c(sample(0:10, 1), sample(1+(1:10)/20, 1)))
as_ts(ts)

## -----------------------------------------------------------------------------
# Create a time series data object with sinusoidal fluctuations
ts <- ts_data(20, trend.coeff = c(2))

# Compute the moving average with an order of 5
result_ts <- ts_moving_average(ts, 5)

# Display the original and extended time series data objects
cat("Original Time Series Data:\n")
str(ts)

cat("\nExtended Time Series Data with Moving Average:\n")
str(result_ts)

## -----------------------------------------------------------------------------
# Create a time series data object with a linear trend
ts <- ts_data(12, trend.coeff = c(sample(0:10, 1), sample(1 + (1:10)/20, 1)))

# Estimate trend and season
result_ts <- ts_trend_season(ts)

# Display the extended time series data object
str(result_ts)

## -----------------------------------------------------------------------------
# Generate data for a confidence interval with estimation error ranging from 0.1 to 1.0
result <- CImulen_data(sigma = 1:10, e = (1:10)/10)
str(result)
result <- CImulen_data(sigma = 1:10, e = (1:10)/10, full=TRUE)
head(result)

## -----------------------------------------------------------------------------
# Generate data for a confidence interval with estimation error 0.1
result <- CIpilen_data(pi = (1:9/10), e = (1:9)/10)

# Display the result
str(result)

## ----echo=FALSE, fig.width=6, fig.height=3------------------------------------
par(mar=c(0,0,0,0))
plot(c(0, 1), c(0.15,1.15), axes=FALSE, type="n", xlab="", ylab="")
rect(0.25, 0.25, 0.75, 0.75)
text(0.25, 0.25, labels="xleft", pos=1)
text(0.75, 0.25, labels="xright", pos=1)
text(0.5, 0.75, labels="width", pos=3)
arrows(0.25, 0.8, 0.75, 0.8, code=3, length=0.1)
arrows(0.0, 0.5, 0.2, 0.5, code=3, col="red", length=0.1)
arrows(0.8, 0.5, 1.0, 0.5, code=3, col="red", length=0.1)
text(0.8, 0.8, "xright+range[1]*width", col="red", srt=90)
text(1, 0.8, "xright+range[2]*width", col="red", srt=90)
text(0, 0.8, "xleft-range[2]*width", col="red", srt=90)
text(0.2, 0.8, "xleft-range[1]*width", col="red", srt=90)

## -----------------------------------------------------------------------------
x <- runif(7, 165, 195)
xr <- add_data(x, "range", n=c(0,1), range=c(1,1.5)) 
round(xr)
xb <- add_data(x, "box", n=c(0,1), range=c(1,1.5)) 
round(xb)
x1 <- add_data(x, box=c(165,195), n=c(0,1), range=c(1,1.5)) 
round(x1)

## -----------------------------------------------------------------------------
n <- sample(seq(25,50,5),1)
y <- meanint_data(n, c(2,12))
x <- meanint_data(n, c(36, 50))
z <- meanint_data(n, c(2,6))
yx <- cor_data(y, x, r=sample((5:9)/10, 1))
yz <- cor_data(y, z, r=sample((5:9)/10, 1))

## -----------------------------------------------------------------------------
# Generate a sequence of sample sizes from 5 to 10
data_n(10)

# Generate a sequence of sample sizes whose square root is an integer, from 9 to 961
data_nsq(1000)

# Generate a sequence of sample sizes divisible only by 2 and 5, from 5 to 1000
data_n25(1000)

## -----------------------------------------------------------------------------
numbers_check <- c(4, 10, 7.00001)
all_integer(numbers_check)

## -----------------------------------------------------------------------------
number_check <- 0.3125
result <- divisor_25(number_check)

## -----------------------------------------------------------------------------
# Taken from the exercise "Club_Raucher2"
maxn <- 100
repeat {
  n  <- sample(seq(5, maxn, 5),1)
  p  <- sample((1:20)/100, 1)
  x  <- n*c(p, 1-p)  
  if (all(has_digits(x, 0))) break
}
print(has_digits(x, 0))

## -----------------------------------------------------------------------------
prime_numbers(20)

## -----------------------------------------------------------------------------
primes(1:5)

## -----------------------------------------------------------------------------
x <- as_result(1/3, "prob")
tol(x)
rounded(x)
digits(x)
val(x)

## -----------------------------------------------------------------------------
x   <- runif(3)
tab <- vec2mat(x, colnames=1:length(x))
as_table(tab)
tab <- vec2mat(x, colnames=sprintf("%.0f-%0.f", 0:2, 1:3))
as_table(tab)

## -----------------------------------------------------------------------------
# Reordering observations in a frequency table to approximate a target association

# Creating a frequency table (2x2) with arbitrary values
frequency_table <- matrix(c(10, 20, 30, 40), nrow = 2, byrow = TRUE)

# Defining a target association value
target_association <- 0.5

# Applying assoc_data to reorder the frequency table to approximate the target association
result_table <- assoc_data(frequency_table, target = target_association, zero = TRUE, tol = 0.1, maxit = 100)

# Displaying the resulting reordered table
print(result_table)

## -----------------------------------------------------------------------------
random(-1:6)

## -----------------------------------------------------------------------------
# Generating a vector of 5 random uniform values
x <- runif(5)

# Applying refer with LaTeX default format
latex_result <- refer(x)
str(latex_result)

# Applying refer with R default format
r_default_result <- refer(x, fmt = "%s[%.0f]")
str(r_default_result)

## -----------------------------------------------------------------------------
# Generate a vector with a mix of positive and negative values
v <- c(2, -3, 1, 0, 5, -4)

# Transform only negative values using a custom shift (a) and scale (b)
transformed_vector <- transformif(v, v < 0, a = 2, b = 0.5)

# Display the original and transformed vectors
cat("Original Vector: ", v, "\n")
cat("Transformed Vector: ", transformed_vector, "\n")

## -----------------------------------------------------------------------------
# Generate a vector
vec <- c(1, 2, 3, 4, 5)

# Convert the vector to a horizontal matrix with custom column names
mat_horizontal <- vec2mat(vec, colnames = c("A", "B", "C", "D", "E"))

# Display the resulting matrix
print(mat_horizontal)

# Convert the vector to a vertical matrix with custom row names
mat_vertical <- vec2mat(vec, rownames = c("First", "Second", "Third", "Fourth", "Fifth"), horizontal = FALSE)

# Display the resulting matrix
print(mat_vertical)

## -----------------------------------------------------------------------------
# Single type
size <- 421
prob <- 0.5
cutoff <- 9
result_single <- binom2norm(size, prob, c=cutoff, type="single")
cat("Single type:", result_single, "\n")

# Double type
result_double <- binom2norm(size, prob, c=cutoff, type="double")
cat("Double type:", result_double, "\n")

## -----------------------------------------------------------------------------
# Check for a broader range of observations
observations <- c(20, 40, 80, 120, 200, 300, 500, 1000)

# Assess whether each observation size is suitable for CLT approximation
clt_approximation_results <- clt2norm(n = observations)

# Display the results
print(clt_approximation_results)

## -----------------------------------------------------------------------------
# Check for a range of observations
observations <- c(10, 30, 50, 100, 200)

# Assess whether each observation size is suitable for t-distribution approximation
approximation_results <- t2norm(n = observations)

# Display the results
print(approximation_results)

## -----------------------------------------------------------------------------
turnier <- ifelse(as.integer(format(Sys.Date(), "%Y")) %% 4 >= 2, "welt", "europa")
popSize     <- 100
classbreaks <- c(0, 50, 100, 200)
gd <- grouped_data(classbreaks, popSize*ddiscrete(runif(length(classbreaks)-1)), 0.5)
print(gd)

## -----------------------------------------------------------------------------
lcmval(c(144, 160, 175))

## -----------------------------------------------------------------------------
# Numeric
x <- sample(1:5, size=25, replace = TRUE)
table(x)
mcval(x)
# Character
x <- sample(letters[1:5], size=25, replace = TRUE)
table(x)
mcval(x)
# Histogram
x <- hist(runif(100), plot=FALSE)
mcval(x)
mcval(x, exact=TRUE)

## -----------------------------------------------------------------------------
tab <- matrix(round(10*runif(15)), ncol=5)
nom.cc(tab)
nom.cc(tab, correct=TRUE)
nom.cramer(tab)
ord.spearman(tab)
ord.kendall(tab)

## -----------------------------------------------------------------------------
data(sos)
n     <- sample(4:8, 1)
rseq  <- seq(-0.95, 0.95, by=0.05)
r     <- sample(rseq, size=1, prob=rseq^2)
xy0   <- pearson_data(r=r, nmax=n, n=100, xsos=sos100)
str(xy0)

## -----------------------------------------------------------------------------
# Example: Decomposing the integer 50 into a sum of squared integers
sos_example <- sumofsquares(50, nmax = 8, zerosum = FALSE, maxt = Inf, size = 100000L)
str(sos_example)

## -----------------------------------------------------------------------------
digits <- 2  # round to two digits
repeat {
  x  <- round(runif(7, min=165, max=195), digits)
  ms <- means_choice(x, digits)         
  if (attr(ms, "mindiff")>0.1) break   # make sure that all values are different by 0.1
}
ms <- unlist(ms)
sc <- to_choice(ms, names(ms)=='mean') # arithmetic mean is the correct solution
str(sc)

## -----------------------------------------------------------------------------
x <- runif(21)
y <- scale_to(x, mean=2, sd=0.5)
print(y)

## -----------------------------------------------------------------------------
variation(7,3)         # without replication
variation(7,3, TRUE)   # with replication
combination(7,3)       # without replication
combination(7,3, TRUE) # with replication
permutation(7)
permutation(7, c(2,1,4)) # three groups with indistinguishable elements
z <- combinatorics(7, 4)
str(z)

## -----------------------------------------------------------------------------
permutation(5, c(2, 2)) 

## -----------------------------------------------------------------------------
lfact(5)
lfactquot(5,3,2)
lbinom(6,3)

## -----------------------------------------------------------------------------
# Probability Mass Function
pdunif2(1:13)

# Distribution Function
ddunif2(1:13)

# Quantile Function
qdunif2((0:4)/4)

# Random Generation
rdunif2(10)

## -----------------------------------------------------------------------------
d <- distribution("t", df=15)
quantile(d, c(0.025, 0.975))
d <- distribution("norm", mean=0, sd=1)
cdf(d, c(-1.96, +1.96))
d <- distribution("binom", size=9, prob=0.5)
pmdf(d, 5)

## -----------------------------------------------------------------------------
# Taken from the exercise "Würfel 2".
d      <- distribution("dunif", min=1, max=6)
border <- sample(1:5, size=1)+1
ptype  <- "point"
lsg    <- prob1(d, border)
sc     <- num_result(lsg, 4)
str(d)
print(lsg)

## -----------------------------------------------------------------------------
# Check if an object is a distribution
x <- distribution("norm", mean=1.4, sd=0,44)
is.distribution(x)

# Check if an object is a specific distribution type
is.distribution(x, "exp")

## -----------------------------------------------------------------------------
# Generate binomial parameters for a specific case
params <- binom_param(600, 0.6, mean = 0, sd = 0)

# Display the generated parameters
print(params)

## -----------------------------------------------------------------------------
# Calculate sqrtnp for different combinations of n and p
result <- sqrtnp(n = c(50, 100, 150), p = c(0.25, 0.5, 0.75), digits = 3)

# Display the resulting data frame
print(result)

## -----------------------------------------------------------------------------
# Create a distribution object for a normal distribution
normal_distribution <- distribution("norm", mean = 0, sd = 1)

# Calculate CDF for normal distribution
quantiles <- seq(-3, 3, by = 0.5)  # Quantiles for which to compute CDF
cdf_values <- cdf(normal_distribution, quantiles)  # Compute CDF values

# Display the results
cat("Quantile\tCDF Value\n")
cat("----------------------------\n")
for (i in 1:length(quantiles)) {
  cat(quantiles[i], "\t\t", cdf_values[i], "\n")
}

## -----------------------------------------------------------------------------
# Taken from the exercise "Haribo_3"
n    <- sample(2:10, 1)    # Gruppe 1: keine Frösche und Himbeeren
nj   <- 0                  
m    <- sample(2:10, 1)    # Gruppe 2: Frösche und Himbeeren
mj   <- sample(1:(m-1), 1)              
k    <- mj+nj
d    <- distribution(name="hyper", m=m, n=n, k=k)
lsg  <- pmdf(d, k)
str(lsg)

## -----------------------------------------------------------------------------
# Generating a set of random discrete probabilities with a total sum of 200
f <- ddiscrete(runif(6), unit=200)

# Checking compatibility for a sequence of sample sizes from 50 to 300 with a step of 1
result_default <- sample_size_freq(seq(50, 300, 1), f)
str(result_default)

# Checking compatibility for a sequence of sample sizes from 10 to 700 with a step of 1, with 'which' set to 200
result_specific <- sample_size_freq(seq(10, 700, 1), f, which=200)
str(result_specific)

## -----------------------------------------------------------------------------
# Estimate mean and standard deviation for a normal distribution based on quantiles.

quantiles <- c(10, 20)  # Example quantiles
probabilities <- c(0.1, 0.9)  # Example probabilities
result <- q2norm(quantiles, probabilities)
str(result)

## -----------------------------------------------------------------------------
# Always includes 100 and 200 in the breakpoints
histbreaks(seq(100, 200, by = 10), 4)

# Always includes 100 and 200; randomly chooses between 3 to 5 breakpoints
histbreaks(seq(100, 200, by = 10), 3:5)

# May not include 100 and 200
histbreaks(seq(100, 200, by = 10), 4, outer = FALSE)

## -----------------------------------------------------------------------------
x  <- runif(25) 
h1 <- hist(x, plot=FALSE)
str(h1)
h2 <- histdata(x)
str(h2)

## -----------------------------------------------------------------------------
x <- runif(25) 
h <- histdata(x)
# mean
mean(h)
# median & quantile
median(h)
quantile(h)
# mode
mcval(h)
mcval(h, exact=TRUE)

## -----------------------------------------------------------------------------
hw <- histwidth(1.6, 2.1, widths=0.05*(1:4))
str(hw)
x  <- histx (hw$breaks, hw$n)
hist(x, hw$breaks)
rug(x)

## -----------------------------------------------------------------------------
breaks <- seq(1.6, 2.1, by=0.1)
x <- histx (breaks, sample(5:15, length(breaks)-1))
hist(x, breaks)
rug(x)

## -----------------------------------------------------------------------------
# Generate a data_prob2 object with default parameters
x <- data_prob2()
str(x)

# Generate a data_prob2 object with colnames="E"
data_prob2(colnames="E")

# Generate a data_prob2 object with nrow=3
data_prob2(nrow=3)

## -----------------------------------------------------------------------------
ddiscrete(6) # fair dice
x <- runif(6)
ddiscrete(x)
ddiscrete(x, zero=TRUE)
ddiscrete(x, unit=15)
fractions(ddiscrete(x, unit=15))

## -----------------------------------------------------------------------------
# Exercise: Modify the discrete probability function for a biased coin

# Given biased coin probabilities (Heads, Tails)
biased_coin_prob <- c(0.8, 0.2, 0, 0, 0, 0)

# 1. Create a discrete probability function for the biased coin
biased_coin_fun <- ddiscrete(biased_coin_prob)
print(biased_coin_fun)

# 2. Create a modified discrete probability function allowing zeros
modified_coin_fun <- ddiscrete(biased_coin_prob, zero = TRUE)
print(modified_coin_fun)

# 3. Experiment with different resolutions (units)
unit_100 <- ddiscrete(biased_coin_prob, unit = 100)
unit_1000 <- ddiscrete(biased_coin_prob, unit = 1000)
print(unit_100)
print(unit_1000)

## -----------------------------------------------------------------------------
r <- ddiscrete(6)
c <- ddiscrete(6)
ddiscrete2(r, c)
ddiscrete2(r, c, FUN=nom.cc, target=0.4)
ddiscrete2(r, c, FUN=nom.cc, target=1)

## -----------------------------------------------------------------------------
is.prob(runif(1))

## -----------------------------------------------------------------------------
y <- pprobability(0:2, coef=seq(-2, 2, by=0.1))
str(y)

## -----------------------------------------------------------------------------
# Compute the probability for an interval in a uniform distribution
d <- distribution("unif", min=1, max=7)
prob(d)

## -----------------------------------------------------------------------------
n     <- sample(4:8, 1)
lm1   <- lm1_data(0.4, nmax=n, xsos=sos100)
print(lm1)

## -----------------------------------------------------------------------------
n   <- sample(c(4,5,8,10),1)
lmr <- lmr_data(c(1,3), c(2,8), n=n, r=sample(seq(0.1, 0.9, by=0.05), 1))
print(lmr)

## -----------------------------------------------------------------------------
tab <- rbind(c(0.02, 0.04, 0.34), c(0.02, 0.28, 0.3))
result <- incomplete_table(tab, 7)
print(result)
# Here column no. 4 and row no. 3 constitute the summaries of their respective columns and rows.

## -----------------------------------------------------------------------------
# attr(,"fillin")
#      [,1] [,2]
# [1,]    2    2
# [2,]    2    2
# [3,]    4    4
# [4,]    1    1
# [5,]    3    3
# [6,]    3    3
# [7,]    1    1

## -----------------------------------------------------------------------------
#  attr(,"full")
#      [,1] [,2] [,3] [,4]
# [1,] 0.02 0.04 0.34  0.4
# [2,] 0.02 0.28 0.30  0.6
# [3,] 0.04 0.32 0.64  1.0

## -----------------------------------------------------------------------------
# Generate a frequency table with 4 rows and 3 columns
generated_table <- table_data(nrow = 4, ncol = 3, unit = 20, n = 150, maxit = 5000)

# Display the generated frequency table
print(generated_table)

## -----------------------------------------------------------------------------
# Set up a base proportion test
n <- 150
x <- sum(runif(n) < 0.6)
basetest <- proptest_num(x = x, n = n)

# Generate all different tests
all_tests <- proptests(basetest, hyperloop = TRUE)
str(all_tests)

# Generate all different random sampling functions
x_functions <- proptests(basetest, elem = "X", hyperloop = TRUE)
str(x_functions)

## -----------------------------------------------------------------------------
# Generate binomial test data with default settings
data_d <- proptest_data()

# Generate binomial test data with custom settings
data_c <- proptest_data(            
  size = 20:50,                            # Vector of sample sizes  
  prob = seq(0.1, 0.9, by = 0.2),          # Vector of probabilities 
  reject = FALSE,                          # Determines whether the generated data leads to a rejection of the null hypothesis
  alternative = "less",                    # Specifies the alternative hypothesis, must be "less" or "greater"
  alpha = 0.05,                            # Vector of significance levels
  norm.approx = TRUE,                      # Specifies whether a normal approximation should be used 
  maxit = 500                              # Maximum number of trials
)
str(data_c)

## -----------------------------------------------------------------------------
# Example with default parameters
n <- 100
x <- sum(runif(n) < 0.4)
result <- proptest_num(x = x, n = n)
str(result)

## -----------------------------------------------------------------------------
# Generate t-test data 
ttest_data_scenario1 <- ttest_data(
  size = c(25, 64, 121),
  mean = c(0, 2, -2),
  sd = c(0.5, 0.7, 1),
  reject = TRUE,  # Rejection condition
  alternative = "two.sided",
  alpha = c(0.01, 0.05, 0.1),
  z = seq(-3.49, 3.49, by = 0.01),
  use.sigma = TRUE
)

## -----------------------------------------------------------------------------
sigma  <- sample(5:30, size=1)
ttest  <- ttest_num(n           = sample((4:8)^2, size=1), 
                    mu0         = sample(seq(1.5, 3, by=0.1)+0.5, size=1),
                    mean        = sample(seq(1.5, 3, by=0.1), size=1),
                    alternative = 'greater',
                    sd          =  sample((sigma-3:sigma+3), size=1)/10,
                    sigma       = sigma/10,   
                    norm        = TRUE)
str(ttest)

## -----------------------------------------------------------------------------
# Generate a base t-test
base_ttest <- ttest_num(mean = 1.2, sd = 0.8, n = 30, sigma = 1)

# Vary the parameters for hyperloop
hyperloop_variation <- list(
  mean = c(base_ttest$mean - 0.5, base_ttest$mean, base_ttest$mean + 0.5),
  n = c(20, 30, 40),
  sd = c(0.7, 0.8, 0.9)
)

# Obtain different t-tests with varied parameters
different_ttests <- ttests(base_ttest, hyperloop = hyperloop_variation)

# Extract t-tests where the element "Conf.Int" differs
confint_differing_ttests <- ttests(base_ttest, "Conf.Int", hyperloop = hyperloop_variation)

## -----------------------------------------------------------------------------
# Generate double intervals 
result_1 <- dbl(2)
print(result_1)

# Generate positive intervals
result_2 <- pos(3)
print(result_2)

# Generate negative intervals
result_3 <- neg(3)
print(result_3)

## -----------------------------------------------------------------------------
degree <- 3
coefficient <- 2

# Generate a monomial with the specified degree and coefficient
result_monomial <- monomial(3, 2)
cat("Monomial:", result_monomial, "\n")

## -----------------------------------------------------------------------------
# Creating a polynomial and finding the minimum within a specified range
custom_polynomial <- polynomial(c(2, -1, 4, -2))  # Represents 2x^3 - x^2 + 4x - 2

# Finding the minimum of the polynomial within the range [-1, 2]
minimum_result <- pminimum(custom_polynomial, -1, 2)

# Displaying the result
print(minimum_result)

## -----------------------------------------------------------------------------
x <- c(1/5, 1/6)
x
fractions(x)
str(fractions(x))

## -----------------------------------------------------------------------------
x <- c(1/5, 1/6)
is_terminal(x)

## -----------------------------------------------------------------------------
# Create a 5x5 matrix with random values
Y <- matrix(runif(25), 5, 5)

# Display the matrix as fractions using the `fractions` function
fractions(Y)

# Perform matrix operations and display the results as fractions
fractions(solve(Y, Y/5))
fractions(solve(Y, Y/5) + 1)

## -----------------------------------------------------------------------------
x <- pi
y <- pi+1e-4
equal(x, y)
equal(x, y, tol=1e-3)

## -----------------------------------------------------------------------------
# Defining a system of economics equations

econ_eq <- equations(
  Y ~ C + I + G + (X - M), "Y = C + I + G + (X - M)",
  C ~ c0 + c1*YD, "C = c_0 + c_1\\cdot YD",
  I ~ I0 - i1*r + i2*Y, "I = I_0 - i_1\\cdot r + i_2\\cdot Y",
  YD ~ Y - T, "YD = Y - T",
  T ~ t0 + t1*Y, "T = t_0 + t_1\\cdot Y",
  M ~ m0 + m1*Y, "M = m_0 + m_1\\cdot Y",
  X ~ x0 + x1*Y, "X = x_0 + x_1\\cdot Y",
  r ~ r0, "r = r_0"
)
print(econ_eq)

## -----------------------------------------------------------------------------
# The equations describe the formulae for an confidence interval of the mean
e <- equations(o~x+c*s/sqrt(n), "v_o=\\bar{x}+c\\cdot\\frac{s^2}{n}",
               u~x-c*s/sqrt(n), "v_u=\\bar{x}-c\\cdot\\frac{s^2}{n}",
               e~c*s/sqrt(n),   "e  =c\\cdot\\frac{s^2}{\\sqrt{n}}",
               l~2*e,           "l  =2\\cdot e"
               )
print(e)

## -----------------------------------------------------------------------------
# The equations describe the formulae for a confidence interval of the mean
e <- equations(o~x+c*s/sqrt(n), "v_o=\\bar{x}+c\\cdot\\frac{s^2}{n}", 
               u~x-c*s/sqrt(n), "v_u=\\bar{x}-c\\cdot\\frac{s^2}{n}", 
               e~c*s/sqrt(n),   "e  =c\\cdot\\frac{s^2}{\\sqrt{n}}",
               l~2*e,           "l  =2\\cdot e"                   
               )

# Set variable values, intervals, and LaTeX representations
e <- variables(e, 
               x=0,    "\\bar{x}",
               c=2.58, dbl(2), 
               s=1,    pos(5), "s^2",
               n=25,   pos(5),
               l=pos(5), 
               e=pos(5),
               u="v_u", o="v_o")

# Print the modified equations object
print(e)

## -----------------------------------------------------------------------------
# The equations describe the formulae for an confidence interval of the mean
e <- equations(o~x+c*s/sqrt(n), "v_o=\\bar{x}+c\\cdot\\frac{s^2}{n}",
               u~x-c*s/sqrt(n), "v_u=\\bar{x}-c\\cdot\\frac{s^2}{n}",
               e~c*s/sqrt(n),   "e  =c\\cdot\\frac{s^2}{\\sqrt{n}}",
               l~2*e,           "l  =2\\cdot e"
               )
# Setting variables and their values
e <- variables(e, x = 0, c = 2.58, s = 1, n = 25, l = pos(5), e = pos(5), u = "v_u", o = "v_o")

# Finding confidence interval length ('l')
ns <- num_solve('l', e)

# Computing all possible values
ns <- num_solve('', e)
print(ns)

## -----------------------------------------------------------------------------
p <- polynomial(c(0,0,0,1))
extremes(p)

## -----------------------------------------------------------------------------
# Sample usage of nearest_arg
valid_colors <- c("red", "blue", "green", "yellow", "orange")

# Input color names with potential typos
input_colors <- c("rad", "blu", "grien", "yello", "ornge")

# Applying nearest_arg to find the closest valid color names
result_colors <- nearest_arg(input_colors, valid_colors)

# Displaying the result
cat("Input Colors:", input_colors)
cat("Nearest Valid Colors:", result_colors)

## -----------------------------------------------------------------------------
# Generate a vector with a unique maximum
vec_unique_max <- c(3, 7, 5, 2, 8, 6, 4)

# Check if vec_unique_max has a unique maximum with the default tolerance (1e-3)
result_default_tol <- unique_max(vec_unique_max)

# Check if vec_unique_max has a unique maximum with a larger tolerance (1)
result_large_tol <- unique_max(vec_unique_max, tol = 1)

# Print the results
cat("Default Tolerance Result:", result_default_tol, "\n")
cat("Large Tolerance Result:", result_large_tol, "\n")

## -----------------------------------------------------------------------------
x <- runif(20)
all_different(x, 1)    # Minimal distance is at least 1
all_different(x, 1e-4) # Minimal distance is at least 0.0001

## -----------------------------------------------------------------------------
# Define functions funa and funb
funb <- function() { calledBy('funa') }
funa <- function() { funb() }

# Call funa and check if it is called by funb
result <- funa()

# Display the result
str(result)

## -----------------------------------------------------------------------------
# Create a new exercise data structure
exer <- exercise()

# Add a parameter 'x' to the exercise data structure
exer <- exercise(exer, x = 3)
str(exer)

## -----------------------------------------------------------------------------
# Example 1: Calculating a solution with default parameters
s <- sol_num(sqrt(2))
str(s)
# Example 2: Numeric solution with tolerance and rounding
sol_num(pi, tol=0.001, digits=3)

## -----------------------------------------------------------------------------
# Example: Creating an integer solution
integer_solution <- sol_int(7.89, tol=0.01, digits=2)
str(integer_solution)

## -----------------------------------------------------------------------------
# Example: Creating a multiple-choice solution for a biology quiz
plants <- c("Moss", "Fern", "Pine", "Rose", "Tulip")
flowering_plants <- c("Rose", "Tulip")
non_flowering_plants <- setdiff(plants, flowering_plants)

s_plants <- sol_mc(non_flowering_plants, flowering_plants, sample=c(2, 2), shuffle=FALSE, none="None of the above")
str(s_plants)

## -----------------------------------------------------------------------------
# Example: Extracting correct answers from a biology quiz
s <- sol_mc(c("Oak", "Maple", "Rose"), c("Tulip", "Sunflower"), sample=c(2, 1), none="No valid options")
sol_ans(s)

## -----------------------------------------------------------------------------
# Example: Extracting True/False solutions from a chemistry quiz
s <- sol_mc(c("Copper", "Silver", "Gold"), c("Oxygen", "Carbon"), sample=c(2, 1), none="None of the above")
sol_tf(s)

## -----------------------------------------------------------------------------
# Example: Displaying Meta-Information for a statistical analysis
stat_analysis <- sol_num(mean(c(5, 8, 12, 15, 18)), tol = 0.01, digits = 2)
info_stat <- sol_info(stat_analysis)
cat(info_stat)

## -----------------------------------------------------------------------------
# Exercise "Bluthochdruck"

alpha <- sample(c(0.01, 0.02, 0.05, 0.1, 0.2), 1)
n     <- sample(5:15, 1)
smean <- 80:160
ssig  <- 1:50
ski   <- sample(smean,1)
sigma <- sample(ssig,1)
a <- ski-sigma
b <- ski+sigma
X <- sample(seq(a,b,1),n,replace=TRUE)
#part a
xBar <- round(mean(X))
s2   <- var(X)
s2   <- round(s2)
s    <- round(sqrt(s2),2)
#part c
c    <- round(qt(1-alpha/2, n-1), 3)
v_u  <- xBar - c * sqrt(s2/n)
v_o  <- xBar + c * sqrt(s2/n)
dig  <- 1-floor(log10((c-qnorm(1-alpha/2))*sqrt(s2/n)))
sc   <- num_result(v_u, digits=dig, tolmult=1)
print(sc)

## -----------------------------------------------------------------------------
makekey(c(3, 7, 10))

## -----------------------------------------------------------------------------
# Modifying a Moodle XML file for multiple-choice questions with multiple correct answers

# Example 1: Using moodle_m2s on a specified file
# Assuming 'my_moodle_file.txt' is the original Moodle XML file
# original_file <- "my_moodle_file.txt"

# Applying moodle_m2s to modify the XML file
# modified_file <- moodle_m2s(original_file)

# Displaying the name of the modified XML file
# cat("Example 1: Modified XML file saved as:", modified_file, "\n")

# Example 2: Using moodle_m2s on a file from the exams.moodle package
# if (interactive()) {
# Creating a temporary file with .xml extension
# newfile <- tempfile(fileext=".xml")
# Using moodle_m2s on the 'klausur-test.xml' file from the exams.forge package
# moodle_m2s(system.file("xml", "klausur-test.xml", package="exams.forge"), newfile=newfile)

# Opening the modified XML file for editing with file.edit(newfile) }

## -----------------------------------------------------------------------------
# Perform spell check on an RMarkdown file, ignoring specific keywords
# spell_result <- spell("path/to/my/file.Rmd")

# Alternatively, perform spell check on multiple files
# spell_result_multiple <- spell(c("path/to/file1.Rmd", "path/to/file2.Rmd"))

# Display the spell check results
# print(spell_result)

## -----------------------------------------------------------------------------
# Call catif with TRUE condition
catif(TRUE, "PDF")      

# Call catif with FALSE condition
catif(FALSE, "Moodle")  # There is no output with this condition

## -----------------------------------------------------------------------------
original_strings <- c("Hello, World!", "<script>alert('Danger!');</script>", "1234567890")
# Applying nosanitize to preserve original strings
unsanitized_strings <- nosanitize(original_strings)
print(unsanitized_strings)

## ----test---------------------------------------------------------------------
# Example 1
x3 <- c((0:16)/8, 1/3)
fcvt(x3)

# Example 2
fcvt(x3, denom=0)

# Example 3
fcvt(x3, denom=1)

# Example 4
fcvt(x3, denom=8)

## -----------------------------------------------------------------------------
x <- 1
str(num2str(x))
y <- 2
str(num2str(x, y))
str(num2str(x, y, z=c(x,y)))

## -----------------------------------------------------------------------------
random_values <- runif(5)
new_value <- affix(random_values, prefix = "$", suffix = "$")

## -----------------------------------------------------------------------------
random_numbers <- c("$15.3", "$7.9", "$22.6")
new_numbers <- unaffix(random_numbers, prefix = "$", suffix = "")

## -----------------------------------------------------------------------------
new_data <- c(5.5, 12.3, 8.9)
cdata_representation <- cdata(new_data)

## -----------------------------------------------------------------------------
cdata_numbers <- c("<![CDATA[30.5]]>", "<![CDATA[18.2]]>", "<![CDATA[45.7]]>")
new_numbers <- uncdata(cdata_numbers)

## -----------------------------------------------------------------------------
existing_values <- c(10, 20, 30)
new_values <- bracket(existing_values)

## -----------------------------------------------------------------------------
numeric_vector <- c(3.14, 2.718, 1.618)
math_representation <- math(numeric_vector)

## -----------------------------------------------------------------------------
quoted_values <- c("\"42.0\"", "\"8.8\"", "\"16.5\"")
unquoted_values <- unquote(quoted_values)

## -----------------------------------------------------------------------------
# Generate breaks for a random normal distribution
x <- rnorm(100, mean = 1.8, sd = 0.1)
breaks(x)

# Generate breaks with specified width for the same distribution
breaks(x, 0.1)

# Generate quantile-based breaks with specified width for the distribution
breaks(x, 0.1, probs = 4)

## -----------------------------------------------------------------------------
x <- round(runif(5), 2)
as_fraction(x)
as_fraction(x, latex = TRUE)

## -----------------------------------------------------------------------------
# Taken from the exercise "Niederschlag"
smean <- 250:350
ssig  <- 1:10
ski   <- sample(smean, 1)
sigma <- sample(ssig, 1)
a <- ski-sigma
b <- ski+sigma
repeat{
  X    <- sample(seq(a,b,1),5,replace=TRUE)
  xbar <- sum(X)/5
  if (abs(xbar-round(xbar))<1e-3) break
}
#part a
sumSize = sum(X)
xBar <- round(xbar,2)
S2   <- round(var(X), 2)
sx   <- as_obs(X, last=" und ")

## -----------------------------------------------------------------------------
# Taken from the exercise "Dart 2"
fields  <- c(6, 13, 4, 18, 1, 20, 5, 12, 9, 14, 11, 8, 16, 7, 19, 3, 17, 2, 15, 10)
N       <- 82
ind     <- sort(sample(20, 2))
mname   <- paste0("eines der Felder, die zu den Nummern ", as_string(fields[ind[1]:ind[2]], last=" oder "), " gehören")
print(mname)

## -----------------------------------------------------------------------------
x <- round(runif(5), 2)
as_sum(x)

## -----------------------------------------------------------------------------
# Execute 4 function calls: sum(1,3,5:6), sum(1,4,5:6), ..., sum(2,4,5:6)
gapply("sum", 1:2, 3:4, I(5:6))

## -----------------------------------------------------------------------------
# Formatting numeric values with a list specifying precision for each variable, overriding y's precision to 0
result1 <- replace_fmt("\\frac{x}{y}", x = 2, y = 3, digits = list(2, y = 0))

# Formatting LaTeX expressions as strings
result1 <- replace_fmt("\\frac{x}{y}", x = "\\\\sum_{i=1}^n x_i", y = "\\\\sum_{i=1}^n y_i")

## -----------------------------------------------------------------------------
# Set the number of answer columns to 2 in the LaTeX document
answercol(2)

## -----------------------------------------------------------------------------
hypothesis_latex("\\mu", alternative=c("eq", "ne", "lt", "le", "gt", "ge"),
                         null=c("eq", "ne", "lt", "le", "gt", "ge"))

## -----------------------------------------------------------------------------
latexdef("myvariable", "42")

## -----------------------------------------------------------------------------
# Taken from the exercise "Constant_Density"
ops   <- c("\\leq", "<", "\\geq", ">")
sym   <- sample(1:2, size=2, replace=TRUE)
dens  <- pdensity(-5:5, size=4, power=0)
xdens <- toLatex(dens$pcoeff, digits=FALSE)
tdens <- toLatex(dens$pcoeff, digits=FALSE, variable="t")
tdist <- toLatex(integral(dens$pcoeff), digits=FALSE, variable="t")
str(dens)
print(tdist)

## -----------------------------------------------------------------------------
# Example: Generating HTML or LaTeX representation based on context
matrix_example <- html_matrix(matrix(1:4, nrow = 2))
result <- toHTMLorLatex(matrix_example)
str(result)

## -----------------------------------------------------------------------------
lsumprod(-2:2, (1:5)/10)

## -----------------------------------------------------------------------------
lsum(-2:2)

## -----------------------------------------------------------------------------
lprod(-3:2)

## -----------------------------------------------------------------------------
lmean(-2:2)

## -----------------------------------------------------------------------------
lvar(1:5)

## -----------------------------------------------------------------------------
 lbr(-2:2)

## -----------------------------------------------------------------------------
lsgn(-3:1)

## -----------------------------------------------------------------------------
# Using lvec to create a LaTeX representation of a vector with square brackets
# lvec(c(1, 2, 3), left = "[", right = "]")

# Using lvec to create a LaTeX representation of a vector with angle brackets and custom collapse
# lvec(c("a", "b", "c"), left = "<", collapse = " \\cdot ")

## -----------------------------------------------------------------------------
# Example: Solving a Genetics Problem
# Consider two genes A and B with the following probabilities:
# P(A) = 0.6, P(B) = 0.4
# P(A|B) = 0.3, P(B|A) = 0.2

# Compute the probability of having both genes A and B (A^B)
result_genetics <- prob_solve("A^B", "A" = 0.6, "B" = 0.4, "A|B" = 0.3, "B|A" = 0.2)

# Print the result
print(result_genetics)

## -----------------------------------------------------------------------------
# Example: Probability Expression Transformation

# Suppose we have a probability expression in a format using ^ and !:
expression <- "!A^B"

# Apply the lprob function to transform the expression
transformed_expression <- lprob(expression)

# Print the original and transformed expressions
cat("Original expression:", expression, "\n")
cat("Transformed expression:", transformed_expression, "\n")

## -----------------------------------------------------------------------------
result <- inline("2 + 2")
cat("The result of the calculation is:", result, "\n")

## -----------------------------------------------------------------------------
rateperhour <- sample(10:25, 1)
rate    <- rateperhour/60
sec     <- 60/rate
d       <- distribution("exp", rate=rate)
number  <- rateperhour
length  <- 60
lambda  <- rate
rvt     <- rv("T", "Wartezeit in Minuten auf den nächsten Wähler")
str(rvt)

## -----------------------------------------------------------------------------
# Example: Creating a dynamic template with embedded R code
tmpl <- "The sum of `r a` and `r b` is: `r a + b`"
result <- template(tmpl, a = 1, b = 2)
cat(result)

## -----------------------------------------------------------------------------
# subset of variables we use, variable names are in German  
data("skalenniveau")
skalen <- c("nominal", "ordinal", "metrisch")
stopifnot(all(skalenniveau$type %in% skalen))  # protect against typos
skala  <- sample(skalenniveau$type, 1)
exvars <- sample(nrow(skalenniveau), 8)
tf     <- (skalenniveau$type[exvars]==skala)
sc     <- to_choice(skalenniveau$name[exvars], tf)
# Additional answer: Does none fit?
sc$questions <- c(sc$questions, "Keine der Variablen hat das gewünschte Skalenniveau") 
sc$solutions <- c(sc$solutions, !any(tf))                                              
sc

## -----------------------------------------------------------------------------
# Subset of variables we use, variable names are in German  
data("skalenniveau")  
skalen <- c("nominal", "ordinal", "metrisch")
skala  <- sample(skalenniveau$type, 1)
exvars <- sample(nrow(skalenniveau), 8)
tf     <- (skalenniveau$type[exvars]==skala)
# select one true and four false answers
sc     <- to_choice(skalenniveau$name[exvars], tf, shuffle=c(1,4))
sc

## -----------------------------------------------------------------------------
# if (interactive()) {
# Read XML data from an RDS file
# resexams <- readRDS(system.file("xml", "klausur-test.rds", package="exams.forge"))
  
# Create and display HTML page
# html_e2m(resexams) # Opens HTML file in the browser}

## -----------------------------------------------------------------------------
# html_matrix_sk(m) 
#   tooltip(sprintf(tooltip, nrow(m), ncol(m)))
#   hm_cell(fmt=fmt, byrow=byrow)

## -----------------------------------------------------------------------------
# Create a matrix
m <- matrix(1:6, ncol=2)

# Generate and display an html_matrix object
html_matrix_sk(m, title="", fmt=c("%.0f", "%.1f"))

# Another small example taken from the exercise "Mobil Telephone 2"
 a  <- runif(4)
  pa <- ddiscrete(a)
  b  <- dpois(0:3, 1)
  pb <- ddiscrete(b)
  studie <- cbind(pa, pb)
hstudie <- html_matrix_sk(studie, "Studie / $x$", fmt=rep("%3.1f", 2))
print(hstudie)

## -----------------------------------------------------------------------------
library("magrittr")
x  <- matrix(1:12, ncol=3)
hm <- html_matrix(x)
toHTML(hm)

# hm <- html_matrix(x) %>% zebra() %>% 
# sprintf("Table has %.0f rows and %.0f columns", nrow(.), ncol(.))
# toHTML(hm)

## -----------------------------------------------------------------------------
firstmatch("d", c("chisq", "cauchy"))
firstmatch("c", c("chisq", "cauchy"))
firstmatch("ca", c("chisq", "cauchy"))

## -----------------------------------------------------------------------------
# Execute three t-test calls: t.test(x, -1), t.test(x, 0), t.test(x, 1)
ga <- gapply(t.test, x = I(rnorm(100)), mu = -1:1)

# No simplification occurs in this case since `data.name` and `conf.int` have lengths larger than one
str(gsimplify(ga))

## -----------------------------------------------------------------------------
x <- runif(100)
correct <- ttest_num(x=x, mu0=0.5, sigma=sqrt(1/12))
str(correct)

## -----------------------------------------------------------------------------
res <- hyperloop(ttest_num, 
                 n           = list(1, correct$n, correct$n+1),
                 mu0         = list(correct$mu0, correct$mean),
                 mean        = list(correct$mu0, correct$mean), 
                 sigma       = list(correct$sigma, correct$sd, sqrt(correct$sigma), sqrt(correct$sd)),
                 sd          = list(correct$sigma, correct$sd, sqrt(correct$sigma), sqrt(correct$sd)),
                 norm        = list(TRUE, FALSE)
                )
# extract all unique test statistics
stat <- unlist(unique_elem(res, "statistic"))
# select 7 wrong test statistic such that the difference 
# between all possible test statistics is at least 0.01
repeat {
  sc <- to_choice(stat, stat==correct$statistic, shuffle=c(1,7))
  if (all_different(sc$questions, 0.005)) break
}
# show possible results for a MC questions
sc$questions
sc$solutions

## -----------------------------------------------------------------------------
knitif(runif(1) < 0.5, 'TRUE' = "`r pi`", 'FALSE' = "$\\pi=`r pi`$")

## -----------------------------------------------------------------------------
substring(now(), 10)

## -----------------------------------------------------------------------------
# Example taken from the exercise "DSL 4"
repeat {
  border  <- sample(3:10, 1)-1
  lambda  <- sample(seq(0.5, 6, by=0.1), 1)
  if (ppois(border, lambda = lambda)>1e-3) break
}
d       <- distribution("pois", lambda=lambda)
ptype   <- "less"
sc      <- num_result(cdf(d, border), 4)
txt     <- nsprintf(border, "%i Netzunterbrechungen",
                    '0'="keine Netzunterbrechung",
                    '1'="eine Netzunterbrechung")
str(txt)

## -----------------------------------------------------------------------------
image_file <- "example_image.jpg"

# Retrieve MIME type for the given image file
mime_type <- mime_image(image_file)

# Display the result
cat("MIME Type for", image_file, ":", mime_type, "\n")

