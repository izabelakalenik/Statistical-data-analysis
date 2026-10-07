# ------------------------------------------------------------------------------------
# Verify the normality of the variable assigned to you,
# both visually and using a statistical test at the significance level α = 0.05.
#
# variable: Red bucket (TTFF)
# ------------------------------------------------------------------------------------

library("readODS")
ttff_t <- read_ods(
  "./ALL_DATA_TRANSPARENT_YELLOW_RED GOGLES_ON_CONSTRUCTION_SITE.ods",
  sheet = "TTFF_T_Time_to_First_Fixation",
  skip = 1,
)

ttff_y <- read_ods(
  "./ALL_DATA_TRANSPARENT_YELLOW_RED GOGLES_ON_CONSTRUCTION_SITE.ods",
  sheet = "TTFF_Y_Time_to_First_Fixation_",
  skip = 1,
)

ttff_r <- read_ods(
  "./ALL_DATA_TRANSPARENT_YELLOW_RED GOGLES_ON_CONSTRUCTION_SITE.ods",
  sheet = "TTFF_R_Time_to_First_Fixation",
  skip = 1,
)

ttff_k <- read_ods(
  "./ALL_DATA_TRANSPARENT_YELLOW_RED GOGLES_ON_CONSTRUCTION_SITE.ods",
  sheet = "TTFF_K_Time_to_First_Fixation",
  skip = 1,
)

rb_t <- ttff_t$'R bucket'
rb_y <- ttff_y$'R bucket'
rb_r <- ttff_r$'R bucket'
rb_k <- ttff_k$'R bucket'

rb_t
rb_y
rb_r
rb_k   # contains "X" values in the column

# Replace "X" with NA and convert to numeric
rb_k_clean <- as.numeric(ifelse(rb_k == "X", NA, rb_k))

# Check whether there are any NAs
summary(rb_k_clean)

# Remove rows with NA
rb_k_clean <- na.omit(rb_k_clean)

# -------------------------------------- 1 -----------------------------------------------
# State the null and alternative hypotheses.

# Histograms
hist(rb_t, main = "Histogram of rb_t", xlab = "Red bucket")
hist(rb_y, main = "Histogram of rb_y", xlab = "Red bucket")
hist(rb_r, main = "Histogram of rb_r", xlab = "Red bucket")
hist(rb_k_clean, main = "Histogram of rb_k_clean", xlab = "Red bucket")

# Statistical test
# α = 0.05

# H_0: The variable follows a normal distribution
# H_1: The variable does not follow a normal distribution

# p-value < alpha --> reject H_0

shapiro.test(rb_t)
shapiro.test(rb_y)
shapiro.test(rb_r)
shapiro.test(rb_k_clean)

# ANSWER: Reject H_0 - the variable does not follow a normal distribution


# -------------------------------------- 2 ----------------------------------------------
# If normality is not met, check whether applying normalizing transformations is helpful.

# Transformations
sqrt_rb_t <- sqrt(rb_t)
fourth_rb_t <- (rb_t)^(1/4)
log_rb_t <- log(rb_t)

sqrt_rb_y <- sqrt(rb_y)
fourth_yb_y <- (rb_y)^(1/4)
log_rb_y <- log(rb_y)

sqrt_rb_r <- sqrt(rb_r)
fourth_rb_r <- (rb_r)^(1/4)
log_rb_r <- log(rb_r)

sqrt_rb_k <- sqrt(rb_k_clean)
fourth_rb_k <- (rb_k_clean)^(1/4)
log_rb_k <- log(rb_k_clean)


# Histograms after transformations
par(mfrow = c(2, 2))
hist(sqrt_rb_t, main = "Square root")
hist(fourth_rb_t, main = "Fourth root")
hist(log_rb_t, main = "Logarithm")

par(mfrow = c(2, 2))
hist(sqrt_rb_y, main = "Square root")
hist(fourth_yb_y, main = "Fourth root")
hist(log_rb_y, main = "Logarithm")

par(mfrow = c(2, 2))
hist(sqrt_rb_r, main = "Square root")
hist(fourth_rb_r, main = "Fourth root")
hist(log_rb_r, main = "Logarithm")

par(mfrow = c(2, 2))
hist(sqrt_rb_k, main = "Square root")
hist(fourth_rb_k, main = "Fourth root")
hist(log_rb_k, main = "Logarithm")

# Statistical test after transformations
shapiro.test(sqrt_rb_t)
shapiro.test(fourth_rb_t)
shapiro.test(log_rb_t)

shapiro.test(sqrt_rb_y)
shapiro.test(fourth_yb_y)
shapiro.test(log_rb_y)

shapiro.test(sqrt_rb_r)
shapiro.test(fourth_rb_r)
shapiro.test(log_rb_r)
# fourth_rb_r, log_rb_r - here the transformations improved the results, but the distribution is still not normal

shapiro.test(sqrt_rb_k)
shapiro.test(fourth_rb_k)
shapiro.test(log_rb_k)
# Here the transformations also improved the results, but the distribution is still not normal

# ANSWER: p-value < alpha --> reject H_0 - the variable does not follow a normal distribution






