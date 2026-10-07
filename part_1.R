#---------------------------------------------------------------------
# For each of the variables TIME, SEX, AGE, H&STEST_RESULTS, EXPERIENCE
# and the T, R and Y groups (transparent, red and yellow):
#---------------------------------------------------------------------

library("readODS")

T_TIME <- read_ods(
   "./ALL_DATA_TRANSPARENT_YELLOW_RED GOGLES_ON_CONSTRUCTION_SITE.ods",
  sheet = "T_TIME",
  skip = 1,
)

R_TIME <- read_ods(
  "./ALL_DATA_TRANSPARENT_YELLOW_RED GOGLES_ON_CONSTRUCTION_SITE.ods",
  sheet = "R_TIME",
  skip = 1,
)

Y_TIME <- read_ods(
  "./ALL_DATA_TRANSPARENT_YELLOW_RED GOGLES_ON_CONSTRUCTION_SITE.ods",
  sheet = "Y_TIME",
  skip = 1,
)

T_SEX <- read_ods(
  "./ALL_DATA_TRANSPARENT_YELLOW_RED GOGLES_ON_CONSTRUCTION_SITE.ods",
  sheet = "T_SEX",
  skip = 1,
)

R_SEX <- read_ods(
  "./ALL_DATA_TRANSPARENT_YELLOW_RED GOGLES_ON_CONSTRUCTION_SITE.ods",
  sheet = "R_SEX",
  skip = 1,
)

Y_SEX <- read_ods(
  "./ALL_DATA_TRANSPARENT_YELLOW_RED GOGLES_ON_CONSTRUCTION_SITE.ods",
  sheet = "Y_SEX",
  skip = 1,
)

T_AGE <- read_ods(
  "./ALL_DATA_TRANSPARENT_YELLOW_RED GOGLES_ON_CONSTRUCTION_SITE.ods",
  sheet = "T_AGE",
  skip = 1,
)

R_AGE <- read_ods(
  "./ALL_DATA_TRANSPARENT_YELLOW_RED GOGLES_ON_CONSTRUCTION_SITE.ods",
  sheet = "R_AGE",
  skip = 1,
)

Y_AGE <- read_ods(
  "./ALL_DATA_TRANSPARENT_YELLOW_RED GOGLES_ON_CONSTRUCTION_SITE.ods",
  sheet = "Y_AGE",
  skip = 1,
)

T_HSTEST_RESULTS <- read_ods(
  "./ALL_DATA_TRANSPARENT_YELLOW_RED GOGLES_ON_CONSTRUCTION_SITE.ods",
  sheet = "T_H&STEST_RESULTS",
  skip = 1,
)

R_HSTEST_RESULTS <- read_ods(
  "./ALL_DATA_TRANSPARENT_YELLOW_RED GOGLES_ON_CONSTRUCTION_SITE.ods",
  sheet = "R_H&STEST_RESULTS",
  skip = 1,
)

Y_HSTEST_RESULTS <- read_ods(
  "./ALL_DATA_TRANSPARENT_YELLOW_RED GOGLES_ON_CONSTRUCTION_SITE.ods",
  sheet = "Y_H&STEST_RESULTS",
  skip = 1,
)

T_EXPERIENCE <- read_ods(
  "./ALL_DATA_TRANSPARENT_YELLOW_RED GOGLES_ON_CONSTRUCTION_SITE.ods",
  sheet = "T_EXPERIENCE",
  skip = 1,
)

R_EXPERIENCE <- read_ods(
  "./ALL_DATA_TRANSPARENT_YELLOW_RED GOGLES_ON_CONSTRUCTION_SITE.ods",
  sheet = "R_EXPERIENCE",
  skip = 1,
)

Y_EXPERIENCE <- read_ods(
  "./ALL_DATA_TRANSPARENT_YELLOW_RED GOGLES_ON_CONSTRUCTION_SITE.ods",
  sheet = "Y_EXPERIENCE",
  skip = 1,
)

# -------------------------------------- 1 ----------------------------------------------
# Determine descriptive statistics from the sample (for each variable and each group):
# mean, standard deviation, median, first and third quartile, minimum and maximum value.

summary(T_TIME$`T task [s]`)
sd(T_TIME$`T task [s]`)
summary(R_TIME$`R [s]`)
sd(R_TIME$`R [s]`)
summary(Y_TIME$`Y [s]`)
sd(Y_TIME$`Y [s]`)

table(T_SEX$`M / F / O`)  # Transparent goggles - sex
table(R_SEX$`M / F / O`)  # Red goggles - sex
table(Y_SEX$`M / F / O`)  # Yellow goggles - sex

summary(T_AGE$`AGE [y]`)
sd(T_AGE$`AGE [y]`)
summary(R_AGE$`AGE [y]`)
sd(R_AGE$`AGE [y]`)
summary(Y_AGE$`AGE [y]`)
sd(Y_AGE$`AGE [y]`)

summary(T_HSTEST_RESULTS$`TEST 0-10 points`)
sd(T_HSTEST_RESULTS$`TEST 0-10 points`)
summary(R_HSTEST_RESULTS$`TEST 0-10 points`)
sd(R_HSTEST_RESULTS$`TEST 0-10 points`)
summary(Y_HSTEST_RESULTS$`TEST 0-10 points`)
sd(Y_HSTEST_RESULTS$`TEST 0-10 points`)

table(T_EXPERIENCE$`Do you have any professional experience beyond internship?`)
table(R_EXPERIENCE$`Do you have any professional experience beyond internship?`)
table(Y_EXPERIENCE$`Do you have any professional experience beyond internship?`)




# -------------------------------------- 2 ----------------------------------------------
# Draw histograms on a 1x3 canvas (in the first column — the transparent group histogram,
# in the second — the red group, in the third — the yellow group).

png("time_histograms.png", width = 1000, height = 400)
time_layout <- matrix(c(1,2,3), nrow=1, ncol=3, byrow = TRUE)
layout(time_layout)

# Histogram of variable `T task [s]`
hist(T_TIME$`T task [s]`,
     main = "Histogram of T_TIME",
     xlab = "Time wearing transparent goggles [s]",
     ylab = "Frequency",
     col = "gray",  # Bar color
     col.main = "black",
     ylim = c(0, 20),
     xlim = c(360,700))  # Title color

# Histogram of variable `R [s]`
hist(R_TIME$`R [s]`,
     main = "Histogram of R_TIME",
     xlab = "Time wearing red goggles [s]",
     ylab = "Frequency",
     col = "red",  # Bar color
     col.main = "black",
     ylim = c(0, 20),
     xlim = c(360,700))  # Title color

# Histogram of variable `Y [s]`
hist(Y_TIME$`Y [s]`,
     main = "Histogram of Y_TIME",
     xlab = "Time wearing yellow goggles [s]",
     ylab = "Frequency",
     col = "yellow",  # Bar color
     col.main = "black",
     ylim = c(0, 20),
     xlim = c(360,700))  # Title color

dev.off()

png("sex_histograms.png", width = 1000, height = 400)
time_layout <- matrix(c(1,2,3), nrow=1, ncol=3, byrow = TRUE)
layout(time_layout)

# Transparent goggles
barplot(table(T_SEX$`M / F / O`),
        main = "Histogram of T_SEX",
        xlab = "Sex",
        ylab = "Count",
        col = "gray",
        ylim = c(0, 40))

# Red goggles
barplot(table(R_SEX$`M / F / O`),
        main = "Histogram of R_SEX",
        xlab = "Sex",
        ylab = "Count",
        col = "red",
        ylim = c(0, 40))

# Yellow goggles
barplot(table(Y_SEX$`M / F / O`),
        main = "Histogram of Y_SEX",
        xlab = "Sex",
        ylab = "Count",
        col = "yellow",
        ylim = c(0, 40))

dev.off()

png("age_histograms.png", width = 1000, height = 400)
time_layout <- matrix(c(1,2,3), nrow=1, ncol=3, byrow = TRUE)
layout(time_layout)

hist(T_AGE$`AGE [y]`, 
     main = "Histogram of T_AGE",
     xlab = "Age",
     ylab = "Count",
     col = "gray",
     col.main = "black",
     ylim = c(0, 40),
     xlim = c(10,70))

hist(R_AGE$`AGE [y]`, 
     main = "Histogram of R_AGE",
     xlab = "Age",
     ylab = "Count",
     col = "red",
     col.main = "black",
     ylim = c(0, 40),
     xlim = c(10,70))

hist(Y_AGE$`AGE [y]`, 
     main = "Histogram of Y_AGE",
     xlab = "Age",
     ylab = "Count",
     col = "yellow",
     col.main = "black",
     ylim = c(0, 40),
     xlim = c(10,70))

dev.off()

png("hs_test_results_histograms.png", width = 1000, height = 400)
time_layout <- matrix(c(1,2,3), nrow=1, ncol=3, byrow = TRUE)
layout(time_layout)

hist(T_HSTEST_RESULTS$`TEST 0-10 points`, 
     main = "Histogram of T_H&STEST_RESULTS",
     xlab = "Score obtained [0-10]",
     ylab = "Count",
     col = "gray",
     col.main = "black",
     xlim = c(0, 10),
     breaks = seq(0, 10, by = 1),
     ylim = c(0,25))

hist(R_HSTEST_RESULTS$`TEST 0-10 points`, 
     main = "Histogram of R_H&STEST_RESULTS",
     xlab = "Score obtained [0-10]",
     ylab = "Count",
     col = "red",
     col.main = "black",
     xlim = c(0, 10),
     breaks = seq(0, 10, by = 1),
     ylim = c(0,25))

hist(Y_HSTEST_RESULTS$`TEST 0-10 points`, 
     main = "Histogram of Y_H&STEST_RESULTS",
     xlab = "Score obtained [0-10]",
     ylab = "Count",
     col = "yellow",
     col.main = "black",
     xlim = c(0, 10),
     breaks = seq(0, 10, by = 1),
     ylim = c(0,25))

dev.off()

png("experience_histograms.png", width = 1000, height = 400)
time_layout <- matrix(c(1,2,3), nrow=1, ncol=3, byrow = TRUE)
layout(time_layout)

# Transparent goggles
barplot(table(T_EXPERIENCE$`Do you have any professional experience beyond intership?`),
        main = "Histogram of T_EXPERIENCE",
        xlab = "Has professional experience beyond internship",
        ylab = "Count",
        col = "gray",
        ylim = c(0, 40))

# Red goggles
barplot(table(R_EXPERIENCE$`Do you have any professional experience beyond intership?`),
        main = "Histogram of R_EXPERIENCE",
        xlab = "Has professional experience beyond internship",
        ylab = "Count",
        col = "red",
        ylim = c(0, 40))

# Yellow goggles
barplot(table(Y_EXPERIENCE$`Do you have any professional experience beyond intership?`),
        main = "Histogram of Y_EXPERIENCE",
        xlab = "Has professional experience beyond internship",
        ylab = "Count",
        col = "yellow",
        ylim = c(0, 40))

dev.off()



# -------------------------------------- 3 -----------------------------------------------
# For numerical variables, draw box-and-whisker plots on a 1x3 canvas
# (in the first column — the transparent group histogram, in the second — the red group,
# in the third — the yellow group).

png("time_boxplots.png", width = 1000, height = 400)
vertical_layout <- matrix(c(1,2,3), nrow=1, ncol=3, byrow = TRUE)
layout(vertical_layout)

boxplot(T_TIME$`T task [s]`, 
        horizontal = TRUE, 
        col = "gray", 
        xlab = "Time wearing transparent goggles [s]",
        ylim = c(350, 700))


boxplot(R_TIME$`R [s]`,  
        horizontal = TRUE, 
        col = "red", 
        xlab = "Time wearing red goggles [s]",
        ylim = c(350, 700))


boxplot(Y_TIME$`Y [s]`,  
        horizontal = TRUE, 
        col = "yellow", 
        xlab = "Time wearing yellow goggles [s]",
        ylim = c(350, 700))


dev.off()

png("age_boxplots.png", width = 1000, height = 400)
vertical_layout <- matrix(c(1,2,3), nrow=1, ncol=3, byrow = TRUE)
layout(vertical_layout)


boxplot(T_AGE$`AGE [y]`, 
        horizontal = TRUE, 
        col = "gray", 
        xlab = "Age",
        ylim = c(15,65))


boxplot(R_AGE$`AGE [y]`,  
        horizontal = TRUE, 
        col = "red", 
        xlab = "Age",
        ylim = c(15,65))


boxplot(Y_AGE$`AGE [y]`,  
        horizontal = TRUE, 
        col = "yellow", 
        xlab = "Age",
        ylim = c(15,65))


dev.off()

png("hs_test_results_boxplots.png", width = 1000, height = 400)
vertical_layout <- matrix(c(1,2,3), nrow=1, ncol=3, byrow = TRUE)
layout(vertical_layout)


boxplot(T_HSTEST_RESULTS$`TEST 0-10 points`, 
        horizontal = TRUE, 
        col = "gray", 
        xlab = "Test score [0-10]",
        ylim = c(7,10))


boxplot(R_HSTEST_RESULTS$`TEST 0-10 points`,  
        horizontal = TRUE, 
        col = "red", 
        xlab = "Test score [0-10]",
        ylim = c(7,10))


boxplot(Y_HSTEST_RESULTS$`TEST 0-10 points`,  
        horizontal = TRUE, 
        col = "yellow", 
        xlab = "Test score [0-10]",
        ylim = c(7,10))


dev.off()

