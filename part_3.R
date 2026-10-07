# ------------------------------------------------------------------------------------
# variable: Red bucket (TTFF)
# ------------------------------------------------------------------------------------


# -------------------------------------- 1 -----------------------------------------------
# For your variable, check normality in all three groups using a quantile-quantile plot.
# Place all three plots on one canvas.

# Required packages
load_or_install <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE)
}

load_or_install("readODS")
load_or_install("dplyr")
load_or_install("tidyr")
load_or_install("rstatix")
load_or_install("ggpubr")

# Sheet and file names
var_sheets <- c(
  "TTFF_T_Time_to_First_Fixation",
  "TTFF_R_Time_to_First_Fixation",
  "TTFF_Y_Time_to_First_Fixation_"
)
# Goggles colors, in the order resulting from the sheets, i.e. var_sheets
goggle_cols <- c("Transparent", "Red", "Yellow")
file_path <- "./ALL_DATA_TRANSPARENT_YELLOW_RED GOGLES_ON_CONSTRUCTION_SITE.ods"

# Function for reading and combining sheets
read_and_combine_ods_sheets <- function(file_path, sheets, skip_rows = 1, include_source = TRUE) {
  all_data <- lapply(sheets, function(sheet) {
    read_ods(file_path, sheet = sheet, skip = skip_rows)
  })
  names(all_data) <- sheets
  if (include_source) {
    bind_rows(all_data, .id = "source_sheet")
  } else {
    bind_rows(all_data)
  }
}

# Load data
ttff <- read_and_combine_ods_sheets(file_path, var_sheets)

# Map sheet to goggles color - create a dictionary
goggles_map <- setNames(goggle_cols, var_sheets)
# Create the goggles variable with the goggles color
ttff <- ttff %>%
  mutate(goggles = recode(source_sheet, !!!goggles_map))

# Convert names to "safe" ones, i.e. replace spaces with dots
names(ttff) <- make.names(names(ttff))

# Quantile-quantile plot
goggle_colors <- c(
  "Transparent" = "skyblue",
  "Red" = "firebrick",
  "Yellow" = "goldenrod"
)

# The + operator adds additional layers to the plot
ggqqplot(
  ttff,
  x = "R.bucket",
  facet.by = "goggles",
  title="TTFF for the R bucket variable",
  color="goggles",
) +
  scale_color_manual(values = goggle_colors) +
  scale_fill_manual(values = goggle_colors)

# -------------------------------------- 2 -----------------------------------------------
# For your variable, check the equality of variances across the groups. State the null and alternative hypotheses.
# Choose the appropriate test based on the number of groups and the normality of the data. Draw conclusions based on the test result.

# Hypotheses:
#  H₀ (null hypothesis): The TTFF variances for R.bucket are equal across all three goggles groups.
#  H₁ (alternative hypothesis): At least one pair of groups has different TTFF variances for R.bucket.
# Test to use:
#  Since the data come from three groups and not all variables are normally distributed,
#  it is best to use the Fligner-Killeen test, which does not require normality.

library(rstatix)
fligner_result <- fligner.test(R.bucket ~ goggles, data = ttff)
print(fligner_result)
# Bartlett's K-squared = 0.49663, df = 2, p-value = 0.7801 >= 0.05 -> We do not have sufficient evidence to reject the null hypothesis

# For confirmation
bartlett.test(R.bucket ~ goggles, data = ttff) # Sensitive to deviations from normality
ttff %>% levene_test(R.bucket ~ goggles, center = "mean") # Robust to lack of normality
ttff %>% levene_test(R.bucket ~ goggles, center = "median") # Robust to lack of normality
# The same conclusion as above


# -------------------------------------- 3 -----------------------------------------------
# For your variable, check whether the number of observations is equal across all three groups using the chi-square test at the significance level
# α = 0.05. State the null and alternative hypotheses. Draw conclusions based on the test result.

# Hypotheses:
#  H₀: The number of observations in the Transparent, Red, and Yellow groups is the same (equally distributed).
#  H₁: The number of observations is not equal across these three groups.

# Number of observations in each group
table(ttff$goggles)

# Chi-square test - classical
chisq.test(table(ttff$goggles))

# p-value=0.926 >= 0.05, so we do not reject the null hypothesis
