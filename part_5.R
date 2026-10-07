# ------------------------------------------------------------------------------------
# variable: Red bucket (TTFF)
# ------------------------------------------------------------------------------------

# -------------------------------------- 1 -----------------------------------------------
# For your variable X and object Y, check the assumptions of the ANOVA test,
# choose the appropriate variant of the test, and perform it. Interpret the results.

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

# Sheet and file names
exp_sheets <- c("T_EXPERIENCE", "R_EXPERIENCE", "Y_EXPERIENCE")
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

# Load TTFF data
ttff <- read_and_combine_ods_sheets(file_path, var_sheets)

# Map sheet to goggles color
goggles_map <- setNames(goggle_cols, var_sheets)
# Create the goggles variable with the goggles color
ttff <- ttff %>%
  mutate(goggles = recode(source_sheet, !!!goggles_map))

# Convert names to "safe" ones, i.e. replace spaces with dots
names(ttff) <- make.names(names(ttff))

# Load data
experience <- read_and_combine_ods_sheets(file_path, exp_sheets)

# Rename the experience variable to a shorter name and convert it to a categorical variable (factor)
experience <- experience  %>%
  rename(
    experience = `Do you have any professional experience beyond intership?`,
  )  %>%
  mutate(
    experience = as.factor(experience)
  )
# Convert names to "safe" ones, i.e. replace spaces with dots
names(experience) <- make.names(names(experience))

# Merge the data
merged_data <- inner_join(
    experience,
    ttff,
    by = c("participant.nr" = "Participant")
  )

#----------------------

# Kruskal-Wallis test
kruskal_test <- kruskal_test(ttff, R.bucket ~ goggles)
print(kruskal_test)

# Dunn post-hoc tests
dunn_test <- dunn_test(ttff, R.bucket ~ goggles)
print(dunn_test)

# Post-hoc tests
robust_posthoc <- lincon(R.bucket ~ goggles, data = ttff)
print(robust_posthoc)

# If you do not have the package:
install.packages("WRS2")

# Robust ANOVA test
WRS2::t1way(R.bucket ~ goggles, data = ttff)

# Resistant post-hoc tests
WRS2::lincon(R.bucket ~ goggles, data = ttff)