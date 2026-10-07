# ------------------------------------------------------------------------------------
# variable: Red bucket (TTFF)
# ------------------------------------------------------------------------------------


# -------------------------------------- 1 -----------------------------------------------
# For your variable, verify whether professional experience matters.
# State the null and alternative hypotheses. Draw conclusions based on the test result at the significance level α = 0.05.

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

# --- 1. Load experience from the sheets
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

# --- 2. Load your variable.
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

# --- 3. Merge the experience and your variable data frames by participant number.
merged_data <- inner_join(
    experience,
    ttff,
    by = c("participant.nr" = "Participant")
  )

# --- 4. Verify normality of the distributions in the groups
shapiro_full <- merged_data %>%
  group_by(experience) %>%
  shapiro_test(R.bucket)

# View the results
shapiro_full

# NO         R.bucket     P= 2.00e- 7 <= 0.05
# YES        R.bucket     P= 5.01e-11 <=0.05
# Reject H₀ -> The distribution in both groups is not normal

# --- 5. Verify homogeneity of variances
levene_result <- merged_data %>%
  levene_test(R.bucket ~ experience)

# View the results
levene_result

#P=0.689 >=0.05
# Homogeneity of variances is satisfied

# --- 6. Choose the appropriate test to compare the location of two groups - experienced and inexperienced people
# If the normality assumption is NOT met, use the Mann-Whitney U test - my variable is not normally distributed:
wilcox.test(rat_weight_gain ~ diet_amount, data = df_rats)

# --- 7. State the hypotheses, perform the test, and formulate conclusions
# Hypotheses
# Null hypothesis (H0): The location of the TTFF distributions (e.g. median) for the R.bucket object is the same for people with professional experience and without experience.
# Alternative hypothesis (H1): The location of the TTFF distributions differs significantly between people with professional experience and people without professional experience.

test_experience <- wilcox.test(
  R.bucket ~ experience,
  data      = merged_data,
)

# Display the result
print(test_experience) #p=0.1735 >=0.05

# We do not have grounds to reject H₀ -> no statistically significant differences were found in the distributions of TTFF values for the R bucket object
# between participants with professional experience and those without experience
