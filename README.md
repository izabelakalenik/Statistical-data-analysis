# Statistical Analysis of Goggles Color Experiment

This project contains the R scripts for a statistical analysis of eye-tracking data collected in an experiment on the influence of goggles frame color on visual behavior during a construction-site safety inspection task.

The study compares three groups of participants wearing:

- transparent goggles (`T`)
- red goggles (`R`)
- yellow goggles (`Y`)

## Repository contents

- `part_1.R` - descriptive statistics, histograms, and boxplots for demographic variables and task results
- `part_2.R` - normality verification for the assigned variable and normalizing transformations
- `part_3.R` - QQ-plots, variance equality testing, and chi-square test for group balance
- `part_4.R` - verification of the effect of professional experience on the assigned variable
- `part_5.R` - ANOVA assumptions, test selection, and robust/non-parametric alternatives

## Data file

The scripts expect the ODS data file:

`ALL_DATA_TRANSPARENT_YELLOW_RED GOGLES_ON_CONSTRUCTION_SITE.ods`

Place it in the same folder as the R scripts, or update the file paths inside the scripts if needed.

## Requirements

- R 4.x
- Packages used across the scripts:
  - `readODS`
  - `dplyr`
  - `tidyr`
  - `rstatix`
  - `ggpubr`
  - `WRS2` (used in `part_5.R`)

If a package is missing, the scripts will attempt to install it automatically.

## How to run

You can run the scripts individually in R or RStudio.

Example from the command line:

```bash
Rscript part_1.R
Rscript part_2.R
Rscript part_3.R
Rscript part_4.R
Rscript part_5.R
```

## Outputs

The scripts generate summary tables and graphics such as:

- histograms
- boxplots
- QQ-plots
- hypothesis test results



