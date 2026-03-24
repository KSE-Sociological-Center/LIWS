## ============================================================
## IT COULD HAVE BEEN WORSE
## ============================================================

# ============================================================
# 0. NECESSARY LIBRARIES
# ============================================================

library(haven)
library(rstatix)


# ============================================================
# 1. DATA LOADING AND PREPARATION
# ============================================================

# Set your working directory to the folder containing the data files, e.g.:
# setwd("C:/path/to/your/data")

data_w1 <- read_sav("LIWS_wave_1.sav")
data_w2 <- read_sav("LIWS_wave_2.sav")

# Panel participants from Wave 1 only (pnlyon == 1)
data_wp1 <- data_w1[data_w1$pnlyon == 1, ]

# --- War trauma index (Wave 2): 10 items ---

# 8 binary war-exposure items (columns 60–67 of data_w2):
#   income reduction, job loss, physical health deterioration,
#   mental health deterioration, family separation, housing damage,
#   property loss, injury to self/family

# explst: death of close contacts (0 = no, 1 = yes)
explst <- rep(NA, 595)
explst[data_w2$dwarlst == 4]                        <- 0
explst[data_w2$dwarlst >= 1 & data_w2$dwarlst <= 3] <- 1

# expdis: forced displacement (0 = no, 1 = yes)
expdis <- rep(NA, 595)
expdis[data_w2$chngplr == 1] <- 0
expdis[data_w2$chngplr >  1] <- 1

# Full trauma index: 8 binary items + explst + expdis = 10 items
war_exp <- apply(data_w2[, 60:67], 1, sum) + explst + expdis

# Dichotomised at the mean
warexpo <- rep(NA, 595)
warexpo[war_exp <= mean(war_exp, na.rm = TRUE)] <- "Less traumatised"
warexpo[war_exp >  mean(war_exp, na.rm = TRUE)] <- "More traumatised"


# Long-format panel dataset needed for paired tests
data_panel <- data.frame(
  Wave    = c(rep("Wave 1", 595), rep("Wave 2", 595)),
  stflife = c(data_wp1$stflife,  data_w2$stflife),
  happy   = c(data_wp1$happy,    data_w2$happy),
  ware    = rep(warexpo, 2)
)
data_panel$Wave <- factor(data_panel$Wave, levels = c("Wave 1", "Wave 2"))
data_panel$ware <- factor(data_panel$ware,
                          levels = c("Less traumatised", "More traumatised"))


# ============================================================
# 2. TABLE 1: MEAN LIFE SATISFACTION AND HAPPINESS BY WAVE
# ============================================================

tapply(data_panel$stflife, data_panel$Wave, mean, na.rm = TRUE) |> round(2)
tapply(data_panel$happy,   data_panel$Wave, mean, na.rm = TRUE) |> round(2)

# Paired Wilcoxon tests (Wave 1 vs. Wave 2)
wilcox_test(data_panel, stflife ~ Wave, paired = TRUE)
wilcox_test(data_panel, happy ~ Wave, paired = TRUE)


# ============================================================
# 3. TABLE 2: SIMULTANEOUS CHANGES IN LIFE SATISFACTION AND HAPPINESS
# ============================================================

delta_stflife <- data_w2$stflife - data_wp1$stflife
delta_happy   <- data_w2$happy   - data_wp1$happy

change_cat <- ifelse(delta_stflife > 0  & delta_happy > 0,  "Simultaneous increase in both",
             ifelse(delta_stflife < 0  & delta_happy < 0,  "Simultaneous decrease in both",
             ifelse(delta_stflife > 0  & delta_happy < 0,  "Increased satisfaction, decreased happiness",
             ifelse(delta_stflife < 0  & delta_happy > 0,  "Decreased satisfaction, increased happiness",
             ifelse(delta_stflife == 0 & delta_happy == 0, "No change in either",
             ifelse(delta_stflife > 0  & delta_happy == 0, "Increased satisfaction only",
             ifelse(delta_stflife < 0  & delta_happy == 0, "Decreased satisfaction only",
             ifelse(delta_stflife == 0 & delta_happy > 0,  "Increased happiness only",
             ifelse(delta_stflife == 0 & delta_happy < 0,  "Decreased happiness only",
                    NA)))))))))

table(change_cat) |> prop.table() |> sort(decreasing = T) |> round(3)*100


# ============================================================
# 4. TABLE 3: CHANGES IN WELL-BEING BY TRAUMA EXPOSURE
# ============================================================

round(tapply(delta_stflife, warexpo, mean, na.rm = TRUE), 2)
round(tapply(delta_happy,   warexpo, mean, na.rm = TRUE), 2)

# Between-group comparisons of changes (Less vs. More traumatised) within each wave
wilcox.test(delta_stflife ~ warexpo)
wilcox.test(delta_happy ~ warexpo)

