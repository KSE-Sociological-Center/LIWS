## ============================================================
## PARADOX OF WELL-BEING IN UKRAINE
## ============================================================

# ============================================================
# 0. NECESSARY LIBRARIES
# ============================================================

library(haven)
library(rstatix)
library(sjPlot)
library(ggplot2)
library(patchwork)


# ============================================================
# 1. DATA LOADING AND PREPARATION
# ============================================================

# Set your working directory to the folder containing the data files, e.g.:
# setwd("C:/path/to/your/data")

data_w1 <- read_sav("LIWS_wave_1.sav")
data_w2 <- read_sav("LIWS_wave_2.sav")

# Panel participants from Wave 1 only (pnlyon == 1)
data_wp1 <- data_w1[data_w1$pnlyon == 1, ]

# --- War experience variables (Wave 2) ---

# explst: loss of someone known (0 = no, 1 = yes)
explst <- rep(NA, 595)
explst[data_w2$dwarlst == 4] <- 0
explst[data_w2$dwarlst >= 1 & data_w2$dwarlst <= 3] <- 1

# expdis: forced displacement (0 = no, 1 = yes)
expdis <- rep(NA, 595)
expdis[data_w2$chngplr == 1] <- 0
expdis[data_w2$chngplr > 1] <- 1

# War exposure index: sum of expdi–expinj (columns 60–67) + explst + expdis
war_exp <- apply(data_w2[, 60:67], 1, sum) + explst + expdis

# Dichotomised at the mean
warexpo <- rep(NA, 595)
warexpo[war_exp <= mean(war_exp, na.rm = TRUE)] <- "Less exposed"
warexpo[war_exp >  mean(war_exp, na.rm = TRUE)] <- "More exposed"

# --- Panel dataset (longitudinal, n = 595 × 2 = 1190 rows) ---
data_panel <- data.frame(
  Wave    = c(rep("Wave 1", 595), rep("Wave 2", 595)),
  stflife = c(data_wp1$stflife,   data_w2$stflife),
  happy   = c(data_wp1$happy,     data_w2$happy),
  age     = rep(data_wp1$agea, 2),
  ware    = rep(warexpo, 2)
)

# Age groups
data_panel$age_g <- cut(data_panel$age,
                        breaks = c(14, 24, 34, 44, 54, 64, 74, Inf),
                        labels = c("15-24","25-34","35-44","45-54","55-64","65-74","75+"))


# ============================================================
# 2. CHANGES IN WELL-BEING (Wave 1 → Wave 2) IN FULL SAMPLE 
#    AND BY WAR EXPOSURE GROUPS
# ============================================================

# --- 2.1 Life satisfaction (stflife) – full sample ---
tapply(data_panel$stflife, data_panel$Wave, mean, na.rm = TRUE) |> round(2)
wilcox_test(data_panel, stflife ~ Wave, paired = TRUE)
wilcox_effsize(data_panel, stflife ~ Wave, paired = TRUE)

# --- 2.2 Life satisfaction – less exposed group ---
tapply(data_panel$stflife[data_panel$ware == "Less exposed"],
       data_panel$Wave[data_panel$ware == "Less exposed"],
       mean, na.rm = TRUE) |> round(2)
wilcox_test(data_panel[data_panel$ware == "Less exposed", ], stflife ~ Wave, paired = TRUE)
wilcox_effsize(data_panel[data_panel$ware == "Less exposed", ], stflife ~ Wave, paired = TRUE)

# --- 2.3 Life satisfaction – more exposed group ---
tapply(data_panel$stflife[data_panel$ware == "More exposed"],
       data_panel$Wave[data_panel$ware == "More exposed"],
       mean, na.rm = TRUE) |> round(2)
wilcox_test(data_panel[data_panel$ware == "More exposed", ], stflife ~ Wave, paired = TRUE)
wilcox_effsize(data_panel[data_panel$ware == "More exposed", ], stflife ~ Wave, paired = TRUE)

# --- 2.4 Happiness (happy) – full sample ---
tapply(data_panel$happy, data_panel$Wave, mean, na.rm = TRUE) |> round(2)
wilcox_test(data_panel, happy ~ Wave, paired = TRUE)
wilcox_effsize(data_panel, happy ~ Wave, paired = TRUE)

# --- 2.5 Happiness – less exposed group ---
tapply(data_panel$happy[data_panel$ware == "Less exposed"],
       data_panel$Wave[data_panel$ware == "Less exposed"],
       mean, na.rm = TRUE) |> round(2)
wilcox_test(data_panel[data_panel$ware == "Less exposed", ], happy ~ Wave, paired = TRUE)
wilcox_effsize(data_panel[data_panel$ware == "Less exposed", ], happy ~ Wave, paired = TRUE)

# --- 2.6 Happiness – more exposed group ---
tapply(data_panel$happy[data_panel$ware == "More exposed"],
       data_panel$Wave[data_panel$ware == "More exposed"],
       mean, na.rm = TRUE) |> round(2)
wilcox_test(data_panel[data_panel$ware == "More exposed", ], happy ~ Wave, paired = TRUE)
wilcox_effsize(data_panel[data_panel$ware == "More exposed", ], happy ~ Wave, paired = TRUE)


# ============================================================
# 3. BETWEEN-GROUP COMPARISONS WITHIN WAVES
#    (Less vs. More exposed)
# ============================================================

# Life satisfaction
wilcox_test(data_panel[data_panel$Wave == "Wave 1", ], stflife ~ ware)
wilcox_effsize(data_panel[data_panel$Wave == "Wave 1", ], stflife ~ ware)

wilcox_test(data_panel[data_panel$Wave == "Wave 2", ], stflife ~ ware)
wilcox_effsize(data_panel[data_panel$Wave == "Wave 2", ], stflife ~ ware)

# Happiness
wilcox_test(data_panel[data_panel$Wave == "Wave 1", ], happy ~ ware)
wilcox_effsize(data_panel[data_panel$Wave == "Wave 1", ], happy ~ ware)

wilcox_test(data_panel[data_panel$Wave == "Wave 2", ], happy ~ ware)
wilcox_effsize(data_panel[data_panel$Wave == "Wave 2", ], happy ~ ware)


# ============================================================
# 4. AGE DISTRIBUTION BY EXPOSURE GROUP
# ============================================================

w2 <- data_panel[data_panel$Wave == "Wave 2", ]

# --- Mean age: overall and by group + Wilcoxon test ---
tapply(w2$age, list(w2$ware), mean, na.rm = TRUE) |>
  c(Overall = mean(w2$age, na.rm = TRUE)) |> round(2)

wilcox_test(w2, age ~ ware)

# --- Age group proportions side by side ---
rbind(
  Overall       = round(prop.table(table(w2$age_g)) * 100, 1),
  Less_exposed  = round(prop.table(table(w2$age_g[w2$ware == "Less exposed"])) * 100, 1),
  More_exposed  = round(prop.table(table(w2$age_g[w2$ware == "More exposed"])) * 100, 1)
)

# --- Proportion tests per age group (Less vs. More exposed) ---
counts <- rbind(
  table(w2$age_g[w2$ware == "Less exposed"]),
  table(w2$age_g[w2$ware == "More exposed"])
)
ns <- c(sum(w2$ware == "Less exposed", na.rm = TRUE),
        sum(w2$ware == "More exposed", na.rm = TRUE))

sapply(levels(w2$age_g), function(g)
  prop.test(counts[, g], ns),
  simplify = FALSE)


# ============================================================
# 6. REGRESSION ANALYSES (quadratic age effect)
#    Figures 1 and 2
# ============================================================

# --- Models Wave 1 ---
fit1_1 <- lm(stflife ~ poly(age, 2), data = data_panel[data_panel$Wave == "Wave 1", ])
fit1_2 <- lm(happy   ~ poly(age, 2), data = data_panel[data_panel$Wave == "Wave 1", ])

# --- Models Wave 2 (overall) ---
fit2_1 <- lm(stflife ~ poly(age, 2), data = data_panel[data_panel$Wave == "Wave 2", ])
fit2_2 <- lm(happy   ~ poly(age, 2), data = data_panel[data_panel$Wave == "Wave 2", ])

# --- Models Wave 2 × exposure group ---
fit3_1 <- lm(stflife ~ poly(age, 2),
             data = subset(data_panel, Wave == "Wave 2" & ware == "Less exposed"))
fit3_2 <- lm(happy   ~ poly(age, 2),
             data = subset(data_panel, Wave == "Wave 2" & ware == "Less exposed"))
fit4_1 <- lm(stflife ~ poly(age, 2),
             data = subset(data_panel, Wave == "Wave 2" & ware == "More exposed"))
fit4_2 <- lm(happy   ~ poly(age, 2),
             data = subset(data_panel, Wave == "Wave 2" & ware == "More exposed"))

summary(fit1_1)  # life satisfaction ~ age², Wave 1
summary(fit1_2)  # happiness ~ age², Wave 1
summary(fit2_1)  # life satisfaction ~ age², Wave 2
summary(fit2_2)  # happiness ~ age², Wave 2

summary(fit3_1)  # life satisfaction ~ age², Wave 2, less exposed
summary(fit3_2)  # happiness ~ age², Wave 2, less exposed
summary(fit4_1)  # life satisfaction ~ age², Wave 2, more exposed
summary(fit4_2)  # happiness ~ age², Wave 2, more exposed


# ============================================================
# 7. FIGURES: predictions (emmeans) from regression models)
# ============================================================

# Figure 1: Age–well-being curves (Wave 1 vs. Wave 2)
p1_1 <- plot_model(fit1_1, type = "emm", terms = "age[15:87]") +
  scale_x_continuous(limits = c(15, 87), breaks = seq(15, 87, 10)) +
  scale_y_continuous(limits = c(0, 10), breaks = 0:10) +
  labs(title = "Before full-scale invasion") +
  xlab("") + ylab("Life satisfaction")

p2_1 <- plot_model(fit2_1, type = "emm", terms = "age[15:87]") +
  scale_x_continuous(limits = c(15, 87), breaks = seq(15, 87, 10)) +
  scale_y_continuous(limits = c(0, 10), breaks = 0:10) +
  labs(title = "During full-scale invasion") +
  xlab("") + ylab("")

p1_2 <- plot_model(fit1_2, type = "emm", terms = "age[15:87]") +
  scale_x_continuous(limits = c(15, 87), breaks = seq(15, 87, 10)) +
  scale_y_continuous(limits = c(0, 10), breaks = 0:10) +
  labs(title = "") +
  xlab("Age") + ylab("Happiness")

p2_2 <- plot_model(fit2_2, type = "emm", terms = "age[15:87]") +
  scale_x_continuous(limits = c(15, 87), breaks = seq(15, 87, 10)) +
  scale_y_continuous(limits = c(0, 10), breaks = 0:10) +
  labs(title = "") +
  xlab("Age") + ylab("")


(p1_1 + p2_1) / (p1_2 + p2_2) # combine plots (Wave 1 vs. Wave 2)

# Figure 2: Age–well-being curves by exposure group (Wave 2)
p4_1 <- plot_model(fit4_1, type = "emm", terms = "age[15:87]") +
  scale_x_continuous(limits = c(15, 87), breaks = seq(15, 87, 10)) +
  scale_y_continuous(limits = c(0, 10), breaks = 0:10) +
  labs(title = "More exposed") +
  xlab("") + ylab("Life satisfaction")

p3_1 <- plot_model(fit3_1, type = "emm", terms = "age[15:87]") +
  scale_x_continuous(limits = c(15, 87), breaks = seq(15, 87, 10)) +
  scale_y_continuous(limits = c(0, 10), breaks = 0:10) +
  labs(title = "Less exposed") +
  xlab("") + ylab("")

p4_2 <- plot_model(fit4_2, type = "emm", terms = "age[15:87]") +
  scale_x_continuous(limits = c(15, 87), breaks = seq(15, 87, 10)) +
  scale_y_continuous(limits = c(0, 10), breaks = 0:10) +
  labs(title = "") +
  xlab("Age") + ylab("Happiness")

p3_2 <- plot_model(fit3_2, type = "emm", terms = "age[15:87]") +
  scale_x_continuous(limits = c(15, 87), breaks = seq(15, 87, 10)) +
  scale_y_continuous(limits = c(0, 10), breaks = 0:10) +
  labs(title = "") +
  xlab("Age") + ylab("")

(p4_1 + p3_1) / (p4_2 + p3_2) # combine plots (more vs less exposed)
