## ============================================================
## Perspective(s) of R. Inglehart's
## Evolutionary Modernization Theory: Analysis of Religiosity
## Changes in Ukraine During the War
## ============================================================

# ============================================================
# 0. LIBRARIES
# ============================================================
library(haven)
library(dplyr)
library(ggstatsplot)
library(ggplot2)

# ============================================================
# 1. DATA LOADING
# ============================================================
# Set your working directory or provide full paths to the data files
# setwd("C:/path/to/your/data")

liws_w1 <- read_sav("LIWS_wave_1.sav")
liws_w2 <- read_sav("LIWS_wave_2.sav")

# Panel subsample (respondents who participated in both waves)
liws_w1_panel <- liws_w1[liws_w1$pnlyon == 1, ]
N <- nrow(liws_w1_panel)   # 595


# ============================================================
# 2. VARIABLE PREPARATION
# ============================================================

# ----- 2.1 Self-assessed religiosity (H1, H2) -----
# rlgdgr: scale 0–10; higher = more religious
rlgdgr_d  <- liws_w2$rlgdgr - liws_w1_panel$rlgdgr


# ----- 2.2 Distance to war (H2) -----
# Category 1 (experienced occupation / active combat): reference
regions_cat1 <- c(
  "Mykolaivska oblast", "Khersonska oblast", "Zaporizka oblast",
  "Donetska oblast (only controlled by Ukraine)",
  "Luhanska oblast (only controlled by Ukraine)",
  "Kharkivska oblast", "Kyivska oblast", "Chernihivska oblast",
  "Sumska oblast", "Kyiv city"
)
# Category 2 (adjacent to cat. 1 oblasts): D2 = 1
regions_cat2 <- c(
  "Odeska oblast", "Dnipropetrovska oblast", "Kirovohradska oblast",
  "Cherkaska oblast", "Poltavska oblast", "Zhytomyrska oblast"
)
# Category 3 (distant from cat. 1 oblasts): D1 = 1
regions_cat3 <- c(
  "Vinnytska oblast", "Khmelnytska oblast", "Chernivetska oblast",
  "Ternopilska oblast", "Rivnenska oblast", "Ivano-Frankivska oblast",
  "Zakarpatska oblast", "Lvivska oblast", "Volynska oblast"
)

region_label <- as.character(as_factor(liws_w1_panel$region))

dist_cat <- case_when(
  region_label %in% regions_cat1 ~ 1L,
  region_label %in% regions_cat2 ~ 2L,
  region_label %in% regions_cat3 ~ 3L
)

# Dummy variables for regression (reference: cat. 1 — occupied / frontline regions)
D1 <- as.integer(dist_cat == 3)   # distant regions
D2 <- as.integer(dist_cat == 2)   # adjacent regions


# ----- 2.3 In-group solidarity (H3) -----
# pplfair: "most people try to be fair and honest" (0–10)
# atchctr: emotional attachment to Ukraine (0–10)
pplfair_d <- liws_w2$pplfair - liws_w1_panel$pplfair
atchctr_d <- liws_w2$atchctr - liws_w1_panel$atchctr


# ----- 2.4 Change in authoritarian values (H3) -----
# lrnobed (obedience to authority) / loylead (loyalty to leaders):
#   1 = Fully agree … 5 = Fully disagree
#   W1 − W2: positive values = increased support for obedience / leaders
lrnobed_d <- liws_w1_panel$lrnobed - liws_w2$lrnobed
loylead_d <- liws_w1_panel$loylead - liws_w2$loylead

# implvdm: importance of living in a country governed democratically (0–10); W2 − W1
implvdm_d <- liws_w2$implvdm - liws_w1_panel$implvdm


# ============================================================
# 3. MACROREGIONS (Table 1)
# ============================================================
west    <- c("Volynska oblast", "Rivnenska oblast", "Lvivska oblast",
             "Ivano-Frankivska oblast", "Ternopilska oblast",
             "Zakarpatska oblast", "Khmelnytska oblast", "Chernivetska oblast")
central <- c("Vinnytska oblast", "Zhytomyrska oblast", "Sumska oblast",
             "Chernihivska oblast", "Poltavska oblast", "Kirovohradska oblast",
             "Cherkaska oblast", "Kyivska oblast", "Kyiv city")
south   <- c("Dnipropetrovska oblast", "Zaporizka oblast", "Mykolaivska oblast",
             "Khersonska oblast", "Odeska oblast")
east    <- c("Donetska oblast (only controlled by Ukraine)",
             "Luhanska oblast (only controlled by Ukraine)",
             "Kharkivska oblast")

macroregion <- case_when(
  region_label %in% west    ~ "Western",
  region_label %in% central ~ "Central",
  region_label %in% south   ~ "Southern",
  region_label %in% east    ~ "Eastern"
)


# ============================================================
# 4. HYPOTHESIS 1
#    Increase in religiosity during the war
# ============================================================
df_h1 <- data.frame(
  id     = rep(seq_len(N), 2),
  wave   = factor(rep(c("Wave 1\n(Jan–Feb 2022)",
                        "Wave 2\n(Aug–Oct 2022)"), each = N),
                  levels = c("Wave 1\n(Jan–Feb 2022)",
                             "Wave 2\n(Aug–Oct 2022)")),
  rlgdgr = c(as.numeric(liws_w1_panel$rlgdgr), as.numeric(liws_w2$rlgdgr))
)

ggwithinstats(
  data            = df_h1,
  x               = wave,
  y               = rlgdgr,
  subject.id      = id,
  type            = "nonparametric",
  centrality.type = "parametric",       # show mean
  point.args      = list(alpha = 0),    # hide individual data points
  point.path.args = list(alpha = 0),    # hide spaghetti lines
  title           = "Fig. 1. Distribution of self-assessed religiosity after the Russian invasion (N = 575)",
  xlab            = "",
  ylab            = "Self-assessed religiosity (0–10)"
)


# ============================================================
# 5. HYPOTHESIS 2
#    Effect of distance to the front on religiosity change
#    Model: Religiosity ~ D1 + D2 + Wave + D1×Wave + D2×Wave
# ============================================================
df_long <- data.frame(
  id          = rep(seq_len(N), 2),
  Religiosity = c(as.numeric(liws_w1_panel$rlgdgr), as.numeric(liws_w2$rlgdgr)),
  Wave        = c(rep(0L, N), rep(1L, N)),
  D1          = rep(D1, 2),
  D2          = rep(D2, 2)
)

model_h2 <- lm(Religiosity ~ D1 + D2 + Wave + D1:Wave + D2:Wave,
               data = df_long)

summary(model_h2)

m_stats <- summary(model_h2)
f_stat  <- m_stats$fstatistic
p_model <- pf(f_stat[1], f_stat[2], f_stat[3], lower.tail = FALSE)
p_fmt   <- ifelse(p_model < 0.001, "p < 0.001", paste0("p = ", round(p_model, 3)))
caption_h2 <- paste0(
  "N = ", nobs(model_h2), ", ",
  "R-square = ", round(m_stats$r.squared, 3), ", ",
  p_fmt
)

ggcoefstats(
  model_h2,
  exclude.intercept = TRUE,
  xlab  = "Unstandardized regression coefficients",
  ylab  = "",
  title = "Fig. 2. Multiple linear regression coefficients"
) +
  labs(caption = caption_h2) +
  scale_y_discrete(labels = c(
    "D1:Wave" = "Distant regions \u00D7 Wave",
    "D2:Wave" = "Adjacent regions \u00D7 Wave",
    "Wave"    = "Wave (war experience)",
    "D2"      = "Adjacent regions",
    "D1"      = "Distant regions"
  ))


# ============================================================
# 6. TABLE 1
#    Comparison of means by macroregion (Wilcoxon signed-rank test)
# ============================================================
complete_rlg <- !is.na(liws_w1_panel$rlgdgr) & !is.na(liws_w2$rlgdgr)

table1 <- lapply(c("Central", "Eastern", "Southern", "Western"),
  function(reg) {
    idx <- macroregion == reg & complete_rlg
    w1v <- liws_w1_panel$rlgdgr[idx]
    w2v <- liws_w2$rlgdgr[idx]
    wt  <- wilcox.test(w2v, w1v, paired = TRUE, exact = FALSE)
    data.frame(
      Macroregion = reg,
      `Wave 1`    = round(mean(w1v, na.rm = TRUE), 2),
      `Wave 2`    = round(mean(w2v, na.rm = TRUE), 2),
      Difference  = round(mean(w2v, na.rm = TRUE) - mean(w1v, na.rm = TRUE), 2),
      `P-value`   = ifelse(wt$p.value < 0.01, "< 0.01", round(wt$p.value, 2)),
      N           = length(w1v),
      check.names = FALSE
    )
  }
)

print(do.call(rbind, table1), row.names = FALSE)


# ============================================================
# 7. HYPOTHESIS 3
#    Spearman correlation matrix: changes in religiosity,
#    in-group solidarity, and authoritarian values
# ============================================================
df_cor <- data.frame(
  "Δ religiosity"             = rlgdgr_d,
  "Δ trust in people"         = pplfair_d,
  "Δ attachment to Ukraine"   = atchctr_d,
  "Δ respect for authority"   = lrnobed_d,
  "Δ support for leaders"     = loylead_d,
  "Δ importance of democracy" = implvdm_d,
  check.names = FALSE
)

ggcorrmat(
  data             = df_cor,
  type             = "nonparametric",
  title            = "Fig. 3. Spearman correlation matrix (N = 575)",
  matrix.type      = "lower",
  p.adjust.method  = "none"
)