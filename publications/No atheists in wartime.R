## ============================================================
## "NO ATHEISTS IN FOXHOLES?"
## ============================================================

# ============================================================
# 0. LIBRARIES
# ============================================================
library(haven)
library(psych)
library(ggplot2)
library(ggpubr)
library(patchwork)
library(plotly)


# ============================================================
# 1. DATA LOADING AND PREPARATION
# ============================================================

# Set your working directory to the folder containing the data files, e.g.:
# setwd("C:/path/to/your/data")

liws_w1       <- read_sav("LIWS_wave_1.sav")
liws_w2       <- read_sav("LIWS_wave_2.sav")
liws_w1_panel <- liws_w1[liws_w1$pnlyon == 1, ]

# Invert pray and attendance: 8 – x  →  1 = never, 7 = every day
liws_w1_panel$rlgatnd <- 8 - as.numeric(liws_w1_panel$rlgatnd)
liws_w2$rlgatnd       <- 8 - as.numeric(liws_w2$rlgatnd)
liws_w1_panel$pray    <- 8 - as.numeric(liws_w1_panel$pray)
liws_w2$pray          <- 8 - as.numeric(liws_w2$pray)

# Religiosity Index: z-score each indicator → mean → rescale 0–1
scale_self   <- scale(c(as.numeric(liws_w1_panel$rlgdgr),  as.numeric(liws_w2$rlgdgr)), center = FALSE, scale = TRUE)
scale_attend <- scale(c(liws_w1_panel$rlgatnd, liws_w2$rlgatnd), center = FALSE, scale = TRUE)
scale_pray   <- scale(c(liws_w1_panel$pray,    liws_w2$pray), center = FALSE, scale = TRUE)
rlg_ind_z    <- apply(data.frame(scale_self, scale_attend, scale_pray), 1, mean, na.rm = TRUE)
rlg_ind      <- (rlg_ind_z - min(rlg_ind_z, na.rm = TRUE)) /
                diff(range(rlg_ind_z, na.rm = TRUE))
rlg_ind_w1   <- rlg_ind[1:595]
rlg_ind_w2   <- rlg_ind[596:1190]

# Cronbach's alpha
cat("Cronbach's alpha:",
    round(psych::alpha(data.frame(scale_self, scale_attend, scale_pray))$total$raw_alpha, 2))

# Change scores (Wave 2 – Wave 1)
self_d   <- as.numeric(liws_w2$rlgdgr)  - as.numeric(liws_w1_panel$rlgdgr)
pray_d   <- as.numeric(liws_w2$pray)    - as.numeric(liws_w1_panel$pray)
attend_d <- as.numeric(liws_w2$rlgatnd) - as.numeric(liws_w1_panel$rlgatnd)
ind_d    <- rlg_ind_w2 - rlg_ind_w1

# Wave labels
w1_label <- "Jan 18 –\nFeb 8, 2022"
w2_label <- "Aug 24 –\nOct 6, 2022"


# ============================================================
# TABLE 1: Distribution of changes (%, increased / no change / decreased)
# ============================================================
change_pct <- function(x) {
  c("Increased"  = round(mean(x > 0,  na.rm = TRUE) * 100, 1),
    "No change"  = round(mean(x == 0, na.rm = TRUE) * 100, 1),
    "Decreased"  = round(mean(x < 0,  na.rm = TRUE) * 100, 1))
}

as.data.frame(t(data.frame(
  "Self-assessed religiosity"    = change_pct(self_d),
  "Prayer frequency"             = change_pct(pray_d),
  "Church attendance"            = change_pct(attend_d),
  "Religious Involvement Index"  = change_pct(ind_d),
  check.names = FALSE
)))


# ============================================================
# TABLE 2: Means by wave + paired t-test
# ============================================================
make_row <- function(w1, w2, label) {
  w1 <- as.numeric(w1); w2 <- as.numeric(w2)
  p  <- t.test(w1, w2, paired = TRUE)$p.value
  sig <- ifelse(p < 0.001, "p < 0.001",
         ifelse(p < 0.01,  "p < 0.01",
         ifelse(p < 0.05,  "p < 0.05", "n.s.")))
  data.frame(
    "Indicator"    = label,
    "Wave 1"       = sprintf("%.2f (SD = %.2f)", mean(w1, na.rm = TRUE), sd(w1, na.rm = TRUE)),
    "Wave 2"       = sprintf("%.2f (SD = %.2f)", mean(w2, na.rm = TRUE), sd(w2, na.rm = TRUE)),
    "Significance" = sig,
    check.names    = FALSE
  )
}

table2 <- rbind(
  make_row(liws_w1_panel$rlgdgr,  liws_w2$rlgdgr,  "Self-assessed religiosity (0–10)"),
  make_row(liws_w1_panel$pray,    liws_w2$pray,    "Prayer frequency (1–7)"),
  make_row(liws_w1_panel$rlgatnd, liws_w2$rlgatnd, "Church attendance (1–7)")
)
rownames(table2) <- NULL

table2


# ============================================================
# FIGURE 1: Religious Involvement Index dynamics
# ============================================================
fig1_data <- data.frame(
  wave = factor(rep(c(w1_label, w2_label), each = 595),
                levels = c(w1_label, w2_label)),
  religiosity = rlg_ind
)

wilcox_p <- wilcox.test(rlg_ind_w1, rlg_ind_w2, paired = TRUE)$p.value
p_text   <- if (wilcox_p < 0.001) "p < 0.001" else paste0("p = ", round(wilcox_p, 3))

ggboxplot(fig1_data, x = "wave", y = "religiosity",
          color = "wave", add = "jitter",
          palette = c("MidnightBlue", "#FF4500")) +
  stat_summary(
    fun.data = function(x) data.frame(y = 1.10, label = paste("M =", round(mean(x), 2))),
    geom = "text", size = 5
  ) +
  stat_summary(
    fun.data = function(x) data.frame(y = 1.04, label = paste("SD =", round(sd(x), 2))),
    geom = "text", size = 5
  ) +
  annotate("text", x = 1.5, y = 1.18,
           label = paste0("Paired Wilcoxon test, ", p_text),
           size = 4.5) +
  scale_y_continuous(name = "Religious Involvement Index (0–1)",
                     limits = c(0, 1.25), breaks = seq(0, 1, 0.25)) +
  scale_x_discrete(name = "") +
  labs(title = "Figure 1. Religious Involvement dynamics") +
  theme(legend.position = "none", text = element_text(size = 13))


# ============================================================
# Helper: segment chart (Figures 2.1, 3.1)
# ============================================================
freq_labels <- c("Never", "Less often", "Only on\nreligious holidays",
                 "At least once\na month", "Once a\nweek",
                 "More than once\na week", "Daily")

# Labels without "\n" for Sankey nodes
freq_labels_s <- c("Never", "Less often", "Only on religious holidays",
                   "At least once a month", "Once a week",
                   "More than once a week", "Daily")

make_segment <- function(w1, w2) {
  n <- length(w1)
  pct <- function(x) c(as.numeric(table(factor(x, 1:7))), sum(is.na(x))) / n * 100
  p1  <- pct(w1); p2 <- pct(w2)
  cats <- c("Don't know/\nRefusal", freq_labels)  # DK at bottom
  data.frame(
    Category  = factor(cats, levels = cats),
    Wave1_pct = c(p1[8], p1[1:7]),
    Wave2_pct = c(p2[8], p2[1:7]),
    direction = ifelse(c(p2[8], p2[1:7]) > c(p1[8], p1[1:7]), "increase", "decrease"),
    change    = c(p2[8], p2[1:7]) - c(p1[8], p1[1:7])
  )
}

draw_segment <- function(df, title_text) {
  x_text <- max(c(df$Wave1_pct, df$Wave2_pct)) + 2
  x_lim  <- x_text + 8

  ggplot(df, aes(y = Category, color = direction)) +
    geom_segment(aes(x = Wave1_pct, xend = Wave2_pct, yend = Category),
                 linewidth = 12, alpha = 0.5) +
    geom_point(aes(x = Wave2_pct), color = "gold", size = 8) +
    geom_text(aes(x = x_text,
                  label = paste0(round(change, 1), "%")),
              color = "black", size = 4, hjust = 0) +
    scale_x_continuous(labels = function(x) paste0(x, "%"),
                       limits = c(0, x_lim),
                       breaks = seq(0, x_lim, 10)) +
    scale_color_manual(values = c("increase" = "MidnightBlue", "decrease" = "#FF4500")) +
    labs(title = title_text, x = "", y = "") +
    theme_minimal(base_size = 13) +
    theme(legend.position = "none",
          panel.grid.major.y = element_blank(),
          panel.grid.minor   = element_blank())
}


# ============================================================
# Helper: interactive Sankey (Figures 2.2, 3.2)
# ============================================================

# Colors per category (from QMD), mapped to inverted scale (1=Never, 7=Daily)
# Original QMD scale: 1=Daily → color[1]="#e3aa03", ..., 7=Never → color[7]="MidnightBlue"
# With 8-x inversion: inverted k corresponds to original (8-k), so color index = 8-k
rlg_colors <- c("#e3aa03", "silver", "#FF4500", "#e3a103", "#fcc31e", "#265999", "MidnightBlue")

make_sankey <- function(w1, w2, title_text) {
  pairs  <- na.omit(data.frame(w1 = as.integer(w1), w2 = as.integer(w2)))
  n_tot  <- 595
  flows  <- as.data.frame(table(w1 = pairs$w1, w2 = pairs$w2))
  flows  <- flows[flows$Freq > 0, ]
  flows$pct    <- round(flows$Freq / n_tot * 100, 2)
  flows$source <- as.integer(as.character(flows$w1)) - 1       # 0-based, wave 1 nodes
  flows$target <- as.integer(as.character(flows$w2)) - 1 + 7  # 0-based, wave 2 nodes
  flows$color  <- rlg_colors[8 - as.integer(as.character(flows$w1))]  # colour by source

  nodes <- c(paste0("Winter 2022: ", freq_labels_s),
             paste0("Autumn 2022: ", freq_labels_s))

  src_lbl <- freq_labels_s[flows$source + 1]
  tgt_lbl <- freq_labels_s[flows$target - 6]
  hover   <- paste0(src_lbl, " \u2192 ", tgt_lbl, ": ", flows$pct, "%")

  plot_ly(
    type        = "sankey",
    orientation = "h",
    arrangement = "freeform",
    node = list(
      pad           = 7,
      thickness     = 5,
      line          = list(color = "black", width = 0.2),
      label         = nodes,
      color         = "black",
      x             = c(rep(1e-9, 7), rep(0.99, 7)),
      y             = c(0.99, 0.80, 0.60, 0.55, 0.50, 0.43, 0.20,
                        0.99, 0.80, 0.65, 0.60, 0.55, 0.50, 0.20),
      hovertemplate = "%{label}<extra></extra>"
    ),
    link = list(
      source        = flows$source,
      target        = flows$target,
      value         = flows$pct,
      color         = flows$color,
      customdata    = hover,
      hovertemplate = "%{customdata}<extra></extra>"
    )
  ) |>
    layout(
      title = list(text = title_text,
                   font = list(size = 16, color = "#333333")),
      font  = list(family = "Arial", size = 15, color = "#333333"),
      xaxis = list(showgrid = FALSE),
      yaxis = list(showgrid = FALSE)
    )
}


# ============================================================
# FIGURES 2.1 + 2.2: Prayer dynamics
# ============================================================
draw_segment(
  make_segment(liws_w1_panel$pray, liws_w2$pray),
  "Figure 2.1. Individual religious practice dynamics (prayer)"
) # The yellow dots represent the proportion of responses in the second wave

make_sankey(
  liws_w1_panel$pray, liws_w2$pray,
  "Figure 2.2. Individual changes in prayer frequency"
)


# ============================================================
# FIGURES 3.1 + 3.2: Church attendance dynamics
# ============================================================
draw_segment(
  make_segment(liws_w1_panel$rlgatnd, liws_w2$rlgatnd),
  "Figure 3.1. Public religious practice dynamics (church attendance)"
) # The yellow dots represent the proportion of responses in the second wave

make_sankey(
  liws_w1_panel$rlgatnd, liws_w2$rlgatnd,
  "Figure 3.2. Individual changes in church attendance"
)
