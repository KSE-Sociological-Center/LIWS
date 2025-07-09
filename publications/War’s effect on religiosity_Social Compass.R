################# Data Preparation #################

# Load required packages

# install.packages("statsExpressions")
# install.packages("ggstatsplot")
# install.packages("ggplot2")
# install.packages("patchwork")

library(foreign)
library(statsExpressions)
library(ggstatsplot)
library(ggplot2)
library(patchwork)

# Load Wave 1 data (no value labels)
liws_w1 <- read.spss("LIWS_wave_1.sav"
                     ,to.data.frame = T
                     ,use.value.labels = F) # insert your pathway

# Select respondents from Wave 1 who participated in the panel
liws_w1_panel <- liws_w1[liws_w1$pnlyon == 1,]

# Load Wave 2 data (no value labels)
liws_w2 <- read.spss("LIWS_wave_2.sav"
                     ,to.data.frame = T
                     ,use.value.labels = F) # insert your pathway

# Key variables:

# Religiosity: 
# rlgdgr - Self-assessed religiosity
# rlgatnd - Frequency of church attendance
# pray - Frequency of prayer

# War exposure variables (Wave 2 only):
# expdi - Decrease in income
# exple - Loss of employment
# expdph - Deterioration of physical health
# expdmh - Deterioration of mental health
# expsfm - Separation from family members
# expldh - Loss or damage to home
# expldp - Loss or damage to other property
# expinj - Injury to yourself or family members
# dwarlst - Lost anyone during the war
# chngplr - Changed place of residence or not after 24 February 2022

# Invert scale for frequency of church attendance (higher = more frequent)
liws_w1_panel$rlgatnd <- 7 - liws_w1_panel$rlgatnd
liws_w2$rlgatnd <- 7 - liws_w2$rlgatnd

# Invert scale for frequency of prayer (higher = more frequent)
liws_w1_panel$pray <- 7 - liws_w1_panel$pray
liws_w2$pray <- 7 - liws_w2$pray

# Calculate change scores (Wave 2 minus Wave 1) for religiosity indicators
self_d <- liws_w2$rlgdgr - liws_w1_panel$rlgdgr        # Directional religious self-assessment change
self_a <- abs(self_d)                                  # Absolute religious self-assessment change

attend_d <- liws_w2$rlgatnd - liws_w1_panel$rlgatnd    # Directional change of church attendance frequency
attend_a <- abs(attend_d)                              # Absolute change of church attendance frequency

pray_d <- liws_w2$pray - liws_w1_panel$pray            # Directional change of pray frequency
pray_a <- abs(pray_d)                                  # Absolute change of pray frequency

# Construct Religiosity Index by standardizing each religiosity variable (pooled across waves)
scale_self <- scale(c(liws_w1_panel$rlgdgr, liws_w2$rlgdgr))
scale_attend <- scale(c(liws_w1_panel$rlgatnd, liws_w2$rlgatnd))
scale_pray <- scale(c(liws_w1_panel$pray, liws_w2$pray))

# Calculate arithmetic means of standardized variables
rlg_ind <- rowMeans(data.frame(scale_self, scale_attend, scale_pray)) # Religiosity Index

# Compute religious change (Religiosity Index in Wave 2 minus Wave 1)
rlg_ind_d <- rlg_ind[596:1190] - rlg_ind[1:595]       # Directional religious change
rlg_ind_a <- abs(rlg_ind_d)                           # Absolute religious change

# Construct previous religiosity variable
prev_rel <- rep(NA, 595)
prev_rel[rlg_ind[1:595] > 0] <- "More religious"
prev_rel[rlg_ind[1:595] <= 0] <- "Less religious"

# explst - Experience of losing someone you know (0 = no, 1 = yes)
explst <- rep(NA, 595)
explst[liws_w2$dwarlst == 4] <- 0
explst[liws_w2$dwarlst >= 1 & liws_w2$dwarlst <= 3] <- 1

# expdis - Experiencing forced displacement (0 = no, 1 = yes)
expdis <- rep(NA, 595)
expdis[liws_w2$chngplr == 1] <- 0
expdis[liws_w2$chngplr > 1] <- 1

# Calculate war exposure index: sum variables expdi to expinj, plus explst and expdis
war_exp <- apply(liws_w2[,60:67], 1, sum) + explst + expdis

# Create final dataframe with constructed variables
data_panel <- data.frame(self_d, attend_d, pray_d, rlg_ind_d
                         ,self_a, attend_a, pray_a, rlg_ind_a
                         ,prev_rel, war_exp)



################# Data Analysis #################

# Figure 2. The correlation between changes in religiosity and war exposure

# Calculate Spearman correlations between war exposure and (a) absolute religious change, (b) directional religious change
spear_2 <- rbind(
  corr_test(data_panel, x = rlg_ind_a, y = war_exp, type = "n")
  ,corr_test(data_panel, x = rlg_ind_d, y = war_exp, type = "n")
  )
spear_2$term <- c("Absolute religious change", "Directional religious change")

# Visualize Spearman correlations (orange = significant, grey = not significant)
ggcoefstats(spear_2
            ,point.args = list(size = 3, color = c("darkorange","grey30"))
            ,vline.args = list(linewidth = 0.7, linetype = "dashed", color = "grey40")
            ,errorbar.args = list(color = c("darkorange","grey30"), height = 0)) +
  xlab(" ") + ylab(" ") +
  scale_x_continuous(limits = c(-1, 1), breaks = seq(-1, 1, 0.25)) +
  geom_label(aes(label = expression), parse = T, size = 3, nudge_y = -0.1)

# Figure 3. Changes in religiosity among more and less religious individuals

p3_1 <- ggbetweenstats(data_panel, x = prev_rel, y = rlg_ind_d, type = "n", centrality.plotting = F) +
  xlab(" ") + ylab("Directional religious change")

p3_2 <- ggbetweenstats(data_panel, x = prev_rel, y = rlg_ind_a, type = "n", centrality.plotting = F) +
  xlab(" ") + ylab("Absolute religious change")

p3_1/p3_2

# Figure 4. Changes in religiosity by indicators of Religiosity Index among more and less religious individuals

p4_1 <- ggbetweenstats(data_panel, x = prev_rel, y = self_d, type = "n", centrality.plotting = F
                       ,point.args = list(alpha = 0)
                       ,boxplot.args = list(aes(fill = prev_rel), width = 0.3, alpha = 0.15, na.rm = T)) +
  xlab(" ") + ylab("Directional religious \n self-assessment change")

p4_2 <- ggbetweenstats(data_panel, x = prev_rel, y = self_a, type = "n", centrality.plotting = F
                       ,point.args = list(alpha = 0)
                       ,boxplot.args = list(aes(fill = prev_rel), width = 0.3, alpha = 0.15, na.rm = T)) +
  xlab(" ") + ylab("Absolute religious \n self-assessment change")

p4_3 <- ggbetweenstats(data_panel, x = prev_rel, y = attend_d, type = "n", centrality.plotting = F
                       ,point.args = list(alpha = 0)
                       ,boxplot.args = list(aes(fill = prev_rel), width = 0.3, alpha = 0.15, na.rm = T)) +
  xlab(" ") + ylab("Directional change of \n church attendance frequency")

p4_4 <- ggbetweenstats(data_panel, x = prev_rel, y = attend_a, type = "n", centrality.plotting = F
                       ,point.args = list(alpha = 0)
                       ,boxplot.args = list(aes(fill = prev_rel), width = 0.3, alpha = 0.15, na.rm = T)) + 
  xlab(" ") + ylab("Absolute change of \n church attendance frequency")

p4_5 <- ggbetweenstats(data_panel, x = prev_rel, y = pray_d, type = "n", centrality.plotting = F
                       ,point.args = list(alpha = 0)
                       ,boxplot.args = list(aes(fill = prev_rel), width = 0.3, alpha = 0.15, na.rm = T)) +
  xlab(" ") + ylab("Directional change of \n pray frequency")

p4_6 <- ggbetweenstats(data_panel, x = prev_rel, y = pray_a, type = "n", centrality.plotting = F
                       ,point.args = list(alpha = 0)
                       ,boxplot.args = list(aes(fill = prev_rel), width = 0.3, alpha = 0.15, na.rm = T)) +
  xlab(" ") + ylab("Absolute change of \n pray frequency")

p4_1/p4_2 # Change of religious self-assessment
p4_3/p4_4 # Change of church attendance frequency
p4_5/p4_6 # Change of prayer frequency

# Figure 5. The correlation between changes in religiosity and war exposure among more and less religious individuals

# Spearman correlations for less religious
spear_5_1 <- rbind(
  corr_test(data_panel[data_panel$prev_rel == "Less religious",], x = rlg_ind_a, y = war_exp, type = "n")
  ,corr_test(data_panel[data_panel$prev_rel == "Less religious",], x = rlg_ind_d, y = war_exp, type = "n")
  )
spear_5_1$term <- c("Absolute religious change", "Directional religious change")

# Spearman correlations for more religious
spear_5_2 <- rbind(
  corr_test(data_panel[data_panel$prev_rel == "More religious",], x = rlg_ind_a, y = war_exp, type = "n")
  ,corr_test(data_panel[data_panel$prev_rel == "More religious",], x = rlg_ind_d, y = war_exp, type = "n")
  )
spear_5_2$term <- c("Absolute religious change", "Directional religious change")

# Visualize correlations for both groups (orange - significant correlation, grey - non significant)
p5_1 <- ggcoefstats(spear_5_1
                    ,point.args = list(size = 3, color = "grey30")
                    ,vline.args = list(linewidth = 0.7, linetype = "dashed", color = "grey40")
                    ,errorbar.args = list(color = "grey30", height = 0)) +
  xlab(" ") + ylab(" ") +
  scale_x_continuous(limits = c(-1, 1), breaks = seq(-1, 1, 0.25)) +
  geom_label(aes(label = expression), parse = T, size = 3, nudge_y = -0.2)

p5_2 <- ggcoefstats(spear_5_2
                    ,point.args = list(size = 3, color = c("darkorange","grey30"))
                    ,vline.args = list(linewidth = 0.7, linetype = "dashed", color = "grey40")
                    ,errorbar.args = list(color = c("darkorange","grey30"), height = 0)) +
  xlab(" ") + ylab(" ") +
  scale_x_continuous(limits = c(-1, 1), breaks = seq(-1, 1, 0.25)) +
  geom_label(aes(label = expression), parse = T, size = 3, nudge_y = -0.2)

p5_1/p5_2