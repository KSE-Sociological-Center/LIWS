# ==============================================
# Analysis of War Exposure and Social Trust
# ==============================================

# Load required libraries
library(tidyverse)
library(viridis)
library(haven)
library(curl)
library(lme4)
library(modelsummary)
library(flextable)

# ----------------------------------------------
# 1. Read panel data directly from GitHub
# ----------------------------------------------
wave1_url  <- "https://raw.githubusercontent.com/hremjach/LIWS/main/LIWS_wave_1.sav"
wave2_url  <- "https://raw.githubusercontent.com/hremjach/LIWS/main/LIWS_wave_2.sav"

df_wave1 <- read_sav(curl(wave1_url, "rb"))
df_wave2 <- read_sav(curl(wave2_url, "rb"))

###

df_wave1 <- df_wave1 %>% 
  select(idnmbr, pplhlp, pplfair, happy) %>% 
  mutate(wave = "first")

df_wave2 <- df_wave2 %>% 
  select(idnmbr, pplhlp, pplfair, expdi, exple, expdph, expdmh, expsfm,
         expldh, expldp, expinj, dwarlst, chngplr, happy) %>% 
  mutate(wave = "second")


# ----------------------------------------------
# 2. Keep only respondents present in both waves
# ----------------------------------------------
common_ids <- intersect(df_wave1$idnmbr, df_wave2$idnmbr)

df_long <- bind_rows(
  filter(df_wave1, idnmbr %in% common_ids),
  filter(df_wave2, idnmbr %in% common_ids)
) %>%
  arrange(idnmbr, wave)


# ----------------------------------------------
# 3. Construct war-exposure and trust variables
# ----------------------------------------------

df_final <- df_long %>%
  # Renaming and recoding death/displacement dummies
  mutate(
    knew_deaths_dummy = case_when(
      dwarlst %in% 1:3       ~ 1,  # Had acquaintances/relatives who died
      dwarlst == 4           ~ 0,  # No acquaintances/relatives died
      dwarlst %in% c(7, 9)   ~ NA_real_
    ),
    family_death_dummy = case_when(
      dwarlst %in% 1:2       ~ 1,  # Immediate family member died
      dwarlst %in% 3:4       ~ 0,  # No immediate family death
      dwarlst %in% c(7, 9)   ~ NA_real_
    ),
    displaced_dummy = case_when(
      chngplr %in% 2:6       ~ 1,  # Ever displaced since 24 Feb 2022
      chngplr == 1           ~ 0,  # Never displaced
      chngplr == -99         ~ NA_real_
    )
  ) %>%
  # Rename hardship items
  mutate(
    income_loss_dummy      = expdi,  # Income decrease
    job_loss_dummy         = exple,  # Job loss
    physical_health_dummy  = expdph,  # Worsened physical health
    mental_health_dummy    = expdmh,  # Worsened mental health
    separation_dummy       = expsfm,  # Separation from family
    housing_damage_dummy   = expldh,  # Housing loss/damage
    property_damage_dummy  = expldp,  # Other property loss/damage
    injury_dummy           = expinj   # Injury to respondent/family
  ) %>%
  
  # Compute composite affected_index (0–10)
  mutate(
    affected_index = rowSums(
      select(
        ., 
        knew_deaths_dummy,
        displaced_dummy,
        income_loss_dummy,
        job_loss_dummy,
        physical_health_dummy,
        mental_health_dummy,
        separation_dummy,
        housing_damage_dummy,
        property_damage_dummy,
        injury_dummy
      ),
      na.rm = TRUE
    )
  ) %>% 
  # Create binary and standardized versions
  mutate(
    any_hardship_dummy  = as.integer(affected_index > 0),
    affected_index_std  = scale(affected_index),
    wave_bn = ifelse(wave == "second", 1, 0)
  )

# ----------------------------------------------
# 4. Carry wave-2 hardship values back to wave-1
# ----------------------------------------------
hardship_cols <- c(
  "knew_deaths_dummy", "family_death_dummy", "displaced_dummy",
  "income_loss_dummy", "job_loss_dummy", "physical_health_dummy",
  "mental_health_dummy", "separation_dummy", "housing_damage_dummy",
  "property_damage_dummy", "injury_dummy",
  "affected_index", "any_hardship_dummy", "affected_index_std"
)

df_final <- df_final %>%
  group_by(idnmbr) %>%
  arrange(wave, .by_group = TRUE) %>%
  mutate(across(all_of(hardship_cols),
                ~ if_else(wave == "first", .x[wave == "second"][1], .x))) %>%
  ungroup()

# ----------------------------------------------
# 5. Fit and summarize random-intercept models
# ----------------------------------------------

# Define pretty names for all predictors
pretty_names <- c(
  "knew_deaths_dummy"      = "Loss of Acquaintances/Relatives",
  "family_death_dummy"     = "Loss of Immediate Family Member",
  "displaced_dummy"        = "Displacement",
  "income_loss_dummy"      = "Income Loss",
  "job_loss_dummy"         = "Job Loss",
  "physical_health_dummy"  = "Physical Health Decline",
  "mental_health_dummy"    = "Mental Health Decline",
  "separation_dummy"       = "Family Separation",
  "housing_damage_dummy"   = "Housing Damage",
  "property_damage_dummy"  = "Property Damage",
  "injury_dummy"           = "Injury",
  "affected_index"         = "Affected Count Index",
  "affected_index_std"     = "Affected Index (std)",
  "any_hardship_dummy"     = "Any Hardship"
)

# Define models for 4 combinations: pplfair/pplhlp × Any/Index
formulas <- list(
  Fair_Any   = pplfair ~ wave_bn * any_hardship_dummy + (1 | idnmbr),
  Fair_Index = pplfair ~ wave_bn * affected_index     + (1 | idnmbr),
  Help_Any   = pplhlp  ~ wave_bn * any_hardship_dummy + (1 | idnmbr),
  Help_Index = pplhlp  ~ wave_bn * affected_index     + (1 | idnmbr)
)

models <- map(formulas, ~ lmer(.x, data = df_final, REML = FALSE, na.action = na.exclude))

# Render table
modelsummary(
  models,
  stars    = TRUE,
  fmt      = 2,
  title    = "Random-Intercept Models: Wave × Hardship Effects",
  coef_rename = c(
    "(Intercept)"                    = "Базовий рівень (хвиля 1, контроль)",
    "wave_bn"                        = "Хвиля 2 (контроль)",
    "any_hardship_dummy"             = "Будь-яка труднощі (ефект у хвилі 1)",
    "affected_index"                 = "Індекс труднощів (ефект у хвилі 1)",
    "wave_bn:any_hardship_dummy"     = "Додаткова зміна (хвиля 2 × Будь-яка)",
    "wave_bn:affected_index"         = "Додаткова зміна (хвиля 2 × Індекс)"
  )
)


# ==============================================
# 6. Separate LMMs for each hardship indicator
# ==============================================

# Fit one random-intercept model per hardship variable (outcome: pplfair)

sep_models_fair <- map(hardship_cols, function(var) {
  # Construct formula: pplfair ~ wave_bn * var + (1 | idnmbr)
  fml <- as.formula(paste0("pplfair ~ wave_bn * ", var, " + (1 | idnmbr)"))
  lmer(
    fml,
    data       = df_final,
    REML       = FALSE,
    na.action  = na.exclude
  )
})

# Name each list element by its hardship variable
names(sep_models_fair) <- hardship_cols

# Example: view summary for 'family_death_dummy'
summary(sep_models_fair$family_death_dummy)

# Render a combined table for all 'pplfair' hardship interactions
modelsummary(
  sep_models_fair,
  stars    = TRUE,
  fmt      = 2,
  title    = "Separate LMMs for Fair-Trust × Each Hardship",
  gof_omit = "IC|Log|REML|Num"
) %>%
  save_as_docx(path = "sep_hardship_fair_models.docx")

# Fit one random-intercept model per hardship variable for the "helpfulness" item
sep_models_help <- map(hardship_cols, function(var) {
  # Construct formula: pplhlp ~ wave_bn * var + (1 | idnmbr)
  fml <- as.formula(paste0("pplhlp ~ wave_bn * ", var, " + (1 | idnmbr)"))
  lmer(
    fml,
    data      = df_final,
    REML      = FALSE,
    na.action = na.exclude
  )
})

# Name each list element by its hardship variable
names(sep_models_help) <- hardship_cols

# Example: view summary for 'family_death_dummy'
summary(sep_models_help$family_death_dummy)

# Render a combined table for all 'pplhlp' hardship interactions
modelsummary(
  sep_models_help,
  stars    = TRUE,
  fmt      = 2,
  title    = "Separate LMMs for Helpfulness-Trust × Each Hardship",
  gof_omit = "IC|Log|REML|Num"
) %>%
  save_as_docx(path = "sep_hardship_helpfulness_models.docx")