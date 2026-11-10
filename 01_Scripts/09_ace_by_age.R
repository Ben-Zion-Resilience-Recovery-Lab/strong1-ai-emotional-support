# 09_ace_by_age.R
# Round B, exploratory (not a study hypothesis): childhood adversity
# (ACE-10) and AI emotional-support use across age groups, in the 3,602
# retained participants. Restores the display in Ziv's 9 September 2026
# comments (section 1c) with current definitions.
# Specification, fixed from the cell sizes before outcomes were examined:
#   ACE groups  0, 1-3, 4 or more (as in Ziv's table).
#   Age groups  18-24, 25-34, 35-44, 45-54, 55-64, 65+. 65-74 and 75+ are
#               pooled because 75+ with ACE 4+ has n = 4; every plotted
#               cell then has n >= 34 (rule: plot only cells with n >= 30).
#   Uncertainty Proportions with logit 95% CIs (as in 03_descriptives.R).
#   Consistency ACE (per SD) x age (per 10 years) interaction, 1 df,
#               adjusted for age and female gender.
# Output: Round_B/ACE_by_Age.xlsx. The publication figure (Figure 3, the
# descriptive cells only) is drawn by 06_tables_figures.R.

source("01_Scripts/00_setup.R")

age_levels <- c("18-24", "25-34", "35-44", "45-54", "55-64", "65+")
ace_levels <- c("0", "1-3", "4 or more")
d <- readRDS(analysis_file)$participants %>%
  mutate(female = as.integer(gender == "Female"),
         age_decades = (age - 40) / 10,
         age_group = cut(age, c(17, 24, 34, 44, 54, 64, Inf), age_levels),
         ace_group = cut(ace, c(-Inf, 0, 3, Inf), ace_levels))
out_dir <- file.path(analysis_dir, "Round_B")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

prop_ci <- function(x) {
  ci <- confint(svyciprop(~x, svydesign(ids = ~1, probs = ~1, data = data.frame(x)), method = "logit"))
  tibble(n = length(x), users = sum(x), pct = 100 * mean(x), CI_lower = 100 * ci[1], CI_upper = 100 * ci[2])
}
cells <- d %>%
  group_by(age_group, ace_group) %>%
  group_modify(~ prop_ci(.x$ai_support_binary)) %>%
  ungroup() %>%
  mutate(plotted = n >= 30)
if (!all(cells$plotted)) stop("A cell has n < 30; revisit the age grouping.")
margins <- bind_rows(
  d %>% group_by(group = ace_group) %>% group_modify(~ prop_ci(.x$ai_support_binary)) %>% ungroup() %>%
    mutate(margin = "ACE group", group = as.character(group), .before = 1),
  d %>% group_by(group = age_group) %>% group_modify(~ prop_ci(.x$ai_support_binary)) %>% ungroup() %>%
    mutate(margin = "Age group", group = as.character(group), .before = 1)
)

# ACE x age (continuous) interaction
m_cont <- fit_logit(d, c("ace_z", "age_decades", "female", "ace_z:age_decades"))
interaction_tests <- odds_ratios(m_cont, c("ace_z", "ace_z:age_decades")) %>%
  mutate(model = "ACE x age (per 10 years), adjusted for age and female gender", .before = 1)

# Monotone gradient within each age group (descriptive check)
monotone <- cells %>% group_by(age_group) %>% arrange(ace_group, .by_group = TRUE) %>%
  summarise(pct_0 = pct[1], pct_1_3 = pct[2], pct_4plus = pct[3],
            monotone_increasing = all(diff(pct) > 0), .groups = "drop")

write_analysis(
  list(Cells = cells, Margins = margins, Interaction_Tests = interaction_tests,
       Monotonicity = monotone),
  file.path("Round_B", "ACE_by_Age.xlsx")
)
