# 08_topics_by_war_exposure.R
# Round B, exploratory (not a study hypothesis): October 7 and war-related
# exposure and the 12 topics discussed with AI, among the 1,059
# emotional-support users. Follows up Ziv's September observation (9
# September 2026 comments, section 1e) that more exposed users raised
# sleep and low mood, not trauma itself, more often. His exposure
# grouping (none / indirect / missile / severe) and code were not
# retained, so the analysis uses the current 14-item exposure index.
# Specification, fixed before the models were run:
#   Exposure    oct7_z, the index per SD of the retained sample (as in M5).
#   Primary     One logistic regression per topic: topic ~ oct7_z + age +
#               female (the covariates of the intensive-user model).
#   Secondary   The same without covariates.
#   Effect size OR per SD with Wald 95% CI (svyglm, as in all models).
#   Multiplicity Holm across the 12 topics, separately in each
#               specification; significance means Holm-adjusted p < .05.
#   Descriptive Topic prevalence in three exposure groups set from the
#               index distribution among users: 0-2, 3, 4 or more.
# Output: Round_B/Topics_by_War_Exposure.xlsx.

source("01_Scripts/00_setup.R")

users <- readRDS(analysis_file)$participants %>%
  filter(ai_support_binary == 1) %>%
  mutate(female = as.integer(gender == "Female"),
         exposure_group = cut(oct7_total_14, c(-Inf, 2, 3, Inf), c("0-2", "3", "4 or more")))

fit_topic <- function(topic, terms, spec) {
  m <- fit_logit(users, terms, outcome = topic)
  y <- users[[topic]]
  odds_ratios(m, "oct7_z") %>%
    transmute(specification = spec, variable = !!topic, topic = item_labels[[!!topic]],
              n = length(y), n_topic = sum(y), pct_topic = 100 * mean(y),
              OR_per_SD = OR, CI_lower, CI_upper, p_value)
}

results <- bind_rows(
  map_dfr(q05_items, fit_topic, c("oct7_z", "age", "female"), "Primary: adjusted for age and female gender"),
  map_dfr(q05_items, fit_topic, "oct7_z", "Secondary: unadjusted")
) %>%
  group_by(specification) %>%
  mutate(p_holm = p.adjust(p_value, "holm"), significant_holm = p_holm < .05,
         nominal_only = p_value < .05 & p_holm >= .05) %>%
  ungroup()

by_group <- map_dfr(q05_items, function(topic) {
  users %>% group_by(exposure_group) %>%
    summarise(n = n(), k = sum(.data[[topic]]), pct = 100 * mean(.data[[topic]]), .groups = "drop") %>%
    mutate(variable = topic, topic = item_labels[[topic]], .before = 1)
})

exposure_summary <- users %>%
  summarise(n = n(), mean = mean(oct7_total_14), sd = sd(oct7_total_14), median = median(oct7_total_14),
            min = min(oct7_total_14), max = max(oct7_total_14),
            mean_z = mean(oct7_z), sd_z = sd(oct7_z))

write_analysis(
  list(Topic_Models = results, Topics_by_Exposure_Group = by_group,
       Group_Sizes = count(users, exposure_group), Exposure_Summary = exposure_summary),
  file.path("Round_B", "Topics_by_War_Exposure.xlsx")
)
