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
#   Consistency (a) ACE (per SD) x age (per 10 years) interaction, 1 df,
#               adjusted for female gender: the primary test;
#               (b) ACE (per SD) x age-group interaction, joint Wald test;
#               (c) ACE OR per SD within each age group, adjusted for age
#               (years) and female gender.
# Output: Round_B/ACE_by_Age.xlsx and the candidate figure in
# Round_B/figures/.

source("01_Scripts/00_setup.R")
suppressPackageStartupMessages(library(ggplot2))

age_levels <- c("18-24", "25-34", "35-44", "45-54", "55-64", "65+")
ace_levels <- c("0", "1-3", "4 or more")
d <- readRDS(analysis_file)$participants %>%
  mutate(female = as.integer(gender == "Female"),
         age_decades = (age - 40) / 10,
         age_group = cut(age, c(17, 24, 34, 44, 54, 64, Inf), age_levels),
         ace_group = cut(ace, c(-Inf, 0, 3, Inf), ace_levels))
out_dir <- file.path(analysis_dir, "Round_B")
dir.create(file.path(out_dir, "figures"), recursive = TRUE, showWarnings = FALSE)

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

# (a) continuous interaction; (b) age-group interaction
m_cont <- fit_logit(d, c("ace_z", "age_decades", "female", "ace_z:age_decades"))
m_group <- fit_logit(d, c("ace_z", "age_group", "female", "ace_z:age_group"))
interaction_terms <- grep("^ace_z:age_group", names(coef(m_group)), value = TRUE)
interaction_tests <- bind_rows(
  odds_ratios(m_cont, c("ace_z", "ace_z:age_decades")) %>%
    mutate(model = "ACE x age (per 10 years), adjusted for female gender", .before = 1),
  wald_test(m_group, "ace_z:age_group", "ACE x age group, joint Wald test (5 df)") %>%
    rename(term = test) %>% mutate(model = "ACE x age group", .before = 1)
)

# (c) ACE OR per SD within each age group
by_age <- map_dfr(age_levels, function(g) {
  s <- filter(d, age_group == g)
  odds_ratios(fit_logit(s, c("ace_z", "age", "female")), "ace_z") %>%
    mutate(age_group = g, n = nrow(s), users = sum(s$ai_support_binary), .before = 1)
})
overall <- odds_ratios(fit_logit(d, c("ace_z", "age", "female")), "ace_z") %>%
  mutate(age_group = "All ages", n = nrow(d), users = sum(d$ai_support_binary), .before = 1)

# Monotone gradient within each age group (descriptive check)
monotone <- cells %>% group_by(age_group) %>% arrange(ace_group, .by_group = TRUE) %>%
  summarise(pct_0 = pct[1], pct_1_3 = pct[2], pct_4plus = pct[3],
            monotone_increasing = all(diff(pct) > 0), .groups = "drop")

write_analysis(
  list(Cells = cells, Margins = margins, Interaction_Tests = interaction_tests,
       ACE_OR_by_Age = bind_rows(by_age, overall), Monotonicity = monotone),
  file.path("Round_B", "ACE_by_Age.xlsx")
)


# ---- Candidate figure -------------------------------------------

palette <- setNames(c("#9DB4CF", "#3F72AF", "#112D4E"), gsub("-", "\u2013", ace_levels))
pd <- position_dodge(width = 0.35)
plot_cells <- mutate(cells, age_group = factor(gsub("-", "\u2013", age_group), gsub("-", "\u2013", age_levels)),
                     ace_group = factor(gsub("-", "\u2013", ace_group), gsub("-", "\u2013", ace_levels)))
p <- ggplot(plot_cells, aes(age_group, pct, colour = ace_group, group = ace_group)) +
  geom_line(position = pd, linewidth = 0.7) +
  geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper), position = pd, width = 0.18, linewidth = 0.45) +
  geom_point(aes(shape = ace_group), position = pd, size = 2.3, fill = "white", stroke = 0.9) +
  scale_colour_manual(values = palette, name = "Adverse childhood experiences (ACE-10)") +
  scale_shape_manual(values = c(21, 22, 24), name = "Adverse childhood experiences (ACE-10)") +
  scale_y_continuous(limits = c(0, 85), breaks = seq(0, 80, 20), labels = function(x) paste0(x, "%"),
                     expand = expansion(mult = c(0, 0.02))) +
  labs(x = "Age group (years)", y = "Used AI for emotional support\n(past 3 months)") +
  theme_classic(base_size = 10, base_family = "Helvetica") +
  theme(legend.position = "top", legend.justification = "left", legend.title = element_text(size = 9),
        axis.text = element_text(colour = "black"), panel.grid.major.y = element_line(colour = "grey90", linewidth = 0.3))
for (ext in c("pdf", "png")) {
  ggsave(file.path(out_dir, "figures", paste0("Candidate_Figure_ACE_by_Age.", ext)), p,
         width = 6.3, height = 3.8, dpi = 300, device = if (ext == "pdf") cairo_pdf else "png")
}
