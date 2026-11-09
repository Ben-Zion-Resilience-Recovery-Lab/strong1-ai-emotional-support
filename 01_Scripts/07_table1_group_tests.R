# 07_table1_group_tests.R
# Round B (Ziv's review): tests comparing AI emotional-support users with
# nonusers on every Table 1 characteristic, in the retained sample (3,602).
# Specification, fixed before the tests were run:
#   Continuous  Welch t-test (unequal variances), mean difference
#               (users - nonusers) with 95% CI and Hedges' g. The count and
#               bounded scores are skewed, so the Wilcoxon rank-sum test is
#               reported alongside as a distribution-free check.
#   Categorical Pearson chi-square with Cramer's V. Where any expected
#               count is below 5, the p-value is a Monte Carlo exact test
#               (100,000 replicates, fixed seed).
#   Holm-adjusted p-values across all Table 1 tests are given as well.
#   Sector (Arab vs Jewish, other or no religion) is added beside the
#   six-category religious affiliation shown in Table 1.
# These are descriptive group comparisons, not adjusted associations.
# Output: Round_B/Table1_Group_Tests.xlsx.

source("01_Scripts/00_setup.R")

d <- readRDS(analysis_file)$participants
user <- d$ai_support_binary == 1
dir.create(file.path(analysis_dir, "Round_B"), showWarnings = FALSE)

continuous_vars <- c(age = "Age (years)", distress_z = "Psychological distress (composite z)",
                     loneliness = "Loneliness (UCLA-3)", social_support = "Perceived social support (mMOS-SS)",
                     pcl = "PTSD symptoms (PCL-5)", ace = "Childhood adversity (ACE-10)",
                     lec_direct = "Direct trauma exposure (LEC-5)", oct7_total_14 = "October 7 and war-related exposure")
categorical_vars <- c(gender = "Gender", education = "Education", income = "Income",
                      religion = "Religious affiliation", sector = "Sector",
                      ai_general_frequency = "General AI-use frequency")

continuous <- imap_dfr(continuous_vars, function(label, v) {
  x1 <- d[[v]][user]; x0 <- d[[v]][!user]
  x1 <- x1[!is.na(x1)]; x0 <- x0[!is.na(x0)]
  w <- t.test(x1, x0, var.equal = FALSE)
  n1 <- length(x1); n0 <- length(x0)
  sp <- sqrt(((n1 - 1) * var(x1) + (n0 - 1) * var(x0)) / (n1 + n0 - 2))
  g <- (mean(x1) - mean(x0)) / sp * (1 - 3 / (4 * (n1 + n0) - 9))
  tibble(variable = v, label, n_users = n1, mean_users = mean(x1), sd_users = sd(x1),
         n_nonusers = n0, mean_nonusers = mean(x0), sd_nonusers = sd(x0),
         difference = mean(x1) - mean(x0), diff_CI_lower = w$conf.int[1], diff_CI_upper = w$conf.int[2],
         t = unname(w$statistic), df = unname(w$parameter), p_welch = w$p.value, hedges_g = g,
         p_wilcoxon = wilcox.test(x1, x0, exact = FALSE)$p.value)
})

set.seed(20261109)
categorical_tests <- imap_dfr(categorical_vars, function(label, v) {
  tab <- table(droplevels(d[[v]]), user)
  chi <- suppressWarnings(chisq.test(tab, correct = FALSE))
  sparse <- any(chi$expected < 5)
  p <- if (sparse) chisq.test(tab, simulate.p.value = TRUE, B = 1e5)$p.value else chi$p.value
  tibble(variable = v, label, chi_square = unname(chi$statistic), df = unname(chi$parameter),
         min_expected = min(chi$expected),
         test = if (sparse) "Monte Carlo exact (expected count < 5)" else "Pearson chi-square",
         p_value = p, cramers_v = sqrt(unname(chi$statistic) / (sum(tab) * (min(dim(tab)) - 1))))
})
categorical_cells <- imap_dfr(categorical_vars, function(label, v) {
  tab <- table(d[[v]], factor(user, c(TRUE, FALSE), c("Users", "Nonusers")))
  tibble(variable = v, label, level = rownames(tab), n_users = tab[, "Users"],
         pct_users = 100 * tab[, "Users"] / sum(user), n_nonusers = tab[, "Nonusers"],
         pct_nonusers = 100 * tab[, "Nonusers"] / sum(!user))
})

all_p <- c(continuous$p_welch, categorical_tests$p_value)
holm <- p.adjust(all_p, "holm")
continuous$p_holm <- holm[seq_len(nrow(continuous))]
categorical_tests$p_holm <- holm[nrow(continuous) + seq_len(nrow(categorical_tests))]

notes <- tibble(note = c(
  "Sample: all 3,602 retained participants; loneliness has 2 missing values (both nonusers).",
  "p_holm adjusts across all 14 Table 1 tests (8 continuous, 6 categorical).",
  "General AI-use frequency is structurally related to the outcome (emotional-support use was asked of everyone, but 'Not at all' general users can only be the 15 inconsistent responders); its test is reported for completeness, not as a finding.",
  "Religious affiliation and sector are two codings of one item; Table 1 currently shows the six categories."
))

write_analysis(
  list(Continuous = continuous, Categorical_Tests = categorical_tests,
       Categorical_Cells = categorical_cells, Notes = notes),
  file.path("Round_B", "Table1_Group_Tests.xlsx")
)
