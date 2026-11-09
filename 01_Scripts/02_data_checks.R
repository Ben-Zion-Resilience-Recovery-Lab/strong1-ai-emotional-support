# 02_data_checks.R
# Data-integrity checks on the analysis dataset: central QC flow, item
# coding and scale scores, LEC-5 direct exposure, reliability, AI follow-up and PCL-5 routing,
# and every analytic sample. Stops if any audited value is not
# reproduced. Output: Data_Checks.xlsx.

source("01_Scripts/00_setup.R")

data <- readRDS(analysis_file)
d <- data$participants
screening <- data$screening


# ---- Central STRONG QC flow --------------------------------------

qc_flags <- c("MissingID", "TestID", "Incomplete", "BeforeSurveyMadePublic", "Inattentive", "Duplicates")
flags <- as.matrix(screening[qc_flags])
flags[is.na(flags)] <- FALSE
excluded <- !screening$Included
# Excluded with no coded flag: completed in under 10 minutes.
fast_completion <- excluded & rowSums(flags) == 0 & screening$DurationInSeconds < 600

qc_flow <- tibble(
  stage = c("Records screened", "Excluded by central QC", "Retained", "Participants in the analysis dataset"),
  n = c(nrow(screening), sum(excluded), sum(!excluded), nrow(d))
)
qc_reasons <- tibble(
  reason = c(qc_flags, "Completion under 600 s, no other flag"),
  n_excluded = c(colSums(flags[excluded, ] == 1), sum(fast_completion, na.rm = TRUE)),
  note = c(rep("Flags can overlap; do not sum", 6), "Mutually exclusive with the flags above")
)

if (!identical(qc_flow$n, c(4979L, 1377L, 3602L, 3602L)) ||
    !setequal(d$UserID, screening$UserID[!excluded]) ||
    sum(fast_completion, na.rm = TRUE) != 57 ||
    any(!excluded & screening$DurationInSeconds < 600, na.rm = TRUE)) {
  stop("Central QC flow does not reproduce the audited counts.")
}


# ---- Item coding, scale scores and reliability -------------------

scales <- list(
  `PHQ-9` = list(items = sprintf("PHQ9_Q%02d", 1:9), codes = 0:3, score = d$PHQ_Score),
  `GAD-7` = list(items = sprintf("GAD7_Q%02d", 1:7), codes = 0:3, score = d$GAD_Score),
  # Items 4, 5, 7 and 8 are stored reverse-scored.
  `PSS-10` = list(items = sprintf("PSS_Q%02d", 1:10), codes = 0:4, score = d$PSS_Score),
  # -99: technical missing code; the score is then not computable.
  `UCLA-3` = list(items = sprintf("UCLA_Loneliness_Q%02d", 1:3), codes = c(-99, 1:3), score = d$loneliness),
  `mMOS-SS` = list(items = sprintf("MOSSS_Q%02d", 1:8), codes = 1:5, score = d$MOSSS_Score),
  `PCL-5` = list(items = sprintf("PCL_Q%02d", 1:20), codes = 0:4, score = d$PCL_Total_Score),
  `ACE-10` = list(items = sprintf("ACE_Q%02d", 1:10), codes = 0:1, score = d$ACE_Score),
  # Code 1 = happened to me (5 = not sure); items 14-15 are never direct.
  `LEC-5` = list(items = lec_items, codes = 0:5, score = d$lec_direct)
)
item_matrix <- function(scale) {
  x <- as.matrix(d[scales[[scale]]$items])
  x[x == -99] <- NA
  x
}

score_validation <- imap_dfr(scales, function(s, scale) {
  x <- item_matrix(scale)
  calculated <- switch(scale, `mMOS-SS` = (rowMeans(x) - 1) * 25, `LEC-5` = rowSums(x[, lec_direct_items] == 1), rowSums(x))
  both <- !is.na(s$score) & !is.na(calculated)
  tibble(scale, items = length(s$items), codes_valid = all(as.matrix(d[s$items]) %in% s$codes),
         n_compared = sum(both), n_mismatch = sum(abs(s$score[both] - calculated[both]) > 1e-8))
})
if (!all(score_validation$codes_valid) || any(score_validation$n_mismatch > 0)) {
  stop("Item codes or supplied scale scores are invalid.")
}


# ---- LEC-5 direct exposure ---------------------------------------
# The corrected score is verified from the items above and here by a second
# route: the supplied 17-item score minus what items 14-15 actually contributed.

lec <- as.matrix(d[lec_items])
q14 <- lec[, "LEC5_Q14"]
q15 <- lec[, "LEC5_Q15"]
removed <- (q14 == 1) + (q15 == 1)
other_direct <- rowSums(lec[, lec_direct_items] == 1)
# Exposure endorsements on items 14-15, for validation only (never a predictor):
# codes 1-4; 0 (did not happen to me) and 5 (not sure) are non-exposure.
indirect_14_15 <- rowSums(lec[, c("LEC5_Q14", "LEC5_Q15")] >= 1 & lec[, c("LEC5_Q14", "LEC5_Q15")] <= 4)

# Code 1 must mean "happened to me" in Hebrew and Arabic, and no other code may.
lec_labels <- filter(data$value_labels, variable == "LEC5_Q01-Q17")
happened_to_me <- c("\u05e7\u05e8\u05d4 \u05dc\u05d9", "\u062d\u062f\u062b \u0645\u0639\u064a \u0634\u062e\u0635\u064a\u0627\u064b")
if (!setequal(lec_labels$label[lec_labels$code == 1], happened_to_me) ||
    any(lec_labels$label[lec_labels$code != 1] %in% happened_to_me)) {
  stop("LEC-5 code 1 is not labelled 'happened to me'.")
}

# Each case: its size (with the audited size where one exists) and the
# change in score that must hold within it.
change <- d$LEC5_Direct_Score - d$lec_direct
other_only <- removed == 0 & (q14 >= 2 | q15 >= 2)
no_exposure <- rowSums(lec == 0 | lec == 5) == ncol(lec)
boundary <- function(label, keep, holds, expected = NA) {
  tibble(case = label, n = sum(keep), expected_n = expected, passed = holds && (is.na(expected) || sum(keep) == expected))
}
lec_boundaries <- bind_rows(
  boundary("Item 14 = happened to me", q14 == 1, TRUE, 86),
  boundary("Item 15 = happened to me", q15 == 1, TRUE, 1316),
  boundary("Item 14 or 15 = happened to me", removed > 0, TRUE, 1329),
  boundary("Both items = happened to me: score falls by 2", removed == 2, all(change[removed == 2] == 2), 73),
  boundary("One item = happened to me: score falls by 1", removed == 1, all(change[removed == 1] == 1), 1256),
  boundary("Neither item = happened to me: score unchanged", removed == 0, all(change[removed == 0] == 0), 2273),
  boundary("Items 14-15 coded 2-5, neither 1: score unchanged", other_only, all(change[other_only] == 0)),
  boundary("Direct exposure only on items 14-15: corrected score 0", other_direct == 0 & removed > 0,
           all(d$lec_direct[other_direct == 0 & removed > 0] == 0)),
  boundary("Supplied score 0: corrected score 0", d$LEC5_Direct_Score == 0, all(d$lec_direct[d$LEC5_Direct_Score == 0] == 0)),
  boundary("Supplied score 1: corrected score 0 or 1", d$LEC5_Direct_Score == 1, all(d$lec_direct[d$LEC5_Direct_Score == 1] %in% 0:1)),
  boundary("Only 'not sure' or 'did not happen to me' on all items: scores 0", no_exposure,
           all(d$lec_direct[no_exposure] == 0 & indirect_14_15[no_exposure] == 0))
)

lec_score_checks <- tibble(
  check = c("Supplied score equals the count of code 1 across all 17 items",
            "Corrected score equals the supplied score minus the contributions of items 14-15",
            "Corrected score is complete, whole and within 0-15",
            "Indirect count of items 14-15 is within 0-2 and excludes codes 0 and 5",
            "Standardized score uses the mean and SD of the 3,602 retained participants"),
  passed = c(
    all(rowSums(lec == 1) == d$LEC5_Direct_Score),
    all(d$LEC5_Direct_Score - removed == d$lec_direct),
    !anyNA(d$lec_direct) && all(d$lec_direct %in% 0:15),
    all(indirect_14_15 %in% 0:2) && all(indirect_14_15 == (q14 %in% 1:4) + (q15 %in% 1:4)),
    all(abs((d$lec_direct - mean(d$lec_direct)) / sd(d$lec_direct) - d$lec_direct_z) < 1e-12)
  )
)
if (!all(lec_boundaries$passed) || !all(lec_score_checks$passed)) stop("LEC-5 direct exposure does not verify.")

lec_groups <- list(`All retained` = rep(TRUE, nrow(d)), `Model sample` = d$model_sample,
                   `Non-users` = d$ai_support_binary == 0, Users = d$ai_support_binary == 1)
lec_comparison <- imap_dfr(lec_groups, function(keep, group) {
  map_dfr(c(`Supplied (17 items)` = "LEC5_Direct_Score", `Corrected (15 items)` = "lec_direct"), function(v) {
    x <- d[[v]][keep]
    tibble(n = length(x), n_missing = sum(is.na(x)), mean = mean(x), sd = sd(x), median = median(x), min = min(x), max = max(x))
  }, .id = "score") %>% mutate(group = group, .before = 1)
})
lec_change <- tibble(
  summary = c("Reduced by 0", "Reduced by 1", "Reduced by 2", "Correlation, supplied vs corrected", "Correlation, standardized supplied vs corrected"),
  value = c(sum(removed == 0), sum(removed == 1), sum(removed == 2), cor(d$LEC5_Direct_Score, d$lec_direct),
            cor(scale(d$LEC5_Direct_Score), d$lec_direct_z))
)
lec_item_codes <- map_dfr(lec_items, ~ tibble(item = .x, code = 0:5, n = as.vector(table(factor(d[[.x]], 0:5)))))
lec_indirect <- tibble(indirect_endorsements = 0:2, n = as.vector(table(factor(indirect_14_15, 0:2))))


cronbach_alpha <- function(scale, x, note = NA_character_) {
  x <- x[complete.cases(x), , drop = FALSE]
  k <- ncol(x)
  tibble(scale, items = k, n = nrow(x), alpha = k / (k - 1) * (1 - sum(apply(x, 2, var)) / var(rowSums(x))), note)
}
reliability <- bind_rows(
  map_dfr(c("PHQ-9", "GAD-7", "PSS-10", "UCLA-3", "mMOS-SS"), ~ cronbach_alpha(.x, item_matrix(.x))),
  cronbach_alpha("PCL-5", item_matrix("PCL-5")[d$pcl_routed == 0, ], "Participants administered the PCL-5"),
  cronbach_alpha("Distress composite", as.matrix(d[c("phq_z", "gad_z", "pss_z")]), "Standardized PHQ-9, GAD-7 and PSS-10 totals")
)


# ---- Routing and analytic samples --------------------------------

# AI follow-up items were answered by exactly the emotional-support users;
# participants routed past the PCL-5 are all scored 0.
follow_ups <- c("MH_AI_Q03", q04_items, q05_items)
if (!all((rowSums(is.na(d[follow_ups])) == 0) == (d$ai_support_binary == 1)) ||
    any(d$pcl[d$pcl_routed == 1] != 0)) {
  stop("AI follow-up or PCL-5 routing is not as expected.")
}

# Every analytic sample with its number of emotional-support users.
es_user <- d$ai_support_binary == 1
intensive <- es_user & d$general_ai_daily == 1
samples <- list(
  "Retained T1 sample (descriptives, Table 1, Figure 1)" = rep(TRUE, nrow(d)),
  "Excluded from the models: loneliness score not computable" = !d$model_sample,
  "Hierarchical models M1-M5 (complete cases)" = d$model_sample,
  "General AI users" = d$general_ai_user == 1,
  "Secondary model (general AI users, complete cases)" = d$model_sample & d$general_ai_user == 1,
  "Emotional-support use without general AI use (inconsistent)" = d$ai_logic_inconsistent == 1,
  "Sensitivity: inconsistent responders excluded" = d$model_sample & d$ai_logic_inconsistent == 0,
  "Routed past the PCL-5 (no index event; scored 0)" = d$pcl_routed == 1,
  "  of whom in the hierarchical-model sample" = d$model_sample & d$pcl_routed == 1,
  "Sensitivity: administered the PCL-5" = d$model_sample & d$pcl_routed == 0,
  "AI emotional-support users (Table S1)" = es_user,
  "Daily emotional-support users (almost every day / every day)" = es_user & d$ai_support_daily == 1,
  "Emotional-support users with daily general AI use (21 comparisons)" = intensive,
  "  of whom daily emotional-support users (comparisons)" = intensive & d$ai_support_daily == 1,
  "Intensive-user model (gender 'Other' excluded)" = intensive & d$gender != "Other",
  "  of whom daily emotional-support users (model)" = intensive & d$gender != "Other" & d$ai_support_daily == 1
)
sample_flow <- imap_dfr(samples, ~ tibble(sample = .y, n = sum(.x), emotional_support_users = sum(es_user[.x])))

audited_samples <- c(3602, 2, 3600, 2785, 2784, 15, 3585, 533, 532, 3068, 1059, 115, 406, 111, 404, 111)
audited_users <- c(1059, 0, 1059, 1044, 1044, 15, 1044, 104, 104, 955, 1059, 115, 406, 111, 404, 111)
if (!identical(sample_flow$n, as.integer(audited_samples)) ||
    !identical(sample_flow$emotional_support_users, as.integer(audited_users))) {
  stop("Analytic samples do not reproduce the audited counts.")
}

# Sector: Arab = Muslim, Christian or Druze; every other affiliation (Jewish,
# atheist or no religion, other) is in the comparison group.
sector_coding <- count(d, religion, sector, name = "n_retained") %>%
  left_join(count(filter(d, model_sample), religion, name = "n_model_sample"), by = "religion")
if (!all(d$sector[d$religion %in% c("Muslim", "Christian", "Druze")] == "Arab") ||
    !all(d$sector[!d$religion %in% c("Muslim", "Christian", "Druze")] == "Jewish, other or no religion") ||
    anyNA(d$sector) || sum(d$sector == "Arab") != 210 || sum(d$sector[d$model_sample] == "Arab") != 209) {
  stop("Sector coding does not reproduce the audited counts.")
}

# Missing values other than the follow-up items skipped by design.
missingness <- tibble(variable = names(d), n_missing = colSums(is.na(d))) %>%
  filter(n_missing > 0, !variable %in% c(follow_ups, "ai_helpfulness", "ai_support_daily"))

write_analysis(
  list(
    QC_Flow = qc_flow,
    QC_Reasons = qc_reasons,
    Score_Validation = score_validation,
    LEC_Score_Checks = lec_score_checks,
    LEC_Boundary_Cases = lec_boundaries,
    LEC_Comparison = lec_comparison,
    LEC_Change = lec_change,
    LEC_Item_Codes = lec_item_codes,
    LEC_Indirect_Validation = lec_indirect,
    Reliability = reliability,
    Response_Coding = data$value_labels,
    Sample_Flow = sample_flow,
    Sector_Coding = sector_coding,
    Missingness = missingness
  ),
  "Data_Checks.xlsx"
)
