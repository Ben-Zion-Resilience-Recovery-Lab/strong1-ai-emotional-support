# run_all.R
# Runs the pipeline from the project root, each script in a fresh R
# process, stopping at the first failure:
#   Rscript 01_Scripts/run_all.R

# 06 builds the publication outputs from the workbooks of 03-05 and 07-09,
# so it runs last.
scripts <- c("01_prepare_data.R", "02_data_checks.R", "03_descriptives.R", "04_models.R",
             "05_intensive_users.R", "07_table1_group_tests.R", "08_topics_by_war_exposure.R",
             "09_ace_by_age.R", "06_tables_figures.R")
for (s in scripts) {
  message("== ", s)
  if (system2(file.path(R.home("bin"), "Rscript"), file.path("01_Scripts", s)) != 0) stop(s, " failed.")
}
