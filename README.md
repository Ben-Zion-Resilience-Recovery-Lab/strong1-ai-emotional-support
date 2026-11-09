# The Burden We Bring to AI

Analysis code for a cross-sectional study of AI emotional-support use in the baseline (T1) wave of STRONG-1, the quota-sampled online longitudinal cohort of the STRONG study (Stress and Trauma Resilience: Opportunities for National Growth). The primary outcome is any use of AI for emotional support or coping with mental distress in the preceding three months. The analyses are a five-step hierarchical logistic regression (M1 demographics, M2 psychological distress, M3 loneliness and perceived social support, M4 PTSD symptoms, M5 trauma exposure), one secondary model among general AI users, three sensitivity analyses, an exploratory comparison of daily with less frequent emotional-support users, and descriptive analyses of users' experiences.

## Data and privacy

This repository contains code only. STRONG-1 participant-level data contain sensitive mental health information; access is governed by the STRONG study team. STRONG-1 was approved by the Ethics Committee of the Faculty of Social Welfare and Health Sciences, University of Haifa (No. 045/26).

`data/` and all generated outputs are git-ignored. After cloning, enable the pre-commit guard, which rejects participant data and generated outputs:

```bash
git config core.hooksPath .githooks
```

## Requirements

R 4.5.1 or later with these CRAN packages:

```r
install.packages(c("dplyr", "purrr", "readxl", "writexl", "survey", "car",
                   "ggplot2", "ggtext", "patchwork", "officer", "flextable"))
```

The outputs were verified with `officer` 0.7.7 and `flextable` 0.10.3. Each analysis workbook records its R and package versions in a `Run_Info` sheet.

## Input data

An authorized researcher places the STRONG-1 workbook at `data/STRONG1Data.xlsx`, with the sheets `T1_Values_200826` (responses), `T1_Labels_200826` (response labels) and `T1_Filtering_200826` (central quality-control records). The workbook is only read, never modified.

`01_prepare_data.R` writes the cleaned analysis dataset `data/STRONG1_T1_analysis_data.rds`, the only data source of the later scripts. It holds the 3,602 retained participants with every item and supplied score of the questionnaires analyzed (PHQ-9, GAD-7, PSS-10, UCLA-3, mMOS-SS, PCL-5 with its routing item, ACE-10, LEC-5, the October 7 exposure items and the study's AI items), the demographic, administration and design variables, and the derived analysis variables; plus the 4,979 central QC records and the response labels of the coded items.

## Running the pipeline

From the project root:

```bash
Rscript 01_Scripts/run_all.R
```

Each script runs in its own R process, in order, and the pipeline stops at the first failure.

| Script | Contents | Output |
| --- | --- | --- |
| `00_setup.R` | Paths, packages and the model helpers; sourced by every script | |
| `01_prepare_data.R` | Variable selection, response-code checks, scoring and derived variables | `data/STRONG1_T1_analysis_data.rds` |
| `02_data_checks.R` | QC flow, item codes and scale scores, LEC-5 direct exposure, sector coding, the 14 war-exposure indicators, reliability, routing, analytic samples, missingness | `Data_Checks.xlsx` |
| `03_descriptives.R` | Prevalence and frequency of AI use, Table 1 summaries, experiences and topics | `Descriptives.xlsx` |
| `04_models.R` | Hierarchical models M1–M5, secondary model, sensitivity analyses | `Models.xlsx` |
| `05_intensive_users.R` | Daily vs less frequent users: adjusted model and 21 Holm-corrected comparisons | `Intensive_Users.xlsx` |
| `06_tables_figures.R` | Publication tables and figures, built only from the workbooks of 03–05 and 07–09; runs last | Tables, figures, source data |
| `07_table1_group_tests.R` | Round B: users vs nonusers on every Table 1 characteristic (Welch t, chi-square or Monte Carlo exact) | `Round_B/Table1_Group_Tests.xlsx` |
| `08_topics_by_war_exposure.R` | Round B, exploratory: October 7 and war-related exposure and the 12 AI topics among users, Holm-corrected | `Round_B/Topics_by_War_Exposure.xlsx` |
| `09_ace_by_age.R` | Round B, exploratory: AI emotional-support use by ACE group across age groups; ACE x age tests; candidate figure | `Round_B/ACE_by_Age.xlsx`, `Round_B/figures/` |

`02_data_checks.R` stops if the audited QC counts, scale scores, LEC-5 direct-exposure checks, routing or analytic sample sizes are not reproduced.

## Analysis notes

- All estimates are unweighted (`TemporaryWeights` is a placeholder of 1s). Models are logistic regressions fitted with `survey::svyglm` on an equal-probability design, giving design-based standard errors and Wald tests. Age is per year; other continuous predictors are per sample SD.
- The models use the 3,600 complete cases; the two excluded participants lack only a loneliness score (UCLA-3 not computable).
- The 533 participants who reported no index trauma were routed past the PCL-5 and are scored 0 (532 in the model sample); a sensitivity analysis excludes them.
- LEC-5 direct exposure (0–15) is calculated from the items: "Happened to me" responses (code 1) on items 1–13 and 16–17. Items 14–15 (sudden violent death; in the administered Hebrew and Arabic versions, sudden unexpected death of a close person) are classified as indirect for every response; "Not sure" and "Did not happen to me" are non-exposure. The supplied `LEC5_Direct_Score` counts all 17 items and is retained only for comparison in `Data_Checks.xlsx`.
- Sector (M1 covariate) is coded Arab (Muslim, Christian or Druze; 210 retained, 209 in the model sample) versus a comparison group of Jewish, other religion and no religion (3,392; 3,391). Participants reporting another religion or no religion (32) were in the non-Jewish group before 9 November 2026.
- The October 7 and war-related exposure index is the STRONG team's 14-item count, including relocation during the Iran war.

## Outputs

| Output | Contents |
| --- | --- |
| `02_Analysis_Outputs/` | Aggregate analysis workbooks, with every reported estimate |
| `03_Main_Outputs/tables/` | Table 1 (participant characteristics, with user vs non-user tests), Table 2 (hierarchical models) |
| `03_Main_Outputs/figures/` | Figure 1 (experiences and topics), Figure 2 (daily vs less frequent users), Figure 3 (ACE across age groups; post hoc); PNG previews in `previews/` |
| `04_Supplement/tables/` | Tables S1 (AI experiences), S2 (sensitivity), S3 (general AI users), S4 (intensive users), S5 (war-related exposure and topics; post hoc) |
| `*/source_data/` | Formatted table contents and figure data, including every coefficient of the Table S3 model |

All outputs are regenerated by the pipeline and are not tracked.

## Citation

> Mendelson, A., & Ben-Zion, Z. (2026). *The burden we bring to AI: Who seeks emotional support from artificial intelligence?* [Manuscript in preparation].

STRONG study protocol: Moallem, D., Maaravi-Hesseg, R., Panitz, D., Pietrzak, R. H., & Ben-Zion, Z. (2026). *The STRONG study: A multi-tiered, multimodal investigation of resilience and recovery following prolonged collective adversity* [Preprint]. medRxiv. https://doi.org/10.64898/2026.08.17.26360579

## Contact and license

Adrian Mendelson, Ben-Zion Resilience & Recovery Lab, University of Haifa (Amende12@campus.haifa.ac.il). Questions about data access go to the STRONG study team.

Released under the MIT License; see [`LICENSE`](LICENSE).
