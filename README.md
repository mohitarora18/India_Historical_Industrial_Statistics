# Replication Package

**Mismeasuring Industrial Change: A Classification-Break Artifact in India's Historical Industrial Statistics**

Mohit Arora
Department of Economics, Union College

## Overview

This repository contains the data and code needed to reproduce all tables and
figures in the paper. The paper documents and diagnoses a classification-change
artifact that arises when India's Second Five-Year Plan (1956–61) industrial
statistics are spliced across the 1959 switch from the Sample Survey of
Manufacturing Industries (SSMI) to the Annual Survey of Industries (ASI). A
difference-in-differences (DD) research design applied to plan-targeted
industries is used as the vehicle through which this classification-change
artifact is exposed and diagnosed.

## Repository Structure

```
.
├── README.md                                  <- this file
├── LICENSE.md                                 <- CC BY 4.0
├── indian_industrial_statistics.R             <- full analysis script
├── SSMI_ASI.xlsx                              <- raw data workbook (all sheets below)
├── harmonized_panel_analysis_sample.csv       <- analysis-ready harmonized panel
└── nonharmonized_pooled_analysis_sample.csv   <- analysis-ready pooled dataset
```

The two `.csv` files are convenience outputs of the data-construction stage of
the script (see "How to Reproduce" below); they let a user re-run the
regressions directly without re-running the harmonization and merging steps.
Running the script from scratch on `SSMI_ASI.xlsx` regenerates both files.

## Data

All raw data live in a single workbook, `SSMI_ASI.xlsx`, with the following
sheets:

| Sheet | Contents |
|---|---|
| `ASI_Sample_Sector_1959_65` | ASI Sample Sector industry statistics, 1959–65 |
| `ASI_Census_Sector_1959_65` | ASI Census Sector industry statistics, 1959–65 |
| `ASI_Sample_Census_Harmonization` | Correspondence between ASI Sample and Census sector codes |
| `ASI_SSMI_Harmonization` | Correspondence between ASI and SSMI classification codes, used to build the harmonized panel |
| `Industrial_Price_Sectors` | Correspondence between ASI/SSMI sectors and wholesale price index categories |
| `Industrial_Prices` | Wholesale price index series, used to deflate monetary outcomes and construct control variables |
| `Learning_ASI_Sample` | Additional ASI Sample Sector variables |
| `Learning_ASI_Census` | Additional ASI Census Sector variables |
| `SSMI_1951_58` | SSMI industry statistics, 1951–58 |
| `Learning_SSMI` | Additional SSMI variables |
| `controls_1951_53` | Pre-treatment (1951–53) control variable inputs |

Sources for the underlying statistics and the industrial classification
concordances are documented in the paper (Section 3 and Appendix A).

## Software Requirements

The script was run under R (>= 4.2) with the following packages:

```r
readxl, writexl, tidyverse, fixest, ggplot2, ggfixest, fwildclusterboot,
kableExtra, broom
```

Install with:

```r
install.packages(c("readxl", "writexl", "tidyverse", "fixest", "ggplot2",
                    "ggfixest", "fwildclusterboot", "kableExtra", "broom"))
```

## How to Reproduce

1. Place `SSMI_ASI.xlsx` in a `Data/` folder and update the file paths at the
   top of `indian_industrial_statistics.R` (search for
   `read_excel(...)`) to point to its location on your machine. Likewise
   update the `ggsave(...)` / `write_xlsx(...)` / `save_kable(...)` output
   paths (currently pointing to `~/Desktop/Results/...`) to your preferred
   output directory.
2. Run the script top to bottom in R. It performs, in order:
   - **Data construction**: merges the ASI Sample and Census sectors,
     harmonizes SSMI and ASI industries into a single panel using the
     `ASI_SSMI_Harmonization` correspondence, and separately constructs a
     non-harmonized pooled dataset that stacks SSMI (1951–58) and ASI
     (1959–65) data without attempting cross-classification alignment.
   - **Deflation and control variables**: deflates monetary outcomes using
     the wholesale price series and constructs pre-treatment (1951–53)
     control variables (wage bill per worker, worker productivity, plant
     size, material input per worker).
   - **Main results, harmonized panel**: static DD estimates (Table 2),
     with wild cluster bootstrap (WCB) inference (`fwildclusterboot::boottest()`,
     9,999 replications, clustered at the industry level); dynamic
     event-study estimates without and with controls (Figure 1, Tables
     C1–C3, Figure C1).
   - **Randomization inference**: for all four harmonized-panel
     specifications (static without controls, static with controls,
     dynamic without controls, dynamic with controls), the script also
     computes cluster-robust t-statistic-based randomization-inference
     p-values, following MacKinnon and Webb (2020, *Journal of
     Econometrics*). The "targeted" label is reassigned at random among
     the 36 industries in the harmonized panel (holding the treated count
     fixed at 5) across 5,000 permutations for the static specifications
     and 2,000 for the dynamic specifications, and each permutation's
     cluster-robust t-statistic is compared against the actual one.
   - **Non-harmonized pooled dataset results**: event-study estimates
     (Figure 2) using cluster-robust standard errors.
   - **Placebo diagnostic**: simulates industry-level data with no true
     treatment effect but the same 1959 classification-switch missingness
     pattern (57 industries observed only through 1958, 42 industries only
     from 1959, years 1954–56 missing), re-estimated on the pooled
     dataset's structure (Figures 3–4).
   - **Summary statistics** (Appendix B, Tables B1–B4), restricted to the
     four outcome variables with complete 1959 coverage (see below).
3. All tables are written as both `.xlsx` and `.tex`; all figures as `.png`.
   `.tex` tables are pre-formatted to match the paper's table style and can
   be included directly via `\input{}`.

## Outcome Variables

The paper restricts every table and figure to the four outcome variables with
complete 1959 coverage: Gross Output, Gross Value Added, Number of Employees,
and Number of Factories. Number of Workers, Material Input, Fixed Capital, and
Working Capital are missing for the ASI Census sector in 1959 and are
therefore excluded from all analysis. These four variables are the only ones
the script logs and carries through to the results section; other variables
present in the raw data are used only for data construction (e.g.,
deflators) or dropped entirely.

## Reproducibility Note on WCB Inference

`fwildclusterboot::boottest()` draws on an internal RNG stream (`dqrng`)
separate from base R's `set.seed()`. The script fixes a `set.seed()` call
immediately before each `boottest()` call, keyed by a stable per-variable
seed lookup (`var_seeds`) rather than by position in the outcome-variable
vector, so that re-running the script reproduces the same bootstrap draws
regardless of the order or number of outcome variables considered. WCB
p-values may still differ in the second or third decimal place across
machines/R versions due to possible multithreading inside `boottest()`; this
does not affect any qualitative conclusion in the paper.

## Citation

If you use this dataset or code, please cite:

Arora, M. (2026). "Mismeasuring Industrial Change: A Classification-Break
Artifact in India's Historical Industrial Statistics."

## Contact

Mohit Arora, Department of Economics, Union College
marorajnu@gmail.com
