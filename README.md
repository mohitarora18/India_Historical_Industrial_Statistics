# Replication Package: Revisiting the Nehru-Mahalanobis Industrial Policy
This repository contains the data and R code underlying "Revisiting the
Nehru-Mahalanobis Industrial Policy: India's State-Led Heavy Industry Drive"
(Mohit Arora, Union College). It reproduces all tables and figures in the
paper and its appendix.
## Files
- `01_build_data.R` — reads the raw workbook and builds the three
  analysis-ready panels below, then writes each one out as a CSV. Run
  this first if you want to reproduce those CSVs yourself; otherwise the
  CSVs are already included and you can skip straight to
  `02_run_regressions.R`.
- `02_run_regressions.R` — the complete analysis script. Running it end
  to end (after adjusting the file paths noted below) reproduces every
  table and figure in the paper. It loads the three CSVs directly rather
  than rebuilding them, so it runs on its own without needing
  `01_build_data.R` or most of `SSMI_ASI.xlsx` (it does still read two
  small reference sheets directly — see the note at the top of the
  script).
- `SSMI_ASI.xlsx` — the raw industry-level data, compiled by hand from the
  Survey of Small and Medium Industries (SSMI, 1951-58) and the Annual
  Survey of Industries (ASI, 1959-65), plus supporting price and
  input-output data used to construct deflators, treatment status, and
  linkage-based robustness checks.
- `harmonized_panel_analysis_sample.csv` — the analysis-ready harmonized
  panel (`industry_panel` in the scripts): 36 industries spanning 1951-65,
  with SSMI- and ASI-era industry codes crosswalked into a single
  consistent classification, plus every constructed variable used in the
  regressions (deflated GO/GVA/Material Input/Capital, treatment and
  post-treatment dummies, pre-trend controls, first-difference variables,
  and the sector/time fixed-effect identifiers).
- `nonharmonized_pooled_analysis_sample.csv` — the analysis-ready
  non-harmonized repeated cross-section (`industry_repeated_cross_section`
  in the scripts): the same underlying SSMI/ASI data, but with each era's
  industries left in their own native classification rather than
  crosswalked, used for the diagnostic and robustness specifications
  described in the paper.
- `asi_factory_analysis_sample.csv` — the raw ASI-era factory panel
  (`asi_factory` in the scripts): industry-by-year GO/GI/GVA, workers,
  employees, capital, and factory counts for 1959-65 in the ASI's own
  classification, before any of the harmonization or deflation applied to
  the other two panels. This is the starting point for the ASI-only
  (1960-65) Learning-by-Doing regressions (Table 8).

  These three files are derived, not primary, data — they are fully
  reconstructed from `SSMI_ASI.xlsx` by `01_build_data.R` and are included
  only to save a replicator the trouble of rerunning the construction
  step. `SSMI_ASI.xlsx` and `01_build_data.R` remain the primary record:
  any question about how a specific figure in one of these CSVs was
  derived can be traced back through that script to the underlying raw
  source data.
## Before running
Both scripts read from and write to hard-coded local paths.
`01_build_data.R` reads raw data from `~/Desktop/Research/JMP/Data/SSMI_ASI.xlsx`
and writes the three CSVs above to `~/Desktop/Results/`.
`02_run_regressions.R` reads those same three CSVs back in from
`~/Desktop/Results/` (plus two small sheets directly from
`SSMI_ASI.xlsx`), and writes all output tables and figures to
`~/Desktop/Results/Tables/` and `~/Desktop/Results/Figures/` (with several
subfolders, e.g. `With_Controls_Harmonized/`, `Parallel_Trends/`,
`Placebo_Tests/`, `First_Difference/`, `Robustness_Checks/`,
`Summary_Stats/`). Update these paths at the top of each script (and
wherever `read_excel()`, `read_csv()`, `write_csv()`, `ggsave()`,
`save_kable()`, or `write_xlsx()` are called) to match your own directory
structure, and create the output subfolders before running, since R will
not create them automatically.

Required R packages: `readxl`, `writexl`, `tidyverse`, `fixest`, `ggplot2`,
`ggfixest`, `fwildclusterboot`, `kableExtra`, `broom`.
## Data dictionary (`SSMI_ASI.xlsx`)
The workbook has 21 sheets and they fall into five groups:
**Raw source data (SSMI, 1951-58).** `SSMI_1951_58` is the combined wide
sheet used by the script; `SSMI_1951` through `SSMI_1958` are the
individual per-year sheets it was assembled from, kept for reference/audit.
Columns: `CMI Code` (Census of Manufacturing Industries industry code,
the SSMI-era classification), `Industrial Sector` (industry name), and
`GO`/`GVA`/`Gross Input` (Gross Output, Gross Value Added, Gross Input,
in current Rupees) for each year, plus `Fixed_Capital`/`Working_Capital`
(1951 is split into `_Power`/`_No_Power` variants in the raw source and
recombined in the script). Data for the years 1954-56 was very sparse and
hence are treated as missing throughout.
**Raw source data (ASI, 1959-65).** `ASI_Sample_Sector_1959_65` and
`ASI_Census_Sector_1959_65` are the ASI's sample-based and full-census
industry aggregates respectively (the script sums them into a single
"factory" series). Key columns: `Labor Bureau Classification` (the ASI-era sample industry code) /
`ASI Classification Code` (the ASI-era census industry code), `Industrial Sector`,
and `GO`/`GI`/`GVA` by year.
**Classification harmonization.** `ASI_SSMI_Harmonization` maps SSMI-era
industry codes to ASI-era codes (`SSMI Code Number`, `ASI Code Number`) —
this is the crosswalk used to build the 36-industry harmonized panel
spanning both eras. `ASI_Sample_Census_Harmonization` maps the ASI's own
sample-based and full-census industry codes onto a common sectoring, and is used directly in the
script to merge the ASI sample and census series before they are combined
with the SSMI data.
**Prices, linkages, and controls.** `Industrial_Price_Sectors` maps each
industry to one of the wholesale price index categories in
`Industrial_Prices` (index values by year, 1951=100 rebased to 1961=100 in
the script), used to deflate nominal GO/GVA/Material Input/Capital into
real terms. `ASI_Input_Output` gives each industry's position in the
1953-54 input-output tables (`Direct Backward/Forward Linkage`,
`Leontief Backward/Forward Linkage`), used to
construct the SUTVA robustness checks (restricting the control group to
low-linkage industries, or explicitly controlling for linkage exposure).
`controls_1951_53` holds the pre-treatment (1951-53) industry
characteristics — wages, employment, material input — averaged in the
script to build the pre-trend control variables used throughout the
"with controls" specifications.
`IO_Sectoral_Classification` maps each of the 36 IO sectors to a broader
category (Primary, etc.) and documents the correspondence between those
sectors and the industrial-production-survey sectors used elsewhere in
the paper (Table A7), following the harmonization scheme in Chakraverti
(1968). The underlying input-output tables themselves are not included in
this deposit (they are available upon request); `ASI_Input_Output` already contains
the industry-level linkage measures derived from them and is all the
script needs.
**Learning-by-doing variables.** `Learning_SSMI`, `Learning_ASI_Sample`,
and `Learning_ASI_Census` provide `Number_of_Workers`, `Number_of_Employees`,
`Material_Input`, `Productive_Capital`, and `Fixed_Capital`/
`Working_Capital` by year for the SSMI and ASI (sample and census) periods
respectively — the inputs to the Section 5.2 Learning-by-Doing regressions.
Note that `Number_of_Workers` (and therefore labor productivity and
cumulative experience) is unavailable for 1959 across these sheets; the script's `Experience`
construction accounts for this explicitly.
## Script structure
### `01_build_data.R`
Runs top to bottom as a single pipeline:
1. **Data construction**: loads and cleans the ASI sample
   and census sheets, harmonizes SSMI and ASI industry codes into a single
   36-industry panel (`harmonized_data`/`industry_panel`) spanning
   1951-65, and separately builds a non-harmonized repeated cross-section
   (`nonharmonized_data`/`industry_repeated_cross_section`) that uses each
   era's native industry classification without crosswalking, plus the
   raw ASI-only factory panel (`asi_factory`, 1959-65). Also builds the
   1951-53 pre-treatment control variables, the deflated (real) versions
   of GO, GVA, Material Input, and Capital, and the first-difference
   columns used in the paper's first-difference specification.
2. **Export**: writes `industry_panel`, `industry_repeated_cross_section`,
   and `asi_factory` out as the three CSVs described under "Files" above.
### `02_run_regressions.R`
Loads the three CSVs above (no need to run `01_build_data.R` first unless
you want to regenerate them) and runs every table and figure in the
paper, top to bottom:
1. **Direct impacts / static difference-in-differences**: the average treatment effect of the heavy industry drive on
   each outcome variable, with and without pre-trend controls, using wild
   cluster bootstrap (WCB) inference throughout (`fwildclusterboot`),
   appropriate given the small number of industry clusters.
2. **Dynamic difference-in-differences**: year-by-year
   event-study estimates for the harmonized panel, with a joint
   cluster-robust Wald test of pre-trends, plus the corresponding event
   study plots.
3. **Placebo / classification-switch test**: a Monte Carlo
   check for whether the 1959 classification change alone could
   mechanically generate a spurious treatment effect, run only on outcome
   variables with complete 1959 coverage.
4. **First-difference regressions**: an alternative
   specification robust to the industry classification change, run on
   both the harmonized panel and the non-harmonized repeated
   cross-section.
5. **SUTVA robustness checks**: re-estimates the main
   effect restricting the control group to low-linkage industries (by
   backward and forward linkage separately) and, alternatively, explicitly
   controlling for linkage exposure, using the `ASI_Input_Output` measures.
6. **Summary statistics and parallel trends plots**:
   Summary statistics tables and the raw parallel-trends
   figures for each outcome variable.
7. **Learning-by-Doing**: estimates Equation 5 of the
   paper (labor productivity on cumulative experience, its interaction
   with treatment status, and controls) on both the harmonized 1957-65
   panel (built from `industry_panel`) and the ASI-only 1960-65 panel
   (built from `asi_factory`), with WCB inference, producing the paper's
   Tables 7 and 8.
Throughout, `[WCB p=X]` in table output denotes a wild cluster bootstrap
p-value (9999 replications, clustered at the industry level); parenthetical
values in the earlier tables denote standard errors. `sig_stars_p()`
applies conventional significance stars (\*\*\* p<0.01, \*\* p<0.05, \* p<0.1)
based on the WCB p-value.
