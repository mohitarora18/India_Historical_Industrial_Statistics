## ======================================================================
## 02_run_regressions.R
##
## All estimation, tables, and figures in the paper. Reads the three
## analysis-ready panels produced by 01_build_data.R
## run 01_build_data.R first (or just make
## sure the three CSVs below already exist), then run this script.
##
## This script does still read two small reference sheets directly from
## SSMI_ASI.xlsx: Industrial_Prices (to rebuild the price-deflator table,
## a two-line calculation) and ASI_Input_Output (used only by the SUTVA
## robustness checks). Neither requires rerunning any of the harmonization
## logic in 01_build_data.R.
##
## Update the CSV/workbook paths below, and the output paths throughout
## (wherever ggsave()/save_kable()/write_xlsx() are called), to match your
## own directory structure. Create the output subfolders first, since R
## will not create them automatically.
## ======================================================================

rm(list=ls())
library(readxl)
library(writexl)
library(tidyverse)
library(fixest)
library(ggplot2)
library(ggfixest)
library(fwildclusterboot)
library(kableExtra)
library(broom)

## --- Load the analysis-ready panels built by 01_build_data.R ---
industry_panel                  <- read_csv("~/Desktop/Results/harmonized_panel_analysis_sample.csv")
industry_repeated_cross_section <- read_csv("~/Desktop/Results/nonharmonized_pooled_analysis_sample.csv")
asi_factory                     <- read_csv("~/Desktop/Results/asi_factory_analysis_sample.csv")

## --- Small derived objects used throughout  ---
industry_panel <- industry_panel %>% mutate(sector_code_f = as.factor(sector_code))
industry_panel_drop_1959 <- industry_panel %>% filter(Year != 1959)

## --- Rebuild the price-deflator table (2-line calculation; the only
## other raw-workbook read this script needs besides ASI_Input_Output
## further below) ---
ind_prices <- read_excel("~/Desktop/Research/JMP/Data/SSMI_ASI.xlsx", sheet="Industrial_Prices")
ind_prices_reindexed <- ind_prices %>%
  mutate(across(-Year, ~ (.x / .x[Year == 1961]) * 100))

## --- Outcome variable list (used throughout the tables below) ---
outcome_vars <- c(
  ln_GO               = "Log of Gross Output",
  ln_GVA              = "Log of Gross Value Added",
  ln_mat_input        = "Log of Material Input",
  ln_employees        = "Log of Number of Employees",
  ln_workers          = "Log of Number of Workers",
  ln_factories        = "Log of Number of Factories",
  ln_fixed_capital    = "Log of Fixed Capital",
  ln_working_capital  = "Log of Working Capital"
)
# --- Step 1: Run static DiD for each outcome, keep WCB inference + Observations + R2 ---

static_did_results <- lapply(names(outcome_vars), function(var) {
  
  f <- as.formula(paste0(var, " ~ targeted:Post + factor(Year) | sector_code_f"))
  mod <- feols(f, data = industry_panel, vcov = ~ sector_code_f)
  
  b <- boottest(mod, param = "targeted:Post", clustid = "sector_code_f",
                B = 9999, fe = "sector_code_f")
  
  tidy(b) %>%
    mutate(
      Variable     = outcome_vars[[var]],
      Observations = nobs(mod),
      R2           = r2(mod, type = "r2"),
      .before = 1
    )
}) %>% bind_rows()

write_xlsx(static_did_results, "~/Desktop/Results/Tables/static_did_results_harmon_data_no_controls.xlsx")

# --- Step 2: Build the row-per-outcome-variable table ---

sig_stars <- function(p) {
  if (is.na(p)) return("")
  if (p < 0.01) return("***")
  if (p < 0.05) return("**")
  if (p < 0.10) return("*")
  return("")
}

n_vars <- nrow(static_did_results)

table_rows <- lapply(seq_len(n_vars), function(i) {
  row   <- static_did_results[i, ]
  stars <- sig_stars(row$p.value)
  
  tibble(
    Term  = c(paste0("\\textbf{", row$Variable, "}"), "", "Observations", "R$^2$"),
    Model = c(
      sprintf("%.4f%s", row$estimate, stars),
      sprintf("[WCB p = %.3f]", row$p.value),
      as.character(row$Observations),
      sprintf("%.5f", round(row$R2, 5))
    )
  )
}) %>% bind_rows()

header_rows <- tibble(
  Term  = c("Coefficient of Interest:", "Model:", "\\textit{Outcome Variables}"),
  Model = c("targeted $\\times$ Post", "(1)", "")
)

footer_rows <- tibble(
  Term  = c("Industry Fixed Effects", "Time Fixed Effects", "Controls"),
  Model = c("Yes", "Yes", "No")
)

full_table <- bind_rows(header_rows, table_rows, footer_rows)

# Row positions: addlinespace after each variable block except the last;
# midrule after header block and again after the last variable block
spacer_positions <- 3 + (seq_len(n_vars - 1) * 4)
last_var_row      <- 3 + n_vars * 4

kbl_out <- full_table %>%
  kable(format = "latex", booktabs = TRUE, escape = FALSE,
        col.names = NULL, align = "lr") %>%
  kable_styling(latex_options = "hold_position") %>%
  row_spec(3, extra_latex_after = "\\midrule") %>%
  row_spec(spacer_positions, extra_latex_after = "\\addlinespace") %>%
  row_spec(last_var_row, extra_latex_after = "\\midrule") %>%
  footnote(
    general = "Wild cluster bootstrap p-values (9999 replications, clustered at the industry level) reported below each estimate. Signif. Codes: ***: 0.01, **: 0.05, *: 0.1",
    general_title = "Notes:", footnote_as_chunk = TRUE, escape = FALSE
  )

save_kable(kbl_out, "~/Desktop/Results/Tables/Table2_Direct_Impacts_Harmonized_NoControls.tex")

#Regression with single time trend interacted with controls
static_did_controls <- lapply(names(outcome_vars), function(var) {
  
  f <- as.formula(paste0(
    var, " ~ targeted:Post + log_avg_wage:time_trend + log_avg_plant_size:time_trend + ",
    "log_avg_material:time_trend + log_avg_productivity:time_trend | sector_code_f + Year"
  ))
  mod <- feols(f, data = industry_panel, vcov = ~ sector_code_f)
  b <- boottest(mod, param = "targeted:Post", clustid = "sector_code_f",
                B = 9999, fe = "sector_code_f")
  
  tidy(b) %>% mutate(Variable = outcome_vars[[var]], Observations = nobs(mod), R2 = r2(mod, type = "r2"), .before = 1)
}) %>% bind_rows()
write_xlsx(static_did_controls, "~/Desktop/Results/Tables/static_did_results_harmon_data_controls.xlsx")
static_did_controls %>%
  mutate(across(c(estimate, conf.low, conf.high, p.value), ~ round(.x, 3))) %>%
  select(Variable, estimate, conf.low, conf.high, p.value, Observations, R2) %>%
  kable(format = "latex", booktabs = TRUE,
        col.names = c("Outcome", "Estimate", "CI Low", "CI High", "WCB p-value", "Observations", "R2")) %>%
  save_kable("~/Desktop/Results/Tables/static_did_results_harmon_data_controls.tex")
 
sig_stars_p <- function(p) {
  if (is.na(p)) return("")
  if (p < 0.01) return("***")
  if (p < 0.05) return("**")
  if (p < 0.1) return("*")
  ""
}

## --- Core function: fit dynamic DD model, get WCB per-year CI/p-value + joint pre-trends Wald test ---
run_dynamic_wcb <- function(var, controls = FALSE, seed = 1, B = 9999) {
  
  rhs <- "i(Year, targeted, 1953)"
  if (controls) {
    rhs <- paste0(rhs,
                  " + log_avg_wage:time_trend + log_avg_plant_size:time_trend + ",
                  "log_avg_material:time_trend + log_avg_productivity:time_trend")
  }
  f <- as.formula(paste0(var, " ~ ", rhs, " | sector_code_f + Year"))
  mod <- feols(f, data = industry_panel, vcov = ~ sector_code_f)
  
  event_terms <- grep("^Year::.*:targeted$", names(coef(mod)), value = TRUE)
  
  term_df <- lapply(seq_along(event_terms), function(i) {
    term <- event_terms[i]
    yr   <- as.numeric(gsub("Year::(\\d+):targeted", "\\1", term))
    set.seed(seed * 100 + i)
    b  <- boottest(mod, param = term, clustid = "sector_code_f", B = B, fe = "sector_code_f")
    bt <- tidy(b)
    data.frame(Year = yr, estimate = coef(mod)[[term]],
               conf.low = bt$conf.low[1], conf.high = bt$conf.high[1],
               wcb_p = bt$p.value[1])
  }) %>% bind_rows() %>% arrange(Year)
  
  ## Joint pre-trends test on pre-1953 terms (1951, 1952), clustered Wald test
  pre_terms <- grep("^Year::19(51|52):targeted$", event_terms, value = TRUE)
  pretrends_p <- if (length(pre_terms) > 0) wald(mod, keep = pre_terms)$p else NA
  
  list(var = var, terms = term_df, pretrends_p = pretrends_p,
       obs = nobs(mod), r2 = r2(mod, type = "r2"), mod = mod)
}

## --- Plotting function (same look as other WCB event study plots) ---
plot_dynamic_wcb <- function(res, save_path) {
  boot_df <- bind_rows(res$terms, data.frame(Year = 1953, estimate = 0, conf.low = 0, conf.high = 0, wcb_p = NA)) %>%
    arrange(Year)
  p <- ggplot(boot_df, aes(x = Year, y = estimate)) +
    geom_point() +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    scale_x_continuous(breaks = seq(1951, 1965, 1)) +
    theme_gray() +
    theme(axis.text.x = element_text(colour = "black"), axis.text.y = element_text(colour = "black")) +
    geom_vline(xintercept = 1956, color = "red") +
    theme(axis.title.x = element_blank()) +
    ylab("Estimate and 95% Conf. Int.")
  ggsave(save_path, p, width = 7, height = 5, dpi = 300)
  p
}

## --- Run for all 8 variables, both specifications ---
dyn_results_nocontrols <- lapply(names(outcome_vars), function(var) {
  res <- run_dynamic_wcb(var, controls = FALSE, seed = which(names(outcome_vars) == var))
  plot_dynamic_wcb(res, paste0("~/Desktop/Results/Figures/No_Controls_Harmonized/event_study_", var, "_noctrl_wcb.png"))
  res
})
names(dyn_results_nocontrols) <- names(outcome_vars)

dyn_results_controls <- lapply(names(outcome_vars), function(var) {
  res <- run_dynamic_wcb(var, controls = TRUE, seed = which(names(outcome_vars) == var))
  plot_dynamic_wcb(res, paste0("~/Desktop/Results/Figures/With_Controls_Harmonized/event_study_", var, "_ctrl_wcb.png"))
  res
})
names(dyn_results_controls) <- names(outcome_vars)

## --- Export raw values (per variable, both specs) for table-building ---
dyn_export <- lapply(names(outcome_vars), function(var) {
  bind_rows(
    dyn_results_nocontrols[[var]]$terms %>% mutate(Variable = outcome_vars[[var]], Controls = "No"),
    dyn_results_controls[[var]]$terms %>% mutate(Variable = outcome_vars[[var]], Controls = "Yes")
  ) %>%
    mutate(
      Observations = ifelse(Controls == "No", dyn_results_nocontrols[[var]]$obs, dyn_results_controls[[var]]$obs),
      R2           = ifelse(Controls == "No", dyn_results_nocontrols[[var]]$r2,  dyn_results_controls[[var]]$r2),
      PreTrends_p  = ifelse(Controls == "No", dyn_results_nocontrols[[var]]$pretrends_p, dyn_results_controls[[var]]$pretrends_p)
    )
}) %>% bind_rows()

write_xlsx(dyn_export, "~/Desktop/Results/Tables/dynamic_did_results_harmon_data_wcb.xlsx")

dyn_export %>%
  mutate(across(c(estimate, conf.low, conf.high, wcb_p, R2, PreTrends_p), ~ round(.x, 4))) %>%
  kable(format = "latex", booktabs = TRUE,
        col.names = c("Year", "Estimate", "CI Low", "CI High", "WCB p-value",
                      "Variable", "Controls", "Observations", "R2", "Pre-Trends Joint p")) %>%
  save_kable("~/Desktop/Results/Tables/dynamic_did_results_harmon_data_wcb.tex")
  
#Event Study Plots
#Equation 2 estimated without controls
event_study_plots_nh_noref <- lapply(names(outcome_vars), function(var) {
  
  f <- as.formula(paste0(var, " ~ i(Year, targeted) | Year"))
  mod <- feols(f, data = industry_repeated_cross_section, vcov = ~ `Sector Code`)
  
  p <- ggiplot(mod, geom = 'errorbar', ref.line = FALSE) +
    theme_gray() +
    scale_x_continuous(breaks = seq(1951, 1965, 1)) +
    theme(axis.text.x = element_text(colour = "black"), axis.text.y = element_text(colour = "black")) +
    geom_vline(xintercept = 1956, color = "red") +
    theme(axis.title.x = element_blank()) +
    ylab("Estimate and 95% Conf. Int.") +
    ggtitle(NULL)
  
  ggsave(paste0("~/Desktop/Results/Figures/No_Controls_Non_Harmonized/event_study_", var, "_nh_noctrl.png"),
         p, width = 7, height = 5, dpi = 300)
  
  p
})

#### 1959 Classification-Switch "Random Artifact" Test ####
## --- Setup: static repeated cross-section skeleton, matching real data structure ---
industries_g1 <- 1:57
years_g1 <- 1951:1958
df1 <- expand.grid(industry_code = industries_g1, Year = years_g1)
industries_g2 <- 58:99
years_g2 <- 1959:1965
df2 <- expand.grid(industry_code = industries_g2, Year = years_g2)
skeleton <- bind_rows(df1, df2)
targeted_g1 <- c(3,7,10,14,17,25,29,35,39,43,49)
targeted_g2 <- c(60,67,70,72,81,85,91)
skeleton <- skeleton %>%
  mutate(targeted = ifelse(industry_code %in% c(targeted_g1, targeted_g2), 1, 0),
         industry_code = as.factor(industry_code))
## --- Restrict to outcome variables with full 1959 coverage in the real data ---
vars_with_1959 <- names(outcome_vars)[sapply(names(outcome_vars), function(var) {
  any(!is.na(industry_repeated_cross_section[[var]][industry_repeated_cross_section$Year == 1959]))
})]
## --- Common missingness pattern for this restricted variable set ---
years_missing <- 1954:1956
last_pre  <- 1958
first_post <- 1959
term_pre  <- paste0("Year::", last_pre, ":targeted")
term_post <- paste0("Year::", first_post, ":targeted")
## --- Function 1: formal placebo test (calibrated SD, fixed missingness pattern) ---
run_placebo_test <- function(var, n_sims = 500, seed = 123) {
  
  resid_mod <- feols(as.formula(paste0(var, " ~ 1 | Year")),
                     data = industry_repeated_cross_section)
  sd_var <- sd(resid(resid_mod), na.rm = TRUE)
  
  actual_mod <- feols(as.formula(paste0(var, " ~ i(Year, targeted) | Year")),
                      data = industry_repeated_cross_section,
                      vcov = ~ `Sector Code`)
  actual_coefs <- coef(actual_mod)
  actual_diff <- actual_coefs[term_post] - actual_coefs[term_pre]
  
  set.seed(seed)
  results_diff <- numeric(n_sims)
  
  for (i in 1:n_sims) {
    sim_data <- skeleton %>%
      mutate(y_sim = 10 + rnorm(n(), mean = 0, sd = sd_var),
             y_sim = ifelse(Year %in% years_missing, NA, y_sim))
    
    reg <- feols(y_sim ~ i(Year, targeted) | Year,
                 data = sim_data, vcov = ~ industry_code)
    coefs <- coef(reg)
    
    if (term_pre %in% names(coefs) && term_post %in% names(coefs)) {
      results_diff[i] <- coefs[term_post] - coefs[term_pre]
    } else {
      results_diff[i] <- NA
    }
  }
  
  p_value <- mean(abs(results_diff) >= abs(actual_diff), na.rm = TRUE)
  
  list(variable = var, sd_used = sd_var, last_pre = last_pre, first_post = first_post,
       actual_diff = actual_diff, p_value = p_value, null_dist = results_diff)
}
## --- Function 2: one illustrative "canned" event study plot ---
plot_canned_event_study <- function(var, seed = 1) {
  
  resid_mod <- feols(as.formula(paste0(var, " ~ 1 | Year")),
                     data = industry_repeated_cross_section)
  sd_var <- sd(resid(resid_mod), na.rm = TRUE)
  
  set.seed(seed)
  sim_data <- skeleton %>%
    mutate(y_sim = 10 + rnorm(n(), mean = 0, sd = sd_var),
           y_sim = ifelse(Year %in% years_missing, NA, y_sim))
  
  reg <- feols(y_sim ~ i(Year, targeted) | Year,
               data = sim_data, vcov = ~ industry_code)
  
  p <- ggiplot(reg, geom = 'errorbar', ref.line = FALSE) +
    scale_x_continuous(breaks = seq(1951, 1965, 1)) +
    theme_gray() +
    theme(axis.text.x = element_text(colour = "black"), axis.text.y = element_text(colour = "black")) +
    geom_vline(xintercept = 1956, color = "red") +
    theme(axis.title.x = element_blank()) +
    ggtitle(NULL) +
    ylab("Estimate and 95% Conf. Int.")
  
  ggsave(paste0("~/Desktop/Results/Figures/Placebo_Tests/canned_event_study_", var, ".png"),
         p, width = 7, height = 5, dpi = 300)
  
  p
}
## --- Run canned event study plots (different seed per variable) ---
canned_plots <- lapply(seq_along(vars_with_1959), function(i) {
  plot_canned_event_study(vars_with_1959[i], seed = i)
})
names(canned_plots) <- vars_with_1959
## --- Run formal placebo test ONCE per variable, build histogram plots from it ---
placebo_results <- lapply(vars_with_1959, function(var) {
  
  res <- run_placebo_test(var)
  
  p <- ggplot(data.frame(difference = res$null_dist), aes(x = difference)) +
    geom_histogram(aes(y = ..density..), binwidth = 0.1, fill = "#0072B2", alpha = 0.7) +
    geom_density(color = "#D55E00", size = 1) +
    geom_vline(xintercept = res$actual_diff, color = "black", linetype = "dashed", size = 1.2) +
    annotate("text", x = res$actual_diff, y = 0,
             label = paste0("Actual Diff\n(", round(res$actual_diff, 3), ")\np = ", round(res$p_value, 3)),
             hjust = -0.1, vjust = -0.5, color = "black", fontface = "bold") +
    labs(x = "Coefficient Difference", y = "Density") +
    theme_gray()
  
  ggsave(paste0("~/Desktop/Results/Figures/Placebo_Tests/placebo_", var, ".png"),
         p, width = 7, height = 5, dpi = 300)
  
  cat(var, "- actual diff:", round(res$actual_diff, 3), "| p-value:", round(res$p_value, 3), "\n")
  
  res
})
names(placebo_results) <- vars_with_1959


## --- Map each outcome variable to its first-difference column ---
diff_vars <- c(
  ln_GO = "godiff", ln_GVA = "gvadiff", ln_factories = "factorydiff",
  ln_workers = "workerdiff", ln_mat_input = "matinputdiff", ln_employees = "employeediff",
  ln_fixed_capital = "fixedcapitaldiff", ln_working_capital = "workingcapitaldiff"
)

## --- Labels matching the paper's row names ---
fd_labels <- c(
  ln_GO = "Gross Output", ln_GVA = "Gross Value Added", ln_factories = "Number of Factories",
  ln_workers = "Number of Workers", ln_employees = "Number of Employees", ln_mat_input = "Material Input",
  ln_fixed_capital = "Fixed Capital", ln_working_capital = "Working Capital"
)

## --- Function: build the 4 columns for one outcome variable (two-way FE kept as-is, matching baseline) ---
build_fd_models <- function(var) {
  dvar <- diff_vars[[var]]
  
  f1 <- as.formula(paste0(dvar, " ~ targeted + targeted:Post | Year"))
  col1 <- feols(f1, data = industry_repeated_cross_section, vcov = ~`Sector Code`)
  
  f2 <- as.formula(paste0(dvar, " ~ targeted + targeted:Post | Year"))
  col2 <- feols(f2, data = industry_panel_drop_1959, vcov = ~sector_code_f)
  
  f3 <- as.formula(paste0(dvar, " ~ targeted:Post | sector_code_f + Year"))
  col3 <- feols(f3, data = industry_panel_drop_1959, vcov = ~sector_code_f)
  
  f4 <- as.formula(paste0(
    dvar, " ~ targeted:Post + avg_mat_input_per_worker:time_trend + ",
    "avg_wage_bill_per_worker:time_trend + avg_worker_productivity:time_trend + ",
    "avg_plant_size:time_trend | sector_code_f + Year"
  ))
  col4 <- feols(f4, data = industry_panel, vcov = ~sector_code_f)
  
  list(col1 = col1, col2 = col2, col3 = col3, col4 = col4)
}

## --- Run for all 8 variables ---
fd_results <- lapply(names(outcome_vars), build_fd_models)
names(fd_results) <- names(outcome_vars)

## --- Helpers: WCB p-value, significance stars ---
get_wcb_pval <- function(mod, fe_var, seed = 1, B = 9999) {
  set.seed(seed)
  b <- boottest(mod, param = "targeted:Post", clustid = "sector_code_f", B = B, fe = fe_var)
  tidy(b)$p.value[1]
}

sig_stars_p <- function(p) {
  if (is.na(p)) return("")
  if (p < 0.01) return("***")
  if (p < 0.05) return("**")
  if (p < 0.1) return("*")
  ""
}

sig_stars_t <- function(coef_val, se_val) {
  if (is.na(se_val) || se_val == 0) return("")
  t <- abs(coef_val / se_val)
  if (t > 2.576) return("***")
  if (t > 1.96) return("**")
  if (t > 1.645) return("*")
  ""
}

## --- Extract the targeted:Post row-block: col1 = standard SE, col2-4 = WCB p-value ---
extract_fd_row <- function(models, var_label, seed = 1) {
  coefs <- sapply(models, function(m) {
    cf <- coef(m)
    if ("targeted:Post" %in% names(cf)) cf[["targeted:Post"]] else NA
  })
  se1 <- se(models$col1)[["targeted:Post"]]
  
  p2 <- get_wcb_pval(models$col2, fe_var = "Year",         seed = seed)
  p3 <- get_wcb_pval(models$col3, fe_var = "sector_code_f", seed = seed)
  p4 <- get_wcb_pval(models$col4, fe_var = "sector_code_f", seed = seed)
  
  obs <- sapply(models, nobs)
  rsq <- sapply(models, function(m) r2(m, type = "r2"))
  
  data.frame(
    Row  = c(paste0("Change in ", var_label), "", "Observations", "R2"),
    col1 = c(sprintf("%.4f%s", coefs[1], sig_stars_t(coefs[1], se1)),
             sprintf("(%.4f)", se1), obs[1], sprintf("%.5f", rsq[1])),
    col2 = c(sprintf("%.4f%s", coefs[2], sig_stars_p(p2)),
             sprintf("[WCB p=%.3f]", p2), obs[2], sprintf("%.5f", rsq[2])),
    col3 = c(sprintf("%.4f%s", coefs[3], sig_stars_p(p3)),
             sprintf("[WCB p=%.3f]", p3), obs[3], sprintf("%.5f", rsq[3])),
    col4 = c(sprintf("%.4f%s", coefs[4], sig_stars_p(p4)),
             sprintf("[WCB p=%.3f]", p4), obs[4], sprintf("%.5f", rsq[4])),
    stringsAsFactors = FALSE
  )
}

## --- Shared Yes/No structure block ---
fd_structure_block <- data.frame(
  Row  = c("Industry Fixed Effects", "Time Fixed Effects", "Controls", "Year 1959",
           "Targeted Dummy", "Data(Pooled Cross-Section (PCS)/Panel(P))"),
  col1 = c("No", "Yes", "No", "No", "Yes", "PCS"),
  col2 = c("No", "Yes", "No", "No", "Yes", "P"),
  col3 = c("Yes", "Yes", "No", "No", "No", "P"),
  col4 = c("Yes", "Yes", "Yes", "Yes", "No", "P"),
  stringsAsFactors = FALSE
)

## --- Group into 3 tables, save as LaTeX and Excel ---
table_groups <- list(
  Table_GO_GVA_Factories = c("ln_GO", "ln_GVA", "ln_factories"),
  Table_Workers_Employees_MatInput = c("ln_workers", "ln_employees", "ln_mat_input"),
  Table_Capital = c("ln_fixed_capital", "ln_working_capital")
)

for (tname in names(table_groups)) {
  vars_in_group <- table_groups[[tname]]
  
  var_rows <- lapply(vars_in_group, function(v) {
    extract_fd_row(fd_results[[v]], fd_labels[[v]], seed = which(names(outcome_vars) == v))
  })
  combined <- bind_rows(var_rows, fd_structure_block)
  
  tex_table <- combined %>%
    kable(format = "latex", booktabs = TRUE, row.names = FALSE, digits = 4,
          col.names = c("", "(1)", "(2)", "(3)", "(4)"),
          caption = paste0("First Difference Regression: ", tname)) %>%
    add_header_above(c(" " = 1, "targeted x Post" = 4))
  
  print(tex_table)
  save_kable(tex_table, paste0("~/Desktop/Results/Tables/First_Difference/", tname, ".tex"))
  write_xlsx(combined, path = paste0("~/Desktop/Results/Tables/First_Difference/", tname, ".xlsx"))
}

## --- WCB event study builder (harmonized panel: two-way FE, matching baseline dynamic DD pattern) ---
build_wcb_event_study <- function(dvar, extra_terms, data, ref_year, seed = 1, B = 9999) {
  rhs <- paste0("i(Year, targeted, ", ref_year, ")")
  if (!is.null(extra_terms)) rhs <- paste0(rhs, " + ", extra_terms)
  f <- as.formula(paste0(dvar, " ~ ", rhs, " | sector_code_f + Year"))
  mod <- feols(f, data = data, vcov = ~sector_code_f)
  
  event_terms <- grep("^Year::.*:targeted$", names(coef(mod)), value = TRUE)
  
  boot_rows <- lapply(seq_along(event_terms), function(i) {
    term <- event_terms[i]
    yr <- as.numeric(gsub("Year::(\\d+):targeted", "\\1", term))
    set.seed(seed * 100 + i)
    b <- boottest(mod, param = term, clustid = "sector_code_f", B = B, fe = "sector_code_f")
    bt <- tidy(b)
    data.frame(Year = yr, estimate = coef(mod)[[term]],
               conf.low = bt$conf.low[1], conf.high = bt$conf.high[1])
  })
  boot_df <- bind_rows(boot_rows)
  boot_df <- bind_rows(boot_df, data.frame(Year = ref_year, estimate = 0, conf.low = 0, conf.high = 0)) %>%
    arrange(Year)
  
  list(mod = mod, boot_df = boot_df)
}
plot_wcb_event_study <- function(boot_df, save_path) {
  p <- ggplot(boot_df, aes(x = Year, y = estimate)) +
    geom_point() +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    scale_x_continuous(breaks = seq(1951, 1965, 1)) +
    theme_gray() +
    theme(axis.text.x = element_text(colour = "black"), axis.text.y = element_text(colour = "black")) +
    geom_vline(xintercept = 1956, color = "red") +
    theme(axis.title.x = element_blank(), plot.title = element_blank()) +
    ylab("Estimate and 95% Conf. Int.")
  ggsave(save_path, p, width = 7, height = 5, dpi = 300)
  p
}
## --- Set 1 (main text): harmonized panel, no controls, 1959 dropped, WCB CIs ---
fd_es_harmonized_nocontrols <- lapply(names(outcome_vars), function(var) {
  dvar <- diff_vars[[var]]
  res <- build_wcb_event_study(dvar, extra_terms = NULL, data = industry_panel_drop_1959,
                               ref_year = 1953, seed = which(names(outcome_vars) == var))
  plot_wcb_event_study(res$boot_df,
                       paste0("~/Desktop/Results/Figures/First_Difference/Harmonized_NoControls/event_study_", dvar, ".png"))
  res$mod
})
names(fd_es_harmonized_nocontrols) <- names(outcome_vars)
## --- Set 2 (main text): non-harmonized pooled data, no controls, no industry FE, standard SE ---
fd_es_nonharmonized <- lapply(names(outcome_vars), function(var) {
  dvar <- diff_vars[[var]]
  fes <- as.formula(paste0(dvar, " ~ i(Year, targeted, 1953) | Year"))
  mod_es <- feols(fes, data = industry_repeated_cross_section, vcov = ~`Sector Code`)
  
  p <- ggiplot(mod_es, geom = 'errorbar') +
    theme_gray() +
    scale_x_continuous(breaks = seq(1951, 1965, 1)) +
    theme(axis.text.x = element_text(colour = "black"), axis.text.y = element_text(colour = "black")) +
    geom_vline(xintercept = 1956, color = "red") +
    theme(axis.title.x = element_blank()) +
    ylab("Estimate and 95% Conf. Int.") +
    ggtitle(NULL)
  
  ggsave(paste0("~/Desktop/Results/Figures/First_Difference/NonHarmonized_NoControls/event_study_", dvar, ".png"),
         p, width = 7, height = 5, dpi = 300)
  mod_es
})
names(fd_es_nonharmonized) <- names(outcome_vars)

## --- Set 3 (appendix): harmonized panel, with controls, 1959 kept, WCB CIs ---
fd_es_harmonized_controls <- lapply(names(outcome_vars), function(var) {
  dvar <- diff_vars[[var]]
  controls_str <- paste0(
    "avg_mat_input_per_worker:time_trend + avg_wage_bill_per_worker:time_trend + ",
    "avg_worker_productivity:time_trend + avg_plant_size:time_trend"
  )
  res <- build_wcb_event_study(dvar, extra_terms = controls_str, data = industry_panel,
                               ref_year = 1953, seed = which(names(outcome_vars) == var))
  plot_wcb_event_study(res$boot_df,
                       paste0("~/Desktop/Results/Figures/First_Difference/Harmonized_Controls_Appendix/event_study_", dvar, ".png"))
  res$mod
})
names(fd_es_harmonized_controls) <- names(outcome_vars)

#### SUTVA Robustness Checks ####

## --- SUTVA 1: restrict control group to low-linkage industries ---
asi_input_output <- read_excel("~/Desktop/Research/JMP/Data/SSMI_ASI.xlsx", sheet = "ASI_Input_Output")
asi_input_output <- asi_input_output %>%
  filter(!(`ASI Code Number` %in% c("311","334","341","342","350+360+370", NA)))

low_bckwrd_link <- asi_input_output %>% filter(`Direct Backward Linkage` < 0.06381624)
low_fwd_link    <- asi_input_output %>% filter(`Direct Forward Linkage` < 0.04158317)

low_bckwrd_panel <- industry_panel %>%
  filter(sector_code %in% c("311","334","341","342","350+360+370") |
           sector_code %in% c(low_bckwrd_link$`ASI Code Number`))

low_fwd_panel <- industry_panel %>%
  filter(sector_code %in% c("311","334","341","342","350+360+370") |
           sector_code %in% c(low_fwd_link$`ASI Code Number`))

## --- SUTVA 2: control for linkage spillovers from targeted to non-targeted ---
asi_input_output2 <- read_excel("~/Desktop/Research/JMP/Data/SSMI_ASI.xlsx", sheet = "ASI_Input_Output")
asi_input_output2 <- asi_input_output2 %>% filter(!(`ASI Code Number` %in% c(NA)))

asi_input_output2 <- asi_input_output2 %>%
  mutate(
    `Backward Linkage` = case_when(
      `ASI Code Number` %in% c("311","334","341","342","350+360+370") & `Direct Backward Linkage` > 0.2067194 ~ 1,
      `ASI Code Number` %in% c("311","334","341","342","350+360+370")                                         ~ 0,
      !(`ASI Code Number` %in% c("311","334","341","342","350+360+370")) & `Direct Backward Linkage` > 0.06381624 ~ 1,
      !(`ASI Code Number` %in% c("311","334","341","342","350+360+370"))                                         ~ 0
    ),
    `Forward Linkage` = case_when(
      `ASI Code Number` %in% c("311","334","341","342","350+360+370") & `Direct Forward Linkage` > 0.2067194 ~ 1,
      `ASI Code Number` %in% c("311","334","341","342","350+360+370")                                        ~ 0,
      !(`ASI Code Number` %in% c("311","334","341","342","350+360+370")) & `Direct Forward Linkage` > 0.04158317 ~ 1,
      !(`ASI Code Number` %in% c("311","334","341","342","350+360+370"))                                        ~ 0
    ),
    non_targeted = ifelse(!(`ASI Code Number` %in% c("311","334","341","342","350+360+370")), 1, 0)
  ) %>%
  rename(sector_code = `ASI Code Number`)

sutva_robustness_2_panel <- industry_panel %>%
  left_join(asi_input_output2 %>% select(sector_code, non_targeted, `Backward Linkage`, `Forward Linkage`),
            by = "sector_code")

## --- Core function: fit model (single sector FE + Year dummies, for WCB compatibility) and wild cluster bootstrap targeted:Post ---
run_wcb_targeted_post <- function(data, dvar, controls = FALSE, extra_terms = NULL, B = 9999, seed = 1) {
  data <- data %>% mutate(Sector_Code_f = as.factor(sector_code))
  
  rhs <- "targeted:Post + factor(Year)"
  if (!is.null(extra_terms)) rhs <- paste0(rhs, " + ", extra_terms)
  if (controls) {
    rhs <- paste0(rhs,
                  " + avg_mat_input_per_worker:time_trend + avg_wage_bill_per_worker:time_trend + ",
                  "avg_worker_productivity:time_trend + avg_plant_size:time_trend")
  }
  f <- as.formula(paste0(dvar, " ~ ", rhs, " | Sector_Code_f"))
  
  mod <- feols(f, data = data, vcov = ~Sector_Code_f)
  
  set.seed(seed)
  b <- boottest(mod, param = "targeted:Post", clustid = "Sector_Code_f", B = B, fe = "Sector_Code_f")
  bt <- tidy(b)
  
  list(coef = coef(mod)[["targeted:Post"]], pval = bt$p.value[1], obs = nobs(mod), r2 = r2(mod, type = "r2"))
}

sig_stars <- function(p) {
  if (is.na(p)) return("")
  if (p < 0.01) return("***")
  if (p < 0.05) return("**")
  if (p < 0.1) return("*")
  ""
}

## --- Build one condensed row-block per outcome variable (coefficient + WCB p-value, Obs, R2) ---
build_sutva_row <- function(data, var, label, extra_terms = NULL, seed = 1) {
  m1 <- run_wcb_targeted_post(data, var, controls = FALSE, extra_terms = extra_terms, seed = seed)
  m2 <- run_wcb_targeted_post(data, var, controls = TRUE,  extra_terms = extra_terms, seed = seed)
  
  data.frame(
    Row  = c(label, "", "Observations", "R2"),
    col1 = c(sprintf("%.4f%s", m1$coef, sig_stars(m1$pval)),
             sprintf("[WCB p=%.3f]", m1$pval),
             m1$obs, sprintf("%.4f", m1$r2)),
    col2 = c(sprintf("%.4f%s", m2$coef, sig_stars(m2$pval)),
             sprintf("[WCB p=%.3f]", m2$pval),
             m2$obs, sprintf("%.4f", m2$r2)),
    stringsAsFactors = FALSE
  )
}

sutva_structure_block <- data.frame(
  Row  = c("Industry Fixed Effects", "Time Fixed Effects", "Controls"),
  col1 = c("Yes", "Yes", "No"),
  col2 = c("Yes", "Yes", "Yes"),
  stringsAsFactors = FALSE
)

## --- Build and save one condensed table for a given panel/check ---
build_sutva_table <- function(data, extra_terms, tname, caption) {
  var_rows <- lapply(names(outcome_vars), function(v) {
    label <- gsub("^Log of ", "", outcome_vars[[v]])
    build_sutva_row(data, v, label, extra_terms = extra_terms, seed = which(names(outcome_vars) == v))
  })
  combined <- bind_rows(var_rows, sutva_structure_block)
  
  tex_table <- combined %>%
    kable(format = "latex", booktabs = TRUE, row.names = FALSE, digits = 4,
          col.names = c("", "(1) No Controls", "(2) With Controls"),
          caption = caption) %>%
    add_header_above(c(" " = 1, "targeted x Post (WCB p-value)" = 2))
  
  print(tex_table)
  save_kable(tex_table, paste0("~/Desktop/Results/Tables/Robustness_Checks/", tname, ".tex"))
  write_xlsx(combined, path = paste0("~/Desktop/Results/Tables/Robustness_Checks/", tname, ".xlsx"))
}

## --- Run all three SUTVA checks ---
build_sutva_table(low_bckwrd_panel, NULL, "SUTVA_Low_Backward_Linkage",
                  "SUTVA Robustness: Control Group Restricted to Low Backward-Linkage Industries")

build_sutva_table(low_fwd_panel, NULL, "SUTVA_Low_Forward_Linkage",
                  "SUTVA Robustness: Control Group Restricted to Low Forward-Linkage Industries")

build_sutva_table(sutva_robustness_2_panel,
                  "`Backward Linkage`:Post:non_targeted + `Forward Linkage`:Post:non_targeted",
                  "SUTVA_Spillover_Control",
                  "SUTVA Robustness: Controlling for Linkage Spillovers to Non-Targeted Industries")

# ============================================================
# Summary Statistics
# ============================================================

# --- Step 1: Attach deflators and construct real (deflated) monetary variables ---
# GO, GVA, Fixed Capital, Working Capital -> deflated by "All Commodities"
# Material Input -> deflated by weighted Fuel/Raw Materials (0.34 / 0.64), matching
# the same formula already used for real_material_input in control_variables

deflate_for_summary <- function(data) {
  data %>%
    left_join(
      ind_prices_reindexed %>%
        select(Year, `All Commodities`, `Fuel, Power, Light, Lubricants`, `Industrial Raw Materials`),
      by = "Year"
    ) %>%
    mutate(
      GO_real               = GO * 100 / `All Commodities`,
      GVA_real              = GVA * 100 / `All Commodities`,
      Fixed_Capital_real    = Fixed_Capital * 100 / `All Commodities`,
      Working_Capital_real  = Working_Capital * 100 / `All Commodities`,
      Material_Input_real   = Material_Input * 100 /
        (0.34 * `Fuel, Power, Light, Lubricants` + 0.64 * `Industrial Raw Materials`)
    )
}

industry_panel_summary                  <- deflate_for_summary(industry_panel)
industry_repeated_cross_section_summary <- deflate_for_summary(industry_repeated_cross_section)

# --- Step 2: Variable list (deflated monetary vars, nominal counts) ---

summary_vars <- c(
  GO_real              = "Gross Output",
  GVA_real             = "Gross Value Added",
  Number_of_Workers    = "Number of Workers",
  Number_of_Employees  = "Number of Employees",
  Material_Input_real  = "Material Input",
  Number_of_Factories  = "Number of Factories",
  Fixed_Capital_real   = "Fixed Capital",
  Working_Capital_real = "Working Capital"
)

# --- Step 3: Table-building function (unchanged) ---

build_summary_table <- function(data, group_val, group_col = "targeted") {
  df <- data %>% filter(.data[[group_col]] == group_val)
  rows <- lapply(names(summary_vars), function(col) {
    x <- df[[col]]
    data.frame(Variable = summary_vars[[col]], N = sum(!is.na(x)),
               Min = min(x, na.rm = TRUE), Max = max(x, na.rm = TRUE),
               Mean = mean(x, na.rm = TRUE), SD = sd(x, na.rm = TRUE), stringsAsFactors = FALSE)
  })
  bind_rows(rows)
}

# --- Step 4: Build the four tables ---

Table_B1_Targeted_Panel    <- build_summary_table(industry_panel_summary, group_val = 1)
Table_B2_NonTargeted_Panel <- build_summary_table(industry_panel_summary, group_val = 0)
Table_B3_Targeted_PCS      <- build_summary_table(industry_repeated_cross_section_summary, group_val = 1)
Table_B4_NonTargeted_PCS   <- build_summary_table(industry_repeated_cross_section_summary, group_val = 0)

# --- Step 5: Save each table as .tex and .xlsx ---

summary_tables <- list(
  Table_B1_Targeted_Panel    = Table_B1_Targeted_Panel,
  Table_B2_NonTargeted_Panel = Table_B2_NonTargeted_Panel,
  Table_B3_Targeted_PCS      = Table_B3_Targeted_PCS,
  Table_B4_NonTargeted_PCS   = Table_B4_NonTargeted_PCS
)

out_dir <- "~/Desktop/Results/Tables/Summary_Stats/"

for (tbl_name in names(summary_tables)) {
  tbl <- summary_tables[[tbl_name]]
  
  tbl %>%
    mutate(across(c(Min, Max, Mean, SD), ~ round(., 2))) %>%
    kable(format = "latex", booktabs = TRUE, digits = 2,
          caption = gsub("_", " ", tbl_name)) %>%
    kable_styling(latex_options = c("hold_position", "scale_down")) %>%
    save_kable(file = paste0(out_dir, tbl_name, ".tex"))
  
  write_xlsx(tbl, path = paste0(out_dir, tbl_name, ".xlsx"))
}

# ============================================================
# Raw Outcome Variable (Average) Plots — Parallel Trends
# (matches Figures C5–C7 style; harmonized panel, deflated monetary vars)
# ============================================================
# --- Step 1: Deflate monetary variables and construct log versions ---
industry_panel_trends <- industry_panel %>%
  left_join(
    ind_prices_reindexed %>%
      select(Year, `All Commodities`, `Fuel, Power, Light, Lubricants`, `Industrial Raw Materials`),
    by = "Year"
  ) %>%
  mutate(
    GO_real                 = GO * 100 / `All Commodities`,
    GVA_real                = GVA * 100 / `All Commodities`,
    Fixed_Capital_real      = Fixed_Capital * 100 / `All Commodities`,
    Working_Capital_real    = Working_Capital * 100 / `All Commodities`,
    Material_Input_real     = Material_Input * 100 /
      (0.34 * `Fuel, Power, Light, Lubricants` + 0.64 * `Industrial Raw Materials`),
    ln_GO_real              = log(GO_real),
    ln_GVA_real             = log(GVA_real),
    ln_Fixed_Capital_real   = log(Fixed_Capital_real),
    ln_Working_Capital_real = log(Working_Capital_real),
    ln_Material_Input_real  = log(Material_Input_real),
    ln_Number_of_Workers    = log(Number_of_Workers),
    ln_Number_of_Employees  = log(Number_of_Employees),
    ln_Number_of_Factories  = log(Number_of_Factories),
    period                  = ifelse(Year <= 1953, "Pre", "Post")
  )
# --- Step 2: Variable list (log, deflated where monetary) ---
trend_vars <- c(
  ln_GO_real              = "Gross Output",
  ln_GVA_real             = "Gross Value Added",
  ln_Number_of_Workers    = "Number of Workers",
  ln_Number_of_Employees  = "Number of Employees",
  ln_Material_Input_real  = "Material Input",
  ln_Number_of_Factories  = "Number of Factories",
  ln_Fixed_Capital_real   = "Fixed Capital",
  ln_Working_Capital_real = "Working Capital"
)
# --- Step 3: Plotting function ---
plot_parallel_trends <- function(varname, label) {
  df <- industry_panel_trends %>%
    group_by(Year, targeted, period) %>%
    summarise(mean_val = mean(.data[[varname]], na.rm = TRUE), .groups = "drop") %>%
    mutate(Sector = ifelse(targeted == 1, "targeted", "non-targeted"))
  
  ggplot(df, aes(x = Year, y = mean_val, color = Sector, linetype = Sector,
                 group = interaction(Sector, period))) +
    geom_line(linewidth = 0.8) +
    geom_vline(xintercept = 1956, color = "red") +
    scale_color_manual(values = c("non-targeted" = "darkred", "targeted" = "blue")) +
    scale_x_continuous(breaks = 1951:1965) +
    labs(x = NULL, y = NULL) +
    theme_gray() +
    theme(axis.text.x = element_text(colour = "black"), axis.text.y = element_text(colour = "black"))
}
# --- Step 4: Generate and save all 8 plots ---
out_dir <- "~/Desktop/Results/Figures/Parallel_Trends/"
for (v in names(trend_vars)) {
  p <- plot_parallel_trends(v, trend_vars[[v]])
  ggsave(filename = paste0(out_dir, "Trend_", v, ".png"),
         plot = p, width = 6, height = 4, dpi = 300)
}

#Learning-by-Doing
#Estimated on the Harmonized Panel for the full period 1957-65
lbd_harmonized_panel <- industry_panel %>%
  filter(!(Year %in% c(1951:1956)))

#Merging the price index data
lbd_harmonized_panel <- left_join(
  lbd_harmonized_panel,
  select(ind_prices_reindexed, Year, `All Commodities`, `Fuel, Power, Light, Lubricants`,
         `Industrial Raw Materials`,`Machinery and Transport Equipments`,`Intermediate Products`),
  by = "Year"
)

#Deflating Gross Value Added (GVA), Material Input, and Capital Input
lbd_harmonized_panel <- lbd_harmonized_panel %>%
  mutate(
    real_GVA = (GVA*100) / (`All Commodities`),
    real_material_input = (Material_Input*100)/(0.34*`Fuel, Power, Light, Lubricants` + 0.64*`Industrial Raw Materials`),
    real_capital_input = (Productive_Capital*100)/(0.58*`Machinery and Transport Equipments` + 0.42*`Intermediate Products`)
  )

#Creating the labor productivity variable
lbd_harmonized_panel <- lbd_harmonized_panel %>%
  mutate(worker_productivity = real_GVA/Number_of_Workers)

#Factor version of sector_code
lbd_harmonized_panel <- lbd_harmonized_panel %>%
  mutate(sector_code_f = factor(sector_code))

#Creating the Experience Variable
#FIX: Number_of_Workers (and therefore worker_productivity) is missing for
#1959 in the harmonized panel due to the classification change. Plain
#cumsum() propagates that NA forward through every later year for a sector.
#replace_na(...,0) treats the missing 1959 contribution as zero for that one
#year, then resumes accumulating real values from 1960 onward, instead of
#silently wiping out 1960-65.
lbd_harmonized_panel <- lbd_harmonized_panel %>%
  arrange(sector_code_f, Year) %>%
  group_by(sector_code_f) %>%
  mutate(Experience = cumsum(replace_na(worker_productivity, 0))) %>%
  ungroup()

#Creating the size variable
lbd_harmonized_panel <- lbd_harmonized_panel %>%
  mutate(size = Number_of_Workers/Number_of_Factories)

#Creating control variables for this regression
lbd_harmonized_panel <- lbd_harmonized_panel %>%
  mutate(mat_input_per_worker = real_material_input/Number_of_Workers,
         cap_input_per_worker = real_capital_input/Number_of_Workers)

#Reparametrized regressors (Experience_T's coefficient IS theta+beta directly)
lbd_harmonized_panel <- lbd_harmonized_panel %>%
  mutate(Experience_NT = Experience * (1 - targeted),
         Experience_T  = Experience * targeted)

#Regression
harmon_twfe1 <- feols(worker_productivity ~ Experience + targeted:Experience + size + mat_input_per_worker + cap_input_per_worker
                      |sector_code_f + Year,
                      data = lbd_harmonized_panel, vcov = ~sector_code_f)
summary(harmon_twfe1)

harmon_twfe2 <- feols(worker_productivity ~ Experience + targeted:Experience + size
                      |sector_code_f + Year,
                      data = lbd_harmonized_panel, vcov = ~sector_code_f)
summary(harmon_twfe2)

harmon_twfe1_r <- feols(worker_productivity ~ Experience_NT + Experience_T + size + mat_input_per_worker + cap_input_per_worker
                        |sector_code_f + Year,
                        data = lbd_harmonized_panel, vcov = ~sector_code_f)

harmon_twfe2_r <- feols(worker_productivity ~ Experience_NT + Experience_T + size
                        |sector_code_f + Year,
                        data = lbd_harmonized_panel, vcov = ~sector_code_f)

#LBD Estimated on the ASI Panel for the period 1960-65
asi_panel <- asi_factory %>%
  pivot_longer(
    cols = -c(`ASI Classification Code`,`Industrial Sector`),
    names_to = c(".value", "Year"),
    names_sep = "\\."
  ) %>%
  mutate(
    Year = as.numeric(Year)
  )

#Filtering the ASI Panel for the period 1960-65 (excluding 1959)
asi_panel <- asi_panel %>%
  filter(!(Year %in% c(1959)))

#Merging the price index data
asi_panel <- left_join(
  asi_panel,
  select(ind_prices_reindexed, Year, `All Commodities`, `Fuel, Power, Light, Lubricants`,
         `Industrial Raw Materials`,`Machinery and Transport Equipments`,`Intermediate Products`),
  by = "Year"
)

#Deflating Gross Value Added (GVA), Material Input, and Capital Input
asi_panel <- asi_panel %>%
  mutate(
    real_GVA = (GVA*100) / (`All Commodities`),
    real_material_input = (Material_Input*100)/(0.34*`Fuel, Power, Light, Lubricants` + 0.64*`Industrial Raw Materials`),
    real_capital_input = (Productive_Capital*100)/(0.58*`Machinery and Transport Equipments` + 0.42*`Intermediate Products`)
  )

#Creating the labor productivity variable
asi_panel <- asi_panel %>%
  mutate(worker_productivity = real_GVA/Number_of_Workers)

#Factor version of the ASI classification code
asi_panel <- asi_panel %>%
  mutate(asi_code_f = factor(`ASI Classification Code`))

#Creating the Experience Variable (no 1959 gap here, but replace_na is a
#harmless no-op if there's nothing missing)
asi_panel <- asi_panel %>%
  arrange(asi_code_f, Year) %>%
  group_by(asi_code_f) %>%
  mutate(Experience = cumsum(replace_na(worker_productivity, 0))) %>%
  ungroup()

#Creating the size variable
asi_panel <- asi_panel %>%
  mutate(size = Number_of_Workers/Factory_Universe)

#Creating control variables for this regression
asi_panel <- asi_panel %>%
  mutate(mat_input_per_worker = real_material_input/Number_of_Workers,
         cap_input_per_worker = real_capital_input/Number_of_Workers)

#Adding the targeted variable
targeted_sectors <- c("311","334","341","342","350","360","370")
asi_panel <- asi_panel %>%
  mutate(
    targeted = ifelse(`ASI Classification Code` %in% targeted_sectors, 1, 0)
  )

#Reparametrized regressors
asi_panel <- asi_panel %>%
  mutate(Experience_NT = Experience * (1 - targeted),
         Experience_T  = Experience * targeted)

#Regression
asi_twfe1 <- feols(worker_productivity ~ Experience + Experience:targeted + size + mat_input_per_worker + cap_input_per_worker
                   |asi_code_f + Year,
                   data = asi_panel, vcov = ~asi_code_f)
summary(asi_twfe1)

asi_twfe2 <- feols(worker_productivity ~ Experience + Experience:targeted + size
                   |asi_code_f + Year,
                   data = asi_panel, vcov = ~asi_code_f)
summary(asi_twfe2)

asi_twfe1_r <- feols(worker_productivity ~ Experience_NT + Experience_T + size + mat_input_per_worker + cap_input_per_worker
                     |asi_code_f + Year,
                     data = asi_panel, vcov = ~asi_code_f)
asi_twfe2_r <- feols(worker_productivity ~ Experience_NT + Experience_T + size
                     |asi_code_f + Year,
                     data = asi_panel, vcov = ~asi_code_f)

## ======================================================================
## Inference: wild cluster bootstrap (WCB), matching every other table
## in the paper.
## ======================================================================

sig_stars_p_local <- if (exists("sig_stars_p")) sig_stars_p else function(p) {
  if (is.na(p)) return("")
  if (p < 0.01) return("***")
  if (p < 0.05) return("**")
  if (p < 0.1) return("*")
  ""
}

format_cell_wcb <- function(mod, term, clustid_var, B, seed) {
  b <- coef(mod)
  if (!(term %in% names(b))) return(c("", ""))
  set.seed(seed)
  bt <- tidy(boottest(mod, param = term, clustid = clustid_var, B = B))
  est <- unname(b[term])
  p <- bt$p.value[1]
  c(sprintf("%.4f%s", est, sig_stars_p_local(p)), sprintf("[WCB p=%.3f]", p))
}

build_lbd_table_wcb <- function(mod_nc, mod_c, mod_nc_r, mod_c_r, clustid_var,
                                beta_name = "Experience:targeted", B = 9999, seed_base = 1) {
  terms_simple <- list(
    list(label = "Experience", name = "Experience"),
    list(label = "Size", name = "size"),
    list(label = "Experience x Targeted", name = beta_name),
    list(label = "Material Input per worker", name = "mat_input_per_worker"),
    list(label = "Capital per worker", name = "cap_input_per_worker")
  )
  rows <- lapply(seq_along(terms_simple), function(i) {
    tm <- terms_simple[[i]]
    cell_nc <- format_cell_wcb(mod_nc, tm$name, clustid_var, B, seed_base*100 + i)
    cell_c  <- format_cell_wcb(mod_c,  tm$name, clustid_var, B, seed_base*100 + i + 50)
    data.frame(Term = c(tm$label, ""), Col1 = cell_nc, Col2 = cell_c, stringsAsFactors = FALSE)
  }) %>% bind_rows()
  
  comb_nc <- format_cell_wcb(mod_nc_r, "Experience_T", clustid_var, B, seed_base*100 + 90)
  comb_c  <- format_cell_wcb(mod_c_r,  "Experience_T", clustid_var, B, seed_base*100 + 91)
  comb_row <- data.frame(Term = c("Experience + Experience x Targeted", ""), Col1 = comb_nc, Col2 = comb_c, stringsAsFactors = FALSE)
  
  footer <- data.frame(
    Term = c("Observations", "R2", "Industry Fixed Effects", "Time Fixed Effects"),
    Col1 = c(as.character(nobs(mod_nc)), sprintf("%.5f", r2(mod_nc, type = "r2")), "Yes", "Yes"),
    Col2 = c(as.character(nobs(mod_c)),  sprintf("%.5f", r2(mod_c,  type = "r2")),  "Yes", "Yes"),
    stringsAsFactors = FALSE
  )
  bind_rows(rows, comb_row, footer)
}

lbd_table_harmonized <- build_lbd_table_wcb(harmon_twfe2, harmon_twfe1, harmon_twfe2_r, harmon_twfe1_r, "sector_code_f", seed_base = 1)
lbd_table_asi        <- build_lbd_table_wcb(asi_twfe2, asi_twfe1, asi_twfe2_r, asi_twfe1_r, "asi_code_f", seed_base = 2)

wcb_note <- "Wild cluster bootstrap p-values (9999 replications, clustered at the industry level) reported in brackets below each estimate. Signif. Codes: ***: 0.01, **: 0.05, *: 0.1"

## --- Table 7: Harmonized panel (1957-65) ---
write_xlsx(lbd_table_harmonized, "~/Desktop/Results/Tables/Table7_LBD_Harmonized.xlsx")
lbd_table_harmonized %>%
  kable(format = "latex", booktabs = TRUE, row.names = FALSE, escape = FALSE,
        col.names = c("", "(1)", "(2)"),
        caption = "Learning-By-Doing: Harmonized Panel (1957-65)") %>%
  add_header_above(c(" " = 1, "Labor Productivity" = 2)) %>%
  footnote(general = wcb_note, general_title = "Notes:", footnote_as_chunk = TRUE, escape = FALSE) %>%
  save_kable("~/Desktop/Results/Tables/Table7_LBD_Harmonized.tex")

## --- Table 8: ASI panel (1960-65) ---
write_xlsx(lbd_table_asi, "~/Desktop/Results/Tables/Table8_LBD_ASI.xlsx")
lbd_table_asi %>%
  kable(format = "latex", booktabs = TRUE, row.names = FALSE, escape = FALSE,
        col.names = c("", "(1)", "(2)"),
        caption = "Learning-By-Doing: ASI Panel (1960-65)") %>%
  add_header_above(c(" " = 1, "Labor Productivity" = 2)) %>%
  footnote(general = wcb_note, general_title = "Notes:", footnote_as_chunk = TRUE, escape = FALSE) %>%
  save_kable("~/Desktop/Results/Tables/Table8_LBD_ASI.tex")
