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

asi_sample <- read_excel("~/Desktop/Research/JMP/Data/SSMI_ASI.xlsx", sheet="ASI_Sample_Sector_1959_65")
asi_census <- read_excel("~/Desktop/Research/JMP/Data/SSMI_ASI.xlsx", sheet="ASI_Census_Sector_1959_65")
sample_census_harmony <- read_excel("~/Desktop/Research/JMP/Data/SSMI_ASI.xlsx", sheet = "ASI_Sample_Census_Harmonization")
asi_ssmi_harmony <- read_excel("~/Desktop/Research/JMP/Data/SSMI_ASI.xlsx", sheet="ASI_SSMI_Harmonization")
industrial_price_sectors <- read_excel("~/Desktop/Research/JMP/Data/SSMI_ASI.xlsx", sheet="Industrial_Price_Sectors")
ind_prices <- read_excel("~/Desktop/Research/JMP/Data/SSMI_ASI.xlsx", sheet="Industrial_Prices")
asi_sample_other_vars <- read_excel("~/Desktop/Research/JMP/Data/SSMI_ASI.xlsx", sheet="Learning_ASI_Sample")
asi_census_other_vars <- read_excel("~/Desktop/Research/JMP/Data/SSMI_ASI.xlsx", sheet="Learning_ASI_Census")

# --- PART 1: Prepare Merged ASI Sample Data ---

# ASI Sample - GO and GVA
asi_sample <- asi_sample %>%
  mutate(across(3:23, as.numeric)) %>%
  replace(is.na(.), 0) %>%
  mutate(`Labor Bureau Classification` = as.character(`Labor Bureau Classification`)) %>%
  mutate(`Labor Bureau Classification` = case_when(
    `Labor Bureau Classification` == "10" ~ "239",
    `Labor Bureau Classification` == "512" ~ "511",
    TRUE ~ `Labor Bureau Classification`
  )) %>%
  group_by(`Labor Bureau Classification`) %>%
  summarise(
    `Industrial Sector` = first(`Industrial Sector`),
    across(where(is.numeric), sum, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(`Labor Bureau Classification`)

# ASI Sample - Other Variables
asi_sample_other_vars <- asi_sample_other_vars %>%
  rename(`Labor Bureau Classification` = `ASI Classification Code`) %>%
  mutate(`Labor Bureau Classification` = as.character(`Labor Bureau Classification`)) %>%
  mutate(`Labor Bureau Classification` = case_when(
    `Labor Bureau Classification` == "10" ~ "239",
    TRUE ~ `Labor Bureau Classification`
  )) %>%
  group_by(`Labor Bureau Classification`) %>%
  summarise(across(where(is.numeric), sum, na.rm = TRUE), .groups = "drop") %>%
  arrange(`Labor Bureau Classification`)

# Merge into a single sample dataframe
merged_asi_sample <- left_join(asi_sample_other_vars, asi_sample, by = "Labor Bureau Classification") %>%
  relocate(`Industrial Sector`, .after = `Labor Bureau Classification`)


# --- PART 2: Prepare Merged ASI Census Data ---

# ASI Census - GO and GVA
asi_census <- asi_census %>%
  mutate(across(3:22, as.numeric)) %>%
  replace(is.na(.), 0) %>%
  mutate(`ASI Classification Code` = as.character(`ASI Classification Code`)) %>%
  filter(!(`ASI Classification Code` %in% c("292", "512")))

# ASI Census - Other Variables
asi_census_other_vars <- asi_census_other_vars %>%
  mutate(`ASI Classification Code` = as.character(`ASI Classification Code`))

# Merge into a single census dataframe
merged_asi_census <- left_join(asi_census_other_vars, asi_census, by = "ASI Classification Code") %>%
  relocate(`Industrial Sector`, .after = `ASI Classification Code`)


# --- PART 3: Reshape Data to Long Format ---

# Reshape the census data
census_long <- merged_asi_census %>%
  select(-starts_with("GI.")) %>%
  pivot_longer(
    cols = contains("."),
    names_to = c("Metric", "Year"),
    names_pattern = "(.+)\\.(\\d{4})",
    values_to = "Value_census"
  ) %>%
  filter(!(Metric %in% c("Fixed_Capital", "Working_Capital") & Year == "1959"))

# Reshape the sample data
sample_long <- merged_asi_sample %>%
  select(-c("Number_of_Workers.1959", "Material_Input.1959",
            "Fixed_Capital.1959", "Working_Capital.1959")) %>%
  select(-starts_with("GI.")) %>%
  pivot_longer(
    cols = contains("."),
    names_to = c("Metric", "Year"),
    names_pattern = "(.+)\\.(\\d{4})",
    values_to = "Value_sample"
  )

# --- PART 4: Harmonize and Combine ---

# Prepare the Harmonization Scheme
codes_to_remove <- c("201", "202", "204", "208", "214", "242", "244", "259", "312", "329",
                     "382", "384", "389", "391", "392", "393", "394", "395", "399")

sample_census_harmony <- sample_census_harmony %>%
  filter(!(`ASI Classification Code` %in% codes_to_remove)) %>%
  mutate(`Labor Bureau Classification` = if_else(`Labor Bureau Classification` == "010 + 239", "239", `Labor Bureau Classification`))

# Add ASI Classification Code to the sample data
sample_long_harmonized <- sample_long %>%
  left_join(sample_census_harmony, by = "Labor Bureau Classification") %>%
  select(-starts_with("Industrial Sector")) # Avoid column name conflicts

# Prepare the census data by duplicating 'Number_of_Factories' to align with sample metrics
census_long_prepped <- census_long %>%
  # Temporarily remove the original factory rows
  filter(Metric != "Number_of_Factories") %>%
  # Add back two new sets of factory rows, one for each new metric
  bind_rows(
    census_long %>%
      filter(Metric == "Number_of_Factories") %>%
      mutate(Metric = "Factory_Universe"),
    census_long %>%
      filter(Metric == "Number_of_Factories") %>%
      mutate(Metric = "Factory_Sample")
  )

# --- PART 5: Create Final DataFrame (Streamlined Logic) ---
# This single operation now replaces the separate factory/other metric chunks.
asi_factory_long <- census_long_prepped %>%
  left_join(sample_long_harmonized, by = c("ASI Classification Code", "Metric", "Year")) %>%
  mutate(Value_factory = rowSums(across(c(Value_census, Value_sample)), na.rm = TRUE)) %>%
  select(`ASI Classification Code`, `Industrial Sector`, Metric, Year, Value_factory)

# Pivot to the final wide format
asi_factory <- asi_factory_long %>%
  pivot_wider(
    names_from = c(Metric, Year),
    values_from = Value_factory,
    names_sep = "."
  )


#Remove unnecessary dataframes
rm(asi_sample,
asi_sample_other_vars,
census_long,
census_long_prepped,
merged_asi_census,
merged_asi_sample,
sample_long,
sample_long_harmonized,
asi_census,
asi_census_other_vars)


#SSMI 1951-58#
# --- PART 1: Load Data ---
ssmi <- read_excel("/Users/aroram/Desktop/Research/JMP/Data/SSMI_ASI.xlsx", sheet="SSMI_1951_58")
ssmi_other_vars <- read_excel("/Users/aroram/Desktop/Research/JMP/Data/SSMI_ASI.xlsx", sheet="Learning_SSMI")
ssmi_control_vars <- read_excel("/Users/aroram/Desktop/Research/JMP/Data/SSMI_ASI.xlsx", sheet="controls_1951_53")

# Combine power and non-power capital columns for 1951 into single Fixed/Working Capital columns
ssmi <- ssmi %>%
  mutate(
    Fixed_Capital.1951   = rowSums(across(c(Fixed_Capital_Power.1951, Fixed_Capital_No_Power.1951)), na.rm = TRUE),
    Working_Capital.1951 = rowSums(across(c(Working_Capital_Power.1951, Working_Capital_No_Power.1951)), na.rm = TRUE)
  ) %>%
  select(-Fixed_Capital_Power.1951, -Fixed_Capital_No_Power.1951,
         -Working_Capital_Power.1951, -Working_Capital_No_Power.1951)
# --- PART 2: Combine Raw SSMI Data ---
# The three SSMI files are joined into a single comprehensive dataframe.

# Standardize the code column names before joining
ssmi <- ssmi %>% rename(SSMI_Code = `CMI (Census of Manufacturing Industries) Code`)
ssmi_other_vars <- ssmi_other_vars %>% rename(SSMI_Code = `CMI (Census of Manufacturing Industries) Code`)
ssmi_control_vars <- ssmi_control_vars %>% rename(SSMI_Code = `Industry Code`)

# Join the three dataframes using a full join to keep all records
ssmi_combined_wide <- ssmi %>%
  full_join(ssmi_other_vars, by = "SSMI_Code") %>%
  full_join(ssmi_control_vars, by = "SSMI_Code") %>%
  # Coalesce the two Industrial Sector names that were created during the first join.
  # Since ssmi_control does not have this column, there is no third version to coalesce.
  mutate(`Industrial Sector` = coalesce(`Industrial Sector.x`, `Industrial Sector.y`)) %>%
  # Select and reorder columns, removing the original .x and .y columns
  select(SSMI_Code, `Industrial Sector`, everything(), -`Industrial Sector.x`, -`Industrial Sector.y`)


# --- PART 3: Create Harmonization Maps ---
# This logic correctly handles both many-to-one and one-to-many relationships
# by creating a unique Group ID for each mapping rule.

# Create a unique Group ID for each rule in the original harmony table
harmony_with_id <- asi_ssmi_harmony %>%
  rename(ASI_Code_Raw = `ASI Code Number`, SSMI_Code_Raw = `SSMI Code Number`) %>%
  mutate(GroupID = row_number())

# Create a clean mapping from each individual SSMI code to its Group ID
ssmi_map <- harmony_with_id %>%
  select(GroupID, SSMI_Code_Raw) %>%
  separate_rows(SSMI_Code_Raw, sep = "\\s*\\+\\s*") %>%
  mutate(SSMI_Code = trimws(as.character(SSMI_Code_Raw))) %>%
  select(GroupID, SSMI_Code)

# Create a clean mapping from each individual ASI code to its Group ID
asi_map <- harmony_with_id %>%
  select(GroupID, ASI_Code_Raw) %>%
  separate_rows(ASI_Code_Raw, sep = "\\s*\\+\\s*") %>%
  mutate(ASI_Code = trimws(as.character(ASI_Code_Raw))) %>%
  select(GroupID, ASI_Code)

# Create a metadata table to add back sector names at the end
harmony_metadata <- harmony_with_id %>%
  select(GroupID, `Industrial Sector`)


# --- PART 4: Process and Harmonize SSMI Data (1951-1958) ---

# Reshape SSMI data to long format and apply cleaning rules
ssmi_long <- ssmi_combined_wide %>%
  mutate(SSMI_Code = as.character(SSMI_Code)) %>%
  mutate(across(-c(SSMI_Code, `Industrial Sector`), as.numeric)) %>%
  pivot_longer(
    cols = -c(SSMI_Code, `Industrial Sector`),
    names_to = c("Metric", "Year"),
    names_pattern = "(.+)\\.(\\d{4})",
    values_to = "Value",
    values_drop_na = FALSE
  ) %>%
  mutate(
    Year = as.numeric(Year),
    Value = if_else(Year %in% 1954:1956, NA_real_, Value)
  ) %>%
  filter(!is.na(Value))

# Aggregate SSMI data by the new GroupID
ssmi_aggregated_long <- ssmi_long %>%
  left_join(ssmi_map, by = "SSMI_Code") %>%
  filter(!is.na(GroupID)) %>%
  group_by(GroupID, Metric, Year) %>%
  summarise(Value = sum(Value, na.rm = TRUE), .groups = "drop")


# --- PART 5: Process and Harmonize ASI Data (1959-1965) ---

# Reshape the asi_factory data to a long format
asi_factory_long <- asi_factory %>%
  pivot_longer(
    cols = -c(`ASI Classification Code`, `Industrial Sector`),
    names_to = c("Metric", "Year"),
    names_pattern = "(.+)\\.(\\d{4})",
    values_to = "Value",
    values_drop_na = TRUE
  ) %>%
  mutate(
    `ASI Classification Code` = as.character(`ASI Classification Code`),
    Year = as.numeric(Year)
  )

# Aggregate ASI data by the new GroupID
asi_aggregated_long <- asi_factory_long %>%
  left_join(asi_map, by = c("ASI Classification Code" = "ASI_Code")) %>%
  filter(!is.na(GroupID)) %>%
  group_by(GroupID, Metric, Year) %>%
  summarise(Value = sum(Value, na.rm = TRUE), .groups = "drop")


# --- PART 6: Combine and Finalize ---

# Combine the two aggregated datasets
harmonized_data_long <- bind_rows(ssmi_aggregated_long, asi_aggregated_long)

# Pivot back to the final wide format
harmonized_data <- harmonized_data_long %>%
  pivot_wider(
    names_from = c(Metric, Year),
    values_from = Value,
    names_sep = "."
  ) %>%
  # Join with metadata to bring back the industrial sector names
  left_join(harmony_metadata, by = "GroupID") %>%
  # Arrange columns for readability
  select(GroupID, `Industrial Sector`, starts_with("Factory_"), starts_with("GO."), everything()) %>%
  arrange(GroupID)

harmonized_data <- harmonized_data %>%
  mutate(`Sector Code` = harmony_with_id$ASI_Code_Raw) %>%
  select(-GroupID) %>%
  relocate(`Sector Code`, .before = `Industrial Sector`)

#Storing non-harmonized pooled dataset by breaking the harmonization and dropping those sectors that are not part of the harmonization map
#From asi_long, remove Sector 382:Manufacture of Railroad Equipment
#From ssmi_long, remove Sectors 38: plastics (including gramophone records)
#Sector 61: Railway Wagon Mfg, and 63: Unspecified industries

asi_factory_long <- asi_factory_long %>%
  filter(`Industrial Sector` != "manufacture of rail-road equipment")

ssmi_long <- ssmi_long %>%
  filter(!`Industrial Sector` %in% c("plastics (including gramophone records)",
                                     "railway wagon manufacturing",
                                     "unspecified industries"))

asi_factory_long <- asi_factory_long %>%
  rename(`Sector Code` = `ASI Classification Code`)

ssmi_long <- ssmi_long %>%
  rename(`Sector Code` = SSMI_Code)

#Rowbind SSMI and ASI Factory Data one below the other
nonharmonized_data_long <- bind_rows(ssmi_long, asi_factory_long)

#Convert the nonharmonized data to a wide format
nonharmonized_data <- nonharmonized_data_long %>%
  pivot_wider(
    names_from = c(Metric, Year),
    values_from = Value,
    names_sep = "."
  )

#Deflators for Control Variables in the Harmonized Panel
#Base Conversion to 1961=100
ind_prices_reindexed <- ind_prices %>%
  mutate(across(-Year, ~ (.x / .x[Year == 1961]) * 100))

#Converting the harmonized data to a long format again to join the price index with it
harmonized_data_long <- harmonized_data %>%
  pivot_longer(
    cols = -c(`Sector Code`, `Industrial Sector`),
    names_to = c("Variable", "Year"),
    names_pattern = "(.*)\\.(.*)"
  )
#Join price index data with the harmonized data in long format
final_data_long <- harmonized_data_long %>%

  # 1. IMPORTANT: Ensure the 'Year' column is numeric for a correct join
  mutate(Year = as.numeric(Year)) %>%

  # 2. Join with the price index data by the "Year" column
  left_join(ind_prices_reindexed, by = c("Year"))

#Deflate data and create control variables
control_variables <- final_data_long %>%
  # 1. Filter for only the years and variables needed for the calculation
  filter(
    Year %in% c(1951, 1952, 1953),
    Variable %in% c("Worker_wage", "Number_of_Workers", "GVA", "Material_Input",
                    "Factory_Universe")
  ) %>%

  # 2. Temporarily pivot wider to make the next calculation step much easier
  # We select only the columns we need first
  select(`Sector Code`, `Industrial Sector`, Year, Variable, value, `All Commodities`,
         `Fuel, Power, Light, Lubricants`, `Industrial Raw Materials`) %>%
  pivot_wider(
    names_from = Variable,
    values_from = value
  ) %>%

  # 3. Calculate the real wage per worker for each year
  # Formula: (Nominal Wage / Deflator) / Number of Workers
  mutate(
    real_wage_per_worker = (Worker_wage*100 / `All Commodities`) / Number_of_Workers,
    real_worker_productivity = (GVA*100 / `All Commodities`) / Number_of_Workers,
    plant_size_ratio = Number_of_Workers / Factory_Universe,
    real_material_input_per_worker = (Material_Input*100/(0.34*`Fuel, Power, Light, Lubricants` + 0.64*`Industrial Raw Materials`)) / Number_of_Workers,
    real_material_input = (Material_Input*100/(0.34*`Fuel, Power, Light, Lubricants` + 0.64*`Industrial Raw Materials`))
  ) %>%

  # 4. Group by sector to calculate the average across the 3 years
  group_by(`Sector Code`, `Industrial Sector`) %>%

  # 5. Calculate the mean.
  summarise(
    avg_wage_bill_per_worker = mean(real_wage_per_worker, na.rm = TRUE),
    avg_worker_productivity = mean(real_worker_productivity, na.rm = TRUE),
    avg_plant_size = mean(plant_size_ratio, na.rm = TRUE),
    avg_mat_input_per_worker = mean(real_material_input_per_worker, na.rm = TRUE),
    avg_mat_input = mean(real_material_input, na.rm=TRUE),
    .groups = 'drop'
  )

#Joining control variables with rest of the dataset
harmonized_data <- harmonized_data %>%
                   left_join(control_variables, by = c("Sector Code", "Industrial Sector"))

harmonized_data <- harmonized_data %>%
                   relocate(starts_with("avg_"), .after = `Industrial Sector`)

#Harmonized Panel Regression
#Setting up the data
industry_panel <- harmonized_data %>%
                  mutate(GO.1954 = NA,
                         GO.1955 = NA,
                         GO.1956 = NA,
                         GVA.1954 = NA,
                         GVA.1955 = NA,
                         GVA.1956 = NA,
                         Material_Input.1954 = NA,
                         Material_Input.1955 = NA,
                         Material_Input.1956 = NA,
                         Number_of_Employees.1954 = NA,
                         Number_of_Employees.1955 = NA,
                         Number_of_Employees.1956 = NA,
                         Number_of_Workers.1954 = NA,
                         Number_of_Workers.1955 = NA,
                         Number_of_Workers.1956 = NA,
                         Factory_Universe.1954 = NA,
                         Factory_Universe.1955 = NA,
                         Factory_Universe.1956 = NA)

industry_panel <- industry_panel %>%
  pivot_longer(
    cols = contains("."), # Selects all columns with a "."
    names_to = c(".value", "Year"),
    names_sep = "\\."
  ) %>%
  # Best practice: convert Year to numeric before sorting
  mutate(Year = as.numeric(Year)) %>%

  # Sort the data frame by Sector and then by Year
  arrange(`Sector Code`, Year) %>%
  relocate(Year, .after = `Industrial Sector`)

#Taking natural logs of values
industry_panel <- industry_panel %>%
  rename(Number_of_Factories = Factory_Universe) %>%
  mutate(ln_GO = log(GO),
         ln_GVA = log(GVA),
         ln_employees = log(Number_of_Employees),
         ln_factories = log(Number_of_Factories),
         log_avg_wage = log(avg_wage_bill_per_worker),
         log_avg_plant_size = log(avg_plant_size),
         log_avg_productivity = log(avg_worker_productivity),
         log_avg_material = log(avg_mat_input)
  )

#Adding time-trend
industry_panel <- industry_panel %>%

  # 2. Group by Sector
  group_by(`Sector Code`) %>%

  # 3. Create the 'time_trend'
  # - !is.na(ln_GO) creates 1s for good data, 0s for NAs
  # - cumsum() counts the good data (e.g., 1, 2, 3, 3, 3, 3, 4, 5...)
  # - ifelse() then turns the 3s in the NA rows into NAs
  mutate(
    time_trend = ifelse(is.na(ln_GO),
                        NA,
                        cumsum(!is.na(ln_GO)))
  ) %>%

  # 4. Ungroup
  ungroup()

#Adding dummy for treated sectors
targeted_sectors <- c("311","334","341","342","350+360+370")
industry_panel <- industry_panel %>%
  mutate(targeted = as.numeric(`Sector Code` %in% targeted_sectors)) %>%
  relocate(targeted, .after = Year)
#Adding dummy for Post-Treatment period
Post <- c(1957:1965)
industry_panel <- industry_panel %>%
                  mutate(Post = as.numeric(Year %in% Post)) %>%
                  relocate(Post, .after = targeted)

industry_panel <- industry_panel %>%
  rename(sector_code = `Sector Code`)

industry_panel <- industry_panel %>%
  mutate(sector_code_f = as.factor(sector_code))

# ============================================================
# OUTCOME VARIABLES
# ============================================================
outcome_vars <- c(
  ln_GO         = "Log of Gross Output",
  ln_GVA        = "Log of Gross Value Added",
  ln_employees  = "Log of Number of Employees",
  ln_factories  = "Log of Number of Factories"
)

# Fixed per-variable bootstrap seeds. IMPORTANT: these are keyed by variable
# NAME, not by position in outcome_vars. Seeding off `which(names(outcome_vars)
# == var)` is fragile -- a variable's position shifts whenever outcome_vars is
# reordered or trimmed (as it was here, from 8 outcomes down to 4), which
# silently changes the seed passed to boottest() for that variable and moves
# its WCB p-values run to run. Keying by name keeps each variable's seed (and
# therefore its bootstrap draws) fixed regardless of what else is in the vector.
var_seeds <- c(ln_GO = 1, ln_GVA = 2, ln_employees = 3, ln_factories = 4)

# --- Step 1: Run static DiD for each outcome, keep WCB inference + Observations + R2 ---

static_did_results <- lapply(names(outcome_vars), function(var) {

  f <- as.formula(paste0(var, " ~ targeted:Post + factor(Year) | sector_code_f"))
  mod <- feols(f, data = industry_panel, vcov = ~ sector_code_f)

  set.seed(var_seeds[[var]])
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
  set.seed(var_seeds[[var]])
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

## --- Core function: fit dynamic DD model, get WCB per-year CI/p-value ---
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
    geom_point(size = 2.2) +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, linewidth = 0.7) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    scale_x_continuous(breaks = seq(1951, 1965, 1)) +
    theme_gray(base_size = 16) +
    theme(
      axis.text.x = element_text(colour = "black", size = 13, angle = 45, hjust = 1),
      axis.text.y = element_text(colour = "black", size = 14),
      axis.title.y = element_text(size = 15)
    ) +
    geom_vline(xintercept = 1956, color = "red") +
    theme(axis.title.x = element_blank()) +
    ylab("Estimate and 95% Conf. Int.")
  ggsave(save_path, p, width = 7, height = 5, dpi = 300)
  p
}

## --- Run for the 4 retained variables, both specifications ---
dyn_results_nocontrols <- lapply(names(outcome_vars), function(var) {
  res <- run_dynamic_wcb(var, controls = FALSE, seed = var_seeds[[var]])
  plot_dynamic_wcb(res, paste0("~/Desktop/Results/Figures/No_Controls_Harmonized/event_study_", var, "_noctrl_wcb.png"))
  res
})
names(dyn_results_nocontrols) <- names(outcome_vars)

dyn_results_controls <- lapply(names(outcome_vars), function(var) {
  res <- run_dynamic_wcb(var, controls = TRUE, seed = var_seeds[[var]])
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

# ============================================================
# Randomization Inference (t-statistic based) for the Harmonized Panel
# ============================================================
# Motivation: WCB relies on asymptotic refinements that can still be
# unreliable when the number of TREATED clusters is very small -- here
# only 5 of the 36 industries are targeted (see `targeted_sectors` above).
# Following MacKinnon and Webb (2020, Journal of Econometrics,
# "Randomization inference for difference-in-differences with few treated
# clusters"), I use their recommended T-STATISTIC-BASED randomization
# inference procedure rather than a coefficient-based one: MacKinnon and
# Webb show that comparing raw coefficients across permutations can be
# unreliable when clusters are heterogeneous (e.g. different numbers of
# years observed per industry, as is the case here), whereas comparing
# t-statistics is robust to this heterogeneity because each permutation's
# own standard error is used to standardize its own coefficient.
#
# Procedure, applied to all four specifications reported in the paper
# (static no-controls / Table 2, static with-controls / Table C1, dynamic
# no-controls / Figure 1, dynamic with-controls / Figure C1): repeatedly
# reassign the "targeted" label at random among the 36 industries (holding
# the number of targeted industries fixed at 5, matching the true split),
# re-estimate the same specification, compute the t-statistic on the
# coefficient(s) of interest in each permutation, and see where the
# actual t-statistic falls in that permutation-based null distribution.

set.seed(2024)
n_perms_static  <- 5000
n_perms_dynamic <- 2000   # fewer reps than the static checks: each dynamic
                          # permutation re-estimates ~11 event-time
                          # coefficients per outcome instead of 1, so this
                          # keeps total runtime reasonable.

# One row per industry with its TRUE targeted status
industry_lookup <- industry_panel %>%
  distinct(sector_code_f, targeted)

n_treated <- sum(industry_lookup$targeted)

# --- Static DD, WITHOUT controls (Table 2) ---

# Returns the t-statistic on placebo_targeted:Post
run_static_tstat <- function(data, var) {
  f <- as.formula(paste0(var, " ~ placebo_targeted:Post + factor(Year) | sector_code_f"))
  mod <- feols(f, data = data, vcov = ~ sector_code_f)
  tt <- broom::tidy(mod)
  tt$statistic[tt$term == "placebo_targeted:Post"]
}

# Actual t-statistic, computed from the real (non-permuted) targeted assignment
run_static_tstat_actual <- function(var) {
  f <- as.formula(paste0(var, " ~ targeted:Post + factor(Year) | sector_code_f"))
  mod <- feols(f, data = industry_panel, vcov = ~ sector_code_f)
  tt <- broom::tidy(mod)
  tt$statistic[tt$term == "targeted:Post"]
}

randomization_inference <- lapply(names(outcome_vars), function(var) {

  actual_tstat <- run_static_tstat_actual(var)

  perm_tstats <- vapply(seq_len(n_perms_static), function(p) {
    placebo_treated_codes <- sample(industry_lookup$sector_code_f, n_treated)

    perm_data <- industry_panel %>%
      mutate(placebo_targeted = as.numeric(sector_code_f %in% placebo_treated_codes))

    run_static_tstat(perm_data, var)
  }, numeric(1))

  ri_p_value <- mean(abs(perm_tstats) >= abs(actual_tstat), na.rm = TRUE)

  list(variable = var, actual_tstat = actual_tstat,
       ri_p_value = ri_p_value, perm_dist = perm_tstats)
})
names(randomization_inference) <- names(outcome_vars)

ri_summary <- bind_rows(lapply(randomization_inference, function(x) {
  tibble(Variable    = outcome_vars[[x$variable]],
         Estimate    = static_did_results$estimate[static_did_results$Variable == outcome_vars[[x$variable]]],
         Actual_tstat = round(x$actual_tstat, 3),
         RI_p_value  = round(x$ri_p_value, 3))
}))

print(ri_summary)
write_xlsx(ri_summary, "~/Desktop/Results/Tables/randomization_inference_static_did.xlsx")

# --- Static DD, WITH controls (Table C1) ---

run_static_tstat_controls <- function(data, var) {
  f <- as.formula(paste0(
    var, " ~ placebo_targeted:Post + log_avg_wage:time_trend + log_avg_plant_size:time_trend + ",
    "log_avg_material:time_trend + log_avg_productivity:time_trend | sector_code_f + Year"
  ))
  mod <- feols(f, data = data, vcov = ~ sector_code_f)
  tt <- broom::tidy(mod)
  tt$statistic[tt$term == "placebo_targeted:Post"]
}

run_static_tstat_controls_actual <- function(var) {
  f <- as.formula(paste0(
    var, " ~ targeted:Post + log_avg_wage:time_trend + log_avg_plant_size:time_trend + ",
    "log_avg_material:time_trend + log_avg_productivity:time_trend | sector_code_f + Year"
  ))
  mod <- feols(f, data = industry_panel, vcov = ~ sector_code_f)
  tt <- broom::tidy(mod)
  tt$statistic[tt$term == "targeted:Post"]
}

randomization_inference_static_controls <- lapply(names(outcome_vars), function(var) {

  actual_tstat <- run_static_tstat_controls_actual(var)

  perm_tstats <- vapply(seq_len(n_perms_static), function(p) {
    placebo_treated_codes <- sample(industry_lookup$sector_code_f, n_treated)
    perm_data <- industry_panel %>%
      mutate(placebo_targeted = as.numeric(sector_code_f %in% placebo_treated_codes))
    run_static_tstat_controls(perm_data, var)
  }, numeric(1))

  ri_p_value <- mean(abs(perm_tstats) >= abs(actual_tstat), na.rm = TRUE)

  list(variable = var, actual_tstat = actual_tstat,
       ri_p_value = ri_p_value, perm_dist = perm_tstats)
})
names(randomization_inference_static_controls) <- names(outcome_vars)

ri_summary_static_controls <- bind_rows(lapply(randomization_inference_static_controls, function(x) {
  tibble(Variable     = outcome_vars[[x$variable]],
         Estimate     = static_did_controls$estimate[static_did_controls$Variable == outcome_vars[[x$variable]]],
         Actual_tstat = round(x$actual_tstat, 3),
         RI_p_value   = round(x$ri_p_value, 3))
}))

print(ri_summary_static_controls)
write_xlsx(ri_summary_static_controls, "~/Desktop/Results/Tables/randomization_inference_static_did_controls.xlsx")

# --- Dynamic DD (event study), without and with controls (Figure 1 / Figure C1) ---
# Re-estimates the full set of event-time coefficients (Year x placebo_targeted,
# relative to 1953) under each permutation, extracts their t-statistics, and
# compares the actual t-statistic for each year against its own
# permutation-based null distribution.

run_dynamic_tstats <- function(data, var, controls = FALSE) {
  rhs <- "i(Year, placebo_targeted, 1953)"
  if (controls) {
    rhs <- paste0(rhs,
                  " + log_avg_wage:time_trend + log_avg_plant_size:time_trend + ",
                  "log_avg_material:time_trend + log_avg_productivity:time_trend")
  }
  f <- as.formula(paste0(var, " ~ ", rhs, " | sector_code_f + Year"))
  mod <- feols(f, data = data, vcov = ~ sector_code_f)

  tt <- broom::tidy(mod)
  event_rows <- tt[grepl("^Year::.*:placebo_targeted$", tt$term), ]
  yrs <- as.numeric(gsub("Year::(\\d+):placebo_targeted", "\\1", event_rows$term))
  setNames(event_rows$statistic, yrs)
}

run_dynamic_ri <- function(controls) {

  actual_results <- if (controls) dyn_results_controls else dyn_results_nocontrols

  lapply(names(outcome_vars), function(var) {

    # Actual model + its t-statistics, using the TRUE targeted assignment
    actual_mod <- actual_results[[var]]$mod
    actual_tt  <- broom::tidy(actual_mod)
    actual_event_rows <- actual_tt[grepl("^Year::.*:targeted$", actual_tt$term), ]
    actual_terms <- tibble(
      Year    = as.numeric(gsub("Year::(\\d+):targeted", "\\1", actual_event_rows$term)),
      tstat   = actual_event_rows$statistic,
      estimate = actual_event_rows$estimate
    ) %>% arrange(Year)

    # Matrix of permutation draws: rows = permutations, columns = event years
    perm_mat <- t(vapply(seq_len(n_perms_dynamic), function(p) {
      placebo_treated_codes <- sample(industry_lookup$sector_code_f, n_treated)
      perm_data <- industry_panel %>%
        mutate(placebo_targeted = as.numeric(sector_code_f %in% placebo_treated_codes))

      tstats <- run_dynamic_tstats(perm_data, var, controls = controls)
      # Align to the actual model's event years (in case a permutation drops a level)
      unname(tstats[as.character(actual_terms$Year)])
    }, numeric(nrow(actual_terms))))

    colnames(perm_mat) <- actual_terms$Year

    ri_p_by_year <- vapply(seq_len(nrow(actual_terms)), function(i) {
      yr_draws <- perm_mat[, i]
      mean(abs(yr_draws) >= abs(actual_terms$tstat[i]), na.rm = TRUE)
    }, numeric(1))

    tibble(
      Variable     = outcome_vars[[var]],
      Year         = actual_terms$Year,
      Estimate     = round(actual_terms$estimate, 4),
      Actual_tstat = round(actual_terms$tstat, 3),
      RI_p_value   = round(ri_p_by_year, 3)
    )
  }) %>% bind_rows()
}

ri_dynamic_nocontrols <- run_dynamic_ri(controls = FALSE)
ri_dynamic_controls   <- run_dynamic_ri(controls = TRUE)

print(ri_dynamic_nocontrols)
print(ri_dynamic_controls)

write_xlsx(ri_dynamic_nocontrols, "~/Desktop/Results/Tables/randomization_inference_dynamic_did_nocontrols.xlsx")
write_xlsx(ri_dynamic_controls,   "~/Desktop/Results/Tables/randomization_inference_dynamic_did_controls.xlsx")

#Non-Harmonized Pooled Dataset
#Setting up the data
industry_repeated_cross_section <- nonharmonized_data %>%
  mutate(GO.1954 = NA,
         GO.1955 = NA,
         GO.1956 = NA,
         GVA.1954 = NA,
         GVA.1955 = NA,
         GVA.1956 = NA,
         Material_Input.1954 = NA,
         Material_Input.1955 = NA,
         Material_Input.1956 = NA,
         Number_of_Employees.1954 = NA,
         Number_of_Employees.1955 = NA,
         Number_of_Employees.1956 = NA,
         Number_of_Workers.1954 = NA,
         Number_of_Workers.1955 = NA,
         Number_of_Workers.1956 = NA,
         Factory_Universe.1954 = NA,
         Factory_Universe.1955 = NA,
         Factory_Universe.1956 = NA)

industry_repeated_cross_section <- industry_repeated_cross_section %>%
  pivot_longer(
    cols = contains("."), # Selects all columns with a "."
    names_to = c(".value", "Year"),
    names_sep = "\\."
  ) %>%
  # Convert Year to numeric before sorting
  mutate(Year = as.numeric(Year)) %>%

  # Sort the data frame by Sector and then by Year
  arrange(`Sector Code`, Year) %>%
  relocate(Year, .after = `Industrial Sector`)

#Taking natural logs of values
#Same restriction as the harmonized panel above: only the 4 outcomes with
#complete 1959 coverage are logged here.
industry_repeated_cross_section <- industry_repeated_cross_section %>%
  rename(Number_of_Factories = Factory_Universe) %>%
  mutate(ln_GO = log(GO),
         ln_GVA = log(GVA),
         ln_employees = log(Number_of_Employees),
         ln_factories = log(Number_of_Factories))

#Adding dummy for treated sectors
targeted_sectors <- c("12","21","22","23","25","27","28","29","32","37","62","311","334","341","342","350","360","370")
industry_repeated_cross_section <- industry_repeated_cross_section %>%
  mutate(targeted = as.numeric(`Sector Code` %in% targeted_sectors)) %>%
  relocate(targeted, .after = Year)
#Adding dummy for Post-Treatment period
Post <- c(1957:1965)
industry_repeated_cross_section <- industry_repeated_cross_section %>%
  mutate(Post = as.numeric(Year %in% Post)) %>%
  relocate(Post, .after = targeted)

#Event Study Plots
#Equation 2 estimated without controls
event_study_plots_nh_noref <- lapply(names(outcome_vars), function(var) {

  f <- as.formula(paste0(var, " ~ i(Year, targeted) | Year"))
  mod <- feols(f, data = industry_repeated_cross_section, vcov = ~ `Sector Code`)

  p <- ggiplot(mod, geom = 'errorbar', ref.line = FALSE) +
    theme_gray(base_size = 16) +
    scale_x_continuous(breaks = seq(1951, 1965, 1)) +
    theme(
      axis.text.x = element_text(colour = "black", size = 13, angle = 45, hjust = 1),
      axis.text.y = element_text(colour = "black", size = 14),
      axis.title.y = element_text(size = 15)
    ) +
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
    theme_gray(base_size = 16) +
    theme(
      axis.text.x = element_text(colour = "black", size = 13, angle = 45, hjust = 1),
      axis.text.y = element_text(colour = "black", size = 14),
      axis.title.y = element_text(size = 15)
    ) +
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
    geom_density(color = "#D55E00", linewidth = 1) +
    geom_vline(xintercept = res$actual_diff, color = "black", linetype = "dashed", linewidth = 1.2) +
    annotate("text", x = res$actual_diff, y = 0,
             label = paste0("Actual Diff\n(", round(res$actual_diff, 3), ")\np = ", round(res$p_value, 3)),
             hjust = -0.1, vjust = -0.5, color = "black", fontface = "bold", size = 5) +
    labs(x = "Coefficient Difference", y = "Density") +
    theme_gray(base_size = 16) +
    theme(
      axis.text.x = element_text(colour = "black", size = 13),
      axis.text.y = element_text(colour = "black", size = 14),
      axis.title = element_text(size = 15)
    )

  ggsave(paste0("~/Desktop/Results/Figures/Placebo_Tests/placebo_", var, ".png"),
         p, width = 7, height = 5, dpi = 300)

  cat(var, "- actual diff:", round(res$actual_diff, 3), "| p-value:", round(res$p_value, 3), "\n")

  res
})
names(placebo_results) <- vars_with_1959

## --- Export analysis-ready panels for the replication package ---
## (final, fully-constructed versions of both panels -- convenience files
## for anyone who wants to rerun the regressions without redoing the full
## data-construction pipeline above)
write_csv(industry_panel, "~/Desktop/Results/harmonized_panel_analysis_sample.csv")
write_csv(industry_repeated_cross_section, "~/Desktop/Results/nonharmonized_pooled_analysis_sample.csv")

# ============================================================
# Summary Statistics
# ============================================================
# Restricted to the same 4 outcomes used throughout the results section
# (Gross Output, Gross Value Added, Number of Employees, Number of
# Factories)

# --- Step 1: Attach deflators and construct real (deflated) monetary variables ---
# GO, GVA -> deflated by "All Commodities"

deflate_for_summary <- function(data) {
  data %>%
    left_join(
      ind_prices_reindexed %>%
        select(Year, `All Commodities`),
      by = "Year"
    ) %>%
    mutate(
      GO_real  = GO * 100 / `All Commodities`,
      GVA_real = GVA * 100 / `All Commodities`
    )
}

industry_panel_summary                  <- deflate_for_summary(industry_panel)
industry_repeated_cross_section_summary <- deflate_for_summary(industry_repeated_cross_section)

# --- Step 2: Variable list (deflated monetary vars, nominal counts) ---

summary_vars <- c(
  GO_real              = "Gross Output",
  GVA_real             = "Gross Value Added",
  Number_of_Employees  = "Number of Employees",
  Number_of_Factories  = "Number of Factories"
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
