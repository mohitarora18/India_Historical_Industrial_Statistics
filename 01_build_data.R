## ======================================================================
## 01_build_data.R
##
## Data construction only. Reads the raw SSMI/ASI workbook, harmonizes
## SSMI- and ASI-era industry codes, builds the three analysis-ready
## panels used throughout the paper, and writes them out as CSVs so that
## 02_run_regressions.R can load them
## directly without rerunning this construction step.
##
## Update the file paths below (and the write_csv() paths at the very
## end) to match your own directory structure before running.
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
# This new logic correctly handles both many-to-one and one-to-many relationships
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
  
  # 5. Calculate the mean. Using mean() is equivalent to (1/3)*sum(...) 
  # and automatically handles cases where a year might be missing for a sector.
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
         ln_mat_input = log(Material_Input),
         ln_employees = log(Number_of_Employees),
         ln_factories = log(Number_of_Factories),
         ln_workers = log(Number_of_Workers),
         ln_fixed_capital = log(Fixed_Capital),
         ln_working_capital = log(Working_Capital),
         working_capital_share = Working_Capital / (Fixed_Capital + Working_Capital),
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

#Regression Specification 1 (Average Impacts)
#Without Controls
industry_panel <- industry_panel %>%
  mutate(sector_code_f = as.factor(sector_code))

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


## ======================================================================
## Non-harmonized pooled data: assemble industry_repeated_cross_section
## (moved here from later in the original single-file script, since it
## is pure data construction with no dependency on any regression run
## between this point and where it originally sat)
## ======================================================================
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
  # Best practice: convert Year to numeric before sorting
  mutate(Year = as.numeric(Year)) %>%
  
  # Sort the data frame by Sector and then by Year
  arrange(`Sector Code`, Year) %>%
  relocate(Year, .after = `Industrial Sector`)

#Taking natural logs of values
industry_repeated_cross_section <- industry_repeated_cross_section %>%
  rename(Number_of_Factories = Factory_Universe) %>%
  mutate(ln_GO = log(GO),
         ln_GVA = log(GVA),
         ln_mat_input = log(Material_Input),
         ln_employees = log(Number_of_Employees),
         ln_factories = log(Number_of_Factories),
         ln_workers = log(Number_of_Workers),
         ln_fixed_capital = log(Fixed_Capital),
         ln_working_capital = log(Working_Capital),
         working_capital_share = Working_Capital / (Fixed_Capital + Working_Capital))

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

## ======================================================================
## First-difference variables, added to both panels here so the exported
## CSVs are the final, fully-constructed analysis-ready panels
## ======================================================================
#### First Difference Regressions ####

## --- Harmonized panel: first differences ---
industry_panel <- industry_panel %>% 
  group_by(sector_code) %>% 
  mutate(godiff = log(GO) - log(lag(GO)),
         gvadiff = log(GVA) - log(lag(GVA)),
         factorydiff = log(Number_of_Factories) - log(lag(Number_of_Factories)),
         workerdiff = log(Number_of_Workers) - log(lag(Number_of_Workers)),
         matinputdiff = log(Material_Input) - log(lag(Material_Input)),
         employeediff = log(Number_of_Employees) - log(lag(Number_of_Employees)),
         fixedcapitaldiff = log(Fixed_Capital) - log(lag(Fixed_Capital)),
         workingcapitaldiff = log(Working_Capital) - log(lag(Working_Capital))
  ) %>% 
  ungroup() %>%
  mutate(sector_code_f = as.factor(sector_code))

industry_panel_drop_1959 <- industry_panel %>% filter(Year != 1959)

## --- Non-harmonized pooled data: first differences (for Column 1 = Equation 4) ---
industry_repeated_cross_section <- industry_repeated_cross_section %>%
  group_by(`Sector Code`) %>%
  mutate(godiff = ln_GO - lag(ln_GO),
         gvadiff = ln_GVA - lag(ln_GVA),
         factorydiff = ln_factories - lag(ln_factories),
         workerdiff = ln_workers - lag(ln_workers),
         matinputdiff = ln_mat_input - lag(ln_mat_input),
         employeediff = ln_employees - lag(ln_employees),
         fixedcapitaldiff = ln_fixed_capital - lag(ln_fixed_capital),
         workingcapitaldiff = ln_working_capital - lag(ln_working_capital)
  ) %>%
  ungroup()

## ======================================================================
## Export analysis-ready panels for the replication package.
## These three files are everything 02_run_regressions.R needs -- it
## does not re-read SSMI_ASI.xlsx for any of the panel-construction
## sheets (it does still read two small reference sheets directly:
## Industrial_Prices and ASI_Input_Output -- see the note at the top of
## that script).
## ======================================================================
write_csv(industry_panel, "~/Desktop/Results/harmonized_panel_analysis_sample.csv")
write_csv(industry_repeated_cross_section, "~/Desktop/Results/nonharmonized_pooled_analysis_sample.csv")
write_csv(asi_factory, "~/Desktop/Results/asi_factory_analysis_sample.csv")
