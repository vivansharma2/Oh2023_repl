# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
#
# PROJECT:		 Replication of "Does Identity Affect Labor Supply?" (Oh, 2023)
# REPLICATORS: Vivan Sharma, Rodrigo Pereira
# TASK:				 data cleaning (bonus experiment data)
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

df <- read_dta(paste0(rawdata, "choice_bonuswage.dta"))

# # # # # # 
# PART 1 
# # # # # #  

# Clean as in part one
df$survey_completed <- df$pid != 179

df <- df %>%
  mutate(age = ifelse(!is.na(b1_age), b1_age, NA),
         married = ifelse(!is.na(b2_marital_status), b2_marital_status == 1, NA),
         famsize = ifelse(b3_hh_all_children >= 0 & b3_hh_adults >= 0 & !is.na(b3_hh_all_children) & !is.na(b3_hh_adults), 
                          b3_hh_adults + b3_hh_all_children, NA),
         
         workmember = ifelse(!is.na(b4_act_engage_paid_work) & b4_act_engage_paid_work <= famsize & b4_act_engage_paid_work >= 0, 
                             b4_act_engage_paid_work, NA),
         workshare = ifelse(!is.na(famsize) & famsize > 0, workmember / famsize, NA), 
         
         year_edu = ifelse(b5_high_level_edu_class >= 0 & b5_high_level_edu_class <= 12, b5_high_level_edu_class, NA),
         year_edu = ifelse(b5_high_level_edu == 0, 0, year_edu),
         year_edu = ifelse(b5_high_level_edu == 1 & b5_high_level_edu_class < 0, 0, year_edu),
         year_edu = ifelse(b5_high_level_edu %in% c(2, 3, 4), 12, year_edu),
         
         read_odiya = ifelse(b6_read_odiya_paper %in% c(0, 1), b6_read_odiya_paper, NA),
         
         pucca_house = ifelse(!is.na(b7_dwell_type), b7_dwell_type == 1, NA),
         semipucca_house = ifelse(!is.na(b7_dwell_type), b7_dwell_type == 2, NA),
         kutcha_house = ifelse(!is.na(b7_dwell_type), b7_dwell_type == 3, NA),
         
         gunta = ifelse(b8_own_land_gunta >= 0, b8_own_land_gunta, 0),
         acre = ifelse(b8_own_land_acre >= 0, b8_own_land_acre, 0),
         
         landsize = ifelse(!is.na(b8_own_land), gunta / 40 + acre, NA),
         
         own_land = ifelse(!is.na(b8_own_land), b8_own_land %in% c(1, 2), NA),
         own_land = ifelse(!is.na(landsize) & landsize == 0, 0, own_land),
         
         income = ifelse(!is.na(b9_hh_tot_income_last_mm) & b9_hh_tot_income_last_mm >= 0, 
                         b9_hh_tot_income_last_mm, NA),
         
         log_income = ifelse(!is.na(income), log(income + sqrt(income^2 + 1)), NA),  # Inverse hyperbolic sine transformation
         
         paid_days = b12_get_paid_7days)

# List of variables to process
v1 <- c("b13a_sew_mach_own", "b13b_bicycle_own", "b13c_motorbike_own", 
        "b13d_fridge_own", "b13e_radio_own", "b13f_tv_own", 
        "b13g_mobile_own", "b13h_land_phone_own", "b13i_stove_own", 
        "b13j_watches_own")

# Clean names and values of above
for (x in v1) {
  varname <- sub("^b13[a-j]_", "", x)  
  df[[varname]] <- ifelse(!is.na(df[[x]]), df[[x]] > 0, NA)
}

df$own_index <- rowSums(df[, c("sew_mach_own", "bicycle_own", "motorbike_own", 
                               "fridge_own", "radio_own", "tv_own", 
                               "mobile_own", "land_phone_own", "stove_own", "watches_own")], 
                        na.rm = FALSE)  # Keeps NA if all values in a row are NA

# PCA loadings
dfn <- read_dta(paste0(cleandata, "choice_jobtakeup_analysis_replicated.dta"))
dfn <- dfn %>% filter(tag_pid == 1)

# Perform PCA (standardizing the data)
v <- c("workshare", "log_income", "kutcha_house", "semipucca_house", 
        "own_land", "landsize", "paid_days", 
        "sew_mach_own", "bicycle_own", "motorbike_own", "fridge_own", 
        "radio_own", "tv_own", "mobile_own", "land_phone_own", 
        "stove_own", "watches_own")

# Identify rows with complete cases
complete_rows <- complete.cases(dfn[, v])
# Run PCA on non-missing observations
pca_result <- prcomp(dfn[complete_rows, v], center = TRUE, scale. = TRUE)
# Extract the first principal component scores
pca_scores <- -pca_result$rotation[, 1]  

# Compute means and standard deviations
means <- colMeans(dfn[, v], na.rm = TRUE)
sds <- apply(dfn[, v], 2, sd, na.rm = TRUE)

# Compute median of the first principal component scores
pca_median <- median(dfn$wealth_pca, na.rm = TRUE)

# Use weights
for (i in seq_along(v)) {
  x <- v[i]  
  df[[paste0("temp_", x)]] <- ((df[[x]] - means[i]) / sds[i]) * pca_scores[i]
}

df$wealth_pca <- NA  # Initialize with NA
df$wealth_pca[df$survey_completed == 1] <-  rowSums(df[df$survey_completed == 1, grep("^temp_", names(df))], na.rm = TRUE)

# Generate hiedu, old, hijobs
df <- df[, !grepl("^temp_", names(df))]
df$hiwealth <- df$wealth_pca > pca_median

edu_median <- median(df$year_edu, na.rm = TRUE)
df$hiedu <- ifelse(!is.na(df$year_edu), df$year_edu > edu_median, NA)

age_median <- median(df$age, na.rm = TRUE)
df$old <- ifelse(!is.na(df$age), df$age > age_median, NA)

pd_median <- median(df$paid_days, na.rm = TRUE)
df$hijobs <- ifelse(!is.na(df$paid_days), df$paid_days > pd_median, NA)

# Binaries for lower values
df <- df %>%
  mutate(across(c(d1_karthik_tuna, d2_bindusagar_rabi, d3_gagan_find_work), 
                ~ ifelse(!is.na(.), ifelse(. %in% c(1, 2), 1, 0), NA),
                .names = "{.col}_cons"))

# Binaries for higher values
df <- df %>%
  mutate(across(c(d4_santhilatha_college, d5_nehru_finish_ssc, d6_sameer_jena, d7_tukuna_naika), 
                ~ ifelse(!is.na(.), ifelse(. %in% c(4, 5), 1, 0), NA),
                .names = "{.col}_cons"))

df <- df %>%
  mutate(conserv_index = rowSums(select(df, d1_karthik_tuna_cons, d2_bindusagar_rabi_cons, 
                                        d3_gagan_find_work_cons, d4_santhilatha_college_cons, 
                                        d5_nehru_finish_ssc_cons, d6_sameer_jena_cons, 
                                        d7_tukuna_naika_cons), na.rm = FALSE))

df <- df %>%
  mutate(conserv5up = ifelse(conserv_index >= 5, TRUE, FALSE))

# Labeling variables
var_label(df$age) <- "Age"
var_label(df$married) <- "Married"
var_label(df$famsize) <- "Family size"
var_label(df$workshare) <- "Share of working members"
var_label(df$year_edu) <- "Years of education"
var_label(df$read_odiya) <- "Able to read"
var_label(df$pucca_house) <- "Non-mud house"
var_label(df$semipucca_house) <- "Semi-mud house"
var_label(df$kutcha_house) <- "Mud house"
var_label(df$own_land) <- "Owns land"
var_label(df$landsize) <- "Land size in acres"
var_label(df$income) <- "Last month income in Rs."
var_label(df$log_income) <- "Log of last month income"
var_label(df$paid_days) <- "Paid work days last week"
var_label(df$own_index) <- "Number of assets owned"
var_label(df$wealth_pca) <- "Wealth PCA score"
var_label(df$conserv_index) <- "Number of caste-sensitive views"
var_label(df$hiwealth) <- "High wealth"
var_label(df$hiedu) <- "High education"
var_label(df$old) <- "Older"
var_label(df$conserv5up) <- "Caste-sensitive (5+)"

# List of base variables
selected_vars <- c("i_a_sweep_latrine", "i_b_sweep_latrine", "i_c_sweep_latrine",
                   "ii_a_wash_agtool", "ii_b_wash_agtool", "ii_c_wash_agtool",
                   "iii_a_repair_grassmat", "iii_b_repair_grassmat", "iii_c_repair_grassmat",
                   "iv_a_construction", "iv_b_construction", "iv_c_construction",
                   "v_a_wash_cloth", "v_b_wash_cloth", "v_c_wash_cloth",
                   "vi_a_repair_shoes", "vi_b_repair_shoes", "vi_c_repair_shoes",
                   "vii_a_shed_sweep", "vii_b_shed_sweep", "vii_c_shed_sweep")

# Select only those of interest (others have numeric suffixes)
long_vars <- names(df)[names(df) %in% unlist(lapply(selected_vars, function(var) grep(paste0("^", var, "\\d+$"), names(df), value = TRUE)))]

# Reshape
dfl <- df %>%
  pivot_longer(
    cols = all_of(long_vars),
    names_to = c(".value", "pay"),  
    names_pattern = "^(.*?)(\\d+)$"  
  ) %>%
  mutate(pay = as.numeric(pay)) 

# Rename accordingly
dfl <- dfl %>% rename(
  task6_1 = i_a_sweep_latrine,
  task6_2 = i_b_sweep_latrine,
  task6_3 = i_c_sweep_latrine,
  task3_1 = ii_a_wash_agtool,
  task3_2 = ii_b_wash_agtool,
  task3_3 = ii_c_wash_agtool,
  task5_1 = iii_a_repair_grassmat,
  task5_2 = iii_b_repair_grassmat,
  task5_3 = iii_c_repair_grassmat,
  task1_1 = iv_a_construction,
  task1_2 = iv_b_construction,
  task1_3 = iv_c_construction,
  task2_1 = v_a_wash_cloth,
  task2_2 = v_b_wash_cloth,
  task2_3 = v_c_wash_cloth,
  task4_1 = vi_a_repair_shoes,
  task4_2 = vi_b_repair_shoes,
  task4_3 = vi_c_repair_shoes,
  task7_1 = vii_a_shed_sweep,
  task7_2 = vii_b_shed_sweep,
  task7_3 = vii_c_shed_sweep
)

dfl <- dfl %>%
  mutate(id_pay = row_number())

# Worker X price X time level reshaping
selected_vars1 <- c("task1_", "task2_", "task3_", "task4_", "task5_", "task6_", "task7_")

# Filter variables that exist in df
long_vars1 <- names(dfl)[names(dfl) %in% unlist(lapply(selected_vars1, function(var)
  grep(paste0("^", var, "\\d+$"), names(dfl), value = TRUE)))]

# Reshape
dfl1 <- dfl %>%
  pivot_longer(
    cols = all_of(long_vars1),
    names_to = c(".value", "timecat"),
    names_pattern = "^(.*?)(\\d+)$"  # Prevent over-capturing of suffix numbers
  ) %>%
  mutate(task = as.integer(timecat))

# Rename
dfl1 <- dfl1 %>% 
  select(-task) %>%
  rename_with(~ gsub("_$", "", .), starts_with("task"))

# Clean and generate time variables
dfl1 <- dfl1 %>%
  mutate(
    timemin = case_when(
      timecat == 1 ~ 10,
      timecat == 2 ~ 30,
      timecat == 3 ~ 60,
      TRUE ~ NA  # Ensure proper handling of other cases
    )
  ) %>%
  mutate(across(starts_with("task"), ~ ifelse(. < 0, NA, .)))

# Label
label <- list(
  task1 = "Moving bricks",
  task2 = "Washing clothes",
  task3 = "Washing farming tools",
  task4 = "Mending leather shoes",
  task5 = "Mending grass mats",
  task6 = "Sweeping latrines",
  task7 = "Sweeping animal sheds"
)

for (var in names(label)) {
  attr(dfl1[[var]], "label") <- label[[var]]
}

# Rename
dfl1 <- dfl1 %>%
  rename(
    extent1_8 = c1_make_paper_bag,
    extent_oth8 = c1_make_paper_bag_oth,
    extent1_2 = c3_wash_cloths1,
    extent2_2 = c3_wash_cloths2,
    extent_oth2 = c3_wash_cloths_oth,
    extent1_4 = c4_repair_old_lthr_shoes,
    extent_oth4 = c4_repair_old_lthr_shoes_oth,
    extent1_6 = c5_sweep_latrine1,
    extent2_6 = c5_sweep_latrine2,
    extent_oth6 = c5_sweep_latrine_oth,
    extent1_1 = c6_hvy_lift_construction1,
    extent2_1 = c6_hvy_lift_construction2,
    extent3_1 = c6_hvy_lift_construction3,
    extent4_1 = c6_hvy_lift_construction4,
    extent5_1 = c6_hvy_lift_construction5,
    extent_oth1 = c6_hvy_lift_construction_oth,
    extent1_3 = c7_wash_agri_tools1,
    extent2_3 = c7_wash_agri_tools2,
    extent3_3 = c7_wash_agri_tools3,
    extent_oth3 = c7_wash_agri_tools_oth,
    extent1_5 = c8_repair_grassmat1,
    extent2_5 = c8_repair_grassmat2,
    extent_oth5 = c8_repair_grassmat_oth,
    extent1_7 = c9_sweep_animal_shed1,
    extent2_7 = c9_sweep_animal_shed2,
    extent3_7 = c9_sweep_animal_shed3,
    extent_oth7 = c9_sweep_animal_shed_oth,
    
    refuse_all2 = c10_wash_cloths,
    reason_main2 = c10a_all_wage_wash_cloth,
    reason_main_oth2 = c10a_all_wage_wash_cloth_oth,
    reason1_2 = c10b_all_wage_oth_wash_cloth1,
    reason2_2 = c10b_all_wage_oth_wash_cloth2,
    reason3_2 = c10b_all_wage_oth_wash_cloth3,
    reason_oth2 = c10b_all_wage_oth_wash_cloth_o,
    extrawage2 = c10c_extra_wage_wash_cloth,
    extrawage_rs2 = c10c_extra_wage_wash_cloth_rs,
    secretagree2 = c10d_some_wage_wash_cloth,
    secret_nowhy2 = c10d_some_wage_wash_cloth_no,
    secret_yeswhy2 = c10d_some_wage_wash_cloth_yes,
    social1_2 = c10e_fin_soc_wash_cloth1,
    social2_2 = c10e_fin_soc_wash_cloth2,
    social3_2 = c10e_fin_soc_wash_cloth3,
    social_oth2 = c10e_fin_soc_wash_cloth_oth,
    
    refuse_all4 = c11_repair_shoes,
    reason_main4 = c11a_all_wage_repr_shoes,
    reason_main_oth4 = c11a_all_wage_repr_shoes_oth,
    reason1_4 = c11b_all_wage_oth_repr_shoes1,
    reason2_4 = c11b_all_wage_oth_repr_shoes2,
    reason3_4 = c11b_all_wage_oth_repr_shoes3,
    reason_oth4 = c11b_all_wage_oth_repr_shoes_o,
    extrawage4 = c11c_extra_wage_repr_shoes,
    extrawage_rs4 = c11c_extra_wage_repr_shoes_rs,
    secretagree4 = c11d_some_wage_repr_shoes,
    secret_nowhy4 = c11d_some_wage_repr_shoes_no,
    secret_yeswhy4 = c11d_some_wage_repr_shoes_yes,
    social1_4 = c11e_fin_soc_repr_shoes1,
    social2_4 = c11e_fin_soc_repr_shoes2,
    social3_4 = c11e_fin_soc_repr_shoes3,
    social_oth4 = c11e_fin_soc_repr_shoes_oth,
    
    refuse_all6 = c12_sweep_latrine,
    reason_main6 = c12a_all_wage_sweep_latr,
    reason_main_oth6 = c12a_all_wage_sweep_latr_oth,
    reason1_6 = c12b_all_wage_oth_sweep_latr1,
    reason2_6 = c12b_all_wage_oth_sweep_latr2,
    reason3_6 = c12b_all_wage_oth_sweep_latr3,
    reason4_6 = c12b_all_wage_oth_sweep_latr4,
    reason_oth6 = c12b_all_wage_oth_sweep_latr_o,
    extrawage6 = c12c_extra_wage_sweep_latr,
    extrawage_rs6 = c12c_extra_wage_sweep_latr_rs,
    secretagree6 = c12d_some_wage_sweep_latr,
    secret_nowhy6 = c12d_some_wage_sweep_latr_no,
    secret_yeswhy6 = c12d_some_wage_sweep_latr_yes,
    social1_6 = c12e_fin_soc_sweep_latr1,
    social2_6 = c12e_fin_soc_sweep_latr2,
    social3_6 = c12e_fin_soc_sweep_latr3,
    social_oth6 = c12e_fin_soc_sweep_latr_oth,
    
    refuse_all1 = c13_hvy_lift_constru,
    reason_main1 = c13a_all_wage_hvy_lift,
    reason_main_oth1 = c13a_all_wage_hvy_lift_oth,
    reason1_1 = c13b_all_wage_oth_hvy_lift,
    reason_oth1 = c13b_all_wage_oth_hvy_lift_o,
    extrawage1 = c13c_extra_wage_hvy_lift,
    extrawage_rs1 = c13c_extra_wage_hvy_lift_rs,
    
    refuse_all3 = c14_wash_agri_tools,
    reason_main3 = c14a_all_wage_wash_agrtool,
    reason_main_oth3 = c14a_all_wage_wash_agrtool_oth,
    reason1_3 = c14b_all_wage_oth_wash_agrtool,
    reason_oth3 = c14b_all_wage_oth_wash_agrtool_o,
    extrawage3 = c14c_extra_wage_wash_agrtool,
    extrawage_rs3 = c14c_extra_wage_wash_agrtool_rs,
    
    refuse_all5 = c15_repair_grassmat,
    reason_main5 = c15a_all_wage_rpr_grsmat,
    reason_main_oth5 = c15a_all_wage_rpr_grsmat_oth,
    reason1_5 = c15b_all_wage_oth_rpr_grsmat1,
    reason2_5 = c15b_all_wage_oth_rpr_grsmat2,
    reason_oth5 = c15b_all_wage_oth_rpr_grsmat_o,
    extrawage5 = c15c_extra_wage_rpr_grsmat,
    extrawage_rs5 = c15c_extra_wage_rpr_grsmat_rs,
    
    refuse_all7 = c16_sweep_animal_shed,
    reason_main7 = c16a_all_wage_shed_swep,
    reason_main_oth7 = c16a_all_wage_shed_swep_oth,
    reason1_7 = c16b_all_wage_oth_shed_swep1,
    reason2_7 = c16b_all_wage_oth_shed_swep2,
    reason_oth7 = c16b_all_wage_oth_shed_swep_o,
    extrawage7 = c16c_extra_wage_shed_swep,
    extrawage_rs7 = c16c_extra_wage_shed_swep_rs,
    
    demandhigh2 = c17_wash_cloths,
    dhreason_main2 = c17a_hgh_wage_wash_cloth,
    dhreason_main_oth2 = c17a_hgh_wage_wash_cloth_oth,
    dhreason1_2 = c17b_hgh_wage_oth_wash_cloth1,
    dhreason2_2 = c17b_hgh_wage_oth_wash_cloth2,
    dhreason_oth2 = c17b_hgh_wage_oth_wash_cloth_o,
    dhextrawage2 = c17c_extra_wage_wash_cloth,
    dhextrawage_rs2 = c17c_extra_wage_wash_cloth_rs,
    dhsecretagree2 = c17d_low_wage_wash_cloth,
    dhsecret_nowhy2 = c17d_low_wage_wash_cloth_no,
    dhsecret_yeswhy2 = c17d_low_wage_wash_cloth_yes,
    
    demandhigh4 = c18_repair_shoes,
    dhreason_main4 = c18a_hgh_wage_rpr_shoe,
    dhreason_main_oth4 = c18a_hgh_wage_rpr_shoe_oth,
    dhreason1_4 = c18b_hgh_wage_oth_rpr_shoe1,
    dhreason2_4 = c18b_hgh_wage_oth_rpr_shoe2,
    dhreason_oth4 = c18b_hgh_wage_oth_rpr_shoe_o,
    dhextrawage4 = c18c_extra_wage_rpr_shoe,
    dhextrawage_rs4 = c18c_extra_wage_rpr_shoe_rs,
    dhsecretagree4 = c18d_low_wage_rpr_shoe,
    dhsecret_nowhy4 = c18d_low_wage_rpr_shoe_no,
    dhsecret_yeswhy4 = c18d_low_wage_rpr_shoe_yes,
    
    demandhigh6 = c19_sweep_latrine,
    dhreason_main6 = c19a_hgh_wage_sweep_latr,
    dhreason_main_oth6 = c19a_hgh_wage_sweep_latr_oth,
    dhreason1_6 = c19b_hgh_wage_oth_sweep_latr1,
    dhreason2_6 = c19b_hgh_wage_oth_sweep_latr2,
    dhreason_oth6 = c19b_hgh_wage_oth_sweep_latr_o,
    dhextrawage6 = c19c_extra_wage_sweep_latr,
    dhextrawage_rs6 = c19c_extra_wage_sweep_latr_rs,
    dhsecretagree6 = c19d_low_wage_sweep_latr,
    dhsecret_nowhy6 = c19d_low_wage_sweep_latr_no,
    dhsecret_yeswhy6 = c19d_low_wage_sweep_latr_yes,
    
    demandhigh1 = c20_hvy_lift,
    dhreason_main1 = c20a_hgh_wage_hvy_lift,
    dhreason_main_oth1 = c20a_hgh_wage_hvy_lift_oth,
    dhreason1_1 = c20b_hgh_wage_oth_hvy_lift,
    dhreason_oth1 = c20b_hgh_wage_oth_hvy_lift_o,
    dhextrawage1 = c20c_extra_wage_hvy_lift,
    dhextrawage_rs1 = c20c_extra_wage_hvy_lift_rs,
    
    demandhigh3 = c21_wash_agri_tool,
    dhreason_main3 = c21a_hgh_wage_wash_agrtool,
    dhreason_main_oth3 = c21a_hgh_wage_wash_agrtool_oth,
    dhreason1_3 = c21b_hgh_wage_oth_wash_agrtool,
    dhreason_oth3 = c21b_hgh_wage_oth_wash_agrtool_o,
    dhextrawage3 = c21c_extra_wage_wash_agrtool,
    dhextrawage_rs3 = c21c_extra_wage_wash_agrtool_rs,
    
    demandhigh5 = c22_repair_grassmat,
    dhreason_main5 = c22a_hgh_wage_rpr_grsmat,
    dhreason_main_oth5 = c22a_hgh_wage_rpr_grsmat_oth,
    dhreason1_5 = c22b_hgh_wage_oth_rpr_grsmat1,
    dhreason2_5 = c22b_hgh_wage_oth_rpr_grsmat2,
    dhreason_oth5 = c22b_hgh_wage_oth_rpr_grsmat_o,
    dhextrawage5 = c22c_extra_wage_rpr_grsmat,
    dhextrawage_rs5 = c22c_extra_wage_rpr_grsmat_rs,
    
    demandhigh7 = c23_sweep_animal_shed,
    dhreason_main7 = c23a_hgh_wage_shed_swep,
    dhreason_main_oth7 = c23a_hgh_wage_shed_swep_oth,
    dhreason1_7 = c23b_hgh_wage_oth_shed_swep1,
    dhreason2_7 = c23b_hgh_wage_oth_shed_swep2,
    dhreason_oth7 = c23b_hgh_wage_oth_shed_swep_o,
    dhextrawage7 = c23c_extra_wage_shed_swep,
    dhextrawage_rs7 = c23c_extra_wage_shed_swep_rs
  )


dfl1 <- dfl1 %>%
  mutate(id_pay_time = row_number())

# String vars
varlist <- c("extent_oth8", "extent_oth2", "extent_oth4", "extent_oth6", "extent_oth1", 
             "extent_oth3", "extent_oth5", "extent_oth7", "dhreason_oth2", "dhreason_oth4", 
             "dhreason_oth6", "dhreason_oth1", "dhreason_oth3", "dhreason_oth5", "dhreason_oth7",
             "dhreason_main_oth6", "reason_oth1", "reason_oth2", "reason_oth3", "reason_oth4",
             "reason_oth5", "reason_oth6", "reason_oth7")

# Loop through each variable and convert if necessary
for (var in varlist) {
  if (var %in% names(dfl1)) {  # Ensure the variable exists in the dataset
    if (!is.character(dfl1[[var]])) {  # Check if the variable is not already a string
      dfl1[[var]] <- as.character(dfl1[[var]])  # Convert to string
    }
  }
}

# Reshape again
selected_vars2 <- c(
  "task", "extent1_", "extent2_", "extent3_", "extent4_", "extent5_", "extent_oth",
  "refuse_all", "reason_main", "reason_main_oth", "reason1_", "reason2_", "reason3_", "reason4_", "reason_oth",
  "extrawage", "extrawage_rs", "secretagree", "secret_nowhy", "secret_yeswhy",
  "social1_", "social2_", "social3_", "social_oth",
  "demandhigh", "dhreason_main", "dhreason_main_oth", "dhreason1_", "dhreason2_", "dhreason_oth",
  "dhextrawage", "dhextrawage_rs", "dhsecretagree", "dhsecret_nowhy", "dhsecret_yeswhy"
)

# Filter 
long_vars2 <- names(dfl1)[names(dfl1) %in% unlist(lapply(selected_vars2, function(var)
  grep(paste0("^", var, "\\d+$"), names(dfl1), value = TRUE)))]

# Reshape long
dfl2 <- dfl1 %>%
  pivot_longer(
    cols = all_of(long_vars2),
    names_to = c(".value", "cat"),
    names_pattern = "^(.*?)(\\d+)$"
  ) %>%
  mutate(cat = as.integer(cat)) 

# Rename again
dfl2 <- dfl2 %>%
  rename(
    agree = task,
    task = cat
  ) %>%
  rename_with(~ gsub("extent(\\d+)_", "extent\\1", .x)) %>%  # Remove underscores in 'extent' variables
  rename_with(~ gsub("(\\w*reason)(\\d*)_", "\\1\\2", .x)) %>%  # Remove underscores in 'reason' variables
  rename_with(~ gsub("social(\\d+)_", "social\\1", .x))  # Remove underscores in 'social' variables

task_labels <- c(
  "1" = "Moving bricks",
  "2" = "Washing clothes",
  "3" = "Washing farming tools",
  "4" = "Mending leather shoes",
  "5" = "Mending grass mats",
  "6" = "Sweeping latrines",
  "7" = "Sweeping animal sheds",
  "8" = "Making paper bags"
)

# dfl2 <- dfl2 %>%
  # mutate(task = factor(task, levels = names(task_labels), labels = task_labels))

# # # # # # 
# PART 2
# # # # # #  

# Generate variables for analysis


dfl2 <- dfl2 %>%
  arrange(pid, task, timecat, pay)

dfl2 <- dfl2 %>%
  group_by(pid, task, timecat) %>%
  mutate(tag_pidtasktime = ifelse(row_number() == 1, 1, 0)) %>%
  ungroup()

# Unique identifier for each (pid, task) combination
dfl2 <- dfl2 %>%
  mutate(pidtask = as.numeric(factor(paste(pid, task, sep = "_"))))

# Tag if at least one row has tag_pidtasktime == 1
dfl2 <- dfl2 %>%
  arrange(pid, task, timecat, pay) %>%
  group_by(pid, task) %>%
  mutate(tag_pidtask = ifelse(row_number() == 1 & any(tag_pidtasktime == 1), 1, 0)) %>%
  ungroup()

# Tag if at least one row has tag_pidtask == 1
dfl2 <- dfl2 %>%
  group_by(pid) %>%
  mutate(tag_pid = ifelse(row_number() == 1 & any(tag_pidtask == 1), 1, 0)) %>%
  ungroup()

# Minimum price demanded - defined at pid X task X time level
dfl2 <- dfl2 %>%
  group_by(pid, task, timecat) %>%
  mutate(temp1 = ifelse(all(is.na(agree)), NA, max(agree, na.rm = TRUE))) %>%
  ungroup() %>%
  mutate(nevertake_tasktime = ifelse(task != 8, temp1 == 0, NA)) %>% # Never take up task for this amount of time
  group_by(pid, task) %>%
  mutate(temp2 = ifelse(all(is.na(agree)), NA, max(agree, na.rm = TRUE))) %>%
  ungroup() %>%
  mutate(nevertake_task = ifelse(task != 8, temp2 == 0, NA)) %>% # Never take up task at all
  select(-temp1, -temp2)

dfl2 <- dfl2 %>%
  mutate(temp1 = ifelse(agree == 1, pay, NA)) %>%
  group_by(pid, task, timecat) %>%
  mutate(minwage_tasktime = ifelse(all(is.na(temp1)), NA, min(temp1, na.rm = TRUE))) %>%
  ungroup() %>%
  group_by(pid, task) %>%
  mutate(minwage_task = ifelse(all(is.na(temp1)), NA, min(temp1, na.rm = TRUE))) %>%
  ungroup() %>%
  select(-temp1)

dfl2 <- dfl2 %>%
  mutate(
    minwage_tasktime_imp = minwage_tasktime,
    minwage_task_imp = minwage_task
  ) %>%
  mutate(
    minwage_tasktime_imp = ifelse(is.na(minwage_tasktime_imp) & task != 8, 5000, minwage_tasktime_imp),
    minwage_task_imp = ifelse(is.na(minwage_task_imp) & task != 8, 5000, minwage_task_imp))

dfl2 <- dfl2 %>%
  mutate(
    take30_tasktime = ifelse(!is.na(minwage_tasktime_imp), minwage_tasktime_imp <= 30, NA),
    take3000_tasktime = ifelse(!is.na(minwage_tasktime_imp), minwage_tasktime_imp <= 3000, NA)
  )

dfl2 <- dfl2 %>%
  mutate(
    temp2 = ifelse(timecat == 1, minwage_tasktime_imp, NA),
    temp3 = ifelse(timecat != 1, minwage_tasktime_imp, NA)
  )

dfl2 <- dfl2 %>%
  group_by(pid, task) %>%
  mutate(
    temp4 = ifelse(all(is.na(temp2)), NA, min(temp2, na.rm = TRUE)),
    temp5 = ifelse(all(is.na(temp3)), NA, min(temp3, na.rm = TRUE))
  ) %>%
  ungroup()

dfl2 <- dfl2 %>%
  mutate(
    incons_time = ifelse(!is.na(minwage_tasktime_imp), temp4 > temp5, NA)
  )

dfl2 <- dfl2 %>%
  mutate(
    temp6 = ifelse(agree == 1, pay, NA),
    temp7 = ifelse(agree == 0, pay, NA)
  ) %>%
  group_by(pid, task, timecat) %>%
  mutate(
    temp8 = ifelse(all(is.na(temp6)), NA, min(temp6, na.rm = TRUE)),
    temp9 = ifelse(all(is.na(temp7)), NA, max(temp7, na.rm = TRUE))
  ) %>%
  ungroup() %>%
  mutate(
    incons_amt = case_when(
      is.na(temp8) & is.na(temp9) ~ NA,  # Both missing gives NA
      is.na(temp8) & !is.na(temp9) ~ 0,        # temp8 missing, temp9 present gives 0
      is.na(minwage_tasktime_imp) ~ NA,  # minwage_tasktime_imp missing gives NA
      temp8 < temp9 ~ 1,                       # temp8 < temp9 gives 1
      TRUE ~ 0                                 # Else 0
    ),
    temp10 = ifelse(!is.na(minwage_tasktime_imp), incons_time + incons_amt, NA)
  ) %>%
  group_by(pid) %>%
  mutate(reversal_pid = ifelse(all(is.na(temp10)), NA, max(temp10, na.rm = TRUE))) %>%
  ungroup()  %>%
  select(-starts_with("temp"))

dfl3 <- dfl2 %>%
  filter(tag_pid == 1) %>%
  select(pid, 
         b7_tea_rs1, b7_mustard_seed_rs1, 
         b8_tea_rs2, b8_mustard_seed_rs2, 
         b9_tea_rs3, b9_mustard_seed_rs3, 
         b10_tea_rs4, b10_mustard_seed_rs4, 
         b11_tea_rs5, b11_mustard_seed_rs5, 
         b12_tea_rs6, b12_mustard_seed_rs6, 
         b13_tea_rs7, b13_mustard_seed_rs7, 
         b14_tea_rs8, b14_mustard_seed_rs8, 
         b15_tea_rs9, b15_mustard_seed_rs9, 
         b16_tea_rs10, b16_mustard_seed_rs10)

tcol <- c("b7_tea_rs1", "b8_tea_rs2", "b9_tea_rs3", "b10_tea_rs4",
                 "b11_tea_rs5", "b12_tea_rs6", "b13_tea_rs7", "b14_tea_rs8",
                 "b15_tea_rs9", "b16_tea_rs10")

for (k in seq_along(tcol)) {
  new_name <- paste0("tea", k)  
  dfl3 <- dfl3 %>%
    rename(!!new_name := all_of(tcol[k])) 
}

mcol <- c("b7_mustard_seed_rs1", "b8_mustard_seed_rs2", "b9_mustard_seed_rs3", 
                     "b10_mustard_seed_rs4", "b11_mustard_seed_rs5", "b12_mustard_seed_rs6",
                     "b13_mustard_seed_rs7", "b14_mustard_seed_rs8", "b15_mustard_seed_rs9",
                     "b16_mustard_seed_rs10")

# Rename using a loop
for (k in seq_along(mcol)) {
  new_name <- paste0("mustard", k)  
  dfl3 <- dfl3 %>%
    rename(!!new_name := all_of(mcol[k]))  
}

# Reshape
dfl3 <- dfl3 %>%
  pivot_longer(
    cols = starts_with("tea") | starts_with("mustard"),
    names_to = c(".value", "time"),   
    names_pattern = "(tea|mustard)(\\d+)" 
  ) %>%
  mutate(time = as.integer(time))

dfl3 <- dfl3 %>%
  rename(price = time) %>%  
  mutate(
    temp1 = ifelse(tea == 1, price, NA),
    temp3 = ifelse(tea == 0, price, NA)
  ) %>%
  group_by(pid) %>%
  mutate(
    temp2 = ifelse(all(is.na(temp1)), NA, max(temp1, na.rm = TRUE)), 
    temp4 = ifelse(all(is.na(temp3)), NA, min(temp3, na.rm = TRUE))   
  ) %>%
  ungroup()

dfl3 <- dfl3 %>%
  mutate(
    incons_tea = ifelse(is.na(temp4), 0, ifelse(temp2 > temp4, 1, 0)),  
    temp5 = ifelse(mustard == 1, price, NA),
    temp7 = ifelse(mustard == 0, price, NA)
  ) %>%
  group_by(pid) %>%
  mutate(
    temp6 = ifelse(all(is.na(temp5)), NA, max(temp5, na.rm = TRUE)),  
    temp8 = ifelse(all(is.na(temp7)), NA, min(temp7, na.rm = TRUE))  
  ) %>%
  ungroup()

dfl3 <- dfl3 %>%
  mutate(
    incons_mustard = ifelse(is.na(temp6) | is.na(temp8), 0, ifelse(temp6 > temp8, 1, 0)))

dfl3 <- dfl3 %>%
  group_by(pid) %>%
  mutate(tag_pid = ifelse(row_number() == 1, 1, 0)) %>%  
  ungroup() %>%
  filter(tag_pid == 1) %>%  
  select(pid, incons_tea, incons_mustard) 

# Merge into dfl2
dfl4 <- dfl2 %>%
  left_join(dfl3, by = "pid")

dfl4 <- dfl4 %>%
  mutate(
    kaibarta = caste == 1,
    pana = caste == 6,
    identity = task %in% c(2, 4, 6),
    pairedcont = task %in% c(3, 5, 7),
    purecont = task %in% c(1, 8),
    exptask = identity | pairedcont,
    lowertask = case_when(
      caste == 1 & task %in% c(2, 3, 4, 5, 6, 7) ~ 1,
      caste == 6 & task %in% c(6, 7) ~ 1,
      TRUE ~ 0
    ),
    iden_lowertask = lowertask * identity
  )

dfl4 <- dfl4 %>%
  mutate(
    caste1 = as.integer(caste == 1),
    caste6 = as.integer(caste == 6),
    task1 = as.integer(task == 1),
    task2 = as.integer(task == 2),
    task3 = as.integer(task == 3),
    task4 = as.integer(task == 4),
    task5 = as.integer(task == 5),
    task6 = as.integer(task == 6),
    task7 = as.integer(task == 7)
  )

dfl4 <- dfl4 %>%
  mutate(across(starts_with("task"), ~ timemin * ., .names = "time_{.col}")) %>%
  mutate(public = as.integer(random_private_public == 0)) %>%
  mutate(across(c(identity, lowertask, iden_lowertask, pairedcont, purecont, exptask), 
                ~ public * ., 
                .names = "pub_{.col}"))

var_label(dfl4$public) <- "Public"
var_label(dfl4$identity) <- "Identity task"
var_label(dfl4$lowertask) <- "Lower task"
var_label(dfl4$iden_lowertask) <- "Lower × Identity"
var_label(dfl4$pub_lowertask) <- "Public × Lower"
var_label(dfl4$pub_iden_lowertask) <- "Public × Lower × Identity"
var_label(dfl4$kaibarta) <- "Kaibarta"
var_label(dfl4$pana) <- "Pana"

# Refusals at task level
dfl4 <- dfl4 %>%
  mutate(refuse = nevertake_task) %>%
  mutate(temp1 = if_else(pairedcont == 1 & tag_pidtask == 1, refuse, NA),
         temp2 = if_else(identity == 1 & tag_pidtask == 1, refuse, NA),
         temp3 = if_else(tag_pidtask == 1, refuse, NA))

dfl4 <- dfl4 %>%
  arrange(pid) %>%
  group_by(pid) %>%
  mutate(numrefuse_cont = sum(temp1, na.rm = TRUE),
         numrefuse_iden = sum(temp2, na.rm = TRUE),
         numrefuse = sum(temp3, na.rm = TRUE)) %>%
  ungroup() %>%
  select(-temp1, -temp2, -temp3)

# Refusing a particular task
dfl4 <- dfl4 %>%
  mutate(across(num_range("task", 1:7), ~ as.integer(refuse == 1 & .x == 1), 
                .names = "temp{str_extract(.col, '\\\\d+')}")) %>%
  group_by(pid) %>%
  mutate(across(starts_with("temp"), max, .names = "refuse_task{str_extract(.col, '\\\\d+')}")) %>%
  ungroup() %>%
  select(-starts_with("temp")) %>%
  mutate(
    hasconcern = numrefuse_iden > 0,
    hasconcern_v2 = if_else(numrefuse < 6, numrefuse_iden > 0, NA), 
    refuseall = numrefuse >= 6,
    hasconcernstr = numrefuse_iden == 3,
    hasconcernstr_v2 = if_else(numrefuse < 6, numrefuse_iden == 3, NA)
  ) %>% # Experience
  mutate(
    neverperf = as.integer(rowSums(across(starts_with("extent"), ~ .x == 1), na.rm = TRUE) > 0),
    ownhhperf = as.integer(rowSums(across(starts_with("extent"), ~ .x == 2), na.rm = TRUE) > 0),
    outhhperf = as.integer(rowSums(across(starts_with("extent"), ~ .x %in% c(3, 4)), na.rm = TRUE) > 0),
    wageperf = as.integer(rowSums(across(starts_with("extent"), ~ .x %in% c(5, 6)), na.rm = TRUE) > 0))  %>%
  mutate(across(c(neverperf, ownhhperf, outhhperf, wageperf), 
                ~ if_else(is.na(extent1) | extent1 < 0, NA, .x))) %>%
  group_by(pid) %>%
  mutate(
    across(c(ownhhperf, outhhperf, wageperf, neverperf), 
           .fns = list(`2` = ~ ifelse(all(is.na(if_else(task == 2 & tag_pidtask == 1, .x, NA))), 
                                      NA, 
                                      max(if_else(task == 2 & tag_pidtask == 1, .x, NA), na.rm = TRUE)),
                       `4` = ~ ifelse(all(is.na(if_else(task == 4 & tag_pidtask == 1, .x, NA))), 
                                      NA, 
                                      max(if_else(task == 4 & tag_pidtask == 1, .x, NA), na.rm = TRUE)),
                       `6` = ~ ifelse(all(is.na(if_else(task == 6 & tag_pidtask == 1, .x, NA))), 
                                      NA, 
                                      max(if_else(task == 6 & tag_pidtask == 1, .x, NA), na.rm = TRUE))),
           .names = "{.col}{.fn}")) %>%
  ungroup() %>%
  mutate(
    reason_main = case_when(
      str_detect(reasonmain_oth, "HARD WORK") ~ 6,
      str_detect(reasonmain_oth, "HEALTH") ~ 12,
      str_detect(reasonmain_oth, "MY VILLAGE PEOPLE") ~ 5,
      TRUE ~ reasonmain  # Keep existing values otherwise
    ),
    reason_main_oth = if_else(reasonmain != -97 & !is.na(reasonmain), "-555", reasonmain_oth)
  ) %>%
  mutate(across(c(reason1, reason2, reason3, reason4), ~ if_else(.x > 20, .x * -1, .x)))

nlist <- c(1:8, 10, 12)

for (i in nlist) {
  temp_col <- paste0("temp", i)
  refuse_col <- paste0("refuse_reason", i)
  dfl4 <- dfl4 %>%
    mutate(!!temp_col := if_else(reason_main == i | reason1 == i | reason2 == i | 
                                   reason3 == i | reason4 == i, 1, 0, missing = 0)) %>%
    mutate(!!refuse_col := if_else(nevertake_task == 1, !!sym(temp_col), NA)) %>% 
    select(-all_of(temp_col))
}

dfl4 <- dfl4 %>%
  mutate(refuse_iden = if_else(identity == 1 & nevertake_task == 1, 0, NA),
         refuse_social = if_else(identity == 1 & nevertake_task == 1, 0, NA),
         refuse_skill = if_else(identity == 1 & nevertake_task == 1, 0, NA),
         refuse_iden = if_else(refuse_iden == 0 & (refuse_reason1 == 1 
                        | refuse_reason2 == 1 | refuse_reason3 == 1), 1, refuse_iden),
         refuse_social = if_else(refuse_social == 0 & (refuse_reason4 == 1 
                                        | refuse_reason5 == 1), 1, refuse_social),
         refuse_skill = if_else(refuse_skill == 0 & (refuse_reason6 == 1 | refuse_reason7 == 1 
                                          | refuse_reason8 == 1), 1, refuse_skill),
         
         refuse_iden_cont = if_else(pairedcont == 1 & nevertake_task == 1, 0, NA),
         refuse_social_cont = if_else(pairedcont == 1 & nevertake_task == 1, 0, NA),
         refuse_skill_cont = if_else(pairedcont == 1 & nevertake_task == 1, 0, NA),
         
         refuse_iden_cont = if_else(refuse_iden_cont == 0 & (refuse_reason1 == 1 
                                  | refuse_reason2 == 1 | refuse_reason3 == 1), 1, refuse_iden_cont),
         refuse_social_cont = if_else(refuse_social_cont == 0 & (refuse_reason4 == 1 
                                  | refuse_reason5 == 1), 1, refuse_social_cont),
         refuse_skill_cont = if_else(refuse_skill_cont == 0 & (refuse_reason6 == 1 
                                  | refuse_reason7 == 1 | refuse_reason8 == 1), 1, refuse_skill_cont),
         
         reason_both = if_else(identity == 1 & nevertake_task == 1, refuse_iden == 1 
                            & refuse_social == 1, NA),
         reason_onlyiden = if_else(identity == 1 & nevertake_task == 1, 
                            refuse_iden == 1 & refuse_social == 0, NA),
         reason_onlysocial = if_else(identity == 1 & nevertake_task == 1, 
                            refuse_iden == 0 & refuse_social == 1, NA),
         reason_neither = if_else(identity == 1 & nevertake_task == 1, 
                          refuse_iden == 0 & refuse_social == 0, NA),
         
         reason_both_cont = if_else(pairedcont == 1 & nevertake_task == 1, 
                            refuse_iden_cont == 1 & refuse_social_cont == 1, NA),
         reason_onlyiden_cont = if_else(pairedcont == 1 & nevertake_task == 1, 
                              refuse_iden_cont == 1 & refuse_social_cont == 0, NA),
         reason_onlysocial_cont = if_else(pairedcont == 1 & nevertake_task == 1,
                            refuse_iden_cont == 0 & refuse_social_cont == 1, NA),
         reason_neither_cont = if_else(pairedcont == 1 & nevertake_task == 1,
                          refuse_iden_cont == 0 & refuse_social_cont == 0, NA),
         b6a_paying_packet = if_else(b6a_paying_packet == -222, 1, b6a_paying_packet))

dfl4$compscore <- rowSums(dfl4[, c("b1a_determine_tea", "b2a_roll_die", "b3a_deter_price",  
                                   "b4a_cards_price", "b5a_get_offer", "b6a_paying_packet",  
                                   "c1a_wrk_paperbag", "c2a_add_task", "c3a_extra_wage",  
                                   "c4a_switch_add_task", "c5a_add_wage")],  
                          na.rm = TRUE)  

median_comp <- median(dfl4$compscore[dfl4$tag_pid == 1], na.rm = TRUE)
dfl4 <- dfl4 %>%
  mutate(hicomp = if_else(!is.na(compscore) & compscore >= median_comp, 1, 0))
attr(dfl4$compscore, "label") <- "Comprehension score"

write_dta(dfl4, file.path(cleandata, "choice_bonuswage_analysis_replicated.dta"))

# # # # # # 
# END 
# # # # # # 



