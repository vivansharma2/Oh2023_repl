# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
#
# PROJECT:		 Replication of "Does Identity Affect Labor Supply?" (Oh, 2023)
# REPLICATORS: Vivan Sharma, Rodrigo Pereira
# TASK:				 Data cleaning (job takeup data)
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

df <- read_dta(paste0(rawdata, "choice_jobtakeup.dta"))

# # # # # # 
# PART 1 
# # # # # #  

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

# # # # # # 
# PART 2
# # # # # #  

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

# Check summary statistics to ensure similarity
summary(df[, c("age", "year_edu", "read_odiya", "married", "famsize", "workmember", 
               "workshare", "kutcha_house", "semipucca_house", "pucca_house", 
               "own_land", "landsize", "income", "log_income", "paid_days", 
               "sew_mach_own", "bicycle_own", "motorbike_own", "fridge_own", 
               "radio_own", "tv_own", "mobile_own", "land_phone_own", 
               "stove_own", "watches_own")])

# Temporarily fill median values for observations with missing values for PCA scores
v2 <- c("workshare", "log_income")
for (x in v2) {
  # Create a backup of the original variable
  df[[paste0(x, "_miss")]] <- df[[x]]
  
  # Compute median, ignoring NA values
  median_value <- median(df[[x]], na.rm = TRUE)
  
  # Replace missing values with the median
  df[[x]][is.na(df[[x]])] <- median_value
}

# Perform PCA (standardizing the data)
v3 <- c("workshare", "log_income", "kutcha_house", "semipucca_house", 
        "own_land", "landsize", "paid_days", 
        "sew_mach_own", "bicycle_own", "motorbike_own", "fridge_own", 
        "radio_own", "tv_own", "mobile_own", "land_phone_own", 
        "stove_own", "watches_own")
# Identify rows with complete cases
complete_rows <- complete.cases(df[, v3])
# Run PCA on non-missing observations
pca_result <- prcomp(df[complete_rows, v3], center = TRUE, scale. = TRUE)
# Extract the first principal component scores
pca_scores <- pca_result$x[, 1]  
# Create a new column and insert PCA scores into the original dataset
df$wealth_pca <- NA  
df$wealth_pca[complete_rows] <- -pca_scores 

# # # # # # 
# NOTE: We believe that the original code for this section may have been 
# slightly incorrect; the code for imputing the missings as median values for
# 'workshare' and 'log_income' came after the 'pca' command in STATA. The 
# 'predict' command that followed did not take into account this change. This 
# difference is not significant, but worth noting anyhow.
# # # # # #  

# Drop imputed vars and rename originals
df <- df[, !names(df) %in% v2]
for (x in v2) {
  miss_var <- paste0(x, "_miss")
  if (miss_var %in% names(df)) {
    names(df)[names(df) == miss_var] <- x
  }
}

# Summarize wealth_pca and create the hiwealth variable
wealth_pca_summary <- summary(df$wealth_pca, na.rm = TRUE)
median_wealth_pca <- wealth_pca_summary[3]  # The median is typically the 3rd value in the summary
df$hiwealth <- df$wealth_pca > median_wealth_pca

# Summarize year_edu and create the hiedu variable
year_edu_summary <- summary(df$year_edu, na.rm = TRUE)
median_year_edu <- year_edu_summary[3]
df$hiedu <- df$year_edu > median_year_edu

# Summarize age and create the old variable
age_summary <- summary(df$age, na.rm = TRUE)
median_age <- age_summary[3]
df$old <- df$age > median_age

# Summarize paid_days and create the hjobs variable
paid_days_summary <- summary(df$paid_days, na.rm = TRUE)
median_paid_days <- paid_days_summary[3]
df$hijobs <- df$paid_days > median_paid_days


# # # # # # 
# PART 3
# # # # # #  

# Rename variables

df <- df %>%
  rename(
    agree10 = c1a_agree_make_rope,
    reason1_not10 = c1b_reason_not_make_rope1,
    reason2_not10 = c1b_reason_not_make_rope2,
    reason3_not10 = c1b_reason_not_make_rope3,
    reason_not_sfy10 = c1b_reason_not_make_rope_oth,
    migrate10 = c1c_migrate_make_rope,
    migrate_oth10 = c1c_migrate_make_rope_oth,
    extent1_10 = c1d_extent_make_rope,
    extent_oth10 = c1d_extent_make_rope_oth,
    perform10 = c1e_perform_make_rope,
    perform_oth10 = c1e_perform_make_rope_oth,
    agree9 = c2a_agree_pnut_dshel,
    reason1_not9 = c2b_reason_not_pnut_dshel,
    reason_not_sfy9 = c2b_reason_not_pnut_dshel_oth,
    migrate9 = c2c_migrate_pnut_dshel,
    migrate_oth9 = c2c_migrate_pnut_dshel_oth,
    extent1_9 = c2d_extent_pnut_dshel1,
    extent2_9 = c2d_extent_pnut_dshel2,
    extent_oth9 = c2d_extent_pnut_dshel_oth,
    perform9 = c2e_perform_pnut_dshel,
    perform_oth9 = c2e_perform_pnut_dshel_oth,
    agree7 = c3a_agree_sweep_shed,
    reason1_not7 = c3b_reason_not_sweep_shed1,
    reason2_not7 = c3b_reason_not_sweep_shed2,
    reason3_not7 = c3b_reason_not_sweep_shed3,
    reason_not_sfy7 = c3b_reason_not_sweep_shed_oth,
    migrate7 = c3c_migrate_sweep_shed,
    migrate_oth7 = c3c_migrate_sweep_shed_oth,
    extent1_7 = c3d_extent_sweep_shed,
    extent_oth7 = c3d_extent_sweep_shed_oth,
    perform7 = c3e_perform_sweep_shed,
    perform_oth7 = c3e_perform_sweep_shed_oth,
    agree3 = c4a_agree_wash,
    reason1_not3 = c4b_reason_not_wash1,
    reason2_not3 = c4b_reason_not_wash2,
    reason3_not3 = c4b_reason_not_wash3,
    reason_not_sfy3 = c4b_reason_not_wash_oth,
    migrate3 = c4c_migrate_wash,
    migrate_oth3 = c4c_migrate_wash_oth,
    extent1_3 = c4d_extent_wash1,
    extent_oth3 = c4d_extent_wash_oth,
    perform3 = c4e_perform_wash,
    perform_oth3 = c4e_perform_wash_oth,
    agree5 = c5a_agree_repair_grassmat,
    reason1_not5 = c5b_reason_not_repair_grassmat1,
    reason2_not5 = c5b_reason_not_repair_grassmat2,
    reason3_not5 = c5b_reason_not_repair_grassmat3,
    reason_not_sfy5 = c5b_reason_not_repair_oth,
    migrate5 = c5c_migrate_repair_grassmat,
    migrate_oth5 = c5c_migrate_repair_grassmat_oth,
    extent1_5 = c5d_extent_repair_grassmat,
    extent_oth5 = c5d_extent_repair_grassmat_oth,
    perform5 = c5e_perform_repair_grassmat,
    perform_oth5 = c5e_perform_repair_grassmat_oth,
    agree11 = c6a_agree_stitching,
    reason1_not11 = c6b_reason_not_stitching1,
    reason2_not11 = c6b_reason_not_stitching2,
    reason3_not11 = c6b_reason_not_stitching3,
    reason_not_sfy11 = c6b_reason_not_stitching_oth,
    migrate11 = c6c_migrate_stitching,
    migrate_oth11 = c6c_migrate_stitching_oth,
    extent1_11 = c6d_extent_stitching,
    extent_oth11 = c6d_extent_stitching_oth,
    perform11 = c6e_perform_stitching,
    perform_oth11 = c6e_perform_stitching_oth,
    agree2 = c7a_agree_wash_cloth,
    reason1_not2 = c7b_reason_not_wash_cloth1,
    reason2_not2 = c7b_reason_not_wash_cloth2,
    reason3_not2 = c7b_reason_not_wash_cloth3,
    reason4_not2 = c7b_reason_not_wash_cloth4,
    reason5_not2 = c7b_reason_not_wash_cloth5,
    reason_not_sfy2 = c7b_reason_not_wash_cloth_oth,
    migrate2 = c7c_migrate_wash_cloth,
    migrate_oth2 = c7c_migrate_wash_cloth_oth,
    extent1_2 = c7d_extent_wash_cloth,
    extent_oth2 = c7d_extent_wash_cloth_oth,
    perform2 = c7e_perform_wash_cloth,
    perform_oth2 = c7e_perform_wash_cloth_oth,
    agree4 = c8a_agree_polish_shoe,
    reason1_not4 = c8b_reason_not_polish_shoe1,
    reason2_not4 = c8b_reason_not_polish_shoe2,
    reason3_not4 = c8b_reason_not_polish_shoe3,
    reason4_not4 = c8b_reason_not_polish_shoe4,
    reason5_not4 = c8b_reason_not_polish_shoe5,
    reason6_not4 = c8b_reason_not_polish_shoe6,
    reason_not_sfy4 = c8b_reason_not_polish_shoe_oth,
    migrate4 = c8c_migrate_polish_shoe,
    migrate_oth4 = c8c_migrate_polish_shoe_oth,
    extent1_4 = c8d_extent_polish_shoe1,
    extent2_4 = c8d_extent_polish_shoe2,
    extent3_4 = c8d_extent_polish_shoe3,
    extent4_4 = c8d_extent_polish_shoe4,
    extent_oth4 = c8d_extent_polish_shoe_oth,
    perform4 = c8e_perform_polish_shoe,
    perform_oth4 = c8e_perform_polish_shoe_oth,
    agree6 = c9a_agree_sweep_latr,
    reason1_not6 = c9b_reason_not_sweep_latr1,
    reason2_not6 = c9b_reason_not_sweep_latr2,
    reason3_not6 = c9b_reason_not_sweep_latr3,
    reason4_not6 = c9b_reason_not_sweep_latr4,
    reason5_not6 = c9b_reason_not_sweep_latr5,
    reason6_not6 = c9b_reason_not_sweep_latr6,
    reason_not_sfy6 = c9b_reason_not_sweep_latr_oth,
    migrate6 = c9c_migrate_sweep_latr,
    migrate_oth6 = c9c_migrate_sweep_latr_oth,
    extent1_6 = c9d_extent_sweep_latr1,
    extent_oth6 = c9d_extent_sweep_latr_oth,
    perform6 = c9e_perform_sweep_latr,
    perform_oth6 = c9e_perform_sweep_latr_oth,
    extent1_8 = c10d_extent_paper_bag1,
    extent2_8 = c10d_extent_paper_bag2,
    extent3_8 = c10d_extent_paper_bag3,
    extent_oth8 = c10d_extent_paper_bag_oth
  )

# # # # # # 
# PART 4
# # # # # #  

# Create binary for lower values (more conservative)
df <- df %>%
  mutate(across(c(d1_karthik_tuna, d2_bindusagar_rabi, d3_gagan_find_work), 
                ~ ifelse(!is.na(.), ifelse(. %in% c(1, 2), 1, 0), NA),
                .names = "{.col}_cons"))
# Create binary for higher values (more conservative)
df <- df %>%
  mutate(across(c(d4_santhilatha_college, d5_nehru_finish_ssc, d6_sameer_jena, d7_tukuna_naika), 
                ~ ifelse(!is.na(.), ifelse(. %in% c(4, 5), 1, 0), NA),
                .names = "{.col}_cons"))

# Compare with original code
summary(df[, c("d1_karthik_tuna_cons", "d2_bindusagar_rabi_cons", "d3_gagan_find_work_cons",
               "d4_santhilatha_college_cons", "d5_nehru_finish_ssc_cons", 
               "d6_sameer_jena_cons", "d7_tukuna_naika_cons")])

df <- df %>%
  mutate(conserv_index = rowSums(select(df, d1_karthik_tuna_cons, d2_bindusagar_rabi_cons, 
                                        d3_gagan_find_work_cons, d4_santhilatha_college_cons, 
                                        d5_nehru_finish_ssc_cons, d6_sameer_jena_cons, 
                                        d7_tukuna_naika_cons), na.rm = FALSE))

df <- df %>%
  mutate(conserv5up = ifelse(conserv_index >= 5, TRUE, FALSE))

v4 <- c("d1_karthik_tuna_cons", "d2_bindusagar_rabi_cons", "d3_gagan_find_work_cons",
        "d4_santhilatha_college_cons", "d5_nehru_finish_ssc_cons", 
        "d6_sameer_jena_cons", "d7_tukuna_naika_cons")

# Rerun the PCA procedure
complete_rows1 <- complete.cases(df[, v4])
pca_result1 <- prcomp(df[complete_rows1, v4], center = TRUE, scale. = TRUE)
pca_scores1 <- pca_result1$x[, 1]  # Extract PC1
df$pca2 <- NA  
df$pca2[complete_rows1] <- pca_scores1  # Fill 

pca2_summary <- summary(df$pca2, na.rm = TRUE)
median_pca2 <- pca2_summary[3]  # The median is typically the 3rd value in the summary
df$hiconserv <- df$pca2 > median_pca2

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
var_label(df$pca2) <- "Caste sensitivity PCA score"
var_label(df$hiwealth) <- "High wealth"
var_label(df$hiedu) <- "High education"
var_label(df$old) <- "Older"
var_label(df$conserv5up) <- "Caste-sensitive (5+)"
var_label(df$hiconserv) <- "Caste-sensitive (above median score)"

# # # # # # 
# PART 5
# # # # # #  

# Reshaping long with time variation
num <- 5

time_labels <- c("10min", "30min", "1hr", "1_30hr")
v5 <- c("wash_clothes", "animal_shed", "shoe_repair", "wash_agri", "deshell", "grass_mat", "stitching", "latrine", "rope")

# Rename columns
temp_names <- c()
for (x in v5) {
  for (i in seq_along(time_labels)) {
    old_name <- paste0("c", num, "_", x, "_", time_labels[i])
    new_name <- paste0(x, i)
    names(df)[names(df) == old_name] <- new_name
  }
  num <- num + 1
}

renamed_columns <- unlist(lapply(v5, function(x) paste0(x, 1:4)))

# Reshape to long format
df1 <- df %>%
  pivot_longer(cols = c(wash_clothes1:wash_clothes4, animal_shed1:animal_shed4, shoe_repair1:shoe_repair4,
                        wash_agri1:wash_agri4, deshell1:deshell4, grass_mat1:grass_mat4, stitching1:stitching4,
                        latrine1:latrine4, rope1:rope4),  # picked columns to reshape
               names_to = c(".value", "timecat"),   
               names_pattern = "(.*)(\\d+)") %>% 
  mutate(timecat = as.integer(timecat)) %>%  
  arrange(pid, timecat)

# Create time variables
df1 <- df1 %>%
  mutate(timemin = case_when(
    timecat == 1 ~ 10,
    timecat == 2 ~ 30,
    timecat == 3 ~ 60,
    timecat == 4 ~ 90,
    TRUE ~ NA_real_
  )) %>%
  mutate(timehr = timemin / 60)  

# Handling missing values (replace -222 with NAs)
df1 <- df1 %>% mutate(across(all_of(v5), ~ na_if(., -222)))
df1 <- df1 %>% mutate(had_rope = !is.na(rope))

# Labelling
label1 <- list(
  wash_clothes = "Washing clothes",
  animal_shed = "Sweeping animal sheds",
  shoe_repair = "Mending leather shoes",
  wash_agri = "Washing farming tools",
  deshell = "Deshelling peanuts",
  grass_mat = "Mending grass mats",
  stitching = "Stitching",
  latrine = "Sweeping latrines",
  rope = "Making ropes",
  had_rope = "Random choice set variation with rope"
)

for (var in names(label1)) {
  attr(df1[[var]], "label") <- label1[[var]]
}

write_dta(df1, file.path(cleandata, "choice_jobtakeup_wide_replicated.dta"))

# # # # # # 
# PART 6
# # # # # #  

# Reshaping long with task variation
df2 <- df1
df2 <- df2 %>% 
  group_by(pid, timecat) %>%  
  mutate(id_time = cur_group_id()) %>%  
  ungroup()  

# Renaming
df2 <- df2 %>% rename(
  takeup2 = wash_clothes,
  takeup3 = wash_agri,
  takeup4 = shoe_repair,
  takeup5 = grass_mat,
  takeup6 = latrine,
  takeup7 = animal_shed,
  takeup9 = deshell,
  takeup10 = rope,
  takeup11 = stitching,
  
  taskorder2 = wash_cloths_sno,
  taskorder3 = wash_agri_tool_sno,
  taskorder4 = shoe_repair_sno,
  taskorder5 = make_grass_mat_sno,
  taskorder6 = latrine_sweep_sno,
  taskorder7 = animal_shed_sweep_sno,
  taskorder9 = deshell_peanut_sno,
  taskorder10 = make_rope_sno,
  taskorder11 = stitching_sno
)

allv <- names(df2)
long_vars <- c("takeup", "taskorder", "agree", "reason1_not", "reason2_not", 
               "reason3_not", "reason4_not", "reason5_not", "reason6_not", 
               "reason_not_sfy", "migrate", "migrate_oth", "extent1_", 
               "extent2_", "extent3_", "extent4_", "extent_oth", "perform", "perform_oth")

# Filter variables that exist in df
v6 <- allv[
  allv %in% unlist(lapply(long_vars, function(x) grep(paste0("^", x, "\\d+$"), allv, value = TRUE)))
]

# Reshape from wide to long
df2 <- df2 %>%
  pivot_longer(
    cols = all_of(v6),
    names_to = c(".value", "task"),
    names_pattern = "^(.*?)(\\d+)$"  # Prevent over-capturing of suffix numbers
  ) %>%
  mutate(task = as.integer(task)) %>%
  arrange(id_time, task)

colnames(df2) <- gsub("_$", "", colnames(df2))

# Define task labels
label2 <- c(
  "2" = "Washing clothes",
  "3" = "Washing farming tools",
  "4" = "Mending leather shoes",
  "5" = "Mending grass mats",
  "6" = "Sweeping latrines",
  "7" = "Sweeping animal sheds",
  "8" = "Making paper bags",
  "9" = "Deshelling peanuts",
  "10" = "Making ropes",
  "11" = "Stitching"
)

label2 <- setNames(
  as.numeric(names(label2)),  # Convert names to numeric
  label2  # Keep values as character labels
)

df2$task <- labelled(df2$task, labels = label2)

# Making unique group identifier for 'pid' and 'task'
df2 <- df2 %>%
  mutate(pidtask = as.integer(interaction(pid, task, drop = TRUE)))

# Flagging unique (pid, task) combinations
df2 <- df2 %>%
  group_by(pid, task) %>%
  mutate(tag_pidtask = ifelse(row_number() == 1, 1, 0)) %>%
  ungroup()

# Specifically for washing clothes
df2 <- df2 %>%
  mutate(tag_pid = ifelse(as_factor(task) == "Washing clothes" & !duplicated(pid), 1, 0))

# # # # # # 
# PART 7
# # # # # #  

# Generating variables for analysis

# Castes
df2$private <- as.integer(df2$random_private_public == 1)
df2$public <- as.integer(df2$random_private_public == 2)

df2$castelev <- ifelse(df2$caste == 7, 4, NA)
df2$castelev <- ifelse(df2$caste %in% c(5, 6), 3, df2$castelev)
df2$castelev <- ifelse(df2$caste %in% c(3, 4), 2, df2$castelev)
df2$castelev <- ifelse(df2$caste %in% c(1, 2), 1, df2$castelev)

df2$castelev_old <- ifelse(df2$caste %in% c(7, 4), 4, NA)
df2$castelev_old <- ifelse(df2$caste %in% c(5, 6), 3, df2$castelev_old)
df2$castelev_old <- ifelse(df2$caste %in% c(1, 3), 2, df2$castelev_old)
df2$castelev_old <- ifelse(df2$caste == 2, 1, df2$castelev_old)
                         
# Creating indicators for each level
for (i in 1:4) {
  df2[[paste0("castelev", i)]] <- as.integer(df2$castelev == i)
  df2[[paste0("castelev_old", i)]] <- as.integer(df2$castelev_old == i)
}

# Tasks
df2$tasklev <- ifelse(df2$task %in% c(8, 9, 10, 11), 1, NA)
df2$tasklev <- ifelse(df2$task %in% c(2, 3), 2, df2$tasklev)
df2$tasklev <- ifelse(df2$task %in% c(4, 5), 3, df2$tasklev)
df2$tasklev <- ifelse(df2$task %in% c(6, 7), 4, df2$tasklev)

for (i in 1:4) {
  df2[[paste0("tasklev", i)]] <- ifelse(df2$tasklev == i, 1, 0)
}

df2 <- df2 %>%
  mutate(taskpair = ifelse(tasklev == 1, task, tasklev)) %>%  
  group_by(pid, taskpair) %>%
  mutate(pidtaskpair = cur_group_id()) %>%
  ungroup()
  
df2$identity <- ifelse(df2$task %in% c(2, 4, 6), 1, 0)
df2$pairedcont <- ifelse(df2$task %in% c(3, 5, 7), 1, 0)
df2$purecont <- ifelse(df2$task %in% c(8, 9, 10, 11), 1, 0)

# Comparison of tasks caste wise
df2 <- df2 %>%
  mutate(lowertask = ifelse(!is.na(tasklev) & !is.na(castelev) & castelev < tasklev, 1, 0),
         lowertask_old = ifelse(!is.na(tasklev) & !is.na(castelev_old) & castelev_old < tasklev, 1, 0),
         lowertask_old2 = lowertask_old) %>%
  mutate(lowertask_old2 = ifelse(caste == 1 & task %in% c(2, 3), 1, lowertask_old2),
         lowertask_old2 = ifelse(caste == 4 & task %in% c(6, 7), 1, lowertask_old2))

# Matching tasks with castes
df2 <- df2 %>%
  mutate(sametask = ifelse(!is.na(tasklev) & !is.na(castelev), 0, NA),
         sametask = ifelse(caste == 3 & tasklev == 2, 1, sametask),
         sametask = ifelse(caste == 5 & tasklev == 3, 1, sametask),
         sametask = ifelse(caste == 7 & tasklev == 4, 1, sametask))

# Be careful of ordering - want to keep NAs
df2$highertask <- ifelse(df2$lowertask == 0 & df2$sametask == 0 & 
                           df2$purecont == 0, 1, 0)
df2$highertask <- ifelse(!is.na(df2$tasklev) & !is.na(df2$castelev) & 
                           df2$purecont == 0, df2$highertask, NA)

df2$diftask <- ifelse(df2$purecont == 0 & df2$sametask != 1, 1, 0)

# Implementation variables
df2 <- df2 %>%        
  mutate( offer_type = case_when(
    job_offer_task == 1 ~ 2,
    job_offer_task == 4 ~ 3,
    job_offer_task == 3 ~ 4,
    job_offer_task == 6 ~ 5,
    job_offer_task == 8 ~ 6,
    job_offer_task == 2 ~ 7,
    job_offer_task == 5 ~ 9,
    job_offer_task == 9 ~ 10,
    job_offer_task == 7 ~ 11,
    TRUE ~ NA_real_  # Default missing value
  ),
  
  # Define offer_tasklev
  offer_tasklev = case_when(
    offer_type %in% c(9, 10, 11) ~ 1,
    offer_type %in% c(2, 3) ~ 2,
    offer_type %in% c(4, 5) ~ 3,
    offer_type %in% c(6, 7) ~ 4,
    TRUE ~ NA_real_
  ),
  
  # Define lowertask_offer
  lowertask_offer = ifelse(castelev < offer_tasklev, 1, 0),
  lowertask_offer = ifelse(!is.na(offer_tasklev) & !is.na(castelev), lowertask_offer, NA),
  
  # Define sametask_offer
  sametask_offer = ifelse(!is.na(offer_tasklev) & !is.na(castelev), 0, NA),
  sametask_offer = ifelse(caste == 3 & offer_tasklev == 2, 1, sametask_offer),
  sametask_offer = ifelse(caste == 5 & offer_tasklev == 3, 1, sametask_offer),
  sametask_offer = ifelse(caste == 7 & offer_tasklev == 4, 1, sametask_offer),
  
  # Define diftask_offer
  diftask_offer = ifelse(!is.na(sametask_offer) & (sametask_offer != 1 & offer_tasklev != 1), 1, 0),
  
  # Define identity-related variables
  iden_offer = ifelse(offer_type %in% c(2, 4, 6), 1, 0),
  iden_offer = ifelse(is.na(offer_type), NA, iden_offer),
  iden_lowertask_offer = lowertask_offer * iden_offer,
  iden_sametask_offer = sametask_offer * iden_offer,
  iden_diftask_offer = diftask_offer * iden_offer,
  
  # Define offer-related outcomes
  offer_accept = ifelse(job_accepted == 1, 1, 0),
  offer_completed = ifelse(job_completed == 1, 1, 0))

df2 <- df2 %>%
  rename(merge_survey = `_merge_survey`)  # Rename first 
df2$survey_completed <- ifelse(df2$merge_survey == 3, 1, 0)


df2 <- df2 %>%
  mutate(across(c(lowertask, lowertask_old, lowertask_old2, sametask, diftask), 
                ~ identity * .x, .names = "iden_{.col}"),
         timeminq = timemin^2)

# Create caste dummy variables for values 1 to 7
for (y in 1:7) {
  df2[[paste0("caste", y)]] <- as.integer(df2$caste == y)
}

# Create task dummies and interaction terms for timemin
numlist <- c(2:7, 9:11) 

for (x in numlist) {
  df2[[paste0("task", x)]] <- ifelse(!is.na(df2$task), as.integer(df2$task == x), 0)
  df2[[paste0("time_task", x)]] <- df2$timemin * df2[[paste0("task", x)]]
  df2[[paste0("time_taskq", x)]] <- df2$timeminq * df2[[paste0("task", x)]]
}

v7 <- c("old", "age", "hiedu", "year_edu", "hiwealth", "wealth_pca", "paid_days", "hijobs")
# Loop through each variable
for (var in v7) {
  for (x in numlist) {
    new_var_name <- paste0(var, x)  # Name for the new variable
    df2[[new_var_name]] <- df2[[var]] * df2[[paste0("task", x)]]
  }
}

df2$compscore <- rowSums(df2[, c("b6_choice_t1", "b7_offer_t1", "b8_offchoice_t1", 
                               "c1_dice_t1", "c2_card_t1", "c3_choiceyes_t1", "c4_choiceno_t1")], na.rm = FALSE)
mcp <- median(df2$compscore[df2$tag_pid == 1], na.rm = TRUE)
df2$hicomp <- ifelse(!is.na(df2$compscore) & df2$compscore >= mcp, 1, 0)
attr(df2$compscore, "label") <- "Comprehension score"

# Sort
df2 <- df2[order(df2$pid, df2$task, df2$timecat), ]

df2$temp1 <- ifelse(df2$takeup == 1, df2$timecat, NA)
df2$temp2 <- ifelse(df2$takeup == 0, df2$timecat, NA)

# Assign max values to temp1, temp2
df2 <- df2 %>%
  group_by(pid, task) %>%
  mutate(temp1b = ifelse(all(is.na(temp1)), NA, max(temp1, na.rm = TRUE)),
         temp2b = ifelse(all(is.na(temp2)), NA, min(temp2, na.rm = TRUE))) %>%
  ungroup()

# First unique combination of pid, task
df2 <- df2 %>%
  mutate(temp5 = as.integer(!duplicated(df2[, c("pid", "task")])))

sum(!is.na(df2$temp1b) & !is.na(df2$temp2b) & df2$temp5 == 1)
sum(!is.na(df2$temp1b) & !is.na(df2$temp2b) & df2$temp5 == 1 & df2$temp1b > df2$temp2b)

# Choice reversal indicator
df2$reversal_task <- !is.na(df2$temp1b) & !is.na(df2$temp2b) & df2$temp1b > df2$temp2b

# Max reversal per pid
df2 <- df2 %>%
  group_by(pid) %>%
  mutate(reversal_pid = max(reversal_task, na.rm = TRUE)) %>%
  ungroup()

table(df2$reversal_task[df2$tag_pidtask == 1])  
table(df2$reversal_pid[df2$tag_pid == 1])

# Drop temp vars
df2 <- df2 %>% select(-starts_with("temp"))

v8<- c("identity", "pairedcont", "purecont", "lowertask", "sametask", 
             "iden_lowertask", "iden_sametask", "diftask", "iden_diftask")
for (x in v8) {
  nvar <- paste0("pub_", x)
  df2[[nvar]] <- df2[["public"]] * df2[[x]]
}

# Reorder columns
df2 <- df2 %>%
  select(pid, task, timecat, timemin, timehr, private, public, 
         id_time, pidtask, tag_pidtask, tag_pid, everything()) 

# Final labels
var_label(df2$age) <- "Age"
var_label(df2$married) <- "Married"
var_label(df2$famsize) <- "Family size"
var_label(df2$workshare) <- "Share of working members"
var_label(df2$year_edu) <- "Years of education"
var_label(df2$read_odiya) <- "Able to read"
var_label(df2$pucca_house) <- "Non-mud house"
var_label(df2$semipucca_house) <- "Semi-mud house"
var_label(df2$kutcha_house) <- "Mud house"
var_label(df2$own_land) <- "Owns land"
var_label(df2$landsize) <- "Land size in acres"
var_label(df2$income) <- "Last month income in Rs."
var_label(df2$log_income) <- "Log of last month income"
var_label(df2$paid_days) <- "Paid work days last week"
var_label(df2$own_index) <- "Number of assets owned"
var_label(df2$wealth_pca) <- "Wealth PCA score"
var_label(df2$conserv_index) <- "Number of caste-sensitive views"
var_label(df2$pca2) <- "Caste sensitivity PCA score"
var_label(df2$hiwealth) <- "High wealth"
var_label(df2$hiedu) <- "High education"
var_label(df2$old) <- "Older"
var_label(df2$conserv5up) <- "Caste-sensitive (5+)"
var_label(df2$hiconserv) <- "Caste-sensitive (above median score)"

write_dta(df2, file.path(cleandata, "choice_jobtakeup_analysis_replicated.dta"))

# # # # # # 
# END 
# # # # # # 

