# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
#
# PROJECT:		 Replication of "Does Identity Affect Labor Supply?" (Oh, 2023)
# REPLICATORS: Vivan Sharma, Rodrigo Pereira
# TASK:				 data analysis (bonus experiment data) (Table 4 & Figure 4)
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

df <- read_dta(paste0(cleandata, "choice_bonuswage_analysis_replicated.dta"))

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
  #		Figure 4: Willingness to switch to working on extra tasks - Panel A
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

df <- df %>%
  group_by(timecat) %>%
  mutate(tag_time = if_else(row_number() == 1, 1, 0)) %>% 
  ungroup() %>%
  mutate(
    take30 = ifelse(is.na(minwage_tasktime_imp), NA, ifelse(minwage_tasktime_imp <= 30, 1, 0)),
    take3000 = ifelse(is.na(minwage_tasktime_imp), NA, ifelse(minwage_tasktime_imp <= 3000, 1, 0)),
    take30_id = ifelse(identity == 1, take30, NA),
    take30_pc = ifelse(pairedcont == 1, take30, NA),
    take3000_id = ifelse(identity == 1, take3000, NA),
    take3000_pc = ifelse(pairedcont == 1, take3000, NA)
  )

# Means within timecat
df <- df %>%
  group_by(timecat) %>%
  mutate(
    take30_id_l = mean(take30_id, na.rm = TRUE),
    take30_pc_l = mean(take30_pc, na.rm = TRUE),
    take3000_id_l = mean(take3000_id, na.rm = TRUE),
    take3000_pc_l = mean(take3000_pc, na.rm = TRUE)
  ) %>%
  ungroup()

# Plot
# Identity tasks plot (p1)
p1 <- ggplot(df %>% filter(tag_time == 1)) +
  geom_line(aes(x = timemin, y = take30_id_l), color = "green", linetype = "dashed", linewidth = 1.2) +
  geom_point(aes(x = timemin, y = take30_id_l), color = "green", size = 3) +
  geom_line(aes(x = timemin, y = take3000_id_l), color = "green", linewidth = 1.2) +
  geom_point(aes(x = timemin, y = take3000_id_l), color = "green", shape = 17, size = 3) + 
  scale_x_continuous(limits = c(5, 65)) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(title = "Identity tasks", x = "Time in minutes", y = "Take-up rate") +
  theme_minimal(base_size = 16) +
  theme(plot.title = element_text(hjust = 0.5))  

# Paired control tasks plot (p2)
p2 <- ggplot(df %>% filter(tag_time == 1)) +
  geom_line(aes(x = timemin, y = take30_pc_l), color = "blue", linetype = "dashed", linewidth = 1.2) +
  geom_point(aes(x = timemin, y = take30_pc_l), color = "blue", size = 3) +
  geom_line(aes(x = timemin, y = take3000_pc_l), color = "blue", linewidth = 1.2) +
  geom_point(aes(x = timemin, y = take3000_pc_l), color = "blue", shape = 17, size = 3) + 
  scale_x_continuous(limits = c(5, 65)) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(title = "Paired control tasks", x = "Time in minutes", y = "Take-up rate") +
  theme_minimal(base_size = 16) +
  theme(plot.title = element_text(hjust = 0.5))  

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
  #		Figure 4: Willingness to switch to working on extra tasks - Panel B
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
  
df <- df %>%
  mutate(
    minwage_imp_v2 = minwage_task_imp / 30,
    minwage_imp_v2 = case_when(
      minwage_imp_v2 == 30 ~ 13,
      minwage_imp_v2 == 50 ~ 16,
      minwage_imp_v2 == 100 ~ 22,
      minwage_imp_v2 > 100 & minwage_imp_v2 < 200 ~ 24,
      TRUE ~ minwage_imp_v2  
  )
)

minwage_labels <- c(
  "0" = "0", "1" = "30", "2" = "60", "3" = "90", "4" = "120", 
  "6" = "180", "8" = "240", "10" = "300", "13" = "900", 
  "16" = "1500", "22" = "3K", "24" = ">3K"
)

# Ref. dataset
all_values <- tibble(minwage_imp_v2 = 0:24)

# Frequencies
dfc_iden1 <- df %>%
  filter(tag_pidtasktime & timecat == 1 & identity == 1) %>%
  count(minwage_imp_v2) %>%
  right_join(all_values, by = "minwage_imp_v2") %>%
  mutate(n = replace_na(n, 0))  

dfc_iden1 <- dfc_iden1 %>%
  mutate(minwage_imp_v2_label = factor(as.character(minwage_imp_v2), 
                                       levels = names(minwage_labels), 
                                       labels = minwage_labels))
# Remove NAs 
dfc_iden1 <- dfc_iden1 %>% filter(!is.na(minwage_imp_v2_label)) %>% droplevels()

# Plot 
# Identity tasks (p3)
p3 <- ggplot(dfc_iden1, aes(x = minwage_imp_v2_label, y = n / sum(n))) +  
  geom_bar(stat = "identity", fill = "green", color = "darkgreen", width = 0.8) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 0.45)) +
  labs(title = "Identity tasks", 
       x = "Minimum additional wage", 
       y = "Share of workers") +
  theme_minimal(base_size = 16) +
  theme(plot.title = element_text(hjust = 0.5))  
                                

# Frequencies
dfc_iden2 <- df %>%
  filter(tag_pidtasktime & timecat == 1 & pairedcont == 1) %>%
  count(minwage_imp_v2) %>%
  right_join(all_values, by = "minwage_imp_v2") %>%
  mutate(n = replace_na(n, 0))  

dfc_iden2 <- dfc_iden2 %>%
  mutate(minwage_imp_v2_label = factor(as.character(minwage_imp_v2), 
                                       levels = names(minwage_labels), 
                                       labels = minwage_labels))
# Remove NAs 
dfc_iden2 <- dfc_iden2 %>% filter(!is.na(minwage_imp_v2_label)) %>% droplevels()

# Paired control tasks (p4)
p4 <- ggplot(dfc_iden2, aes(x = minwage_imp_v2_label, y = n / sum(n))) +  
  geom_bar(stat = "identity", fill = "blue", color = "darkblue", width = 0.8) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 0.45)) +
  labs(title = "Identity tasks", 
       x = "Minimum additional wage", 
       y = "Share of workers") +
  theme_minimal(base_size = 16) +
  theme(plot.title = element_text(hjust = 0.5))  

cplot <- (p1 + p2) / (p3 + p4) 
ggsave(file.path(output, "Figure4.png"), plot = cplot, width = 16, height = 12)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
  #		Table 4: Caste inconsistency and refusal of all offers involving a task
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

df <- df %>%
  filter(purecont != 1) %>%  # Drop rows where purecont == 1
  mutate(across(c(old, age, hiedu, year_edu, own_index, compscore, hicomp, hiwealth, wealth_pca), 
                .fns = list(
                  "1" = ~ . * task1, 
                  "2" = ~ . * task2, 
                  "3" = ~ . * task3, 
                  "4" = ~ . * task4, 
                  "5" = ~ . * task5, 
                  "6" = ~ . * task6, 
                  "7" = ~ . * task7),
                .names = "{.col}{.fn}")) 

# Run the regressions
# Caste FE
m1 <- feols(nevertake_task ~ identity | caste, 
            cluster = ~pid, data = df %>% filter(tag_pidtask == 1))

# Worker FE
m2 <- feols(nevertake_task ~ identity | pid, 
            cluster = ~pid, data = df %>% filter(tag_pidtask == 1))

# Demographic controls (binary)
m3 <- feols(nevertake_task ~ identity + old2 + old3 + old4 + old5 + old6 + old7 + hiedu2 +
              hiedu3 + hiedu4 + hiedu5 + hiedu6 + hiedu7 + hiwealth2 + 
              hiwealth3 + hiwealth4 + hiwealth5 + hiwealth6 + hiwealth7 | pid, 
            cluster = ~pid, data = df %>% filter(tag_pidtask == 1))

# Public identity
m4 <- feols(nevertake_task ~ identity + pub_identity | pid, 
            cluster = ~pid, data = df %>% filter(tag_pidtask == 1))

# Public identity X controls
m5 <- feols(nevertake_task ~ identity + pub_identity + old2 + old3 + old4 + old5 + old6 + old7 + hiedu2 +
              hiedu3 + hiedu4 + hiedu5 + hiedu6 + hiedu7 + hiwealth2 + 
              hiwealth3 + hiwealth4 + hiwealth5 + hiwealth6 + hiwealth7 | pid, 
            cluster = ~pid, data = df %>% filter(tag_pidtask == 1))

models <- list(m1, m2, m3, m4, m5)

# Labels
cm <- c('identity'    = 'Identity tasks',
        'pub_identity'  = 'Public X Identity')

# Define goodness-of-fit statistics
gm <- list(
  list("raw" = "nobs", "clean" = "Sample Size", "fmt" = 0),
  list("raw" = "r.squared", "clean" = "R<sup>2</sup>", "fmt" = 2),
  list("raw" = "adj.r.squared", "clean" = "Adjusted R<sup>2</sup>", "fmt" = 2)
)
titletable4 <- 'Table 4: Caste inconsistency and refusal of all offers involving a task)'

modelsummary(models,
             coef_map = cm,
             gof_map = gm,
             stars = TRUE,
             title = titletable4,
             coef_omit = "i\\.task|old[2-7]|hiedu[2-7]|hiwealth[2-7]]",
             output = file.path(output, "table4.txt"))

# # # # # # 
# END 
# # # # # # 