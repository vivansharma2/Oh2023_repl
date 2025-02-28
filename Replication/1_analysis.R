# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
#
# PROJECT:		 Replication of "Does Identity Affect Labor Supply?" (Oh, 2023)
# REPLICATORS: Vivan Sharma, Rodrigo Pereira
# TASK:				 Data Analysis Part-1 (Table 2, 3 & Figure 3)
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

df <- read_dta(paste0(cleandata, "choice_jobtakeup_analysis_replicated.dta"))

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
  #		Figure 3: Reasons for turning down job offers
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# Replace Inf and -Inf with NAs
df[df == -Inf | df == Inf] <- NA

df <- df %>%
  group_by(pid, task) %>%
  mutate(any_no = min(takeup, na.rm = TRUE),
         any_yes = max(takeup, na.rm = TRUE)) %>%
  ungroup()

# Check
table(df$any_yes[df$tag_pidtask == 1], df$agree[df$tag_pidtask == 1], useNA = "ifany")
df$all_no <- ifelse(is.na(df$any_yes) | is.na(df$agree), 0, (df$any_yes == 0 & df$agree == 0)) # refused to do the task and answered survey

# Reason for refusal of task - creating indicators
for (i in 1:8) {
  df[[paste0("temp", i)]] <- rowSums(df[, paste0("reason", 1:6, "_not")] == i, na.rm = TRUE) > 0
  df[[paste0("refuse_reason", i)]] <- ifelse(df$all_no == 1, df[[paste0("temp", i)]], NA)
  df[[paste0("temp", i)]] <- NULL
}

df$temp9 <- rowSums(df[, paste0("reason", 1:6, "_not")] == -97, na.rm = TRUE) > 0
df$refuse_reason9 <- ifelse(df$all_no == 1, df$temp9, NA)
df$temp9 <- NULL  

df$refuse_iden <- ifelse(df$identity == 1 & df$all_no == 1, 0, NA)
df$refuse_iden <- ifelse(!is.na(df$refuse_iden) & df$refuse_iden == 0 & 
                           (df$refuse_reason1 == 1 | df$refuse_reason2 == 1 | df$refuse_reason3 == 1), 1, df$refuse_iden)

df$refuse_social <- ifelse(df$identity == 1 & df$all_no == 1, 0, NA)
df$refuse_social <- ifelse(!is.na(df$refuse_social) & df$refuse_social == 0 & 
                             (df$refuse_reason4 == 1 | df$refuse_reason5 == 1), 1, df$refuse_social)

df$refuse_skill <- ifelse(df$identity == 1 & df$all_no == 1, 0, NA)
df$refuse_skill <- ifelse(!is.na(df$refuse_skill) & df$refuse_skill == 0 & 
                            (df$refuse_reason6 == 1 | df$refuse_reason7 == 1 | df$refuse_reason8 == 1), 1, df$refuse_skill)

df$refuse_iden_cont <- ifelse(df$pairedcont == 1 & df$all_no == 1, 0, NA)
df$refuse_iden_cont <- ifelse(!is.na(df$refuse_iden_cont) & df$refuse_iden_cont == 0 & 
                                (df$refuse_reason1 == 1 | df$refuse_reason2 == 1 | df$refuse_reason3 == 1), 1, df$refuse_iden_cont)

df$refuse_social_cont <- ifelse(df$pairedcont == 1 & df$all_no == 1, 0, NA)
df$refuse_social_cont <- ifelse(!is.na(df$refuse_social_cont) & df$refuse_social_cont == 0 & 
                                  (df$refuse_reason4 == 1 | df$refuse_reason5 == 1), 1, df$refuse_social_cont)

df$refuse_skill_cont <- ifelse(df$pairedcont == 1 & df$all_no == 1, 0, NA)
df$refuse_skill_cont <- ifelse(!is.na(df$refuse_skill_cont) & df$refuse_skill_cont == 0 & 
                                 (df$refuse_reason6 == 1 | df$refuse_reason7 == 1 | df$refuse_reason8 == 1), 1, df$refuse_skill_cont)

# Identity group
df$reason_both <- ifelse(df$tag_pidtask == 1 & df$identity == 1 & df$all_no == 1, 
                         df$refuse_iden == 1 & df$refuse_social == 1, NA)

df$reason_onlyiden <- ifelse(df$tag_pidtask == 1 & df$identity == 1 & df$all_no == 1, 
                             df$refuse_iden == 1 & df$refuse_social == 0, NA)

df$reason_onlysocial <- ifelse(df$tag_pidtask == 1 & df$identity == 1 & df$all_no == 1, 
                               df$refuse_iden == 0 & df$refuse_social == 1, NA)

df$reason_neither <- ifelse(df$tag_pidtask == 1 & df$identity == 1 & df$all_no == 1, 
                            df$refuse_iden == 0 & df$refuse_social == 0, NA)

# Paired control group 
df$reason_both_cont <- ifelse(df$tag_pidtask == 1 & df$pairedcont == 1 & df$all_no == 1, 
                              df$refuse_iden_cont == 1 & df$refuse_social_cont == 1, NA)

df$reason_onlyiden_cont <- ifelse(df$tag_pidtask == 1 & df$pairedcont == 1 & df$all_no == 1, 
                                  df$refuse_iden_cont == 1 & df$refuse_social_cont == 0, NA)

df$reason_onlysocial_cont <- ifelse(df$tag_pidtask == 1 & df$pairedcont == 1 & df$all_no == 1, 
                                    df$refuse_iden_cont == 0 & df$refuse_social_cont == 1, NA)

df$reason_neither_cont <- ifelse(df$tag_pidtask == 1 & df$pairedcont == 1 & df$all_no == 1, 
                                 df$refuse_iden_cont == 0 & df$refuse_social_cont == 0, NA)

# Collapse with means, sd, counts
dfcol <- df %>%
  summarise(
    mean1 = mean(reason_both, na.rm = TRUE),
    mean2 = mean(reason_onlyiden, na.rm = TRUE),
    mean3 = mean(reason_onlysocial, na.rm = TRUE),
    mean4 = mean(reason_neither, na.rm = TRUE),
    mean5 = mean(reason_both_cont, na.rm = TRUE),
    mean6 = mean(reason_onlyiden_cont, na.rm = TRUE),
    mean7 = mean(reason_onlysocial_cont, na.rm = TRUE),
    mean8 = mean(reason_neither_cont, na.rm = TRUE),
    
    sd1 = sd(reason_both, na.rm = TRUE),
    sd2 = sd(reason_onlyiden, na.rm = TRUE),
    sd3 = sd(reason_onlysocial, na.rm = TRUE),
    sd4 = sd(reason_neither, na.rm = TRUE),
    sd5 = sd(reason_both_cont, na.rm = TRUE),
    sd6 = sd(reason_onlyiden_cont, na.rm = TRUE),
    sd7 = sd(reason_onlysocial_cont, na.rm = TRUE),
    sd8 = sd(reason_neither_cont, na.rm = TRUE),
    
    n1 = sum(!is.na(reason_both)),
    n2 = sum(!is.na(reason_onlyiden)),
    n3 = sum(!is.na(reason_onlysocial)),
    n4 = sum(!is.na(reason_neither)),
    n5 = sum(!is.na(reason_both_cont)),
    n6 = sum(!is.na(reason_onlyiden_cont)),
    n7 = sum(!is.na(reason_onlysocial_cont)),
    n8 = sum(!is.na(reason_neither_cont))
  )

dfcol <- dfcol %>% mutate(cat = row_number()) 

# Reshape wide to long
dfcol <- dfcol %>%
  pivot_longer(cols = starts_with(c("mean", "sd", "n")), 
               names_to = c(".value", "order"), 
               names_pattern = "([a-z]+)(\\d+)") %>%
  mutate(order = as.numeric(order))

dfcol$identity <- ifelse(dfcol$order <= 4, 1, 0)
dfcol$order <- ifelse(dfcol$identity == 0, dfcol$order-4, dfcol$order)

dfcol$order2 <- dfcol$order + 0.08
dfcol$mean2 <- dfcol$mean + 0.02

dfcol<- dfcol %>%
  mutate(barlabel = round(mean, 2))

# CI
dfcol <- dfcol %>%
  mutate(hiz = mean + 1.96 * (sd / sqrt(n)),
         loz = mean - 1.96 * (sd / sqrt(n)))

### 3-A Paired Control Tasks
colors <- c("blue", "green", "red", "gray")

p3a <- ggplot(dfcol %>% filter(identity == 0), aes(x = factor(order), y = mean, fill = factor(order))) +
  geom_bar(stat = "identity", width = 0.7, alpha = 0.9) + 
  scale_fill_manual(values = colors) +  
  geom_errorbar(aes(ymin = loz, ymax = hiz), width = 0.2, color = "black") +  
  geom_text(aes(label = round(barlabel, 2), y = hiz + 0.02), vjust = 0, size = 6, color = "black") +
  scale_y_continuous(limits = c(0, 0.7), breaks = seq(0, 0.6, by = 0.2)) + 
  labs(y = "Share of answers", x = "", title = "Paired control tasks") +  
  theme_minimal(base_size = 16) +  
  theme(
    legend.position = "none",  
    plot.title = element_text(size = 24, hjust = 0.5),  
    axis.text.x = element_text(size = 18, margin = margin(t = 10))  
  ) +
  scale_x_discrete(labels = c(
    "1" = "Identity &\nSocial Image",
    "2" = "Identity\nOnly",
    "3" = "Social\nImage Only",
    "4" = "Neither"
  ))  # Custom X-axis labels

print(p3a)

### 3-B Identity Tasks
p3b <- ggplot(dfcol %>% filter(identity == 1), aes(x = factor(order), y = mean, fill = factor(order))) +
  geom_bar(stat = "identity", width = 0.7, alpha = 0.9) +  
  scale_fill_manual(values = colors) +  
  geom_errorbar(aes(ymin = loz, ymax = hiz), width = 0.2, color = "black") +  
  geom_text(aes(label = round(barlabel, 2), y = hiz + 0.02),  
            vjust = 0, size = 6, color = "black") + 
  scale_y_continuous(limits = c(0, 0.65), breaks = seq(0, 0.6, by = 0.2)) +  
  labs(y = "Share of answers", x = "", title = "Identity tasks") +  
  theme_minimal(base_size = 16) +  
  theme(
    legend.position = "none",  
    plot.title = element_text(size = 24, hjust = 0.5),  
    axis.text.x = element_text(size = 18, margin = margin(t = 10))  
  ) +
  scale_x_discrete(labels = c(
    "1" = "Identity &\nSocial Image",
    "2" = "Identity\nOnly",
    "3" = "Social\nImage Only",
    "4" = "Neither"
  ))  

print(p3b)

# Combine and export
fig3 <- p3a | p3b
ggsave(file.path(output, "Figure3.png"), plot = fig3, width = 16, height = 8)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
#		Table 2: Identity inconsistency and job offer take-up
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

dft2 <- df[df$purecont != 1, ]

# Run the regressions
# Caste FE
m1 <- feols(takeup ~ iden_diftask + iden_lowertask + diftask + lowertask + i(task) + timehr | caste, 
            cluster = ~pid, data = dft2)

# Worker FE
m2 <- feols(takeup ~ iden_diftask + iden_lowertask + diftask + lowertask + i(task) + timehr | pid, 
            cluster = ~pid, data = dft2)

# Follow up survey
m3 <- feols(takeup ~ iden_diftask + iden_lowertask + diftask + lowertask + i(task) + timehr | pid, 
            cluster = ~pid, data = dft2 %>% filter(survey_completed == 1))

# Demographic controls (Linear)
m4 <- feols(takeup ~ iden_diftask + iden_lowertask + diftask + lowertask + 
                 i(task) + timehr + age3 + age4 + age5 + age6 + age7 + age9 + age10 + age11 + 
                 year_edu3 + year_edu4 + year_edu5 + year_edu6 + year_edu7 + year_edu9 + year_edu10 + year_edu11 + 
                 wealth_pca3 + wealth_pca4 + wealth_pca5 + wealth_pca6 + wealth_pca7 + wealth_pca9 + wealth_pca10 + wealth_pca11 | 
                 pid,  
               cluster = ~pid, 
               data = dft2)
# Demographic controls (binary)
m5 <- feols(takeup ~ iden_diftask + iden_lowertask + diftask + lowertask + 
              i(task) + timehr + old3 + old4 + old5 + old6 + old7 + old9 + old10 + old11 + 
              hiedu3 + hiedu4 + hiedu5 + hiedu6 + hiedu7 + hiedu9 + hiedu10 + hiedu11 + 
              hiwealth3 + hiwealth4 + hiwealth5 + hiwealth6 + hiwealth7 + hiwealth9 + hiwealth10 + hiwealth11 | 
              pid,  
            cluster = ~pid, 
            data = dft2)

# Create a list 
models <- list(m1, m2, m3, m4, m5)

# Labels
cm <- c('iden_diftask'    = 'Identity × Different',
        'iden_lowertask'  = 'Identity × Lower',
        'diftask'         = 'Different tasks',
        'lowertask'       = 'Lower tasks',
        'timehr'          = 'Hours on extra tasks')

# Define goodness-of-fit statistics
gm <- list(
  list("raw" = "nobs", "clean" = "Sample Size", "fmt" = 0),
  list("raw" = "r.squared", "clean" = "R<sup>2</sup>", "fmt" = 2),
  list("raw" = "adj.r.squared", "clean" = "Adjusted R<sup>2</sup>", "fmt" = 2)
)
titletable2 <- 'Table 2 — Identity Inconsistency and Job Offer Take-Up)'

modelsummary(models,
             coef_map = cm,
             gof_map = gm,
             stars = TRUE,
             title = titletable2,
             coef_omit = "i\\.task|year_edu[3-9]|age[3-9]|wealth_pca[3-9]|old[3-9]|hiedu[3-9]|hiwealth[3-9]",
             output = file.path(output, "table2.txt"))

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
#		Table 3: Role of Social Image Concerns
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# Run regressions on same filtered dataset
m6 <- feols(takeup ~ iden_diftask + iden_lowertask + diftask + lowertask + pub_iden_diftask 
            + pub_iden_lowertask + pub_diftask + pub_lowertask + pub_identity + public 
            + timehr + i(task) + timehr | caste, 
            cluster = ~pid, data = dft2)

# Worker FE
m7 <- feols(takeup ~ iden_diftask + iden_lowertask + diftask + lowertask + pub_iden_diftask 
            + pub_iden_lowertask + pub_diftask + pub_lowertask + pub_identity 
            + timehr + i(task) + timehr |pid, 
           cluster = ~pid, data = dft2)

# Follow up survey
m8 <- feols(takeup ~ iden_diftask + iden_lowertask + diftask + lowertask + pub_iden_diftask 
            + pub_iden_lowertask + pub_diftask + pub_lowertask + pub_identity 
            + timehr + i(task) + timehr |pid, 
            cluster = ~pid, data = dft2 %>% filter(survey_completed == 1))

# Demographic controls (Linear)
m9 <- feols(takeup ~ iden_diftask + iden_lowertask + pub_iden_diftask + pub_iden_lowertask 
            + diftask + lowertask + pub_diftask + pub_lowertask + pub_identity  
            + timehr + i(task) + timehr + age3 + age4 + age5 + age6 + age7 + age9 
            + age10 + age11 + year_edu3 + year_edu4 
            + year_edu5 + year_edu6 + year_edu7 + year_edu9 + year_edu10 + year_edu11 
            + wealth_pca3 + wealth_pca4 + wealth_pca5 + wealth_pca6 + wealth_pca7 
            + wealth_pca9 + wealth_pca10 + wealth_pca11 | pid,  
            cluster = ~pid, 
            data = dft2)

# Demographic controls (binary)
m10 <-feols(takeup ~ iden_diftask + iden_lowertask + diftask + lowertask + pub_iden_diftask 
            + pub_iden_lowertask + pub_diftask + pub_lowertask + pub_identity 
            + timehr + i(task) + timehr  
            + old3 + old4 + old5 + old6 + old7 + old9 + old10 + old11 
            + hiedu3 + hiedu4 + hiedu5 + hiedu6 + hiedu7 + hiedu9 + hiedu10 + hiedu11 
            + hiwealth3 + hiwealth4 + hiwealth5 + hiwealth6 + hiwealth7 + hiwealth9 + hiwealth10 + hiwealth11 | pid,   
            cluster = ~pid, 
            data = dft2)

# Create a list 
models1 <- list(m6, m7, m8, m9, m10)

# Labels
cm1 <- c('iden_diftask'    = 'Identity × Different',
        'iden_lowertask'  = 'Identity × Lower',
        'pub_iden_diftask'         = 'Public × Identity × Different',
        'pub_iden_lowertask'       = 'Public × Identity × Lowers',
        'timehr'          = 'Hours on extra tasks')

# Define goodness-of-fit statistics
gm1 <- list(
  list("raw" = "nobs", "clean" = "Sample Size", "fmt" = 0),
  list("raw" = "r.squared", "clean" = "R<sup>2</sup>", "fmt" = 2),
  list("raw" = "adj.r.squared", "clean" = "Adjusted R<sup>2</sup>", "fmt" = 2)
)

titletable3 <- 'Table 3 — Role of Social Image Concerns'

modelsummary(models1,
             coef_map = cm1,
             gof_map = gm1,
             stars = TRUE,
             title = titletable3,
             coef_omit = "i\\.task|year_edu[3-9]|age[3-9]|wealth_pca[3-9]|old[3-9]|hiedu[3-9]|hiwealth[3-9]",
             output = file.path(output, "table3.txt"))

# # # # # # 
# END 
# # # # # # 


