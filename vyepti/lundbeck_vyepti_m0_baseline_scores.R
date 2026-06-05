
library(data.table)
library(tidyverse)
library(readxl)


REALIZE_FINALE <- read_excel("../data/REALIZE FINALE.xlsx", sheet = 1, col_types = "text")

vars <- c("HAD A", "HAD D", "HAD TOTAL", "EQ-5D 3L", "EQ-5D %", 
          "MIDAS", "MIDAS en jours", "HIT-6 VA", "HIT-6 >60", 
          "Céphalées", "Migraine", "Traitements", "Triptans", 
          "Surconsommation", "m-TOQ tot", "m-TOQ Q1", "m-TOQ Q2", 
          "m-TOQ Q3", "m-TOQ Q4", "m-TOQ Q5", "m-TOQ Q6", 
          "WPAI Absenteisme", "WPAI Baisse productivité", 
          "WPAI Altération globale travail", "WPAI Altération activités", 
          "PGIC", "MIBS-4", "PAS pré", "PAD pré", "PAS post", "PAD post",
          "Céphalée lors de la perfusion", "Céphalée post perfusion",
          "ENA pré perfusion", "ENA post perfusion")

m0_data <- REALIZE_FINALE %>%
  filter(Visite == "M0") %>%
  select(all_of(vars))



summary_stats <- function(x) {
  data.frame(
    n = sum(!is.na(x)),
    mean = mean(x, na.rm = TRUE),
    sd = sd(x, na.rm = TRUE),
    median = median(x, na.rm = TRUE),
    q1 = quantile(x, 0.25, na.rm = TRUE),
    q3 = quantile(x, 0.75, na.rm = TRUE)
  )
}

data.frame(m0_data %>% select(`HAD TOTAL`) %>% distinct())

vars <- c("HAD A", "HAD D", "HAD TOTAL")

m0_had  <- m0_data %>%
  select(all_of(vars)) %>%
  mutate(across(everything(), as.numeric))

results <- lapply(m0_had , summary_stats) %>% bind_rows(.id = "Variable")
results

#          Variable   n      mean       sd median q1    q3
# 25%...1     HAD A 445  9.350562 4.455312      9  6 12.00
# 25%...2     HAD D 445  7.116854 4.418920      7  4 10.00
# 25%...3 HAD TOTAL 448 16.502232 7.768173     16 11 21.25

m0_had_long <- m0_had %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Score") %>%
  filter(!is.na(Score))

plot <- ggplot(m0_had_long, aes(x = Variable, y = Score, fill = Variable)) +
  geom_violin(trim = FALSE, alpha = 0.3, colour = "white") +
  geom_boxplot(width = 0.15, outlier.shape = NA, notch = TRUE, 
               alpha = 0.7, colour = "white") +
  labs(title = "HAD baseline distribution (M0)",
       x = NULL,
       y = "HAD Score \n") +
  scale_fill_manual(values = c("HAD A" = "#513055", 
                               "HAD D" = "#27343f", 
                               "HAD TOTAL" = "#2c7fb8")) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        plot.title = element_text(face = "bold", size = 14),
        axis.title = element_text(face = "bold", size = 12),
        axis.text = element_text(face = "bold", size = 10, color = "black"),
        legend.position = "none")  # remove legend because fill already identifies variables

plot

ggsave(file="../out/plot_had.svg", plot=plot, width=4, height=4)





m0_eq5d <- m0_data %>%
  select("EQ-5D 3L", "EQ-5D %") %>%
  mutate(across(everything(), as.numeric))

results <- lapply(m0_eq5d , summary_stats) %>% bind_rows(.id = "Variable")
results

#       Variable   n       mean         sd median    q1   q3
# 25%...1 EQ-5D 3L 164  0.6457134  0.2513639  0.715  0.49  0.8
# 25%...2  EQ-5D % 163 56.6134969 20.3012270 60.000 40.00 70.0

m0_long <- m0_eq5d %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Value") %>%
  mutate(Value_scaled = ifelse(Variable == "EQ-5D %", Value / 100, Value))

plot <- ggplot(m0_long, aes(x = Variable, y = Value_scaled, fill = Variable)) +
  geom_violin(trim = FALSE, alpha = 0.3, colour = "white", position = position_dodge(0.9)) +
  geom_boxplot(width = 0.15, outlier.shape = NA, notch = TRUE, alpha = 0.7,
               colour = "white", position = position_dodge(0.9)) +
  scale_y_continuous(
    name = "EQ-5D 3L (utility)\n",
    sec.axis = sec_axis(~ . * 100, name = "EQ-5D % (0–100)\n")
  ) +
  scale_fill_manual(values = c("EQ-5D 3L" = "#27343f", "EQ-5D %" = "#513055")) +
  labs(title = "EQ-5D at baseline (M0)", x = NULL) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold", size = 14),
    axis.title = element_text(face = "bold", size = 12),
    axis.text = element_text(face = "bold", size = 10, color = "black"),
    axis.title.y.left = element_text(color = "#513055"),
    axis.title.y.right = element_text(color = "#27343f"),
    legend.position = "none"
  )

plot
ggsave(file="../out/plot_eq5d.svg", plot=plot, width=4, height=4)





m0_midas <- m0_data %>%
  select(MIDAS, `MIDAS en jours`) %>%
  mutate(across(everything(), as.numeric))

results <- lapply(m0_midas, summary_stats) %>%
  bind_rows(.id = "Variable")

print(results)

#               Variable   n      mean         sd median    q1  q3
# 25%...1          MIDAS 251  3.633466  0.8252972      4  4.00   4
# 25%...2 MIDAS en jours 250 71.830000 67.9493381     47 24.25 100


m0_long <- m0_midas %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Value") %>%
  mutate(Value_scaled = ifelse(Variable == "MIDAS en jours", Value / 100, Value))

plot <- m0_long %>% mutate(Variable=ifelse(Variable=="MIDAS en jours", "MIDAS (days)", Variable)) %>%
  ggplot(aes(x = Variable, y = Value_scaled, fill = Variable)) +
  geom_violin(trim = FALSE, alpha = 0.3, colour = "white", position = position_dodge(0.9)) +
  geom_boxplot(width = 0.15, outlier.shape = NA, notch = TRUE, alpha = 0.7,
               colour = "white", position = position_dodge(0.9)) +
  scale_y_continuous(
    name = "MIDAS\n",
    sec.axis = sec_axis(~ . * 100, name = "MIDAS (days)\n")
  ) +
  scale_fill_manual(values = c("MIDAS" = "#513055", "MIDAS en jours" = "#27343f")) +
  labs(title = "MIDAS at baseline (M0)", x = NULL) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold", size = 14),
    axis.title = element_text(face = "bold", size = 12),
    axis.text = element_text(face = "bold", size = 10, color = "black"),
    axis.title.y.left = element_text(color = "#513055"),
    axis.title.y.right = element_text(color = "#27343f"),
    legend.position = "none"
  )

plot

ggsave(file="../out/plot_midas.svg", plot=plot, width=4, height=4)





library(dplyr)

m0_hit6 <- m0_data %>%
  select("HIT-6 VA", "HIT-6 >60") %>%
  mutate(`HIT-6 VA` = as.numeric(`HIT-6 VA`) ) %>%
  mutate(`HIT-6 >60` = ifelse(`HIT-6 >60`=="O", "1",
                              ifelse(`HIT-6 >60`=="N",0,`HIT-6 >60`))) %>%
  mutate(`HIT-6 >60` =as.numeric( `HIT-6 >60`))

hit6_cont <- m0_hit6$`HIT-6 VA`
cont_stats <- data.frame(
  Variable = "HIT-6 VA",
  n = sum(!is.na(hit6_cont)),
  mean = mean(hit6_cont, na.rm = TRUE),
  sd = sd(hit6_cont, na.rm = TRUE),
  median = median(hit6_cont, na.rm = TRUE),
  q1 = quantile(hit6_cont, 0.25, na.rm = TRUE),
  q3 = quantile(hit6_cont, 0.75, na.rm = TRUE)
)

hit6_bin <- m0_hit6$`HIT-6 >60`
bin_stats <- data.frame(
  Variable = "HIT-6 >60 (proportion)",
  n = sum(!is.na(hit6_bin)),
  proportion = mean(hit6_bin, na.rm = TRUE),
  ci_lower = prop.test(sum(hit6_bin, na.rm = TRUE), sum(!is.na(hit6_bin)))$conf.int[1],
  ci_upper = prop.test(sum(hit6_bin, na.rm = TRUE), sum(!is.na(hit6_bin)))$conf.int[2]
)

print(cont_stats)

#    Variable   n     mean       sd median q1 q3
# 25% HIT-6 VA 448 66.51116 5.942669     67 64 70

print(bin_stats)

#                 Variable   n proportion  ci_lower  ci_upper
# 1 HIT-6 >60 (proportion) 448  0.8995536 0.8669922 0.9250518




p1 <- ggplot(m0_hit6, aes(x = "HIT-6 VA", y = `HIT-6 VA`)) +
  geom_violin(trim = FALSE, alpha = 0.3, fill = "#513055", colour = "white") +
  geom_boxplot(width = 0.15, outlier.shape = NA, notch = TRUE, 
               alpha = 0.7, fill = "#513055", colour = "white") +
  labs(title = "HIT-6 VA at baseline (M0)",
       x = NULL, y = "HIT-6 VA score\n") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        plot.title = element_text(face = "bold", size = 14),
        axis.title = element_text(face = "bold", size = 12),
        axis.text = element_text(face = "bold", size = 10, color = "black"),
        axis.text.x = element_text(face = "bold", size = 10))

print(p1)

ggsave(file="../out/plot_hit6cont.svg", plot=p1, width=3, height=4)


# Create a binary factor with labels
m0_binary <- m0_hit6 %>%
  mutate(Status = factor(`HIT-6 >60`, levels = c(0, 1), labels = c("≤60", ">60"))) %>%
  drop_na(Status) %>%
  count(Status) %>%
  mutate(prop = n / sum(n))

p2 <- ggplot(m0_binary, aes(x = "", y = prop, fill = Status)) +
  geom_bar(stat = "identity", width = 0.5, alpha = 0.8) +
  geom_text(aes(label = scales::percent(prop, accuracy = 1)), 
            position = position_stack(vjust = 0.5), size = 4, fontface = "bold", color = "white") +
  scale_fill_manual(values = c("≤60" = "#27343f", ">60" = "#513055")) +
  labs(title = "% HIT-6 >60 at baseline (M0)",
       x = NULL, y = NULL) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        plot.title = element_text(face = "bold", size = 14),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),   # removes y-axis numbers
        axis.ticks = element_blank(),    # removes all ticks
        legend.title = element_text(face = "bold", size = 12),
        legend.text = element_text(face = "bold", size = 10),
        legend.position = "top")

print(p2)

ggsave(file="../out/plot_hit6bin.svg", plot=p2, width=3, height=4)




m0_tx <- m0_data %>%
  select(Céphalées, Migraine, Traitements, Triptans) %>%
  mutate(across(everything(), as.numeric))    %>%
  rename(
    "Headache" = Céphalées,
    "Migraine" = Migraine,
    "Treatment" = Traitements,
    "Triptan" = Triptans
  )

results <- lapply(m0_tx, summary_stats) %>%
  bind_rows(.id = "Variable")

print(results)

#            Variable   n     mean       sd median q1 q3
# 25%...1   Céphalées 443 20.67306 8.505212     20 14 30
# 25%...2    Migraine 444 16.42905 7.445389     15 11 20
# 25%...3 Traitements 411 15.55191 8.732420     14  9 20
# 25%...4    Triptans 410 12.65650 8.901738     12  7 17


m0_long <- m0_tx %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Days") %>%
  filter(!is.na(Days))

plot <- ggplot(m0_long, aes(x = Variable, y = Days, fill = Variable)) +
  geom_violin(trim = FALSE, alpha = 0.3, colour = "white") +
  geom_boxplot(width = 0.15, outlier.shape = NA, notch = TRUE, alpha = 0.7, colour = "white") +
  scale_fill_manual(values = c("Headache" = "#27343f", 
                               "Migraine" = "#2c7fb8", 
                               "Treatment" = "#513055", 
                               "Triptan" = "#114e5b")) +
  labs(title = "Monthly days at baseline (M0)",
       x = NULL, y = "Days per month\n") +
  coord_cartesian(ylim=c(0,35)) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        plot.title = element_text(face = "bold", size = 14),
        axis.title = element_text(face = "bold", size = 12),
        axis.text = element_text(face = "bold", size = 10, color = "black"),
        legend.position = "none")

plot
ggsave(file="../out/plot_migtreatdaysmonth.svg", plot=plot, width=5, height=4)





m0_surconsommation <- m0_data %>% select(Surconsommation) %>% 
  mutate(Surconsommation=ifelse(Surconsommation=="N", 0,1)) %>%
  drop_na()


bin_stats <- data.frame(
  Variable = "Overuse",
  n = sum(!is.na(m0_surconsommation$Surconsommation)),
  proportion = mean(m0_surconsommation$Surconsommation, na.rm = TRUE),
  ci_lower = prop.test(sum(m0_surconsommation$Surconsommation, na.rm = TRUE), sum(!is.na(m0_surconsommation$Surconsommation)))$conf.int[1],
  ci_upper = prop.test(sum(m0_surconsommation$Surconsommation, na.rm = TRUE), sum(!is.na(m0_surconsommation$Surconsommation)))$conf.int[2]
)


print(bin_stats)

#  Variable   n proportion  ci_lower  ci_upper
# 1  Overuse 403  0.6898263 0.6417727 0.7341972



# Create a binary factor with labels
m0_binary <- m0_surconsommation %>%
  mutate(Status = factor(Surconsommation, levels = c(0, 1), labels = c("No overuse", "Overuse"))) %>%
  drop_na(Status) %>%
  count(Status) %>%
  mutate(prop = n / sum(n))

p2 <- ggplot(m0_binary, aes(x = "", y = prop, fill = Status)) +
  geom_bar(stat = "identity", width = 0.5, alpha = 0.8) +
  geom_text(aes(label = scales::percent(prop, accuracy = 1)), 
            position = position_stack(vjust = 0.5), size = 4, fontface = "bold", color = "white") +
  scale_fill_manual(values = c("No overuse" = "#27343f", "Overuse" = "#513055")) +
  labs(title = "% Overuse at baseline (M0)",
       x = NULL, y = NULL) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        plot.title = element_text(face = "bold", size = 14),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),   # removes y-axis numbers
        axis.ticks = element_blank(),    # removes all ticks
        legend.title = element_text(face = "bold", size = 12),
        legend.text = element_text(face = "bold", size = 10),
        legend.position = "top")

print(p2)

ggsave(file="../out/plot_overusebin.svg", plot=p2, width=3, height=4)



mtoq_df <- m0_data %>%
  select(`m-TOQ tot`, `m-TOQ Q1`, `m-TOQ Q2`, `m-TOQ Q3`, 
         `m-TOQ Q4`, `m-TOQ Q5`, `m-TOQ Q6`) %>%
  mutate(across(everything(), as.numeric))

summary_stats <- function(x) {
  data.frame(
    n = sum(!is.na(x)),
    mean = mean(x, na.rm = TRUE),
    sd = sd(x, na.rm = TRUE),
    median = median(x, na.rm = TRUE),
    q1 = quantile(x, 0.25, na.rm = TRUE),
    q3 = quantile(x, 0.75, na.rm = TRUE)
  )
}

total_stats <- summary_stats(mtoq_df$`m-TOQ tot`) %>%
  mutate(Variable = "m-TOQ total", .before = 1)

item_stats <- lapply(mtoq_df[,2:7], summary_stats) %>%
  bind_rows(.id = "Variable")

all_stats <- bind_rows(total_stats, item_stats)
print(all_stats)


#            Variable   n      mean        sd median   q1 q3
# 25%...1 m-TOQ total 167 16.664671 5.4159634     18 13.5 21
# 25%...2    m-TOQ Q1 162  2.888889 1.0863352      3  2.0  4
# 25%...3    m-TOQ Q2 163  2.711656 1.1796836      3  2.0  4
# 25%...4    m-TOQ Q3 163  2.423313 1.0823825      2  2.0  3
# 25%...5    m-TOQ Q4 163  3.570552 0.7111921      4  3.0  4
# 25%...6    m-TOQ Q5 161  3.080745 1.1290215      4  2.0  4
# 25%...7    m-TOQ Q6 161  2.416149 1.0871925      2  2.0  3


p_total <- ggplot(mtoq_df, aes(x = "m-TOQ total", y = `m-TOQ tot`)) +
  geom_violin(trim = FALSE, alpha = 0.3, fill = "#513055", colour = "white") +
  geom_boxplot(width = 0.15, outlier.shape = NA, notch = TRUE, 
               alpha = 0.7, fill = "#513055", colour = "white") +
  labs(title = "m-TOQ at baseline (M0)",
       x = NULL, y = "m-TOQ Total score\n") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        plot.title = element_text(face = "bold", size = 14),
        axis.title = element_text(face = "bold", size = 12),
        axis.text = element_text(face = "bold", size = 10, color = "black"),
        axis.text.x = element_text(face = "bold", size = 10))

print(p_total)

ggsave(file="../out/plot_mtoqtot.svg", plot=p_total, width=4, height=4)

items_long <- mtoq_df %>%
  select(`m-TOQ Q1`:`m-TOQ Q6`) %>%
  pivot_longer(everything(), names_to = "Item", values_to = "Score") %>%
  filter(!is.na(Score))

items_long <- items_long %>%
  mutate(Item = gsub("m-TOQ ", "", Item))

p_items <- ggplot(items_long, aes(x = Item, y = Score, fill = Item)) +
  geom_violin(trim = FALSE, alpha = 0.3, colour = "white", fill="#27343f") +
  geom_boxplot(width = 0.15, outlier.shape = NA, notch = TRUE, alpha = 0.7, fill="#513055", colour = "white") +
  labs(title = "m-TOQ individual items at baseline (M0)",
       x = NULL, y = "Individual Score \n") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        plot.title = element_text(face = "bold", size = 14),
        axis.title = element_text(face = "bold", size = 12),
        axis.text = element_text(face = "bold", size = 10, color = "black"),
        axis.text.x = element_text(angle = 0, hjust = 1, face = "bold"),
        legend.position = "none")

print(p_items)

ggsave(file="../out/plot_mtoqitems.svg", plot=p_items, width=5, height=4)


library(dplyr)

mibs <- m0_data %>%
  select(`MIBS-4`) %>%  
  mutate(across(everything(), as.numeric))

mibs_stats <- summary_stats(mibs[[1]])
print(mibs_stats)

#      n     mean       sd median q1 q3
# 25% 199 5.949749 3.809542      6  3  9

p <- ggplot(mibs, aes(x = "MIBS-4", y = `MIBS-4`)) +
  geom_violin(trim = FALSE, alpha = 0.3, fill = "#513055", colour = "white") +
  geom_boxplot(width = 0.15, outlier.shape = NA, notch = TRUE, 
               alpha = 0.7, fill = "#513055", colour = "white") +
  labs(title = "MIBS-4 at baseline (M0)",
       x = NULL, y = "MIBS-4 Score\n ") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        plot.title = element_text(face = "bold", size = 14),
        axis.title = element_text(face = "bold", size = 12),
        axis.text = element_text(face = "bold", size = 10, color = "black"))

print(p)

ggsave(file="../out/plot_mibs4.svg", plot=p, width=3, height=4)





wpai_df <- m0_data %>%
  select("WPAI Absenteisme", "WPAI Baisse productivité", 
         "WPAI Altération globale travail", "WPAI Altération activités") %>%
  mutate(across(everything(), as.numeric))

wpai_df <- wpai_df %>%
  rename(
    Absenteeism = "WPAI Absenteisme",
    `Productivity decline` = "WPAI Baisse productivité",
    `Global work impair` = "WPAI Altération globale travail",
    `Activity altrations` = "WPAI Altération activités"
  )



results <- lapply(wpai_df, summary_stats) %>%
  bind_rows(.id = "Variable")

print(results)

#                     Variable   n      mean        sd median  q1     q3
# 25%...1          Absenteeism 100 0.0778000 0.2033238    0.0 0.0 0.0325
# 25%...2 Productivity decline  96 0.4708333 0.2247416    0.5 0.3 0.6000
# 25%...3   Global work impair  96 0.5089583 0.2570213    0.5 0.3 0.7000
# 25%...4  Activity altrations 159 0.5584906 0.2470847    0.6 0.4 0.7000



wpai_long <- wpai_df %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Proportion") %>%
  filter(!is.na(Proportion))

plot <- ggplot(wpai_long, aes(x = Variable, y = Proportion, fill = Variable)) +
  geom_violin(trim = FALSE, alpha = 0.3, colour = "white", fill="#27343f") +
  geom_boxplot(width = 0.15, outlier.shape = NA, notch = TRUE, fill = "#513055", alpha = 0.7, colour = "white") +

  labs(title = "WPAI at baseline (M0)",
       x = NULL, y = "% impairment\n") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        plot.title = element_text(face = "bold", size = 14),
        axis.title = element_text(face = "bold", size = 12),
        axis.text = element_text(face = "bold", size = 10, color = "black"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

ggsave(file="../out/plot_wpaiitems.svg", plot=plot, width=4, height=4)


perf <- m0_data %>%
  select(`Céphalée lors de la perfusion`, `Céphalée post perfusion`) %>%
   mutate(across(everything(), as.numeric))




get_prop_ci <- function(x) {
  x_clean <- x[!is.na(x)]
  n_yes <- sum(x_clean == 1)
  n_total <- length(x_clean)
  prop <- n_yes / n_total
  ci <- binom.test(n_yes, n_total)$conf.int
  return(c(prop = prop, lower = ci[1], upper = ci[2], n = n_total))
}

res1 <- get_prop_ci(perf[[1]])
res2 <- get_prop_ci(perf[[2]])





get_prop <- function(x) {
  x_clean <- x[!is.na(x)]
  mean(x_clean, na.rm = TRUE)
}

prop1 <- get_prop(perf[[1]])
prop2 <- get_prop(perf[[2]])

# Create data frames for plotting (only Yes proportion, No is implied)
plot_df1 <- data.frame(Status = "Yes", prop = prop1)
plot_df2 <- data.frame(Status = "Yes", prop = prop2)

# Function for stacked bar (only Yes segment, No segment is absent because we only show the proportion of Yes)
# To get a full bar that sums to 1, we need a dummy "No" category. But the overuse plot had two categories.
# Actually the overuse plot had two categories: Overuse and No overuse. 
# For consistency, we'll create two categories: Yes and No, with proportions.
plot_stacked <- function(prop, title) {
  plot_df <- data.frame(
    Status = c("Yes", "No"),
    prop = c(prop, 1 - prop)
  )
  
  ggplot(plot_df, aes(x = "", y = prop, fill = Status)) +
    geom_bar(stat = "identity", width = 0.5, alpha = 0.8) +
    geom_text(aes(label = scales::percent(prop, accuracy = 1)), 
              position = position_stack(vjust = 0.5), size = 4, fontface = "bold", color = "white") +
    scale_fill_manual(values = c("Yes" = "#513055", "No" = "#27343f")) +
    labs(title = title, x = NULL, y = NULL) +
    theme_minimal() +
    theme(panel.grid = element_blank(),
          plot.title = element_text(face = "bold", size = 12),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          legend.title = element_text(face = "bold", size = 10),
          legend.text = element_text(face = "bold", size = 9),
          legend.position = "top")
}

p1 <- plot_stacked(prop1, "Headache during infusion")
p2 <- plot_stacked(prop2, "Headache after infusion")


ggsave(file="../out/plot_headacheduring.svg", plot=p1, width=3, height=4)
ggsave(file="../out/plot_headacheafter.svg", plot=p2, width=3, height=4)




ena_df <- m0_data %>%
  select(`ENA pré perfusion`, `ENA post perfusion`) %>%
  mutate(across(everything(), as.numeric))

stats_pre <- summary_stats(ena_df$`ENA pré perfusion`) %>%
  mutate(Variable = "ENA pre", .before = 1)
stats_post <- summary_stats(ena_df$`ENA post perfusion`) %>%
  mutate(Variable = "ENA post", .before = 1)

stats <- bind_rows(stats_pre, stats_post)
print(stats)

#       Variable   n     mean       sd median q1 q3
# 25%...1  ENA pre 175 1.245714 2.093276      0  0  2
# 25%...2 ENA post 158 1.006329 1.979181      0  0  1


ena_long <- ena_df %>%
  pivot_longer(everything(), names_to = "Time", values_to = "Score") %>%
  filter(!is.na(Score))

ena_long <- ena_long %>%
  mutate(Time = ifelse(Time == "ENA pré perfusion", "A) Pre-infusion", "B) Post-infusion"))

plot <- ggplot(ena_long, aes(x = Time, y = Score, fill = Time)) +
  geom_violin(trim = FALSE, alpha = 0.3, colour = "white") +
  geom_boxplot(width = 0.15, outlier.shape = NA, notch = TRUE, alpha = 0.7, colour = "white") +
  scale_fill_manual(values = c("A) Pre-infusion" = "#513055", "B) Post-infusion" = "#27343f")) +
  labs(title = "ENA before vs. after (M0)",
       x = NULL, y = "ENA Score\n") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        plot.title = element_text(face = "bold", size = 14),
        axis.title = element_text(face = "bold", size = 12),
        axis.text = element_text(face = "bold", size = 10, color = "black"),
        legend.position = "none")

plot

ggsave(file="../out/plot_ena.svg", plot=plot, width=3, height=4)








bp_df <- m0_data %>%
  select("PAS pré", "PAD pré", "PAS post", "PAD post") %>%
  mutate(across(everything(), as.numeric))

bp_df <- bp_df %>%
  rename(
    SBP_pre = "PAS pré",
    DBP_pre = "PAD pré",
    SBP_post = "PAS post",
    DBP_post = "PAD post"
  )




max(bp_df$DBP_pre,na.rm=T)


bp_df <- bp_df %>% mutate(DBP_pre=ifelse(DBP_pre==8384, 84, DBP_pre))

results <- lapply(bp_df, summary_stats) %>%
  bind_rows(.id = "Variable")

print(results)


#         Variable   n      mean       sd median     q1  q3
# 25%...1  SBP_pre 368 122.14402 16.30344    120 110.00 132
# 25%...2  DBP_pre 368  73.56793 11.87518     74  65.75  80
# 25%...3 SBP_post 177 117.92090 15.24998    116 108.00 128
# 25%...4 DBP_post 177  69.88701 11.09457     69  62.00  77



bp_long <- bp_df %>%
  pivot_longer(everything(), names_to = "Measurement", values_to = "mmHg") %>%
  filter(!is.na(mmHg))

bp_long$Measurement <- factor(bp_long$Measurement, 
                              levels = c("SBP_pre", "DBP_pre", "SBP_post", "DBP_post"))

plot <- ggplot(bp_long, aes(x = Measurement, y = mmHg, fill = Measurement)) +
  geom_violin(trim = FALSE, alpha = 0.3, colour = "white") +
  geom_boxplot(width = 0.15, outlier.shape = NA, notch = TRUE, alpha = 0.7, colour = "white") +
  scale_fill_manual(values = c("SBP_pre" = "#513055", "DBP_pre" = "#27343f",
                               "SBP_post" = "#513055", "DBP_post" = "#27343f")) +
  labs(title = "BP before vs. after infusion (M0)",
       x = NULL, y = "mmHg \n") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        plot.title = element_text(face = "bold", size = 14),
        axis.title = element_text(face = "bold", size = 12),
        axis.text = element_text(face = "bold", size = 10, color = "black"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

plot


ggsave(file="../out/plot_bps.svg", plot=plot, width=4, height=4)
