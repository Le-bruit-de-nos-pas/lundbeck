
library(data.table)
library(tidyverse)
library(readxl)


REALIZE_FINALE <- read_excel("../data/REALIZE FINALE.xlsx", sheet = 1, col_types = "text")

prevent_cols <- c(
  "ERENUMAB...53", "FREMANEZUMAB...54", "GALCANEZUMAB...55", "EPTINEZUMAB...56",
  "TOXINE BOTULIQUE...57", "AMITRIPTYLINE...58", "VENLAFAXINE...59", "TOPIRAMATE...60",
  "VALPROATE SODIUM...61", "LAMOTRIGINE...62", "CANDESARTAN...63", "LISINOPRIL...64",
  "VERAPAMIL...65", "PROPRANOLOL...66", "METOPROLOL...67", "NIBEVOLOL...68",
  "ATENOLOL...69", "INFILTRATION NGO...70", "FLUNARIZINE...71", "OXETORONE...72",
  "PIZOTIFENE...73"
)

m0_prevent <- REALIZE_FINALE %>%
  filter(Visite == "M0") %>%
  select(all_of(prevent_cols))

all_values <- m0_prevent %>%
  pivot_longer(everything(), names_to = "col", values_to = "val") %>%
  filter(!is.na(val)) %>%
  pull(val) %>%
  unique() %>%
  sort()

print("Unique values across all preventive treatment columns at M0:")
print(all_values)

m0_drugs <- REALIZE_FINALE %>%
  filter(Visite == "M0") %>%
  select(all_of(prevent_cols))



m0_binary <- m0_drugs %>%
  mutate(across(everything(), ~ ifelse(
    !is.na(.) & !(. %in% c("0", "?", "ND")), 1, 0
  )))



drug_counts <- m0_binary %>%
  summarise(across(everything(), sum)) %>%
  pivot_longer(everything(), names_to = "Drug", values_to = "n") %>%
  mutate(percentage = n / nrow(m0_binary) * 100) %>%
  arrange(desc(percentage))

drug_counts <- drug_counts %>%
  mutate(Drug_clean = gsub("\\.\\.\\.[0-9]+$", "", Drug))

print(drug_counts)



prior_preventive_treat <- drug_counts

prior_preventive_treat <- prior_preventive_treat %>% select(Drug_clean, percentage) %>%
  rename("percentage_prior"="percentage")



current_cols <- c(
  "ERENUMAB...76", "FREMANEZUMAB...77", "GALCANEZUMAB...78", "EPTINEZUMAB...79",
  "TOXINE BOTULIQUE...80", "AMITRIPTYLINE...81", "VENLAFAXINE...82", "TOPIRAMATE...83",
  "VALPROATE SODIUM...84", "LAMOTRIGINE...85", "CANDESARTAN...86", "LISINOPRIL...87",
  "VERAPAMIL...88", "PROPRANOLOL...89", "METOPROLOL...90", "NIBEVOLOL...91",
  "ATENOLOL...92", "INFILTRATION NGO...93", "FLUNARIZINE...94", "OXETORONE...95",
  "PIZOTIFENE...96", "PREGABALINE"
)

m0_current <- REALIZE_FINALE %>%
  filter(Visite == "M0") %>%
  select(all_of(current_cols))


all_values <- m0_current %>%
  pivot_longer(everything(), names_to = "col", values_to = "val") %>%
  filter(!is.na(val)) %>%
  pull(val) %>%
  unique() %>%
  sort()

print("Unique values across all preventive treatment columns at M0:")
print(all_values)


m0_binary_current <- m0_current %>%
  mutate(across(everything(), ~ ifelse(
    !is.na(.) & !(. %in% c("0", "arret", "NI")), 1, 0
  )))



current_counts <- m0_binary_current %>%
  summarise(across(everything(), sum)) %>%
  pivot_longer(everything(), names_to = "Drug", values_to = "n") %>%
  mutate(percentage = n / nrow(m0_binary_current) * 100) %>%
  arrange(desc(percentage))

current_counts <- current_counts %>%
  mutate(Drug_clean = gsub("\\.\\.\\.[0-9]+$", "", Drug))

print(current_counts)


current_preventive_treat <- current_counts

current_preventive_treat <- current_preventive_treat %>% select(Drug_clean, percentage) %>%
  rename("percentage_current"="percentage")


all_drugs <- data.frame(prior_preventive_treat %>% full_join(current_preventive_treat)) %>%
  mutate(percentage_prior=ifelse(is.na(percentage_prior ), 0, percentage_prior )) %>%
  arrange(desc(percentage_prior + percentage_current)) 

all_drugs <- all_drugs %>%
  mutate(Drug_clean = case_when(
    Drug_clean == "AMITRIPTYLINE" ~ "Amitriptyline",
    Drug_clean == "EPTINEZUMAB" ~ "Eptinezumab",
    Drug_clean == "TOPIRAMATE" ~ "Topiramate",
    Drug_clean == "PROPRANOLOL" ~ "Propranolol",
    Drug_clean == "TOXINE BOTULIQUE" ~ "Botulinum toxin",
    Drug_clean == "OXETORONE" ~ "Oxetorone",
    Drug_clean == "CANDESARTAN" ~ "Candesartan",
    Drug_clean == "VENLAFAXINE" ~ "Venlafaxine",
    Drug_clean == "PIZOTIFENE" ~ "Pizotifen",
    Drug_clean == "VALPROATE SODIUM" ~ "Valproate sodium",
    Drug_clean == "METOPROLOL" ~ "Metoprolol",
    Drug_clean == "GALCANEZUMAB" ~ "Galcanezumab",
    Drug_clean == "FLUNARIZINE" ~ "Flunarizine",
    Drug_clean == "ERENUMAB" ~ "Erenumab",
    Drug_clean == "VERAPAMIL" ~ "Verapamil",
    Drug_clean == "LAMOTRIGINE" ~ "Lamotrigine",
    Drug_clean == "FREMANEZUMAB" ~ "Fremanezumab",
    Drug_clean == "INFILTRATION NGO" ~ "Infiltration NGO",
    Drug_clean == "ATENOLOL" ~ "Atenolol",
    Drug_clean == "NIBEVOLOL" ~ "Nebivolol",
    Drug_clean == "LISINOPRIL" ~ "Lisinopril",
    Drug_clean == "PREGABALINE" ~ "Pregabalin",
    TRUE ~ str_to_title(Drug_clean)  # fallback
  ))

all_drugs

plot_data <- all_drugs %>%
  pivot_longer(cols = c(percentage_prior, percentage_current), 
               names_to = "time", values_to = "percentage") %>%
  mutate(time = ifelse(time == "percentage_prior", "Prior", "Current"))

#         Drug_clean percentage_prior percentage_current
# 1     Amitriptyline       84.3612335         20.0440529
# 2       Eptinezumab        3.9647577         98.2378855
# 3        Topiramate       74.4493392          9.2511013
# 4       Propranolol       71.8061674          7.9295154
# 5   Botulinum toxin       54.4052863         14.5374449
# 6         Oxetorone       62.5550661          1.1013216
# 7       Candesartan       45.1541850         14.7577093
# 8       Venlafaxine       22.0264317          8.1497797
# 9         Pizotifen       25.5506608          0.6607930
# 10 Valproate sodium       20.2643172          3.3039648
# 11       Metoprolol       18.2819383          3.7444934
# 12     Galcanezumab       17.8414097          0.8810573
# 13      Flunarizine       11.4537445          0.0000000
# 14         Erenumab        8.1497797          0.0000000
# 15        Verapamil        5.7268722          1.7621145
# 16      Lamotrigine        5.5066079          1.7621145
# 17     Fremanezumab        5.2863436          0.4405286
# 18 Infiltration NGO        2.2026432          0.4405286
# 19         Atenolol        1.5418502          0.0000000
# 20        Nebivolol        0.8810573          0.4405286
# 21       Lisinopril        0.6607930          0.0000000
# 22       Pregabalin        0.0000000          0.2202643

plot <- ggplot(plot_data, aes(x = reorder(Drug_clean, -percentage), y = percentage, fill = time)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), alpha=0.8, width = 0.8) +
  geom_text(aes(label = sprintf("%.1f%%", percentage)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, size = 2.5, fontface = "bold", color = "black") +
  labs(title = "Prior vs. Current preventive treatments at baseline (M0, N=454)",
       x = NULL, y = "Percentage of patients \n", fill = "Status") +
  scale_fill_manual(values = c("Prior" = "#513055", "Current" = "#27343f")) +
  coord_cartesian(ylim = c(0, 105)) +
  facet_wrap(~ time, ncol = 1) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", size = 11),
    plot.title = element_text(face = "bold", size = 14),
    axis.title = element_text(face = "bold", size = 12),
    axis.text.x = element_text(face = "bold", size = 9, angle = 45, hjust = 1),
    axis.text.y = element_text(face = "bold", size = 9),
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(face = "bold", size = 10)
  )

plot
ggsave(file="../out/plot_prev_drugs_beforecurrent.svg", plot=plot, width=8, height=7)



current_acute_cols <- c(
  "ALMOTRIPTAN...111", "ELETRIPTAN...112", "FROVATRIPTAN...113", "NARATRIPTAN...114",
  "RIZATRIPTAN...115", "SUMATRIPTAN oral...116", "SUMATRIPTAN inj...117", "SUMATRIPTAN nasal...118",
  "ZOLMITRIPTAN...119", "AINS...120", "ASPIRINE...121", "PARACETAMOL...122",
  "OPIOIDE FAIBLE...123", "OPIOIDE FORT...124", "ERGOTAMINE...125", "DHE nasale...126",
  "NEFOPAM...127", "RIMEGEPANT...128"
)

m0_current_acute <- REALIZE_FINALE %>%
  filter(Visite == "M0") %>%
  select(all_of(current_acute_cols))


all_values <- m0_current_acute %>%
  pivot_longer(everything(), names_to = "col", values_to = "val") %>%
  filter(!is.na(val)) %>%
  pull(val) %>%
  unique() %>%
  sort()

print("Unique values across all preventive treatment columns at M0:")
print(all_values)

# Binary conversion (NA, "0", "arret", "NI" -> 0; else -> 1)
m0_binary_current_acute <- m0_current_acute %>%
  mutate(across(everything(), ~ ifelse(
    !is.na(.) & !(. %in% c("0", "arret", "NI")), 1, 0
  )))


current_acute_counts <- m0_binary_current_acute %>%
  summarise(across(everything(), sum)) %>%
  pivot_longer(everything(), names_to = "Drug", values_to = "n") %>%
  mutate(percentage = n / nrow(m0_binary_current_acute) * 100) %>%
  arrange(desc(percentage))

# Clean drug names (remove suffix "...XXX")
current_acute_counts <- current_acute_counts %>%
  mutate(Drug_clean = gsub("\\.\\.\\.[0-9]+$", "", Drug),
         Drug_clean = gsub("^SUMATRIPTAN", "Sumatriptan", Drug_clean),
         Drug_clean = gsub("ZOLMITRIPTAN", "Zolmitriptan", Drug_clean),
         Drug_clean = gsub("ALMOTRIPTAN", "Almotriptan", Drug_clean),
         Drug_clean = gsub("ELETRIPTAN", "Eletriptan", Drug_clean),
         Drug_clean = gsub("FROVATRIPTAN", "Frovatriptan", Drug_clean),
         Drug_clean = gsub("NARATRIPTAN", "Naratriptan", Drug_clean),
         Drug_clean = gsub("RIZATRIPTAN", "Rizatriptan", Drug_clean),
         Drug_clean = gsub("AINS", "NSAIDs", Drug_clean),
         Drug_clean = gsub("OPIOIDE FAIBLE", "Weak opioid", Drug_clean),
         Drug_clean = gsub("OPIOIDE FORT", "Strong opioid", Drug_clean),
         Drug_clean = gsub("DHE nasale", "DHE nasal", Drug_clean),
         Drug_clean = gsub("RIMEGEPANT", "Rimegepant", Drug_clean))

previous_acute_cols <- c(
  "ALMOTRIPTAN...130", "ELETRIPTAN...131", "FROVATRIPTAN...132", "NARATRIPTAN...133",
  "RIZATRIPTAN...134", "SUMATRIPTAN oral...135", "SUMATRIPTAN inj...136", "SUMATRIPTAN nasal...137",
  "ZOLMITRIPTAN...138", "AINS...139", "ASPIRINE...140", "PARACETAMOL...141",
  "OPIOIDE FAIBLE...142", "OPIOIDE FORT...143", "ERGOTAMINE...144", "DHE nasale...145",
  "NEFOPAM...146", "RIMEGEPANT...147", "OXYGENE", "AMITRIPTYLINE...149"
)

m0_previous_acute <- REALIZE_FINALE %>%
  filter(Visite == "M0") %>%
  select(all_of(previous_acute_cols))


all_values <- m0_previous_acute %>%
  pivot_longer(everything(), names_to = "col", values_to = "val") %>%
  filter(!is.na(val)) %>%
  pull(val) %>%
  unique() %>%
  sort()

print("Unique values across all preventive treatment columns at M0:")
print(all_values)



m0_binary_previous_acute <- m0_previous_acute %>%
  mutate(across(everything(), ~ ifelse(
    !is.na(.) & !(. %in% c("0", "?", "ND")), 1, 0
  )))


previous_acute_counts <- m0_binary_previous_acute %>%
  summarise(across(everything(), sum)) %>%
  pivot_longer(everything(), names_to = "Drug", values_to = "n") %>%
  mutate(percentage = n / nrow(m0_binary_previous_acute) * 100) %>%
  arrange(desc(percentage))

previous_acute_counts <- previous_acute_counts %>%
  mutate(Drug_clean = gsub("\\.\\.\\.[0-9]+$", "", Drug),
         Drug_clean = gsub("^SUMATRIPTAN", "Sumatriptan", Drug_clean),
         Drug_clean = gsub("ZOLMITRIPTAN", "Zolmitriptan", Drug_clean),
         Drug_clean = gsub("ALMOTRIPTAN", "Almotriptan", Drug_clean),
         Drug_clean = gsub("ELETRIPTAN", "Eletriptan", Drug_clean),
         Drug_clean = gsub("FROVATRIPTAN", "Frovatriptan", Drug_clean),
         Drug_clean = gsub("NARATRIPTAN", "Naratriptan", Drug_clean),
         Drug_clean = gsub("RIZATRIPTAN", "Rizatriptan", Drug_clean),
         Drug_clean = gsub("AINS", "NSAIDs", Drug_clean),
         Drug_clean = gsub("OPIOIDE FAIBLE", "Weak opioid", Drug_clean),
         Drug_clean = gsub("OPIOIDE FORT", "Strong opioid", Drug_clean),
         Drug_clean = gsub("DHE nasale", "DHE nasal", Drug_clean),
         Drug_clean = gsub("RIMEGEPANT", "Rimegepant", Drug_clean),
         Drug_clean = gsub("OXYGENE", "Oxygen", Drug_clean))

prior <- previous_acute_counts %>% select(Drug_clean, percentage) %>% rename(percentage_prior = percentage)
current <- current_acute_counts %>% select(Drug_clean, percentage) %>% rename(percentage_current = percentage)

all_acute <- full_join(prior, current, by = "Drug_clean") %>%
  mutate(across(starts_with("percentage"), ~ ifelse(is.na(.), 0, .))) %>%
  arrange(desc(percentage_prior + percentage_current))

all_acute <- all_acute %>%
  mutate(Drug_clean = case_when(
    Drug_clean == "NSAIDs" ~ "NSAIDs",
    Drug_clean == "DHE nasal" ~ "DHE nasal",
    Drug_clean == "PARACETAMOL" ~ "Paracetamol",
    Drug_clean == "ASPIRINE" ~ "Aspirin",
    Drug_clean == "NEFOPAM" ~ "Nefopam",
    Drug_clean == "ERGOTAMINE" ~ "Ergotamine",
    Drug_clean == "OXYGENE" ~ "Oxygen",
    Drug_clean == "AMITRIPTYLINE" ~ "Amitriptyline",
    TRUE ~ str_to_title(Drug_clean)
  ))

# 1 NSAIDs                      56.6               62.1  
#  2 Eletriptan                  39.2               40.1  
#  3 Paracetamol                 40.7               31.5  
#  4 Zolmitriptan                34.8               23.3  
#  5 Weak Opioid                 30.2               22.9  
#  6 Rizatriptan                 31.3               19.6  
#  7 Almotriptan                 25.1               12.1  
#  8 Aspirin                     19.8               16.1  
#  9 Sumatriptan Nasal           20.9               12.3  
# 10 Naratriptan                 15.9               10.1  
# 11 Sumatriptan Oral            16.3                9.25 
# 12 Nefopam                      9.47               9.25 
# 13 Frovatriptan                10.8                5.29 
# 14 Sumatriptan Inj              8.59               7.49 
# 15 Ergotamine                   5.95               4.41 
# 16 Strong Opioid                2.42               3.30 
# 17 DHE nasal                    2.86               2.42 
# 18 Rimegepant                   0.661              0.220
# 19 Oxygen                       0.441              0    
# 20 Amitriptyline                0.220              0    


plot_acute <- all_acute %>%
  pivot_longer(cols = c(percentage_prior, percentage_current),
               names_to = "time", values_to = "percentage") %>%
  mutate(time = ifelse(time == "percentage_prior", "Prior", "Current"))

# Plot
p_acute <- ggplot(plot_acute, aes(x = reorder(Drug_clean, -percentage), y = percentage, fill = time)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), alpha = 0.8, width = 0.8) +
  geom_text(aes(label = sprintf("%.1f%%", percentage)),
            position = position_dodge(width = 0.9),
            vjust = -0.5, size = 2.5, fontface = "bold", color = "black") +
  labs(title = "Prior vs. Current acute treatments at baseline (M0, N=454)",
       x = NULL, y = "Percentage of patients\n", fill = "Status") +
  scale_fill_manual(values = c("Prior" = "#513055", "Current" = "#27343f")) +
  coord_cartesian(ylim = c(0, 105)) +
  facet_wrap(~ time, ncol = 1) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", size = 11),
    plot.title = element_text(face = "bold", size = 14),
    axis.title = element_text(face = "bold", size = 12),
    axis.text.x = element_text(face = "bold", size = 9, angle = 45, hjust = 1),
    axis.text.y = element_text(face = "bold", size = 9),
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(face = "bold", size = 10)
  )

print(p_acute)
ggsave("../out/plot_acute_prior_current.svg", plot = p_acute, width = 8, height = 7)
