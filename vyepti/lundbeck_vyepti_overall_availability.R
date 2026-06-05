
library(data.table)
library(tidyverse)
library(readxl)


REALIZE_FINALE <- read_excel("../data/REALIZE FINALE.xlsx", sheet = 1, col_types = "text")

# SUMMARY DEMOGRAPHICS

n_unique <- length(unique(REALIZE_FINALE$Patient))
print(n_unique) # 454

REALIZE_FINALE %>% group_by(Sexe) %>% count()

# 1 F       375
# 2 H        49
# 3 M        29
# 4 NA     8911

plot <- REALIZE_FINALE %>%
  filter(!is.na(Sexe)) %>%
  mutate(Sexe = ifelse(Sexe == "F", "Female", "Male")) %>%
  group_by(Sexe) %>%
  count() %>% ungroup() %>%
  mutate(percentage = round(n / sum(n) * 100, 0)) %>%
  ggplot(aes(x = "", y = percentage, fill = Sexe)) +
  geom_bar(stat = "identity", width = 0.5, alpha=0.9, colour="white") +
  labs(x = NULL, y = "Gender percentage (%)\n", 
       title = "% cohort by gender") +
  scale_fill_manual(values = c("Female" = "#513055", "Male" = "#27343f")) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold", size = 16),
    axis.title = element_text(face = "bold", size = 14),
    axis.text = element_text(face = "bold", size = 12, color = "black"),
        legend.title = element_text(face = "bold", size = 12),  
        legend.text = element_text(face = "bold", size = 11), 
    legend.position = "top"
  ) +
  geom_text(aes(label = sprintf("%.0f%%", percentage)), 
            position = position_stack(vjust = 0.5), 
            size = 4, fontface = "bold", color = "white")

plot
ggsave(file="../out/plot_gender.svg", plot=plot, width=3.0, height=4)



data.frame(REALIZE_FINALE %>% filter(!is.na(Age))  %>%
             group_by(Age) %>% count() %>% ungroup() ) %>% summarise(tot=sum(n))

# 17 -92

REALIZE_FINALE %>% filter(!is.na(Age)) %>%
  mutate(Age=as.numeric(Age)) %>%
    summarise(mean=mean(Age),
              sd=sd(Age),
              median=median(Age),
              q1=quantile(Age, 0.25),
              q3=quantile(Age, 0.75))

# mean    sd median    q1    q3
#   <dbl> <dbl>  <dbl> <dbl> <dbl>
# 1  45.2  12.6     45    37    53


REALIZE_FINALE %>% filter(!is.na(Age)) %>%
  mutate(Age=as.numeric(Age)) %>%
  filter(!is.na(Sexe)) %>%
  mutate(Sexe = ifelse(Sexe == "F", "Female", "Male")) %>%
  group_by(Sexe) %>%
    summarise(mean=mean(Age),
              sd=sd(Age),
              median=median(Age),
              q1=quantile(Age, 0.25),
              q3=quantile(Age, 0.75))

# Sexe    mean    sd median    q1    q3
#   <chr>  <dbl> <dbl>  <dbl> <dbl> <dbl>
# 1 Female  45.1  12.4     45    37    53
# 2 Male    45.2  13.5     45    38    53


plot <- REALIZE_FINALE %>% filter(!is.na(Age)) %>%
  mutate(Age=as.numeric(Age)) %>%
  filter(!is.na(Sexe)) %>%
  mutate(Sexe = ifelse(Sexe == "F", "Female", "Male")) %>%
  ggplot(aes(x = factor(Sexe), y = Age, fill = factor(Sexe))) +
  coord_cartesian(ylim=c(15, 90)) +
  geom_violin( trim = FALSE, alpha = 0.5, colour="white") +
  geom_boxplot( width = 0.15, outlier.shape = NA, notch = TRUE, alpha = 0.8, colour="white") +
labs(title = "Age distribution by gender", 
       x = "\n Gender", 
       y = "Age (years) \n",
       fill = "Gender") +
  scale_fill_manual(values = c("Female" = "#513055", "Male" = "#27343f")) +
  theme_minimal() +
  theme(panel.grid = element_blank(),   
        plot.title = element_text(face = "bold", size = 16),
        axis.title = element_text(face = "bold", size = 14),
        axis.text = element_text(face = "bold", size = 12, color = "black"),
        legend.title = element_text(face = "bold", size = 12),  
        legend.text = element_text(face = "bold", size = 11))


ggsave(file="../out/plot_age.svg", plot=plot, width=4, height=4)



# WITH ANY EVAL

cols_of_interest <- c("HAD A", "HAD D", "HAD TOTAL", "EQ-5D 3L", "EQ-5D %", 
                      "MIDAS", "MIDAS en jours", "HIT-6 VA", "HIT-6 >60")

filtered_data <- REALIZE_FINALE %>%
  filter(rowSums(!is.na(select(., all_of(cols_of_interest)))) > 0)


unique(filtered_data$Visite)

# Count unique patients in the filtered data
n_unique_filtered <- length(unique(filtered_data$Patient))
print(n_unique_filtered) # 454


# VISITS PER PATIENT

cols <- c("HAD A", "HAD D", "HAD TOTAL", "EQ-5D 3L", "EQ-5D %", 
          "MIDAS", "MIDAS en jours", "HIT-6 VA", "HIT-6 >60")

visits <- c("M0", "M3", "M6", "M9", "M12", "M15", "M18", "M21")

# Function to count unique patients per visit with at least one non-NA in cols
count_patients_per_visit <- function(visit) {
  data_visit <- REALIZE_FINALE %>% filter(Visite == visit)
  # Check for each row if any of cols is not NA
  has_data <- apply(data_visit[, cols], 1, function(row) any(!is.na(row)))
  unique_patients <- unique(data_visit$Patient[has_data])
  return(length(unique_patients))
}

# Apply for each visit
result <- sapply(visits, count_patients_per_visit)
names(result) <- visits
print(result)

#  M0  M3  M6  M9 M12 M15 M18 M21 
# 451 442 361 299 255 201 173  23 


plot_data <- data.frame(
  Visit = factor(names(result), levels = names(result)),
  Patients = result
)

plot <-  ggplot(plot_data, aes(x = Visit, y = Patients)) +
  geom_col(fill = "#27343f", width = 0.7) +
  geom_text(aes(label = Patients), vjust = -0.5, size = 3.5, fontface = "bold") +
  labs(title = "Number of patients with clinical data per visit",
       x = "\n Visit",
       y = "# of patients with\n clinical evaluation \n") +
  coord_cartesian(ylim=c(0,500)) +
  theme_minimal() +
  theme(panel.grid = element_blank(),   
        plot.title = element_text(face = "bold", size = 16),
        axis.title = element_text(face = "bold", size = 14),
        axis.text = element_text(face = "bold", size = 12, color = "black"),
        legend.title = element_text(face = "bold", size = 12),  
        legend.text = element_text(face = "bold", size = 11))

plot
ggsave(file="../out/number_of_pats_with_each_visit.svg", plot=plot, width=5, height=4)




has_data_for_visit <- function(patient_id, visit) {
  dat <- REALIZE_FINALE %>% filter(Patient == patient_id, Visite == visit)
  if(nrow(dat) == 0) return(FALSE)
  any(!is.na(dat[, cols]))
}

patients <- unique(REALIZE_FINALE$Patient)

visit_counts <- sapply(patients, function(pt) {
  sum(sapply(visits, function(v) has_data_for_visit(pt, v)))
})

freq_table <- table(visit_counts)
print(freq_table)

plot_df <- data.frame(
  n_visits = as.numeric(names(freq_table)),
  n_patients = as.numeric(freq_table)
)

plot <- ggplot(plot_df, aes(x = factor(n_visits), y = n_patients)) +
  geom_col(fill = "#27343f", width = 0.7) +
  geom_text(aes(label = n_patients), vjust = -0.5, size = 3.5, fontface = "bold") +
  labs(title = "Number of visits per patient\nwith clinical data\n",
       x = "\n# of visits with data",
       y = "# of patients \n") +
  coord_cartesian(ylim=c(0,160)) +
  theme_minimal() +
  theme(panel.grid = element_blank(),   
        plot.title = element_text(face = "bold", size = 16),
        axis.title = element_text(face = "bold", size = 14),
        axis.text = element_text(face = "bold", size = 12, color = "black"),
        legend.title = element_text(face = "bold", size = 12),  
        legend.text = element_text(face = "bold", size = 11))

plot
ggsave(file="../out/number_of_visits_per_pat.svg", plot=plot, width=5, height=4)


visits <- c("M0", "M3", "M6", "M9", "M12", "M15", "M18", "M21")
n_patients <- c(451, 442, 361, 299, 255, 201, 173, 23)

retention <- data.frame(
  Visit = factor(visits, levels = visits),
  N = n_patients,
  Proportion = n_patients / n_patients[1]  # relative to M0
)

plot <-  ggplot(retention, aes(x = as.numeric(Visit), y = Proportion)) +
  geom_step(size = 2.2, alpha=0.7 , color = "#513055") +
  geom_point(size = 3, shape=1, stroke=2, color = "#27343f") +
  scale_x_continuous(breaks = 1:length(visits), labels = visits) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
  labs(title = "Patient retention curve\nwith clinical data",
       x = "\n Visit",
       y = "Retention rate\n% of patients still with data\n") +
  theme_minimal() +
  theme(panel.grid = element_blank(),   
        plot.title = element_text(face = "bold", size = 16),
        axis.title = element_text(face = "bold", size = 14),
        axis.text = element_text(face = "bold", size = 12, color = "black"),
        legend.title = element_text(face = "bold", size = 12),  
        legend.text = element_text(face = "bold", size = 11))

plot
ggsave(file="../out/step_retention_curve.svg", plot=plot, width=5, height=4)





# For each row, mark if any of the cols is non-NA
REALIZE_FINALE$has_data <- apply(REALIZE_FINALE[, cols], 1, function(x) any(!is.na(x)))


# Filter rows that have data and are in the relevant visits
valid <- REALIZE_FINALE[REALIZE_FINALE$Visite %in% visits & REALIZE_FINALE$has_data == TRUE, ]

# Create a list of visits per patient
patient_visits <- valid %>%
  group_by(Patient) %>%
  summarise(Visits = list(sort(Visite)), .groups = "drop")

# View first few rows
head(patient_visits)

# If you prefer a comma‑separated string instead of a list:
patient_visits_str <- valid %>%
  group_by(Patient) %>%
  summarise(Visits = paste(sort(Visite), collapse = ", "), .groups = "drop")

fwrite(patient_visits_str, "../out/patient_visits_str.csv")


side_effects <- data.frame(
  SideEffect = c("Rhinitis / rhinopharyngitis", "Constipation (including worsening)", 
                 "Fatigue (general, post-injection, etc.)", "Asthenia (weakness, tiredness)",
                 "Erythema / skin redness", "Pruritus (itching, laryngeal, oropharyngeal)",
                 "Alopecia (hair loss)", "Sore throat / throat irritation", 
                 "Cough (dry cough, cough)", "Allergic reaction (with cough/oppression)",
                 "Asthma attack", "Ecchymosis (bruising)", "Flu-like syndrome",
                 "Laryngeal edema sensation", "Malaise (vasovagal)", 
                 "Menstrual cycle disorder", "Migraine worsening", "Muscle cramps (legs)",
                 "Nasal congestion", "Nasal discharge", "Psoriasis", 
                 "Status migrainosus", "Tinnitus", "Weight gain"),
  Count = c(11, 10, 9, 8, 3, 3, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
)

side_effects <- side_effects %>%
  arrange(desc(Count)) %>%
  mutate(SideEffect = factor(SideEffect, levels = rev(SideEffect)))  #

plot <- ggplot(side_effects, aes(x = SideEffect, y = Count)) +
  geom_col(fill = "#513055", width = 0.7) +
  coord_flip() +  
  geom_text(aes(label = Count), hjust = -0.3, size = 3.5, fontface = "bold") +
  labs(title = "Frequency of reported \nSigns|Symptoms|Adverse Effects",
       x = NULL,
       y = "\n Sign|Symptom count") +
  theme_minimal() +
 scale_y_continuous(limits = c(0, 15), breaks = seq(0, 15, by = 5)) +
  theme(panel.grid = element_blank(),   
        plot.title = element_text(face = "bold", size = 16),
        axis.title = element_text(face = "bold", size = 14),
        axis.text = element_text(face = "bold", size = 12, color = "black"),
        legend.title = element_text(face = "bold", size = 12),  
        legend.text = element_text(face = "bold", size = 11))

plot

ggsave(file="../out/adverseeffects.svg", plot=plot, width=8, height=6)


# MAP centers

centers <- REALIZE_FINALE %>% select(Patient) %>% distinct() %>% mutate(Patient=str_sub(Patient, 1L, 1L))

unique(centers$Patient)

library(tidyverse)
library(sf)
library(ggrepel)

centers <- REALIZE_FINALE %>%
  select(Patient) %>%
  distinct() %>%
  mutate(center_code = str_sub(Patient, 1, 1)) %>%
  group_by(center_code) %>%
  summarise(n_patients = n(), .groups = "drop")

center_map <- tribble(
  ~center_code, ~city,
  "1", "toulouse",
  "2", "lille",
  "3", "lille",
  "4", "rouen",
  "5", "nantes",
  "6", "clermontferrand",
  "7", "montpellier"
)

df_map <- centers %>%
  left_join(center_map, by = "center_code") %>%
  group_by(city) %>%
  summarise(Patients = sum(n_patients), .groups = "drop") %>%
  mutate(city = as.character(city))

france_dept <- st_read("https://raw.githubusercontent.com/gregoiredavid/france-geojson/master/departements.geojson")

city_to_dept <- tribble(
  ~city, ~code,
  "limoges", "87",
  "lyon", "69",
  "lille", "59",
  "paris", "75",
  "marseille", "13",
  "montpellier", "34",
  "toulouse", "31",
  "poitiers", "86",
  "creteil", "94",
  "rouen", "76",
  "nice", "06",
  "strasbourg", "67",
  "aix", "13",
  "caen", "14",
  "nancy", "54",
  "reims", "51",
  "amiens", "80",
  "nantes", "44",
  "rennes", "35",
  "bordeaux", "33",
  "besancon", "25",
  "nmes", "30",
  "dijon", "21",
  "clermontferrand", "63",
  "avicenne", "93"
)

df_map <- df_map %>% left_join(city_to_dept, by = "city")

france_data <- france_dept %>%
  left_join(df_map, by = c("code" = "code"))

total_patients <- sum(df_map$Patients)

labeled_depts <- france_data %>%
  filter(!is.na(Patients)) %>%
  mutate(
    centroid = st_centroid(geometry),
    lon = st_coordinates(centroid)[, 1],
    lat = st_coordinates(centroid)[, 2],
    pct = round(Patients / total_patients * 100, 1),
    label = paste0(str_to_title(city), " ", round(pct,0) , "%\n[n=", Patients, "]")
  )

plot <- ggplot(france_data) +
  geom_sf(aes(fill = Patients), color = "white") +
  scale_fill_gradient(low = "#C4E0FF", high = "#27343F", na.value = "#F5F5F5") +
  ggrepel::geom_label_repel(
    data = labeled_depts,
    aes(x = lon, y = lat, label = label),
    size = 3, min.segment.length = 0,
    box.padding = 0.3,
    fontface = "bold"               # bold labels
  ) +
  labs(
    title = "Number of patients per center",
    fill = "Patients"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    panel.grid = element_blank(),
    panel.background = element_blank(),
    plot.background = element_blank(),
    strip.background = element_blank(),
    strip.text = element_blank(),
    plot.margin = margin(5, 5, 5, 5, "pt"),
    plot.title = element_text(face = "bold", size = 14),       # bold title
    legend.title = element_text(face = "bold", size = 12),     # bold legend title
    legend.text = element_text(face = "bold", size = 10)       # bold legend text
  )

print(plot)

ggsave("../out/patients_per_center_map.svg", plot = plot, width = 5, height = 5, device = "svg")












# WITH PERFUSION


cols_of_interest <- c("HAD A", "HAD D", "HAD TOTAL", "EQ-5D 3L", "EQ-5D %", 
          "MIDAS", "MIDAS en jours", "HIT-6 VA", "HIT-6 >60", 
          "Céphalées", "Migraine"  , "Traitements" , "Surconsommation",
          "PAS pré", "PAD pré", "PAS post", "PAD post")

filtered_data <- REALIZE_FINALE %>%
  filter(rowSums(!is.na(select(., all_of(cols_of_interest)))) > 0)

unique(filtered_data$Visite)

# Count unique patients in the filtered data
n_unique_filtered <- length(unique(filtered_data$Patient))
print(n_unique_filtered) # 454


# VISITS PER PATIENT

cols <-c("HAD A", "HAD D", "HAD TOTAL", "EQ-5D 3L", "EQ-5D %", 
          "MIDAS", "MIDAS en jours", "HIT-6 VA", "HIT-6 >60", 
          "Céphalées", "Migraine"  , "Traitements" , "Surconsommation",
          "PAS pré", "PAD pré", "PAS post", "PAD post")

visits <- c("M0", "M3", "M6", "M9", "M12", "M15", "M18", "M21")

# Function to count unique patients per visit with at least one non-NA in cols
count_patients_per_visit <- function(visit) {
  data_visit <- REALIZE_FINALE %>% filter(Visite == visit)
  # Check for each row if any of cols is not NA
  has_data <- apply(data_visit[, cols], 1, function(row) any(!is.na(row)))
  unique_patients <- unique(data_visit$Patient[has_data])
  return(length(unique_patients))
}

# Apply for each visit
result <- sapply(visits, count_patients_per_visit)
names(result) <- visits
print(result)

#  M0  M3  M6  M9 M12 M15 M18 M21 
# 453 446 366 304 259 201 174  24 


plot_data <- data.frame(
  Visit = factor(names(result), levels = names(result)),
  Patients = result
)

plot <-  ggplot(plot_data, aes(x = Visit, y = Patients)) +
  geom_col(fill = "#27343f", width = 0.7) +
  geom_text(aes(label = Patients), vjust = -0.5, size = 3.5, fontface = "bold") +
  labs(title = "Number of patients with clinical data per visit",
       x = "\n Visit",
       y = "# of patients with\n clinical evaluation \n") +
  coord_cartesian(ylim=c(0,500)) +
  theme_minimal() +
  theme(panel.grid = element_blank(),   
        plot.title = element_text(face = "bold", size = 16),
        axis.title = element_text(face = "bold", size = 14),
        axis.text = element_text(face = "bold", size = 12, color = "black"),
        legend.title = element_text(face = "bold", size = 12),  
        legend.text = element_text(face = "bold", size = 11))

plot
ggsave(file="../out/number_of_pats_with_each_visit.svg", plot=plot, width=5, height=4)




has_data_for_visit <- function(patient_id, visit) {
  dat <- REALIZE_FINALE %>% filter(Patient == patient_id, Visite == visit)
  if(nrow(dat) == 0) return(FALSE)
  any(!is.na(dat[, cols]))
}

patients <- unique(REALIZE_FINALE$Patient)

visit_counts <- sapply(patients, function(pt) {
  sum(sapply(visits, function(v) has_data_for_visit(pt, v)))
})

freq_table <- table(visit_counts)
print(freq_table)

plot_df <- data.frame(
  n_visits = as.numeric(names(freq_table)),
  n_patients = as.numeric(freq_table)
)

plot <- ggplot(plot_df, aes(x = factor(n_visits), y = n_patients)) +
  geom_col(fill = "#27343f", width = 0.7) +
  geom_text(aes(label = n_patients), vjust = -0.5, size = 3.5, fontface = "bold") +
  labs(title = "Number of visits per patient\nwith clinical data\n",
       x = "\n# of visits with data",
       y = "# of patients \n") +
  coord_cartesian(ylim=c(0,160)) +
  theme_minimal() +
  theme(panel.grid = element_blank(),   
        plot.title = element_text(face = "bold", size = 16),
        axis.title = element_text(face = "bold", size = 14),
        axis.text = element_text(face = "bold", size = 12, color = "black"),
        legend.title = element_text(face = "bold", size = 12),  
        legend.text = element_text(face = "bold", size = 11))

plot
ggsave(file="../out/number_of_visits_per_pat.svg", plot=plot, width=5, height=4)


visits <- c("M0", "M3", "M6", "M9", "M12", "M15", "M18", "M21")
n_patients <- c(453, 446, 366, 304, 259, 201, 174, 24)


retention <- data.frame(
  Visit = factor(visits, levels = visits),
  N = n_patients,
  Proportion = n_patients / n_patients[1]  # relative to M0
)

plot <-  ggplot(retention, aes(x = as.numeric(Visit), y = Proportion)) +
  geom_step(size = 2.2, alpha=0.7 , color = "#513055") +
  geom_point(size = 3, shape=1, stroke=2, color = "#27343f") +
  scale_x_continuous(breaks = 1:length(visits), labels = visits) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
  labs(title = "Patient retention curve\nwith clinical data",
       x = "\n Visit",
       y = "Retention rate\n% of patients still with data\n") +
  theme_minimal() +
  theme(panel.grid = element_blank(),   
        plot.title = element_text(face = "bold", size = 16),
        axis.title = element_text(face = "bold", size = 14),
        axis.text = element_text(face = "bold", size = 12, color = "black"),
        legend.title = element_text(face = "bold", size = 12),  
        legend.text = element_text(face = "bold", size = 11))

plot
ggsave(file="../out/step_retention_curve.svg", plot=plot, width=5, height=4)


