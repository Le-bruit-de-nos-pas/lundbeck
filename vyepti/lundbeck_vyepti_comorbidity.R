
library(data.table)
library(tidyverse)
library(readxl)


REALIZE_FINALE <- read_excel("../data/REALIZE FINALE.xlsx", sheet = 1, col_types = "text")

comorb_cols <- c("AVC", "Allergie", "Rhinite allergique", "Anxiété", "Arthrose", 
                 "Asthme", "Adénome prostate", "Cancer", "Cervicalgie/Lombalgie", 
                 "Colite inflammatoire", "COVID-19", "Dépression", 
                 "Dissection artérielle vertébrale", "Douleurs neuropathiques", 
                 "Insuf respi", "Insuf cardiaque", "Insuf coronarienne", "IDM", 
                 "Diabète", "Dyslipidémie", "Dysthyroidie", "Endométriose", 
                 "Epilepsie", "Fibromyalgie", "Gastrite", "Glaucome", 
                 "Hernie hiatale", "HTA", "Maladie foie", "Maladie rein", 
                 "Rhumatisme infla", "Trouble ATM", "Trouble sommeil", 
                 "Trouble bipolaire", "Patho psychiatrique", "Trouble dig fonctionnel", 
                 "Ulcère gastrique", "Syndrome myofacial", "Surpoids", "FOP", 
                 "Sjogren", "MAV", "Patho cutanée")

patients_unique <- REALIZE_FINALE %>%
  distinct(Patient) %>%
  pull(Patient)   # 454 patients

m0_comorb <- REALIZE_FINALE %>%
  filter(Visite == "M0") %>%
  select(all_of(comorb_cols))

get_unique <- function(col) {
  vals <- unique(col[!is.na(col)])
  if(length(vals) == 0) return(character(0))
  sort(vals)
}

# Apply to each column
unique_vals <- map(m0_comorb, get_unique)

# Print results
for(col in names(unique_vals)) {
  cat(col, ":\n")
  print(unique_vals[[col]])
  cat("\n")
}

m0_comorb_binary <- m0_comorb %>%
  mutate(across(everything(), ~ ifelse(is.na(.) | . == "0", 0,1)) )

sum(m0_comorb_binary$AVC)
         
         
comorb_counts <- m0_comorb_binary %>%
  summarise(across(everything(), sum)) %>%
  pivot_longer(everything(), names_to = "Comorbidity", values_to = "n") %>%
  mutate(percentage = n / nrow(m0_comorb_binary) * 100) %>%
  arrange(desc(percentage))

comorb_counts <- comorb_counts %>%
  mutate(
    Comorbidity_eng = case_when(
      Comorbidity == "Anxiété" ~ "Anxiety",
      Comorbidity == "Trouble sommeil" ~ "Sleep disorder",
      Comorbidity == "Cervicalgie/Lombalgie" ~ "Neck/back pain",
      Comorbidity == "COVID-19" ~ "COVID-19",
      Comorbidity == "Dépression" ~ "Depression",
      Comorbidity == "Allergie" ~ "Allergy",
      Comorbidity == "Arthrose" ~ "Osteoarthritis",
      Comorbidity == "Rhinite allergique" ~ "Allergic rhinitis",
      Comorbidity == "Surpoids" ~ "Overweight",
      Comorbidity == "Trouble dig fonctionnel" ~ "Functional digestive disorder",
      Comorbidity == "Asthme" ~ "Asthma",
      Comorbidity == "Gastrite" ~ "Gastritis",
      Comorbidity == "Endométriose" ~ "Endometriosis",
      Comorbidity == "Fibromyalgie" ~ "Fibromyalgia",
      Comorbidity == "Dysthyroidie" ~ "Thyroid disorder",
      Comorbidity == "Trouble ATM" ~ "TMD (jaw joint)",
      Comorbidity == "HTA" ~ "Hypertension",
      Comorbidity == "Cancer" ~ "Cancer",
      Comorbidity == "Dyslipidémie" ~ "Dyslipidemia",
      Comorbidity == "Hernie hiatale" ~ "Hiatal hernia",
      Comorbidity == "Ulcère gastrique" ~ "Gastric ulcer",
      Comorbidity == "Maladie rein" ~ "Kidney disease",
      Comorbidity == "Patho psychiatrique" ~ "Psychiatric disorder",
      Comorbidity == "Rhumatisme infla" ~ "Inflammatory rheumatism",
      Comorbidity == "Colite inflammatoire" ~ "Inflammatory colitis",
      Comorbidity == "Diabète" ~ "Diabetes",
      Comorbidity == "Insuf respi" ~ "Respiratory insufficiency",
      Comorbidity == "Epilepsie" ~ "Epilepsy",
      Comorbidity == "Douleurs neuropathiques" ~ "Neuropathic pain",
      Comorbidity == "Trouble bipolaire" ~ "Bipolar disorder",
      Comorbidity == "Patho cutanée" ~ "Skin disorder",
      Comorbidity == "Maladie foie" ~ "Liver disease",
      Comorbidity == "AVC" ~ "Stroke",
      Comorbidity == "Glaucome" ~ "Glaucoma",
      Comorbidity == "Adénome prostate" ~ "Prostate adenoma",
      Comorbidity == "Insuf cardiaque" ~ "Heart failure",
      Comorbidity == "FOP" ~ "Patent foramen ovale",
      Comorbidity == "Sjogren" ~ "Sjögren's syndrome",
      Comorbidity == "MAV" ~ "Arteriovenous malformation",
      Comorbidity == "Dissection artérielle vertébrale" ~ "Vertebral artery dissection",
      Comorbidity == "Syndrome myofacial" ~ "Myofascial syndrome",
      Comorbidity == "Insuf coronarienne" ~ "Coronary insufficiency",
      Comorbidity == "IDM" ~ "Myocardial infarction",
      TRUE ~ Comorbidity
    )
  )


# View table
data.frame(print(comorb_counts))

# Bar chart (horizontal)
plot <- ggplot(comorb_counts, aes(x = reorder(Comorbidity_eng, percentage), y = percentage)) +
  geom_col(fill = "#27343f", width = 0.7) +
  coord_flip() +
  geom_text(aes(label = sprintf("%.1f%%", percentage)), 
            hjust = -0.2, size = 3, fontface = "bold") +
  labs(title = "Prevalence of comorbidities (M0, N=454)",
       x = NULL, y = "Percentage of patients") +
 theme_minimal() +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold", size = 16),
    axis.title = element_text(face = "bold", size = 14),
    axis.text = element_text(face = "bold", size = 12, color = "black"),
        legend.title = element_text(face = "bold", size = 12),  
        legend.text = element_text(face = "bold", size = 11)
  ) 

plot
ggsave(file="../out/plot_comorb.svg", plot=plot, width=8, height=8)
