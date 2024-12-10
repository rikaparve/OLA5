### Libraries
library(tidyverse)
library(ggthemes)
library(gridExtra)
### Indlæs CSV-fil ###
# Læs filens indhold med korrekt encoding
låne_df <- read.csv("Data/regnskaber_industri_transport_byg_5_25000_ansatte_anonym.csv",
                    sep = ";", 
                    fileEncoding = "ISO-8859-1", 
                    check.names = FALSE)
# Vi renser datasættet
print(unique(låne_df$`Hvordan ser du mulighederne for at låne penge til din virksomhed? (fiktivt spørgsmål)`))

låne_df$`Hvordan ser du mulighederne for at låne penge til din virksomhed? (fiktivt spørgsmål)` <- gsub("Dårlige","Dårlig",låne_df$`Hvordan ser du mulighederne for at låne penge til din virksomhed? (fiktivt spørgsmål)`)
låne_df <- låne_df[låne_df$`Hvordan ser du mulighederne for at låne penge til din virksomhed? (fiktivt spørgsmål)` !="Ved ikke",]

# Opretter en ny dataframe med kun relevante variabler
låne_df_clean <- data.frame(låne_df$`Hvordan ser du mulighederne for at låne penge til din virksomhed? (fiktivt spørgsmål)`,
                            låne_df$`Antal ansatte Cvr-nr.`)

colnames(låne_df_clean) <- c("Holdning til at låne penge","Ansatte")

# Vi opretter en frekvenstabel til at se hvor mange gange de forskellige muligheder er besvaret samt med antal ansatte
freq_holdning <- låne_df_clean %>%
  group_by(`Holdning til at låne penge`) %>%
  summarise(
    Frekvens = n(),
    `Gns. medarbejdere` = mean(Ansatte, na.rm = TRUE) 
  )

freq_holdning$`Holdning til at låne penge` <- factor(freq_holdning$`Holdning til at låne penge`, levels = c("Meget gode", "Gode", "Neutrale", "Dårlig", "Meget dårlige"))

#### Visualisering af låne muligheder baseret på antal svar
ggplot(freq_holdning, aes(x = `Holdning til at låne penge`, y = Frekvens, fill = `Holdning til at låne penge`))+
  geom_bar(stat = "identity")

#### Visualisering af låne muligheder baseret på antal svar i %
freq_holdning$`Svar i procent` <- as.numeric(round(freq_holdning$Frekvens/sum(freq_holdning$Frekvens),2))*100

ggplot(freq_holdning, aes(x = `Holdning til at låne penge`, y = `Svar i procent`, fill = `Holdning til at låne penge`))+
  geom_bar(stat = "identity", position = "dodge")+
  scale_fill_brewer(palette = "set2") +
  labs(
    title = "Virksomhederne vurderer generelt finansieringsklimaet til at være godt",
    subtitle = "Procentvis fordeling af virksomhedernes holdning til deres muligheder for at låne penge",
    x = NULL,
    y = "Pct (%).",
    caption = "Kilde: Baums top dollar csv-fil")+
  scale_y_continuous(breaks = seq(0, max(freq_holdning$`Svar i procent`+5), by = 5))+
  theme_bw()+
  theme(
    plot.title = element_text(face = "bold", size = 14),
    panel.grid.major.x = element_blank(),  
    panel.grid.minor.x = element_blank(),  
    panel.grid.minor.y = element_blank(),  
    axis.text.y = element_text(size = 10),
    axis.title.y = element_text(vjust = 1, size = 12),
    legend.title = element_blank(),
    legend.position = "bottom"
    )

### Originale metode, som Baum ikke kan lide
  # Visualisering af låne muligheder baseret antal medarbejdere
    # Alternativ visualisering, som Baum ikke er glad for

låne_df_clean <- låne_df_clean %>%
  mutate(Antal_medarbejdere = case_when(
    Ansatte <= 20 ~ "1-20",
    Ansatte > 20 & Ansatte <= 50 ~ "21-50",
    Ansatte > 50 & Ansatte <= 100 ~ "51-100",
    Ansatte > 100 ~ "Over 100"
  ))

freq_table_ansatte <- as.data.frame(table(låne_df_clean$`Holdning til at låne penge`, låne_df_clean$Antal_medarbejdere))
freq_table_ansatte$Var1 <- factor(freq_table_ansatte$Var1, levels = c("Meget gode", "Gode", "Neutrale", "Dårlig", "Meget dårlige"))
colnames(freq_table_ansatte) <- c("Vurdering","Ansatte_interval","Antal")

freq_table_ansatte <- freq_table_ansatte %>%
  group_by(Ansatte_interval) %>%  
  mutate(Procentuel = round((Antal / sum(Antal)) * 100, 2)) %>% 
  ungroup()

ggplot(freq_table_ansatte, aes(x = Vurdering, y = Procentuel, fill = Ansatte_interval))+
  geom_bar(stat = "identity", position = position_dodge(width =0.8), width = 0.7)+
  scale_fill_manual(values = c("deepskyblue3", "gray60", "black", "deepskyblue4")) +
  labs(
    title = "Større virksomheder med mange ansatte, har nemmere ved at anskaffe kapital",
    x = NULL,
    y = "pct. (%)",
    caption = "Kilde: Baums csv-fil")+
  scale_y_continuous(breaks = seq(0,65,by = 5))+
  theme_bw()+
  theme(
    plot.title = element_text(face = "bold", size = 14),
    panel.grid.major.x = element_blank(),  
    panel.grid.minor.x = element_blank(),  
    panel.grid.minor.y = element_blank(),  
    axis.text.y = element_text(size = 10),
    axis.title.y = element_text(vjust = 1, size = 12),
    legend.position = "bottom",
    legend.text = element_text(size = 8),
    legend.title = element_blank()
  )

