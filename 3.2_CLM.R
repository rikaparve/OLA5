##### Opgave 3.2 CLM #####

  #### Værdierne renses
    ### Vi tilføjer følgende: Hvordan ser du muligheden, ansatte, etableringsdato, bruttofortjeneste,
    ### Balance, afkgastningsgrad, Soliditetsgrad
låne_df_renses = låne_df[, c(1,2,4,194,206,218)]
låne_df_renses <- na.omit(låne_df_renses)

    ### Lortet renses
      ### Standardisering af 1. spørgsmål
låne_df_renses <- låne_df_renses %>% mutate(
  standardisering = `Hvordan ser du mulighederne for at låne penge til din virksomhed? (fiktivt spørgsmål)` %>% 
    gsub("Meget dårlige", "1", .) %>% 
    gsub("Dårlig", "2", .) %>% 
    gsub("Neutrale", "3", .) %>% 
    gsub("Gode", "4", .) %>% 
    gsub("Meget gode", "5", .))

låne_df_renses$standardisering <- factor(låne_df_renses$standardisering, levels = c("1","2","3","4","5"))
head(låne_df_renses)
    ### Dato fikses
låne_df_renses$Etableringsdato <- as.Date(låne_df_renses$Etableringsdato, "%d-%m-%Y")
låne_df_renses$Etableringsdato <- as.numeric(format(låne_df_renses$Etableringsdato, "%Y"))

# Der trækkes 2021 idet at det er året datasættes tages udgangspunkt i.
låne_df_renses$Alder <- 2021-låne_df_renses$Etableringsdato

    ### Afkastningsgrad fikses
låne_df_renses$`Afkastningsgrad 2020 (%)` <- gsub(",",".",låne_df_renses$`Afkastningsgrad 2020 (%)`)
låne_df_renses$`Afkastningsgrad 2020 (%)` <- as.numeric(låne_df_renses$`Afkastningsgrad 2020 (%)`)

    ### Soliditetsgrad fikses
låne_df_renses$`Soliditetsgrad 2020 (%)` <- gsub(",",".",låne_df_renses$`Soliditetsgrad 2020 (%)`)
låne_df_renses$`Soliditetsgrad 2020 (%)` <- as.numeric(låne_df_renses$`Soliditetsgrad 2020 (%)`)


### Balance skal logges
låne_df_renses <- låne_df_renses %>% mutate(
  "log(balance)" = log(`Balance 2020 (1.000 kr)`)
)

colnames(låne_df_renses) <- c("Holdning til at låne penge",
                              "Ansatte",
                              "Etableringsdato",
                              "Balance",
                              "Afkastningsgrad",
                              "Soliditetsgrad",
                              "Standardisering",
                              "Alder",
                              "Balance_log")


låne_df_clm_data <- låne_df_renses[,c(2,5:9)]

#### CLM-LAD OSSSSS #####
library(ordinal)

clm_ansatte <- clm(Standardisering ~ Ansatte, data = låne_df_clm_data)
summary(clm_ansatte)
clm_afk <- clm(Standardisering ~ Afkastningsgrad, data = låne_df_clm_data)
summary(clm_afk)
clm_sol <- clm(Standardisering ~ Soliditetsgrad, data = låne_df_clm_data)
summary(clm_sol)
clm_alder <- clm(Standardisering ~ Alder, data = låne_df_clm_data)
summary(clm_alder)
clm_balance <- clm(Standardisering ~ Balance_log, data = låne_df_clm_data)
summary(clm_balance)


corrplot::corrplot(cor(låne_df_clm_data[,c(1,2,3,5,6)]), method = "number")

colnames(låne_df_clm_data)


clm_alle <- clm(Standardisering~Balance_log + Alder +Afkastningsgrad + Soliditetsgrad, data = låne_df_clm_data)
summary(clm_alle)
coef_table <- summary(clm_alle)$coefficients
p_values <- data.frame(coef_table[, "Pr(>|z|)"])

options(scipen = 3)
summary(clm_alle)


##SVAR: Vi fortsætter sgu med Log(balance), da den overtrumfer alle andre variabler.
clm_balance <- clm(Standardisering ~ Balance_log, data = låne_df_clm_data)
summary(clm_balance)
