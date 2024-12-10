#### Visualisering af modellerne
  ### Log(balance)

låne_log <- låne_df

låne_log <- låne_log[,c(1,194)]

colnames(låne_log) <- c("Låne_penge","Balance 2020")

# Vi omdanner til 3 variabler istedet for 5

log_distinct <- distinct(låne_log,låne_log$Låne_penge)

låne_log <- låne_log %>%
  mutate(Låne_penge_sammensat = case_when(
    Låne_penge %in% c("Dårlig", "Meget dårlige") ~ "Dårlig/Meget dårlige",
    Låne_penge == "Neutrale" ~ "Neutrale",
    Låne_penge %in% c("Gode", "Meget gode") ~ "Gode/Meget gode"
  ))
log_distinct_sammensat <- distinct(låne_log,låne_log$Låne_penge_sammensat)

# Log(balance) udregnes for de 5 kategorier enkeltvis
låne_log$`Balance 2020` <- as.numeric(låne_log$`Balance 2020`)

låne_log <- låne_log %>% mutate(
  "log(balance)"=log(`Balance 2020`)
)

låne_log_agg_2 <- låne_log %>%
  group_by(Låne_penge) %>%
  summarise(mean_log_balance = mean(`log(balance)`, na.rm = TRUE))

låne_log_agg_2$Låne_penge <- factor(låne_log_agg_2$Låne_penge, levels = c("Meget dårlige", "Dårlig", "Neutrale", "Gode", "Meget gode"))

ggplot(låne_log_agg_2, aes(x =Låne_penge , y = mean_log_balance, fill = Låne_penge))+
  geom_bar(stat = "identity", position = "dodge", width = 0.8)+
  scale_fill_brewer(palette = "Blues") +
  labs(
    title = "Størrelsen på aktiver har betydning for adgangen til kapital",
    subtitle = "Virksomheders balance i forhold til deres vurdering af mængden af kapital",
    x = NULL,
    y = "Log(Balance)",
    caption = "Kilde: Baums top dollar csv-fil")+
  scale_y_continuous(breaks = seq(0, max(freq_holdning$`Svar i procent`+2), by = 2))+
  theme_bw()+
  theme(
    plot.title = element_text(face = "bold", size = 14),
    panel.grid.major.x = element_blank(),  
    panel.grid.minor.x = element_blank(),  
    panel.grid.minor.y = element_blank(),  
    axis.text.y = element_text(size = 10),
    axis.title.y = element_text(vjust = 1, size = 12),
    legend.title = element_blank(),
    legend.position = "bottom")


# Visualisering af afkastningsgrad og soliditetsgrad
  # Kommaer erstattes med punktummer.
låne_df_nøgletal <- låne_df[,c(1,206,218)]

låne_df_nøgletal[2:3] = lapply(låne_df_nøgletal[2:3], function(x) {
  as.numeric(gsub(",",".", x))
})

# udregner gns. til plot - grupperet på holdning til lånemuligheder

nøgletal_df <- låne_df_nøgletal %>%
  group_by(`Hvordan ser du mulighederne for at låne penge til din virksomhed? (fiktivt spørgsmål)`) %>%
  summarise(
    "Afkastningsgrad 2020" = mean(`Afkastningsgrad 2020 (%)`, na.rm = TRUE),
  )

colnames(nøgletal_df) <- c("Holdning til at låne penge", "Værdier")

nøgletal_df_2 <- låne_df_nøgletal %>%
  group_by(`Hvordan ser du mulighederne for at låne penge til din virksomhed? (fiktivt spørgsmål)`) %>%
  summarise(
    "Soliditetsgrad 2020" = mean(`Soliditetsgrad 2020 (%)`, na.rm = TRUE),
  )

colnames(nøgletal_df_2) <- c("Holdning til at låne penge", "Værdier")

nøgletal_df <- rbind(nøgletal_df, nøgletal_df_2)

nøgletal_df$Nøgletal <- rep(c("Afkastningsgrad", "Soliditetsgrad"),each = 5)

# Sørg for, at `Nøgletal` er en faktor for at bevare rækkefølgen i plottet
nøgletal_df <- nøgletal_df %>% arrange(desc(nøgletal_df$Værdier))


nøgletal_df$`Holdning til at låne penge` <- factor(
  nøgletal_df$`Holdning til at låne penge`,
  levels = c("Meget dårlige", "Dårlig", "Neutrale", "Gode", "Meget gode")
)
view(nøgletal_df)

# Opret barplot med unikke observationer og `fill`
ggplot(nøgletal_df, aes(x = Nøgletal, y = Værdier, fill = `Holdning til at låne penge`))+
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  scale_fill_brewer(palette = "PuBu") +
  labs(
    title = "Virksomheder med en højere soliditetsgrad synes bedre om finansieringsklimaet",
    x = NULL,
    y = "Pct (%).",
    fill = NULL,
    caption = "Kilde: Baums fede datasæt"
  ) +
  theme_bw()+
  scale_y_continuous(breaks = seq(0, max(nøgletal_df$Værdier+10), by = 5))+
  theme(
    plot.title = element_text(face = "bold", size = 12),
    axis.text.x = element_text(size = 10, vjust = 0.5),
    axis.title.y = element_text(size = 10, vjust = 1),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.position = "bottom",
    legend.text = element_text(size = 8)
  )
  
