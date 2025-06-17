
#installeren en inladen van de benodigde packages
install.packages("tidyverse")
install.packages("cbsodataR")
install.packages("sf")
install.packages("ggplot2")
library(tidyverse)
library(cbsodataR)
library(sf)
library(ggplot2)

#inladen verschillende data sets
data_set_huizenprijzen <- read.csv2("data/Huisprijzen.csv")
data_set_inkomen <- read.csv2("data/Regionale_kerncijfers_Nederland_04062025_092105.csv")

data_set_bevolkingsdichtheid <- read.csv("data/Regionale_kerncijfers_Nederland_16062025_162909.csv")
data_set_bevolkingsdichtheid <- data_set_bevolkingsdichtheid %>%
  separate(
    col = Perioden.Regio.s.Bevolking.Bevolkingssamenstelling.op.1.januari.Bevolkingsdichtheid..aantal.inwoners.per.km..,
    into = c("Perioden", "Regio.s", "Bevolkingsdichtheid"),
    sep = ";"
  ) %>%
  mutate(
    Perioden = as.integer(Perioden),
    Regio.s = trimws(Regio.s),
    Bevolkingsdichtheid = as.numeric(Bevolkingsdichtheid)
  ) %>%
  filter(!is.na(Bevolkingsdichtheid))


# Pak kolommen 1 t/m 3 uit huizenprijzen, bijvoorbeeld: Regio.s, Perioden, Verkoopprijs
subset1 <- data_set_huizenprijzen[, 1:3]

# Pak kolommen 1, 2 en 4 uit inkomen, bijvoorbeeld: Regio.s, Perioden, Inkomen
subset2 <- data_set_inkomen[, c(1, 2, 4)]

# Hernoem kolom 3 in subset2 naar Inkomen 
colnames(subset2)[3] <- "Inkomen"

# Merge op Regio.s en Perioden
merged_data <- merge(subset1, subset2, by = c("Regio.s", "Perioden"))

# Verwijder rijen met missende waarden
clean_data <- na.omit(merged_data)

#Blank cells will become NA's
clean_data$Inkomen=as.numeric(clean_data$Inkomen)

#gemiddelde verkoopprijs delen door 1000
clean_data$Gemiddelde.verkoopprijs..euro. = clean_data$Gemiddelde.verkoopprijs..euro. / 1000

#verwijderen van rijen met missende data 
clean_data <- na.omit(clean_data)

#het maken van een nieuwe kolom met het prijs inkomens ratio. En het fixen van de naam
clean_data$`Prijs Inkomensratio` <- clean_data[[3]] / clean_data[[4]]
names(clean_data)[names(clean_data) == "Prijs Inkomensratio"] <- "Prijs.Inkomensratio"

# maken van nieuwe kolommen waarbij de groei ten opzichte van het vorige jaar word berekent
clean_data <- clean_data %>%
  arrange(Regio.s, Perioden) %>%
  group_by(Regio.s) %>%
  mutate(
    Groei_Verkoopprijs = (Gemiddelde.verkoopprijs..euro. - lag(Gemiddelde.verkoopprijs..euro.)) / lag(Gemiddelde.verkoopprijs..euro.),
    Groei_Inkomen = (Inkomen - lag(Inkomen)) / lag(Inkomen),
    Groei_PIR = (Prijs.Inkomensratio - lag(Prijs.Inkomensratio)) / lag(Prijs.Inkomensratio)
  ) %>%
  ungroup()

#nieuwe tabel aangemaakt voor shapefiles
data_2019 <- clean_data %>%
  filter(Perioden == 2019) %>%
  select(Regio.s, PIR_2019 = 5)

data_2023 <- clean_data %>%
  filter(Perioden == 2023) %>%
  select(Regio.s, PIR_2023 = 5)

pir_data <- merge(data_2019, data_2023, by = "Regio.s")

#laad shapefile
gemeente_map <- cbs_get_sf("gemeente", 2023, verbose = TRUE)
head(gemeente_map)

#een nieuwe dataset maken waarbij de juiste data word samengevoegd voor de shapefile om te werken
colnames(pir_data)[which(colnames(pir_data) == "Regio.s")] <- "statnaam"
kaart_met_data <- left_join(gemeente_map, pir_data, by = "statnaam")

ggplot(kaart_met_data) +
  geom_sf(aes(fill = PIR_2019), color = "white") +
  scale_fill_viridis_c(option = "C", na.value = "grey90", breaks = seq(5, 10, by = 1), lim = c(4, 11)) +
  labs(title = "Price/Income Ratio per Municipality (PIR) (2019)\n", fill = "PIR") +
  theme_minimal()

ggplot(kaart_met_data) +
  geom_sf(aes(fill = PIR_2023), color = "white") +
  scale_fill_viridis_c(option = "C", na.value = "grey90", breaks = seq(5, 10, by = 1), lim = c(4, 11)) +
  labs(title = "Price/Income Ratio per Municipality (PIR) (2023)\n", fill = "PIR") +
  theme_minimal()


# grafiek groei PIR
gemiddelde_groei_per_jaar <- clean_data %>%
  group_by(Perioden) %>%
  summarise(gemiddelde_groei = mean(Groei_PIR, na.rm = TRUE))

ggplot(gemiddelde_groei_per_jaar, aes(x = Perioden, y = gemiddelde_groei)) +
  geom_line(color = "blue", size = 1) +
  geom_smooth(method = "lm", se = FALSE, color = "grey", linetype = "dashed") +
  geom_hline(yintercept = 0, color = "black", size = 0.8) +
  labs(title = "Average growth of the PIR per Year\n",
       x = "Year\n",
       y = "Average growth\n") +
  scale_x_continuous ("Year", breaks = 2011:2023) +
  scale_y_continuous ("Average growth", lim= c(-0.12,0.12)) +
  theme_minimal()

#grafiek groei gemiddelde inkomen
gemiddelde_groei_inkomen_per_jaar <- clean_data %>%
  group_by(Perioden) %>%
  summarise(gemiddelde_groei = mean(Groei_Inkomen, na.rm = TRUE))

ggplot(gemiddelde_groei_inkomen_per_jaar, aes(x = Perioden, y = gemiddelde_groei)) +
  geom_line(color = "blue", size = 1) +
    geom_smooth(method = "lm", se = FALSE, color = "grey", linetype = "dashed") +
  geom_hline(yintercept = 0, color = "black", size = 0.8) +
  labs(title = "Average growth of the income per Year\n",
       x = "Year\n",
       y = "Average growth\n") +
  scale_x_continuous ("Year", breaks = 2011:2023) +
  scale_y_continuous ("Average growth") +
  theme_minimal()

# maken van boxplot
df_quintielen <- data_set_bevolkingsdichtheid %>%
  filter(!is.na(Bevolkingsdichtheid)) %>%               
  group_by(Perioden) %>%                                
  mutate(Bevolkingsdichtheid_quintiel = ntile(Bevolkingsdichtheid, 5)) %>%
  ungroup()

df_quintielen <- df_quintielen %>%
  mutate(density_group = case_when(
    Bevolkingsdichtheid_quintiel == 1 ~ "Very low density",
    Bevolkingsdichtheid_quintiel == 2 ~ "Low density",
    Bevolkingsdichtheid_quintiel == 3 ~ "Medium density",
    Bevolkingsdichtheid_quintiel == 4 ~ "High density",
    Bevolkingsdichtheid_quintiel == 5 ~ "Very high density"
  ))

# Zet juiste factorvolgorde 
df_quintielen$density_group <- factor(df_quintielen$density_group,
                                   levels = c("Very low density", "Low density",
                                              "Medium density", "High density",
                                              "Very high density"))

# join de datasets
df_complete <- clean_data %>%
  left_join(df_quintielen %>% select(Regio.s, Perioden, density_group),
            by = c("Regio.s", "Perioden"))

#Boxplot voor 2019 en 2023
ggplot(df_complete %>% filter(Perioden %in% c(2019, 2023)),
       aes(x = density_group, y = Prijs.Inkomensratio)) +
  geom_boxplot(aes(fill = as.factor(Perioden))) +
  labs(
    title = "Price-to-Income Ratio by Population Density Group (2019 vs 2023)\n",
    x = "Quintiles of Population Density\n",
    y = "Price-to-Income Ratio\n",
    fill = "Year"
  ) +
  theme_minimal()
