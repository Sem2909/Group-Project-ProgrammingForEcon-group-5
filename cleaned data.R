#inladen verschillende data sets
data_set_huizenprijzen <- read.csv2(file.choose(), header = TRUE)
data_set_inkomen <- read.csv2(file.choose(), header = TRUE)

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

#installeren en inladen van de package tidyverse
install.packages("tidyverse")
library(tidyverse)

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

#installeren en inladen van de benodigde packages
install.packages("cbsodataR")
library(cbsodataR)
install.packages("sf")
install.packages("ggplot2")
library(sf)
library(ggplot2)

#laad shapefile
gemeente_map <- cbs_get_sf("gemeente", 2023, verbose = TRUE)
head(gemeente_map)

#een nieuwe dataset maken waarbij de juiste data word samengevoegd voor de shapefile om te werken
colnames(pir_data)[which(colnames(pir_data) == "Regio.s")] <- "statnaam"
kaart_met_data <- left_join(gemeente_map, pir_data, by = "statnaam")

ggplot(kaart_met_data) +
  geom_sf(aes(fill = PIR_2019), color = "white") +
  scale_fill_viridis_c(option = "C", na.value = "grey90") +
  labs(title = "Prijs/Inkomensratio per Gemeente (PIR) (2019)", fill = "PIR") +
  theme_minimal()

ggplot(kaart_met_data) +
  geom_sf(aes(fill = PIR_2023), color = "white") +
  scale_fill_viridis_c(option = "C", na.value = "grey90") +
  labs(title = "Prijs/Inkomensratio per Gemeente (PIR) (2023)", fill = "PIR") +
  theme_minimal()


# nieuwe data set met het gemiddelde ratio per jaar om een grafiek te plotten
gemiddelde_ratio_per_jaar <- clean_data %>%
  group_by(Perioden) %>%
  summarise(gemiddelde_ratio = mean(Prijs.Inkomensratio, na.rm = TRUE))

ggplot(gemiddelde_ratio_per_jaar, aes(x = Perioden, y = gemiddelde_ratio)) +
  geom_line(color = "blue", size = 1) +
  labs(title = "Gemiddelde ratio per jaar\n",
       x = "Jaar\n",
       y = "Gemiddelde ratio\n") +
  scale_x_continuous ("jaar", breaks = 2011:2023) +
  scale_y_continuous ("Gemiddelde ratio", breaks = 5:8, lim= c(5,8)) +
  theme_minimal()


quintiel_data <- clean_data %>%
  filter(Perioden %in% c(2019, 2023)) %>%
  group_by(Perioden) %>%
  mutate(
    PrijsInkomen_Quintiel = ntile(Prijs.Inkomensratio, 5)
  ) %>%
  ungroup()

#selecteer de quintielen
quintiel_2023 <- quintiel_data %>%
  filter(Perioden == 2023)

# Maak de boxplot
ggplot(quintiel_2023, aes(x = as.factor(PrijsInkomen_Quintiel), y = Prijs.Inkomensratio)) +
  geom_boxplot(fill = "skyblue", color = "darkblue") +
  labs(
    title = "Verdeling van Prijs-Inkomensratio per Quintiel (2023)",
    x = "Quintiel",
    y = "Prijs-Inkomensratio"
  ) +
  theme_minimal()