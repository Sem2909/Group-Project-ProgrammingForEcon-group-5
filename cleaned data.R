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
