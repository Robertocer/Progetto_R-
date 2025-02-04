library(dplyr)
library(ggplot2)
#####################BOX PLOT
# Filtra i dati e crea le fasce di magnitudo
DS3_filtered <- Ert_q %>%
  filter(depth < 300) %>%  # Escludiamo solo outlier estremi
  mutate(mag_group = cut(mag, breaks = seq(5, 9, by = 0.5), include.lowest = TRUE))

# Rimuovi le fasce di magnitudo che non contengono dati
DS3_filtered <- DS3_filtered %>%
  filter(!is.na(mag_group))

# Crea il grafico
ggplot(DS3_filtered, aes(x = mag_group, y = depth)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 16) +
  labs(title = "Distribuzione della profondità per fasce di magnitudo",
       x = "Fasce di Magnitudo",
       y = "Profondità (km)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))



#########################VIOLINO
library(dplyr)
library(ggplot2)

# Filtra i dati e crea le fasce di magnitudo
library(dplyr)
library(ggplot2)

# Filtra i dati e crea le fasce di magnitudo
DS3_filtered <- Ert_q %>%
  filter(depth > 0) %>%  # Rimuove profondità negative o zero
  mutate(mag_group = cut(mag, breaks = seq(5, 9, by = 0.5), include.lowest = TRUE)) %>%
  filter(!is.na(mag_group)) %>%  # Rimuove fasce di magnitudo senza dati
  group_by(mag_group) %>%
  filter(n() >= 10) %>%  # Mantiene solo gruppi con almeno 10 osservazioni
  ungroup()

# Creiamo il violin plot
ggplot(DS3_filtered, aes(x = mag_group, y = depth, fill = mag_group)) +
  geom_violin(scale = "width", alpha = 0.7) +  
  labs(title = "Distribuzione della profondità per fasce di magnitudo",
       x = "Fasce di Magnitudo",
       y = "Profondità (km)") +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +  # Aumenta la leggibilità delle etichette
  theme_minimal() +
  theme(legend.position = "none", 
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1.2))  # Allinea le etichette
