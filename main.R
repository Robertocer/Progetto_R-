#Librerie
library(sf)
library(ggplot2)
library(dplyr)
library(maps)
library(tidyr)
library(RColorBrewer)
library(stringr)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggrepel)
library(lubridate)

# Carico i dataset
setwd("./dataset")
terremoti <- read.csv("Earthquake_1970-2025.csv")
faglie <- st_read("gem_active_faults.shp")

# ----- Analisi preliminare -----
head(terremoti) # Prime righe
tail(terremoti) # Ultime righe

str(terremoti) # Struttura

summary(terremoti) # Riassunto variabili

colSums(is.na(terremoti)) # Eventuali NA

table(terremoti$magType)
table(terremoti$status)
# "automatic": I dati sono stati generati automaticamente dal sistema, con una velocità di diffusione maggiore ma con una maggiore possibilità di imprecisioni.
# "reviewed": I dati sono stati esaminati manualmente da esperti, con una maggiore accuratezza e affidabilità, ma richiedono più tempo per essere disponibili.

table(terremoti$type)
table(terremoti$locationSource)
table(terremoti$magSource)

# ----- Inizio analisi -----

# Filtro via: magType, gap, dmin, rms, net, id, updated
# elimino le righe status!=reviewed
terremoti_df <- terremoti %>%
  filter(status == "reviewed") %>%
  select(time,latitude,longitude,depth,mag,nst,place,type,horizontalError,depthError,magError) %>%
  as.data.frame()

summary(terremoti_df)

# Diminuisco il campione e filtro solo i terremoti di origine naturale
terremoti_sampled <- terremoti_df %>%
  sample_frac(0.1) %>%
  filter(type == "earthquake")

# Dimensione del dataset campionato
nrow(terremoti_sampled) 

ggplot() +
  # Aggiungi la mappa del mondo
  borders("world", colour = "gray50", fill = "lightgray") + 
  # Aggiungi le faglie
  geom_sf(data = faglie, color = "red", size = 1, alpha = 0.7) +
  labs(title = "Faglie Geologiche Attive", x = "Longitudine", y = "Latitudine", color = "Magnitudo") +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    legend.position = "right"
  )

ggplot() +
  # Aggiungi la mappa del mondo
  borders("world", colour = "gray50", fill = "lightgray") + 
  # Aggiungi le faglie
  geom_sf(data = faglie, color = "red", size = 1, alpha = 0.7) +
  # Aggiungi i terremoti
  geom_point(data = terremoti_sampled, aes(x = longitude, y = latitude, color = mag, size = mag), alpha = 0.6) +
  scale_color_viridis_c() + 
  scale_size_continuous(range = c(0.5, 3)) + 
  labs(title = "Terremoti e Faglie Geologiche", x = "Longitudine", y = "Latitudine", color = "Magnitudo") +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    legend.position = "right"
  )

# Filtro tutto ciò che non è earthquake
terremoti_sampled_2 <- terremoti_df %>%
  filter(type != "earthquake")
nrow(terremoti_sampled_2) 

ggplot() +
  borders("world", colour = "gray50", fill = "lightgray") +
  geom_point(data = terremoti_sampled_2, aes(x = longitude, y = latitude, color = type, size = mag), alpha = 0.6) +
  scale_size_continuous(range = c(1, 4)) +
  labs(title = "Terremoti non dovuti a faglie", x = "Longitudine", y = "Latitudine", 
       color = "Tipo di Terremoto", size = "Magnitudo") +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    legend.position = "right"
  )

# ----- Vediamo le frequenze dei vari tipi di terremoti -----

frequenza_terremoti <- terremoti_df %>%
  count(type) %>%
  mutate(Percentuale = n / sum(n) * 100)

frequenza_terremoti <- frequenza_terremoti %>%
  mutate(Percentuale = n / sum(n) * 100) %>% 
  mutate(type = ifelse(Percentuale < 1, "Altri", as.character(type))) %>% 
  group_by(type) %>%
  summarise(n = sum(n), Percentuale = sum(Percentuale)) %>%
  ungroup()

ggplot(data = frequenza_terremoti, aes(x = reorder(type, Percentuale), y = Percentuale)) +
  geom_bar(stat = "identity", fill = "#4CAF50", color = "black", width = 0.7) +
  coord_flip() +  
  labs(title = "Frequenza Percentuale dei Terremoti per Tipo", 
       x = "Tipo di Terremoto", 
       y = "Percentuale (%)") +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold", color = "#2C3E50"),
    axis.text.x = element_text(size = 12, color = "#2C3E50"),
    axis.text.y = element_text(size = 12, color = "#2C3E50"),
    axis.title = element_text(size = 14, color = "#2C3E50"),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank(),  
    axis.ticks = element_line(color = "gray80"), 
    plot.margin = margin(10, 20, 10, 20)
  ) +
  geom_text(aes(label = round(Percentuale, 1)), hjust = -0.2, color = "#2C3E50", size = 5)

frequenza_terremoti_minori <- terremoti_df %>%
  filter(type != "earthquake") %>%
  count(type) %>%
  mutate(Percentuale = n / sum(n) * 100)


ggplot(data = frequenza_terremoti_minori, aes(x = reorder(type, Percentuale), y = Percentuale)) +
  geom_bar(stat = "identity", fill = "#007fff", color = "black", width = 0.7) +
  coord_flip() +  
  labs(title = "Frequenza Percentuale dei Terremoti per Tipo (Altri)", 
       x = "Tipo di Terremoto", 
       y = "Percentuale (%)") +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold", color = "#2C3E50"),
    axis.text.x = element_text(size = 12, color = "#2C3E50"),
    axis.text.y = element_text(size = 12, color = "#2C3E50"),
    axis.title = element_text(size = 14, color = "#2C3E50"),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank(),  
    axis.ticks = element_line(color = "gray80"), 
    plot.margin = margin(10, 20, 10, 20)
  ) +
  geom_text(aes(label = round(Percentuale, 1)), hjust = -0.2, color = "#2C3E50", size = 5)

# ----- Dividiamo la frequenza in base agli stati -----
# Vogliamo raccogliere gli stati americani come una sola nazione
usa_state = c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", 
              "Delaware", "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", 
              "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", 
              "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", 
              "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", 
              "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota", "Tennessee", 
              "Texas", "Utah", "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming")

usa_state = tolower(usa_state)

# Ho filtrato anche le parole: region, earthquake, sequence, california-baja
earthquakes <- terremoti_df %>%
  filter(type == "earthquake") %>%
  separate(place, into = c("location", "state"), sep = ",", extra = "drop") %>%
  mutate(state = trimws(state)) %>%
  filter(!is.na(state)) %>%
  mutate(state = tolower(state)) %>%
  mutate(state = str_replace_all(state, "region|earthquake|sequence|california-baja", "")) %>%
  mutate(state = str_trim(state)) %>%
  mutate(state = case_when(
    state %in% usa_state ~ "usa",
    TRUE ~ state  # mantiene il valore originale per le altre nazioni
  ))%>%
  count(state) %>%
  mutate(Percentuale = n / sum(n) * 100) %>%
  arrange(-n)

head(earthquakes)
tail(earthquakes)


state_map <- map_data("state")
world_map <- map_data("world")

centroids_state <- state_map %>%
  group_by(region) %>%
  mutate(region = tolower(region)) %>%
  summarise(
    latitude = mean(lat),
    longitude = mean(long)
  )

centroids_world <- world_map %>%
  group_by(region) %>%
  mutate(region = tolower(region)) %>%
  summarise(
    latitude = mean(lat),
    longitude = mean(long)
  )

# Visualizza i primi risultati
head(centroids_world)
head(centroids_state)



# Ora uniamo i due dataset
merged_data <- merge(earthquakes, centroids_world, by.x = "state", by.y = "region", all.x = TRUE)

# Visualizza il risultato
head(merged_data)

# Rimuovi i valori NA nelle coordinate
merged_data_clean <- merged_data %>% 
  filter(!is.na(longitude) & !is.na(latitude)) %>%
  arrange(-n)

# ----- Vediamo quali sono gli stati con più terremoti ----
limite = 5

# Funzione per filtrare gli stati a più alta percentuale
filtra_percentuale <- function(dataset, soglia = 3) {
  dataset %>%
    filter(Percentuale >= soglia)
}

stati_importanti <- filtra_percentuale(merged_data_clean, limite)

ggplot(stati_importanti, aes(x = "", y = Percentuale, fill = state, group = state)) +
  geom_bar(stat = "identity", width = 1, show.legend = FALSE) + 
  coord_polar(theta = "y") +
  theme_void() +
  labs(title = "Distribuzione Percentuale degli Stati (>5%)") +
  theme(axis.text.x = element_blank(),
        legend.position = "right",
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold")) +
  scale_fill_brewer(palette = "Set3") +
  geom_bar(stat = "identity", width = 1, aes(y = Percentuale)) +
  geom_text(aes(label = paste(str_trunc(str_to_title(state), width = 14, ellipsis = "..."), "\n", round(Percentuale, 1), "%")),
            position = position_stack(vjust = 0.6), size = 3.8, color = "black") +
  scale_y_continuous(labels = scales::percent)

# ----- Ci concentriamo sull'Asia ----

# Carica i dati geospaziali del mondo (continenti e paesi)
world <- ne_countries(scale = "medium", returnclass = "sf")

asia <- world %>% filter(continent == "Asia")

# Converte i dati dei punti in un oggetto sf
merged_data_sf <- st_as_sf(merged_data_clean, coords = c("longitude", "latitude"), crs = st_crs(asia))

# Filtro i punti che sono dentro il continente africano
merged_data_filtered <- st_intersection(merged_data_sf, asia)

# Estrai le coordinate dalla geometria (punto sf)
merged_data_filtered <- merged_data_filtered %>%
  mutate(longitude = st_coordinates(geometry)[, 1],
         latitude = st_coordinates(geometry)[, 2])

st_crs(faglie) <- 4326  # Imposta il CRS di 'faglie'
st_crs(asia) <- 4326    # Imposta il CRS di 'asia'
faglie_in_asia <- st_intersection(faglie, asia)

asia_merged <- st_join(asia, merged_data_sf %>% select(state, Percentuale))


ggplot(data = asia_merged) +
  geom_sf(aes(fill = Percentuale), color = "gray") +
  scale_fill_gradient(low = "yellow", high = "red", na.value = "gray90", 
                      name = "Percentuale") +  
  geom_point(data = merged_data_filtered, aes(x = longitude, y = latitude), alpha = 0.6) +  
  geom_sf(data = faglie_in_asia, color = "violet", size = 1, alpha = 0.7) +
  geom_text_repel(data = merged_data_filtered, 
                  aes(x = longitude, y = latitude, label = str_to_title(state)), 
                  size = 3, fontface = "bold") +
  labs(title = "Paesi più simici in Asia e relative faglie geologiche", x = "Longitudine", y = "Latitudine", fill = "Percentuale", caption = 'In grigio sono indicati gli stati su cui non abbiamo dati.') +
  guides(fill = guide_colorbar(title = "Percentuale", title.position = "top", barwidth = 0.8, barheight = 15)) +
  theme_minimal(base_size = 16) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
    legend.position = "right",
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12),
  )




# ---- Calcola la magnitudo massima per ogni stato ----
max_magnitudes <- terremoti_df %>%
  filter(type == "earthquake") %>%
  separate(place, into = c("location", "state"), sep = ",", extra = "drop") %>%
  mutate(state = trimws(state)) %>%
  filter(!is.na(state)) %>%
  mutate(state = tolower(state)) %>%
  mutate(state = str_replace_all(state, "region|earthquake|sequence|california-baja", "")) %>%
  mutate(state = str_trim(state)) %>%
  mutate(state = case_when(
    state %in% usa_state ~ "usa",
    TRUE ~ state
  )) %>%
  group_by(state) %>%
  summarise(Max_Mag = max(mag, na.rm = TRUE))

limite = 7.55

# Funzione per filtrare gli stati a più alta mag
filtra_percentuale <- function(dataset, soglia = 3) {
  dataset %>%
    filter(Max_Mag >= soglia)
}

max_magnitudes <- filtra_percentuale(max_magnitudes, limite)

head(max_magnitudes)

ggplot(max_magnitudes, 
       aes(x=Max_Mag, 
           y=reorder(state, Max_Mag))) +
  geom_point(color="blue", 
             size = 2) +
  geom_segment(aes(x = limite, 
                   xend = Max_Mag, 
                   y = reorder(state, Max_Mag), 
                   yend = reorder(state, Max_Mag)),
               color = "lightgrey") +
  labs (x = "MAG",
        y = "",
        title = "Terremoti più forti per ogni stato",
        subtitle = "Dal 1970 al 2014") +
  theme_minimal() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# ---- Frequenza di terremoti nell mondo ogni anno ----

df =terremoti_df %>%
  filter(type %in% c("earthquake", "nuclear explosion")) %>%
  mutate(time = ymd_hms(time, tz = "UTC"),  
         year = year(time)) %>%
  group_by(year, type) %>%
  count(year)

head(df)

ggplot(df, aes(x = year, y = n, color = type)) +
  geom_point(size=2) +
  facet_wrap(~type, scales = "free_y") +
  labs(title = "Distribuzione percentuale dei terremoti per type",
       x = "Anno",
       y = "#") +
  theme_minimal()

df_earthquake =terremoti_df %>%
  filter(type %in% c("earthquake")) %>%
  mutate(time = ymd_hms(time, tz = "UTC"),  
         year = year(time)) %>%
  group_by(year, type) %>%
  count(year)

ggplot(df_earthquake, aes(x = year, 
               y = n, 
               color=type)) +
  geom_point(size=2) +
  geom_smooth(se=TRUE, 
              method = "lm", 
              #formula = y~poly(x,4), 
              size = 1, color = "darkgray") +
  labs(title = "Distribuzione percentuale dei terremoti dovuti a faglie") +
  theme_minimal()


df_filtered <- terremoti_df %>%
  filter(!type %in% c("rock burst", "mine collapse")) %>%
  mutate(time = ymd_hms(time, tz = "UTC"),
         year = year(time)) %>%
  mutate(year_group = cut(year, 
                          breaks = seq(min(year), max(year) + 5, by = 5),
                          include.lowest = TRUE, 
                          right = FALSE)) %>% 
  group_by(type, year_group) %>%
  summarise(count = n(), .groups = "drop")
  
ggplot(df_filtered, aes(x = year_group, fill = type, weight = count)) + 
  geom_bar(position = "fill") +
  scale_y_continuous(trans = "log") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(y = 'log(#)', x = "Anni", title = "Grafico in scala logaritmica dei vari terremoti")

