################################################################################
#                                 Temporal Encoding
################################################################################

train_data_raw<-read_csv("data/traindata/train_data.csv")

# --- for 1 time sereis ---
train_ts_raw<-train_data_raw|> filter(coords== unique(train_data_raw$coords)[1])

# --- add temporal encoding ---
train_ts_raw<-train_ts_raw |>encode_woy()

# --- Plot temporal Encoding ---

train_ts_raw|>
  ggplot(aes(x=as_date(time)))+
  geom_line(aes(y=woy_sin, col= "woy_sin"))+
  geom_point(aes(y=woy_sin, col= "woy_sin"))+
  geom_line(aes(y=woy_cos, col= "woy_cos"))+
  geom_point(aes(y=woy_cos, col= "woy_cos"))


################################################################################
#
################################################################################


train_data_raw_enc<- train_data_raw|>encode_woy()


train_data_raw_enc




csv<-read_csv("G:/Meine Ablage/BA_Data/conifer_distubed_all.csv")
csv<-read_csv("G:/Meine Ablage/BA_Data/training_points.csv")

points<-csv|>
  group_by(coords)|>
  slice(1)|>
  mutate(col = 1) |>
  select(coords, col)|>
  separate(coords, into = c("lon", "lat"), sep = "/") |>
  mutate(lon = as.numeric(lon), lat = as.numeric(lat)) |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
  ungroup()

# Leaflet-Karte initialisieren
map <- leaflet() |>
  addProviderTiles("Esri.WorldImagery")

# Jeden Punkt-Datensatz zur Karte hinzufügen
map <- map |>
  addCircleMarkers(data = points, radius = 3,
                     color = "red", opacity = 0.8)

map









healthy_data_cleaned<- csv|> iso_clean_fast()

names <- c("time", "nir","B8A", "red", "rededge1", "rededge2", "rededge3", "blue", "green", "swir16", "swir22")

healthy_data_cleaned_int <- gf_impute_df_sgma_new(healthy_data_cleaned,
                                                  group_key = "coords",
                                                  order = 3,
                                                  aggregate = "week",
                                                  length = NULL,
                                                  names = names,
                                                  padding = FALSE,                # TRUE: gaps > k  -> 0; FALSE: everyting is imputed
                                                  mask = 0,                      # Maskvalue if padding =T: 0 or NA
                                                  k_ma = 4,
                                                  weighting = "exponential",
                                                  dt = FALSE)



healthy_data_cleaned2<- healthy_data_cleaned_int|>
#  filter(year(time)<2024)|>
  group_by(coords)|>
#  filter(!any(month(time) %in% c(11,12,1,2) & swir22 >=0.025))|>
  filter(!any(month(time) %in% c(11,12,1,2) & swir22 >=0.04))|>
  filter(!any(month(time) %in% c(11,12,1,2) & B8A >=0.21))|>
  ungroup()

healthy_data_cleaned3<- healthy_data_cleaned2|>
  mutate(ndvi =(nir-red)/(nir+red) )|>
  group_by(coords)|>
  filter(all(ndvi >= 0.55)) |>
  filter(all(swir22 <= 0.065)) |>
  ungroup()





healthy_data_cleaned3|> 
#  filter(coords %in% unique(healthy_data_cleaned_int$coords)[1:299])|>
  ggplot(aes(x=as_date(time), y = B8A, group = coords))+
  geom_line()


healthy_data_cleaned3|>pull(coords)|>unique()|>length()


healthy_data_cleaned3







small_test_area<-read_csv("G:/Meine Ablage/small_test_area/S2_all_valid_pixels.csv")


small_test_area<-small_test_area|>mutate(coords=geo2coords(.geo))


small_test_area|>get_extent()




small_test_area|>
  filter(coords==small_test_area$coords[1] )|>
  ggplot(aes(x=as_date(time), y = B12))+
  geom_line()+
  geom_point()








