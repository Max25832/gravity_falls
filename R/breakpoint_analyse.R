# baum_val <- read_csv("C:/Users/maxki/Documents/Domainproject/data/final_data/validierungs_ids_und_species.csv")
# baum_val2 <- read_csv("C:/Users/maxki/Documents/Domainproject/data/final_data/for_students/Validierungs_Datensatz.csv")
# 
# baum_tr <- read_csv("C:/Users/maxki/Documents/Domainproject/data/final_data/for_students/Trainings_Datensatz.csv")
# 
#comb<- left_join(baum_val2, baum_val, by= "id")





length(unique(comb$id))


table(baum_val$species)



comb<-comb|>
  mutate(ndvi = (b8-b4)/(b8+b4))

library(fpp3)
library(dplyr)


wachstumsY<-comb|>
  mutate(month = month(time),
         year=year(time)) |>
  filter(month  %in% c(4:9))|>
  group_by(id, year) |>
  mutate(ndvi_year = median(ndvi, na.rm=T))|>
#  mutate(ndvi_year = median(b12, na.rm=T))|>
  dplyr::select(all_of(c("ndvi_year", "year", "id", "species")))  |>distinct()




wachstumsY2 <- wachstumsY |>
  arrange(id, year) |>        # oder 'time', wenn das dein Datumsfeld ist
  group_by(id) |>
  mutate(
    slope_year = ndvi_year - lag(ndvi_year)   ) |>
  ungroup()|>na.omit()



wachstumsY2 |>
  group_by(id) |>
  filter(species != "soil") |>
  filter(species != "disturbed") |>
  ggplot()+
  geom_density(aes(x=slope_year, colour = species))+
  geom_vline(aes(xintercept =quantile(slope_year,0.02)), col="red")





################################################################################
#                               For Species
################################################################################



# Only Norwayspruce, Pine and mixed!!

# --> isolation forest!

# 1. slope methode
# 2. STL trend / year test 
# 3. Mean 






#--- STL-----
id_x<-wachstumsY$id[1]





ts_df_clean <- comb|>
  dplyr::filter(id == id_x) %>%
  mutate(time = yearmonth(time))|>group_by(time)|> summarise_all(mean)
  
  tsibble::as_tsibble(index = time) %>%
  tsibble::fill_gaps(.full = TRUE) %>%        
  mutate(ndvi_year = ifelse(is.na(ndvi_year), mean(ndvi_year), ndvi_year))


# STL-Decomposition
stl_model <- ts_df_clean %>%
  fabletools::model(
    classical_decomposition(ndvi_year, type = "additive")
  )

# Komponenten extrahieren und plotten
stl_model %>%
  fabletools::components() %>%
  feasts::autoplot()


################################################################################
# Packages
################################################################################
library(tidyverse)
library(tsibble)
library(feasts)
library(fable)
library(fpp3)
library(isotree)     # für Isolation Forest
library(lubridate)
library(slider)

################################################################################
# 1. Isolation Forest: grobe Outlier pro Zeitreihe (id) entfernen
################################################################################

filter<- dplyr::filter
select<- dplyr::select
summarise<-dplyr::summarise
# Beispiel: NDVI als Zielvariable
# -> Isolation Forest pro ID (Zeitreihe)
remove_outliers_iforest <- function(df, contamination = 0.02) {
  iso <- isolation.forest(matrix(df$ndvi, ncol = 1), ntrees = 100)
  preds <- predict(iso, matrix(df$ndvi, ncol = 1))
  
  df$anomaly_score <- preds
  # Outlier = Score über Quantil (bspw. 98%)
  cutoff <- quantile(preds, probs = 1 - contamination)
  df %>% dplyr::filter(anomaly_score < cutoff) %>% dplyr::select(-anomaly_score)
}

# Anwendung pro ID
comb_filtered <- comb %>%
  filter(species=="Norway_spruce")|>
  group_by(id) %>%
  group_modify(~ remove_outliers_iforest(.x, contamination = 0.02)) %>%
  ungroup()

################################################################################
# 2. Monatliche Aggregation + Forward Fill
################################################################################

comb_monthly <- comb_filtered %>%
  mutate(yearmonth = yearmonth(time)) %>%
  group_by(id, yearmonth, species) %>%
  dplyr::summarise(ndvi_month = mean(ndvi, na.rm = TRUE), .groups = "drop") %>%
  as_tsibble(key = id, index = yearmonth) %>%
  group_by_key() %>%
  fill_gaps() %>%
  mutate(ndvi_month = zoo::na.locf(ndvi_month, na.rm = FALSE),   # forward fill
         ndvi_month = zoo::na.locf(ndvi_month, fromLast = TRUE),
         species = zoo::na.locf(species, na.rm = FALSE),   # forward fill
         species = zoo::na.locf(species, fromLast = TRUE))|>
  ungroup()


################################################################################
# 3. STL-Decomposition mit fpp3 (trend + saison + remainder)
################################################################################

# Beispiel: nur eine ID plotten
id_x <- unique(comb_monthly$id)[17]

comb_monthly|> filter(ndvi_month<0.5)
id_x<-17
id_x<-162  
stl_model <- comb_monthly %>%
  filter(id == id_x) %>%
  model(STL(ndvi_month ~ trend(window = 12) + season(window = 12),
            robust = TRUE))

# Ergebnisse plotten
stl_model %>%
  components() %>%
  autoplot() +
  ggtitle(paste("STL Decomposition for ID", id_x))

################################################################################
#                         Breakpointananlyse
################################################################################
library(strucchange)

id_x<-162
id_x<-64  
id_x<-unique(comb_monthly$id)[64]  
id_x<-bad_ids[70]
# Beispiel: NDVI-Zeitreihe einer ID
ts_id <- comb_monthly %>%
  filter(id == id_x) %>%
  arrange(yearmonth)

# in ts-Objekt umwandeln (notwendig für strucchange)
ndvi_ts <- ts(ts_id$ndvi_month, frequency = 12, start = c(year(min(ts_id$yearmonth)), month(min(ts_id$yearmonth))))

# Strukturelle Brüche im Mittelwert (Levelshift)
bp <- breakpoints(ndvi_ts ~ 1)  # ~1 = nur Level, kein Trend


ggplot()+
  geom_line(aes(x= as.Date(time(ndvi_ts)),y = as.numeric(ndvi_ts) ), col = "grey40")+
  geom_line(aes(x= as.Date(time(fitted(bp, breaks = 1))),y = as.numeric(fitted(bp, breaks = 1)) ), col = "red", lwd = 2)





#---------------------------------------------------



detect_levelshift <- function(df, var = "ndvi_month", threshold = 0.2) {
  var <- sym(var)  # Symbol aus Zeichenkette
  
  df <- df %>% drop_na(!!var)
  if (nrow(df) < 10) {
    return(tibble(id = unique(df$id), break_date = NA, mean_before = NA, mean_after = NA, level_shift = NA))
  }
  
  # Zeitreihe aus gewählter Variable
  ts_data <- ts(pull(df, !!var), frequency = 12,
                start = c(year(min(df$yearmonth)), month(min(df$yearmonth))))
  
  bp <- tryCatch(breakpoints(ts_data ~ 1), error = function(e) return(NULL))
  if (is.null(bp) || all(is.na(bp$breakpoints))) {
    return(tibble(id = unique(df$id), break_date = NA, mean_before = NA, mean_after = NA, level_shift = NA))
  }
  
  # Alle Breakpoints prüfen
  fits <- fitted(bp, breaks = length(bp$breakpoints))
  shifts <- tibble()
  
  for (i in seq_along(bp$breakpoints)) {
    bp_i <- bp$breakpoints[i]
    mean_before <- mean(fits[1:bp_i])
    mean_after <- mean(fits[(bp_i + 1):length(fits)])
    diff <- mean_after - mean_before
    shifts <- bind_rows(shifts, tibble(break_id = i, break_date = breakdates(bp)[i],
                                       mean_before, mean_after, level_shift = diff))
  }
  
  # Größter absoluter Shift
  best <- shifts %>% filter(abs(level_shift) == max(abs(level_shift), na.rm = TRUE))
  
  if (nrow(best) == 0 || abs(best$level_shift) < threshold) {
    tibble(id = unique(df$id), break_date = NA, mean_before = NA, mean_after = NA, level_shift = NA)
  } else {
    tibble(id = unique(df$id), best)
  }
}




bp_results <- comb_monthly %>%
  group_by(id) %>%
  group_modify(~ detect_levelshift(.x, threshold = 0.2)) %>%
  ungroup()


bp_results01 <- comb_monthly %>%
  group_by(id) %>%
  group_modify(~ detect_levelshift(.x, threshold = 0.05)) %>%
  ungroup()









bad_ids<-bp_results01|>
  na.omit(break_date)|>
  filter(level_shift < 0)|>
  filter(break_date==2018)|>
  # filter(level_shift > -0.18)|>
  # filter(level_shift < -0.17)|>
#  filter(mean_after<0.6)|>
  pull(id)

# bad_ids<-bp_results01|> na.omit(break_date)|>
#   dplyr::filter(level_shift > -0.188)|>
#   pull(id)


bp_results01|>
  filter(id %in% bad_ids)|>
#  filter(id == bad_ids[4])|>
  na.omit(break_date)|>
  pull(level_shift)|>
  max()


#id_x1<-16800

comb|>
  filter( id==bad_ids[2])|>
  ggplot()+
 # geom_line(aes(x=as_date(time), y = scale(b12)))+
  geom_line(aes(x=as_date(time), y = ndvi), col="red")+
  #geom_point(aes(x=as_date(time), y = scale(b12)))+
  geom_point(aes(x=as_date(time), y = ndvi), col="red")+
  labs(title= paste0("Level Shift: ", (bp_results01|>
                                         filter(id == bad_ids[1])|>
                                         pull(level_shift)) ))





#TODO Diese filtern oder relabeln für jede species (NSP, NSP_mixed, pine )
#TODO Filtern, wo disturbance year vor 2018      --> entfernen 
#TODO Filtern, wo disturbance year == 2018 -->  entfernen 
#TODO Filtern, levelshift zwischen 0 und -0.17  --> entfernen 
#TODO Filtern, levelshift < --0.17  --> relabeln 


################################################################################
#                                 new spruce
################################################################################
comb_filtered <- comb %>%
  filter(species=="Norway_spruce")|>
  group_by(id) %>%
  group_modify(~ remove_outliers_iforest(.x, contamination = 0.02)) %>%
  ungroup()


#------------- 2. Monatliche Aggregation + Forward Fill -------------

comb_monthly <- comb_filtered %>%
  mutate(yearmonth = yearmonth(time)) %>%
  group_by(id, yearmonth, species) %>%
  dplyr::summarise(ndvi_month = mean(ndvi, na.rm = TRUE), .groups = "drop") %>%
  as_tsibble(key = id, index = yearmonth) %>%
  group_by_key() %>%
  fill_gaps() %>%
  mutate(ndvi_month = zoo::na.locf(ndvi_month, na.rm = FALSE),   # forward fill
         ndvi_month = zoo::na.locf(ndvi_month, fromLast = TRUE),
         species = zoo::na.locf(species, na.rm = FALSE),   # forward fill
         species = zoo::na.locf(species, fromLast = TRUE))|>
  ungroup()



bp_results01 <- comb_monthly %>%
  group_by(id) %>%
  group_modify(~ detect_levelshift(.x, threshold = 0.05)) %>%
  ungroup()



NSP_filter_ids<- bp_results01|> 
  na.omit()|>
  filter(break_date<=2018| (level_shift < 0  & level_shift > -0.17))|>
  pull(id)



NSP_relable_ids<- bp_results01|> 
  na.omit()|>
  filter(break_date>2018)|>
  filter(level_shift <= -0.17 )|>
  pull(id)
  



relable_df<-comb|> filter(species=="Norway_spruce")|>
  filter(id %in% NSP_relable_ids)

comb|> filter(species=="Norway_spruce")|>pull(id)|>unique()|> length()

NSP_relable_ids|>length()
NSP_filter_ids|>length()
NSP_M_filter_ids|>length()
s_p_filter_ids




relable_df|> 
  filter(id == unique(relable_df$id)[213])|>
  ggplot(aes(x=as_date(time), y = b12))+
  geom_line()+
  geom_point()

#unique(comb$species)

################################################################################
#                               Preparation 
################################################################################
# 1. Outlier removal (Isolation Forest)
# 2. to monthly df and Forward Fill
# 3. Detect level shift for each species 


comb<- read_csv("C:/Users/maxki/Documents/Domainproject/data/final_data/final_val/FINAL_VAL_mit_species.csv")

comb<-comb|>mutate(ndvi= ((b8-b4)/(b8+b4)))


comb_filtered_all <- comb %>%
  group_by(id, species) %>%
  group_modify(~ remove_outliers_iforest(.x, contamination = 0.02)) %>%
  ungroup()


#------------- 2. Monatliche Aggregation + Forward Fill -------------

comb_monthly_all <- comb_filtered_all %>%
  mutate(yearmonth = yearmonth(time)) %>%
  group_by(id, yearmonth, species) %>%
  dplyr::summarise(ndvi_month = mean(ndvi, na.rm = TRUE), .groups = "drop") %>%
  as_tsibble(key = id, index = yearmonth) %>%
  group_by_key() %>%
  fill_gaps() %>%
  mutate(ndvi_month = zoo::na.locf(ndvi_month, na.rm = FALSE),   # forward fill
         ndvi_month = zoo::na.locf(ndvi_month, fromLast = TRUE),
         species = zoo::na.locf(species, na.rm = FALSE),   # forward fill
         species = zoo::na.locf(species, fromLast = TRUE))|>
  ungroup()




bp_results005_nsp <- comb_monthly_all %>%
  filter(species == "Norway_spruce")|>
  group_by(id) %>%
  group_modify(~ detect_levelshift(.x, threshold = 0.05)) %>%
  ungroup()

bp_results005_nspMixed <- comb_monthly_all %>%
  filter(species == "Norway_spruce_mixed")|>
  group_by(id) %>%
  group_modify(~ detect_levelshift(.x, threshold = 0.05)) %>%
  ungroup()

bp_results005_sPine <- comb_monthly_all %>%
  filter(species == "Scots_pine")|>
  group_by(id) %>%
  group_modify(~ detect_levelshift(.x, threshold = 0.05)) %>%
  ungroup()



bad_ids_s_P<-bp_results005_sPine|>
  na.omit(break_date)|>
  filter(level_shift < 0)|>
  filter(break_date>2018)|>
  filter(level_shift < -0.15)|>
  filter(level_shift == max(level_shift))|>
  pull(id)




NSP_M_filter_ids<- bp_results005_nspMixed|> 
  na.omit()|>
  filter(break_date<=2018| (level_shift < 0  & level_shift > -0.15))|>
  pull(id)



NSP_M_relable_ids<- bp_results005_nspMixed|> 
  na.omit()|>
  filter(break_date>2018)|>
  filter(level_shift <= -0.15)|>
  pull(id)




NSP_filter_ids<- bp_results005_nsp|> 
  na.omit()|>
  filter(break_date<=2018| (level_shift < 0  & level_shift > -0.17))|>
  pull(id)



NSP_relable_ids<- bp_results005_nsp|> 
  na.omit()|>
  filter(break_date>2018)|>
  filter(level_shift <= -0.17 )|>
  pull(id)




#comb|> filter(species=="Norway_spruce_mixed")|>pull(id)|>unique()|> length()

# s_p_relable_ids
# NSP_M_relable_ids|>length()
# NSP_M_filter_ids|>length()

########################################
#           Scots Pine
########################################




s_p_filter_ids<- bp_results005_sPine|> 
  na.omit()|>
  filter(break_date<=2018| (level_shift < 0  & level_shift > -0.11))|>
  pull(id)



s_p_relable_ids<- bp_results005_sPine|> 
  na.omit()|>
  filter(break_date>2018)|>
  filter(level_shift <= -0.11)|>
  pull(id)


# 
# comb|> filter(species=="Scots_pine")|>pull(id)|>unique()|> length()
# 
# s_p_relable_ids|>length()
# s_p_filter_ids|>length()


# 
# 
# i<-31
# 
# 
# 
# comb|>
#   filter( id==s_p_relable_ids[i])|>
#   ggplot()+
# # geom_line(aes(x=as_date(time), y = scale(b12)))+
#   geom_line(aes(x=as_date(time), y = ndvi), col="red")+
#  # geom_point(aes(x=as_date(time), y = scale(b12)))+
#   geom_point(aes(x=as_date(time), y = ndvi), col="red")+
#   labs(title= paste0("Level Shift: ", (bp_results005_sPine|>
#                                          filter(id == s_p_relable_ids[i])|>
#                                          pull(level_shift)) ))
# 
# 

########################################
#           Disturbed
########################################

#comb_monthly_all|>filter(species=="disturbed")|>pull(id)|>length()


bp_results_disturbed <- comb_monthly_all %>%
  filter(species == "disturbed")|>
  group_by(id) %>%
  group_modify(~ detect_levelshift(.x, threshold = 0)) %>%
  ungroup()




# bp_results_disturbed


dist_ids<- bp_results_disturbed|> 
  na.omit()|>
#  filter(break_date>2018)|>
  filter(level_shift > -0.05)|>
  filter(level_shift == min(level_shift))|>
  pull(id)



#TODO ---> diese entfernen 

# i= 100
# 
# comb|>
#   filter( id==del_ids2[i])|>
# #  filter( id==28205 )|>
#   ggplot()+
#   # geom_line(aes(x=as_date(time), y = scale(b12)))+
#   geom_line(aes(x=as_date(time), y = ndvi), col="red")+
#   # geom_point(aes(x=as_date(time), y = scale(b12)))+
#   geom_point(aes(x=as_date(time), y = ndvi), col="red")+
#   labs(title= paste0("Level Shift: ", (bp_results_disturbed|>
#                                          filter(id == dist_ids[i])|>
#                                          pull(level_shift)) ))
# 
# 

######################################
#           Delete Ids
######################################

#------ Dist --------

del_ids1<- bp_results_disturbed|>filter(is.na(break_id))|>pull(id)

del_ids2<- bp_results_disturbed|> 
  na.omit()|>
  filter(level_shift > -0.05)|>
  pull(id)


del_ids_all<-c(del_ids1,
del_ids2,
NSP_filter_ids,
NSP_M_filter_ids,
s_p_filter_ids
)

comb2<- comb|>filter(!id %in% del_ids_all)



######################################
#           Relable Ids
######################################

#------ Dist --------

relable_ids<- c(
  s_p_relable_ids,
  NSP_M_relable_ids,
  NSP_relable_ids
)

comb3<- comb2|> 
  mutate(
    species = ifelse(id %in% relable_ids ,"disturbed",species)
  )

########################################################
comb|> group_by(id)|> dplyr::summarise(species = first(species))|>pull(species)|>table()
comb2|> group_by(id)|> dplyr::summarise(species = first(species))|>pull(species)|>table()
comb3|> group_by(id)|> dplyr::summarise(species = first(species))|>pull(species)|>table()



comb3|>
  group_by(id)|>
  dplyr::summarise(species = first(species))|>
  write_csv("C:/Users/maxki/Documents/Domainproject/data/final_data/final_val/FINAL_validierungs_ids_und_species.csv")


comb3|>
  select(-species, - ndvi)|>
  write_csv("C:/Users/maxki/Documents/Domainproject/data/final_data/final_val/FINAL_Validierungs_Datensatz.csv")




#-------------------------------------------------------------------------------
#                                   test
#-------------------------------------------------------------------------------


test_s <- read_csv("C:/Users/maxki/Documents/Domainproject/data/final_data/final_val/FINAL_validierungs_ids_und_species.csv")


FVD <- read_csv("C:/Users/maxki/Documents/Domainproject/data/final_data/final_val/FINAL_Validierungs_Datensatz.csv")

#FVD|> pull(id)|> unique()|> max()

unique(FVD$id)|>length()
test_s$id|>length()

 
comb_test<- left_join(FVD, test_s, by= "id")





relable_ids

plotlist<-list()
for(sample_id in sample(relable_ids, 10)){
  p<-comb_test|>
    filter(id == sample_id  )|>
    ggplot(aes(x=as_date(time), y = b12))+
    geom_line()+
    geom_point()
  plotlist[[paste0("id",sample_id)]]<- p
}

plotlist









unique(comb_test$id)


train_df<- read_csv("C:/Users/maxki/Documents/Domainproject/data/final_data/for_students/Trainings_Datensatz.csv")

intersect(unique(comb_test$id),unique(train_df$id))





ggplot()+
  geom_line(data = (train_df |> filter(id==11583 )),aes(x=as_date(time),y = b12, col= "train"))+
  geom_line(data = (comb_test|> filter(id==11583 )), aes(x=as_date(time),y = b12, col= "val"))











ts_df_clean <- train_df|>
  dplyr::filter(id == 11583) %>%
  mutate(time = yearmonth(time))|>group_by(time)|> summarise_all(mean)

ts_df_clean2<-ts_df_clean|>tsibble::as_tsibble(index = time) %>%
  tsibble::fill_gaps(.full = TRUE) |> 
  tidyr::fill(b8a, .direction = "downup")
# STL-Decomposition
stl_model <- ts_df_clean2 %>%
  fabletools::model(
    STL(
      b8a ~ trend(window = 84) + season(window = "periodic"),
      robust = TRUE
      )
  )

# Komponenten extrahieren und plotten
stl_model %>%
  fabletools::components() %>%
  feasts::autoplot()
