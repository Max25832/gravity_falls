################################################################################
#                               Fast Model Inference 
################################################################################

#-------------------------------------------------------------------------------
#                                    Load Data
#-------------------------------------------------------------------------------
data4inference<-read.csv("data/testdata/small_test_set.csv")
data4inference$time<-as.Date(data4inference$time)

# our models don't take b8a as input  
names<-c("time", "nir", "red", "rededge1", "rededge2", "rededge3", "blue","green", "swir16", "swir22")

# store number of coords 
n_coords<-length(unique(data4inference$coords))


#-------------------------------------------------------------------------------
#                                    Load Models
#-------------------------------------------------------------------------------

# --- TensorFlow Model used in Paper ---
# scaler_sgma<-readRDS("models_and_scaler/used_in_paper/scaler52_sgma.rds")
# model_path="models_and_scaler/used_in_paper/sae52_sgma_V1_checkpoint.keras"
# sgma52_model <- keras3::load_model(model_path)

# --- Test Model ---
test_scaler<-readRDS("models_and_scaler/52sgma_scaler_test.RDS")
model_path="models_and_scaler/test_model.keras"
test_model <- keras3::load_model(model_path)


#-------------------------------------------------------------------------------
#                                    Run Inference
#-------------------------------------------------------------------------------
# --- TensorFlow Model ---

used_scaler= test_scaler
used_model = test_model


names_nt<-names[2:length(names)]
data4inference<-data4inference|>select(all_of(names), "coords")
data4inference.scaled<-data4inference
data4inference.scaled[names_nt] <-  predict(used_scaler, data4inference[names_nt]) 

#data4inference.scaled<-data4inference.scaled|>mutate(time=as.Date(time))

# Savitzky-Golay smoothing and Imputation 
#t1 <- Sys.time()
data4inference.imputed <- gf_impute_df_sgma_new(
  data4inference.scaled,
  group_key = "coords",
  order = 3,
  aggregate = "week",
  padding = FALSE,
  dt = FALSE,
  names = names
)

#TODO change yearweek to Date

# t2 <- Sys.time()
# t2 - t1

#data4inference.imputed<-read_csv("data/testdata/small_test_set_imputed.csv")

###
t3<-Sys.time()
reconstructed_data<- gf_reconstruction(model=used_model,
                                       test_data= data4inference.imputed, 
                                       step = 52,seq_len = 52, forward = T, 
                                       model_language = "tf")
t4<-Sys.time()
t4-t3



reconstructed_data$an_score1 <- an_score_column(reconstructed_data, c("nir", "red", "rededge1", "rededge2", "rededge3"), preix= "pred_") # gut 
reconstructed_data$an_score2 <- an_score_column(reconstructed_data, c("nir", "red", "rededge1", "rededge2", "rededge3", "swir16", "swir22"), preix= "pred_")
reconstructed_data$an_score3 <- an_score_column(reconstructed_data, c("nir", "red", "swir16", "swir22"), preix= "pred_")
reconstructed_data$an_score4 <- an_score_column(reconstructed_data, c("nir", "red", "green", "blue", "swir16", "swir22"), preix= "pred_")
reconstructed_data$an_score5 <- an_score_column(reconstructed_data, c("green", "red", "blue"), preix= "pred_")                  
reconstructed_data$an_score6 <- an_score_column(reconstructed_data, c("rededge1", "rededge2", "rededge3"), preix= "pred_")  # bad
reconstructed_data$an_score_all <- an_score_column(reconstructed_data, names[2:length(names)], preix= "pred_")



#reconstructed_data|>write_csv("data/reconstructed_data/reconstructed_data_test_model.csv")


# Plot a time series 

reconstructed_data|>
  filter(year(time)<2023)|>
 # filter(coords == unique(data4inference.imputed$coords)[1500])|>    # Disturbed
#  filter(coords == unique(data4inference.imputed$coords)[2500])|>    # Disturbed
  filter(coords == unique(data4inference.imputed$coords)[3000])|>     # Healthy 
  an_plot(
    an_col = "an_score3",
    obs_col = "swir22",
    prefix = "pred_", 
    th = 0.25)




################################################################################
#                         for near realtime AN detection
################################################################################
# 
# t5<-Sys.time()
# reconstructed_data.backward<- gf_reconstruction(model=used_model,
#                                                 test_data= data4inference.imputed, 
#                                                 step = 52,seq_len = 52, forward = F, 
#                                                 model_language = "tf")
# t6<-Sys.time()
# 
# 
# 
# #reconstructed_data|> 
# reconstructed_data.backward|>
#   filter(coords == unique(data4inference.imputed$coords)[1500])|>
#   ggplot(aes(x=as_date(time)))+
#   geom_line(aes(y=swir22))+
#   geom_line(aes(y=pred_swir22), col= "orange")+
#   geom_line(aes(y=abs(pred_swir22-swir22)), col= "red")
# 
# 
# 




################################################################################
#                       AN Map Creation
################################################################################


tiff52<-reconstructed_data|>
  filter(year(time)<2023)|>
  an_map_to_tiff( th1 = 0.25, rec_err_adj="an_score3", reference_date = "2018-01-01")


tiff52|>plot()
