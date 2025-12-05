################################################################################
#                                 Main
################################################################################



# --- preparing training data ---

# train_data <- read_csv(file = "data/traindata/train_data.csv")
# selected_coords_mask <- read.csv("data/traindata/selected_coords_mask.csv")$selected_coords_mask # filter out coords that might show anomalous patterns
# train_data<-train_data|>filter(coords %in% selected_coords_mask)
# 
# # Savitzky-Golay smoothing and Imputation 

# train_data_final_int <- gf_impute_df_sgma_new(train_data,
#                                               group_key = "coords",
#                                               order = 3,
#                                               aggregate = "week",
#                                               length = NULL,
#                                               names = names,
#                                               padding = FALSE,                # TRUE: gaps > k  -> 0; FALSE: everyting is imputed
#                                               mask = 0,                      # Maskvalue if padding =T: 0 or NA
#                                               k_ma = 4,
#                                               weighting = "exponential",
#                                               dt = FALSE)
# 
#train_data_final_int|>mutate(time = as_date(time))|>write_csv(file = "data/traindata/train_data_imputed.csv")
#--> Skip the previous steps and read the prepared data directly 

train_data_final_int<-read_csv(file = "data/traindata/train_data_imputed.csv")

# # Show the extend of the points
# get_extent(train_data_final_int, color = "green")
# 
# # Filter train data more 
# train_data_final_int|>
#   ggplot(aes(x=as_date(time), y =swir22))+
#   geom_line(aes(group=coords))



train_data_final_int2<- train_data_final_int|>
  filter(year(time)<2024)|>
  group_by(coords)|>
  filter(!any(month(time) %in% c(11,12,1,2) & swir22 >=0.025))|>
  ungroup()

train_data_final_int3<- train_data_final_int2|>
  mutate(ndvi =(nir-red)/(nir+red) )|>
  group_by(coords)|>
  filter(all(ndvi >= 0.55)) |>
  filter(all(swir22 <= 0.056)) |>
  ungroup()



# # Vizualize the new training data
# ggplot()+
#   geom_line(data= train_data_final_int,aes(x = as_date(time), y = swir22, group= coords, col= "total"))+
#   geom_line(data= train_data_final_int2,aes(x = as_date(time), y = swir22, group= coords, col= "swirs filtered"))+
#   geom_line(data= train_data_final_int3,aes(x = as_date(time), y = swir22, group= coords, col= "swirs filtered2"))



################################################################################
#                            Model Training Setup
################################################################################
#--- Setup ---
# dfining the bands we want as inputs
names <- c("time", "nir", "red", "rededge1", "rededge2", "rededge3", "blue", "green", "swir16", "swir22")
names_nt<-names[2:length(names)] # names ithout time
# Defining the sequence length
seq_len<-52

 

# --- Scaling the training data ---
train_data_final_int3<-train_data_final_int3|>select(all_of(names), "coords")
scaler_sgma <- preProcess(train_data_final_int3[names_nt],method = c("range"))
scaler_sgma|> write_rds("models_and_scaler/52sgma_scaler_test.RDS")


# scaling the training data
data4train.scaled<-train_data_final_int3
data4train.scaled[names_nt] <-  predict(scaler_sgma, train_data_final_int3[names_nt]) 




tr_tensor<-gf_data2tensor(data4train.scaled, seq_len =seq_len, step = 52)  # step is the distance between the rolling window that creates the tensors 

# for tensorflow
tr_tensor_tf <- as.array(tr_tensor$to(dtype = torch_float(), device = "cpu"))
tr_tensor_tf
###
dim(tr_tensor_tf)


model_architecture <- function(tr_tensor3d) {
  time_steps <- dim(tr_tensor3d)[2] # length of each batch
  n_variables <- dim(tr_tensor3d)[3] #  Variables in each batch
  
  seq_ae_model <- keras_model_sequential()
  seq_ae_model <- seq_ae_model |> 
    layer_lstm(units = 256, activation = 'tanh', return_sequences = TRUE, input_shape = c(time_steps, n_variables)) |> 
    layer_dropout(rate = 0.2) |> 
    layer_lstm(units = 128, activation = 'tanh', return_sequences = TRUE) |> 
    layer_lstm(units = 64, activation = 'tanh', return_sequences = FALSE) |> 
    layer_repeat_vector(n = time_steps) |> 
    layer_lstm(units = 64, activation = 'tanh', return_sequences = TRUE) |> 
    layer_lstm(units = 128, activation = 'tanh', return_sequences = TRUE) |> 
    layer_dropout(rate = 0.2) |> 
    layer_lstm(units = 256, activation = 'tanh', return_sequences = TRUE) |> 
    time_distributed(layer_dense(units = n_variables, activation = "sigmoid")) |> 
    keras3::compile(optimizer = 'adam', loss = 'mse')
  
  return(seq_ae_model)
}



# ________________________ Train Model ________________________

# --- Initialize Model
remove(lstm_ae)
lstm_ae<- model_architecture(tr_tensor_tf)


# --- fit Model ---
test_model <- lstm_ae|> fit(
  tr_tensor_tf, tr_tensor_tf, 
  epochs = 5, 
  batch_size = 512, 
  validation_split = 0.2,
  shuffle=T, 
  verbose = 1
)

keras::save_model_hdf5(lstm_ae, "models_and_scaler/test_model.h5")
save_model(lstm_ae, "models_and_scaler/test_model.keras")




