set.seed(42)
torch_manual_seed(42)


################################################################################
#                           Main Torch4R Model 
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

get_extent(train_data_final_int)
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



# Vizualize the new training data
# ggplot()+
#   geom_line(data= train_data_final_int,aes(x = as_date(time), y = swir22, group= coords, col= "total"))+
#   geom_line(data= train_data_final_int2,aes(x = as_date(time), y = swir22, group= coords, col= "swirs filtered"))+
#   geom_line(data= train_data_final_int3,aes(x = as_date(time), y = swir22, group= coords, col= "swirs filtered2"))
# 


################################################################################
#                            Model Training Setup
################################################################################
#--- Setup ---
# dfining the bands we want as inputs
names <- c("time","blue", "green", "red", "rededge1", "rededge2", "rededge3",  "nir", "B8A", "swir16", "swir22")
names_nt<-names[2:length(names)] # names ithout time
# Defining the sequence length
seq_len<-52
# batch size for the dataloaders
batch_size = 256
#_______________________________________________________________________________
#                             Input Data 
#_______________________________________________________________________________
train_data_final_int3<-train_data_final_int3|>select(all_of(names), "coords")

# train Val split: 70/30 split
dat <- gf_group_aware_sample(train_data_final_int3,samplesize = 0.3,group_key = "coords")



# --- Scaling the training data ---
scaler <- preProcess(dat$rest[names_nt],method = c("range"))
#scaler_sgma|> write_rds("models_and_scaler/52sgma_scaler_test.RDS")


# scaling the training data
data4train.scaled<-dat$rest
data4train.scaled[names_nt] <-  predict(scaler_sgma, data4train.scaled[names_nt]) 

# scaling the validation data
data4val.scaled<-dat$sampled
data4val.scaled[names_nt] <-  predict(scaler_sgma, data4val.scaled[names_nt]) 

# step = 40
# step = 13

# to tensor 
tr_tensor <- create_sequences_by_group(
  data4train.scaled, 
  seq_len =seq_len,
  step = 13,
  group_col = "coords")

val_tensor <- create_sequences_by_group(
  data4val.scaled, 
  seq_len =seq_len,
  step = 13,
  group_col = "coords")




# ________________________Train Data________________________

# Dataloader
tr_loader <- dataloader(
  dataset = tensor_dataset(tr_tensor),
  batch_size = batch_size,
  shuffle = FALSE
)

# 4. Dataloader für Validation
val_loader <- dataloader(
  dataset = tensor_dataset(val_tensor),
  batch_size = batch_size,
  shuffle = FALSE)



# ________________________ Train Model ________________________

# todo -> ML Flow checken nach besten ergebnissen 

# --- Initialize Model
remove(ae_model)
remove(ae_model_seeded)
ae_model <- lstm_ae(seq_len = dim(tr_tensor)[2], n_features = dim(tr_tensor)[3], embedding_dim = 64, dropout = 0.2)
ae_model_seeded <- lstm_ae_seeded(seq_len = dim(tr_tensor)[2], n_features = dim(tr_tensor)[3], embedding_dim = 64, dropout = 0.2)


# --- fit Model ---
ae_model_trained <- ae_model|> train_model(
  train_loader = tr_loader,
  val_loader = val_loader,
  n_epochs = 120,
  device = "cpu",
  lr = 1e-3,
  crit ="mae",
  save_model_path = "models_and_scaler/torch/m1_step13.pt"  
)

ae_model_trained_seeded <- ae_model_seeded|> train_model(
  train_loader = tr_loader,
  val_loader = val_loader,
  n_epochs = 120,
  device = "cpu",
  lr = 1e-3,
  crit ="mae",
  save_model_path = "models_and_scaler/torch/m1_seeded__step13.pt"  
)


hist<-data.frame(ae_model_trained$history) 
hist|>ggplot(aes(x=1:120))+
  geom_line(aes(y=train, col= "train"))+
  geom_line(aes(y=val, col= "val"))

histseeded<-data.frame(ae_model_trained_seeded$history) 
histseeded|>ggplot(aes(x=1:120))+
  geom_line(aes(y=train, col= "train"))+
  geom_line(aes(y=val, col= "val"))

# ________________________ Model Inference ________________________


test_data<-read_csv("data/testdata/small_test_set2.csv")
# 
# t1<-Sys.time()
# # test_data.imputed <-test_data|>gf_impute_df_sgma_new(group_key = "coords",names = names,padding = F,aggregate ="week")
# t2<-Sys.time()
# imp_time<-t2-t1
#imp_time  Time difference of -6.965974 mins

#test_data.imputed|> mutate(time= as.Date(time))|> write_csv(file = "data/testdata/small_test_set_imputed2.csv")
test_data.imputed<- read_csv(file = "data/testdata/small_test_set_imputed2.csv")

test_data.imputed<- test_data.imputed|>select(all_of(names), "coords")


test_data.imputed.scaled<- test_data.imputed
test_data.imputed.scaled[names_nt] <-  predict(scaler_sgma, test_data.imputed.scaled[names_nt]) 



test_data_sample<-test_data.imputed.scaled|>
  filter(coords%in% unique(test_data2$coords)[300:600])

modeltr<-ae_model_trained$model
modeltr_seeded<-ae_model_trained_seeded$model

rec_dat <- gf_reconstruction_torch(model = modeltr, test_data = test_data_sample,seq_len = 52, forward = F,group_col = "coords")
rec_dat_seeded <- gf_reconstruction_torch(model = modeltr_seeded, test_data = test_data_sample,seq_len = 52, forward = F,group_col = "coords")


ts<-rec_dat|>
  filter(coords == unique(rec_dat$coords)[15])

ts_seeded<-rec_dat_seeded|>
  filter(coords == unique(rec_dat$coords)[15])

# 10,12 gut, 11, kritisch


ggplot()+
  geom_line(data= ts, aes(x= as_date(time),y=rec_swir22, col= "ogAE"))+
  geom_line(data= ts_seeded, aes(x= as_date(time),y=rec_swir22, col= "seeded"))+
  geom_line(data= ts, aes(x= as_date(time), y=swir22, col= "normal"))+
  scale_color_manual(values = c("ogAE" = "orange", "seeded"= "blue", "normal" = "black"))
 

