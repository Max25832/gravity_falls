################################################################################
#                                 Main
################################################################################

train_data_final_int<-read_csv(file = "data/traindata/train_data_imputed.csv")




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





################################################################################
#                            Model Training Setup
################################################################################
#--- Setup ---
# dfining the bands we want as inputs
names <- c("time", "nir",  "red", "rededge1", "rededge2", "rededge3", "blue", "green", "swir16", "swir22")
names_nt<-names[2:length(names)] # names ithout time
# Defining the sequence length
seq_len<-52
seq_len<-104

 

# --- Scaling the training data ---
train_data_final_int3<-train_data_final_int3|>select(all_of(names), "coords")

scaler_sgma<-readRDS("models_and_scaler/52sgma_scaler_test.RDS")

# scaling the training data
data4train.scaled<-train_data_final_int3
data4train.scaled[names_nt] <-  predict(scaler_sgma, train_data_final_int3[names_nt]) 


tr_tensor<-gf_data2tensor(data4train.scaled, seq_len =seq_len, step = 52)  # step is the distance between the rolling window that creates the tensors 

# for tensorflow
tr_tensor_tf <- as.array(tr_tensor$to(dtype = torch_float(), device = "cpu"))
tr_tensor_tf
###
dim(tr_tensor_tf)


attention_model <- function(time_steps, n_variables, D = 64, lr = 1e-3,
                            out_activation = c("sigmoid","linear","tanh")[1]) {
  inputs <- layer_input(shape = c(time_steps, n_variables))
  
  # Encoder
  enc_seq <- inputs %>%
    layer_masking(mask_value = 0) %>%
    layer_lstm(256, return_sequences = TRUE, activation = "tanh") %>%
    layer_dropout(0.2) %>%
    layer_lstm(128, return_sequences = TRUE, activation = "tanh") %>%
    layer_lstm(D,   return_sequences = TRUE, activation = "tanh")   # [B,T,D]
  
  # Attention pooling to a 1-step context (bottleneck)
  q <- enc_seq %>%
    layer_global_average_pooling_1d() %>%   # [B,D]
    layer_dense(units = D, activation = "tanh") %>%
    layer_reshape(target_shape = c(1, D))   # [B,1,D]
  
  context <- layer_attention(list(q, enc_seq), use_scale = TRUE)  # [B,1,D]
  z <- context %>% layer_flatten()                                # [B,D]
  
  # Decoder
  y <- z %>%
    layer_repeat_vector(time_steps) %>%
    layer_lstm(D,   return_sequences = TRUE, activation = "tanh") %>%
    layer_lstm(128, return_sequences = TRUE, activation = "tanh") %>%
    layer_dropout(0.2) %>%
    layer_lstm(256, return_sequences = TRUE, activation = "tanh") %>%
    time_distributed(layer_dense(units = n_variables, activation = out_activation))
  
  model <- keras_model(inputs, y)
  model %>% compile(optimizer = optimizer_adam(learning_rate = lr, clipnorm = 1.0),
                    loss = "mse")
  model
}


# ________________________ Train Model ________________________

# --- Initialize Model
remove(lstm_ae_att)
lstm_ae_att<- attention_model(time_steps = 104,n_variables = 9,D = 64, out_activation = "linear")
lstm_ae_att

# --- fit Model ---
test_model_att <- lstm_ae_att|> fit(
  tr_tensor_tf, tr_tensor_tf, 
  epochs = 10, 
  batch_size = 512, 
  validation_split = 0.2,
  shuffle=T, 
  verbose = 1
)




keras::save_model_hdf5(lstm_ae_att, "models_and_scaler/test_model_att.h5")
save_model(lstm_ae_att, "models_and_scaler/test_model_att.keras")









################################################################################
#                                   Inferenz
################################################################################


# --- Test Model ---
test_scaler<-readRDS("models_and_scaler/52sgma_scaler_test.RDS")
model_path="models_and_scaler/test_model_att.keras"
test_model_att <- keras3::load_model(model_path)


#-------------------------------------------------------------------------------
#                                    Run Inference
#-------------------------------------------------------------------------------
# --- TensorFlow Model ---

used_scaler= test_scaler
used_model = test_model_att


data4inference.imputed<-read_csv("data/testdata/small_test_set_imputed.csv")


# data4inference.imputed<-data4inference.imputed|>
#   mutate(time = convert_week_to_date(time))




reconstructed_data<- gf_reconstruction(model=used_model,
                                       test_data= data4inference.imputed, 
                                       step = 104,seq_len = 104, forward = T, 
                                       model_language = "tf")



reconstructed_data$an_score1 <- an_score_column(reconstructed_data, c("nir", "red", "rededge1", "rededge2", "rededge3"), prefix= "pred_") 
reconstructed_data$an_score2 <- an_score_column(reconstructed_data, c("nir", "red", "rededge1", "rededge2", "rededge3", "swir16", "swir22"), prefix= "pred_")
reconstructed_data$an_score3 <- an_score_column(reconstructed_data, c("nir", "red", "swir16", "swir22"), prefix= "pred_")
reconstructed_data$an_score4 <- an_score_column(reconstructed_data, c("nir", "red", "green", "blue", "swir16", "swir22"), prefix= "pred_")
reconstructed_data$an_score5 <- an_score_column(reconstructed_data, c("green", "red", "blue"), prefix= "pred_")                  
reconstructed_data$an_score6 <- an_score_column(reconstructed_data, c("rededge1", "rededge2", "rededge3"), prefix= "pred_")  # bad
reconstructed_data$an_score_all <- an_score_column(reconstructed_data, names[2:length(names)], prefix= "pred_")



reconstructed_data|>write_csv("data/reconstructed_data/reconstructed_data_test_model.csv")


# Plot a time series 

reconstructed_data|>
#  filter(year(time)<2023)|>
# filter(coords == unique(data4inference.imputed$coords)[1500])|>    # Disturbed
#  filter(coords == unique(data4inference.imputed$coords)[2500])|>    # Disturbed
#  filter(coords == unique(data4inference.imputed$coords)[3000])|>     # Healthy 
  filter(coords == unique(data4inference.imputed$coords)[500])|>     # Healthy 
  an_plot(
    an_col = "an_score3",
    obs_col = "swir22",
    prefix = "pred_", 
    th = 0.25)




################################################################################
#TODO more epochs
#TODO Add WOY cycle encoding 
################################################################################


names<-c(names, "woy_sin","woy_cos")
# --- Scaling the training data with week encoding---
names_nt_wenc<-c(names[2:length(names)]) 

train_data_final_int3_wenc<-train_data_final_int3|>encode_woy()
scaler_wenc<- preProcess(train_data_final_int3_wenc[names_nt_wenc],method = c("range"))


#scaler_wenc|>write_rds("models_and_scaler/52sgma_scaler_wenc.RDS")

# scaling the training data
data4train.scaled_wenc<-train_data_final_int3_wenc
data4train.scaled_wenc[names_nt_wenc] <-  predict(scaler_wenc, train_data_final_int3_wenc[names_nt_wenc]) 

tr_tensor<-gf_data2tensor(data4train.scaled_wenc, seq_len =seq_len, step = 60)  # step is the distance between the rolling window that creates the tensors 

# for tensorflow
tr_tensor_tf <- as.array(tr_tensor$to(dtype = torch_float(), device = "cpu"))
tr_tensor_tf
###
dim(tr_tensor_tf)


# ________________________ Train Model ________________________

# --- Initialize Model
remove(lstm_ae_att_wenc)
lstm_ae_att_wenc<- attention_model(time_steps = 104,n_variables = 11,D = 64, out_activation = "linear")
lstm_ae_att_wenc

# --- fit Model ---
test_model_att_wenc <- lstm_ae_att_wenc|> fit(
  tr_tensor_tf, tr_tensor_tf, 
  epochs = 30, 
  batch_size = 512, 
  validation_split = 0.2,
  shuffle=T, 
  verbose = 1
)




keras::save_model_hdf5(lstm_ae_att_wenc, "models_and_scaler/test_model_att_wenc30ep.h5")
save_model(lstm_ae_att_wenc, "models_and_scaler/test_model_att_wenc30ep.keras")



test_model_att_wenc|>plot()


#-------------------------------------------------------------------------------
#                                    Run Inference
#-------------------------------------------------------------------------------
# --- TensorFlow Model ---


# --- Test Model ---
test_scaler_wenc<-readRDS("models_and_scaler/52sgma_scaler_wenc.RDS")
model_path="models_and_scaler/test_model_att_wenc30ep.keras"
test_model_att_wenc <- keras3::load_model(model_path)


used_scaler= test_scaler_wenc
used_model = lstm_ae_att_wenc


data4inference.imputed<-read_csv("data/testdata/small_test_set_imputed.csv")
data4inference.imputed<-data4inference.imputed|>encode_woy()

used_scaler_12_13 <- preProcess(data4inference.imputed[, c(12, 13)],
                                method = "range")
data4inference.imputed[,c(12,13)] <-  predict(used_scaler_12_13, data4inference[,c(12,13)]) 


reconstructed_data<- gf_reconstruction(model=used_model,
                                       test_data= data4inference.imputed, 
                                       step = 104,seq_len = 104, forward = T, 
                                       model_language = "tf")



reconstructed_data$an_score1 <- an_score_column(reconstructed_data, c("nir", "red", "rededge1", "rededge2", "rededge3"), prefix= "pred_") 
reconstructed_data$an_score2 <- an_score_column(reconstructed_data, c("nir", "red", "rededge1", "rededge2", "rededge3", "swir16", "swir22"), prefix= "pred_")
reconstructed_data$an_score3 <- an_score_column(reconstructed_data, c("nir", "red", "swir16", "swir22"), prefix= "pred_")
reconstructed_data$an_score4 <- an_score_column(reconstructed_data, c("nir", "red", "green", "blue", "swir16", "swir22"), prefix= "pred_")
reconstructed_data$an_score5 <- an_score_column(reconstructed_data, c("green", "red", "blue"), prefix= "pred_")                  
reconstructed_data$an_score6 <- an_score_column(reconstructed_data, c("rededge1", "rededge2", "rededge3"), prefix= "pred_")  # bad
reconstructed_data$an_score_all <- an_score_column(reconstructed_data, names[2:length(names)], prefix= "pred_")



#reconstructed_data|>write_csv("data/reconstructed_data/reconstructed_data_test_att_wenc30.csv")
reconstructed_data2<-read_csv("data/reconstructed_data/reconstructed_data_test_model.csv")


# Plot a time series 

reconstructed_data2|>
  #  filter(year(time)<2023)|>
  # filter(coords == unique(data4inference.imputed$coords)[1500])|>    # Disturbed
  #filter(coords == unique(data4inference.imputed$coords)[2500])|>    # Disturbed
  #  filter(coords == unique(data4inference.imputed$coords)[3000])|>     # Healthy 
#  filter(coords == unique(data4inference.imputed$coords)[610])|>      # spannend 
  filter(coords == unique(reconstructed_data2$coords)[710])|> 
  an_plot(
    an_col = "an_score3",
    obs_col = "red",
    prefix = "pred_", 
    th = 0.25)


reconstructed_data2|>
  filter(coords == unique(reconstructed_data2$coords)[710])|> 
  plot_index_pdscore( 
    defoliation_column= "defoliation",
    an_column = "an_first",
    index="tcw",
    th = -0.03,
    AN_prediction = "an_first",
    legend=F
  )


reconstructed_data|>
  filter(coords == unique(reconstructed_data2$coords)[710])|> 
  an_plot()
################################################################################



library(keras)
library(tensorflow)

# Hilfsfunktion: Sampling-Layer für VAE
sampling_layer <- function(args) {
  mu_z    <- args[[1]]
  logvarz <- args[[2]]
  eps     <- k_random_normal(shape = k_shape(mu_z))
  sigma_z <- k_exp(0.5 * logvarz)
  mu_z + eps * sigma_z
}

# Hilfsfunktion: KL-Loss (Standard-N(0,1)-Prior)
kl_loss_fn <- function(mu_z, logvar_z) {
  # KL pro Zeitstep und Feature
  kl <- -0.5 * k_mean(1 + logvar_z - k_square(mu_z) - k_exp(logvar_z))
  kl
}

# Hauptfunktion: erstellt MA-VAE Modell
build_ma_vae <- function(T = 256L,
                         d_x = 13L,
                         d_z = 64L,   # latent dim
                         d_k = 64L,   # key/query dim
                         num_heads = 8L) {
  
  # ---- Inputs ----
  x_in <- layer_input(shape = c(T, d_x), name = "x_in")
  
  # ---- Encoder: 2x BiLSTM ---- [page:8]
  enc <- x_in %>%
    bidirectional(layer_lstm(512, return_sequences = TRUE), name = "enc_bilstm_1") %>%
    bidirectional(layer_lstm(256, return_sequences = TRUE), name = "enc_bilstm_2")
  
  # ---- Latent-Verteilung (zeitabhängig) ---- [page:4]
  mu_z    <- enc %>% layer_dense(d_z, name = "mu_z")
  logvar_z <- enc %>% layer_dense(d_z, name = "logvar_z")
  
  # Sampling nur im Training, im Inference: mu_z verwenden [page:4][page:6]
  z_sample <- list(mu_z, logvar_z) %>%
    layer_lambda(sampling_layer, name = "z_sample")
  
  # ---- Multi-Head Attention: Q,K aus X, V aus Z ---- [page:4][page:5][page:6]
  # Wir nutzen die eingebaute MultiHeadAttention von tf.keras
  mha <- tf$keras$layers$MultiHeadAttention(
    num_heads      = as.integer(num_heads),
    key_dim        = as.integer(d_k),
    output_shape   = as.integer(d_z),
    name           = "multi_head_attention"
  )
  
  # calls: mha(query, value, key)
  c_ctx <- mha(
    query = x_in,  # Q
    value = z_sample, # V (Z)
    key   = x_in   # K
  )
  
  # ---- Decoder: 2x BiLSTM ---- [page:8]
  dec <- c_ctx %>%
    bidirectional(layer_lstm(256, return_sequences = TRUE), name = "dec_bilstm_1") %>%
    bidirectional(layer_lstm(512, return_sequences = TRUE), name = "dec_bilstm_2")
  
  # Ausgabeverteilung p(X|Z): mu_x, logvar_x [page:4]
  mu_x    <- dec %>% layer_dense(d_x, name = "mu_x")
  logvar_x <- dec %>% layer_dense(d_x, name = "logvar_x")
  
  # Rekonstruktions-Sigma
  sigma_x <- layer_lambda(function(lv) k_exp(0.5 * lv), name = "sigma_x")(logvar_x)
  
  # ---- Custom Loss (ELBO) als Lambda-Layer für Keras compile ---- [page:4][page:7]
  # Negative log-likelihood bei Gaussian mit diag-Cov
  nll_layer <- layer_lambda(
    function(inputs) {
      x_true   <- inputs[[1]]
      mu_x_    <- inputs[[2]]
      logvar_x_ <- inputs[[3]]
      # NLL pro Sample mitteln
      # nll = 0.5 * (log(2*pi) + logvar + (x-mu)^2 / exp(logvar))
      const_term <- 0.5 * log(2 * pi)
      nll_t <- const_term + 0.5 * logvar_x_ +
        0.5 * k_square(x_true - mu_x_) / k_exp(logvar_x_)
      k_mean(nll_t)
    },
    name = "nll_layer"
  )
  
  nll_out <- nll_layer(list(x_in, mu_x, logvar_x))
  
  # KL-Loss
  kl_out <- layer_lambda(
    function(inputs) {
      mu_    <- inputs[[1]]
      logv_  <- inputs[[2]]
      kl_loss_fn(mu_, logv_)
    },
    name = "kl_layer"
  )(list(mu_z, logvar_z))
  
  # Gesamtverlust als zusätzliches Output, damit man ihn beobachten kann
  loss_sum <- layer_lambda(
    function(inputs) {
      nll_ <- inputs[[1]]
      kl_  <- inputs[[2]]
      nll_ + kl_   # hier ohne β; β kann man separat einführen [page:8]
    },
    name = "total_loss"
  )(list(nll_out, kl_out))
  
  # Modell: Input → (mu_x, logvar_x, total_loss)
  model <- keras_model(
    inputs  = x_in,
    outputs = list(mu_x, logvar_x, loss_sum)
  )
  
  # Custom-Loss für compile: benutzt y_true = x_in, y_pred = mu_x/logvar_x
  vae_loss <- function(y_true, y_pred) {
    # y_pred hier ist mu_x; wir nutzen aber die im Graph definierten Loss-Tensoren
    # Trick: nll_out + kl_out sind bereits im Graph; via k_mean zurückgeben
    loss_sum
  }
  
  model %>% compile(
    optimizer = optimizer_adam(),
    loss = list(vae_loss, NULL, NULL),  # nur erster Output für Keras-Loss
    loss_weights = list(1.0, 0.0, 0.0)
  )
  
  model
}





args(an_groups_function)
