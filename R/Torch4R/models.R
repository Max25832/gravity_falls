library(torch)

################################################################################
#                             Torch 4 R 
################################################################################
# Encoder <- nn_module(
#   "Encoder",
#   initialize = function(seq_len, n_features, embedding_dim = 64, dropout = 0.2, bottleneck= NULL) {
#     
#     if(is.null(bottleneck)){
#       bn = embedding_dim 
#     }else{bn = bottleneck}
#     
#     
#     self$seq_len <- seq_len
#     self$bn <- bn
#     self$n_features <- n_features
#     self$embedding_dim <- embedding_dim
#     self$hidden_dim1 <- 2 * embedding_dim
#     self$hidden_dim2 <- 4 * embedding_dim
#     
#     self$rnn1 <- nn_lstm(
#       input_size = n_features,
#       hidden_size = self$hidden_dim2,
#       batch_first = TRUE
#     )
#     self$rnn2 <- nn_lstm(
#       input_size = self$hidden_dim2,
#       hidden_size = self$hidden_dim1,
#       batch_first = TRUE
#     )
#     self$rnn3 <- nn_lstm(
#       input_size = self$hidden_dim1,
#       hidden_size = self$bn, 
#       batch_first = TRUE
#     )
#     self$dropout <- nn_dropout(p = dropout)
#     self$mask <- NULL
#     
#   },
#   
#   forward = function(x) {
#     x <- self$rnn1(x)[[1]]
#     x <- self$dropout(x)
#     x <- self$rnn2(x)[[1]]
#     hidden_n <- self$rnn3(x)[[2]][[1]]
#     hidden_n$squeeze(1)  # shape: [batch_size, embedding_dim]
#     h_last <- hidden_n[hidden_n$size(1), , ]  # [batch, bn]
#     h_last
#   }
# )


################################################################################
# Decoder 
################################################################################

Decoder <- nn_module(
  "Decoder",
  initialize = function(seq_len, input_dim = 64, n_features = 9, dropout = 0.2, bottleneck= NULL) {

    
    if(is.null(bottleneck)){
      bn = input_dim 
    }else{bn = bottleneck}
    
    
    
    self$seq_len <- seq_len
    self$bn <- bn
    self$input_dim <- input_dim
    self$hidden_dim1 <- 2 * input_dim
    self$hidden_dim2 <- 4 * input_dim
    self$n_features <- n_features
    
    self$rnn1 <- nn_lstm(
      input_size = self$bn,
      hidden_size = self$input_dim,
      batch_first = TRUE
    )
    self$rnn2 <- nn_lstm(
      input_size = self$input_dim,
      hidden_size = self$hidden_dim1,
      batch_first = TRUE
    )
    self$rnn3 <- nn_lstm(
      input_size = self$hidden_dim1,
      hidden_size = self$hidden_dim2,
      batch_first = TRUE
    )
    
    self$dropout <- nn_dropout(p = dropout)
    self$output_layer <- nn_linear(self$hidden_dim2, n_features)
  },
  
  forward = function(x) {
    batch_size <- x$size(1)
    x <- x$unsqueeze(2)  # [batch_size, 1, input_dim]
    #  x <- torch_tile(x, c(1, self$seq_len, 1))  # [batch_size, seq_len, input_dim]
    # x$repeat  --> niht available
    
    # Wiederhole x seq_len-mal entlang der zweiten Dimension
#    x <- torch_cat(replicate(self$seq_len, x, simplify = FALSE), dim = 2)
    x <- x$expand(c(x$size(1), self$seq_len, self$bn))$contiguous()
    x <- self$rnn1(x)[[1]]
    x <- self$rnn2(x)[[1]]
    x <- self$dropout(x)
    x <- self$rnn3(x)[[1]]
    
    self$output_layer(x)  # [batch, seq_len, n_features]
  }
)








################################################################################
# Autoencoder 
################################################################################


lstm_ae <- nn_module(
  "LSTM_Autoencoder",
  initialize = function(seq_len, n_features, embedding_dim = 64, dropout = 0.2, bottleneck= NULL) {
    
    self$encoder <- Encoder(seq_len = seq_len,n_features = n_features, embedding_dim = embedding_dim,dropout = dropout, bottleneck = bottleneck)
    self$decoder <- Decoder(seq_len = seq_len, input_dim = embedding_dim, n_features = n_features, dropout = dropout, bottleneck = bottleneck )
      },
  
  forward = function(x) {
    x <- self$encoder(x)
    x <- self$decoder(x)
    x
  }
)



################################################################################
#                               Varriante 2 
################################################################################

#' in the previous variant we used the prediction of the last value as the input for the first layer of the decoder and predicted it 
#' 
#' 
#' 
#' 

Encoder_seeded <- nn_module(
  "Encoder_Seeded",
  initialize = function(seq_len, n_features, embedding_dim = 64, dropout = 0.2, bottleneck = NULL) {
    
    if (is.null(bottleneck)) bn <- embedding_dim else bn <- bottleneck
    
    self$seq_len <- seq_len
    self$bn <- bn
    self$n_features <- n_features
    self$embedding_dim <- embedding_dim
    self$hidden_dim1 <- 2 * embedding_dim
    self$hidden_dim2 <- 4 * embedding_dim
    
    self$rnn1 <- nn_lstm(input_size = n_features,      hidden_size = self$hidden_dim2, batch_first = TRUE)
    self$rnn2 <- nn_lstm(input_size = self$hidden_dim2, hidden_size = self$hidden_dim1, batch_first = TRUE)
    self$rnn3 <- nn_lstm(input_size = self$hidden_dim1, hidden_size = self$bn,          batch_first = TRUE)
    
    self$dropout <- nn_dropout(p = dropout)
    self$mask <- NULL
  },
  
  forward = function(x) {
    x <- self$rnn1(x)[[1]]
    x <- self$dropout(x)
    x <- self$rnn2(x)[[1]]
    
    out3 <- self$rnn3(x)
    h_n <- out3[[2]][[1]]  # [num_layers, batch, bn]
    c_n <- out3[[2]][[2]]  # [num_layers, batch, bn]
    
    z <- h_n[h_n$size(1), , ]  # [batch, bn] (last Layer-Representation)
    
    list(z = z, h0 = h_n, c0 = c_n)
  }
)


Decoder_seeded <- nn_module(
  "Decoder_Seeded",
  initialize = function(seq_len, input_dim = 64, n_features = 9, dropout = 0.2, bottleneck = NULL) {
    
    if (is.null(bottleneck)) bn <- input_dim else bn <- bottleneck
    
    self$seq_len <- seq_len
    self$bn <- bn
    self$input_dim <- input_dim
    self$hidden_dim1 <- 2 * input_dim
    self$hidden_dim2 <- 4 * input_dim
    self$n_features <- n_features
    
    self$rnn1 <- nn_lstm(input_size = self$bn,         hidden_size = self$bn,   batch_first = TRUE)
    self$rnn2 <- nn_lstm(input_size = self$input_dim,  hidden_size = self$hidden_dim1, batch_first = TRUE)
    self$rnn3 <- nn_lstm(input_size = self$hidden_dim1, hidden_size = self$hidden_dim2, batch_first = TRUE)
    
    self$dropout <- nn_dropout(p = dropout)
    self$output_layer <- nn_linear(self$hidden_dim2, n_features)
  },
  
  forward = function(z, h0, c0) {
    # z: [batch, bn] ; h0/c0: [num_layers, batch, input_dim] 
    # 
    # if (self$bn != self$input_dim) {
    #   stop("Für hx-Übergabe ohne Mapping muss bn == input_dim gelten. Setze bottleneck = NULL oder bottleneck == embedding_dim.")
    # }
    
    x <- z$unsqueeze(2)  # [batch, 1, bn]
    x <- x$expand(c(x$size(1), self$seq_len, self$bn))$contiguous()
    
    out1 <- self$rnn1(x, hx = list(h0, c0))
    x <- out1[[1]]
    
    x <- self$rnn2(x)[[1]]
    x <- self$dropout(x)
    x <- self$rnn3(x)[[1]]
    
    self$output_layer(x)
  }
)



#_______________________________________________________________________________
# Autoencoder
#_______________________________________________________________________________


lstm_ae_seeded <- nn_module(
  "LSTM_Autoencoder_Seeded",
  initialize = function(seq_len, n_features, embedding_dim = 64, dropout = 0.2, bottleneck = NULL) {
    
    self$encoder_seeded <- Encoder_seeded(
      seq_len = seq_len,
      n_features = n_features,
      embedding_dim = embedding_dim,
      dropout = dropout,
      bottleneck = bottleneck
    )
    
    self$decoder_seeded <- Decoder_seeded(
      seq_len = seq_len,
      input_dim = embedding_dim,
      n_features = n_features,
      dropout = dropout,
      bottleneck = bottleneck
    )
  },
  
  forward = function(x) {
    enc <- self$encoder_seeded(x)
    self$decoder_seeded(enc$z, enc$h0, enc$c0)
  }
)
































################################################################################
# Encoder 
################################################################################
Encoder <- nn_module(
  "Encoder",
  initialize = function(seq_len, n_features, embedding_dim = 64, dropout = 0.2, masking=TRUE) {
    self$seq_len <- seq_len
    self$n_features <- n_features
    self$embedding_dim <- embedding_dim
    self$hidden_dim1 <- 2 * embedding_dim
    self$hidden_dim2 <- 4 * embedding_dim
    
    self$rnn1 <- nn_lstm(
      input_size = n_features,
      hidden_size = self$hidden_dim2,
      batch_first = TRUE
    )
    self$rnn2 <- nn_lstm(
      input_size = self$hidden_dim2,
      hidden_size = self$hidden_dim1,
      batch_first = TRUE
    )
    self$rnn3 <- nn_lstm(
      input_size = self$hidden_dim1,
      hidden_size = embedding_dim,
      batch_first = TRUE
    )
    self$dropout <- nn_dropout(p = dropout)
    self$mask <- NULL
    
  },
  
  forward = function(x) {
    x <- self$rnn1(x)[[1]]
    x <- self$dropout(x)
    x <- self$rnn2(x)[[1]]
    hidden_n <- self$rnn3(x)[[2]][[1]]
    hidden_n$squeeze(1)  # shape: [batch_size, embedding_dim]
  }
)



################################################################################
# Decoder 
################################################################################

Decoder <- nn_module(
  "Decoder",
  initialize = function(seq_len, input_dim = 64, n_features = 9, dropout = 0.2) {
    self$seq_len <- seq_len
    self$input_dim <- input_dim
    self$hidden_dim1 <- 2 * input_dim
    self$hidden_dim2 <- 4 * input_dim
    self$n_features <- n_features
    
    self$rnn1 <- nn_lstm(
      input_size = input_dim,
      hidden_size = input_dim,
      batch_first = TRUE
    )
    self$rnn2 <- nn_lstm(
      input_size = input_dim,
      hidden_size = self$hidden_dim1,
      batch_first = TRUE
    )
    self$rnn3 <- nn_lstm(
      input_size = self$hidden_dim1,
      hidden_size = self$hidden_dim2,
      batch_first = TRUE
    )
    
    self$dropout <- nn_dropout(p = dropout)
    self$output_layer <- nn_linear(self$hidden_dim2, n_features)
  },
  
  forward = function(x) {
    batch_size <- x$size(1)
    x <- x$unsqueeze(2)  # [batch_size, 1, input_dim]
    #  x <- torch_tile(x, c(1, self$seq_len, 1))  # [batch_size, seq_len, input_dim]
    # x$repeat  --> niht available
    
    # Wiederhole x seq_len-mal entlang der zweiten Dimension
    x <- torch_cat(replicate(self$seq_len, x, simplify = FALSE), dim = 2)
    x <- self$rnn1(x)[[1]]
    x <- self$rnn2(x)[[1]]
    x <- self$dropout(x)
    x <- self$rnn3(x)[[1]]
    
    self$output_layer(x)  # [batch, seq_len, n_features]
  }
)

################################################################################
# Autoencoder 
################################################################################


lstm_ae <- nn_module(
  "LSTM_Autoencoder",
  initialize = function(seq_len, n_features, embedding_dim = 64, dropout = 0.2) {
    self$encoder <- Encoder(seq_len, n_features, embedding_dim, dropout)
    self$decoder <- Decoder(seq_len, embedding_dim, n_features, dropout)
  },
  
  forward = function(x) {
    x <- self$encoder(x)
    x <- self$decoder(x)
    x
  }
)



