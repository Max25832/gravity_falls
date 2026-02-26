################################################################################
#                       Preparation 4 Torch
################################################################################




#_______________________________________________________________________________
# Random Sample Creator   
#_______________________________________________________________________________


gf_group_aware_sample <- function(df, samplesize=0.4, group_key = "coords") {

  
  key_sym  <- ensym(group_key)
  key_name <- as_name(key_sym)
  
  if (!key_name %in% names(df)) {
    stop(paste0("Column '", key_name, "' not found in df."))
  }
  
  groups <- df |>
    dplyr::distinct(!!key_sym) |>
    dplyr::pull(!!key_sym)
  
  n <- length(groups)
  n_samples <- floor(n * samplesize)
  
  groups_sampled <- sample(groups, n_samples)
  
  df_sampled <- df |>
    dplyr::filter((!!key_sym) %in% groups_sampled)
  
  df_rest <- df |>
    dplyr::filter(!(!!key_sym) %in% groups_sampled)
  
  list(
    sampled = df_sampled,
    rest    = df_rest
  )
}


# DF to Torch Tensor for training

create_sequences_by_group <- function(df, seq_len, step, group_col = "coords", time_col = "time", device = "cpu") {
    
    groups <- split(df, df[[group_col]])
    
    seq_list <- list()
    k <- 1
    
    for (g in groups) {
      
      g <- g |> arrange(.data[[time_col]])
      
      x <- as.matrix(g |> select(-all_of(c(group_col, time_col))))
      storage.mode(x) <- "numeric"
      
      max_start <- nrow(x) - seq_len + 1
      if (is.na(max_start) || max_start < 1) next
      
      starts <- seq(1, max_start, by = step)
      
      for (s in starts) {
        seq_list[[k]] <- torch_tensor(
          x[s:(s + seq_len - 1), ],
          dtype = torch_float(),
          device = device
        )
        k <- k + 1
      }
    }
    
    torch_stack(seq_list)
  }
  




#_______________________________________________________________________________
# Model Training 
#_______________________________________________________________________________



train_model <- function(model, train_loader, val_loader, n_epochs, device = "cpu", 
                        lr = 1e-3, crit = "mae", save_model_path = NULL) {
  
  model <- model$to(device = device)
  optimizer <- optim_adam(model$parameters, lr = lr)
  
  criterion <- switch(
    crit,
    "mae" = nn_l1_loss(reduction = "mean"),
    "mse" = nn_mse_loss(reduction = "mean"),
    stop("crit must equal 'mse' or 'mae'.")
  )
  
  history <- list(train = numeric(), val = numeric())
  best_loss <- Inf
  best_model_state <- model$state_dict()
  
  for (epoch in 1:n_epochs) {
    model$train()
    train_loss_sum <- 0
    train_batches <- 0
    
    coro::loop(for (batch in train_loader) {
      seq_true <- batch[[1]]$to(device = device)
      
      optimizer$zero_grad()
      seq_pred <- model(seq_true)
      loss <- criterion(seq_pred, seq_true)
      loss$backward()
      optimizer$step()
      
      train_loss_sum <- train_loss_sum + loss$item()
      train_batches <- train_batches + 1
    })
    
    train_loss <- train_loss_sum / train_batches
    
    # Validation
    model$eval()
    val_loss_sum <- 0
    val_batches <- 0
    
    coro::loop(for (batch in val_loader) {
      seq_true <- batch[[1]]$to(device = device)
      seq_pred <- model(seq_true)
      loss <- criterion(seq_pred, seq_true)
      val_loss_sum <- val_loss_sum + loss$item()
      val_batches <- val_batches + 1
    })
    
    val_loss <- val_loss_sum / val_batches
    
    history$train <- c(history$train, train_loss)
    history$val <- c(history$val, val_loss)
    
    # Ausgabe alle 2 Epochen
    if (epoch %% 2 == 0) {
      cat(sprintf("Epoch %d: train loss %.4f | val loss %.4f\n", epoch, train_loss, val_loss))
    }
    
    # Speichern nur alle 10 Epochen bei Verbesserung
    if (val_loss < best_loss && epoch %% 10 == 0) {
      best_loss <- val_loss
      best_model_state <- model$state_dict()
      if (!is.null(save_model_path)) {
        torch_save(best_model_state, save_model_path)
        cat(sprintf("Model saved at epoch %d to '%s' (val loss: %.4f)\n", epoch, save_model_path, best_loss))
      }
    }
  }
  
  model$load_state_dict(best_model_state)
  model$eval()
  
  list(model = model, history = history)
}



################################################################################
#                                 Model inference
################################################################################




gf_reconstruction_torch <- function(
    model,
    test_data,
    seq_len = 52,
    device = "cpu",
    forward=TRUE,
    group_col= "coords",
    time_col="time"
) {
  
  model$eval()
  model$to(device = device)
  
  # -------------------------------
  # 1) Build sequences
  # -------------------------------
  
  
  # if forward is true the end of time series is cut to create the sequences, else the beginning will be cut  
  if(isTRUE(forward)){
    
    groups <- split(test_data, test_data[[group_col]])
    
    seq_list <- list()
    k <- 1
    
    for (g in groups) {
      
      g <- g |> arrange(.data[[time_col]])
      
      x <- as.matrix(g |> select(-all_of(c(group_col, time_col))))
      storage.mode(x) <- "numeric"
      
      max_start <- nrow(x) - seq_len + 1
      if (is.na(max_start) || max_start < 1) next
      
      starts <- seq(1, max_start, by = seq_len)
      
      for (s in starts) {
        seq_list[[k]] <- torch_tensor(
          x[s:(s + seq_len - 1), ],
          dtype = torch_float(),
          device = device
        )
        k <- k + 1
      }
    }
    
    x <- torch_stack(seq_list)
    ref_idx <- test_data %>%
      arrange(coords, time) %>%
      group_by(coords) %>%
      mutate(row_id = row_number(),
             n_full = n() %/% seq_len,
             keep_n = n_full * seq_len) %>%
      filter(row_id <= keep_n) %>%
      ungroup() %>%
      select(coords, time)
  
   }else{
     
     groups <- split(test_data, test_data[[group_col]])
     
     seq_list <- list()
     k <- 1
     
     for (g in groups) {
       
       g <- g |> arrange(.data[[time_col]])
       
       x <- as.matrix(g |> select(-all_of(c(group_col, time_col))))
       storage.mode(x) <- "numeric"
       
       n <- nrow(x)
       n_full <- n %/% seq_len
       if (n_full < 1) next
       start_idx <- n - n_full * seq_len + 1
       max_start <- n - seq_len + 1
       
       starts <- seq(start_idx, max_start, by = seq_len)
       
       for (s in starts) {
         seq_list[[k]] <- torch_tensor(
           x[s:(s + seq_len - 1), ],
           dtype = torch_float(),
           device = device
         )
         k <- k + 1
       }
     }
     
     x <- torch_stack(seq_list)
     ref_idx <- test_data %>%
       arrange(coords, time) %>%
       group_by(coords) %>%
       mutate(
         total_rows = n(),
         n_full = total_rows %/% seq_len,
         keep_n = seq_len * n_full,
         min_start = total_rows - keep_n + 1
       ) %>%
       # exakt dieselben Zeilen wie dataset_backward()
       slice(min_start:total_rows) %>% 
       ungroup() %>%
       select(coords, time)
   }

  # -------------------------------
  # 2) Forward pass (NO predict())
  # -------------------------------
  with_no_grad({
    y_hat <- model(x)
  })
  
  
  # -------------------------------
  # 3) Flatten back to rows
  # -------------------------------
  pred_array <- y_hat$reshape(c(-1, y_hat$size(3))) |> as_array()
  pred_df <- as.data.frame(pred_array)
  colnames(pred_df) <- paste0("rec_", names[!names %in% "time"])
  
  
  
  bind_cols(ref_idx, pred_df) |>
    left_join(test_data, by = c("coords", "time")) |>
    arrange(coords, time)
}





################################################################################
#                           Imputation
################################################################################




gf_impute_df_sgma_new <- function(
    df,
    group_key = "coords",
    order = 3,
    aggregate = c("week", "month", "year"),
    length = NULL,
    names = c("time","b2","b3","b4","b5","b6","b7","b8","b8a","b11","b12"),
    padding = FALSE,                # TRUE: >k Lücken -> 0; FALSE: alles interpoliert
    mask = 0,                      # Wert für lange Lücken, z.B. 0 oder NA
    k_ma = 4,
    weighting = "exponential",
    dt = TRUE                      # Wenn TRUE: Zeitabstand dt hinzufügen
) {
  aggregate <- rlang::arg_match(aggregate)
  key_sym   <- rlang::ensym(group_key)
  key_name  <- rlang::as_name(key_sym)
  
  # --- Checks ---
  if (!key_name %in% names(df)) {
    rlang::abort(paste0("Spalte '", key_name, "' nicht im DataFrame vorhanden."))
  }
  if (!"time" %in% names(df)) {
    rlang::abort("Spalte 'time' fehlt im DataFrame (wird für die Aggregation benötigt).")
  }
  
  # Aggregationsfunktion
  tfun <- switch(
    aggregate,
    week  = tsibble::yearweek,
    month = tsibble::yearmonth,
    year  = tsibble::year
  )
  
  # gültige Savitzky–Golay Fensterlänge: ungerade und > order
  n_win <- if (is.null(length)) 2L * order + 1L else as.integer(length)
  if (n_win %% 2L == 0L) n_win <- n_win + 1L
  if (n_win <= order)    n_win <- order + 3L + as.integer(((order + 3L) %% 2L) == 0L)
  
  # --- Aggregation ---
  out <- df |>
    dplyr::select(!!key_sym, dplyr::all_of(names)) |>
    dplyr::mutate(time = tfun(.data$time)) |>
    dplyr::group_by(!!key_sym, .data$time) |>
    dplyr::summarise(
      dplyr::across(dplyr::where(is.numeric), ~ mean(.x, na.rm = TRUE)),
      .groups = "drop"
    ) |>
    tidyr::drop_na()
  
  # Messspalten (ohne key/time)
  non_metric <- c(key_name, "time")
  metric_idx <- setdiff(seq_along(out), match(non_metric, names(out)))
  metric_idx <- metric_idx[vapply(out[metric_idx], is.numeric, logical(1))]
  num_cols   <- names(out)[metric_idx]
  
  # Helper: kurze Lücken imputieren, lange -> mask
  impute_small_gaps_then_zero <- function(x, k = 4, weighting = "exponential") {
    if (all(!is.na(x))) return(x)
    na_run <- is.na(x)
    r <- rle(na_run)
    lenrep <- rep(r$lengths, r$lengths)
    long_gap <- na_run & (lenrep > k)
    xi <- imputeTS::na_ma(x, k = k, weighting = weighting)
    if (any(long_gap)) xi[long_gap] <- mask
    xi
  }
  
  # --- Gruppen splitten ---
  gsplit <- out |>
    dplyr::group_by(!!key_sym) |>
    dplyr::group_split(.keep = TRUE)
  
  # --- Progress Bar + Verarbeitung (keine Parallelisierung) ---
  pbapply::pboptions(type = "txt")
  glist <- pbapply::pblapply(gsplit, function(gdf) {
    
    # 1) Optional: echte Zeitdifferenz dt VOR der Imputation berechnen
    if (isTRUE(dt)) {
      gdf <- gdf |>
        dplyr::arrange(time) |>
        dplyr::mutate(
          dt = as.numeric(time - dplyr::lag(time, default = first(time))))
    }
    
    # 2) Savitzky–Golay Glättung je Messspalte
    for (nm in num_cols) {
      x <- gdf[[nm]]
      if (sum(!is.na(x)) >= n_win)
        gdf[[nm]] <- signal::sgolayfilt(x, p = order, n = n_win)
    }
    
    # 3) Gaps explizit machen
    gts <- tsibble::as_tsibble(gdf, index = time, key = !!key_sym) |>
      tsibble::fill_gaps()
    
    # 4) Imputation
    if (isTRUE(padding)) {
      gts <- dplyr::mutate(
        gts,
        dplyr::across(
          dplyr::all_of(num_cols),
          ~ impute_small_gaps_then_zero(.x, k = k_ma, weighting = weighting)
        )
      )
    } else {
      gts <- dplyr::mutate(
        gts,
        dplyr::across(
          dplyr::all_of(num_cols),
          ~ imputeTS::na_ma(.x, k = 4, weighting = weighting)
        )
      )
    }
    

    as.data.frame(gts)
  })
  
  res <- dplyr::bind_rows(glist) |>
    dplyr::arrange(!!key_sym, time)
  if(isTRUE(dt)){
    res<-res|>mutate(dt = ifelse(dt<= k_ma+1,mask,dt))
  }
  return(res)
}






