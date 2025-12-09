


convert_week_to_date <- function(x) {
  # "2020 W01" → "2020-W01-1"
  x2 <- gsub(" ", "-", x)
  ISOweek2date(paste0(x2, "-1"))
}


encode_woy <- function(df) {
  df<-df|>mutate(woy = week(time))
  woy_sin <- sin(2 * pi * df$woy / 52)
  woy_cos <- cos(2 * pi * df$woy / 52)
  df1<-data.frame(woy_sin,woy_cos)
  
  df<-cbind(df,df1)
  df<-df|>select(-woy)
  
  return(df)
  
  
}


################################################################################
#                             Gap Filling Functions
################################################################################


gf_impute_df_sgma_new <- function(
    df,
    group_key = "coords",
    order = 3,
    aggregate = c("week", "month", "year"),
    length = NULL,
    names = c("time","b2","b3","b4","b5","b6","b7","b8","b8a","b11","b12"),
    padding = TRUE,                # TRUE: >k Lücken -> 0; FALSE: alles interpoliert
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
    
    # # 4) Optional: Zeitdifferenz dt hinzufügen
    # if (isTRUE(dt)) {
    #   gts <- gts |>
    #     dplyr::arrange(time) |>
    #     dplyr::mutate(dt = as.numeric(time - dplyr::lag(time, default = first(time))))
    # }
    
    as.data.frame(gts)
  })
  
  res <- dplyr::bind_rows(glist) |>
    dplyr::arrange(!!key_sym, time)
  if(isTRUE(dt)){
    res<-res|>mutate(dt = ifelse(dt<= k_ma+1,mask,dt))
  }
  return(res)
}


################################################################################
#                             Deep Learninng Functions
################################################################################
gf_data2tensor<-function(df, seq_len = seq_len, step = step) {
  coord_seqs <- map(
    unique(df$coords),
    function(coord) {
      ts <- df |>
        filter(coords == coord) |>
        ungroup() |>
        select(-coords, -time)
      x <- as.matrix(ts)
      storage.mode(x) <- "numeric"
      max_start <- nrow(x) - seq_len + 1
      if (is.na(max_start) || max_start < 1) return(NULL)
      starts <- seq(1, max_start, by = step)
      map(starts, ~ torch_tensor(x[.x:(.x + seq_len - 1), ], dtype = torch_float()))
    }
  )
  seqs <- flatten(coord_seqs)
  seqs <- compact(seqs) 
  torch_stack(seqs)
  }



gf_reconstruction<- function(model,
                             test_data,step=52,
                             seq_len=52,
                             forward=TRUE, 
                             model_language = c("tf", "torch")[1]
){
  
  # if forward is true the end of time series is cut to create the sequences, else the beginning will be cut  
  if(isTRUE(forward)){
    dataset_forward<-function(df, seq_len = seq_len, step = step) {
      coord_seqs <- map(
        unique(df$coords),
        function(coord) {
          ts <- df |>
            filter(coords == coord) |>
            ungroup() |>
            select(-coords, -time)
          x <- as.matrix(ts)
          storage.mode(x) <- "numeric"
          max_start <- nrow(x) - seq_len + 1
          if (is.na(max_start) || max_start < 1) return(NULL)
          starts <- seq(1, max_start, by = step)
          map(starts, ~ torch_tensor(x[.x:(.x + seq_len - 1), ], dtype = torch_float()))
        }
      )
      seqs <- flatten(coord_seqs)
      seqs <- compact(seqs) 
      torch_stack(seqs)
    }
    test_tens<-test_data|>dataset_forward(seq_len = seq_len,step = step)
    
    
    ref_idx <- test_data %>%
      arrange(coords, time) %>%
      group_by(coords) %>%
      mutate(row_id = row_number(),
             n_full = n() %/% step,
             keep_n = n_full * step) %>%
      filter(row_id <= keep_n) %>%
      ungroup() %>%
      select(coords, time)
    
  }else{
    dataset_backward<-function(df, seq_len = seq_len, step = step) {
      coord_seqs <- map(
        unique(df$coords),
        function(coord) {
          ts <- df |>
            filter(coords == coord) |>
            ungroup() |>
            select(-coords, -time)
          x <- as.matrix(ts)
          storage.mode(x) <- "numeric"
          max_start <- nrow(x) - seq_len + 1
          min_start <- nrow(x)-(seq_len*(nrow(x) %/% seq_len))
          if (is.na(max_start) || max_start < 1) return(NULL)
          starts <- seq(min_start, nrow(x)-1, by = step)
          map(starts, ~ torch_tensor(x[.x:(.x + seq_len - 1), ], dtype = torch_float()))
        }
      )
      seqs <- flatten(coord_seqs)
      seqs <- compact(seqs) 
      torch_stack(seqs)
    }
    
    test_tens<-test_data|>dataset_backward(seq_len = seq_len,step = step)
    
    #DONE dataset backwards=T --> muss ändern, dass die input zeitreihe gleich wie die output zeitreihe abgeschnitten wird 
    
    ref_idx <- test_data %>%
      arrange(coords, time) %>%
      group_by(coords) %>%
      mutate(
        total_rows = n(),
        n_full = total_rows %/% step,
        keep_n = seq_len * n_full,
        min_start = total_rows - keep_n + 1
      ) %>%
      # exakt dieselben Zeilen wie dataset_backward()
      slice(min_start:total_rows) %>% 
      ungroup() %>%
      select(coords, time)
    
  }
  
  # torch tensor as arry 
  if(model_language=="tf"){
    test_tens <- as.array(test_tens$to(dtype = torch_float(), device = "cpu"))
  }
  
  # run model
  y_hat <- predict(model, test_tens)  # [W, 52, F]
  
  # flatten windows back to rows in same order as ref_idx
  W <- dim(y_hat)[1]; Tt <- dim(y_hat)[2]; Ff <- dim(y_hat)[3]
  pred_mat <- do.call(rbind, lapply(seq_len(W), function(i) y_hat[i,,]))  # [W*52, F]
  pred_df  <- as.data.frame(pred_mat)
  
  names_nt <- setdiff(names(test_data), c("time", "coords"))
  pred_names_nt<-lapply(names_nt, function(x){paste0("pred_", x)})
  colnames(pred_df) <- pred_names_nt  # your variable names
  
  pred_with_meta <- bind_cols(ref_idx, pred_df) %>%
    arrange(coords, time)
  pred_with_meta<-left_join(test_data, pred_with_meta, by = c("coords", "time"))
  
  
  
  return(pred_with_meta)
}








################################################################################
# Cleaner function for GEE data
################################################################################

iso_clean_fast <- function(df, q = 0.98, ntrees = 100) {
  stopifnot("time" %in% names(df))
  
  
  numeric_cols <- names(df)[sapply(df, is.numeric)]
  num_data <- as.data.table(df[, numeric_cols, drop = FALSE])
  scaled_data <- scale(num_data)
  
  n_cols <- ncol(num_data)
  keep_mask <- matrix(TRUE, nrow = nrow(num_data), ncol = n_cols)
  
  # Progress bar 
  pb <- progress_bar$new(
    format = "→ Berechne Isolation Forests [:bar] :percent | Spalte :current/:total",
    total = n_cols,
    clear = FALSE,
    width = 60
  )
  
  # loop over all columns Spalten
  for (j in seq_len(n_cols)) {
    col_j <- scaled_data[, j, drop = FALSE]
    model <- isolation.forest(col_j, ntrees = ntrees, ndim = 1, missing_action = "impute")
    preds <- predict(model, col_j)
    thresh <- quantile(preds, probs = q)
    keep_mask[, j] <- preds <= thresh
    
    pb$tick()  # Fortschritt anzeigen
  }
  
  # Nur Zeilen behalten, die in allen Spalten unterhalb des Schwellenwerts liegen
  keep <- rowSums(!keep_mask) == 0
  clean_df <- df[keep, , drop = FALSE]
  
  return(clean_df)
}

multipoint_data_cleaner <- function(df) {
  clean_funct<-function(df){
    df<-as.data.frame(df)
    n=ncol(df)
    for(i in c(2:n)){
      df[,i]<-tsclean(df[,i])
    }
    return(df)
  }
  
  iso_clean<-function(df){
    # Convert to time series data
    time <- date(df$time)
    data <- df|> dplyr::select(-c(time))
    scaled_data <- scale(data)
    model <- isolation.forest(scaled_data,ndim=NULL, ntrees=100,
                              missing_action="impute")
    preds<-predict(model, scaled_data)
    df<-data.frame(df,anomaly=preds)
    thresh<-quantile(preds, probs = .98)
    df<-df[df$anomaly<=thresh,]
    
    df<-df|> dplyr::select(-anomaly)
    return(df)
  }
  
  # combining all previously applied functions 
  clean_ts_function <- function(df) {
    df<-df|> 
      as.data.frame()|>
      clean_funct()|>    # decide if I let both cleaning methods run or just the isolation forest 
      iso_clean()
    return(df)
  }
  
  
  cc_nest <-df|> group_by(coords)|> nest()
  
  # Create a progress bar
  pb <- progress_bar$new(total = nrow(cc_nest))
  
  cc_nest <- cc_nest |> 
    mutate(data = purrr::map(data, function(data) {
      pb$tick()  # Increment progress bar
      clean_ts_function(data)
    }))
  
  a <- cc_nest |> unnest(cols = c(data))|> as.data.frame()
  return(a)
}


geo2coords <- function(string) {
  # Extract the part after "[" and before "]" and replace commas with slashes
  result <- gsub(",", "/", sub(".*\\[(.*?)\\].*", "\\1", string))
  return(result)
}

#____________________________Calculating Indices________________________________ 


indices<-function(ts_adj){
  ts_adj<-ts_adj|> mutate(
    ndvi = (nir - red)/(nir + red),
    # Normalized Difference Moisture Index 
    ndmi= (nir - swir16)/(nir + swir16),
    # Normalized Difference Water Index 
    ndwi= (nir - green)/(nir + green),
    # Enhanced Vegetation Index 
    #evi2 = 2.5*(nir - red)/(nir + red + 1),
    # Enhanced Vegetation Index 
    evi = 2.5*((nir - red)/(nir + 6 * red  -7.5 *blue+1)),
    # Tasseled Cap Wetness
    tcw = (0.1509 * blue + 0.1973 * green + 0.3279 * red + 0.3406 * nir - 0.7112 * swir16 - 0.4572 * swir22),
    # tc transformations after Nedkov (2017)
    s2tcw= (0.1363 * blue + 0.2802 * green + 0.3072 * red -0.0807 * nir - 0.4064 * swir16 - 0.5602 * swir22),
    s2tcg= (-0.1128 * blue -0.1680 * green - 0.3480 * red+ 0.3165 * nir - 0.4578 * swir16 - 0.4046 * swir22),
    s2tcb=(0.0822 * blue +0.1360 * green+ 0.2611 * red+ 0.3895 * nir +0.3882 * swir16 +0.1366 *swir22),
    # normalized difference red edge index
    #ndre1=  (nir-rededge1)/(nir + rededge1),
    # red edge inflection point 
    #reip1 = (700 + 40 * ((red + rededge3)/2 - rededge1) / (rededge2 - rededge1)),
    # DSWI (Disease Water Stress Index) 
    dwsi = (nir-green)/(swir16+red),
    # 
    srswir = (swir16)/(swir22),
    srswir_2 = (nir)/(swir16),
    srswir_3 = ((nir)-(swir16))/((nir)+(swir16)),
    # Sentinel2  Red-Edge Position
    s2rep =  (705 + 35 * ((red + rededge3)/2 - rededge1) / (rededge2 - rededge1))
    #s2rep =  (705 + 35 * (((red + rededge3)/2) - rededge1 / (rededge2 - rededge1)))
  )
  
  return(ts_adj)
}              

################################################################################
#                       Random Sample Creator   
################################################################################

sample_coords_selector <- function(df, samplesize=0.4) {
  dfc<- df|> select(coords)
  coords <- unique(dfc[,1])$coords
  n<- length(coords)
  n_samples=n*samplesize
  coords_sampled<-sample(coords,n_samples)
  df_sampled <- df |> filter(coords %in% coords_sampled)
  return(df_sampled)
  
}

################################################################################
#                               Visualisation Functions 
################################################################################




coord_str_2_32632<-function(input_string){
  
  # Split the string to extract latitude and longitude
  coords <- strsplit(input_string, "/")[[1]]
  longitude <- as.numeric(coords[1])
  latitude <- as.numeric(coords[2])
  
  # Create a data frame with the coordinates
  df <- data.frame(lon = longitude, lat = latitude)
  
  # Convert to spatial object (assuming input coordinates are in WGS84)
  sf_object <- st_as_sf(df, coords = c("lon", "lat"), crs = 4326)
  
  # Transform coordinates to EPSG:32632
  sf_transformed <- st_transform(sf_object, crs = 32632)
  
  # Extract the transformed coordinates
  transformed_coords <- st_coordinates(sf_transformed)
  
  # Format the result as "X: ..., Y: ..."
  result_string <- paste0("X: ", transformed_coords[1, "X"], ", Y: ", transformed_coords[1, "Y"], " (EPGS: 32632)")
  return(result_string)
}


#_________________________Extent_of_map_________________________________________


get_extent<-function(df, color = "blue"){
  
  id11_spat<-df|>
    select(coords)|> 
    mutate(col=1)|>
    group_by(coords)|>
    summarise_all(mean)|> 
    as.data.frame()|>
    separate(coords, into= c("lon", "lat"), sep= "/")|>
    st_as_sf(coords = c("lon", "lat"), crs = 4326)|> ungroup()|> st_as_sf()
  
  
  # to spatial data frame  
  id11_spdf <- as(id11_spat, "Spatial")
  
  # Define the new CRS
  new_crs <- "+proj=utm +zone=32 +datum=WGS84"
  # Reproject poly3 to EPSG 32632
  id11_utm <- spTransform(id11_spdf, CRS(new_crs))
  
  r_extent <- extent(id11_utm)
  # Create an empty raster with the same extent and resolution as poly3_utm
  r <- raster(r_extent, res = c(10.5,10.5))
  # Rasterize poly3_utm to fill the raster cells with point values
  h_r <- rasterize(id11_utm, r, field = "col")  # Use any field you want as values
  
  # to terra raster 
  rast_h<-rast(h_r)
  crs(rast_h)<-new_crs
  
  
  
  leaflet()|>
    addProviderTiles("Esri.WorldImagery")|>
    addRasterImage(rast_h, col= color,  opacity = 0.5) 
}



get_extent_points <- function(df_list, color = "red") {
  # DataFrames zu sf-Objekten konvertieren
  sf_list <- lapply(df_list, function(df) {
    df |>
      select(coords) |>
      mutate(col = 1) |>
      group_by(coords) |>
      summarise_all(mean) |>
      separate(coords, into = c("lon", "lat"), sep = "/") |>
      mutate(lon = as.numeric(lon), lat = as.numeric(lat)) |>
      st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
      ungroup()
  })
  
  # Leaflet-Karte initialisieren
  map <- leaflet() |>
    addProviderTiles("Esri.WorldImagery")
  
  # Jeden Punkt-Datensatz zur Karte hinzufügen
  for(i in seq_along(sf_list)) {
    map <- map |>
      addCircleMarkers(data = sf_list[[i]], radius = 3,
                       color = color[min(i, length(color))], opacity = 0.8)
  }
  
  # Finales Map-Objekt zurückgeben
  map
}

# # example: 

#get_extent_points(list(df1, df2), color = c("red", "blue"))


an_map_to_tiff<-function(rec, th1 = 0.5, rec_err_adj="an_score3", reference_date = NULL, na = NA){
  
  first_an_funct <- function(x, rec_err_adj, th0) {
    x <- x[order(x$time), ]
    
    err <- x[[rec_err_adj]]
    next1 <- dplyr::lead(err, 1)
    next2 <- dplyr::lead(err, 2)
    next3 <- dplyr::lead(err, 3)
    
    keep <- err > th0 & next1 > th0 & (next2 > th0 | next3 > th0)
    
    # Stelle sicher: keep ist logisch und gültig
    keep[is.na(keep)] <- FALSE
    
    if (sum(keep) > 0) {
      return(min(x$time[keep], na.rm = TRUE))
    } else {
      return(as.Date(NA))  # erzwingt konsistenten Typ: Date
    }
  }
  

  
  
  col_map_input<-rec|> dplyr::select(coords, time, all_of(rec_err_adj))|> as.data.table()

  result <- col_map_input[, .(first_an_time = first_an_funct(.SD,  rec_err_adj= rec_err_adj, th0 = th1)), by = coords]
  
  # time as values after 2018
  if(is.null(reference_date)){
  reference_date <- min(yearmonth(rec$time))
  }else{
  reference_date<-yearmonth(reference_date)
  }
  # Convert the 'time' column to days since the reference date
  col_map_input4 <- result|>
    mutate(time = as.numeric(yearmonth(first_an_time) - yearmonth(reference_date)))
  
  col_map_input4<-col_map_input4|> mutate(time= ifelse(is.na(time), na, time))
  
  # Convert to sf object if it is not already
  
  
  col_map_input4<- separate(col_map_input4, coords, into = c("lon", "lat"), sep = "/")
  
  # convert to spatial data frame 
  col_map_input4 <- st_as_sf(col_map_input4, coords = c("lon", "lat"), crs = st_crs('+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0'))
  
  
  
  col_map_sp <- st_as_sf(col_map_input4)
  
  # to spatial data frame  
  col_map_spdf <- as(col_map_sp, "Spatial")
  
  # Define the new CRS
  new_crs <- "+proj=utm +zone=32 +datum=WGS84"
  # Reproject poly3 to EPSG 32632
  cm_utm <- spTransform(col_map_spdf, CRS(new_crs))
  cm_utm$time<-as.numeric(cm_utm$time)
  r_extent <- extent(cm_utm)
  # Create an empty raster with the same extent and resolution as poly3_utm
  r <- raster(r_extent, res = c(10,10))
  # Rasterize poly3_utm to fill the raster cells with point values
  
  cm_r <- rasterize(cm_utm, r, field = "time", fun= mean )  
  crs(cm_r)<-"+proj=utm +zone=32 +datum=WGS84"
  # to terra raster 
  rast_cm<-rast(cm_r)
  return(rast_cm)
}




################################################################################
#                                 Figures
################################################################################

gf_anomaly_plot<-function(ts, features = 10, ncol=2, calculate_errors= T, combined = F){
  

if(!"first_an" %in% names(ts)){
  if(!"ndvi" %in% names(ts)){
    ts$ndvi <- ((ts$nir-ts$red)/(ts$nir+ts$red))
    }
  ts$first_an <- ifelse(ts$ndvi<0.5& lag(ts$ndvi)<0.5,1,0)
}


p0<-ts|> ggplot(aes(as_date(time)))+
  geom_line(aes(y=nir), size= 0.7)+
  geom_line(aes(y=rec_nir), col="orange", size= 0.8)+
  stat_difference(aes(x = as_date(time), y = nir, ymax = nir, ymin = rec_nir, fill = "Reconstruction Error"), alpha = 0.1) +
  geom_vline(data =(ts[ts$first_an==1,]),  aes(xintercept = as_date(time), col="Defoliation"), size= 2, alpha = 0.4)+
  scale_color_manual(values = c("Defoliation" = "red")) +
  scale_fill_manual(values = c("Reconstruction Error" = "#E61E1E")) +
  theme_bw()+
  theme(legend.position = "none")


p1<-ts|> ggplot(aes(as_date(time)))+
  geom_line(aes(y=B8A), size= 0.7)+
  geom_line(aes(y=rec_B8A), col="orange", size= 0.8)+
  stat_difference(aes(x = as_date(time), y = B8A, ymax = B8A, ymin = rec_B8A, fill = "Reconstruction Error"), alpha = 0.1) +
  geom_vline(data =(ts[ts$first_an==1,]),  aes(xintercept = as_date(time), col="Defoliation"), size= 2, alpha = 0.4)+
  scale_color_manual(values = c("Defoliation" = "red")) +
  scale_fill_manual(values = c("Reconstruction Error" = "#E61E1E")) +
  theme_bw()+
  theme(legend.position = "none")


p2<-ts|> ggplot(aes(as_date(time)))+
  geom_line(aes(y=red), size= 0.7)+
  geom_line(aes(y=rec_red), col="orange", size= 0.8)+
  stat_difference(aes(x = as_date(time), y = red, ymax = red, ymin = rec_red, fill = "Reconstruction Error"), alpha = 0.1) +
  geom_vline(data =(ts[ts$first_an==1,]),  aes(xintercept = as_date(time), col="Defoliation"), size= 2, alpha = 0.4)+
  scale_color_manual(values = c("Defoliation" = "red")) +
  scale_fill_manual(values = c("Reconstruction Error" = "#E61E1E")) +
  theme_bw()+
  theme(legend.position = "none")

p3<-ts|> ggplot(aes(as_date(time)))+
  geom_line(aes(y=blue), size= 0.7)+
  geom_line(aes(y=rec_blue), col="orange", size= 0.8)+
  stat_difference(aes(x = as_date(time), y = blue, ymax = blue, ymin = rec_blue, fill = "Reconstruction Error"), alpha = 0.1) +
  geom_vline(data =(ts[ts$first_an==1,]),  aes(xintercept = as_date(time), col="Defoliation"), size= 2, alpha = 0.4)+
  scale_color_manual(values = c("Defoliation" = "red")) +
  scale_fill_manual(values = c("Reconstruction Error" = "#E61E1E")) +
  theme_bw()+
  theme(legend.position = "none")

p4<-ts|> ggplot(aes(as_date(time)))+
  geom_line(aes(y=green), size= 0.7)+
  geom_line(aes(y=rec_green), col="orange", size= 0.8)+
  stat_difference(aes(x = as_date(time), y = green, ymax = green, ymin = rec_green, fill = "Reconstruction Error"), alpha = 0.1) +
  geom_vline(data =(ts[ts$first_an==1,]),  aes(xintercept = as_date(time), col="Defoliation"), size= 2, alpha = 0.4)+
  scale_color_manual(values = c("Defoliation" = "red")) +
  scale_fill_manual(values = c("Reconstruction Error" = "#E61E1E")) +
  theme_bw()+
  theme(legend.position = "none")

p5<-ts|> ggplot(aes(as_date(time)))+
  geom_line(aes(y=rededge1), size= 0.7)+
  geom_line(aes(y=rec_rededge1), col="orange", size= 0.8)+
  stat_difference(aes(x = as_date(time), y = rededge1, ymax = rededge1, ymin = rec_rededge1, fill = "Reconstruction Error"), alpha = 0.1) +
  geom_vline(data =(ts[ts$first_an==1,]),  aes(xintercept = as_date(time), col="Defoliation"), size= 2, alpha = 0.4)+
  scale_color_manual(values = c("Defoliation" = "red")) +
  scale_fill_manual(values = c("Reconstruction Error" = "#E61E1E")) +
  theme_bw()+
  theme(legend.position = "none")

p6<-ts|> ggplot(aes(as_date(time)))+
  geom_line(aes(y=rededge2), size= 0.7)+
  geom_line(aes(y=rec_rededge2), col="orange", size= 0.8)+
  stat_difference(aes(x = as_date(time), y = rededge2, ymax = rededge2, ymin = rec_rededge2, fill = "Reconstruction Error"), alpha = 0.1) +
  geom_vline(data =(ts[ts$first_an==1,]),  aes(xintercept = as_date(time), col="Defoliation"), size= 2, alpha = 0.4)+
  scale_color_manual(values = c("Defoliation" = "red")) +
  scale_fill_manual(values = c("Reconstruction Error" = "#E61E1E")) +
  theme_bw()+
  theme(legend.position = "none")

p7<-ts|> ggplot(aes(as_date(time)))+
  geom_line(aes(y=rededge3), size= 0.7)+
  geom_line(aes(y=rec_rededge3), col="orange", size= 0.8)+
  stat_difference(aes(x = as_date(time), y = rededge3, ymax = rededge3, ymin = rec_rededge3, fill = "Reconstruction Error"), alpha = 0.1) +
  geom_vline(data =(ts[ts$first_an==1,]),  aes(xintercept = as_date(time), col="Defoliation"), size= 2, alpha = 0.4)+
  scale_color_manual(values = c("Defoliation" = "red")) +
  scale_fill_manual(values = c("Reconstruction Error" = "#E61E1E")) +
  theme_bw()+
  theme(legend.position = "none")

p8<-ts|> ggplot(aes(as_date(time)))+
  geom_line(aes(y=swir16), size= 0.7)+
  geom_line(aes(y=rec_swir16), col="orange", size= 0.8)+
  stat_difference(aes(x = as_date(time), y = swir16, ymax = swir16, ymin = rec_swir16, fill = "Reconstruction Error"), alpha = 0.1) +
  geom_vline(data =(ts[ts$first_an==1,]),  aes(xintercept = as_date(time), col="Defoliation"), size= 2, alpha = 0.4)+
  scale_color_manual(values = c("Defoliation" = "red")) +
  scale_fill_manual(values = c("Reconstruction Error" = "#E61E1E")) +
  theme_bw()+
  theme(legend.position = "none")

p9<-ts|> ggplot(aes(as_date(time)))+
  geom_line(aes(y=swir22, color = "Original"), size= 0.7)+
  geom_line(aes(y=rec_swir22, col="Reconstruction"), size= 0.8)+
  stat_difference(aes(x = as_date(time), y = swir22, ymax = swir22, ymin = rec_swir22, fill = "Difference"), alpha = 0.1) +
  geom_vline(data =(ts[ts$first_an==1,]),  aes(xintercept = as_date(time), col="Defoliation"), size= 2, alpha = 0.4)+
  scale_fill_manual(
    name = "",  # gleicher Titel = gemeinsame Legende
    values = c("Difference" = "#E61E1E")
    
  ) +
  scale_color_manual(
    name = "",  # gemeinsamer Legendentitel
    values = c(
      "Original" = "black",
      "Reconstruction" = "orange",
      "Difference" = "#E61E1E",
      "Defoliation" = "red"
    )
  ) +
  
  guides(
    fill = guide_legend(override.aes = list(color = "#E61E1E", fill = "#E61E1E")),
    color = guide_legend())+
  theme_bw() +
  theme(
    axis.title.y = element_text(size = 20),  
    legend.position = c(0.01, 0.02),
    legend.justification = c("left", "bottom"),
    #legend.spacing.y =  unit(-4, "pt"),
    legend.key.size = unit(22,"pt"),
    legend.text   = element_text(size = 14),  # Schriftgröße Legende
    
    legend.background = element_rect(fill="white"),
    legend.title = element_blank(), 
    #legend.margin = margin(t = -6, b = -6),  # tighten inside legend box
    legend.spacing = unit(5, "pt")  # reduce spacing between title and keys (horizontal)
    
  )



  if(calculate_errors){
  ts$rec_err_adj1= an_score_column(ts,cols = c("nir", "red", "rededge1", "rededge2", "rededge3"))
  ts$rec_err_adj2= an_score_column(ts,cols =  c("nir", "red", "rededge1", "rededge2", "rededge3", "swir16", "swir22"))
  ts$rec_err_adj3= an_score_column(ts, cols = c("rededge1", "rededge2", "rededge3"))
  ts$rec_err_adj4= an_score_column( ts,cols = c("nir", "red", "swir16", "swir22"))
  ts$rec_err= an_score_column(ts,cols = colnames(ts)[3:(3+features)])
  }


cols <- c(
  "Rec. Err." = "#8F00FF",
  "Rec. Err. Adj. 1" = "#FFDD00",
  "Rec. Err. Adj. 2" = "#FF006D",
  "Rec. Err. Adj. 3" = "#ADFF02",
  "Rec. Err. Adj. 4" = "#01BEFE"
)


rec_p <- ts |>
  ggplot(aes(as_date(time))) +
  geom_line(aes(y = rec_err, colour = "Rec. Err."), size = 0.7) +
  stat_difference(aes(y = rec_err, ymax = rec_err, ymin = 0, fill = "Rec. Err."), alpha = 0.1) +
  
  geom_line(aes(y = rec_err_adj1, colour = "Rec. Err. Adj. 1"), size = 0.7) +
  stat_difference(aes(y = rec_err_adj1, ymax = rec_err_adj1, ymin = 0, fill = "Rec. Err. Adj. 1"), alpha = 0.1) +
  
  geom_line(aes(y = rec_err_adj2, colour = "Rec. Err. Adj. 2"), size = 0.7) +
  stat_difference(aes(y = rec_err_adj2, ymax = rec_err_adj2, ymin = 0, fill = "Rec. Err. Adj. 2"), alpha = 0.1) +
  
  geom_line(aes(y = rec_err_adj3, colour = "Rec. Err. Adj. 3"), size = 0.7) +
  stat_difference(aes(y = rec_err_adj3, ymax = rec_err_adj3, ymin = 0, fill = "Rec. Err. Adj. 3"), alpha = 0.1) +
  
  geom_line(aes(y = rec_err_adj4, colour = "Rec. Err. Adj. 4"), size = 0.7) +
  stat_difference(aes(y = rec_err_adj4, ymax = rec_err_adj4, ymin = 0, fill = "Rec. Err. Adj. 4"), alpha = 0.1) +
  
  geom_vline(data = ts[ts$first_an == 1, ], 
             aes(xintercept = as_date(time)), 
             color = "red", size = 2, alpha = 0.4) +
  
  scale_color_manual(name = "", values = cols) +
  scale_fill_manual(name = "", values = cols) +
  guides(
    fill  = guide_legend(override.aes = list(alpha = 0.1)),
    color = guide_legend()
  )+
  # scale_color_manual(name = "", values = c(
  #   "Rec. Err." = "#8F00FF",
  #   "Rec. Err. Adj. 1" = "#FFDD00",
  #   "Rec. Err. Adj. 2" = "#FF006D",
  #   "Rec. Err. Adj. 3" = "#ADFF02",
  #   "Rec. Err. Adj. 4" = "#01BEFE"
  # )) +
  # 
  # scale_fill_manual(name = "", values = c(
  #   "Rec. Err." = "#8F00FF",
  #   "Rec. Err. Adj. 1" = "#FFDD00",
  #   "Rec. Err. Adj. 2" = "#FF006D",
  #   "Rec. Err. Adj. 3" = "#ADFF02",
  #   "Rec. Err. Adj. 4" = "#01BEFE"
  # )) +
  # 
  # guides(
  #   fill = guide_legend(override.aes = list(
  #     color = NA,
  #     fill = c("#8F00FF", "#FFDD00", "#FF006D", "#ADFF02", "#01BEFE"),
  #     alpha = 0.1
  #   )),
  #   color = guide_legend(override.aes = list(
  #     color =c("#8F00FF", "#FFDD00", "#FF006D", "#ADFF02", "#01BEFE")))) +
  
  labs(color = "", fill = "") +
  theme_bw() +
  theme(
    panel.border = element_rect(colour = "black", fill = NA, size = 2),  # schwarzer Rahmen
    # axis.text.y = element_text(size = 10),   # Y-Achsen-Beschriftung größer
    axis.title.y = element_text(size = 20),  
    #    legend.margin = units(2, "pt"),
    legend.position = c(0.01, 0.02),
    legend.key.size = unit(22,"pt"),  # 15
    legend.justification = c("left", "bottom"),
    legend.text   = element_text(size = 14),  # Schriftgröße Legende
    legend.title = element_blank(), 
    #legend.margin = margin(t = -6, b = -6),  # tighten inside legend box
    legend.spacing = unit(5, "pt")  # reduce spacing between title and keys (horizontal) ### 0
  )



plots <- list(p0,p1, p2, p3, p4, p5, p6, p7, p8, p9 , rec_p)
plots <- lapply(plots, function(p) p + theme(axis.title.x = element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank(),     axis.title.y = element_text(size = 20)))

# # Arrange with reduced spacing
#ggarrange(plotlist = plots, ncol = 1, heights = rep(1, 9), align = "v")
if (combined) {
  ggg <- ggpubr::ggarrange(plotlist = plots, ncol = ncol, align = "v")
  return(ggg)
} else {
  return(plots)
}
}



an_plot<-function(reconstructed_data,
                  an_col = "an_score3",
                  obs_col = "swir22",
                  prefix = "pred_",
                  th = 0.25
){
  
  pred= paste0(prefix,obs_col)
  
  
  facet_labs <- setNames(
    c("Reconstruction", "Anomaly Score"),   
    c(obs_col, an_col))
  
  
  
  reconstructed_data|> 
    select(time, all_of(c(an_col, obs_col, pred)))|>
    pivot_longer(cols = all_of(c(an_col, obs_col)))|>
    mutate(!!pred:=ifelse(name== an_col, NA,.data[[pred]]),
           pred_fac = pred,
           name = factor(name, levels = c(obs_col, an_col))
           )|>
    ggplot(aes(x = as_date(time))) +
    geom_line(aes(y = value, color = name)) +
    geom_line(aes(y = .data[[pred]], color = pred_fac)) +
    
    geom_hline(
      data = data.frame(name = an_col, yint = th),
      aes(yintercept = yint, color= "Threshold"),
      linetype = "dashed"
    ) +
    
 
    scale_color_manual(
      values = setNames(
        c("red", "black", "orange", "purple"),
        c(an_col, obs_col, pred, "Threshold")
      )
    )+
    
    theme_bw()+
    facet_wrap(~name, ncol=1,
               labeller = as_labeller(facet_labs),
               scales= "free_y")
}



multiplier_plot<-function(ts, AN_prediction= "an_first",  defoliation = "defoliation",pd_fx_column="fx") {
  
  yy<-ts
  # changing names so that the wanted column gets plotted
  yy$AN_pred <- ts[[AN_prediction]]
  yy$first_an<- ts[[defoliation]]
  yy$fx <- ts[[pd_fx_column]]
  
  # Check if AN == 1 or AN_pred == 1 exists
  defoliation_time <- if (any(yy$first_an == 1)) {
    yy[yy$first_an == 1, ]$time
  } else {
    NA
  }
  
  anomaly_prediction_time <- if (any(yy$AN_pred == 1)) {
    yy[yy$AN_pred == 1, ]$time
  } else {
    NA
  }

    # Generate the plot
  yy |>
    ggplot(aes(x = as_date(time), y = fx)) +
    geom_line() +
    # Add defoliation line only if AN == 1 exists
    geom_vline(aes(xintercept = as_date(defoliation_time), col = "Defoliation"), size = 1.2)+
    # Add anomaly points only if AN_pred == 1 exists
    geom_point(data = yy[yy$AN_pred == 1, ],
               aes(x = as_date(time), y = fx, col = "Anomaly Prediction"),
               shape = 4, size = 2, stroke = 1.2)  +
    geom_text(data = yy[yy$AN_pred == 1, ], aes(label = round(fx,4)), vjust = 2, size = 5, color = "black")+
    # Dynamic title
    labs(x = "time",
         y = "Multiplication Factor",
         color = ""
    ) +
    theme_bw() +
    scale_color_manual(values = c("Defoliation" = "red", "Anomaly Prediction" = "blue")) +
    theme(
      legend.position ="right"
    )
}



plot_index_pdscore <- function(ts,
                               index = c("tcw", "ndvi"),
                               defoliation_column = "defoliation",
                               an_column = "an_first",
                               th = -0.03,
                               AN_prediction = "an_first") {
  index <- match.arg(index)
  
  ## 1) Defoliation-Index bestimmen ---------------------------------------
  def_idx_vec <- which(ts[[defoliation_column]] == 1)
  
  if (length(def_idx_vec) == 0) {
    # keine Defoliation gefunden -> virtuelle Defoliation am Ende
    defoliation_time_index <- nrow(ts)
  } else {
    defoliation_time_index <- def_idx_vec[1]
  }
  
  ## 2) PD-Score berechnen -------------------------------------------------
  ts_temp <- ts |>
    dplyr::mutate(fx = pd_score_equation(seq_len(dplyr::n()), defoliation_time_index))
  
  ## 3) Index-TS bauen -----------------------------------------------------
  index_ts <- ts_temp |>
    dplyr::select(time, dplyr::all_of(c(index, an_column, defoliation_column))) |>
    tsibble::as_tsibble(index = time) |>
    stats::na.omit()
  
  ## 4) Oberer Plot: Index -------------------------------------------------
  tcw_s <- ggplot2::ggplot() +
    ggplot2::geom_line(
      data = index_ts,
      ggplot2::aes(
        x = lubridate::as_date(time),
        y = .data[[index]],
        colour = index
      ),
      na.rm = TRUE
    ) +
    ggplot2::geom_hline(
      ggplot2::aes(yintercept = th, colour = "Threshold"),
      size = 0.8,
      linetype = "dashed"
    ) +
    ggplot2::geom_vline(
      data = dplyr::filter(index_ts, .data[[an_column]] == 1),
      ggplot2::aes(
        xintercept = lubridate::as_date(time),
        colour = "Model detected AN"
      ),
      size = 2.5,
      alpha = 0.4
    ) +
    # wenn es keine defoliation==1 gibt, ist dieses data-Subset einfach leer -> keine Linie
    ggplot2::geom_vline(
      data = index_ts[index_ts[[defoliation_column]] == 1, ],
      ggplot2::aes(
        xintercept = lubridate::as_date(time),
        colour = "Defoliation"
      ),
      size = 2.5,
      alpha = 0.4
    ) +
    ggplot2::labs(y = index, x = NULL, colour = NULL) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.title          = ggplot2::element_text(size = 15),
      axis.text           = ggplot2::element_text(size = 14),
      legend.title        = ggplot2::element_text(size = 16),
      legend.text         = ggplot2::element_text(size = 14),
      strip.text.x        = ggplot2::element_text(size = 16),
      legend.position     = c(0.01, 0.01),
      legend.justification = c("left", "bottom")
    ) +
    ggplot2::scale_color_manual(
      values = stats::setNames(
        c("forestgreen", "red", "blue", "maroon"),
        c(index, "Defoliation", "Model detected AN", "Threshold")
      )
    )
  
  ## 5) Unterer Plot: PD-Score --------------------------------------------
  pdscore <- ts_temp |>
    multiplier_plot(
      AN_prediction = AN_prediction,
      defoliation   = defoliation_column,
      pd_fx_column  = "fx"
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.title          = ggplot2::element_text(size = 15),
      axis.text           = ggplot2::element_text(size = 14),
      legend.title        = ggplot2::element_text(size = 16),
      legend.text         = ggplot2::element_text(size = 14),
      strip.text.x        = ggplot2::element_text(size = 16),
      legend.position     = c(0.01, 0.99),
      legend.justification = c("left", "top")
    ) +
    ggplot2::labs(
      title    = NULL,
      subtitle = NULL,
      y        = "Pre-Defoliation Score",
      x        = "Time"
    )
  
  ## 6) Kombinieren --------------------------------------------------------
  ggpubr::ggarrange(tcw_s, pdscore, nrow = 2, align = "v")
}


################################################################################
#                                 Evaluation Functions
################################################################################


################################################################################
#                                   Label Defoliation
################################################################################

defoliation_indices<-function(ts, weekly =T){
  ts$time<-as.Date(ts$time)
  if(isTRUE(weekly)){
    ts<-ts|>
      dplyr::mutate(time = yearweek(time)) |>
      dplyr::group_by(time) |>
      dplyr::summarise(
        dplyr::across(dplyr::where(is.numeric), ~ mean(.x, na.rm = TRUE)),
        .groups = "drop")
  }
  result <- ts |>
    mutate(
      ndvi = (nir - red) / (nir + red),
      tcw = 0.1509 * blue + 0.1973 * green + 0.3279 * red + 
        0.3406 * nir - 0.7112 * swir16 - 0.4572 * swir22,
      defoliation = ifelse(ndvi < 0.55 | tcw < -0.03, 1, 0))
  result<- result|>mutate(first_defo= ifelse(defoliation ==1 & lead(defoliation)==1& lead(defoliation,2)==1, 1, 0))
  result$temp<- 0
  result[is.na(result$first_defo),]$first_defo<- 0
  if(any(result$first_defo==1)){
    result[result$first_defo==1,][1,]$temp<- 1
  }
  result$defoliation <- result$temp
  result<-result|> select(!all_of(c("first_defo", "temp")))
  result$time<-as.Date(result$time)
  return(result)
}

################################################################################
#                                   PD -Score
################################################################################


#   Equation
pd_score_equation<-function(anomaly_date,defoliation_date ,k=52, shift =2, FP_value= -1){
  
  lag <- anomaly_date-defoliation_date
  score <- (-3 / ((2 * k) + shift)) * (lag - shift)
  
  cond1 <- abs(lag -shift) > ((2/3)*k)
  cond2 <- ((lag -shift)  < (-(2/3)*(k))) & ((lag+shift) >= (-(k)))
  
  score[cond1] <- FP_value
  score[cond2] <- 1
  
  return(score)
}


pd_score_add_score_column<-function(ts, defoliation_column= "defoliation", an_column= "an") {
  ts$pd_score_fx <- 0  # Initialisiere mit Nullen
  ts$an_temp<- ts[[an_column]]
  ts$defo_temp<- ts[[defoliation_column]]
  n_time_index <- which(ts$defo_temp == 1)[1]
  anomaly_indices <- which(ts$an_temp == 1)
  
  if (is.na(n_time_index)) {
    # Kein Defoliation
    if (length(anomaly_indices) == 0) {
      ts$pd_score_fx[1] <- 0.5  # True Negative
    } else {
      ts$pd_score_fx[anomaly_indices] <- -1  # False Positive
    }
  } else {
    # Defoliation vorhanden
    if (length(anomaly_indices) == 0) {
      ts$pd_score_fx[1] <- -1  # False Negative
    } else {
      # True Positive oder Mischung
      for (i in anomaly_indices) {
        ts$pd_score_fx[i] <- pd_score_equation(i, n_time_index)
      }
    }
  }
  ts<-ts|>select(!all_of(c("defo_temp", "an_temp")))
  return(ts)
}



################################################################################
#                               AN Score Calculation 
################################################################################

an_score_column <- function(df, cols = c("nir", "red", "rededge1", "rededge2", "rededge3"), prefix= "rec_") {
  an_score <- rep(0, nrow(df))  # leeres Ergebnis-Vector mit Länge der Zeilen
  for (col in cols) {
    rec_col <- paste0(prefix, col)
    score <- abs(df[[col]] - df[[rec_col]])
    an_score <- an_score + score
  }
  an_score <- an_score / length(cols)
  return(an_score)
}



an_function<-function(ts, th = 0.2, rec_err = "rec_err", steps = 3) {
  if (!steps %in% c(2, 3)) {
    stop("steps must be 2 or 3")
  }
  else if(steps==3){
    ts <- ts |> mutate(
      an = ifelse(
        !!sym(rec_err) > th &
          lead(!!sym(rec_err), 1) > th &
          lead(!!sym(rec_err), 2) > th,1, 0 ))
  }else if(steps==2){
    ts <- ts |> mutate(
      an = ifelse(
        !!sym(rec_err) > th &
          lead(!!sym(rec_err), 1) > th,1, 0 ))
  }
  return(ts)
}



# THis function gropus consecutive aomaly time steps into one Anomaly. Furhtermore the first time step for each Anomaly Group is labeled as an_first
an_groups_function <- function(df, group_col = "coords", an_col = "an") {
  setDT(df)
  
  # Dynamischer Zugriff auf Spalten
  df[, an_lag := shift(get(an_col), fill = 0), by = group_col]
  df[, an_group := cumsum(get(an_col) == 1 & an_lag == 0), by = group_col]
  df[get(an_col) != 1, an_group := NA_integer_]
  df[, an_first := as.integer(.I == .I[1]), by = c(group_col, "an_group")]
  df[, an_lag := NULL]
  df[is.na(an_group), an_first := 0L]
  return(df)
}




