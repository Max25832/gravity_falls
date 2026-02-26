################################################################################
#                                 All level Shifts
################################################################################

df <- result |>
  filter(coords %in% unique(result$coords)[1450:1500])



detect_levelshift_monthly <- function(df, var = "ndvi", threshold = 0) {
  
  var_sym <- rlang::sym(var)
  
  df <- df |>
    dplyr::select(coords, time, !!var_sym) |>
    na.omit()|>
    mutate(time= yearmon(time))
  
  split_df <- split(df, df$coords)
  
  res <- lapply(names(split_df), function(coord) {
    
    d <- split_df[[coord]] |>
      group_by(time)|>
      summarise(
        !!var_sym := mean(!!var_sym, na.rm = TRUE)
      )|>
      ungroup()|>
      #    complete(time = seq(min(time), max(time), by = 1/12))|>
      dplyr::arrange(time)
    
    
    
    if (nrow(d) < 10) {
      return(tibble(
        coords = coord,
        break_date = as.Date(NA),
        mean_before = NA_real_,
        mean_after = NA_real_,
        level_shift = NA_real_
      ))
    }
    
    start_year  <- floor(as.numeric(min(d$time)))
    start_month <- round((as.numeric(min(d$time)) %% 1) * 12) + 1
    ts_data <- ts(
      d[[var]],
      frequency = 12,
      start = c(start_year,start_month)
    )
    
    bp <- tryCatch(
      strucchange::breakpoints(ts_data ~ 1),
      error = function(e) NULL
    )
    
    if (is.null(bp) || all(is.na(bp$breakpoints))) {
      return(tibble(
        coords = coord,
        break_date = as.Date(NA),
        mean_before = NA_real_,
        mean_after = NA_real_,
        level_shift = NA_real_
      ))
    }
    
    # Mittelwerte effizient berechnen
    bps <- bp$breakpoints
    values <- d[[var]]
    
    shifts <- vapply(
      bps,
      function(i) mean(values[(i + 1):length(values)]) -
        mean(values[1:i]),
      numeric(1)
    )
    
    # alle Breaks + zugehörige Level-Shifts
    out <- tibble(
      coords = coord,
      break_index = bps,
      break_date  = as.Date(zoo::as.yearmon(d$time[bps])),
      mean_before = vapply(bps, function(i) mean(values[1:i]), numeric(1)),
      mean_after  = vapply(bps, function(i) mean(values[(i + 1):length(values)]), numeric(1)),
      level_shift = shifts
    )
    
    # Threshold anwenden
    out <- out |> dplyr::filter(abs(level_shift) >= threshold)
    
    # Falls kein Break übrig bleibt → NA-Zeile zurückgeben
    if (nrow(out) == 0) {
      return(tibble(
        coords = coord,
        break_index = NA_integer_,
        break_date = as.Date(NA),
        mean_before = NA_real_,
        mean_after = NA_real_,
        level_shift = NA_real_
      ))
    }
    
    
    return(out)
  })
  dplyr::bind_rows(res)
}

detect_levelshift_quarter <- function(df, var = "ndvi", threshold = 0) {
  
  var_sym <- rlang::sym(var)
  
  df <- df |>
    dplyr::select(coords, time, !!var_sym) |>
    na.omit()|>
    mutate(time= yearquarter(time))
  
  split_df <- split(df, df$coords)
  
  res <- lapply(names(split_df), function(coord) {
    
    d <- split_df[[coord]] |>
      group_by(time)|>
      summarise(
        !!var_sym := mean(!!var_sym, na.rm = TRUE)
      )|>
      ungroup()|>
      #    complete(time = seq(min(time), max(time), by = 1/12))|>
      dplyr::arrange(time)
    
    
    
    if (nrow(d) < 10) {
      return(tibble(
        coords = coord,
        break_date = as.Date(NA),
        mean_before = NA_real_,
        mean_after = NA_real_,
        level_shift = NA_real_
      ))
    }
    
    start_year  <- floor(as.numeric(year(min(d$time))))
    start_q <-  as.numeric(substr(min(d$time), 7,7))
    
    ts_data <- ts(
      d[[var]],
      frequency = 4,
      start = c(start_year,start_q)
    )
    
    bp <- tryCatch(
      strucchange::breakpoints(ts_data ~ 1),
      error = function(e) NULL
    )
    
    if (is.null(bp) || all(is.na(bp$breakpoints))) {
      return(tibble(
        coords = coord,
        break_date = as.Date(NA),
        mean_before = NA_real_,
        mean_after = NA_real_,
        level_shift = NA_real_
      ))
    }
    
    # Mittelwerte effizient berechnen
    bps <- bp$breakpoints
    values <- d[[var]]
    
    shifts <- vapply(
      bps,
      function(i) mean(values[(i + 1):length(values)]) -
        mean(values[1:i]),
      numeric(1)
    )
    
    # alle Breaks + zugehörige Level-Shifts
    out <- tibble(
      coords = coord,
      break_index = bps,
      break_date  = as.Date(d$time[bps]),
      mean_before = vapply(bps, function(i) mean(values[1:i]), numeric(1)),
      mean_after  = vapply(bps, function(i) mean(values[(i + 1):length(values)]), numeric(1)),
      level_shift = shifts
    )
    
    # Threshold anwenden
    out <- out |> dplyr::filter(abs(level_shift) >= threshold)
    
    # Falls kein Break übrig bleibt → NA-Zeile zurückgeben
    if (nrow(out) == 0) {
      return(tibble(
        coords = coord,
        break_index = NA_integer_,
        break_date = as.Date(NA),
        mean_before = NA_real_,
        mean_after = NA_real_,
        level_shift = NA_real_
      ))
    }
    
    return(out)
  })
  dplyr::bind_rows(res)
}

detect_levelshift_weekly <- function(df, var = "ndvi", threshold = 0) {
  
  var_sym <- rlang::sym(var)
  
  df <- df |>
    dplyr::select(coords, time, !!var_sym) |>
    na.omit()
  
  split_df <- split(df, df$coords)
  
  res <- lapply(names(split_df), function(coord) {
    
    d <- split_df[[coord]] |>
      dplyr::arrange(time)
    
    if (nrow(d) < 10) {
      return(tibble(
        coords = coord,
        break_date = as.Date(NA),
        mean_before = NA_real_,
        mean_after = NA_real_,
        level_shift = NA_real_
      ))
    }
    
    ts_data <- ts(
      d[[var]],
      frequency = 52,
      start = c(lubridate::year(min(d$time)),
                lubridate::isoweek(min(d$time)))
    )
    
    bp <- tryCatch(
      strucchange::breakpoints(ts_data ~ 1),
      error = function(e) NULL
    )
    
    if (is.null(bp) || all(is.na(bp$breakpoints))) {
      return(tibble(
        coords = coord,
        break_date = as.Date(NA),
        mean_before = NA_real_,
        mean_after = NA_real_,
        level_shift = NA_real_
      ))
    }
    
    # Mittelwerte effizient berechnen
    bps <- bp$breakpoints
    values <- d[[var]]
    
    shifts <- vapply(
      bps,
      function(i) mean(values[(i + 1):length(values)]) -
        mean(values[1:i]),
      numeric(1)
    )
    
    # alle Breaks + zugehörige Level-Shifts
    out <- tibble(
      coords = coord,
      break_index = bps,
      break_date  = d$time[bps],
      mean_before = vapply(bps, function(i) mean(values[1:i]), numeric(1)),
      mean_after  = vapply(bps, function(i) mean(values[(i + 1):length(values)]), numeric(1)),
      level_shift = shifts
    )
    
    # Threshold anwenden
    out <- out |> dplyr::filter(abs(level_shift) >= threshold)
    
    # Falls kein Break übrig bleibt → NA-Zeile zurückgeben
    if (nrow(out) == 0) {
      return(tibble(
        coords = coord,
        break_index = NA_integer_,
        break_date = as.Date(NA),
        mean_before = NA_real_,
        mean_after = NA_real_,
        level_shift = NA_real_
      ))
    }
    
    return(out)
  })
  dplyr::bind_rows(res)
}



