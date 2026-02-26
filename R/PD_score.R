
reconstructed_data<-read_csv("data/reconstructed_data/reconstructed_data_test_model.csv")
reconstructed_data2<-read_csv("data/reconstructed_data/reconstructed_data_paper_model.csv")
# if the time format of data is saved as charracter (yyyy-Www) e.g. "2020 W01" this function should be applied otherwize applying furhter functions can take very long   
#reconstructed_data2$time<- convert_week_to_date(reconstructed_data2$time)

reconstructed_data3<-read_csv("data/reconstructed_data/reconstructed_data_test_att_wenc30.csv")


# if the time format of data is saved as charracter (yyyy-Www) e.g. "2020 W01" this function should be applied otherwize applying furhter functions can take very long   
# reconstructed_data$time<- convert_week_to_date(reconstructed_data$time)


################################################################################
#                     Validation Dataframe 
################################################################################

raw_test_data<-read_csv("data/testdata/small_test_set.csv")

raw_test_data2<-raw_test_data|>
  group_by(coords)|>
  group_modify(~ defoliation_indices(.x, weekly =T)) |> # calculat the defoliation dates for each coordinate time series 
  ungroup() 

validation_df<-raw_test_data2|> 
  #filter(defoliation==1)|>
  select(coords, time, defoliation, tcw, ndvi)


# --- Cclp ---

val_cclp<-read_csv("data/testdata/smallSampleAreas_validation.csv")|>
  select(coords, date_vals)|>
  rename(time=date_vals)|>
  na.omit()|>
  mutate(  defoliation=1)





################################################################################
#                 Validation With 2 different validation DFs
################################################################################


# ----- with Canopy Cover Loss Product (CCLP) -----

scored_df_cclp<-gf_score_function(validation_df = val_cclp,
                                  reconstructed_data = reconstructed_data,
                                  threshold = 0.25,
                                  rec_err_col = "an_score3",
                                  an_col = "an",
                                  defoliation_column = "defoliation",
                                  an_column = "an_first")


scored_df_att_cclp<-gf_score_function(validation_df = val_cclp,
                                      reconstructed_data = reconstructed_data3,
                                      threshold = 0.25,
                                      rec_err_col = "an_score3",
                                      an_col = "an",
                                      defoliation_column = "defoliation",
                                      an_column = "an_first")




scored_df<-gf_score_function(validation_df = validation_df,
                             reconstructed_data = reconstructed_data,
                             threshold = 0.25,
                             rec_err_col = "an_score3",
                             an_col = "an",
                             defoliation_column = "defoliation",
                             an_column = "an_first")


scored_df_att<-gf_score_function(validation_df = validation_df,
                                 reconstructed_data = reconstructed_data3,
                                 threshold = 0.25,
                                 rec_err_col = "an_score3",
                                 an_col = "an",
                                 defoliation_column = "defoliation",
                                 an_column = "an_first")




scored_df_paper<-gf_score_function(validation_df = validation_df,
                             reconstructed_data = reconstructed_data2,
                             threshold = 0.25,
                             rec_err_col = "an_score3",
                             an_col = "an",
                             defoliation_column = "defoliation",
                             an_column = "an_first")


scored_df_paper_cclp<-gf_score_function(validation_df = val_cclp,
                                 reconstructed_data = reconstructed_data2,
                                 threshold = 0.25,
                                 rec_err_col = "an_score3",
                                 an_col = "an",
                                 defoliation_column = "defoliation",
                                 an_column = "an_first")


m1<-scored_df|>
  na.omit()|>
  preparation_for_boxplot(model_name = "test")
m2<-scored_df_att|>
  na.omit()|>
  preparation_for_boxplot(model_name = "att_2y")

m3<-scored_df_cclp|>
  na.omit()|>
  preparation_for_boxplot(model_name = "test_cclp_scored")

m4<-scored_df_att_cclp|>
  na.omit()|>
  preparation_for_boxplot(model_name = "att_2y_cclp_scored")


m5<-scored_df_paper|>
  na.omit()|>
  preparation_for_boxplot(model_name = "paper")

m6<-scored_df_paper_cclp|>
  na.omit()|>
  preparation_for_boxplot(model_name = "paper_cclp")


m_all<-rbind(m1,m2, m3, m4,m5,m6)




m_all|>
  filter(.data[[score_column]]!=0.5)|>
  ggplot() +
  aes(x = model,
      y = .data[[score_column]],
      fill= model) +
  geom_flat_violin( position = position_nudge(x = .2), alpha = 0.5) +
  geom_point(aes(color = model), size= 0.3,
             position = position_jitter(w = .15)) +
  geom_boxplot(width = .25, alpha= 0.6) + # New code added here
  #  coord_flip() +
  # scale_fill_manual(values = c("LSTM AE" ="#568be8", "BFAST" =  "#ed88ab")) +
  # scale_color_manual(values = c("LSTM AE" ="#568be8", "BFAST" =  "#ed88ab")) +
  # scale_fill_manual(values = c("LSTM AE" ="#336a97", "BFAST" =  "#e43535")) +
  # scale_color_manual(values = c("LSTM AE" ="#336a97", "BFAST" =  "#e43535")) +
  theme_bw()+ 
  labs(x="Models", y = "Pre-Defoliation Score")+
  # facet_grid2(~method, scales = "free_x", space = "fixed" ) +
  facet_grid2(~model, scales = "free_x", space = "fixed" ) +
  theme(legend.position = "none")



################################################################################
# How to plot the PD_score line 
################################################################################
# 1. 
ts <- pd_df|>filter(coords== unique(pd_df$coords)[3000]) # healthy
ts <- pd_df|>filter(coords== unique(pd_df$coords)[1500]) # disturbed


ts|>plot_index_pdscore( 
  defoliation_column= "defoliation",
  an_column = "an_first",
  index="tcw",
  th = -0.03,
  AN_prediction = "an_first",
  legend = F)


ts|>an_plot()


bad_coords<- scored_df_paper|>group_by(coords)|> filter(any(pd_score_fx== -1))|>pull(coords)
bad_c<-unique(bad_coords)[150]


scored_df_paper|>filter(coords==bad_c )|>
  plot_index_pdscore( 
    defoliation_column= "defoliation",
    an_column = "an_first",
    index="ndvi",
    th = 0.5,
    AN_prediction = "an_first",
    legend = F)

  
  



scored_df_paper|>filter(coords==bad_c )|>an_plot(an_col = "an_score7")



scored_df_paper|>filter(coords%in%bad_c )|>get_extent()
