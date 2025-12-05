#TODO val data auf raw data erstellen. 
#TODO Join!-  Lefjoin? 
#TODO Score

#TODO val data auf raw data erstellen. 

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


################################################################################
#                   Calculate Detected Anomalies from Model
################################################################################

reconstructed_data<-read_csv("data/reconstructed_data/reconstructed_data_test_model.csv")

# if the time format of data is saved as charracter (yyyy-Www) e.g. "2020 W01" this function should be applied otherwize applying furhter functions can take very long   
reconstructed_data$time<- convert_week_to_date(reconstructed_data$time)


reconstructed_data_scored<-reconstructed_data|>
  group_by(coords)|>
  group_modify(~ an_function(.x, th= 0.25, rec_err = "an_score3")) |> 
  ungroup()|>
  an_groups_function(an_col = "an")|>
  as.data.frame()



#----  Join   ----
validation_df<-validation_df|> mutate(time= yearweek(time))
#reconstructed_data_scored_selected<-reconstructed_data_scored_selected|> mutate(time= yearweek(time))
reconstructed_data_scored_selected<-reconstructed_data_scored|> mutate(time= yearweek(time))

pd_df<-left_join(reconstructed_data_scored_selected, validation_df, by = c("time", "coords"))

pd_df<-pd_df|> mutate(defoliation = ifelse(is.na(defoliation), 0, defoliation))


#----  Score  ----

pd_df<-pd_df|>group_by(coords)|>
  group_modify(~ pd_score_add_score_column(.x, defoliation_column = "defoliation",an_column = "an_first")) |> 
  ungroup()
  


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
AN_prediction = "an_first"
)


ts|>an_plot()


  

pd_df|>
  filter(pd_score_fx!=0)|>
  ggplot()+
  #  geom_density(aes(x = pd_score_fx, col="pd_score_fx", fill= "pd_score_fx"),alpha=0.5)+
  geom_bar(aes(x = pd_score_fx,col="pd_score_fx", fill= "pd_score_fx"),linetype = "dotted")+
  #  geom_hline(aes(yintercept = mean(pd_score_fx)),size=1.5, col="red", show.legend = F)
  theme_bw()


pd_df|>filter(coords== pd_df$coords[1500])|>
  ggplot(aes(x=as_date(time)))+
  geom_line(aes(y= an_score3, colour = "anscore"))+
  geom_line(aes(y= pd_score_fx, colour = "score"))+
  geom_point(data= (pd_df|>filter(coords== pd_df$coords[1])|>filter(an_first==1)), aes(y= an_score3, colour = "first detected AN"))




