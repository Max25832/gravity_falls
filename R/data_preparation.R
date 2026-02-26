#data_path <- "data/traindata/raw_GEE_data/"
data_path <- "data/testdata/raw_GEE_data/"


files <- list.files(path=data_path, pattern="*.csv", all.files=T, full.names=T)
df_all <- files |>
  lapply(read_csv) |>
  bind_rows()

csv<-df_all[,2:ncol(df_all)]


# # extracting the coordinates based on the .geo collumn 

csv <- csv |> mutate(coords = geo2coords(.geo))


#scaling the data to the similar values as in the training 

csv2<- csv|>mutate(B2=B2/10000,
                   B3=B3/10000,
                   B4=B4/10000,
                   B5=B5/10000,
                   B6=B6/10000,
                   B7=B7/10000,
                   B8=B8/10000,
                   B8A=B8A/10000,
                   B11=B11/10000,
                   B12=B12/10000)


# changeing the column names 
csv2 <- csv2 |> 
  rename(
    swir22 = B12,
    swir16 = B11,
    nir = B8,
    B8A = B8A,
    rededge3 = B7,
    rededge2 = B6,
    rededge1 = B5,
    red = B4,
    green = B3,
    blue = B2
  )


csv2 <- csv2 |>
  mutate(QA60 = if ("QA60" %in% names(csv2)) QA60 else 0)

# filtering the data based on QA60 
csv2<-csv2|> filter(QA60 ==0)

# removing the .geo column
csv2<-csv2|> select(-.geo)


bands <- c("coords","time","blue", "green", "red", "rededge1", "rededge2", "rededge3",  "nir", "B8A", "swir16", "swir22")

# Cleaning the Test Data 

csv2<-csv2|> select(all_of(bands),"QA60", "SCL" )
csv3<-csv2|> multipoint_data_cleaner() #|> indices() we dont need to calculate the indices


# Saving the Data
#write_csv(csv3, file = "data/traindata/train_data.csv")
write_csv(csv3, file = "data/testdata/small_test_set2.csv")




# Plot to show whats cleaned 
# samplecord <- unique(csv2$coords)[100]
# 
# # plotting 
# ggplot()+
#   geom_line(data= (csv2|> filter(coords== samplecord)), aes(x=as_date(time), y = swir22), col= "darkgreen")+
#   geom_point(data= (csv2|> filter(coords== samplecord)), aes(x=as_date(time), y = swir22), col= "darkgreen")+
#   geom_line(data= (csv3|> filter(coords== samplecord)), aes(x=as_date(time), y = swir22), col= "green")+
#   geom_point(data= (csv3|> filter(coords== samplecord)), aes(x=as_date(time), y = swir22), col= "green")+
#   theme_bw()
# 
# 
# 
