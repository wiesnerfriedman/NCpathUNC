# read the WWTP data 
library(readr) 
WWTPsamplingsites_Sampling_Sites <- read_csv("WWTPsamplingsites - Sampling Sites.csv")

# copy the data
wwtpInfo<-WWTPsamplingsites_Sampling_Sites

# clean up
wwtpInfo<-wwtpInfo[-1,];
wwtpInfo$Plant<-toupper(wwtpInfo$Plant) #capitalize letters
wwtpInfo$Plant<-gsub(" ", "", wwtpInfo$Plant, fixed = TRUE) # remove spaces
wwtpInfo$Plant<-gsub("-", "", wwtpInfo$Plant, fixed = TRUE) # remove dashes
wwtpInfo$`Plant  ID`<-gsub("-", "", wwtpInfo$`Plant  ID`, fixed = TRUE)
wwtpInfo$`Plant  ID`<-gsub(" ", "", wwtpInfo$`Plant  ID`, fixed = TRUE)
wwtpdf<-wwtpInfo[,c(3,5,6)] #make new dataframe that can be used as input to meteo_nearby_stations
names(wwtpdf)[names(wwtpdf) == "Plant  ID"] <- "id"
names(wwtpdf)[names(wwtpdf) == "Plant Lat"] <- "latitude"
names(wwtpdf)[names(wwtpdf) == "Plant_long"] <- "longitude"
# open rnoaa
library(rnoaa)
station_data <- ghcnd_stations() #load station data for use
nearby_stations <- meteo_nearby_stations(lat_lon_df = wwtpdf,station_data=station_data,radius=20) # find stations within a radius of 20 km from the wwtp (large enough to reduce NAs, small enough to capture local trends)


#get the precipitation data from december 2018 to december 2020
idnms<-names(nearby_stations) # get the station names
stations<-c() # create empty vector
origIds<-c() # create empty vector
prcpval<-matrix(data=NA,nrow=18,ncol=732) # create empty matrix with 18 rows (no. of stations) and 732 columns (no. of days)
for (i in 1:length(idnms)) {
  stationIDs<-nearby_stations[[idnms[i]]]$id # get station IDs for those within distance of wwtp id name
  placeid<-idnms[i] #store wwtp id
  for (j in stationIDs) {
    newstation<-j # store the station id 
    stations<-c(stations,newstation) # append station ids in list (before grouped by wwtp id)
    origIds<-c(origIds,placeid) # relate the wwtp id with station ids they are related to
  }
  alldata<-meteo_pull_monitors(stationIDs,var="PRCP",date_min = "2018-12-01", date_max = "2020-12-01") # get the data for the 732 days for each station 
  udates<-unique(alldata$date) # find all the unique days 
  for (k in 1:length(udates)) {
    idx<-alldata$date==udates[k] # for each unique day index those days
    temp<-alldata$prcp[idx] # get the precipitation data from all stations for those days for that wwtp ID
    idxna<-is.na(temp) # find NA values from the precipitation data from all stations for those days for that wwtp ID
    prcpval[i,k]<-mean(temp[!idxna]) # find the mean precipitation for that day from all stations for that wwtp ID
  }
}

tmp<-as.character(udates) #store the unique dates as character class for assigning df column names
colnames(prcpval)<-tmp # assign column names
rownames(prcpval)<-idnms # assign wwtp ID as rownames
prcpdf<-data.frame(prcpval) # make dataframe
write.csv(prcpdf,'NCwwPathPrecip.csv') #save .csv

# temperature doesn't seem to work...yet... 
# idnms<-names(nearby_stations)
# stations<-c()
# origIds<-c()
# tval<-matrix(data=NA,nrow=18,ncol=732)
# for (i in 1:length(idnms)) {
#   stationIDs<-nearby_stations[[idnms[i]]]$id
#   placeid<-idnms[i]
#   for (j in stationIDs) {
#     newstation<-j
#     stations<-c(stations,newstation)
#     origIds<-c(origIds,placeid)
#   }
#   alldata<-meteo_pull_monitors(stationIDs,var="TAVG",date_min = "2018-12-01", date_max = "2020-12-01")
#   udates<-unique(alldata$date)
#   for (k in 1:length(udates)) {
#     idx<-alldata$date==udates[k]
#     temp<-alldata$prcp[idx]
#     idxna<-is.na(temp)
#     tval[i,k]<-mean(temp[!idxha])
#   }
# }
# 
# tmp<-as.character(tval)
# colnames(tval)<-tmp
# rownames(tval)<-idnms