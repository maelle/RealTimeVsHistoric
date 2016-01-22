######################################################
# LOAD PACKAGES
######################################################
library("readr")
library("lubridate")
library("dplyr")
library("Ropenaq")
library("ggplot2")

######################################################
# CHECK LOCATIONS ROPENAQ DELHI PM2.5
######################################################
Ropenaq::locations(city="Delhi", parameter="pm25")
# we'll use only the 4 first ones since the first one
# is US embassy data
locationsDelhi <- Ropenaq::locations(city="Delhi", 
                                     parameter="pm25")[1:4,]

######################################################
# LOAD DATA FROM CPCB
######################################################
dataCPCB <- readr::read_csv("cpcb_ambient_panel.csv")
# change this name for compatibility with Open AQ name
dataCPCB$station[dataCPCB$station=="R K Puram"] <- "RK Puram"
# filter the locations we have with OpenAQ
dataCPCB <- dplyr::filter(dataCPCB,
                          station %in% locationsDelhi$location)
# now off to translating date
# I am too lazy for finding something more elegant
dataCPCB$dt_clean <- gsub("apr", "-04-", dataCPCB$dt_clean)
dataCPCB$dt_clean <- gsub("may", "-05-", dataCPCB$dt_clean)
dataCPCB$dt_clean <- gsub("jun", "-06-", dataCPCB$dt_clean)
dataCPCB$dt_clean <- gsub("jul", "-07-", dataCPCB$dt_clean)
dataCPCB$dt_clean <- gsub("aug", "-08-", dataCPCB$dt_clean)
dataCPCB$dt_clean <- gsub("sep", "-09-", dataCPCB$dt_clean)
dataCPCB$dt_clean <- gsub("oct", "-10-", dataCPCB$dt_clean)
dataCPCB$dt_clean <- gsub("nov", "-11-", dataCPCB$dt_clean)
dataCPCB$dt_clean <- gsub("dec", "-12-", dataCPCB$dt_clean)
dataCPCB <- dplyr::mutate(dataCPCB,
                          dateLocal=lubridate::dmy_hms(dt_clean))
# name the column differently
dataCPCB <- dplyr::mutate(dataCPCB,
                          historicValue=reading_value)
# drop useless columns
dataCPCB <- dplyr::select(dataCPCB,
                          - dt_clean,
                          - date_r,
                          - monitor_read,
                          - reading_value)

######################################################
# NOW GET OPENAQ DATA
######################################################
# dataOpenAQ <- NULL
# for (i in 1:length(locationsDelhi)){
#   firstUpdated <- locationsDelhi[i,]$firstUpdated
#   locationURL <- locationsDelhi[i,]$locationURL
#   
#   seqDays <- seq(from=lubridate::ymd(format(firstUpdated, "%Y-%m-%d")),
#                  to=lubridate::ymd("2015-12-31"),
#                  by="1 day")
#   seqDays <- format(seqDays, "%Y-%m-%d")
#   for(i in 1:(length(seqDays)-1)){
#     dataOpenAQTemp <- try(Ropenaq::measurements(location=locationURL,
#                                                      parameter="pm25",
#                                                      limit=1000,
#                                                      date_from=seqDays[i],
#                                                      date_to=seqDays[i+1]), silent=TRUE)
#     print(seqDays[i])
#     
#     if(class(dataOpenAQTemp)[1]!="try-error"){
#       dataOpenAQ <- rbind(dataOpenAQ,
#                                dataOpenAQTemp)
#     }
#     
#   }
#   
# 
# }
# # might be useful later
# dataOpenAQ <- unique(dataOpenAQ)
# save(dataOpenAQ, file="dataOpenAQ.RData")
load("dataOpenAQ.RData")

######################################################
# NOW PUT IT IN SHAPE
######################################################
dataOpenAQ <- dplyr::mutate(dataOpenAQ,
                            valueOpenAQ=value,
                            station=location)
dataOpenAQ <- dplyr::select(dataOpenAQ,
                            dateLocal,
                            station,
                            valueOpenAQ)

######################################################
# EXPLORATION
######################################################
for (station in levels(as.factor(dataOpenAQ$station))){
  print(station)
  minDate <- min(dataOpenAQ$dateLocal[dataOpenAQ$station==station])
  dataTempCPCB <- dataCPCB[dataCPCB$station==station&
                         dataCPCB$dateLocal>=minDate,]
  maxDate <- max(dataTempCPCB$dateLocal)
  dataTempOpenAQ <- dataOpenAQ[dataOpenAQ$station==station&
                                 dataOpenAQ$dateLocal<=maxDate]
  p <- ggplot() + 
    geom_point(data=dataOpenAQ,
              aes(x=dateLocal, y=valueOpenAQ),
              col="purple")+ 
    geom_point(data=dataTempCPCB,
              aes(x=dateLocal, y=historicValue),
              col="blue")+
    ggtitle(station)
  ggsave(p, file=paste0(station,".png"))
}
