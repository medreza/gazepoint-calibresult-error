library(NISTunits)
library(plyr)
library(stringr)


file_data_asli <- "DataAsli_8ms_uncalibrated.csv"
file_data_terfilter <- "Filter_Result_8ms_uncalibrated.csv"
file_hasil_nslr <- "HasilSampleClass_8ms_uncalibrated.csv"

h <- 25 # monitor height in centimeter
d <- 60 # distance between monitor and person eye in centimeter
r <- 768 # monitor vertical resolution

deg_per_px <-  NISTradianTOdeg(atan2(.5*h, d)) / (.5*r)

pixel2degree <- function(px) {
  deg <-  px * deg_per_px
  return(deg)
}

degree2pixel <- function(deg) {
  px <-  deg / deg_per_px
  return(px)
}

euclidean_dist <- function(x1,y1,x2,y2) {
  return(sqrt((x1-x2)^2 + (y1-y2)^2))
}

object_delay <- str_extract(file_data_asli, "\\dms")
object_calib <- str_extract(file_data_asli, "(?<=ms_).+(?=.csv)")

# load raw gaze data
data_asli <-  read.csv(file_data_asli, header = FALSE)
names(data_asli) <- c('Time','X_Gaze','Y_Gaze','X_Obj','Y_Obj')

# load filtered gaze data
data_terfilter <-  read.csv(file_data_terfilter, header = FALSE)
names(data_terfilter) <- c('Time','X_Gaze_F','Y_Gaze_F','X_Obj','Y_Obj')

data_terfilter['X_Gaze_F'] <-  degree2pixel(data_terfilter['X_Gaze_F'] )
data_terfilter['Y_Gaze_F'] <-  degree2pixel(data_terfilter['Y_Gaze_F'] )
data_cols <- c(data_asli['X_Gaze'], data_asli['Y_Gaze'], 
              data_terfilter['X_Gaze_F'], data_terfilter['Y_Gaze_F'],
              data_terfilter['X_Obj'], data_terfilter['Y_Obj'])
data_all <- data.frame(data_cols)

data_all['ED_Gaze_to_Obj'] <-  
  euclidean_dist(data_all['X_Gaze'], 
                 data_all['Y_Gaze'], 
                 data_all['X_Obj'], 
                 data_all['Y_Obj'])


data_all['ED_GazeFiltered_to_Obj'] <-  
  euclidean_dist(data_all['X_Gaze_F'], 
                 data_all['Y_Gaze_F'], 
                 data_all['X_Obj'], 
                 data_all['Y_Obj'])

data_all['X_Gaze_Error'] <- abs(data_all['X_Gaze'] - data_all['X_Obj'])
data_all['Y_Gaze_Error'] <- abs(data_all['Y_Gaze'] - data_all['Y_Obj'])
data_all['X_Gaze_F_Error'] <- abs(data_all['X_Gaze_F'] - data_all['X_Obj'])
data_all['Y_Gaze_F_Error'] <- abs(data_all['Y_Gaze_F'] - data_all['Y_Obj'])

data_all <-  pixel2degree(data_all) # change pixels to visual degree

rerata_error <- matrix(c(mean(data_all$ED_Gaze_to_Obj),
                mean(data_all$X_Gaze_Error),
                mean(data_all$Y_Gaze_Error),
                mean(data_all$ED_GazeFiltered_to_Obj),
                mean(data_all$X_Gaze_F_Error),
                mean(data_all$Y_Gaze_F_Error)), nrow = 2, ncol = 3)

dimnames(rerata_error) <- list(c('Gaze','Gaze_Filtered'), c('ED_Error','X_Error','Y_Error'))

#png('hasil_rerata_error.png')
barplot(rerata_error, beside=T, 
       col=c("aquamarine3","coral"), 
       main= paste("Rerata Error Terhadap Objek\n (",object_delay," ",object_calib,")",sep = ""),
       ylim=c(0,4),
       ylab='degree')
legend("topleft", c("Raw Gaze","Filtered Gaze (EMA20)"), pch=15, 
       col=c("aquamarine3","coral"), 
       bty="n", inset=c(0,-0.05), xpd=TRUE)
#dev.off()

# load gaze classification result
hasil_nslrhmm <-  read.csv(file_hasil_nslr, header = FALSE)
hasil_nslrhmm_count <- count(hasil_nslrhmm[4])['freq']
hasil_nslrhmm_freq <- hasil_nslrhmm_count[1:nrow(hasil_nslrhmm_count),]
hasil_nslrhmm_percentage <- round((hasil_nslrhmm_freq/sum(hasil_nslrhmm_freq))*100, 1)

#png('hasil_klasifikasi.png')
pie(hasil_nslrhmm_freq, 
    labels = hasil_nslrhmm_percentage,  
    main = paste("Hasil Klasifikasi dengan EMA20 + NSLR-HMM \n(",object_delay," ",object_calib,")",sep = ""),
    col = rainbow(length(hasil_nslrhmm_percentage)))
legend("bottom", c(paste("Fixation", hasil_nslrhmm_percentage[1], "%"), paste("Smooth Pursuit", hasil_nslrhmm_percentage[2], "%")), 
       cex = 0.8, inset=c(0,-0.2),
       fill = rainbow(length(hasil_nslrhmm_percentage)), xpd=TRUE)
#dev.off()
