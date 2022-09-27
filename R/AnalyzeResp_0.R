
# install package pacman, load library. Use it to automatically install needed packages if not present, or load libraries if already present. 
# all libraries needed to run the MR functions 
##if (!require("pacman")) install.packages("pacman")
if (!require("rMR")) install.packages("rMR")
if (!require("plotrix")) install.packages("plotrix")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("gridExtra")) install.packages("gridExtra")
if (!require("chron")) install.packages("chron")
if (!require("lubridate")) install.packages("lubridate")
if (!require("dplyr")) install.packages("dplyr")
if (!require("pryr")) install.packages("pryr")
if (!require("ggpubr")) install.packages("ggpubr")
if (!require("magrittr")) install.packages("magrittr")
if (!require("tidyr")) install.packages("tidyr")
if (!require("broom")) install.packages("broom")
if (!require("purrr")) install.packages("purrr")
if (!require("mclust")) install.packages("mclust") 
if (!require("data.table")) install.packages("data.table") 
if (!require("stringr")) install.packages("stringr")
if (!require("DescTools")) install.packages("DescTools")
###pacman::p_load(ggplot2, gridExtra, chron, lubridate, dplyr, pryr, ggpubr, magrittr, tidyr, broom, purrr, mclust)


# can be done manually
# library(ggplot2)
# library(gridExtra)
# library(chron)
# library(lubridate)
# library(dplyr)
# library(pryr)
# library(ggpubr)
# library(magrittr)
# library(tidyr)
# library(broom)
# library(purrr)
# library(mclust)



###### Needed updates: #########
# expenential backterial growth 
# not size adjusted values for MR 
# get firesting split function here
# make sliding window an independent function? 
# make EPOC.spar an independent function?


###### Organizational functions ####################### 



convert.o2.Firesting <- function(csv.data, units_from = "pct", units_to = "mg/L", channels = c(1,2,3,4), salinity, atm_pressure = 1, n_ch = 4){
  # convert O2 units
  data.convert<-read.csv(csv.data)
  
  if(n_ch==4){
    temp_ch1 <- data.convert$Ch1_temp
    temp_ch2 <- data.convert$Ch1_temp
    temp_ch3 <- data.convert$Ch1_temp
    temp_ch4 <- data.convert$Ch1_temp
  }
  if(n_ch==2){
    temp_ch1 <- data.convert$Ch1_temp
    temp_ch2 <- data.convert$Ch1_temp
  }
  if(n_ch==8){
    temp_ch1 <- data.convert$Ch1_temp
    temp_ch2 <- data.convert$Ch2_temp
    temp_ch3 <- data.convert$Ch3_temp
    temp_ch4 <- data.convert$Ch4_temp
  }
  
  # data.convert<-read.csv("SDA/csv_files/Sep29_2020_box1_urchintest_SDA.csv")
  # NOTE: can make a loop for this to be effective
  # to get the same output (adjust the parameters), but add this to your data frame right away. Repeat for all channels
 
  if(any(channels == 1)){ 
    # data.convert$Ch1_O2 <- conv_o2 (o2 = data.convert$Ch1_O2, from = "percent_a.s.", to = "mg_per_l", temp = temp_ch1, sal = salinity, atm_pres = atm_pressure)
  
      data.convert$Ch1_O2 <- rMR::DO.unit.convert(data.convert$Ch1_O2, DO.units.in = units_from, DO.units.out = units_to, bar.units.in ="atm",
                    bar.press=1, temp.C = temp_ch1, bar.units.out = "atm",
                    salinity = salinity, salinity.units = "pp.thou")
  
  }
  
  if(any(channels == 2)){ 
    # data.convert$Ch2_O2 <- conv_o2 (o2 = data.convert$Ch2_O2, from = "percent_a.s.", to = "mg_per_l", temp = temp_ch2, sal = salinity, atm_pres = atm_pressure)
    if (n_ch!= 8){ 
        data.convert$Ch2_O2 <-  rMR::DO.unit.convert(data.convert$Ch2_O2, DO.units.in = units_from, DO.units.out = units_to, bar.units.in ="atm",
                    bar.press=1, temp.C = temp_ch1, bar.units.out = "atm",
                    salinity = salinity, salinity.units = "pp.thou")
    
    }else{
        data.convert$Ch2_O2 <- rMR::DO.unit.convert(data.convert$Ch2_O2, DO.units.in = units_from, DO.units.out = units_to, bar.units.in ="atm",
                    bar.press=1, temp.C = temp_ch2, bar.units.out = "atm",
                    salinity = salinity, salinity.units = "pp.thou")
    }
    
  }
  
  if(any(channels == 3)){ 
    # data.convert$Ch3_O2 <- conv_o2 (o2 = data.convert$Ch3_O2, from = "percent_a.s.", to = "mg_per_l", temp = temp_ch3, sal = salinity, atm_pres = atm_pressure)
    if (n_ch!= 8){ 
      data.convert$Ch3_O2 <- rMR::DO.unit.convert(data.convert$Ch3_O2, DO.units.in = units_from, DO.units.out = units_to, bar.units.in ="atm",
                    bar.press=1, temp.C = temp_ch1, bar.units.out = "atm",
                    salinity = salinity, salinity.units = "pp.thou")
    }else{
      data.convert$Ch3_O2 <- rMR::DO.unit.convert(data.convert$Ch3_O2, DO.units.in = units_from, DO.units.out = units_to, bar.units.in ="atm",
                                                  bar.press=1, temp.C = temp_ch3, bar.units.out = "atm",
                                                  salinity = salinity, salinity.units = "pp.thou")
    }
  }
  
  if(any(channels == 4)){ 
    # print(data.convert)
    # data.convert$Ch4_O2 <- conv_o2 (o2 = data.convert$Ch4_O2, from = "percent_a.s.", to = "mg_per_l", temp = temp_ch4, sal = salinity, atm_pres = atm_pressure)
    if (n_ch!= 8){ 
      data.convert$Ch4_O2 <- rMR::DO.unit.convert(data.convert$Ch4_O2, DO.units.in = units_from, DO.units.out = units_to, bar.units.in ="atm",
                    bar.press=1, temp.C = temp_ch1, bar.units.out = "atm",
                    salinity = salinity, salinity.units = "pp.thou")
    }else{
      data.convert$Ch4_O2 <- rMR::DO.unit.convert(data.convert$Ch4_O2, DO.units.in = units_from, DO.units.out = units_to, bar.units.in ="atm",
                                                  bar.press=1, temp.C = temp_ch4, bar.units.out = "atm",
                                                  salinity = salinity, salinity.units = "pp.thou")
    }
  }
  
  
  # save new file: 
  # for any other o2 value conversions please see link:
  # https://www.rdocumentation.org/packages/respirometry/versions/1.0.0/topics/conv_o2
  
  filename<-paste("./csv_files_unitConvert/", gsub('.{4}$', '', csv.data), "_mgO2L.csv", sep='')
  if (!dir.exists("./csv_files_unitConvert/")){
    message("Creating a new directory to store converted files: ./csv_files_unitConvert/" )
    dir.create(file.path("./csv_files_unitConvert/"), recursive = TRUE)
  }
  write.csv(file= filename, data.convert , row.names=FALSE)

}



# organize data / create directories as needed to stay organized 
# create directories


organize_MR_analysis <- function (create =  "Full") {
  
  if(create == "AUTO"){
    if (!dir.exists("./AUTO")){
      dir.create(file.path("AUTO"), recursive = TRUE)
      dir.create(file.path("AUTO","csv_analyzed"), recursive = TRUE)
      dir.create(file.path("AUTO","csv_files"), recursive = TRUE)
      dir.create(file.path("AUTO","plots_all_together"), recursive = TRUE)
      # dir.create(file.path("AUTO","plots_channel_temperature"), recursive = TRUE)
      dir.create(file.path("AUTO","plots_channel_cycle"), recursive = TRUE)
      dir.create(file.path("AUTO","plots_channel"), recursive = TRUE)
      dir.create(file.path("AUTO","plots_summary_respo"), recursive = TRUE)
    }
  }
  
  if(create == "MANUAL"){
    if (!dir.exists("./MANUAL")){
      dir.create(file.path("MANUAL"), recursive = TRUE)
      dir.create(file.path("MANUAL","csv_analyzed"), recursive = TRUE)
      dir.create(file.path("MANUAL","csv_files"), recursive = TRUE)
      dir.create(file.path("MANUAL","channel_plots"), recursive = TRUE)
      dir.create(file.path("MANUAL","channel_plots_MMRanalysis"), recursive = TRUE)
      dir.create(file.path("MANUAL","channel_sliding_sets"), recursive = TRUE)
    }
  }
  
  if(create == "BACTERIAL_RESP"){
     if (!dir.exists("./BACTERIAL_RESP")){
      dir.create(file.path("BACTERIAL_RESP"), recursive = TRUE)
      dir.create(file.path("BACTERIAL_RESP","csv_analyzed"), recursive = TRUE)
      dir.create(file.path("BACTERIAL_RESP","csv_files"), recursive = TRUE)
      dir.create(file.path("BACTERIAL_RESP","plots_all_together"), recursive = TRUE)
      # dir.create(file.path("BACTERIAL_RESP","plots_channel_temperature"), recursive = TRUE)
      dir.create(file.path("BACTERIAL_RESP","plots_channel_cycle"), recursive = TRUE)
      dir.create(file.path("BACTERIAL_RESP","plots_channel"), recursive = TRUE)
      dir.create(file.path("BACTERIAL_RESP","plots_summary_respo"), recursive = TRUE)
     }
  }
  
  if(create == "SDA"){
     if (!dir.exists("./SDA")){
      dir.create(file.path("SDA"), recursive = TRUE)
      dir.create(file.path("SDA", "csv_analyzed_SDA"), recursive = TRUE) # equivalent to EPOC for the MMR_SMR_AS_EPOC function
      dir.create(file.path("SDA", "csv_analyzed_SMR"), recursive = TRUE) # equivalent to EPOC for the MMR_SMR_AS_EPOC function
      dir.create(file.path("SDA", "csv_analyzed_SDA_hrly"), recursive = TRUE) 
      dir.create(file.path("SDA", "csv_analyzed_MR"), recursive = TRUE)
      dir.create(file.path("SDA", "csv_input_files"), recursive = TRUE)

    
      dir.create(file.path("SDA", "csv_analyzed"), recursive = TRUE)
      dir.create(file.path("SDA", "csv_files"), recursive = TRUE)
      dir.create(file.path("SDA", "plots_ch_SDA"), recursive = TRUE) # equivalent to EPOC for the MMR_SMR_AS_EPOC function 
      dir.create(file.path("SDA", "plots_methods_sum_SMR"), recursive = TRUE)
      dir.create(file.path("SDA", "plots_min_values_SMR"), recursive = TRUE)
      dir.create(file.path("SDA", "plots_mlnd_SMR"), recursive = TRUE)
      dir.create(file.path("SDA", "plots_SDA_hourly"), recursive = TRUE)
      dir.create(file.path("SDA","plots_summary_respo"), recursive = TRUE)
      dir.create(file.path("SDA","plots_channel"), recursive = TRUE)
      dir.create(file.path("SDA","plots_all_together"), recursive = TRUE)


     }
  }
  
  if(create == "Full"){
    
    if (!dir.exists("./AUTO")){
      dir.create(file.path("AUTO"), recursive = TRUE)
      dir.create(file.path("AUTO","csv_analyzed"), recursive = TRUE)
      dir.create(file.path("AUTO","csv_files"), recursive = TRUE)
      dir.create(file.path("AUTO","plots_all_together"), recursive = TRUE)
      # dir.create(file.path("AUTO","plots_channel_temperature"), recursive = TRUE)
      dir.create(file.path("AUTO","plots_channel_cycle"), recursive = TRUE)
      dir.create(file.path("AUTO","plots_channel"), recursive = TRUE)
      dir.create(file.path("AUTO","plots_summary_respo"), recursive = TRUE)
    }
    
     if (!dir.exists("./BACTERIAL_RESP")){
      dir.create(file.path("BACTERIAL_RESP"), recursive = TRUE)
      dir.create(file.path("BACTERIAL_RESP","csv_analyzed"), recursive = TRUE)
      dir.create(file.path("BACTERIAL_RESP","csv_files"), recursive = TRUE)
      dir.create(file.path("BACTERIAL_RESP","plots_all_together"), recursive = TRUE)
      # dir.create(file.path("BACTERIAL_RESP","plots_channel_temperature"), recursive = TRUE)
      dir.create(file.path("BACTERIAL_RESP","plots_channel_cycle"), recursive = TRUE)
      dir.create(file.path("BACTERIAL_RESP","plots_channel"), recursive = TRUE)
      dir.create(file.path("BACTERIAL_RESP","plots_summary_respo"), recursive = TRUE)
     }
    
    if (!dir.exists("./MANUAL")){
      dir.create(file.path("MANUAL"), recursive = TRUE)
      dir.create(file.path("MANUAL","csv_analyzed"), recursive = TRUE)
      dir.create(file.path("MANUAL","csv_files"), recursive = TRUE)
      dir.create(file.path("MANUAL","channel_plots"), recursive = TRUE)
      dir.create(file.path("MANUAL","channel_plots_MMRanalysis"), recursive = TRUE)
      dir.create(file.path("MANUAL","channel_sliding_sets"), recursive = TRUE)
    }
  
   if (!dir.exists("./MMR_SMR_AS_EPOC")){
      dir.create(file.path("MMR_SMR_AS_EPOC"), recursive = TRUE)
      dir.create(file.path("MMR_SMR_AS_EPOC", "csv_analyzed_EPOC"), recursive = TRUE)
      dir.create(file.path("MMR_SMR_AS_EPOC", "csv_analyzed_MMR"), recursive = TRUE)
      dir.create(file.path("MMR_SMR_AS_EPOC", "csv_analyzed_SMR"), recursive = TRUE)
      dir.create(file.path("MMR_SMR_AS_EPOC", "csv_analyzed_MR"), recursive = TRUE)
      dir.create(file.path("MMR_SMR_AS_EPOC", "csv_input_files"), recursive = TRUE)
      dir.create(file.path("MMR_SMR_AS_EPOC", "plots_ch_EPOC"), recursive = TRUE)
      dir.create(file.path("MMR_SMR_AS_EPOC", "plots_methods_sum_SMR"), recursive = TRUE)
      dir.create(file.path("MMR_SMR_AS_EPOC", "plots_min_values_SMR"), recursive = TRUE)
      dir.create(file.path("MMR_SMR_AS_EPOC", "plots_mlnd_SMR"), recursive = TRUE)
   }
  }
}


txt_csv_convert<-function(txt_file, N_Ch=4, path = ".", exclude_first_measurement_s = 0){
	
  if(N_Ch == 4 | N_Ch==2){
  	new_csv<-as.data.frame(matrix(nrow=0, ncol=8))
  	colnames(new_csv)<-c("date", "time", "time_sec", "Ch1_O2", "Ch1_temp", "Ch2_O2", "Ch3_O2", "Ch4_O2")
  	
  	d<-read.delim(txt_file, skip=19+exclude_first_measurement_s)
  	
  	# don't think need this anymore
  	# if (!colnames(d[1])=="Date"){
  	# 	d<-read.delim(txt_file, skip=26+exclude_first_measurement_s)
  	# 	#true
  	# }
  	
  	d<-d[,1:15]
  
  	nr<-nrow(d)
  	nc<-ncol(d)
  	
  	new_csv[nr,]<-NA
  	new_csv$date<-d[,1]
  	new_csv$time<-d[,2]
  	new_csv$time_sec<-d[,3]
  	# oxyegen below
  	new_csv$Ch1_O2<-d[,5]
  	
  	new_csv$Ch1_temp<-d[,15]# temp Ch1 - but same for all 
  	
  	new_csv$Ch2_O2<-d[,6]
  	new_csv$Ch3_O2<-d[,7]
  	new_csv$Ch4_O2<-d[,8]
  } # end od N_Ch == 2
  
  
  if(N_Ch==8){
    new_csv<-as.data.frame(matrix(nrow=0, ncol=11))
  	colnames(new_csv)<-c("date", "time", "time_sec", "Ch1_O2", "Ch1_temp", "Ch2_O2", "Ch3_O2", "Ch4_O2", "Ch2_temp", "Ch3_temp", "Ch4_temp")
  	
  	d<-read.delim(txt_file, skip=26+exclude_first_measurement_s)
  	# exclude rows with wrong data, no data, no time formats for the two columns
  	d<-d[c(which(grepl( "/", d[,1]) & grepl( ":", d[,2]))),]
  	d<-d[,1:12]
  	nr<-nrow(d)
  	nc<-ncol(d)
  	
  	new_csv[nr,]<-NA
  	new_csv$date<-d[,1]
  	new_csv$time<-d[,2]
  	new_csv$time_sec<-d[,3]
  	# oxyegen below
  	new_csv$Ch1_O2<-d[,5]
  	
  	new_csv$Ch1_temp<-d[,9]# unique ch temps
  	new_csv$Ch2_temp<-d[,10]# unique ch temps 
  	new_csv$Ch3_temp<-d[,11]# unique ch temps 
  	new_csv$Ch4_temp<-d[,12]# unique ch temps 

  	new_csv$Ch2_O2<-d[,6]
  	new_csv$Ch3_O2<-d[,7]
  	new_csv$Ch4_O2<-d[,8]
  }
  
  
  
	# save in the current directory (default)
	if(path == "."){
    write.csv(file=paste(gsub('.{4}$', '', txt_file), ".csv", sep=''), new_csv, row.names=FALSE)
	}
	
	#save in newly created folder - AUTO; use function: organize_MR_analysis() 
	if(path == "AUTO"){
    write.csv(file=paste("AUTO/csv_files/", gsub('.{4}$', '', txt_file), ".csv", sep=''), new_csv, row.names=FALSE)
	}
  
	#save in newly created folder - MANUAL; organize_MR_analysis() 
	if(path == "MANUAL"){
    write.csv(file=paste("MANUAL/csv_files/", gsub('.{4}$', '', txt_file), ".csv", sep=''), new_csv, row.names=FALSE)
	}
	
	# save in newly created folder - BACTERIAL_RESP; organize_MR_analysis() 
	if(path == "BACTERIAL_RESP"){
    write.csv(file=paste("BACTERIAL_RESP/csv_files/", gsub('.{4}$', '', txt_file), ".csv", sep=''), new_csv, row.names=FALSE)
	}
  
  if(path == "SDA"){
    write.csv(file=paste("SDA/csv_files/", gsub('.{4}$', '', txt_file), ".csv", sep=''), new_csv, row.names=FALSE)
  }

}



# the function to "glue" together two smr files 
# use analyzed csv files, with the slopes already extracted and cleaned (it is the easiest to glue files right before feeding the analyzed slopes etc. in the SMR_MMR_EPOC_AS function.

# be in the working directory where you have your smr analyzed csv files available
combine_smr <- function (smr_files, date_format = "m/d/y", path = "."){
  if(exists("data_glued")){
    rm("data_glued")
  }
  
  data_glued <- read.csv(smr_files[1])
    
  for(i in 2:length(smr_files)){
    data <- read.csv(smr_files[i])
    data_glued <- rbind(data_glued, data)
  }
  
  # first order all data by channels 
  data_glued <- data_glued[order(data_glued$Ch), ]

  data_glued$DateTime_start2<-as.character(data_glued$DateTime_start)
  data_glued$DateTime_start2<-as.character(substr(data_glued$DateTime_start2, start=2, stop=18)) 
  
  if(date_format == "m/d/y"){
    data_glued$DateTime_start2<-as.POSIXlt(data_glued$DateTime_start2, format="%m/%d/%y %H:%M:%S")
  }
  
  if(date_format == "d/m/y"){
    data_glued$DateTime_start2<-as.POSIXlt(data_glued$DateTime_start2, format="%d/%m/%y %H:%M:%S")
  }
  
  if(date_format == "y-m-d"){
    data_glued$DateTime_start2<-as.POSIXlt(data_glued$DateTime_start2, format="%Y-%m-%d %H:%M:%S")
  }
  #~ 	DateTime<- chron(dates=dataMMR$date,times=dataMMR$time,format=c('y-m-d','h:m:s')) # Healy firesting date format 

  data_glued$time_diff<-NA
  
  for(i in 2:nrow(data_glued)){
    data_glued$time_diff[i]<-data_glued$min_start[1]+(as.numeric(difftime(data_glued$DateTime_start2[i],data_glued$DateTime_start2[1], units="min")))
  }
  
  data_glued$time_diff[1]<-data_glued$min_start[1]
	data_glued$min_start<-data_glued$time_diff
  
	data_glued<-data_glued[,1:16]
	
	glued_name<-paste(gsub('.{4}$', '', smr_files[1]),"_GLUED_n",length(smr_files),".csv",sep="")
	glued_name2<-paste("../../MMR_SMR_AS_EPOC/csv_input_files/", gsub('.{4}$', '', smr_files[1]),"_GLUED_n",length(smr_files),".csv",sep="")

	write.csv (file = glued_name, data_glued)
	
	if (path != "."){
    write.csv (file = glued_name2, data_glued)
	  write.csv (file = glued_name, data_glued)
	}

}


# Function to split MR manual and auto respirometry files when using Firesting
# Prior function: Run the raw Firesting data through txt_csv_convert function in MR_firesting file. use csv file appropriately formatted. 
#
#
# Args:
#   data = a character sring of the data to be imported; csv files to be split in two (manual/MMR and auto/SMR files); no default  
#   split_data_name = the name that will be used to save all output files. Suggest using similar than the data name, also add destination to unique folders using this argument; no default 

#   cycle_full_min = cycle length (min) that the auto file was run on, this includes both flush + measure; no default 
#   timeSplit = the min when to make a split, this will be plotted too and confirmed for precision (use plots) 
#   split = FALSE; if ready to cut files and save two csv files, make this argument to TRUE; default is FALSE 

# Returns and saves: 
#   1. plot1 (.. "_split_respo_cycles.png") = plot with bound time 0 to t2;  thick blue line is at timeSplit, and dashed lines that represent the auto cycles; all 4 channels are plotted
#   2. plot2 (.. "_split_respo_confirm.png") = plot with bounds of t1 to t2; red line is at timeSplit, zoomed in version; plotted are all channels
#   -- below are only produced if split=TRUE
#   3. plot3 (.. "_split_respo.png") = for each cahnnel i) manual file, and ii) auto file (only rows 1:1200)
#   4. csv file (.. "_auto_split.csv") = the new split file that contains data for "SMR" automatic file
#   5. csv file (.. "_manual_split.csv") = the new split file that contains data for "MMR" manual file
# 
# note: For the split auto file, it only plots the first 1200 lines of data. 


csv_file_split <- function (data, split_data_name, cycle_full_min, timeSplit, date_format=NULL, split=FALSE, N_Ch=4 ) {
 
  if(is.null(date_format)) stop("Must provide date_format, can be either: \"m/d/y\", \"d/m/y\", \"y-m-d\" ")
  
  # need 8 ch code and flexible date format
  plotname1<-paste(split_data_name, "_split_respo_cycles.png", sep="")
  plotname2<-paste(split_data_name, "_split_respo_confirm.png", sep="")

  data_full<-read.csv(data)
  data_full$date<-as.character(data_full$date)
  data_full$time<-as.character(data_full$time)

  # DateTime<-chron(dates=data_full$date,times=data_full$time,format=c('m/d/y','h:m:s'))
  if (date_format== "m/d/y"){
  	DateTime<- chron(dates=data_full$date,times=data_full$time,format=c('m/d/y','h:m:s')) # Eliason Lab firesting date format
#~ 	DateTime<- chron(dates=data_full$date,times=data_full$time,format=c('y-m-d','h:m:s')) # Healy firesting date format 
	}
	if (date_format== "d/m/y"){
    # 8 channel firesting with date (day first) European style
    DateTime<- chron(dates=data_full$date,times=data_full$time,format=c('d/m/y','h:m:s')) # Eliason Lab firesting date format
	}
	if (date_format== "y-m-d"){
    # 8 channel firesting with date (day first) European style
    DateTime<- chron(dates=data_full$date,times=data_full$time,format=c('Y-m-d','h:m:s')) # Eliason Lab firesting date format
  }
  
  data_full$DateTime<-DateTime
  data_full$time_sec2[2:nrow(data_full)]<-diff(data_full$time_sec)
  data_full$hr<-round(data_full$time_sec/3600,2) 
  data_full$hr2<-round(data_full$time_sec/3600,0) 
  data_full$time_min<-round(data_full$time_sec/60,2) 
  
	if(as.character(data_full$Ch1_O2[1])=="--- " || as.character(data_full$Ch1_O2[1])=="---"){
		data_full$Ch1_O2<-0
	}
	if(as.character(data_full$Ch2_O2[1])=="--- " || as.character(data_full$Ch2_O2[1])=="---"){
		data_full$Ch2_O2<-0
	}
	if(as.character(data_full$Ch3_O2[1])=="--- " || as.character(data_full$Ch3_O2[1])=="---"){
		data_full$Ch3_O2<-0
	}
	if(as.character(data_full$Ch4_O2[1])=="--- " || as.character(data_full$Ch4_O2[1])=="---"){
		data_full$Ch4_O2<-0
	}
	

  t1<-timeSplit-1
  t2<-timeSplit+1

  t1_row<-which(data_full$time_min==Closest(data_full$time_min, t1))
  t2_row<-which(data_full$time_min==Closest(data_full$time_min, t2)[1])
  timeSplit_row<-which(data_full$time_min==Closest(data_full$time_min, timeSplit)[1])

  data1<-data_full[1:which(data_full$time_min==Closest(data_full$time_min, timeSplit+60)[1]),]
  # d1<-data_full[t1:nrow(data_full),]
  
  png(plotname1, width = 10, height = 10, units="in", res=300)
    par(mfrow=c(2,2))
    plot(data1$time_min, data1$Ch1_O2, main="Channel 1")
    # abline(v=data1$time_min[t1], col="red")
    abline(v=data1$time_min[timeSplit_row], col="blue", lwd=5)
    abline(v=seq(data1$time_min[timeSplit_row],nrow(data1),cycle_full_min), col="blue", lty="dashed")
    
    plot(data1$time_min, data1$Ch2_O2, main="Channel 2")
    # abline(v=data1$time_min[t1], col="red")
    abline(v=data1$time_min[timeSplit_row], col="blue", lwd=5)
    abline(v=seq(data1$time_min[timeSplit_row],nrow(data1),cycle_full_min), col="blue", lty="dashed")
    
    plot(data1$time_min, data1$Ch3_O2, main="Channel 3")
    # abline(v=data1$time_min[t1], col="red")
    abline(v=data1$time_min[timeSplit_row], col="blue", lwd=5)
    abline(v=seq(data1$time_min[timeSplit_row],nrow(data1),cycle_full_min), col="blue", lty="dashed")
    
    plot(data1$time_min, data1$Ch4_O2, main="Channel 4")
    # abline(v=data1$time_min[t1], col="red")
    abline(v=data1$time_min[timeSplit_row], col="blue", lwd=5)
    abline(v=seq(data1$time_min[timeSplit_row],nrow(data1),cycle_full_min), col="blue", lty="dashed")
  dev.off() 
  
  # print(d2)
  
  d2<-data_full[t1_row:t2_row,]
  row<-d2[d2$time_min==Closest(d2$time_min, timeSplit)[1],]

  png(plotname2,  width = 10, height = 10, units="in", res=300)
    par(mfrow=c(2,2))
    plot(d2$time_min, d2$Ch1_O2)
    abline(v=row$time_min, col="red")  
    rect(xleft=d2$time_min[1], xright=row$time_min, ytop=max(d2$Ch1_O2), ybottom=min(d2$Ch1_O2), col= '#FF003322', border=NA)
    title("shaded = manual/mmr, blank = auto/smr", cex.main=0.8)
    
    plot(d2$time_min, d2$Ch2_O2)
    abline(v=row$time_min, col="red")  
    rect(xleft=d2$time_min[1], xright=row$time_min, ytop=max(d2$Ch2_O2), ybottom=min(d2$Ch2_O2), col= '#FF003322', border=NA)
    plot(d2$time_min, d2$Ch3_O2)
    abline(v=row$time_min, col="red")
    rect(xleft=d2$time_min[1], xright=row$time_min, ytop=max(d2$Ch3_O2), ybottom=min(d2$Ch3_O2), col= '#FF003322', border=NA)
    plot(d2$time_min, d2$Ch4_O2)
    abline(v=row$time_min, col="red") 
    rect(xleft=d2$time_min[1], xright=row$time_min, ytop=max(d2$Ch2_O2), ybottom=min(d2$Ch4_O2), col= '#FF003322', border=NA)
  dev.off()  
 
  # full file splitting part
  if(split==TRUE){
    
    plotname3<-paste(split_data_name, "_split_respo.png", sep="")
    autoname<-paste(split_data_name, "_smr_split.csv", sep="")
  	manualname<-paste(split_data_name, "_mmr_split.csv", sep="")
    
  	data_manual<-data_full[1:timeSplit_row,]
  	data_auto<-data_full[timeSplit_row:nrow(data_full),]
  
  	data_auto$time_sec<-abs(data_auto$time_sec[1] - data_auto$time_sec)
  	# data_manual$time_sec<-abs(data_manual$time_sec[1] - data_manual$time_sec)
  		
    data_auto$time_min<-data_auto$time_sec/60
  	data_manual$time_min<-data_manual$time_sec/60
	 

	  png(plotname3,  width = 10, height = 20, units="in", res=300)
	  par(mfrow=c(4,2))
	  plot(data_auto$time_min[1:2000], data_auto$Ch1_O2[1:2000], main="AUTO-split", xlab="time (min)", ylab="[O2] Ch1")
	  plot(data_manual$time_min, data_manual$Ch1_O2, main="MANUAL-split", xlab="time (min)", ylab="[O2] Ch1")
	  plot(data_auto$time_min[1:2000], data_auto$Ch2_O2[1:2000], main="AUTO-split", xlab="time (min)", ylab="[O2] Ch2")
	  plot(data_manual$time_min, data_manual$Ch2_O2, main="MANUAL-split", xlab="time (min)", ylab="[O2] Ch2")
	  plot(data_auto$time_min[1:2000], data_auto$Ch3_O2[1:2000], main="AUTO-split", xlab="time (min)", ylab="[O2] Ch3")
	  plot(data_manual$time_min, data_manual$Ch3_O2, main="MANUAL-split", xlab="time (min)", ylab="[O2] Ch3")
	  plot(data_auto$time_min[1:2000], data_auto$Ch4_O2[1:2000], main="AUTO-split", xlab="time (min)", ylab="[O2] Ch4")
	  plot(data_manual$time_min, data_manual$Ch4_O2, main="MANUAL-split", xlab="time (min)", ylab="[O2] Ch4")
	  dev.off()
  
    
    # get rid of extra columns
    if(N_Ch != 4){
      data_manual<-data_manual[,1:11]
      data_auto<-data_auto[,1:11]
    }else{
      data_manual<-data_manual[,1:8]
      data_auto<-data_auto[,1:8]
    }
  	  write.csv(file=manualname, data_manual, row.names=FALSE)
  	  write.csv(file=autoname, data_auto, row.names=FALSE)
  }else{
    message("Data outputs not saved. To save add arg: split = TRUE")
  }	

}


sum_backgr<-function(data, Ch, resp.ID, resp.Vol){
  
    newdata<-matrix(ncol=11, nrow=0)
		colnames(newdata)<-c("Ch","temp", "r2_min", "r2_max", "r2_mean", "slope_min", "slope_max", "slope_mean", "n_measurements", "resp.ID", "resp.Vol")
		
		d<-read.csv(data)
		
		# analyze only specified levels
    n_Ch<-length(Ch)
	     for(i in 1:n_Ch){
  	      d_temp <- d[(substr(as.character(d$Ch), start=3, stop=3)==Ch[i]),]  %>% 
  	        group_by (Ch) %>% 
  	        summarize (  temp = mean(t_mean), r2_min = min(r2), r2_max = max(r2), r2_mean = mean(r2), slope_min = min (m), slope_max = max(m), slope_mean = mean(m), n=length(m))
  	     	 
  	      d_temp$resp.ID <- resp.ID[i]
  	      d_temp$rsep.Vol <- resp.Vol[i]
  	     
  
          newdata<-rbind(newdata, as.data.frame(d_temp))	
          
        png(paste(gsub('.{4}$', d_temp$Ch[1], data), "_backSlopes_PLOT.png", sep=''), width=5, height =5, units = "in", res=200)
  	      hist(d[(substr(as.character(d$Ch), start=3, stop=3)==Ch[i]),"m"], main="Slope Histogram", xlab="slopes")
  	    dev.off()
  	    
	     }
  
    newdata$datafile<-data
    write.csv(file=paste(gsub('.{4}$', '', data), "_BACK_SLOPES.csv", sep=''), newdata, row.names=FALSE)
}





 
###### SMR functions #################### 
# these three channel functions are called inside the SMR function as requested/appropriate to analyze data of each channel
# don't need to worry about the variables inside this function, the MMR_SMR provides all variables as it is right now. 

# SMR methods: 
# fish with activity:
# 1 - taking the avrg of the variable number of the lowest values of MO2 / from 5-10 values / 10% lowest values 
	# 1.4 - avrg continuous block means and taking the mean value of the given number of the lowest block means 
	# 1.5 - outliers statistically (> 2 SD ) removed from teh arg of the low values 
	#### MO2 values should be normally distributed and the lowest values may be far from the mean
	#### all about choosing the ring # and best representing MO2 values to be avrged  

# fish with no activity 
# 2 - taking a variable quantile from the values o MO2 
	# 2.1 splits data in p smallest and 1-p values largest values/ p chosen by experimenter e.g. 0.05 to 0.1 
	#### MO2 varies around SMR / assuming half above and half below 
	#### all about choosing right p

# fish with activity 
# 3 - assigning MO2 values to a mixture of normal distributions and using MLND (mean of the lowest norm distrib)
#  Frequency distribution bimodal or multinomal 
	# 3.1 one distribution associated with spantaneous activity and aclimation; another for low MO2 values / the MLND = robust estimate of SMR 
	#### drawback - large overlap between the distributions 


Channel_slope_analysis<-function(Ch, seq_st, seq_end, plotname, data1, chop_start, chop_end, inv.data, newdata){
	
	# if(!exists("newdata")){
	# 	newdata<-as.data.frame("new")
	# }
	# !(inv.data$type=="slope_analysis")
	inv.data.slope<-inv.data[inv.data$type=="slope_analysis",]

	for (i in 1:length(seq_end)){

		# if this is the first cycle for this fish - start a new data file
			if(as.character(newdata[1,1])=="new"){
				newdata<-matrix(ncol=15,nrow=0)
				colnames(newdata)<-c("time_frame","min_start", "r2" ,"b", "m" , "t_min", "t_max", "t_mean","O2_min", "O2_max", "O2_mean", "Ch", "DateTime_start", "type", "n_min")	
			}
				
			# if this is the first cycle for this fish - start a new plot
			if (i == 1){
				
				if(length(seq_st)<=100){
					png(plotname, width=40, height=40, units="in",  res=200)  
					par(mfrow=c(10,10)) # This will fit 100 plots. 
				}
				if(length(seq_st)>100 & length(seq_st)<=150){
					png(plotname, width=40, height=60, units="in",  res=200)  
					par(mfrow=c(15,10)) # This will fit 150 plots
				}
				if(length(seq_st)>150){
					png(plotname, width=50, height=50, units="in",  res=200)  
					par(mfrow=c(15,15)) # This will fit 150 plots
				}
					
			}
								
		start<-seq_st[i]+chop_start # chopping off first x min from the beginning of each slopes
		end<-seq_end[i]-chop_end # chopping off last x min from the end of each slope 
		
		# row 4 = start of the file 
		# row 5 = start of the "sectioned" slope
		# row 6 = end of the "sectioned" slope

		n<-which(inv.data.slope[,4]>=start-1 & inv.data.slope[,4]<end+1)
			
		if(length(n)==0 | length(n)>3){
			cycle_use<-"use full cycle"

			if(length(n)>30){
				message("more than 3 slopes: full slope plotted")
			}
			
		}else{
			cycle_use<-"slope analysis"
			# replace zeros in the inventory data to real values; in inv. data clean - this is where in invenotory file I added 0 when it just starts from the "start" of teh cycle and ends at the "end" of the cycle
##			print(length(n))
			
			if(length(n)==1){

				if(inv.data.slope[n,4]==0){
					inv.data.slope[n,4]<-start
				} 
				if(inv.data.slope[n,5]==0){
					inv.data.slope[n,5]<-start
				} 						
				if(inv.data.slope[n,6]==0){
					inv.data.slope[n,6]<-end
				}
				if(start==inv.data.slope[n,5] & end==inv.data.slope[n,6]){
					cycle_use<-"skip cycle"
				}
			}else{
				for(m in 1:length(n)){
				
					if(inv.data.slope[n[m],4]==0){
						inv.data.slope[n[m],4]<-start
					} 
					if(inv.data.slope[n[m],5]==0){
						inv.data.slope[n[m],5]<-start
					} 						
					if(inv.data.slope[n[m],6]==0){
						inv.data.slope[n[m],6]<-end
					}
					if(start==inv.data.slope[n[m],5] & end==inv.data.slope[n[m],6]){
						cycle_use<-"skip cycle"
					}
				
				}
			}
		}

		if(cycle_use=="use full cycle"){
##			print("use full cycle")
			d0<-data1[c(which(data1$time_min>start & data1$time_min<end)),] # from teh entire files, get only the data section we need / not cleaned no need to be cleaned

			# full data / not cleaned
			DateTime_start0<-as.character(d0$DateTime[1])
			lm_coef0<-coef(lm(d0[,Ch]~d0$time_min)) # get linear regression fit 
			r20<-round(summary(lm(d0[,Ch]~d0$time_min))$r.squared,3) # get r2
			m0<-round(lm_coef0[2],5) # get slope
			b0<-round(lm_coef0[1],2) # get intercept
			n_min0<-d0$time_min[nrow(d0)]-d0$time_min[1]	
			# temp recorded for each measure cycle		
			temp_mean0<-round(mean(d0$Ch1_temp),2) # paste this value on the plot
			temp_max0<-round(max(d0$Ch1_temp),2)	
			temp_min0<-round(min(d0$Ch1_temp),2)	
			
			# O2 recorded for each measure cycle		
			O2_mean0<-round(mean(d0[,Ch]),2) # paste this value on the plot
			O2_max0<-round(max(d0[,Ch]),2)	
			O2_min0<-round(min(d0[,Ch]),2)	
			
			time_frame<-paste("min", round(start,2), "_", round(end,2), sep="")
			type="SMR"

			values<-as.data.frame(t(c(time_frame, start, r20, b0, m0, temp_min0, temp_max0, temp_mean0,O2_min0, O2_max0, O2_mean0, substr(colnames(d0[Ch]), start=1, stop=3), DateTime_start0, type, n_min0)))
			colnames(values)<-c("time_frame","min_start", "r2" ,"b", "m" , "t_min", "t_max", "t_mean","O2_min", "O2_max", "O2_mean", "Ch", "DateTime_start", "type", "n_min")	
			
		
			# plotting each segment with slope, r2, and equations on it		
			plot(d0[,Ch]~time_min, d=d0, ylab=expression(paste(O2~(mg~L^{-1}))),xlab="Time (min)")
				if(cycle_use=="use cleaned cycle"){
					rect(xleft=inv.data.clean[n2,5], ybottom=min(d0[,Ch]),xright=inv.data.clean[n2,6],ytop=max(d0[,Ch]) ,col= alpha("grey", 0.3), border=NA)
					points(d_clean[,Ch]~time_min, d=d_clean, ylab=expression(paste(O2~(mg~L^{-1}))),xlab="Time (min)")
					abline(lm(d_clean[,Ch]~d_clean$time_min), col="red",lwd=2)
					mtext(bquote(y == .(lm_coef_clean[2])*x + .(lm_coef_clean[1])), adj=1, padj=0, cex=0.8, line=0) # display equation 
					mtext(bquote(italic(R)^2 == .(format(r2_clean, digits = 3))),adj=1, padj=0, cex=0.8, line=1)
					
					if(r2_clean<0.9){
						box(lty="dashed", col="orange", lwd=2)
					}
					if(r2_clean<0.85){
						box(lty="solid", col="orange", lwd=2)
					}
					if(r2_clean<0.80){
						box(lty="solid", col="red", lwd=2)
					}
					
				}else{
					abline(lm(d0[,Ch]~d0$time_min), col="red",lwd=2)
					mtext(bquote(y == .(lm_coef0[2])*x + .(lm_coef0[1])), adj=1, padj=0, cex=0.8, line=0) # display equation 
					mtext(bquote(italic(R)^2 == .(format(r20, digits = 3))),adj=1, padj=0, cex=0.8, line=1)
					
					if(r20<0.9){
						box(lty="dashed", col="orange", lwd=2)
					}
					if(r20<0.85){
						box(lty="solid", col="orange", lwd=2)
					}
					if(r20<0.80){
						box(lty="solid", col="red", lwd=2)
					}
				}
				
				# if this is the last cycle for this fish - save the plot
				if (i == length(seq_end)){
					dev.off()
		#~ 			print("close plot-CH") # sanity check that things runf properly 
				}
				
									
		}



			
			
		if (cycle_use=="slope analysis" && (length(n)==1 | length(n)==2 | length(n)==3) ){
				
##			print("slope analysis cycle")	
##			print(n)	
			d0<-data1[c(which(data1$time_min>start & data1$time_min<end)),] # from teh entire files, get only the data section we need / not cleaned no need to be cleaned
				
				if(length(n)==1){
	#~ 						print("one sectioned slope")
					
					cut_start<-inv.data.slope[n,5]
					cut_end<-inv.data.slope[n,6]
						
						# making this so that there is always pre and post
						if(round(as.numeric(start),1)==round(as.numeric(cut_start),1)){
							cut_start<-cut_start+2/60 # adding two seconds
						}
						if(round(as.numeric(end),1)==round(as.numeric(cut_end),1)){
							cut_end<-cut_end-2/60 # substracting two seconds
						}
												
					# section to be cut out 
					d_cut<-data1[c(which(data1$time_min>cut_start & data1$time_min<cut_end)),] # from teh entire files, get only the data section we need 

					# find the lenght min
					cut_length_min<-cut_start
					
					# find time in a day
					cut_time_min<-d_cut$DateTime[1]

					# shallow slope cut 
					DateTime_start_cut<-as.character(d_cut$DateTime[1])
					lm_coef_cut<-coef(lm(d_cut[,Ch]~d_cut$time_min)) # get linear regression fit 
					r2_cut<-round(summary(lm(d_cut[,Ch]~d_cut$time_min))$r.squared,3) # get r2
					m_cut<-round(lm_coef_cut[2],5) # get slope
					b_cut<-round(lm_coef_cut[1],2) # get intercept
					n_min_cut<-d_cut$time_min[nrow(d_cut)]-d_cut$time_min[1]
					o2_cut<-d_cut[1,Ch]
					temp_mean_cut<-round(mean(d_cut$Ch1_temp),2) 	# temp recorded for each measure section	
					temp_max_cut<-round(max(d_cut$Ch1_temp),2)	
					temp_min_cut<-round(min(d_cut$Ch1_temp),2)
					O2_mean_cut<-round(mean(d_cut[,Ch]),2) 	# O2 recorded for each measure section	
					O2_max_cut<-round(max(d_cut[,Ch]),2)	
					O2_min_cut<-round(min(d_cut[,Ch]),2)
				
					time_frame_cut<-paste("min", round(cut_start,2), "_", round(cut_end,2), sep="")
					type_cut="SMR-cut"
					

					values<-as.data.frame(t(c(time_frame_cut,round(cut_start,2), r2_cut, b_cut, m_cut, temp_min_cut, temp_max_cut, temp_mean_cut, O2_min_cut, O2_max_cut, O2_mean_cut, substr(colnames(d_cut[Ch]), start=1, stop=3), DateTime_start_cut, type_cut, n_min_cut)))
					
					colnames(values)<-c("time_frame","min_start", "r2" ,"b", "m" , "t_min", "t_max", "t_mean","O2_min", "O2_max", "O2_mean", "Ch", "DateTime_start", "type", "n_min")	
					newdata<-rbind(newdata, values) # add all values of interest to the individual fish specific data file (this is saved in MMR_SMR function) 
					colnames(newdata)<-c("time_frame","min_start", "r2" ,"b", "m" , "t_min", "t_max", "t_mean","O2_min", "O2_max", "O2_mean", "Ch", "DateTime_start", "type", "n_min")
					# adjust if there are more than 2 or three new starts / if yes define this
					
##print(c("pass 1"))	

					
					d_pre<-data1[c(which(data1$time_min>start & data1$time_min<cut_start)),] # section pre "cut"
					d_post<-data1[c(which(data1$time_min>cut_end & data1$time_min<end)),] # section pre "cut"
					
					# pre shallow slope cut 
					DateTime_start_pre<-as.character(d_pre$DateTime[1])
					lm_coef_pre<-coef(lm(d_pre[,Ch]~d_pre$time_min)) # get linear regression fit 
					r2_pre<-round(summary(lm(d_pre[,Ch]~d_pre$time_min))$r.squared,3) # get r2
					m_pre<-round(lm_coef_pre[2],5) # get slope
					b_pre<-round(lm_coef_pre[1],2) # get intercept
					n_min_pre<-d_pre$time_min[nrow(d_pre)]-d_pre$time_min[1]
					temp_mean_pre<-round(mean(d_pre$Ch1_temp),2) 	# temp recorded for each measure section	
					temp_max_pre<-round(max(d_pre$Ch1_temp),2)	
					temp_min_pre<-round(min(d_pre$Ch1_temp),2)	
					O2_mean_pre<-round(mean(d_pre[,Ch]),2) 	# O2 recorded for each measure section	
					O2_max_pre<-round(max(d_pre[,Ch]),2)	
					O2_min_pre<-round(min(d_pre[,Ch]),2)	
					time_frame_pre<-paste("min", round(start,2), "_", round(cut_start,2), sep="")
					type_pre="SMR-pre"
					
					values<-as.data.frame(t(c(time_frame_pre, round(start,2), r2_pre, b_pre, m_pre, temp_min_pre, temp_max_pre, temp_mean_pre, O2_min_pre, O2_max_pre, O2_mean_pre, substr(colnames(d_pre[Ch]), start=1, stop=3), DateTime_start_pre, type_pre, n_min_pre)))
					colnames(values)<-c("time_frame","min_start", "r2" ,"b", "m" , "t_min", "t_max", "t_mean","O2_min", "O2_max", "O2_mean", "Ch", "DateTime_start", "type", "n_min")	
					newdata<-rbind(newdata, values) # add all values of interest to the individual fish specific data file (this is saved in MMR_SMR function) 
					colnames(newdata)<-c("time_frame","min_start", "r2" ,"b", "m" , "t_min", "t_max", "t_mean","O2_min", "O2_max", "O2_mean", "Ch", "DateTime_start", "type", "n_min")


					# post shallow slope cut 
					DateTime_start_post<-as.character(d_post$DateTime[1])
					lm_coef_post<-coef(lm(d_post[,Ch]~d_post$time_min)) # get linear regression fit 
					r2_post<-round(summary(lm(d_post[,Ch]~d_post$time_min))$r.squared,3) # get r2
					m_post<-round(lm_coef_post[2],5) # get slope
					b_post<-round(lm_coef_post[1],2) # get intercept
					n_min_post<-d_post$time_min[nrow(d_post)]-d_post$time_min[1]
					temp_mean_post<-round(mean(d_post$Ch1_temp),2) 	# temp recorded for each measure section	
					temp_max_post<-round(max(d_post$Ch1_temp),2)	
					temp_min_post<-round(min(d_post$Ch1_temp),2)
					O2_mean_post<-round(mean(d_post[,Ch]),2) 	# O2 recorded for each measure section	
					O2_max_post<-round(max(d_post[,Ch]),2)	
					O2_min_post<-round(min(d_post[,Ch]),2)	
					time_frame_post<-paste("min", round(cut_end,2), "_", round(end,2), sep="")
					type_post="SMR-post"
	##				
					values<-as.data.frame(t(c(time_frame_post,round(cut_end,2), r2_post, b_post, m_post, temp_min_post, temp_max_post, temp_mean_post,O2_min_post, O2_max_post, O2_mean_post, substr(colnames(d_post[Ch]), start=1, stop=3), DateTime_start_post, type_post, n_min_post)))
					colnames(values)<-c("time_frame","min_start", "r2" ,"b", "m" , "t_min", "t_max", "t_mean","O2_min", "O2_max", "O2_mean", "Ch", "DateTime_start", "type", "n_min")	
					newdata<-rbind(newdata, values) # add all values of interest to the individual fish specific data file (this is saved in MMR_SMR function) 
					colnames(newdata)<-c("time_frame","min_start", "r2" ,"b", "m" , "t_min", "t_max", "t_mean","O2_min", "O2_max", "O2_mean", "Ch", "DateTime_start", "type", "n_min")
					

					plot(d0[,Ch]~time_min, data=d0, ylab=expression(paste(O2~(mg~L^{-1}))),xlab="Time (min)")
					points(d_cut[,Ch]~time_min,d=d_cut, col="grey75")
					points(d_pre[,Ch]~time_min,d=d_pre, col="green4")
					points(d_post[,Ch]~time_min,d=d_post, col="red3")
					abline(lm(d_cut[,Ch]~d_cut$time_min), col="grey75",lwd=2, lty=2)
					abline(lm(d_pre[,Ch]~d_pre$time_min), col="green4",lwd=2, lty=2)
					abline(lm(d_post[,Ch]~d_post$time_min), col="red3",lwd=2, lty=2)
				}
				
				if(length(n)==2){

					cut_start1<-inv.data.slope[n[1],5]
					cut_end1<-inv.data.slope[n[1],6]
					cut_start2<-inv.data.slope[n[2],5]
					cut_end2<-inv.data.slope[n[2],6]
					
						# making this so that there is always pre and post
						if(round(as.numeric(start),1)==round(as.numeric(cut_start1),1)){
							cut_start1<-cut_start1+2/60 # adding two seconds
						}
						if(round(as.numeric(end),1)==round(as.numeric(cut_end2),1)){
							cut_end2<-cut_end2-2/60 # substracting two seconds
						}
						
					# section to be cut out 
					d_cut1<-data1[c(which(data1$time_min>cut_start1 & data1$time_min<cut_end1)),] # from teh entire 
					# adjust if there are more than 2 or three new starts / if yes define this
					# DATA FRAME  / little chops 
					d_pre1<-data1[c(which(data1$time_min>start & data1$time_min<cut_start1)),] # section pre "cut"
					d_pre2<-data1[c(which(data1$time_min>cut_end1 & data1$time_min<cut_start2)),] # section pre "cut2"/ also section of the post cut1
					d_cut2<-data1[c(which(data1$time_min>cut_start2 & data1$time_min<cut_end2)),] # section pre "cut"
					d_post<-data1[c(which(data1$time_min>cut_end2 & data1$time_min<end)),] # section pre "cut"
					
					
					# pre1 shallow slope cut1 
					DateTime_start_pre1<-as.character(d_pre1$DateTime[1])
					lm_coef_pre1<-coef(lm(d_pre1[,Ch]~d_pre1$time_min)) # get linear regression fit 
					r2_pre1<-round(summary(lm(d_pre1[,Ch]~d_pre1$time_min))$r.squared,3) # get r2
					m_pre1<-round(lm_coef_pre1[2],5) # get slope
					b_pre1<-round(lm_coef_pre1[1],2) # get intercept
					n_min_pre1<-d_pre1$time_min[nrow(d_pre1)]-d_pre1$time_min[1]
					temp_mean_pre1<-round(mean(d_pre1$Ch1_temp),2) 	# temp recorded for each measure section	
					temp_max_pre1<-round(max(d_pre1$Ch1_temp),2)	
					temp_min_pre1<-round(min(d_pre1$Ch1_temp),2)
					O2_mean_pre1<-round(mean(d_pre1[,Ch]),2) 	# O2 recorded for each measure section	
					O2_max_pre1<-round(max(d_pre1[,Ch]),2)	
					O2_min_pre1<-round(min(d_pre1[,Ch]),2)	
					time_frame_pre1<-paste("min", round(start,2), "_", round(cut_start1,2), sep="")
					type_pre1="SMR-pre1"
					values<-as.data.frame(t(c(time_frame_pre1, round(start,2), r2_pre1, b_pre1, m_pre1, temp_min_pre1, temp_max_pre1, temp_mean_pre1,O2_min_pre1, O2_max_pre1, O2_mean_pre1, substr(colnames(d_pre1[Ch]), start=1, stop=3), DateTime_start_pre1, type_pre1, n_min_pre1)))
					colnames(values)<-c("time_frame","min_start", "r2" ,"b", "m" , "t_min", "t_max", "t_mean","O2_min", "O2_max", "O2_mean", "Ch", "DateTime_start", "type", "n_min")	
					newdata<-rbind(newdata, values) # add all values of interest to the individual fish specific data file (this is saved in MMR_SMR function) 
					colnames(newdata)<-c("time_frame","min_start", "r2" ,"b", "m" , "t_min", "t_max", "t_mean","O2_min", "O2_max", "O2_mean", "Ch", "DateTime_start", "type", "n_min")
					
					

					# CUT 1
					# find the lenght min
					cut_length_min<-cut_start1
					# find time in a day
					cut_time_min<-d_cut1$DateTime[1]
					# shallow slope cut 
					DateTime_start_cut1<-as.character(d_cut1$DateTime[1])
					lm_coef_cut1<-coef(lm(d_cut1[,Ch]~d_cut1$time_min)) # get linear regression fit 
					r2_cut1<-round(summary(lm(d_cut1[,Ch]~d_cut1$time_min))$r.squared,3) # get r2
					m_cut1<-round(lm_coef_cut1[2],5) # get slope
					b_cut1<-round(lm_coef_cut1[1],2) # get intercept
					n_min_cut1<-d_cut1$time_min[nrow(d_cut1)]-d_cut1$time_min[1]
			
					o2_cut1<-d_cut1[1,Ch]
					temp_mean_cut1<-round(mean(d_cut1$Ch1_temp),2) 	# temp recorded for each measure section	
					temp_max_cut1<-round(max(d_cut1$Ch1_temp),2)	
					temp_min_cut1<-round(min(d_cut1$Ch1_temp),2)
					O2_mean_cut1<-round(mean(d_cut1[,Ch]),2) 	# O2 recorded for each measure section	
					O2_max_cut1<-round(max(d_cut1[,Ch]),2)	
					O2_min_cut1<-round(min(d_cut1[,Ch]),2)	
					time_frame_cut1<-paste("min", round(d_cut1$time_min[1],2), "_",  round(d_cut1$time_min[nrow(d_cut1)]), sep="")
					start_cut1<-round(d_cut1$time_min[1],2)
					type_cut1="SMR-cut1"
					values<-as.data.frame(t(c(time_frame_cut1,start_cut1, r2_cut1, b_cut1, m_cut1, temp_min_cut1, temp_max_cut1, temp_mean_cut1,O2_min_cut1, O2_max_cut1, O2_mean_cut1, substr(colnames(d_cut1[Ch]), start=1, stop=3), DateTime_start_cut1, type_cut1, n_min_cut1)))
					colnames(values)<-c("time_frame","min_start", "r2" ,"b", "m" , "t_min", "t_max", "t_mean","O2_min", "O2_max", "O2_mean", "Ch", "DateTime_start", "type", "n_min")	
					newdata<-rbind(newdata, values) # add all values of interest to the individual fish specific data file (this is saved in MMR_SMR function) 
					colnames(newdata)<-c("time_frame","min_start", "r2" ,"b", "m" , "t_min", "t_max", "t_mean","O2_min", "O2_max", "O2_mean", "Ch", "DateTime_start", "type", "n_min")


					# pre2 = post cut 1 shallow slope cut1  / pre cut 2
					DateTime_start_pre2<-as.character(d_pre2$DateTime[1])
					lm_coef_pre2<-coef(lm(d_pre2[,Ch]~d_pre2$time_min)) # get linear regression fit 
					r2_pre2<-round(summary(lm(d_pre2[,Ch]~d_pre2$time_min))$r.squared,3) # get r2
					m_pre2<-round(lm_coef_pre2[2],5) # get slope
					b_pre2<-round(lm_coef_pre2[1],2) # get intercept
					n_min_pre2<-d_pre2$time_min[nrow(d_pre2)]-d_pre2$time_min[1]
					temp_mean_pre2<-round(mean(d_pre2$Ch1_temp),2) 	# temp recorded for each measure section	
					temp_max_pre2<-round(max(d_pre2$Ch1_temp),2)	
					temp_min_pre2<-round(min(d_pre2$Ch1_temp),2)
					O2_mean_pre2<-round(mean(d_pre2[,Ch]),2) 	# O2 recorded for each measure section	
					O2_max_pre2<-round(max(d_pre2[,Ch]),2)	
					O2_min_pre2<-round(min(d_pre2[,Ch]),2)	
					time_frame_pre2<-paste("min", round(d_pre2$time_min[1],2), "_",  round(d_pre2$time_min[nrow(d_pre2)]), sep="")
					type_pre2="SMR-pre2"
					values<-as.data.frame(t(c(time_frame_pre2, round(cut_end1,2), r2_pre2, b_pre2, m_pre2, temp_min_pre2, temp_max_pre2, temp_mean_pre2,O2_min_pre2, O2_max_pre2, O2_mean_pre2, substr(colnames(d_pre2[Ch]), start=1, stop=3), DateTime_start_pre2, type_pre2, n_min_pre2)))
					colnames(values)<-c("time_frame","min_start", "r2" ,"b", "m" , "t_min", "t_max", "t_mean","O2_min", "O2_max", "O2_mean", "Ch", "DateTime_start", "type", "n_min")	
					newdata<-rbind(newdata, values) # add all values of interest to the individual fish specific data file (this is saved in MMR_SMR function) 
					colnames(newdata)<-c("time_frame","min_start", "r2" ,"b", "m" , "t_min", "t_max", "t_mean","O2_min", "O2_max", "O2_mean", "Ch", "DateTime_start", "type", "n_min")

					# CUT 2
					# find time in a day
					cut_time_min<-d_cut2$DateTime[1]
					# shallow slope cut 
					DateTime_start_cut2<-as.character(d_cut2$DateTime[1])
					lm_coef_cut2<-coef(lm(d_cut2[,Ch]~d_cut2$time_min)) # get linear regression fit 
					r2_cut2<-round(summary(lm(d_cut2[,Ch]~d_cut2$time_min))$r.squared,3) # get r2
					m_cut2<-round(lm_coef_cut2[2],5) # get slope
					b_cut2<-round(lm_coef_cut2[1],2) # get intercept
					n_min_cut2<-d_cut2$time_min[nrow(d_cut2)]-d_cut2$time_min[1]
					o2_cut2<-d_cut2[1,Ch]
					temp_mean_cut2<-round(mean(d_cut2$Ch1_temp),2) 	# temp recorded for each measure section	
					temp_max_cut2<-round(max(d_cut2$Ch1_temp),2)	
					temp_min_cut2<-round(min(d_cut2$Ch1_temp),2)
					O2_mean_cut2<-round(mean(d_cut2[,Ch]),2) 	# O2 recorded for each measure section	
					O2_max_cut2<-round(max(d_cut2[,Ch]),2)	
					O2_min_cut2<-round(min(d_cut2[,Ch]),2)	
					time_frame_cut2<-paste("min", round(d_cut2$time_min[1],2), "_",  round(d_cut2$time_min[nrow(d_cut2)]), sep="")
					start_cut2<-round(d_cut2$time_min[1],2)
					type_cut2="SMR-cut2"
					values<-as.data.frame(t(c(time_frame_cut2,start_cut2, r2_cut2, b_cut2, m_cut2, temp_min_cut2, temp_max_cut2, temp_mean_cut2,O2_min_cut2, O2_max_cut2, O2_mean_cut2, substr(colnames(d_cut2[Ch]), start=1, stop=3), DateTime_start_cut2, type_cut2, n_min_cut2)))
					colnames(values)<-c("time_frame","min_start", "r2" ,"b", "m" , "t_min", "t_max", "t_mean","O2_min", "O2_max", "O2_mean", "Ch", "DateTime_start", "type", "n_min")	
					newdata<-rbind(newdata, values) # add all values of interest to the individual fish specific data file (this is saved in MMR_SMR function) 
					colnames(newdata)<-c("time_frame","min_start", "r2" ,"b", "m" , "t_min", "t_max", "t_mean","O2_min", "O2_max", "O2_mean", "Ch", "DateTime_start", "type", "n_min")
##print(c("pass 7"))	
					
					# post = post the second cut 
					DateTime_start_post<-as.character(d_post$DateTime[1])
					lm_coef_post<-coef(lm(d_post[,Ch]~d_post$time_min)) # get linear regression fit 
					r2_post<-round(summary(lm(d_post[,Ch]~d_post$time_min))$r.squared,3) # get r2
					m_post<-round(lm_coef_post[2],5) # get slope
					b_post<-round(lm_coef_post[1],2) # get intercept
					n_min_post<-d_post$time_min[nrow(d_post)]-d_post$time_min[1]
					temp_mean_post<-round(mean(d_post$Ch1_temp),2) 	# temp recorded for each measure section	
					temp_max_post<-round(max(d_post$Ch1_temp),2)	
					temp_min_post<-round(min(d_post$Ch1_temp),2)
					O2_mean_post<-round(mean(d_post[,Ch]),2) 	# O2 recorded for each measure section	
					O2_max_post<-round(max(d_post[,Ch]),2)	
					O2_min_post<-round(min(d_post[,Ch]),2)	
					time_frame_post<-paste("min", round(cut_end2,2), "_", round(end,2), sep="")
					type_post="SMR-post"
					values<-as.data.frame(t(c(time_frame_post,round(cut_end2,2), r2_post, b_post, m_post, temp_min_post, temp_max_post, temp_mean_post,  O2_min_post, O2_max_post, O2_mean_post, substr(colnames(d_post[Ch]), start=1, stop=3), DateTime_start_post, type_post, n_min_post)))
					colnames(values)<-c("time_frame","min_start", "r2" ,"b", "m" , "t_min", "t_max", "t_mean","O2_min", "O2_max", "O2_mean", "Ch", "DateTime_start", "type", "n_min")	
					newdata<-rbind(newdata, values) # add all values of interest to the individual fish specific data file (this is saved in MMR_SMR function) 
					colnames(newdata)<-c("time_frame","min_start", "r2" ,"b", "m" , "t_min", "t_max", "t_mean","O2_min", "O2_max", "O2_mean", "Ch", "DateTime_start", "type", "n_min")
					
##print(c("pass 8"))	

					plot(d0[,Ch]~d0$time_min,ylab=expression(paste(O2~(mg~L^{-1}))),xlab="Time (min)")
					points(d_cut1[,Ch]~time_min,d=d_cut1, col="grey75")
					points(d_cut2[,Ch]~time_min,d=d_cut2, col="grey75")
					points(d_pre1[,Ch]~time_min,d=d_pre1, col="green4")
					points(d_pre2[,Ch]~time_min,d=d_pre2, col="red3")
					points(d_post[,Ch]~time_min,d=d_post, col="red3")
	#~ 						abline(lm(d_pre2[,Ch]~d_pre2$time_min), col="red3",lwd=2, lty=2)
	#~ 						abline(lm(d_post[,Ch]~d_post$time_min), col="red3",lwd=2, lty=2)

				}
				
				if(length(n)==3){
					message("more than 2 mini slopes/ plot a full smr slope")
					
					cut_start1<-inv.data.slope[n[1],5]
					cut_end1<-inv.data.slope[n[1],6]
					cut_start2<-inv.data.slope[n[2],5]
					cut_end2<-inv.data.slope[n[2],6]
					cut_start3<-inv.data.slope[n[3],5]
					cut_end3<-inv.data.slope[n[3],6]
					
						# making this so that there is always pre and post
						if(round(as.numeric(start),1)==round(as.numeric(cut_start1),1)){
							cut_start1<-cut_start1+2/60 # adding two seconds
						}
						if(round(as.numeric(end),1)==round(as.numeric(cut_end3),1)){
							cut_end2<-cut_end2-2/60 # substracting two seconds
						}
						
					# section to be cut out 
					d_cut1<-data1[c(which(data1$time_min>cut_start1 & data1$time_min<cut_end1)),] # from teh entire 
					# adjust if there are more than 2 or three new starts / if yes define this
					# DATA FRAME  / little chops 
					d_pre1<-data1[c(which(data1$time_min>start & data1$time_min<cut_start1)),] # section pre "cut"
					d_pre2<-data1[c(which(data1$time_min>cut_end1 & data1$time_min<cut_start2)),] # section pre "cut2"/ also section of the post cut1
					d_cut2<-data1[c(which(data1$time_min>cut_start2 & data1$time_min<cut_end2)),] # section pre "cut"
					d_pre3<-data1[c(which(data1$time_min>cut_end2 & data1$time_min<cut_start3)),] # section pre "cut2"/ also section of the post cut1
					d_cut3<-data1[c(which(data1$time_min>cut_start3 & data1$time_min<cut_end3)),] # section pre "cut"
					d_post<-data1[c(which(data1$time_min>cut_end3 & data1$time_min<end)),] # section pre "cut"
					
				# pre1 shallow slope cut1 
					DateTime_start_pre1<-as.character(d_pre1$DateTime[1])
					lm_coef_pre1<-coef(lm(d_pre1[,Ch]~d_pre1$time_min)) # get linear regression fit 
					r2_pre1<-round(summary(lm(d_pre1[,Ch]~d_pre1$time_min))$r.squared,3) # get r2
					m_pre1<-round(lm_coef_pre1[2],5) # get slope
					b_pre1<-round(lm_coef_pre1[1],2) # get intercept
					n_min_pre1<-d_pre1$time_min[nrow(d_pre1)]-d_pre1$time_min[1]
					temp_mean_pre1<-round(mean(d_pre1$Ch1_temp),2) 	# temp recorded for each measure section	
					temp_max_pre1<-round(max(d_pre1$Ch1_temp),2)	
					temp_min_pre1<-round(min(d_pre1$Ch1_temp),2)
					O2_mean_pre1<-round(mean(d_pre1[,Ch]),2) 	# O2 recorded for each measure section	
					O2_max_pre1<-round(max(d_pre1[,Ch]),2)	
					O2_min_pre1<-round(min(d_pre1[,Ch]),2)	
					time_frame_pre1<-paste("min", round(start,2), "_", round(cut_start1,2), sep="")
					type_pre1="SMR-pre1"
					values<-as.data.frame(t(c(time_frame_pre1, round(start,2), r2_pre1, b_pre1, m_pre1, temp_min_pre1, temp_max_pre1, temp_mean_pre1,O2_min_pre1, O2_max_pre1, O2_mean_pre1, substr(colnames(d_pre1[Ch]), start=1, stop=3), DateTime_start_pre1, type_pre1, n_min_pre1)))
					colnames(values)<-c("time_frame","min_start", "r2" ,"b", "m" , "t_min", "t_max", "t_mean","O2_min", "O2_max", "O2_mean", "Ch", "DateTime_start", "type", "n_min")	
					newdata<-rbind(newdata, values) # add all values of interest to the individual fish specific data file (this is saved in MMR_SMR function) 
					colnames(newdata)<-c("time_frame","min_start", "r2" ,"b", "m" , "t_min", "t_max", "t_mean","O2_min", "O2_max", "O2_mean", "Ch", "DateTime_start", "type", "n_min")
##print(c("pass 9"))	
					
					# CUT 1
					lm_coef_cut1<-coef(lm(d_cut1[,Ch]~d_cut1$time_min)) # get linear regression fit 
					r2_cut1<-round(summary(lm(d_cut1[,Ch]~d_cut1$time_min))$r.squared,3) # get r2
					m_cut1<-round(lm_coef_cut1[2],5) # get slope
					b_cut1<-round(lm_coef_cut1[1],2) # get intercept
					DateTime_start_cut1<-as.character(d_cut1$DateTime[1])
					o2_cut1<-d_cut1[1,Ch]
					n_min_cut1<-d_cut1$time_min[nrow(d_cut1)]-d_cut1$time_min[1]
					temp_mean_cut1<-round(mean(d_cut1$Ch1_temp),2) 	# temp recorded for each measure section	
					temp_max_cut1<-round(max(d_cut1$Ch1_temp),2)	
					temp_min_cut1<-round(min(d_cut1$Ch1_temp),2)
					O2_mean_cut1<-round(mean(d_cut1[,Ch]),2) 	# O2 recorded for each measure section	
					O2_max_cut1<-round(max(d_cut1[,Ch]),2)	
					O2_min_cut1<-round(min(d_cut1[,Ch]),2)	
					time_frame_cut1<-paste("min", round(d_cut1$time_min[1],2), "_",  round(d_cut1$time_min[nrow(d_cut1)]), sep="")
					start_cut1<-round(d_cut1$time_min[1],2)
					type_cut1="SMR-cut1"
					values<-as.data.frame(t(c(time_frame_cut1,start_cut1, r2_cut1, b_cut1, m_cut1, temp_min_cut1, temp_max_cut1, temp_mean_cut1,O2_min_cut1, O2_max_cut1, O2_mean_cut1, substr(colnames(d_cut1[Ch]), start=1, stop=3), DateTime_start_cut1, type_cut1, n_min_cut1)))
					colnames(values)<-c("time_frame","min_start", "r2" ,"b", "m" , "t_min", "t_max", "t_mean","O2_min", "O2_max", "O2_mean", "Ch", "DateTime_start", "type", "n_min")	
					newdata<-rbind(newdata, values) # add all values of interest to the individual fish specific data file (this is saved in MMR_SMR function) 
					colnames(newdata)<-c("time_frame","min_start", "r2" ,"b", "m" , "t_min", "t_max", "t_mean","O2_min", "O2_max", "O2_mean", "Ch", "DateTime_start", "type", "n_min")
##print(c("pass 10"))	


					# pre2 = post cut 1 shallow slope cut1  / pre cut 2
					DateTime_start_pre2<-as.character(d_pre2$DateTime[1])
					lm_coef_pre2<-coef(lm(d_pre2[,Ch]~d_pre2$time_min)) # get linear regression fit 
					r2_pre2<-round(summary(lm(d_pre2[,Ch]~d_pre2$time_min))$r.squared,3) # get r2
					m_pre2<-round(lm_coef_pre2[2],5) # get slope
					b_pre2<-round(lm_coef_pre2[1],2) # get intercept
					n_min_pre2<-d_pre2$time_min[nrow(d_pre2)]-d_pre2$time_min[1]
					temp_mean_pre2<-round(mean(d_pre2$Ch1_temp),2) 	# temp recorded for each measure section	
					temp_max_pre2<-round(max(d_pre2$Ch1_temp),2)	
					temp_min_pre2<-round(min(d_pre2$Ch1_temp),2)
					O2_mean_pre2<-round(mean(d_pre2[,Ch]),2) 	# O2 recorded for each measure section	
					O2_max_pre2<-round(max(d_pre2[,Ch]),2)	
					O2_min_pre2<-round(min(d_pre2[,Ch]),2)	
					time_frame_pre2<-paste("min", round(d_pre2$time_min[1],2), "_",  round(d_pre2$time_min[nrow(d_pre2)]), sep="")
					type_pre2="SMR-pre2"
					values<-as.data.frame(t(c(time_frame_pre2, round(cut_end1,2), r2_pre2, b_pre2, m_pre2, temp_min_pre2, temp_max_pre2, temp_mean_pre2,O2_min_pre2, O2_max_pre2, O2_mean_pre2, substr(colnames(d_pre2[Ch]), start=1, stop=3), DateTime_start_pre2, type_pre2, n_min_pre2)))
					colnames(values)<-c("time_frame","min_start", "r2" ,"b", "m" , "t_min", "t_max", "t_mean","O2_min", "O2_max", "O2_mean", "Ch", "DateTime_start", "type", "n_min")	
					newdata<-rbind(newdata, values) # add all values of interest to the individual fish specific data file (this is saved in MMR_SMR function) 
					colnames(newdata)<-c("time_frame","min_start", "r2" ,"b", "m" , "t_min", "t_max", "t_mean","O2_min", "O2_max", "O2_mean", "Ch", "DateTime_start", "type", "n_min")
##print(c("pass 11"))	

				
					# CUT 2
					DateTime_start_cut2<-as.character(d_cut2$DateTime[1])
					lm_coef_cut2<-coef(lm(d_cut2[,Ch]~d_cut2$time_min)) # get linear regression fit 
					r2_cut2<-round(summary(lm(d_cut2[,Ch]~d_cut2$time_min))$r.squared,3) # get r2
					m_cut2<-round(lm_coef_cut2[2],5) # get slope
					b_cut2<-round(lm_coef_cut2[1],2) # get intercept
					n_min_cut2<-d_cut2$time_min[nrow(d_cut2)]-d_cut2$time_min[1]
					o2_cut2<-d_cut2[1,Ch]
					temp_mean_cut2<-round(mean(d_cut2$Ch1_temp),2) 	# temp recorded for each measure section	
					temp_max_cut2<-round(max(d_cut2$Ch1_temp),2)	
					temp_min_cut2<-round(min(d_cut2$Ch1_temp),2)
					O2_mean_cut2<-round(mean(d_cut2[,Ch]),2) 	# O2 recorded for each measure section	
					O2_max_cut2<-round(max(d_cut2[,Ch]),2)	
					O2_min_cut2<-round(min(d_cut2[,Ch]),2)	
					time_frame_cut2<-paste("min", round(d_cut2$time_min[1],2), "_",  round(d_cut2$time_min[nrow(d_cut2)]), sep="")
					start_cut2<-round(d_cut2$time_min[1],2)
					type_cut2="SMR-cut2"
					values<-as.data.frame(t(c(time_frame_cut2,start_cut2, r2_cut2, b_cut2, m_cut2, temp_min_cut2, temp_max_cut2, temp_mean_cut2,O2_min_cut2, O2_max_cut2, O2_mean_cut2, substr(colnames(d_cut2[Ch]), start=1, stop=3), DateTime_start_cut2, type_cut2, n_min_cut2)))
					colnames(values)<-c("time_frame","min_start", "r2" ,"b", "m" , "t_min", "t_max", "t_mean","O2_min", "O2_max", "O2_mean", "Ch", "DateTime_start", "type", "n_min")	
					newdata<-rbind(newdata, values) # add all values of interest to the individual fish specific data file (this is saved in MMR_SMR function) 
					colnames(newdata)<-c("time_frame","min_start", "r2" ,"b", "m" , "t_min", "t_max", "t_mean","O2_min", "O2_max", "O2_mean", "Ch", "DateTime_start", "type", "n_min")
##print(c("pass 12"))	
			
					
					# pre3 = post cut 3 shallow slope cut2  / pre cut 3
					DateTime_start_pre3<-as.character(d_pre3$DateTime[1])
					lm_coef_pre3<-coef(lm(d_pre3[,Ch]~d_pre3$time_min)) # get linear regression fit 
					r2_pre3<-round(summary(lm(d_pre3[,Ch]~d_pre3$time_min))$r.squared,3) # get r2
					m_pre3<-round(lm_coef_pre3[2],5) # get slope
					b_pre3<-round(lm_coef_pre3[1],2) # get intercept
					n_min_pre3<-d_pre3$time_min[nrow(d_pre3)]-d_pre3$time_min[1]
					temp_mean_pre3<-round(mean(d_pre3$Ch1_temp),2) 	# temp recorded for each measure section	
					temp_max_pre3<-round(max(d_pre3$Ch1_temp),2)	
					temp_min_pre3<-round(min(d_pre3$Ch1_temp),2)
					O2_mean_pre3<-round(mean(d_pre3[,Ch]),2) 	# O2 recorded for each measure section	
					O2_max_pre3<-round(max(d_pre3[,Ch]),2)	
					O2_min_pre3<-round(min(d_pre3[,Ch]),2)	
					time_frame_pre3<-paste("min", round(d_pre3$time_min[1],2), "_",  round(d_pre3$time_min[nrow(d_pre3)]), sep="")
					type_pre3="SMR-pre3"
					values<-as.data.frame(t(c(time_frame_pre3, round(cut_end2,2), r2_pre3, b_pre3, m_pre3, temp_min_pre3, temp_max_pre3, temp_mean_pre3, O2_min_pre3, O2_max_pre3, O2_mean_pre3, substr(colnames(d_pre3[Ch]), start=1, stop=3), DateTime_start_pre3, type_pre3, n_min_pre3)))
					colnames(values)<-c("time_frame","min_start", "r2" ,"b", "m" , "t_min", "t_max", "t_mean","O2_min", "O2_max", "O2_mean", "Ch", "DateTime_start", "type", "n_min")	
					newdata<-rbind(newdata, values) # add all values of interest to the individual fish specific data file (this is saved in MMR_SMR function) 
					colnames(newdata)<-c("time_frame","min_start", "r2" ,"b", "m" , "t_min", "t_max", "t_mean","O2_min", "O2_max", "O2_mean", "Ch", "DateTime_start", "type", "n_min")		
##print(c("pass 13"))	

					
					# CUT 3
					DateTime_start_cut3<-as.character(d_cut3$DateTime[1])
					lm_coef_cut3<-coef(lm(d_cut3[,Ch]~d_cut3$time_min)) # get linear regression fit 
					r2_cut3<-round(summary(lm(d_cut3[,Ch]~d_cut3$time_min))$r.squared,3) # get r2
					m_cut3<-round(lm_coef_cut3[2],5) # get slope
					b_cut3<-round(lm_coef_cut3[1],2) # get intercept
					n_min_cut3<-d_cut3$time_min[nrow(d_cut3)]-d_cut3$time_min[1]
					o2_cut3<-d_cut3[1,Ch]
					temp_mean_cut3<-round(mean(d_cut3$Ch1_temp),2) 	# temp recorded for each measure section	
					temp_max_cut3<-round(max(d_cut3$Ch1_temp),2)	
					temp_min_cut3<-round(min(d_cut3$Ch1_temp),2)
					O2_mean_cut3<-round(mean(d_cut3[,Ch]),2) 	# O2 recorded for each measure section	
					O2_max_cut3<-round(max(d_cut3[,Ch]),2)	
					O2_min_cut3<-round(min(d_cut3[,Ch]),2)	
					time_frame_cut3<-paste("min", round(d_cut3$time_min[1],2), "_",  round(d_cut3$time_min[nrow(d_cut3)]), sep="")
					start_cut3<-round(d_cut3$time_min[1],2)
					type_cut3="SMR-cut3"
					values<-as.data.frame(t(c(time_frame_cut3,start_cut3, r2_cut3, b_cut3, m_cut3, temp_min_cut3, temp_max_cut3, temp_mean_cut3, O2_min_cut3, O2_max_cut3, O2_mean_cut3, substr(colnames(d_cut3[Ch]), start=1, stop=3), DateTime_start_cut3, type_cut3, n_min_cut3)))
					colnames(values)<-c("time_frame","min_start", "r2" ,"b", "m" , "t_min", "t_max", "t_mean","O2_min", "O2_max", "O2_mean", "Ch", "DateTime_start", "type", "n_min")	
					newdata<-rbind(newdata, values) # add all values of interest to the individual fish specific data file (this is saved in MMR_SMR function) 
					colnames(newdata)<-c("time_frame","min_start", "r2" ,"b", "m" , "t_min", "t_max", "t_mean","O2_min", "O2_max", "O2_mean", "Ch", "DateTime_start", "type", "n_min")
##print(c("pass 14"))	
					
					
					# post = post the second cut 
					DateTime_start_post<-as.character(d_post$DateTime[1])
					lm_coef_post<-coef(lm(d_post[,Ch]~d_post$time_min)) # get linear regression fit 
					r2_post<-round(summary(lm(d_post[,Ch]~d_post$time_min))$r.squared,3) # get r2
					m_post<-round(lm_coef_post[2],5) # get slope
					b_post<-round(lm_coef_post[1],2) # get intercept
					temp_mean_post<-round(mean(d_post$Ch1_temp),2) 	# temp recorded for each measure section	
					temp_max_post<-round(max(d_post$Ch1_temp),2)	
					temp_min_post<-round(min(d_post$Ch1_temp),2)
					O2_mean_post<-round(mean(d_post[,Ch]),2) 	# O2 recorded for each measure section	
					O2_max_post<-round(max(d_post[,Ch]),2)	
					O2_min_post<-round(min(d_post[,Ch]),2)	
					time_frame_post<-paste("min", round(cut_end2,2), "_", round(end,2), sep="")
					type_post="SMR-post"
					n_min_post<-d_post$time_min[nrow(d_post)]-d_post$time_min[1]
					values<-as.data.frame(t(c(time_frame_post,round(cut_end3,2), r2_post, b_post, m_post, temp_min_post, temp_max_post, temp_mean_post, O2_min_post, O2_max_post, O2_mean_post, substr(colnames(d_post[Ch]), start=1, stop=3), DateTime_start_post, type_post, n_min_post)))
					colnames(values)<-c("time_frame","min_start", "r2" ,"b", "m" , "t_min", "t_max", "t_mean","O2_min", "O2_max", "O2_mean", "Ch", "DateTime_start", "type", "n_min")	
					newdata<-rbind(newdata, values) # add all values of interest to the individual fish specific data file (this is saved in MMR_SMR function) 
					colnames(newdata)<-c("time_frame","min_start", "r2" ,"b", "m" , "t_min", "t_max", "t_mean","O2_min", "O2_max", "O2_mean", "Ch", "DateTime_start", "type", "n_min")
##print(c("pass 15"))	
				
##					print("plotting")
					plot(d0[,Ch]~d0$time_min,ylab=expression(paste(O2~(mg~L^{-1}))),xlab="Time (min)")
					points(d_cut1[,Ch]~time_min,d=d_cut1, col="grey75")
					points(d_cut2[,Ch]~time_min,d=d_cut2, col="grey75")
					points(d_cut3[,Ch]~time_min,d=d_cut3, col="grey75")
					points(d_pre1[,Ch]~time_min,d=d_pre1, col="green4")
					points(d_pre2[,Ch]~time_min,d=d_pre2, col="red3")
					points(d_pre3[,Ch]~time_min,d=d_pre3, col="red3")
					points(d_post[,Ch]~time_min,d=d_post, col="red3")
				}	
				
													
			if (i == length(seq_end)){
				dev.off()
			}
				
		}
	
	}	
	
	
	# assign("newdata", newdata, envir=.GlobalEnv)	
	return(newdata)
	
}

# this function is called inside the MMR_SMR function to analyze data of each channel
# don't need to worry about the variable inside this function, the MMR_SMR provides them all
Channel<-function(Ch, temp, seq_st, seq_end, plotname, data1, chop_start, chop_end,  inv.data, newdata, plot_temp, N_Ch2){ # Ch = colum of the channel not the actual channel
	
	# if(!exists("newdata")){
	# 	newdata<-as.data.frame("new")
	# }
  
  # print(newdata)
	
  if(plot_temp==TRUE){
    # temp plot name 
    sub_directory_name<-gsub(pattern="plots_channel", replacement= "plots_channel_temperature", x= plotname)
    plotname_temperature<-gsub(pattern=".png", replacement= "_temp.png", x= sub_directory_name)
  }
  
	if(!N_Ch2==8){
		if (Ch==4){
		  temp<-5
		}
		if (Ch==6){
		  temp<-5
		}
		if (Ch==7){
		}
		if (Ch==8){
		  temp<-5
		}
	}else{
   if (Ch==4){
  		  temp<-5
  		}
  		if (Ch==6){
  		  temp<-9
  		}
  		if (Ch==7){
  		  temp<-10
  		}
  		if (Ch==8){
  		  temp<-11
  		}
	  }

	if(nrow(inv.data)==0){
		if(Ch==4){message(paste("Ch1: NO inventory data"))}else{message(paste("Ch",Ch-4 ,": NO inventory data", sep=""))}
	  # print(length(seq_st))
		# run through each "measure cycle" and get slope, intercept, r2. All channel cycles plotted together in one file 
		for (i in 1:length(seq_end)){
			
			start<-seq_st[i]+chop_start # chopping off first x min
			end<-seq_end[i]-chop_end # chopping off last x min
			
			d<-data1[c(which(data1$time_min>start & data1$time_min<end)),] # from teh entire files, get only the data section we need 
			
			DateTime_start<-as.character(d$DateTime[1])
			lm_coef<-coef(lm(d[,Ch]~d$time_min)) # get linear regression fit 
			r2<-round(summary(lm(d[,Ch]~d$time_min))$r.squared,3) # get r2
			
			m<-round(lm_coef[2],5) # get slope
			b<-round(lm_coef[1],2) # get intercept
			
				# if this is the first cycle for this fish - start a new data file
				if(newdata[1,1]=="new"){
					
					newdata<-matrix(ncol=15,nrow=0)
					colnames(newdata)<-c("time_frame","min_start", "r2" ,"b", "m" , "t_min", "t_max", "t_mean","O2_min", "O2_max", "O2_mean", "Ch", "DateTime_start", "type", "n_min")	
				}
			
				# if this is the first cycle for this fish - start a new plot
			
				if (i == 1){					
					if(length(seq_st)<=100){
						png(plotname, width=40, height=40, units="in",  res=200)  
						par(mfrow=c(10,10)) # This will fit 100 plots. 
					}
					if(length(seq_st)>100 & length(seq_st)<=150){
						png(plotname, width=40, height=60, units="in",  res=200)  
						par(mfrow=c(15,10)) # This will fit 150 plots
					}
					if(length(seq_st)>150 & length(seq_st)<=225){
						png(plotname, width=50, height=50, units="in",  res=200)  
						par(mfrow=c(15,15)) # This will fit 150 plots
					}
					if(length(seq_st)>=225 & length(seq_st)<=375){
  			  	png(plotname, width=50, height=70, units="in",  res=200)  
  				  par(mfrow=c(15,25)) # This will fit 375 plots
  		    }
  		    if(length(seq_st)>375 ){
  			   	png(plotname, width=65, height=80, units="in",  res=200)  
  			  	par(mfrow=c(18,35)) # This will >  plots
  			  	# print("here")
  		    }
				}
				
			# plotting each segment with slope, r2, and equations on it		
			plot(d[,Ch]~time_min, d=d, ylab=expression(paste(O2~(mg~L^{-1}))),xlab="Time (min)")
			abline(lm(d[,Ch]~d$time_min), col="red",lwd=2)
			mtext(bquote(y == .(lm_coef[2])*x + .(lm_coef[1])), adj=1, padj=0, cex=0.8, line=0) # display equation 
			mtext(bquote(italic(R)^2 == .(format(r2, digits = 3))),adj=1, padj=0, cex=0.8, line=1)
			# the bwlow code for the frames
				
				if(r2<0.9){
					box(lty="dashed", col="orange", lwd=2)
				}
				if(r2<0.85){
					box(lty="solid", col="orange", lwd=2)
				}
				if(r2<0.80){
					box(lty="solid", col="red", lwd=2)
				}
				# if this is the last cycle for this fish - save the plot
				if (i == length(seq_end)){
					dev.off()
				}
		
			# temp recorded for each measurement cycle		
			temp_mean<-round(mean(d[,temp]),2) # paste this value on the plot
			temp_max<-round(max(d[,temp]),2)	
			temp_min<-round(min(d[,temp]),2)	
			n_min<-d$time_min[nrow(d)]-d$time_min[1]
			
			#oxygen recorded for each measurement cycle 
			O2_mean<-round(mean(d[,Ch], na.rm=TRUE),2)
			O2_max<-round(max(d[,Ch], na.rm=TRUE),2)	
			O2_min<-round(min(d[,Ch], na.rm=TRUE),2)	
		  
			type<-"SMR"
			time_frame<-paste("min", round(start,2), "_", round(end,2), sep="")
			values<-as.data.frame(t(c(time_frame,start, r2, b, m, temp_min, temp_max, temp_mean, O2_min, O2_max, O2_mean, substr(colnames(d[Ch]), start=1, stop=3), DateTime_start,type, n_min)))
			colnames(values)<-c("time_frame","min_start", "r2" ,"b", "m" , "t_min", "t_max", "t_mean","O2_min", "O2_max", "O2_mean", "Ch", "DateTime_start", "type", "n_min")	

			newdata<-rbind(newdata, values) # add all values of interest to the individual fish specific data file (this is saved in MMR_SMR function) 
			colnames(newdata)<-c("time_frame","min_start", "r2" ,"b", "m" , "t_min", "t_max", "t_mean","O2_min", "O2_max", "O2_mean", "Ch", "DateTime_start", "type", "n_min")	
		}
	  
	  if(plot_temp==TRUE){
      # run thrugh the loop one more time, but only for temperature
      # no inventory data
      for (i in 1:length(seq_end)){
        
        start<-seq_st[i]+chop_start # chopping off first x min
  			end<-seq_end[i]-chop_end # chopping off last x min
  			
  			d<-data1[c(which(data1$time_min>start & data1$time_min<end)),] # from teh entire files, get only the data section we need 
  			# print(plotname_temperature)
        ### only temp plots: 
    			# if this is the first cycle for this fish - start a new plot
    			if (i == 1){
    
    				if(length(seq_st)<=100){
    					png(plotname_temperature, width=40, height=40, units="in",  res=200)
    					par(mfrow=c(10,10)) # This will fit 100 plots.
    				}
    				if(length(seq_st)>100 & length(seq_st)<=150){
    					png(plotname_temperature, width=40, height=60, units="in",  res=200)
    					par(mfrow=c(15,10)) # This will fit 150 plots
    				}
    				if(length(seq_st)>150 & length(seq_st)<=225){
    					png(plotname_temperature, width=50, height=50, units="in",  res=200)
    					par(mfrow=c(15,15)) # This will fit 150 plots
    				}
        		if(length(seq_st)>=225 & length(seq_st)<=375){
    			  	png(plotname_temperature, width=50, height=70, units="in",  res=200)  
    				  par(mfrow=c(15,25)) # This will fit 375 plots
    		    }
    		    if(length(seq_st)>375 ){
    			   	png(plotname_temperature, width=65, height=80, units="in",  res=200)  
    			  	par(mfrow=c(18,35)) # This will > 525 plots
    		    }
    
    			}
    
    				# plotting each segment with slope, r2, and equations on it
    			plot(d[,temp]~time_min, d=d, ylab="Temperature (C)", xlab="Time (min)", col="darkgreen")
    				# if this is the last cycle for this fish - save the plot
    				if (i == length(seq_end)){
    					dev.off()
    				}
    ### end temp plots
       } # end of the temp plot loop
	  }
	  
	}
  
	if(nrow(inv.data)>0){
	  if(Ch==4){message(paste("Ch1: Inventory: data are cleaned for quality sections only"))}else{message(paste("Ch",Ch-4 ,": Inventory: data are cleaned for quality sections only", sep =""))}
	  cols = c(3, 4, 5, 6)
		
		inv.data[,cols] %<>% lapply(function(x) as.numeric(as.character(x)))
		
		inv.data.clean<-inv.data[!(inv.data$type=="slope_analysis"),]
		
		for (i in 1:length(seq_end)){
			# if this is the first cycle for this fish - start a new data file
				if(as.character(newdata[1,1])=="new"){
					newdata<-matrix(ncol=15,nrow=0)
					colnames(newdata)<-c("time_frame","min_start", "r2" ,"b", "m" , "t_min", "t_max", "t_mean","O2_min", "O2_max", "O2_mean", "Ch", "DateTime_start", "type", "n_min")	
				}
					
				# if this is the first cycle for this fish - start a new plot
				if (i == 1){
					
					if(length(seq_st)<=100){
						png(plotname, width=40, height=40, units="in",  res=200)  
						par(mfrow=c(10,10)) # This will fit 100 plots. 
					}
					if(length(seq_st)>100 & length(seq_st)<=150){
						png(plotname, width=40, height=60, units="in",  res=200)  
						par(mfrow=c(15,10)) # This will fit 150 plots
					}
					if(length(seq_st)>150){
						png(plotname, width=50, height=50, units="in",  res=200)  
						par(mfrow=c(15,15)) # This will fit 150 plots
					}
						
				}
									
			start<-seq_st[i]+chop_start # chopping off first x min from the beginning of each slopes
			end<-seq_end[i]-chop_end # chopping off last x min from the end of each slope 
		
				
				n2<-which((inv.data.clean[,4]>=start-1 & inv.data.clean[,4]<end+1) | (inv.data.clean[,5]>=start-1 & inv.data.clean[,5]<end+1) | (inv.data.clean[,6]>=start-1 & inv.data.clean[,6]<end+1))
				
				if(length(n2)==0){
					cycle_use<-"use full cycle"
				}else{

				  # print(n2)
					# replace zeros in the inventory data to real values; in inv. data clean - this is where in invenotory file I added 0 when it just starts from the "start" of teh cycle and ends at the "end" of the cycle
				  if(inv.data.clean[n2,5] == 0 & inv.data.clean[n2,6] == 0){
				    cycle_use<-"skip cycle"
				  }else{
				    cycle_use<-"use cleaned cycle"
				  }
				  
				  if(inv.data.clean[n2,4]==0){
						inv.data.clean[n2,4]<-start
					} 
					if(inv.data.clean[n2,5]==0){
						inv.data.clean[n2,5]<-start
					} 						
					if(inv.data.clean[n2,6]==0){
						inv.data.clean[n2,6]<-end
					}
					
				  # if(start==inv.data.clean[n2,5] & end==inv.data.clean[n2,6]){
				  

					
					d_clean<-data1[c(which(data1$time_min>inv.data.clean[n2,5] & data1$time_min<inv.data.clean[n2,6])),]

				}

			if (cycle_use=="use full cycle" | cycle_use=="use cleaned cycle" | cycle_use=="skip cycle"){	
				d0<-data1[c(which(data1$time_min>start & data1$time_min<end)),] # from the entire files, get only the data section we need / not cleaned no need to be cleaned

					if(cycle_use=="use cleaned cycle"){
						# cleaned section only
						DateTime_start_clean<-as.character(d_clean$DateTime[1])
						lm_coef_clean<-coef(lm(d_clean[,Ch]~d_clean$time_min)) # get linear regression fit 
						r2_clean<-round(summary(lm(d_clean[,Ch]~d_clean$time_min))$r.squared,3) # get r2
						m_clean<-round(lm_coef_clean[2],5) # get slope
						b_clean<-round(lm_coef_clean[1],2) # get intercept
						n_min_clean<-d_clean$time_min[nrow(d_clean)]-d_clean$time_min[1]
						
						# temp recorded for each measure cycle		
						temp_mean_clean<-round(mean(d_clean[,temp]),2) # paste this value on the plot
						temp_max_clean<-round(max(d_clean[,temp]),2)	
						temp_min_clean<-round(min(d_clean[,temp]),2)	
						
						# O2 recorded for each measure cycle		
						O2_mean_clean<-round(mean(d_clean[,Ch]),2) 
						O2_max_clean<-round(max(d_clean[,Ch]),2)	
						O2_min_clean<-round(min(d_clean[,Ch]),2)	
						
						time_frame<-paste("min", round(inv.data.clean[n2,5],2), "_", round(inv.data.clean[n2,6],2), sep="")
						type="SMR"
            
						if(!cycle_use=="skip cycle"){
						  values<-as.data.frame(t(c(time_frame, inv.data.clean[n2,5], r2_clean, b_clean, m_clean, temp_min_clean, temp_max_clean, temp_mean_clean,O2_min_clean, O2_max_clean, O2_mean_clean, substr(colnames(d_clean[Ch]), start=1, stop=3), DateTime_start_clean, type, n_min_clean)))
						  colnames(values)<-c("time_frame","min_start", "r2" ,"b", "m" , "t_min", "t_max", "t_mean","O2_min", "O2_max", "O2_mean", "Ch", "DateTime_start", "type", "n_min")	
						}
						
					}else{
						# full data / not cleaned
						DateTime_start0<-as.character(d0$DateTime[1])
						lm_coef0<-coef(lm(d0[,Ch]~d0$time_min)) # get linear regression fit 
						r20<-round(summary(lm(d0[,Ch]~d0$time_min))$r.squared,3) # get r2
						m0<-round(lm_coef0[2],5) # get slope
						b0<-round(lm_coef0[1],2) # get intercept
						n_min0<-d0$time_min[nrow(d0)]-d0$time_min[1]	
						# temp recorded for each measure cycle		
						temp_mean0<-round(mean(d0[,temp]),2) # paste this value on the plot
						temp_max0<-round(max(d0[,temp]),2)	
						temp_min0<-round(min(d0[,temp]),2)	
						
						# O2 recorded for each measure cycle		
						O2_mean0<-round(mean(d0[,Ch]),2) # paste this value on the plot
						O2_max0<-round(max(d0[,Ch]),2)	
						O2_min0<-round(min(d0[,Ch]),2)	
						
						time_frame<-paste("min", round(start,2), "_", round(end,2), sep="")
						type="SMR"
					
  						if(!cycle_use=="skip cycle"){
  						  values<-as.data.frame(t(c(time_frame, start, r20, b0, m0, temp_min0, temp_max0, temp_mean0,O2_min0, O2_max0, O2_mean0, substr(colnames(d0[Ch]), start=1, stop=3), DateTime_start0, type, n_min0)))
  						  colnames(values)<-c("time_frame","min_start", "r2" ,"b", "m" , "t_min", "t_max", "t_mean","O2_min", "O2_max", "O2_mean", "Ch", "DateTime_start", "type", "n_min")	
  						}
						}
											
				# plotting each segment with slope, r2, and equations on it		
				plot(d0[,Ch]~time_min, d=d0, ylab=expression(paste(O2~(mg~L^{-1}))),xlab="Time (min)")
					if(cycle_use=="use cleaned cycle"){
						rect(xleft=inv.data.clean[n2,5], ybottom=min(d0[,Ch]),xright=inv.data.clean[n2,6],ytop=max(d0[,Ch]) ,col= alpha("grey", 0.3), border=NA)
						points(d_clean[,Ch]~time_min, d=d_clean, ylab=expression(paste(O2~(mg~L^{-1}))),xlab="Time (min)")
						abline(lm(d_clean[,Ch]~d_clean$time_min), col="red",lwd=2)
						mtext(bquote(y == .(lm_coef_clean[2])*x + .(lm_coef_clean[1])), adj=1, padj=0, cex=0.8, line=0) # display equation 
						mtext(bquote(italic(R)^2 == .(format(r2_clean, digits = 3))),adj=1, padj=0, cex=0.8, line=1)
						
						if(r2_clean<0.9){
							box(lty="dashed", col="orange", lwd=2)
						}
						if(r2_clean<0.85){
							box(lty="solid", col="orange", lwd=2)
						}
						if(r2_clean<0.80){
							box(lty="solid", col="red", lwd=2)
						}
						
					}else{
						abline(lm(d0[,Ch]~d0$time_min), col="red",lwd=2)
						mtext(bquote(y == .(lm_coef0[2])*x + .(lm_coef0[1])), adj=1, padj=0, cex=0.8, line=0) # display equation 
						mtext(bquote(italic(R)^2 == .(format(r20, digits = 3))),adj=1, padj=0, cex=0.8, line=1)
						
						if(cycle_use=="skip cycle"){
						  legend("center","center", c("EXCLUDING \n from dataset"),  pch=" ", cex=2)
						  box(lty="solid", col="red", lwd=3)
						}
						  
						if(r20<0.9){
							box(lty="dashed", col="orange", lwd=2)
						}
						if(r20<0.85){
							box(lty="solid", col="orange", lwd=2)
						}
						if(r20<0.80){
							box(lty="solid", col="red", lwd=2)
						}
					}
 
					
					# if this is the last cycle for this fish - save the plot
					if (i == length(seq_end)){
						dev.off()
					}		
				
				if(!cycle_use=="skip cycle"){
  				newdata<-rbind(newdata, values) # add all values of interest to the individual fish specific data file (this is saved in MMR_SMR function) 
  				colnames(newdata)<-c("time_frame","min_start", "r2" ,"b", "m" , "t_min", "t_max", "t_mean","O2_min", "O2_max", "O2_mean", "Ch", "DateTime_start", "type", "n_min")	
				}
			}
	
		}
	
	  # run thrugh the loop one more time, but only for temperature
    # WITH inventory data
		if(plot_temp==TRUE){
      for (i in 1:length(seq_end)){
     
      start<-seq_st[i]+chop_start # chopping off first x min from the beginning of each slopes
			end<-seq_end[i]-chop_end # chopping off last x min from the end of each slope 
	
			n2<-which((inv.data.clean[,4]>=start-1 & inv.data.clean[,4]<end+1) | (inv.data.clean[,5]>=start-1 & inv.data.clean[,5]<end+1) | (inv.data.clean[,6]>=start-1 & inv.data.clean[,6]<end+1))
				
				if(length(n2)==0){
					cycle_use<-"use full cycle"
				  }else{

					# replace zeros in the inventory data to real values; in inv. data clean - this is where in invenotory file I added 0 when it just starts from the "start" of teh cycle and ends at the "end" of the cycle
					if(inv.data.clean[n2,4]==0){
						inv.data.clean[n2,4]<-start
					} 
					if(inv.data.clean[n2,5]==0){
						inv.data.clean[n2,5]<-start
					} 						
					if(inv.data.clean[n2,6]==0){
						inv.data.clean[n2,6]<-end
					}
					if(start==inv.data.clean[n2,5] & end==inv.data.clean[n2,6]){
						cycle_use<-"skip cycle"
					}else{
						cycle_use<-"use cleaned cycle"
					}
					
					d_clean<-data1[c(which(data1$time_min>inv.data.clean[n2,5] & data1$time_min<inv.data.clean[n2,6])),]

			  } # finalizing start and end values

			if (cycle_use=="use full cycle" | cycle_use=="use cleaned cycle"){	# this is needed to NOT plot cycles that are discarded
				d0<-data1[c(which(data1$time_min>start & data1$time_min<end)),] # use the entire files to get only the data section sectin secified by cleaning 
       
    		# if this is the first cycle for this fish - start a new temperature plot
    		if (i == 1){
    			if(length(seq_st)<=100){
    				png(plotname_temperature, width=40, height=40, units="in",  res=200)
    				par(mfrow=c(10,10)) # This will fit 100 plots.
    			}
    			if(length(seq_st)>100 & length(seq_st)<=150){
    				png(plotname_temperature, width=40, height=60, units="in",  res=200)
    				par(mfrow=c(15,10)) # This will fit 150 plots
    			}
    			if(length(seq_st)>150){
    				png(plotname_temperature, width=50, height=50, units="in",  res=200)
    				par(mfrow=c(15,15)) # This will fit 150 plots
    			}
    		}
  
  			# plotting each segment with slope, r2, and equations on it
  		  plot(d0[,temp]~time_min, d=d0, ylab="Temperature (C)", xlab="Time (min)", col="darkgreen")
  		  # data plotted, not make the grey box for the cleaned section
  			if(cycle_use=="use cleaned cycle"){
  				rect(xleft=inv.data.clean[n2,5], ybottom=min(d0[,temp]),xright=inv.data.clean[n2,6],ytop=max(d0[,temp]) ,col= alpha("grey", 0.3), border=NA)
  			} 
  			# if this is the last cycle for this fish - save the plot
  		  if (i == length(seq_end)){
  				dev.off()
  			}
			}
			
    }
		}
	
	}
		
	
	#assign("newdata", newdata, envir=.GlobalEnv)	
	return(newdata)
  # return(N_Ch)
	
}

# some experimental design checks  - this plots the entire cycle of flush + MR measure. With veritcal line whetre the flush should start.
# this is great to see if the cycles stay on track and you are getting the slope/ section that you actually need/want
Channel_cycle<-function(Ch, seq_end,seq_st, flush, plotname, data1, plot_temp, N_Ch3){
	
   if(plot_temp==TRUE){
    # temp plot name 
    sub_directory_name<-gsub(pattern="plots_channel_cycle", replacement= "plots_channel_temperature_cycle", x= plotname)
    plotname_temperature<-gsub(pattern=".png", replacement= "_temp.png", x= sub_directory_name)
  
    # print( plotname_temperature)
    }
  
	cycle_time<-seq_end[2]-seq_end[1]
	for (i in 1:length(seq_end)){
		
		end<-seq_end[i] 
		start<-end-cycle_time
		
		d<-data1[c(which(data1$time_min>start & data1$time_min<end)),]
			
		if (i == 1){
			message("plot -cycle")
			if(length(seq_st)<=100){
				png(plotname, width=40, height=40, units="in",  res=200)  
				par(mfrow=c(10,10)) # This will fit 100 plots. 
			}
			if(length(seq_st)>100 & length(seq_st)<=150){
				png(plotname, width=40, height=60, units="in",  res=200)  
				par(mfrow=c(15,10)) # This will fit 150 plots
			}
			if(length(seq_st)>=150 & length(seq_st)<=225){
				png(plotname, width=50, height=50, units="in",  res=200)  
				par(mfrow=c(15,15)) # This will fit 225 plots
			}
		  if(length(seq_st)>=225 & length(seq_st)<=375){
				png(plotname, width=50, height=70, units="in",  res=200)  
				par(mfrow=c(15,25)) # This will fit 375 plots
		  }
		  if(length(seq_st)>375 ){
				png(plotname, width=65, height=80, units="in",  res=200)  
				par(mfrow=c(18,35)) # This will > 525 plots
		  }
		  if(length(seq_st)>525){message("more than 525 measurement cycles - channel plots re-set, first 525 not plotted")}
		}
				
			plot(d[,Ch]~time_min, d=d, ylab=expression(paste(O2~(mg~L^{-1}))),xlab="Time (min)")
			abline(v=start+flush, col="red")
						
		if (i == length(seq_end)){
			dev.off()
		}
				
	}
	
	# same loop just for temperature
	if (plot_temp==TRUE){
		for (i in 1:length(seq_end)){
		
  		end<-seq_end[i] 
  		start<-end-cycle_time
  		
  		d<-data1[c(which(data1$time_min>start & data1$time_min<end)),]
  	    if (i == 1){
  			message("plot -cycle temp")
  			if(length(seq_st)<=100){
  				png(plotname_temperature, width=40, height=40, units="in",  res=200)  
  				par(mfrow=c(10,10)) # This will fit 100 plots. 
  			}
  			if(length(seq_st)>100 & length(seq_st)<=150){
  				png(plotname_temperature, width=40, height=60, units="in",  res=200)  
  				par(mfrow=c(15,10)) # This will fit 150 plots
  			}
  			if(length(seq_st)>150 & length(seq_st)<=225){
  				png(plotname_temperature, width=50, height=50, units="in",  res=200)  
  				par(mfrow=c(15,15)) # This will fit 150 plots
  			}
  	     if(length(seq_st)>=225 & length(seq_st)<=375){
			  	png(plotname_temperature, width=50, height=70, units="in",  res=200)  
				  par(mfrow=c(15,25)) # This will fit 375 plots
		    }
		    if(length(seq_st)>375 ){
			   	png(plotname_temperature, width=65, height=80, units="in",  res=200)  
			  	par(mfrow=c(18,35)) # This will > 525 plots
		    }
  		}
  		
    		if(!N_Ch3==8){
      		if (Ch==4){
      		  temp<-5
      		}
      		if (Ch==6){
      		  temp<-5
      		}
      		if (Ch==7){
      		  temp<-5
      		}
      		if (Ch==8){
      		  temp<-5
      		}
    		}else{
    		  if (Ch==4){
      		  temp<-5
      		}
      		if (Ch==6){
      		  temp<-9
      		}
      		if (Ch==7){
      		  temp<-10
      		}
      		if (Ch==8){
      		  temp<-11
      		}
    		}
  		
  	  plot(d[,temp]~time_min, d=d, ylab="Temperature (C)", xlab="Time (min)", col="darkgreen")
  		abline(v=start+flush, col="red")

    		if (i == length(seq_end)){
    			dev.off()
    		}
		} # end of loop
	}
	
}
 
	  
SMR<-function(data, cycle_start, cycle_end, chop_start, chop_end, N_Ch = 4, inventory_data = NA, flush_plot = "OFF", path = ".", date_format = c("m/d/y","d/m/y","y-m-d"), plot_temp = FALSE, test="AerobicScope"){
	
	newdata<-as.data.frame("new")
	
	# create folders internallly
  
	data1<-read.csv(data)
	
	# current directory if no specific foders are used 
	if(path == "."){
	  plotname1<-paste(gsub('.{4}$', '', data), "_all_ChO2.png", sep='')
	  
	  plotname2<-paste( gsub('.{4}$', '', data), "_full.png", sep='')
  	plotname2.1<-paste( gsub('.{4}$', '', data), "_Ch1.png", sep='')
  	plotname2.2<-paste( gsub('.{4}$', '', data), "_Ch2.png", sep='')
  	plotname2.3<-paste( gsub('.{4}$', '', data), "_Ch3.png", sep='')
  	plotname2.4<-paste( gsub('.{4}$', '', data), "_Ch4.png", sep='')
  
  	plotname2.1c<-paste( gsub('.{4}$', '', data), "_cycle_Ch1.png", sep='')
  	plotname2.2c<-paste( gsub('.{4}$', '', data), "_cycle_Ch2.png", sep='')
  	plotname2.3c<-paste( gsub('.{4}$', '', data), "_cycle_Ch3.png", sep='')
  	plotname2.4c<-paste( gsub('.{4}$', '', data), "_cycle_Ch4.png", sep='')

  	plotname_full<-paste(gsub('.{4}$', '', data), "ALL_TOGETHER.png", sep='')
  	
  	filename<-paste(gsub('.{4}$', '', data), "_analyzed.csv", sep='') # save file in the current SMR wd
	  filename2<-paste(gsub('.{4}$', '', data), "_analyzed.csv", sep='') # save in the EPOC_AS etc folder for final SMR and fiull EPOC analysis

	}else{ 
	  plotname1<-paste("../plots_summary_respo/", gsub('.{4}$', '', data), "_all_ChO2.png", sep='')
  
	  plotname2<-paste("../plots_channel/", gsub('.{4}$', '', data), "_full.png", sep='')
  	plotname2.1<-paste("../plots_channel/", gsub('.{4}$', '', data), "_Ch1.png", sep='')
  	plotname2.2<-paste("../plots_channel/", gsub('.{4}$', '', data), "_Ch2.png", sep='')
  	plotname2.3<-paste("../plots_channel/", gsub('.{4}$', '', data), "_Ch3.png", sep='')
  	plotname2.4<-paste("../plots_channel/", gsub('.{4}$', '', data), "_Ch4.png", sep='')
  
  	plotname2.1c<-paste("../plots_channel_cycle/", gsub('.{4}$', '', data), "_cycle_Ch1.png", sep='')
  	plotname2.2c<-paste("../plots_channel_cycle/", gsub('.{4}$', '', data), "_cycle_Ch2.png", sep='')
  	plotname2.3c<-paste("../plots_channel_cycle/", gsub('.{4}$', '', data), "_cycle_Ch3.png", sep='')
  	plotname2.4c<-paste("../plots_channel_cycle/", gsub('.{4}$', '', data), "_cycle_Ch4.png", sep='')

  	plotname_full<-paste("../plots_all_together/",gsub('.{4}$', '', data), "ALL_TOGETHER.png", sep='')
  	
  	filename<-paste("../csv_analyzed/", gsub('.{4}$', '', data), "_analyzed.csv", sep='') # save file in the current SMR wd
	 
  	if(test=="SDA"){
  	 filename2<-paste("../csv_input_files/", gsub('.{4}$', '', data), "_analyzed.csv", sep='') # save in the EPOC_AS etc folder for final SMR and full EPOC analysis

  	 }else{
  	 filename2<-paste("../../MMR_SMR_AS_EPOC/csv_input_files/", gsub('.{4}$', '', data), "_analyzed.csv", sep='') # save in the EPOC_AS etc folder for final SMR and full EPOC analysis
  	 }

	}
	
	if(plot_temp==TRUE){
    if(!dir.exists("../plots_channel_temperature")){
      dir.create(file.path("../","plots_channel_temperature"), recursive = TRUE)
     if(flush_plot=="ON"){
	    dir.create(file.path("../","plots_channel_temperature_cycle"), recursive = TRUE)
     }
    }
	}
	
	
	if (is.na(inventory_data)){
		data2<-as.data.frame(matrix(nrow=0, ncol=0))
	}else{		
		data2<-read.csv(inventory_data)
		 if (plot_temp ==TRUE){
        message("Temperature plots are not saved when using inventory files / cleaning data")
      }
	}
		
	if(as.character(data1$Ch1_O2[1])=="--- " || as.character(data1$Ch1_O2[1])=="---"){
		data1$Ch1_O2<-0
	}
	if(as.character(data1$Ch2_O2[1])=="--- " || as.character(data1$Ch2_O2[1])=="---"){
		data1$Ch2_O2<-0
	}
	if(as.character(data1$Ch3_O2[1])=="--- " || as.character(data1$Ch3_O2[1])=="---"){
		data1$Ch3_O2<-0
	}
	if(as.character(data1$Ch4_O2[1])=="--- " || as.character(data1$Ch4_O2[1])=="---"){
		data1$Ch4_O2<-0
	}
		
	data1$Ch1_temp<-as.numeric(as.character(data1$Ch1_temp))
	data1$Ch1_O2<-as.numeric(as.character(data1$Ch1_O2))
	data1$Ch2_O2<-as.numeric(as.character(data1$Ch2_O2))
	data1$Ch3_O2<-as.numeric(as.character(data1$Ch3_O2))
	data1$Ch4_O2<-as.numeric(as.character(data1$Ch4_O2))
	
	if (N_Ch==8){
  	data1$Ch2_temp<-as.numeric(as.character(data1$Ch2_temp))
  	data1$Ch3_temp<-as.numeric(as.character(data1$Ch3_temp))
  	data1$Ch4_temp<-as.numeric(as.character(data1$Ch4_temp))
  		#
  }

	# #temperature
	# temp_mean<-mean(data1$Ch1_temp, na.rm=TRUE)
	# temp_max<-max(data1$Ch1_temp, na.rm=TRUE)	
	# temp_min<-min(data1$Ch1_temp, na.rm=TRUE)	

	#oxygen
	O2_mean1<-mean(data1$Ch1_O2, na.rm=TRUE)
	O2_max1<-max(data1$Ch1_O2, na.rm=TRUE)	
	O2_min1<-min(data1$Ch1_O2, na.rm=TRUE)	
  
	O2_mean2<-mean(data1$Ch2_O2, na.rm=TRUE)
	O2_max2<-max(data1$Ch2_O2, na.rm=TRUE)	
	O2_min2<-min(data1$Ch2_O2, na.rm=TRUE)	
	
	O2_mean3<-mean(data1$Ch3_O2, na.rm=TRUE)
	O2_max3<-max(data1$Ch3_O2, na.rm=TRUE)	
	O2_min3<-min(data1$Ch3_O2, na.rm=TRUE)
	
	O2_mean4<-mean(data1$Ch4_O2, na.rm=TRUE)
	O2_max4<-max(data1$Ch4_O2, na.rm=TRUE)	
	O2_min4<-min(data1$Ch4_O2, na.rm=TRUE)	
	
	message (paste("Minimal O2: ", "[Ch1: ", round(O2_min1,2),"] [Ch2: ", round(O2_min2,2), "] [Ch3: ", round(O2_min3,2), "] [Ch4: ", round(O2_min4,2), "] (mgO2/L)", sep=""))

	data1$date<-as.character(data1$date)
	data1$time<-as.character(data1$time)

	if (date_format== "m/d/y"){
  	DateTime<- chron(dates=data1$date,times=data1$time,format=c('m/d/y','h:m:s')) # Eliason Lab firesting date format
#~ 	DateTime<- chron(dates=data1$date,times=data1$time,format=c('y-m-d','h:m:s')) # Healy firesting date format 
	}
	if (date_format== "d/m/y"){
    # 8 channel firesting with date (day first) European style
    DateTime<- chron(dates=data1$date,times=data1$time,format=c('d/m/y','h:m:s')) # Eliason Lab firesting date format
	}
	if (date_format== "y-m-d"){
    # 8 channel firesting with date (day first) European style
    DateTime<- chron(dates=data1$date,times=data1$time,format=c('y-m-d','h:m:s')) # Eliason Lab firesting date format
  }
# 	if (N_Ch!=8){
#   	DateTime<- chron(dates=data1$date,times=data1$time,format=c('m/d/y','h:m:s')) # Eliason Lab firesting date format
# #~ 	DateTime<- chron(dates=dataMMR$date,times=dataMMR$time,format=c('y-m-d','h:m:s')) # Healy firesting date format 
#   }else{
#     # 8 channel firesting with date (day first) European style
#     DateTime<- chron(dates=data1$date,times=data1$time,format=c('d/m/y','h:m:s')) # Eliason Lab firesting date format
#   }
# 	
	data1$DateTime<-DateTime
	data1$hr<-round(data1$time_sec/3600,2) 
	data1$hr2<-round(data1$time_sec/3600,0) 
	data1$time_min<-round(data1$time_sec/60,2) 

	# gives min and max O2 in each discrete hr
	sum<-data1 %>% 
	  group_by(hr2) %>%
	  summarize(
	   minO2 = min(Ch1_O2),
	   maxO2 = max(Ch1_O2)
	  )

	hr_min_mean<-mean(sum$minO2)
	hr_max_mean<-mean(sum$maxO2)

	png(plotname1, width=25, height=18,units="in",  res=200)
	
	par(mfrow=c(5,1))
	# cannel 1
	plot(data1$time_min, data1$Ch1_O2, main=paste(data1$DateTime[1], "Channel1"))
	abline(h=O2_min1, lty=2, col="darkred")
	abline(h=hr_min_mean, lty=1, col="darkred")	
	# mtext(paste("temp_min=", temp_min, "temp_max=", temp_max),adj=1, padj=0, cex=0.8, line=1)

	# channel 2
	plot(data1$time_min, data1$Ch2_O2, main=paste(data1$DateTime[1], "Channel2"))
	abline(h=O2_min2, lty=2, col="darkred")
	abline(h=hr_min_mean, lty=1, col="darkred")	
	
	# channel 3
	plot(data1$time_min, data1$Ch3_O2, main=paste(data1$DateTime[1], "Channel3"))
	abline(h=O2_min3, lty=2, col="darkred")
	abline(h=hr_min_mean, lty=1, col="darkred")	
	
	# channel 4
	plot(data1$time_min, data1$Ch4_O2, main=paste(data1$DateTime[1], "Channel4"))
	abline(h=O2_min4, lty=2, col="darkred")
	abline(h=hr_min_mean, lty=1, col="darkred")	
	
	# temperature trace
	plot(data1$time_min, data1$Ch1_temp, main= "Temperature", col="grey", ylim=c(5, 35))
	if (N_Ch>4){
	  	points(data1$time_min, data1$Ch2_temp, main= "Temperatures", col="brown")
	  	points(data1$time_min, data1$Ch3_temp, main= "Temperature", col="orange")
    	points(data1$time_min, data1$Ch4_temp, main= "Temperature", col="purple")
	  }
	dev.off()
	
	# make this interval adjustable 
	t_m<-data1$time_min[nrow(data1)] # the max min in the file
	seq_st<-seq(cycle_start, t_m, cycle_end)
	seq_end<-seq(cycle_end, t_m, cycle_end)
	flush<-seq_st[1]
	

	# write a channel function prior and call it here
	# channel 1

	# channel first argument -> col # for the Channel 1: 4 Ch2: 6, Ch3:7 Ch4:8
	# channel second and third arguments: seq_start and seq_end
	
	# find box number regardless of where it is written in the datafile name
	Box_n_data<-(which(!is.na(str_locate(data, c("box", "BOX", "Box"))))[2])

	rows<-c(4,6,7,8) # the order Ch1, Ch2, Ch3, Ch4
	
	if(N_Ch==8){
  	rows_temp<-c(5,9,10,11) # the order Ch1, Ch2, Ch3, Ch4
	  }else {
	 	rows_temp<-c(5,5,5,5) # the order Ch1, Ch2, Ch3, Ch4
	}


		if (!data1$Ch1_O2[1]==0){
			inv.data<-data2[which(!is.na(str_locate(data, as.character(data2$date))) &
			                        as.numeric(data2$channel)==1 &
			                        grepl(substr(data, start = (str_locate(data, c("box", "BOX", "Box"))[Box_n_data])+1,
			                                     stop = (str_locate(data, c("box", "BOX", "Box"))[Box_n_data])+1), as.character(data2$box))),]
		
		
			if(nrow(inv.data)==0 || (!nrow(inv.data)==0 & !(inv.data$type=="slope_analysis"))){
			  
				newdata<-Channel(Ch=rows[1], temp=rows_temp[1], seq_st, seq_end, plotname2.1, data1, chop_start, chop_end, inv.data, newdata, plot_temp, N_Ch) # Ch 1 (col 4 in data1) 
			# for "partitioned" section or slope analysis. e.g. lobster apnea
			}else{
				newdata<-Channel_slope_analysis(Ch=rows[1], seq_st, seq_end, plotname2.1, data1, chop_start, chop_end, inv.data, newdata) # Ch 1 (col 4 in data1) 
			}
			
		}
	
		if (!data1$Ch2_O2[1]==0){
			inv.data<-data2[which(!is.na(str_locate(data, as.character(data2$date))) & as.numeric(data2$channel)==2 &
			                        grepl(substr(data, start = (str_locate(data, c("box", "BOX", "Box"))[Box_n_data])+1,
			                                     stop = (str_locate(data, c("box", "BOX", "Box"))[Box_n_data])+1), as.character(data2$box))),]
			
			if(nrow(inv.data)==0 || (!nrow(inv.data)==0 & !(inv.data$type=="slope_analysis"))){
				newdata<-Channel(Ch=rows[2], temp=rows_temp[2], seq_st, seq_end, plotname2.2, data1, chop_start, chop_end, inv.data, newdata, plot_temp, N_Ch) # Ch 2 (col 6 in data1)
			# for "partitioned" section or slope analysis. e.g. lobster apnea
			}else{
				newdata<-Channel_slope_analysis(Ch=6, seq_st, seq_end, plotname2.2, data1, chop_start, chop_end, inv.data, newdata) # Ch 1 (col 4 in data1) 
			}
			
		}
	
		if (!data1$Ch3_O2[1]==0){
			inv.data<-data2[which(!is.na(str_locate(data, as.character(data2$date))) & as.numeric(data2$channel)==3 &
			                        grepl(substr(data, start = (str_locate(data, c("box", "BOX", "Box"))[Box_n_data])+1,
			                                     stop = (str_locate(data, c("box", "BOX", "Box"))[Box_n_data])+1), as.character(data2$box))),]
			
			if(nrow(inv.data)==0 || (!nrow(inv.data)==0 & !(inv.data$type=="slope_analysis"))){
				newdata<-Channel(Ch=rows[3], temp=rows_temp[3], seq_st, seq_end, plotname2.3, data1, chop_start, chop_end, inv.data, newdata, plot_temp, N_Ch) # Ch 3 (col 7 in data1)
			}else{
				newdata<-Channel_slope_analysis(Ch=7, seq_st, seq_end, plotname2.3, data1, chop_start, chop_end, inv.data, newdata) # Ch 3 (col 7 in data1)
			}	
			
		}
	
		if (!data1$Ch4_O2[1]==0){
			inv.data<-data2[which(!is.na(str_locate(data, as.character(data2$date))) & as.numeric(data2$channel)==4 &
			                       grepl(substr(data, start = (str_locate(data, c("box", "BOX", "Box"))[Box_n_data])+1,
			                                     stop = (str_locate(data, c("box", "BOX", "Box"))[Box_n_data])+1), as.character(data2$box))),]
			
			if(nrow(inv.data)==0 || (!nrow(inv.data)==0 &!(inv.data$type=="slope_analysis"))){
				newdata<-Channel(Ch=rows[4], temp=rows_temp[4], seq_st, seq_end, plotname2.4, data1, chop_start, chop_end, inv.data, newdata, plot_temp, N_Ch) # Ch 4 (col 8 in data1)
			}else{
				newdata<-Channel_slope_analysis(Ch=8, seq_st, seq_end, plotname2.4, data1, chop_start, chop_end, inv.data, newdata) # Ch 4 (col 8 in data1)
			}

		}

  	if (flush_plot=="ON"){
  		Channel_cycle(4, seq_end, seq_st, flush, plotname2.1c, data1, plot_temp, N_Ch)
  		Channel_cycle(6, seq_end, seq_st, flush, plotname2.2c, data1, plot_temp, N_Ch)
  		Channel_cycle(7, seq_end, seq_st, flush, plotname2.3c, data1, plot_temp, N_Ch)
  		Channel_cycle(8, seq_end, seq_st, flush, plotname2.4c, data1, plot_temp, N_Ch)
  	}
		
	newdata$min_start<-as.numeric(as.character(newdata$min_start))
	newdata$m<-abs(as.numeric(as.character(newdata$m)))
	newdata$ID_code<-substr(data,start=6, stop=14) # modify this to your needs

	graphics.off()
	

	plot<-ggplot(data=newdata, aes(as.numeric(min_start), as.numeric(m), col=as.factor(Ch)) )+
		geom_point()+
		geom_line()+
		theme_classic()+
		ylab("Slope (in absolute value)")+
		xlab("time (min)")+
		theme(legend.title=element_blank())


	png(plotname_full, width=14, height=5, res=200, units='in')
		print(plot)
	dev.off()

	write.csv(file=filename, newdata, row.names=FALSE)
	# print(filename2)
	write.csv(file=filename2, newdata, row.names=FALSE)

	# newdata<-as.data.frame("new")
	# 
	# assign("newdata", newdata, envir=.GlobalEnv)	
	# return(newdata)
	
}



#### MMR functions ##################### 

MMRslide<-function(d, Ch, data.MMR, r, r_temp, newdata_mmr, path){
    
		# for the sliding ones: cycle_type=MMR_slide
		# for the sliding ones: cycle_start=the min on when the slide starts 
		# for the sliding ones: cycle_end=the min on when the slide ends
		# for the sliding ones: delay_min=NA
		# for the sliding ones: cycle_mmr= 60,90,120,180 (indicate the sliding length
    newdata_set<-matrix(ncol=12, nrow=0)
    colnames(newdata_set)<-c("cycle_type", "cycle_start","cycle_end",  "cycle_mmr", "r2" ,"m", "b" , "t_min", "t_max", "t_mean", "Ch", "DateTime_start")
    
    newdata_setMean<-matrix(ncol=14, nrow=0)
    colnames(newdata_setMean)<-c("cycle_type", "cycle_startMean","cycle_endMean", "cycle_mmrMean","min_m", "mean_m" ,"sd_m", "min_r2", "mean_r2" ,"sd_r2", "t_min", "t_max", "t_mean", "Ch")
    #~ 		normalize time to have all files start at 0 
			cycle_type="MMR_slide"

			for (i in 2:nrow(d)){
			  d$time_sec[i]<-d$time_sec[i]-d$time_sec[1]
			}
			d$time_sec[1]<-0
			
			endtime<-d$time_sec[nrow(d)]
			starttime<-d$time_sec[1]
			
			if(endtime>180){
				# message("FILE > 3 min")
##			length_slide<-c(10,20,30,60,90,120,180)
				length_slide<-c(60,90,120,180)
				# timeDiff_slide<-c(1)

				for (k in 1:length(length_slide)){
											
					string_eval <- sprintf("
						for (i in seq(starttime,(endtime-%s), by=%s)){
					
							l<-i+%s
							
							data_set<-d[d$time_sec>i & d$time_sec<l, ]
							DateTime_start<-as.character(data_set$DateTime[1])
																			
							fit_set<-lm(data_set[,r]~data_set$time_min)
							lm_coef_set <- round(coef(fit_set), 5) 	
							r2_set<-round(as.numeric(as.character(summary(fit_set)$adj.r.squared)),2)
						
							interc_set<-round(lm_coef_set[1],2)
							slope_set<-round(lm_coef_set[2],6)
							
							temp_mean_set<-round(mean(data_set[,r_temp]),2)
							temp_max_set<-round(max(data_set[,r_temp]),2)	
							temp_min_set<-round(min(data_set[,r_temp]),2)	
							
							cycle_start<-data_set$time_min[1]
							cycle_end<-data_set$time_min[nrow(data_set)]
							cycle_mmr<-%s
							
							values_set<-as.data.frame(t(c(cycle_type, cycle_start, cycle_end,  cycle_mmr, r2_set ,slope_set, interc_set , temp_min_set, temp_max_set, temp_mean_set, Ch, DateTime_start)))	
							colnames(values_set)<-c(\"cycle_type\", \"cycle_start\",\"cycle_end\", \"cycle_mmr\", \"r2\" ,\"m\", \"b\" , \"t_min\", \"t_max\", \"t_mean\", \"Ch\", \"DateTime_start\")
						
							newdata_set<-rbind(newdata_set, values_set)
												
						}
							
									
					", length_slide[k], 1, length_slide[k],length_slide[k] )
					eval(parse(text = string_eval)) 
				}
				

				for (i in 1:length(length_slide)){
								
					subsetName<-paste("s",length_slide[i],"_1", sep="")
					subsetD<-newdata_set[newdata_set$cycle_mmr ==length_slide[i],]

					assign(subsetName, subsetD)
				
				}
				
					list<-list(s60_1, s90_1, s120_1, s180_1)
			
    					
    		newdata_set$cycle_mmr<- as.numeric(as.character(newdata_set$cycle_mmr))
    		
    		if(path == "."){
    		  filename_set<-paste( gsub('.{4}$', '', data.MMR), "_SLIDINGset", Ch , ".csv", sep='')
    		  filename_setMean<-paste( gsub('.{4}$', '', data.MMR), "_SLIDINGsetMean", Ch , ".csv", sep='')
    		  plotnameMean<-paste( gsub('.{4}$', '', data.MMR), "_SLIDING_MMRanalysis", Ch , ".png", sep='')
    
    		}else{
    		  filename_set<-paste("../channel_sliding_sets/", gsub('.{4}$', '', data.MMR), "_SLIDINGset", Ch , ".csv", sep='')
    		  filename_setMean<-paste("../channel_sliding_sets/", gsub('.{4}$', '', data.MMR), "_SLIDINGsetMean", Ch , ".csv", sep='')
    		  plotnameMean<-paste("../channel_plots_MMRanalysis/", gsub('.{4}$', '', data.MMR), "_SLIDING_MMRanalysis", Ch , ".png", sep='')
        }
    		
    		write.csv(newdata_set,file=filename_set, row.names=FALSE)	
    		
    		newdata_set<-as.data.frame(newdata_set)
    		## Jan 2022: 
    		newdata_set$cycle_mmr<-factor(newdata_set$cycle_mmr)
    		newdata_set$m<-as.numeric(as.character(newdata_set$m))
    		newdata_set$r2<-as.numeric(as.character(newdata_set$r2))
    		
    		yScaleAdjustVal<-1+abs(mean((newdata_set$m)))

    		png(filename = plotnameMean, height=5, width=6, res=200, units="in")
          print(ggplot(data=newdata_set, aes(x=cycle_mmr, y=m))+
          theme_light()+
          # geom_hline(yintercept = -1*(abs(mean((newdata_set$m)))), color = "darkred", alpha=0.6, lty=2)+
          xlab("Time of sliding windows (sec)")+
          scale_y_continuous(name = "Oxygen decrease slope - proxy MMR", sec.axis = sec_axis(trans = ~.+yScaleAdjustVal, breaks = c(1, 0.99, 0.95, 0.8, 0.5), name = expression(R^2)))+
    		  geom_boxplot(width = 0.5, mapping = aes(x=cycle_mmr, y=r2-yScaleAdjustVal), fill="white", color = "darkred", position = position_nudge(x = +0.5))+
    		  geom_point( mapping = aes(x=cycle_mmr, y=r2-yScaleAdjustVal), fill="white", color = "darkred", alpha=0.5, position = position_nudge(x = +0.5), pch=1, size=1)+
          geom_boxplot(width = 0.5, fill="white", color = "grey")+
          geom_point(pch=1, size=1)+
          ggtitle(paste(Ch, " - MMR metrics", sep="")))
        dev.off()
    		
    		for (i in 1:length(list)){
    			
    			df_s<-list[[i]]
    			
    			df_s$r2<-as.numeric(as.character(df_s$r2))
    			df_s$m<-as.numeric(as.character(df_s$m))
    			r2_set_max<-(which(df_s$r2 == max(df_s$r2)))
    			slope_set_max<-(which(df_s$m == min(df_s$m)))# slope is negative steepest slope is the min(slope)
    				
    			if(length(slope_set_max)==1){
    				mmrMax<-df_s[slope_set_max,]
    		  	}else{
    				# selecting the one with highest r2
    				message(paste(df_s$Ch[1], ": Several MMR slopes: n = ", length(slope_set_max), sep=""))
    				maxset<-df_s[c(slope_set_max),]
    					if(var(maxset$r2)==0){
    					message(df_s$Ch[1], ": Steepest slopes have equivalently good R2, use the first slope recorded", sep="")
    						mmrMax<-maxset[1,]
    						# if the same them I am selecting the first one - that is also plotted 
    												
    					}else{
    						message(df_s$Ch[1], ": Steepest slopes with best R2 selected", sep="")
    						bestr2<-which(maxset$r2 == max(maxset$r2))
    						mmrMax<-maxset[bestr2,]
    
    					}
    			}
    
    			cols<-c(2:6,8:10)
    			newdata_mmr[,cols] %<>% lapply(function(x) as.numeric(as.character(x)))
    			mmrMax[,cols] %<>% lapply(function(x) as.numeric(as.character(x)))
    
    			newdata_mmr<-rbind(newdata_mmr,mmrMax)
    			
    			
    			# auto correlation
    		
    			### create a sliding for the mean slopes
    			# if(nrow(df_s) > 60){
    				# for (t in 1:(nrow(df_s)-60)){
    				# 
    				# 	k<-t+60
    				# 	
    				# 	data_setMean<-df_s[t:k, ]
    				# 
    				# 	temp_mean_setMean<-mean(as.numeric(as.character(data_setMean$t_mean)))
    				# 	temp_min_setMean<-min(as.numeric(as.character(data_setMean$t_min)))
    				# 	temp_max_setMean<-max(as.numeric(as.character(data_setMean$t_max)))
    				# 
    				# 	cycle_startMean<-as.character(data_setMean$cycle_start[1])
    				# 	cycle_endMean<-as.character(data_setMean$cycle_start[nrow(data_setMean)])
    				# 	cycle_mmrMean<-as.character(data_setMean$cycle_mmr[1])
    				# 	
    				# 	mean_m<-mean(as.numeric(as.character(data_setMean$m)))
    				# 	min_m<-min(as.numeric(as.character(data_setMean$m)))
    				# 	sd_m<-sd(as.numeric(as.character(data_setMean$m)))
    				# 	
    				# 	mean_r2<-mean(as.numeric(as.character(data_setMean$r2)))
    				# 	min_r2<-min(as.numeric(as.character(data_setMean$r2)))
    				# 	sd_r2<-sd(as.numeric(as.character(data_setMean$r2)))
    				# 	
    				# 	# get mean and sd for r2
    				# 
    				# 	values_setMean<-as.data.frame(t(c("Mean_1minSlopes", cycle_startMean, cycle_endMean, cycle_mmrMean, min_m, mean_m , sd_m, min_r2, mean_r2, sd_r2, temp_min_setMean, temp_max_setMean, temp_mean_setMean, Ch)))	
    				# 	colnames(values_setMean)<-c("cycle_type", "cycle_startMean","cycle_endMean", "cycle_mmrMean","min_m", "mean_m" ,"sd_m", "min_r2", "mean_r2" ,"sd_r2", "t_min", "t_max", "t_mean", "Ch")
    				# 
    				# 	newdata_setMean<-rbind(newdata_setMean, values_setMean)
    				# 						
    				# }
    			# }
    			
    	  	}
      # find the lowest value and add it to a dataframe
    		# 
    		# listMean<-split(newdata_setMean, newdata_setMean$cycle_mmrMean)
    		# 
    		# for (v in 1:length(listMean)){
    		#   df_sMean<-listMean[[v]]
    		# 	
    		# 	df_sMean$mean_r2<-as.numeric(as.character(df_sMean$mean_r2))
    		# 	df_sMean$min_r2<-as.numeric(as.character(df_sMean$min_r2))
    		# 	df_sMean$mean_m<-as.numeric(as.character(df_sMean$mean_m))
    		# 	r2_set_maxMean<-(which(df_sMean$mean_r2 == max(df_sMean$mean_r2)))
    		# 	slope_set_maxMean<-(which(df_sMean$mean_m == min(df_sMean$mean_m)))# slope is negative steepest slope is the min(slope)
    		# 		
    		# 	if(length(slope_set_maxMean)==1){
    		# 		mmrMaxMean<-df_sMean[slope_set_maxMean,]
    		#     }else{
    		# 		# selecting the one with the lowest mo2 included, and then with the highest min r2
    		# 		maxsetMean<-df_sMean[c(slope_set_maxMean),]
    		# 		mmrMaxMean<-maxsetMean[which(maxsetMean$min_r2==max(maxsetMean$min_r2))[1],] # if the same them I am selecting the first one, with the highest r2
    		#   }
    		# 	
    		# 	cols<-c(9,6,7,11)
    		# 	mmrMaxMean[,cols] %<>% lapply(function(x) as.numeric(as.character(x)))
    		# 	
    		# 	mmrMaxMean_vals<-as.data.frame(t(c("Mean_1minSlopes", mmrMaxMean[,2],
    		# 	                                   as.character(mmrMaxMean[,3]), as.character( mmrMaxMean[,4]),
    		# 	                                   as.character(round(mmrMaxMean[,9],4)), as.character(round(mmrMaxMean[,6], 4)),
    		# 	                                   paste("SD_slope:",as.character(round(mmrMaxMean[,7], 4))), as.character(mmrMaxMean[,11]),
    		# 	                                   as.character(mmrMaxMean[,12]), as.character(mmrMaxMean[,13]), as.character(mmrMaxMean[,14]),
    		# 	                                   as.character(df_s$DateTime_start[1]))))	
    		# 	colnames(mmrMaxMean_vals)<-c("cycle_type", "cycle_start","cycle_end",  "cycle_mmr", "r2" ,"m", "b" , "t_min", "t_max", "t_mean", "Ch", "DateTime_start")	
    		# 	
    		# 	newdata_mmr<-rbind(newdata_mmr, mmrMaxMean_vals)
    		# }
    		  
    #     cols<-c(2:13)
    # 	  newdata_setMean[,cols] %<>% lapply(function(x) as.numeric(as.character(x)))
    # 		newdata_setMean[,cols] %<>% lapply(function(x) as.numeric(as.character(x)))
    # 
    # 		# best slopes using the singly fastest slope analysis
    # 		bestdata<-newdata_mmr[c(newdata_mmr$cycle_type=="MMR_slide" & newdata_mmr$Ch==Ch), ]
    # 		names(bestdata)[names(bestdata) == 'cycle_mmr'] <- 'cycle_mmrMean'
    # 		
    # 		bestdata$cycle_mmrMean<-as.numeric(as.character(bestdata$cycle_mmrMean))
    # 		# newdata_setMean$cycle_mmrMean<-factor(newdata_setMean$cycle_mmrMean, levels=c('60','90','120','180'))
    # 		
    # 		newdata_setMean$cycle_startMean+0.5
    # 		
    #   	 error_plot_m<-ggplot(data=newdata_setMean, aes(y=mean_m, x=cycle_startMean+0.5))+
    #   	    geom_point(pch=21, size=2, fill="black")+
    #   	    geom_point(aes(y=min_m, x=cycle_startMean+0.5), pch=1, size=1)+
    #   	    geom_errorbar(aes(ymin=mean_m+sd_m, ymax=mean_m-sd_m), width=0.1, size=0.2)+
    #   	    theme_light()+
    #   	    geom_abline(data=bestdata, aes(slope=0, intercept=as.numeric(as.character(m))), lty=2, colour="red", size=1)+
    #   	    ggtitle(paste(Ch, " slope", sep=""))+
    #   	    facet_grid(cycle_mmrMean~.)
    #   	            
    #   	 error_plot_r2<-ggplot(data=newdata_setMean, aes(y=mean_r2, x=cycle_startMean+0.5))+
    #   	    geom_point(pch=21, size=2, color="red", fill="red")+
    #   	    geom_point(aes(y=min_r2, x=cycle_startMean+0.5), pch=1, color="red", size=1)+
    #   	    geom_errorbar(aes(ymin=mean_r2+sd_r2, ymax=mean_r2-sd_r2), width=0.1, size=0.2, colour="red")+
    #   	    theme_light()+
    #   	    ggtitle(paste(Ch, " r2", sep=""))+
    #   	    facet_grid(cycle_mmrMean~.)
    #   	 
    #      png(plotnameMean, height=10, width=5, res=300, units="in")
    #   	   grid.arrange(error_plot_m, error_plot_r2, ncol=1, nrow=2)
    #      dev.off()
    #      
    #      write.csv(newdata_setMean,file=filename_setMean, row.names=FALSE)
    # 
    		
    		#      png(plotnameMean, height=10, width=5, res=300, units="in")
    		#   	   grid.arrange(mmrMeans_plot_m, error_plot_r2, ncol=1, nrow=2)
    		#      dev.off()
    		
    		
			}else{
				 
		  	message(paste(Ch, ": file < 3 min: no steepest slope analysis"))
			# message(c(starttime, endtime))
			}
	
			
		return(newdata_mmr)
		
			
		
	}

# dataMMR = the actual data file
# data.MMR = 
# cycle_start
# cycle_end
# Ch_list 
# cycles
# j
# rows =
# rows_temp = 
	
	
Channel_mmr<-function(data.MMR, dataMMR, cycle_start, cycle_end, clean_start_mmr, clean_end_mmr, Ch_list, cycles, j, rows, rows_temp, newdata_mmr, path, inv.data.clean){

		if(!Ch_list[j]==0){
			mmr_start<-clean_start_mmr[j]
			mmr_end<-clean_end_mmr[j]
			r<-rows[j]
			r_temp<-rows_temp[j]
			cycle_mmr<-Ch_list[j]
			
			d<-dataMMR[c(which(dataMMR$time_min>(mmr_start) & dataMMR$time_min<mmr_end)),]
			
			lm_coef<-coef(lm(d[,r]~d$time_min))
			r2<-round(summary(lm(d[,r]~d$time_min))$r.squared,3) 
			
			m<-round(lm_coef[2],5)
			b<-round(lm_coef[1],2)
			
			clean_cycle2<-NA
			clean_cycle3<-NA
			clean_cycle4<-NA
			
			### is there cleaning? 
  		if(nrow(inv.data.clean)>0){
  		   message(paste("Ch",j, ": INVENTORY data, adjusting non-MMR cycle times", sep=""))
  	      # find cycle 2 cleaning
  		   # print(clean_cycle2)
  	      if(any(inv.data.clean$cycle==2)){
  	         clean_cycle2<-"Y"
  	         # print(clean_cycle2)
  	         # if(!inv.data.clean[which(inv.data.clean$cycle==2),"start"] == 0){
  	           start_c2<-inv.data.clean[which(inv.data.clean$cycle==2),"start"] # min start
  	           end_c2<-inv.data.clean[which(inv.data.clean$cycle==2),"end"]
  	         # }
  	         # else{
  	         #   start_c2<-cycle_start[2]
  	         #   end_c2<-cycle_end[2]
  	         # }
  	         # print(start_c2)
  	      }
  		   # find cycle 3 cleaning
  	      if(any(inv.data.clean$cycle==3)){
  	        # print(inv.data.clean)
  	         clean_cycle3<-"Y"
  	         # if(!inv.data.clean[which(inv.data.clean$cycle==3),"start"] == 0){
    	         start_c3<-inv.data.clean[which(inv.data.clean$cycle==3),"start"] # min start
    	         end_c3<-inv.data.clean[which(inv.data.clean$cycle==3),"end"]
#   	         }
#   	         else{
# 	            start_c3<-0
# 	            end_c3<-0
#   	         }
    	         
  	      }
  		  # find cycle 4 cleaning
  	      if(any(inv.data.clean$cycle==4)){
  	         # print(inv.data.clean)
  	         clean_cycle4<-"Y"
  	         # if(!inv.data.clean[which(inv.data.clean$cycle==4),"start"] == 0){
    	         start_c4<-inv.data.clean[which(inv.data.clean$cycle==4),"start"] # min start
    	         end_c4<-inv.data.clean[which(inv.data.clean$cycle==4),"end"]
  	         # }
  	         # else{
  	         #   start_c4<-cycle_start[4]
  	         #   end_c4<-cycle_end[4]
  	         # }
  	      }
  		}
			
			###  end to cleaning times for cycles 
			
				# print(c(clean_cycle2, start_c2, end_c2, j))
				
			## these are standard cycles without any cleaning 
			if (cycles>=2){
			  if(is.na(clean_cycle2)){
			    # start_c2<-cycle_start[2]
			    # end_c2<-cycle_end[2]
				  d2<-dataMMR[c(which(dataMMR$time_min>cycle_start[2] & dataMMR$time_min<cycle_end[2])),]
			  }else{
			    if(start_c2==0 & end_c2 ==0){
			      d2<-dataMMR[c(which(dataMMR$time_min>cycle_start[2] & dataMMR$time_min<cycle_end[2])),]
			    }else{
			      d2<-dataMMR[c(which(dataMMR$time_min>start_c2 & dataMMR$time_min<end_c2)),]
			    }
			  }
  				lm_coef.2<-coef(lm(d2[,r]~d2$time_min))
  				r2.2<-round(summary(lm(d2[,r]~d2$time_min))$r.squared,3) 
  				m.2<-round(lm_coef.2[2],5)
  				b.2<-round(lm_coef.2[1],2)
			}
			if (cycles>=3){
			 if(is.na(clean_cycle3)){
  		   # start_c3<-cycle_start[3]
  		   # end_c3<-cycle_end[3]
			   d3<-dataMMR[c(which(dataMMR$time_min>cycle_start[3] & dataMMR$time_min<cycle_end[3])),]
			 }else{
			   if(start_c3==0 & end_c3 ==0){
			     d3<-dataMMR[c(which(dataMMR$time_min>cycle_start[3] & dataMMR$time_min<cycle_end[3])),]
			   }else{
			     d3<-dataMMR[c(which(dataMMR$time_min>start_c3 & dataMMR$time_min<end_c3)),]
			   }
			 }  
			  
				lm_coef.3<-coef(lm(d3[,r]~d3$time_min))
				r2.3<-round(summary(lm(d3[,r]~d3$time_min))$r.squared,3) 
				m.3<-round(lm_coef.3[2],5)
				b.3<-round(lm_coef.3[1],2)
			}
			if (cycles>=4){
			  if(is.na(clean_cycle4)){
			    # start_c4<-cycle_start[4]
			    # end_c4<-cycle_end[4]
				  d4<-dataMMR[c(which(dataMMR$time_min>cycle_start[4] & dataMMR$time_min<cycle_end[4])),]
			  }else{
			    if(start_c4==0 & end_c4 ==0){
			    }
			      d4<-as.data.frame(dataMMR[c(which(dataMMR$time_min>start_c4 & dataMMR$time_min<end_c4)),])
			    # print(c(end_c4, start_c4))
			  }
				lm_coef.4<-coef(lm(d4[,r]~d4$time_min))
				r2.4<-round(summary(lm(d4[,r]~d4$time_min))$r.squared,3) 
				m.4<-round(lm_coef.4[2],5)
				b.4<-round(lm_coef.4[1],2)
			}
			
  		if (path == "."){
         plotname<-paste( gsub('.{4}$', '', data.MMR),"_", substr(colnames(d[r]), start=1, stop=3), ".png", sep="")#
     
      	}else{
      	 plotname<-paste("../channel_plots/", gsub('.{4}$', '', data.MMR),"_", substr(colnames(d[r]), start=1, stop=3), ".png", sep="")#
    	}
  			

			# all MMR file trace
			p %<a-%{
				plot(dataMMR[,r]~time_min, data=dataMMR, ylab=expression(paste(O2~(mg~L^{-1}))),xlab="Time (min)", main="The entire MMR file");
				rect(xleft=mmr_start[1],ybottom=min(dataMMR[,r]),xright=mmr_end[1],ytop=max(dataMMR[,r]) ,col= '#FF003322', border=NA)
			}
					
			# MMR plots
			if (Ch_list[j]==1 & cycles>=1){

				DateTime_start<-as.character(d$DateTime[1])
				cycle_type<-"MMR"
				Ch<-paste("Ch",j, sep="")
				
				values_mmr<-as.data.frame(t(c(cycle_type, mmr_start, mmr_end,  cycle_mmr, r2, m, b,
				                              round(min(d[,r_temp]), 4), 
				                              round(max(d[,r_temp]), 4),
				                              round(mean(d[,r_temp]), 4),
				                              Ch ,DateTime_start )))
				colnames(values_mmr)<-c("cycle_type","cycle_start","cycle_end",  "cycle_mmr", "r2" ,"m", "b" , "t_min", "t_max", "t_mean", "Ch", "DateTime_start")	
				newdata_mmr<-rbind(newdata_mmr, values_mmr)
				
				
				print(newdata_mmr)
				newdata_mmr<-MMRslide(d, Ch, data.MMR, r, r_temp, newdata_mmr, path)
				print(newdata_mmr)
				

				newdata_mmr60<-newdata_mmr[which(newdata_mmr$Ch == Ch & newdata_mmr$cycle_mmr == 60),]
				newdata_mmr90<-newdata_mmr[which(newdata_mmr$Ch == Ch & newdata_mmr$cycle_mmr == 90),]
				newdata_mmr120<-newdata_mmr[which(newdata_mmr$Ch == Ch & newdata_mmr$cycle_mmr == 120),]

				# print(c(Ch_list[j], Ch))
				
				p1 %<a-% {
				  plot(d[,r]~time_min, data=d, col="grey", ylab=expression(paste(O2~(mg~L^{-1}))), xlab="Time (min)",main="MMR");		
				  abline(lm(d[,r]~d$time_min), col="black",lwd=2);
				  if(nrow(newdata_mmr60)>0){
				    
				    ablineclip(lm(d[which(d$time_min > newdata_mmr60$cycle_start &
				                            d$time_min < newdata_mmr60$cycle_end),r]~
				                    d[which(d$time_min > newdata_mmr60$cycle_start &
				                              d$time_min < newdata_mmr60$cycle_end),"time_min"]), # purple
				               col=rgb(red = 128, green = 0, blue = 128, alpha = 230, max = 255),
				               lty = "solid", lwd=4,lend="round",
				               x1=newdata_mmr60$cycle_start, x2=newdata_mmr60$cycle_end);
				    ablineclip(lm(d[which(d$time_min > newdata_mmr90$cycle_start &
				                            d$time_min < newdata_mmr90$cycle_end),r]~
				                    d[which(d$time_min > newdata_mmr90$cycle_start &
				                              d$time_min < newdata_mmr90$cycle_end),"time_min"]), # orange
				               col=rgb(red = 255, green = 127, blue = 80, alpha = 230, max = 255),
				               lty = "solid", lwd=4, lend="round",
				               x1=newdata_mmr90$cycle_start, x2=newdata_mmr90$cycle_end);
				    ablineclip(lm(d[which(d$time_min > newdata_mmr120$cycle_start &
				                            d$time_min < newdata_mmr120$cycle_end),r]~
				                    d[which(d$time_min > newdata_mmr120$cycle_start &
				                              d$time_min < newdata_mmr120$cycle_end),"time_min"]),
				               col=rgb(red = 0, green = 128, blue = 128, alpha = 230, max = 255), # teal
				               lty = "solid", lwd=4, lend="round",
				               x1=newdata_mmr120$cycle_start, x2=newdata_mmr120$cycle_end);
				  }
  				  
				  
				  # points(d[,r]~d$time_min, col="grey");
				  mtext(bquote(y == .(lm_coef[2])*x + .(lm_coef[1])), adj=1, padj=0, cex=0.8, line=0); # display equation 
				  mtext(bquote(italic(R)^2 == .(format(r2, digits = 3))),adj=1, padj=0, cex=0.8, line=1)
				}
			}
			
			if (Ch_list[j]==2 & cycles>=2){
		
				DateTime_start<-as.character(d$DateTime[1])
				cycle_type<-"MMR"
				Ch<-paste("Ch",j, sep="")
				values_mmr<-as.data.frame(t(c(cycle_type, mmr_start, mmr_end,  cycle_mmr, r2, m, b, 
				                              round(min(d[,r_temp]), 4), 
				                              round(max(d[,r_temp]), 4),
				                              round(mean(d[,r_temp]), 4),
				                              Ch ,DateTime_start )))
				colnames(values_mmr)<-c("cycle_type","cycle_start","cycle_end", "cycle_mmr", "r2" ,"m", "b" , "t_min", "t_max", "t_mean", "Ch", "DateTime_start")	
				newdata_mmr<-rbind(newdata_mmr, values_mmr)
				
				newdata_mmr<-MMRslide(d, Ch, data.MMR, r, r_temp, newdata_mmr, path)
				
				newdata_mmr60<-newdata_mmr[which(newdata_mmr$Ch == Ch & newdata_mmr$cycle_mmr == 60),]
				newdata_mmr90<-newdata_mmr[which(newdata_mmr$Ch == Ch & newdata_mmr$cycle_mmr == 90),]
				newdata_mmr120<-newdata_mmr[which(newdata_mmr$Ch == Ch & newdata_mmr$cycle_mmr == 120),]


				p2 %<a-% {
				  plot(d[,r]~time_min, data=d,  col="grey", ylab=expression(paste(O2~(mg~L^{-1}))),xlab="Time (min)",main="MMR");		
				  abline(lm(d[,r]~d$time_min), col="black",lwd=2);
				  
				  if(nrow(newdata_mmr60)>0){
				    ablineclip(lm(d[which(d$time_min > newdata_mmr60$cycle_start &
  				                          d$time_min < newdata_mmr60$cycle_end),r]~
  				                  d[which(d$time_min > newdata_mmr60$cycle_start &
  				                            d$time_min < newdata_mmr60$cycle_end),"time_min"]), # purple
  				             col=rgb(red = 128, green = 0, blue = 128, alpha = 230, max = 255),
  				             lty = "solid", lwd=4,lend="round",
  				             x1=newdata_mmr60$cycle_start, x2=newdata_mmr60$cycle_end);
  				  ablineclip(lm(d[which(d$time_min > newdata_mmr90$cycle_start &
  				                          d$time_min < newdata_mmr90$cycle_end),r]~
  				                  d[which(d$time_min > newdata_mmr90$cycle_start &
  				                            d$time_min < newdata_mmr90$cycle_end),"time_min"]), # orange
  				             col=rgb(red = 255, green = 127, blue = 80, alpha = 230, max = 255),
  				             lty = "solid", lwd=4, lend="round",
  				             x1=newdata_mmr90$cycle_start, x2=newdata_mmr90$cycle_end);
  				  ablineclip(lm(d[which(d$time_min > newdata_mmr120$cycle_start &
  				                          d$time_min < newdata_mmr120$cycle_end),r]~
  				                  d[which(d$time_min > newdata_mmr120$cycle_start &
  				                            d$time_min < newdata_mmr120$cycle_end),"time_min"]),
  				             col=rgb(red = 0, green = 128, blue = 128, alpha = 230, max = 255), # teal
  				             lty = "solid", lwd=4, lend="round",
  				             x1=newdata_mmr120$cycle_start, x2=newdata_mmr120$cycle_end);
				  }
				  
				  # points(d[,r]~d$time_min, col="grey");
				  mtext(bquote(y == .(lm_coef[2])*x + .(lm_coef[1])), adj=1, padj=0, cex=0.8, line=0); # display equation 
				  mtext(bquote(italic(R)^2 == .(format(r2, digits = 3))),adj=1, padj=0, cex=0.8, line=1);
				}
			}
			
			if (Ch_list[j]==3 & cycles>=3){
				
				DateTime_start<-as.character(d$DateTime[1])
				cycle_type<-"MMR"
				Ch<-paste("Ch",j, sep="")
				values_mmr<-as.data.frame(t(c(cycle_type, mmr_start, mmr_end,  cycle_mmr, r2, m, b, 
				                              round(min(d[,r_temp]), 4), 
				                              round(max(d[,r_temp]), 4),
				                              round(mean(d[,r_temp]), 4),
				                              Ch ,DateTime_start )))
				colnames(values_mmr)<-c("cycle_type","cycle_start","cycle_end",  "cycle_mmr", "r2" ,"m", "b" , "t_min", "t_max", "t_mean", "Ch", "DateTime_start")	
				newdata_mmr<-rbind(newdata_mmr, values_mmr)
				
				newdata_mmr<-MMRslide(d, Ch, data.MMR, r, r_temp, newdata_mmr, path)
				newdata_mmr60<-newdata_mmr[which(newdata_mmr$Ch == Ch & newdata_mmr$cycle_mmr == 60),]
				newdata_mmr90<-newdata_mmr[which(newdata_mmr$Ch == Ch & newdata_mmr$cycle_mmr == 90),]
				newdata_mmr120<-newdata_mmr[which(newdata_mmr$Ch == Ch & newdata_mmr$cycle_mmr == 120),]
				

				p3 %<a-% {
				  plot(d[,r]~time_min, data=d, col="grey", ylab=expression(paste(O2~(mg~L^{-1}))),xlab="Time (min)",main="MMR");		
				  abline(lm(d[,r]~d$time_min), col="black",lwd=2);
				 
				  
				  if(nrow(newdata_mmr60)>0){
				    ablineclip(lm(d[which(d$time_min > newdata_mmr60$cycle_start &
  				                          d$time_min < newdata_mmr60$cycle_end),r]~
  				                  d[which(d$time_min > newdata_mmr60$cycle_start &
  				                            d$time_min < newdata_mmr60$cycle_end),"time_min"]), # purple
  				             col=rgb(red = 128, green = 0, blue = 128, alpha = 230, max = 255),
  				             lty = "solid", lwd=4,lend="round",
  				             x1=newdata_mmr60$cycle_start, x2=newdata_mmr60$cycle_end);
  				  ablineclip(lm(d[which(d$time_min > newdata_mmr90$cycle_start &
  				                          d$time_min < newdata_mmr90$cycle_end),r]~
  				                  d[which(d$time_min > newdata_mmr90$cycle_start &
  				                            d$time_min < newdata_mmr90$cycle_end),"time_min"]), # orange
  				             col=rgb(red = 255, green = 127, blue = 80, alpha = 230, max = 255),
  				             lty = "solid", lwd=4, lend="round",
  				             x1=newdata_mmr90$cycle_start, x2=newdata_mmr90$cycle_end);
  				  ablineclip(lm(d[which(d$time_min > newdata_mmr120$cycle_start &
  				                          d$time_min < newdata_mmr120$cycle_end),r]~
  				                  d[which(d$time_min > newdata_mmr120$cycle_start &
  				                            d$time_min < newdata_mmr120$cycle_end),"time_min"]),
  				             col=rgb(red = 0, green = 128, blue = 128, alpha = 230, max = 255), # teal
  				             lty = "solid", lwd=4, lend="round",
  				             x1=newdata_mmr120$cycle_start, x2=newdata_mmr120$cycle_end);
				  }
				  # points(d[,r]~d$time_min, col="grey");
				  mtext(bquote(y == .(lm_coef[2])*x + .(lm_coef[1])), adj=1, padj=0, cex=0.8, line=0); # display equation 
				  mtext(bquote(italic(R)^2 == .(format(r2, digits = 3))),adj=1, padj=0, cex=0.8, line=1)
				}
			}
			
			if (Ch_list[j]==4 & cycles>=4){
			
				DateTime_start<-as.character(d$DateTime[1])
				cycle_type<-"MMR"
				Ch<-paste("Ch",j, sep="")
				values_mmr<-as.data.frame(t(c(cycle_type, mmr_start, mmr_end, cycle_mmr, r2, m, b, 
				                              round(min(d[,r_temp]), 4), 
				                              round(max(d[,r_temp]), 4),
				                              round(mean(d[,r_temp]), 4),
				                              Ch, DateTime_start)))
				colnames(values_mmr)<-c("cycle_type","cycle_start","cycle_end",  "cycle_mmr", "r2" ,"m", "b" , "t_min", "t_max", "t_mean", "Ch", "DateTime_start")	
				newdata_mmr<-rbind(newdata_mmr, values_mmr)
				
				newdata_mmr<-MMRslide(d, Ch, data.MMR,r, r_temp, newdata_mmr, path)
				newdata_mmr60<-newdata_mmr[which(newdata_mmr$Ch == Ch & newdata_mmr$cycle_mmr == 60),]
				newdata_mmr90<-newdata_mmr[which(newdata_mmr$Ch == Ch & newdata_mmr$cycle_mmr == 90),]
				newdata_mmr120<-newdata_mmr[which(newdata_mmr$Ch == Ch & newdata_mmr$cycle_mmr == 120),]
				

				p4 %<a-% {
				  plot(d[,r]~time_min, data=d, col="grey", ylab=expression(paste(O2~(mg~L^{-1}))),xlab="Time (min)",main="MMR");		
				  abline(lm(d[,r]~d$time_min), col="black",lwd=2);
				  if(nrow(newdata_mmr60)>0){
				    ablineclip(lm(d[which(d$time_min > newdata_mmr60$cycle_start &
  				                          d$time_min < newdata_mmr60$cycle_end),r]~
  				                  d[which(d$time_min > newdata_mmr60$cycle_start &
  				                            d$time_min < newdata_mmr60$cycle_end),"time_min"]), # purple
  				             col=rgb(red = 128, green = 0, blue = 128, alpha = 230, max = 255),
  				             lty = "solid" , lwd=4,lend="round",
  				             x1=newdata_mmr60$cycle_start, x2=newdata_mmr60$cycle_end);
  				  ablineclip(lm(d[which(d$time_min > newdata_mmr90$cycle_start &
  				                          d$time_min < newdata_mmr90$cycle_end),r]~
  				                  d[which(d$time_min > newdata_mmr90$cycle_start &
  				                            d$time_min < newdata_mmr90$cycle_end),"time_min"]), # orange
  				             col=rgb(red = 255, green = 127, blue = 80, alpha = 230, max = 255),
  				             lty = "solid", lwd=4, lend="round",
  				             x1=newdata_mmr90$cycle_start, x2=newdata_mmr90$cycle_end);
  				  ablineclip(lm(d[which(d$time_min > newdata_mmr120$cycle_start &
  				                          d$time_min < newdata_mmr120$cycle_end),r]~
  				                  d[which(d$time_min > newdata_mmr120$cycle_start &
  				                            d$time_min < newdata_mmr120$cycle_end),"time_min"]),
  				             col=rgb(red = 0, green = 128, blue = 128, alpha = 230, max = 255), # teal
  				             lty = "solid", lwd=4, lend="round",
  				             x1=newdata_mmr120$cycle_start, x2=newdata_mmr120$cycle_end);
				  }
				  # points(d[,r]~d$time_min, col="grey");
				  mtext(bquote(y == .(lm_coef[2])*x + .(lm_coef[1])), adj=1, padj=0, cex=0.8, line=0); # display equation 
				  mtext(bquote(italic(R)^2 == .(format(r2, digits = 3))),adj=1, padj=0, cex=0.8, line=1)
				}
				
			}
			
					
			# 
			## non MMR plots
			if ((Ch_list[j]==1 & cycles==2) | (Ch_list[j]==1 & cycles==3) | (Ch_list[j]==1 & cycles>=4)){
			# if ((Ch_list[j]==1 & cycles>=2)){
 
				p2 %<a-% {
					plot(d2[,r]~time_min, data=d2, col="grey", ylab=expression(paste(O2~(mg~L^{-1}))),xlab="Time (min)",main="cycle2");		
					abline(lm(d2[,r]~d2$time_min), col="red",lwd=2);
					mtext(bquote(y == .(lm_coef.2[2])*x + .(lm_coef[1])), adj=1, padj=0, cex=0.8, line=0); # display equation 
					mtext(bquote(italic(R)^2 == .(format(r2.2, digits = 3))),adj=1, padj=0, cex=0.8, line=1);
				  if(!is.na(clean_cycle2)){
				    # print("here here")
				    
				    if(start_c2==0 & end_c2 ==0){
				      legend("center","center", c("EXCLUDING \n from dataset"), col=c("red"), pch=" ", cex=2)
				    }else{
				      legend("topright", c("Time-adjusted"), col=c("red"), pch=" ", cex=1)
				    }
				  }
				}
				
				DateTime_start<-as.character(d2$DateTime[1])
				cycle_type<-"cycle2"
				Ch<-paste("Ch",j, sep="")
				
				if(is.na(clean_cycle2)){ 
  			  values_mmr<-as.data.frame(t(c(cycle_type, cycle_start[2], cycle_end[2],  cycle_mmr, r2.2, m.2, b.2,
  			                                round(min(d2[,r_temp]),4), 
  			                                round(max(d2[,r_temp]), 4),
  			                                round(mean(d2[,r_temp]),4),
  			                                Ch, DateTime_start)))
  			  colnames(values_mmr)<-c("cycle_type","cycle_start","cycle_end",  "cycle_mmr", "r2" ,"m", "b" , "t_min", "t_max", "t_mean", "Ch", "DateTime_start")	
  			  newdata_mmr<-rbind(newdata_mmr, values_mmr)
				}else{
				  if(start_c2==0 & end_c2 ==0){
				    message(paste("Take out cycle 2 from MMR trends - Channel: ", j))
				  }else{
            values_mmr<-as.data.frame(t(c(cycle_type, start_c2, end_c2,  cycle_mmr, r2.2, m.2, b.2,
                                          round(min(d2[,r_temp]),4), 
                                          round(max(d2[,r_temp]), 4),
                                          round(mean(d2[,r_temp]),4),
                                          Ch, DateTime_start)))
            colnames(values_mmr)<-c("cycle_type","cycle_start","cycle_end",  "cycle_mmr", "r2" ,"m", "b" , "t_min", "t_max", "t_mean", "Ch", "DateTime_start")	
            newdata_mmr<-rbind(newdata_mmr, values_mmr)
				  }
				}
			}
				
			# if ((Ch_list[j]==1 | Ch_list[j]==2 | Ch_list[j]==4) & cycles>=3){
			if ((Ch_list[j]==1 & cycles==3) | (Ch_list[j]== 1 & cycles==4) | (Ch_list[j]==2 & cycles==3) | (Ch_list[j]== 2 & cycles==4)){
				p3 %<a-% {
					plot(d3[,r]~time_min, data=d3, col="grey", ylab=expression(paste(O2~(mg~L^{-1}))),xlab="Time (min)",main="cycle3");		
					abline(lm(d3[,r]~d3$time_min), col="red",lwd=2);
					mtext(bquote(y == .(lm_coef.3[2])*x + .(lm_coef.3[1])), adj=1, padj=0, cex=0.8, line=0); # display equation 
					mtext(bquote(italic(R)^2 == .(format(r2.3, digits = 3))),adj=1, padj=0, cex=0.8, line=1);
				  if(!is.na(clean_cycle3)){
				    
				    if(start_c3==0 & end_c3 ==0){
				      legend("center","center", c("EXCLUDING \n from dataset"), col=c("red"), pch=" ", cex=2)
				    }else{
				      legend("topright", c("Time-adjusted"), col=c("red"), pch=" ", cex=1)
            }
				  }
				}
				# print("here -here")
				# print(c(start_c3, end_c3))
				DateTime_start<-as.character(d3$DateTime[1])
				cycle_type<-"cycle3"
				Ch<-paste("Ch",j, sep="")
				if(is.na(clean_cycle3)){ 
          values_mmr<-as.data.frame(t(c(cycle_type, cycle_start[3], cycle_end[3],  cycle_mmr, r2.3, m.3, b.3,
                                        round(min(d3[,r_temp]),4), 
                                        round(max(d3[,r_temp]), 4),
                                        round(mean(d3[,r_temp]),4),
                                        Ch, DateTime_start)))
          colnames(values_mmr)<-c("cycle_type","cycle_start","cycle_end",  "cycle_mmr", "r2" ,"m", "b" , "t_min", "t_max", "t_mean", "Ch", "DateTime_start")	
          newdata_mmr<-rbind(newdata_mmr, values_mmr)
				}else{
				  if(start_c3==0 & end_c3 ==0){
				    message(paste("Take out cycle 3 from MMR trends - Channel: ", j))
				  }else{
				    values_mmr<-as.data.frame(t(c(cycle_type, start_c3, end_c3, cycle_mmr, r2.3, m.3, b.3, 
				                                  round(min(d3[,r_temp]),4), 
				                                  round(max(d3[,r_temp]), 4),
				                                  round(mean(d3[,r_temp]),4),
				                                  Ch, DateTime_start)))
				    colnames(values_mmr)<-c("cycle_type","cycle_start","cycle_end",  "cycle_mmr", "r2" ,"m", "b" , "t_min", "t_max", "t_mean", "Ch", "DateTime_start")	
				    newdata_mmr<-rbind(newdata_mmr, values_mmr)
				  }
				}
			}
			
			# if ((Ch_list[j]==1 | Ch_list[j]==2 | Ch_list[j]==3) & cycles>=4){
			if ((Ch_list[j]==1 & cycles==4) | (Ch_list[j]== 2 & cycles==4) | (Ch_list[j]==3 & cycles==4)){

				p4 %<a-% {
					plot(d4[,r]~time_min, data=d4, col="grey", ylab=expression(paste(O2~(mg~L^{-1}))),xlab="Time (min)",main="cycle4");		
					abline(lm(d4[,r]~d4$time_min), col="red",lwd=2);
					mtext(bquote(y == .(lm_coef.4[2])*x + .(lm_coef.4[1])), adj=1, padj=0, cex=0.8, line=0); # display equation 
					mtext(bquote(italic(R)^2 == .(format(r2.4, digits = 3))),adj=1, padj=0, cex=0.8, line=1);
				  if(!is.na(clean_cycle4)){
				    if(start_c4==0 & end_c4 ==0){
				      legend("center","center", c("EXCLUDING \n from dataset"), col=c("red"), pch=" ", cex=2)
				    }else{
				      legend("topright", c("Time-adjusted"), col=c("red"), pch=" ", cex=1)
				    }		  
				  }
				}
				
				DateTime_start<-as.character(d4$DateTime[1])
				cycle_type<-"cycle4"
				Ch<-paste("Ch",j, sep="")
				
				if(is.na(clean_cycle4)){
				  values_mmr<-as.data.frame(t(c(cycle_type, cycle_start[4], cycle_end[4],  cycle_mmr, r2.4, m.4, b.4, 
				                                round(min(d4[,r_temp]),4), 
				                                round(max(d4[,r_temp]), 4),
				                                round(mean(d4[,r_temp]),4),
				                                Ch, DateTime_start)))
				  colnames(values_mmr)<-c("cycle_type","cycle_start","cycle_end",  "cycle_mmr", "r2" ,"m", "b" , "t_min", "t_max", "t_mean", "Ch", "DateTime_start")	
				  newdata_mmr<-rbind(newdata_mmr, values_mmr)
				}else{
				  if(start_c4==0 & end_c4 ==0){
				    message(paste("Take out cycle 4 from MMR trends - Channel: ", j))
				  }else{
				    values_mmr<-as.data.frame(t(c(cycle_type, start_c4, end_c4,  cycle_mmr, r2.4, m.4, b.4,
				                                  round(min(d4[,r_temp]),4), 
				                                  round(max(d4[,r_temp]), 4),
				                                  round(mean(d4[,r_temp]),4),
				                                  Ch, DateTime_start)))
				    colnames(values_mmr)<-c("cycle_type","cycle_start","cycle_end",  "cycle_mmr", "r2" ,"m", "b" , "t_min", "t_max", "t_mean", "Ch", "DateTime_start")	
				    newdata_mmr<-rbind(newdata_mmr, values_mmr)
				  }
				}
			}
			
		
			# null plot 	
			p_null %<a-% {
				plot(1, type="n", axes=T, xlab="", ylab="");
				text(1, 1, "MO2 NOT MEASURED",cex = .8)
			}
		
			png(plotname, width=20, height=5, units="in",  res=200)
			par(mfrow=c(1,5))
				if (cycles==1){
					p
					p1
					p_null
					p_null
					p_null	
				}
				
				if (cycles==2){
					if (Ch_list[j]==1){
						p
						p1
						p2
						p_null
						p_null
					}
					if (Ch_list[j]==2){
						p
						p_null
						p2
						p_null
						p_null
					}	
				}
				
				if (cycles==3){
					if (Ch_list[j]==1){
						p
						p1 # MMR
						p2
						p3
						p_null
					}
					if (Ch_list[j]==2){
						p
						p_null
						p2 # MMR
						p3
						p_null
					}
					if (Ch_list[j]==3){
						p
						p_null
						p_null
						p3 # MMR
						p_null
					}	
				}
				
				if (cycles>=4){
					if (Ch_list[j]==1){
						p
						p1 # MMR
						p2
						p3
						p4
					}
					if (Ch_list[j]==2){
						p
						p_null
						p2 # MMR
						p3
						p4
					}
					if (Ch_list[j]==3){
						p
						p_null
						p_null
						p3 # MMR
						p4
					}
					if (Ch_list[j]==4){
						p
						p_null
						p_null
						p_null
						p4 # MMR
					}	
				}
			
			dev.off()
			# 
			#  save file now -- write code 	
		}else{
				message(paste("NO Channel - ",j, sep=""))
		}
		
	#~ 	assign("newdata_mmr", newdata_mmr, envir=.GlobalEnv)	
		
		# Channel_mmr<-newdata_mmr
		return(newdata_mmr)

	}# end of Channel_mmr function	
	





MMR<-function(data.MMR,cycles, 
              cycle_start, 
              cycle_end,
              mmr_Ch1=0,
              mmr_Ch2=0,
              mmr_Ch3=0,
              mmr_Ch4=0,
              clean_Ch1=c(0,0), clean_Ch2=c(0,0), clean_Ch3=c(0,0), clean_Ch4=c(0,0), 
              N_Ch = 4, path = ".", date_format = "m/d/y", 
              inv.data = NA){
	
 
	newdata_mmr<-matrix(ncol=12,nrow=0)
	colnames(newdata_mmr)<-c("cycle_type", "cycle_start","cycle_end",  "cycle_mmr", "r2" ,"m", "b" , "t_min", "t_max", "t_mean", "Ch", "DateTime_start")	
	

	if (path == "."){
    filename<-paste( gsub('.{4}$', '', data.MMR), "_analyzed.csv", sep='')
    filename2<-paste( gsub('.{4}$', '', data.MMR), "_analyzed.csv", sep='') # save in the EPOC_AS etc folder for final MMR and full EPOC analysis

	}else{
	  filename<-paste("../csv_analyzed/", gsub('.{4}$', '', data.MMR), "_analyzed.csv", sep='')
    filename2<-paste("../../MMR_SMR_AS_EPOC/csv_input_files/", gsub('.{4}$', '', data.MMR), "_analyzed.csv", sep='') # save in the EPOC_AS etc folder for final MMR and full EPOC analysis

	}
	
  
	dataMMR<-read.csv(data.MMR)
	if(as.character(dataMMR$Ch1_O2[1])=="--- " || as.character(dataMMR$Ch1_O2[1])=="---"){
		dataMMR$Ch1_O2<-0
	}
	if(as.character(dataMMR$Ch2_O2[1])=="--- " || as.character(dataMMR$Ch2_O2[1])=="---"){
		dataMMR$Ch2_O2<-0
	}
	if(as.character(dataMMR$Ch3_O2[1])=="--- " || as.character(dataMMR$Ch3_O2[1])=="---"){
		dataMMR$Ch3_O2<-0
	}
	if(as.character(dataMMR$Ch4_O2[1])=="--- " || as.character(dataMMR$Ch4_O2[1])=="---"){
		dataMMR$Ch4_O2<-0
	}
	
	n<-length(which(!dataMMR[1,c(4, 6:8)]==0))
	
	
	dataMMR$date<-as.character(dataMMR$date)
	dataMMR$time<-as.character(dataMMR$time)
	
	if (date_format== "m/d/y"){
  	DateTime<- chron(dates=dataMMR$date,times=dataMMR$time,format=c('m/d/y','h:m:s')) # Eliason Lab firesting date format
#~ 	DateTime<- chron(dates=dataMMR$date,times=dataMMR$time,format=c('y-m-d','h:m:s')) # Healy firesting date format 
	}
	if (date_format== "d/m/y"){
    # 8 channel firesting with date (day first) European style
    DateTime<- chron(dates=dataMMR$date,times=dataMMR$time,format=c('d/m/y','h:m:s')) # Eliason Lab firesting date format
	}
	if (date_format== "y-m-d"){
    # 8 channel firesting with date (day first) European style
    DateTime<- chron(dates=dataMMR$date,times=dataMMR$time,format=c('Y-m-d','h:m:s')) # Eliason Lab firesting date format
  }
 	
	dataMMR$DateTime<-DateTime
			
	dataMMR$hr<-round(dataMMR$time_sec/3600,2) 
	dataMMR$hr2<-round(dataMMR$time_sec/3600,0) 
	dataMMR$time_min<-round(dataMMR$time_sec/60,2) 
	  
	dataMMR$Ch1_temp<-as.numeric(as.character(dataMMR$Ch1_temp))

	if (N_Ch==8){
  	dataMMR$Ch2_temp<-as.numeric(as.character(dataMMR$Ch2_temp))
  	dataMMR$Ch3_temp<-as.numeric(as.character(dataMMR$Ch3_temp))
  	dataMMR$Ch4_temp<-as.numeric(as.character(dataMMR$Ch4_temp))
  		# 
    temp_mean2<-mean(dataMMR$Ch2_temp, na.rm=TRUE)
    temp_max2<-max(dataMMR$Ch2_temp, na.rm=TRUE)
    temp_min2<-min(dataMMR$Ch2_temp, na.rm=TRUE)

    temp_mean3<-mean(dataMMR$Ch3_temp, na.rm=TRUE)
    temp_max3<-max(dataMMR$Ch3_temp, na.rm=TRUE)
    temp_min3<-min(dataMMR$Ch3_temp, na.rm=TRUE)

    temp_mean4<-mean(dataMMR$Ch4_temp, na.rm=TRUE)
    temp_max4<-max(dataMMR$Ch4_temp, na.rm=TRUE)
    temp_min4<-min(dataMMR$Ch4_temp, na.rm=TRUE)
  }

	dataMMR$Ch1_O2<-as.numeric(as.character(dataMMR$Ch1_O2))
	dataMMR$Ch2_O2<-as.numeric(as.character(dataMMR$Ch2_O2))
	dataMMR$Ch3_O2<-as.numeric(as.character(dataMMR$Ch3_O2))
	dataMMR$Ch4_O2<-as.numeric(as.character(dataMMR$Ch4_O2))
	
	#temperature
	temp_mean1<-mean(dataMMR$Ch1_temp, na.rm=TRUE)
	temp_max1<-max(dataMMR$Ch1_temp, na.rm=TRUE)	
	temp_min1<-min(dataMMR$Ch1_temp, na.rm=TRUE)	

	Ch_list<-c(mmr_Ch1, mmr_Ch2, mmr_Ch3, mmr_Ch4) ## specifies in which cycle does the channel has MMR at 
	rows<-c(4,6,7,8) # the order Ch1, Ch2, Ch3, Ch4
	
	if(N_Ch==8){
  	rows_temp<-c(5,9,10,11) # the order Ch1, Ch2, Ch3, Ch4
	  }else {
	 	rows_temp<-c(5,5,5,5) # the order Ch1, Ch2, Ch3, Ch4
	}
	
	if(clean_Ch1[1] == 0 & mmr_Ch1!=0){
    clean_Ch1[1] <- cycle_start[mmr_Ch1]
	}
	if(clean_Ch2[1] == 0 & mmr_Ch2!=0){
    clean_Ch2[1] <- cycle_start[mmr_Ch2]
	}
	if(clean_Ch3[1] == 0 & mmr_Ch3!=0){
    clean_Ch3[1] <- cycle_start[mmr_Ch3]
	}
	if(clean_Ch4[1] == 0 & mmr_Ch4!=0){
    clean_Ch4[1] <- cycle_start[mmr_Ch4]
	}
	
	if(clean_Ch1[2] == 0 & mmr_Ch1!=0){
    clean_Ch1[2] <- cycle_end[mmr_Ch1]
	}
	if(clean_Ch2[2] == 0 & mmr_Ch2!=0){
    clean_Ch2[2] <- cycle_end[mmr_Ch2]
	}
	if(clean_Ch3[2] == 0 & mmr_Ch3!=0){
    clean_Ch3[2] <- cycle_end[mmr_Ch3]
	}
	if(clean_Ch4[2] == 0 & mmr_Ch4!=0){
    clean_Ch4[2] <- cycle_end[mmr_Ch4]
	}
	
  clean_start_mmr<-c(clean_Ch1[1], clean_Ch2[1], clean_Ch3[1], clean_Ch4[1]) # cleaning start 
  # cycle_start
  clean_end_mmr<-c(clean_Ch1[2], clean_Ch2[2], clean_Ch3[2], clean_Ch4[2]) # cleaning end
  # cycle_end
  
  # if the last cycle end is provided to be longer (min) than the actual file is, then rewrite the end time to be equal to the time the respo is run. 
	if (cycle_end[length(cycle_end)]>dataMMR$time_min[nrow(dataMMR)]){
		cycle_end[length(cycle_end)]<-dataMMR$time_min[nrow(dataMMR)]
	}

 
    
      Box_n_data<-(which(!is.na(str_locate(data.MMR, c("box", "BOX", "Box"))))[2])

      if(!is.na(inv.data)){
        data2<-read.csv(inv.data)
        
        # print(data2)
      }
    
      	for (j in 1:4){
      	  
      	  
      	  if(!is.na(inv.data)){
      
        	 
        	   if(j==1){inv.data.clean<-data2[which(grepl(substr(data.MMR, start=1, stop=5), as.character(data2$date)) & as.numeric(data2$channel)==1 &
        			                        grepl(substr(data.MMR, start = (str_locate(data.MMR, c("box", "BOX", "Box"))[Box_n_data])+1,
        			                                     stop = (str_locate(data.MMR, c("box", "BOX", "Box"))[Box_n_data])+1), as.character(data2$box))),]
        	   }
        	   if(j==2){inv.data.clean<-data2[which(grepl(substr(data.MMR, start=1, stop=5), as.character(data2$date)) & as.numeric(data2$channel)==2 &
        			                        grepl(substr(data.MMR, start = (str_locate(data.MMR, c("box", "BOX", "Box"))[Box_n_data])+1,
        			                                     stop = (str_locate(data.MMR, c("box", "BOX", "Box"))[Box_n_data])+1), as.character(data2$box))),]
        	   }
        	   if(j==3){inv.data.clean<-data2[which(grepl(substr(data.MMR, start=1, stop=5), as.character(data2$date)) & as.numeric(data2$channel)==3 &
        			                        grepl(substr(data.MMR, start = (str_locate(data.MMR, c("box", "BOX", "Box"))[Box_n_data])+1,
        			                                     stop = (str_locate(data.MMR, c("box", "BOX", "Box"))[Box_n_data])+1), as.character(data2$box))),]
        	   }
        	   if(j==4){inv.data.clean<-data2[which(grepl(substr(data.MMR, start=1, stop=5), as.character(data2$date)) & as.numeric(data2$channel)==4 &
        			                        grepl(substr(data.MMR, start = (str_locate(data.MMR, c("box", "BOX", "Box"))[Box_n_data])+1,
        			                                     stop = (str_locate(data.MMR, c("box", "BOX", "Box"))[Box_n_data])+1), as.character(data2$box))),]
        	   }
      	  
      	  }else{
      	    
      	    inv.data.clean<-as.data.frame(matrix(nrow=0, ncol=0))
      	  }
      	  
      		newdata_mmr<-Channel_mmr(data.MMR, dataMMR, cycle_start, cycle_end, clean_start_mmr, clean_end_mmr, Ch_list, cycles, j, rows, rows_temp, newdata_mmr, path, inv.data.clean)
      	}
      

	newdata_mmr$m<-abs(as.numeric(as.character(newdata_mmr$m)))
	
	write.csv(file=filename, newdata_mmr, row.names=FALSE)
	write.csv(file=filename2, newdata_mmr, row.names=FALSE)

#~ 	assign("newdata_mmr", newdata_mmr, envir=.GlobalEnv)

}





####### FULL MR analysis functions #############

#~ data.MMR -- the anaylzed data csv file / export file from the MMR function
#~ data.SMR -- the anaylzed data csv file / export file from the MMR_SMR function
#~ AnimalID -- al list of four elements / animal ID / if animal is not in the reso (empty channel then add "NA"- can construct the functions to loop through some summary file 
#~ BW.animal --  al list of four elements / animal body weight in kg, if animal is not in the reso (empty channel then add anizero (0)/ - can construct the functions to loop through some summary file 
#~ resp.V -- the volume of respirometers in L 


# mmr_background = "SAME_slope" (default is to take the same value as the entire smr), must specify mmr_background = back_prior to have a prior slope only, or add a specific value to be substracted for mmr background

# min_length_mmr = indicate the time length that the mmr is calculated from (in sec) / in MMR fucntion we do 60, 90, 120, 180, and a full file/ these are the options also here: 60, 90, 120, 180, or "full"


# start of EPOC.spar plot function
EPOC.spar <- function(spar, d,  EPOCdata, mmr.val, epoc_threshold , recovMMR_threshold , newdata.smr, MLND, end_EPOC, scaling_exponent_mmr, scaling_exponent_smr, common_mass){

	#1 smooth and then predict the curve f
  fit<-smooth.spline(d$time_mo2,d$mo2, spar=spar)
	f = function(x) {predict(fit, x)$y}
	
	end<-round(d$time_mo2[nrow(d)],1)
	newx<-seq(0,end, by=1)
	newy<-predict(fit, newx, deriv=0)
	lapply(newy, as.numeric)
	newVal<-data.frame(Reduce(cbind,newy)) # creating dummy dataset with predicted values 
	
	smr.row<-newdata.smr[which(as.character(newdata.smr$Ch)==as.character(d$Ch[1])),]
	ID<-smr.row["ID"]
	
	# smr_mean10minVal SMR_low10quant SMR_low15quant SMR_low20quant  smr_mlnd
	b1.1<-as.numeric(round(smr.row["smr_mean10minVal"],2))
	b1.2<-as.numeric(round(smr.row["SMR_low10quant"],2)) 
	b1.3<-as.numeric(round(smr.row["SMR_low15quant"],2))
	b1.4<-as.numeric(round(smr.row["SMR_low20quant"],2))
	
	if(MLND == TRUE){ # need this argument because MLND gives another SMR thresholdm that with MLND = FALSE is not available. 
  	b1.5<-as.numeric(round(smr.row["smr_mlnd"],2))
	}else{
	  b1.5<-0
	}
	
	smr_type_list<-c("smr_mean10minVal", "SMR_low10quant", "SMR_low15quant", "SMR_low20quant", "smr_mlnd")
	b_list<-c(b1.1,b1.2,b1.3,b1.4,b1.5)
	
	for (i in 1:5){
		
		if(i == 1){
			EPOCdata.temp<-matrix(ncol=26, nrow=0)
			colnames(EPOCdata.temp) <- c("ID", "smr_type", "smr","spar", "EPOC_full", "end_EPOC_min", "SMR_intergral_full", "SMR_threshold", "EPOC_1hr", "MO2_1hr", "EPOC_2hr", "MO2_2hr", "EPOC_3hr", "MO2_3hr", "EPOC_4hr", "MO2_4hr",  "EPOC_5hr", "MO2_5hr", "end_EPOC.mmr", "EPOC_mmr", "MO2_mmr", "MMR", "MMR_percent",  "scaling_exponent_mmr",  "scaling_exponent_smr", "common_mass")
		}
		
	  
	  # adjustable epoc threshold (default is just SMR values as calculated using different methods)
		if (epoc_threshold == 1){
		  	b <- b_list[i]
		  	
		}else{
		  
		    b <- b_list[i] * epoc_threshold
		}
		
	  b.mmr <- recovMMR_threshold * mmr.val # recv thresholds gives a proportion (or percent) and the mmr is the MMR is the MO2 value

		smr_type <- smr_type_list[i]			
		#2-1- establish SMR threshold m1 and b1 for SMR (b1= y intercept, m1=slope)
		m0 <- 0 # slope
		
		# smr function
		f.smr <- function(x)(m0*x)+b 
		
		# % mmr recovery function; default 50% 
		f.mmr <- function(x)(m0*x)+b.mmr # smr function
		
		# if manually provided EPOC threshold does not exist then find when EPOC ends
		if (is.na(end_EPOC)){
  		# Calculate the area under the SMR for all types of SMR calculated in the MMR_SMR_analyze function	
  		#2-2 the EPOC cutoff in the smoothed function / this is used also for the SMR block
  		end_EPOC <- newVal$init[(which(round(newVal$V2, 3)<=b))[1]]
  		
  		if(is.na(end_EPOC)){
  			end_EPOC<-newVal$init[nrow(newVal)]
  		}
		}
		
		# % mmr end EPOC 
		end_EPOC.mmr <- newVal$init[(which(round(newVal$V2, 3)<=b.mmr))[1]]
		if(is.na(end_EPOC.mmr)){
			end_EPOC.mmr<-newVal$init[nrow(newVal)]
			# message(paste("The animal does not reach % MMR recovery threshold: ", recovMMR_threshold, sep=""))
		}
		
		#2-3 the Breakpoit cuttoff
		#bp1<-round(d$bp[1],0) # smr - or the y intercept ### This is the one Emily settled on for data analyses
		
		f.int_mmr = function(x) {integrate(f, lower=0, upper=end_EPOC.mmr)$value}
		f.int.vec_mmr = Vectorize(f.int_mmr, vectorize.args='x')
		# f.int.vec_mmr = Vectorize(f.int_mmr, vectorize.args='x')
		full_mmr<-f.int.vec_mmr(end_EPOC.mmr)
		# SMR block
		recovMMR<-integrate(f.mmr, lower=0, upper=end_EPOC.mmr)$value
		EPOC_mmr<-round(full_mmr-recovMMR,3)
		MO2_mmr<-round(newVal$V2[which(newVal$init==end_EPOC.mmr)],3)
		
		# print(c(MO2_mmr, EPOC_mmr, recovMMR, end_EPOC.mmr,end_EPOC, f.smr))
		
		#3 integrate recovery curve with to the provided end EPOC time
		f.int = function(x) {integrate(f, lower=0, upper=end_EPOC)$value}
		f.int.vec = Vectorize(f.int, vectorize.args='x')
		f.int.vec = Vectorize(f.int, vectorize.args='x')
		full<-f.int.vec(end_EPOC)
		# SMR block
		SMR<-integrate(f.smr, lower=0, upper=end_EPOC)$value
		EPOC_full<-round(full-SMR,3)
		MO2_full<-round(newVal$V2[which(newVal$init==end_EPOC)],3)

		#3.1 integrate revovery curve for the first hr
		f.int_1hr = function(x) {integrate(f, lower=0, upper=60)$value}
		f.int.vec_1hr = Vectorize(f.int_1hr, vectorize.args='x')
		full_1hr<-f.int.vec_1hr(60)
		# SMR block
		SMR_1hr<-integrate(f.smr, lower=0, upper=60)$value
		EPOC_1hr<-round(full_1hr-SMR_1hr,3)
		MO2_1hr<-round(newVal$V2[60],3)

		#3.2 integrate revovery curve for the first 2hrs
		f.int_2hr = function(x) {integrate(f, lower=0, upper=120)$value}
		f.int.vec_2hr = Vectorize(f.int_2hr, vectorize.args='x')
		full_2hr<-f.int.vec_2hr(120)
		# SMR block
		SMR_2hr<-integrate(f.smr, lower=0, upper=120)$value
		EPOC_2hr<-round(full_2hr-SMR_2hr,3)
		MO2_2hr<-round(newVal$V2[120],3)

		#3.3 integrate revovery curve for the first 3hrs
		f.int_3hr = function(x) {integrate(f, lower=0, upper=180)$value}
		f.int.vec_3hr = Vectorize(f.int_3hr, vectorize.args='x')
		full_3hr<-f.int.vec_3hr(180)
		# SMR block
		SMR_3hr<-integrate(f.smr, lower=0, upper=180)$value
		EPOC_3hr<-round(full_3hr-SMR_3hr,3)
		MO2_3hr<-round(newVal$V2[180],3)

		#3.4 integrate revovery curve for the first 4hrs
		f.int_4hr = function(x) {integrate(f, lower=0, upper=240)$value}
		f.int.vec_4hr = Vectorize(f.int_4hr, vectorize.args='x')
		full_4hr<-f.int.vec_4hr(240)
		# SMR block
		SMR_4hr<-integrate(f.smr, lower=0, upper=240)$value
		EPOC_4hr<-round(full_4hr-SMR_4hr,3)
		MO2_4hr<-round(newVal$V2[240],3)

		#3.5 integrate revovery curve for the first 5hrs
		f.int_5hr = function(x) {integrate(f, lower=0, upper=300)$value}
		f.int.vec_5hr = Vectorize(f.int_5hr, vectorize.args='x')
		full_5hr<-f.int.vec_5hr(300)
		# SMR block
		SMR_5hr<-integrate(f.smr, lower=0, upper=300)$value
		EPOC_5hr<-round(full_5hr-SMR_5hr,3)
		MO2_5hr<-round(newVal$V2[300],3)
			
		# 3.bp1 integrate revovery curve for the first breakpoint
		#f.int_bp1 = function(x) {integrate(f, lower=0, upper=bp1)$value}
		#f.int.vec_bp1 = Vectorize(f.int_bp1, vectorize.args='x')
		#full_bp1<-f.int.vec_bp1(bp1_val)
		# SMR block
		#SMR_bp1<-integrate(f.smr, lower=0, upper=bp1)$value
		#EPOC_bp1<-round(full_bp1-SMR_bp1,2)
		#MO2_bp1<-round(newVal$V2[bp1],2)
  	if(end_EPOC.mmr == 0){
  		  MO2_mmr=0
  	}
		
		values<-as.data.frame(t(c(as.character(d$ID[1]),smr_type, b,  spar, EPOC_full, end_EPOC, SMR, epoc_threshold, EPOC_1hr, MO2_1hr, EPOC_2hr, MO2_2hr, EPOC_3hr, MO2_3hr, EPOC_4hr, MO2_4hr, EPOC_5hr, MO2_5hr, end_EPOC.mmr, EPOC_mmr, MO2_mmr, mmr.val, (MO2_mmr/mmr.val*100), scaling_exponent_mmr, scaling_exponent_smr, common_mass)))

		colnames(values)<-c("ID", "smr_type", "smr","spar", "EPOC_full", "end_EPOC_min", "SMR_intergral_full", "SMR_threshold",
		"EPOC_1hr", "MO2_1hr", "EPOC_2hr", "MO2_2hr", "EPOC_3hr", "MO2_3hr", "EPOC_4hr", "MO2_4hr",  "EPOC_5hr", "MO2_5hr", "end_EPOC.mmr", "EPOC_mmr", "MO2_mmr", "MMR", "MMR_percent", "scaling_exponent_mmr",  "scaling_exponent_smr", "common_mass")
  
		
		EPOCdata.temp<-rbind(EPOCdata.temp,values)

		col_smr<-c("black",  "darkmagenta", "darkred", "darkorange","darkgreen")
		scale<-c(0.02,0.07,0.12,0.17,0.22)

		if(MLND == TRUE){ # need this argument because MLND gives another SMR thresholdm that with MLND = FALSE is not available. 
			if(i==1){
				plot(x=range(newVal$init), xlim=c(0,max(d$time_mo2, na.rm=TRUE)+50), ylim=c(0,max(d$mo2, na.rm=TRUE)),type='n', ylab="MO2", xlab="time (min)", main=paste("Smoothness=",spar, sep=""))
				points(d$time_mo2,d$mo2)
				lines(newy[[1]], newy[[2]], col="blue")
				abline(v=60, col="grey", lty=2)
				abline(v=120, col="grey", lty=2)
				abline(v=180, col="grey", lty=2)
				abline(v=240, col="grey", lty=2)
				abline(v=300, col="grey", lty=2)
				abline(h=b, col=col_smr[i],lty=1, lwd=1)
				abline(v=end_EPOC, col=col_smr[i], lty=1)
				abline(v=end_EPOC.mmr, col="magenta", lty=2,lwd=2)
				text(x=(max(d$time_mo2)+50)-(0.01*(max(d$time_mo2)+50)),y=(max(d$mo2))-(scale[i]*(max(d$mo2))), label=paste("EPOC=",EPOC_full,"/ ",smr_type, sep=""), cex=0.8, col=col_smr[i], pos=2)
				
			}else{
				abline(h=b, col=col_smr[i],lty=1, lwd=1)
				abline(v=end_EPOC, col=col_smr[i], lty=1)
				abline(v=end_EPOC.mmr, col="magenta", lty=2,lwd=2)
				text(x=(max(d$time_mo2)+50)-(0.01*(max(d$time_mo2)+50)),y=(max(d$mo2))-(scale[i]*(max(d$mo2))), label=paste("EPOC=",EPOC_full,"/ ",smr_type, sep=""), cex=0.8, col=col_smr[i], pos=2)
				
			}
	
		}else{
			if(i==1){
				plot(x=range(newVal$init), xlim=c(0,max(d$time_mo2, na.rm=TRUE)+50), ylim=c(0,max(d$mo2, na.rm=TRUE)),type='n', ylab="MO2", xlab="time (min)", main=paste("Smoothness=",spar, sep=""))
				points(d$time_mo2,d$mo2)
				lines(newy[[1]], newy[[2]], col="blue")
				abline(v=60, col="grey", lty=2)
				abline(v=120, col="grey", lty=2)
				abline(v=180, col="grey", lty=2)
				abline(v=240, col="grey", lty=2)
				abline(v=300, col="grey", lty=2)
				abline(h=b, col=col_smr[i],lty=1, lwd=1)
				abline(v=end_EPOC, col=col_smr[i], lty=1)
				abline(v=end_EPOC.mmr, col="magenta", lty=2,lwd=2)
				text(x=(max(d$time_mo2)+50)-(0.01*(max(d$time_mo2)+50)),y=(max(d$mo2))-(scale[i]*(max(d$mo2))), label=paste("EPOC=",EPOC_full,"/ ",smr_type, sep=""), cex=0.8, col=col_smr[i], pos=2)
				
				
			}
		 if(!i==1 & !i==5){
				abline(h=b, col=col_smr[i],lty=1, lwd=1)
				abline(v=end_EPOC, col=col_smr[i], lty=1)
				abline(v=end_EPOC.mmr, col="magenta", lty=2,lwd=2)
				text(x=(max(d$time_mo2)+50)-(0.01*(max(d$time_mo2)+50)),y=(max(d$mo2))-(scale[i]*(max(d$mo2))), label=paste("EPOC=",EPOC_full,"/ ",smr_type, sep=""), cex=0.8, col=col_smr[i], pos=2)
			
			}
		  
		  
		}# end of "if(MLND == TRUE)"
	
		if (i == 5){
	
		  EPOCdata<-rbind(EPOCdata,EPOCdata.temp)
    }
			
	}	
  
	return(EPOCdata)
	
}# end of EPOC.spar plot function

# WORK: add defaults! - min_length_mmr should have a default especially if data.MMR = "none"

MMR_SMR_AS_EPOC<-function(data.MMR,
                          data.SMR,
                          AnimalID,
                          BW.animal,
                          resp.V, 
                          r2_threshold_smr,
                          r2_threshold_mmr,
                          min_length_mmr,
                          scaling_exponent_mmr=1,
                          scaling_exponent_smr=1,
                          common_mass=1,
                          mo2_val_for_calc = "mo2_1kg", # mo2_common_mass_kg, mo2_per_individual_kg
                          plot_smr_quantile=15, 
                          mmr_type = "min", # the standard way
                          date_format = "m/d/y", #c("m/d/y","d/m/y","y-m-d"),
                          N_Ch = 4, 
                          drop_ch = NULL, 
                          MLND = TRUE, 
                          epoc_threshold = 1,
                          recovMMR_threshold = 0.5,
                          end_EPOC_Ch=NA,
                          background_prior = NA,
                          background_post = NA,
                          background_slope = NULL,
                          background.V = NULL, 
                          background_linear_gr = FALSE,
                          match_background_Ch = FALSE, 
                          mmr_background = "SAME_slope",
                          path = ".",
                          # test = "development_NA", 
                          spars_levels = c(0.1, 0.2, 0.3)){
	graphics.off()	
  
   filename.SMR<-paste(gsub('.{4}$', '',data.SMR), "_SMR", sep="")
	 filename.MMR<-paste(gsub('.{4}$', '',data.MMR), "_MMR_", sep="")
  
	 
	 
  # **********************************************
  # START-- >>> background 
  if((is.null(background.V) & !is.null(background_slope) ) |(!is.null(background.V) & is.null(background_slope))) {
    stop_function <- TRUE
    if(stop_function) stop("Must provide background unique slope and volume together, or provide a background file with same volumes as animal respirometers")
    }
  
 # background file manipulation
  # 1. find what channels recorded background 
  if (!is.na(background_post) | !is.na(background_prior) ) {
    # 2 calculate mean for each background channel 
    if((is.null(background.V) & !is.null(background_slope) ) |(!is.null(background.V) & is.null(background_slope))) {
    stop_function <- TRUE
      if(stop_function) stop("If using manually input background slopes, must provide both, a unique slope and volume of the repirometer")
    }
    
    
    if(!is.na(background_prior)){
      back_prior<-read.csv(background_prior)
      back_ch<-length (unique(back_prior$Ch))
    }
    if(!is.na(background_post)){
      back_post<-read.csv(background_post)
      back_ch<-length (unique(back_post$Ch))
    }
    
     # Jan 4 2020: make linear regression over time and correct background based on a predicted value
    # create 
    if (background_linear_gr==TRUE){
      if(!exists("back_prior") | !exists("back_post")){
       stop("Missing a file to model the growth of bacteria, must provide both: \"back_prior\" \"and back_post\" files")
      }
        
      if (!dir.exists("../plots_background")){
        dir.create(file.path("../plots_background"), recursive = TRUE)
      }
      # getting the right date and time format 
        back_prior$DateTime_start<-as.character(gsub("(", "", back_prior$DateTime_start, fixed=TRUE))
  	  	back_prior$DateTime_start<-as.character(gsub(")", "", back_prior$DateTime_start, fixed=TRUE))
  	  	back_post$DateTime_start<-as.character(gsub("(", "", back_post$DateTime_start, fixed=TRUE))
  	    back_post$DateTime_start<-as.character(gsub(")", "", back_post$DateTime_start, fixed=TRUE))
  	    
    	if (date_format== "m/d/y"){
         back_prior$DateTime_start<- strptime(back_prior$DateTime_start, format="%m/%d/%y %H:%M:%OS") 
  	     back_post$DateTime_start<- strptime(back_post$DateTime_start, format="%m/%d/%y %H:%M:%OS") 
    	}
    	if (date_format== "d/m/y"){
         back_prior$DateTime_start<- strptime(back_prior$DateTime_start, format="%d/%m/%y %H:%M:%OS") 
  	     back_post$DateTime_start<- strptime(back_post$DateTime_start, format="%d/%m/%y %H:%M:%OS") 
    	}
    	if (date_format== "y-m-d"){
        # DateTime<- chron(dates=back_$date,times=back_$time,format=c('y-m-d','h:m:s')) 
    	   back_prior$DateTime_start<- strptime(back_prior$DateTime_start, format="%y-%m-%d %H:%M:%OS") 
  	     back_post$DateTime_start<- strptime(back_post$DateTime_start, format="%y-%m-%d %H:%M:%OS") 
    	}
      
  	  back_all<- rbind(back_prior, back_post)
  	  back_all$DateTime_start<- as.POSIXct(back_all$DateTime_start)
      
  	  back_regression_plot<-ggplot(data=back_all, aes(x=DateTime_start, y=m, colour=Ch, group=Ch))+
        geom_point()+
        geom_smooth(method="lm", se=FALSE)+
        theme_bw()+
        ylab("Regression slope value- mgO2/min")+
  	    theme(axis.text.x = element_text(angle = 45))+
  	    facet_grid(Ch~.)
  	
  	  if (path == "."){
  	    # plotname.backgr.analysis<-paste( filename.SMR,"_PLOT_BACKGROUND_regressions.png", sep="")	
  	    plotname.backgr<-paste( filename.SMR,"_PLOT_BACKGROUND.png", sep="")	
  	  }else{
  	   	# plotname.backgr.analysis<-paste("../plots_background/", filename.SMR,"_PLOT_BACKGROUND_regressions.png", sep="")			  
  	   	plotname.backgr<-paste("../plots_background/", filename.SMR,"_PLOT_BACKGROUND.png", sep="")	

  	  }
  	  
  	  
		  # png(plotname.backgr.analysis, width=4, height=7, res=300, units="in")
		  #   print(back_regression_plot)
		  # dev.off()
		  #  

  	 if (match_background_Ch==TRUE){
  	    message("Background: assuming a linear growth of bacteria over time | Using channel specific growth slopes")

        back_ch_regressions<-list()
        
        for(i in 1:back_ch){
          back_ch_d<- back_all[back_all$Ch==(unique(back_all$Ch))[i],]
          Ch<-substr(as.character(back_ch_d$Ch[1]), start=3, stop=3)
      
          back_regression_name<- paste("back_regression", Ch, sep="") # channel names with a channel # at the end
          regression<- lm(m~DateTime_start, data = back_ch_d) 
          assign(back_regression_name, regression)
          back_ch_regressions[[i]] <- assign(back_regression_name, regression)
        
        }
        # WORK
        # save background regressions

      }else{# end for ch specific regressions
        # get one background slope based on all datapoints collected 
        back_regression<- lm(m~DateTime_start, data = back_all) 
        message("Background: assuming a linear growth of bacteria over time | Using one mean slope for all channels")
      } 
  	  
  	  
    } # end for getting linear regressions for the background 
   
    if (background_linear_gr==FALSE){   
      if (!is.na(background_prior)){
        back_ch_prior<-list()
        back_ch_prior_names<-list()
        
        for( i in 1:back_ch){
          back_ch_d<-back_prior[back_prior$Ch==(unique(back_prior$Ch))[i],]
          Ch<-substr(as.character(back_ch_d$Ch[1]), start=3, stop=3)
        
          back_m_name<-paste("back_m_prior", Ch, sep="") # channel names with a channel # at the end
          mean_m<-sum(back_ch_d$m)/nrow(back_ch_d) # get average slopes from the prior background cycles can be limitless essentially 
          assign(back_m_name, mean_m)
          back_ch_prior[[i]] <- assign(back_m_name, mean_m)
          back_ch_prior_names[[i]] <- back_m_name
          
        }
        
      }
      # 3. estiamte one background slope mean to be used in MR corrections
      if (!is.na(background_post)){
       
        back_ch_post<-list()
        back_ch_post_names<-list()
        
          for( i in 1:back_ch){
            
            back_ch_d<-back_post[back_post$Ch==(unique(back_post$Ch))[i],]
            Ch<-substr(as.character(back_ch_d$Ch[1]), start=3, stop=3)
            
            back_m_name<-paste("back_m_post", Ch, sep="")
            mean_m<-sum(back_ch_d$m)/nrow(back_ch_d)
            
            assign(back_m_name, mean_m)
            
            back_ch_post[[i]] <- assign(back_m_name, mean_m)
            back_ch_post_names[[i]] <- back_m_name
        
          }
      }
    
      # get a list of our values 
  
      ## should look for possible variables:
      # back_m_post1
      # back_m_post2
      # back_m_post3
      # back_m_post4 
      # back_m_prior1
      # back_m_prior2
      # back_m_prior3
      # back_m_prior4
      
      if (match_background_Ch==TRUE){
        # conditions possible:
        # prior only 
        # post only 
        # prior and post for a specific channel 
        
        if (!is.na(background_prior)){
          ch_available<-as.numeric(substr(as.character(unique(back_prior$Ch)),start=3, stop=3))
        }
         if (!is.na(background_post)){
          ch_available<-as.numeric(substr(as.character(unique(back_post$Ch)),start=3, stop=3))
        }
         
  
        for (i in 1:back_ch){
        
          # prior only condition
         if(exists(paste("back_m_prior", ch_available[i], sep="")) & !exists(paste("back_m_post", ch_available[i], sep=""))){
            
           if(back_ch_prior_names[[i]]==paste("back_m_prior", ch_available[i], sep="")){
              message("matching background Channels")
            
              back_m_name2<-paste("back_m", ch_available[i], sep="")
              assign(back_m_name2, back_ch_prior[[i]])
              #next # continue the loop to the nect channel 
  
           }
           
         }
          
          # post only condition
          if(!exists(paste("back_m_prior", ch_available[i], sep="")) & exists(paste("back_m_post",ch_available[i], sep=""))){
  
            if(back_ch_post_names[[i]]==paste("back_m_post", ch_available[i], sep="")){
              message("matching background Channels")
              
              back_m_name2<-paste("back_m", ch_available[i], sep="")
              assign(back_m_name2, back_ch_post[[i]])
              #next # continue the loop to the next channel 
              
            }
            
          }
          
          if(exists(paste("back_m_prior", ch_available[i], sep="")) & exists(paste("back_m_post",ch_available[i], sep=""))){
            
            if(back_ch_post_names[[i]]==paste("back_m_post", ch_available[i], sep="")){
             # message("matching background Channels")
              prior_post_mean<-(back_ch_prior[[i]] + back_ch_post[[i]]) / 2
              
              back_m_name2<-paste("back_m", ch_available[i], sep="")
              assign(back_m_name2,  prior_post_mean)
              #next # continue the loop to the nect channel 
              
            }
            
          } # closes 'prior and post available' condition statement 
  
        } # closes the loop -  here have 
      
      }else{ #match_background_Ch=TRUE switch to FALSE 
      
       # 1. prior file only 
       # 2. post file only 
       # 3. prior and post 
        
        if(is.na(background_post) | !is.na(background_prior)){
         
          prior_mean<-sum(as.numeric(back_ch_prior)) / (length (unique(back_prior$Ch)))
          back_m<-prior_mean
          
        }
        
        if(!is.na(background_post) | is.na(background_prior)){
          
          post_mean<-sum(as.numeric(back_ch_post)) / (length (unique(back_post$Ch)))
          back_m<-post_mean
          
        }
        
        if(!is.na(background_post) & !is.na(background_prior)){
          
          prior_mean<-sum(as.numeric(back_ch_prior)) / (length (unique(back_prior$Ch)))
          post_mean<-sum(as.numeric(back_ch_post)) / (length (unique(back_post$Ch)))
          back_m<-(prior_mean+post_mean) / 2
          
        }
      
      } # end of match background == FALSE (the else part of if statement)
    }# end of background_linear_gr == FALSE
      
  }# the end of getting the background slopes 
  
  # if match_background_Ch=FALSE then correct all SMR values using back_m (the total avrg)
  # if match_background_Ch=TRUE then correct all SMR values using Chanel specific back_mCh, where Ch is the number of the channel (the total avrg)
  # END -- >>> background 
  # **********************************************
	 
	newdata.smr<-as.data.frame(matrix(ncol=33, nrow=0))
	names(newdata.smr)<-c("filename", "ID", "Ch", "BW","t_min","t_max", "t_mean", "N_mo2", #8
	"smr_mean10minVal","smr_SD10minVal", "smr_CV10minVal", "SMR_low10quant","SMR_low15quant","SMR_low20quant", #6
	"smr_mlnd", "smr_CVmlnd", "smr_Nmlnd", #3
	"mmr", "mmr_overall", #2
	"AS_smr_mean10minVal", "AS_SMR_low10quant", "AS_SMR_low15quant", "AS_SMR_low20quant", "AS_smr_mlnd", #5
	"AS_smr_mean10minVal_overall", "AS_SMR_low10quant_overall", "AS_SMR_low15quant_overall", "AS_SMR_low20quant_overall", "AS_smr_mlnd_overall", #5
	"mmr_length_cycle", "scaling_exponent_mmr", "scaling_exponent_smr", "common_mass")#1

	# cols = c(4:17, 19:32)
	# newdata.smr[,cols] %<>% lapply(function(x) as.numeric(as.character(x)))
	# cols2 = c(1:3, 18)
	# newdata.smr[,cols2] %<>% lapply(function(x) as.character(x))

	cols = c(4:33)
	newdata.smr[,cols] %<>% lapply(function(x) as.numeric(as.character(x)))
	cols2 = c(1:3)
	newdata.smr[,cols2] %<>% lapply(function(x) as.character(x))

	# print(ncol(newdata.smr))
	
	 
	 
 	# **********************************************
  # START -- >>> MMR file available
  if (data.MMR!="none"){
    
    d_MMR<-read.csv(data.MMR)
      	
    # feb 23 note: 
    # the previous version of the code before MMR became a flexible version (beginning of feb 2020), has an extra column: the delay is still there. If that is still there, but that file is desired to be used in the "new" MMR_SMR_AS_EPOC, thne get rid of that column and write a warning message:
    if(colnames(d_MMR)[4] == "delay_min"){
      d_MMR<-d_MMR[, -4]
      message("Older version of MMR analyzed file is used: dropping unused column \"delay_min\"")
    }
    
    
    ### getting the right data frame of MMR values / choosing the right "sliding"
  	if (min_length_mmr=="full"){min_length_mmr<-1}	
  	
  	times_list<-c(60,90,120,180,1)
  	ntime<-which(times_list==min_length_mmr)	
  			
		d_MMR$DateTime_start<-as.character(gsub("(", "", d_MMR$DateTime_start, fixed=TRUE))
		d_MMR$DateTime_start<-as.character(gsub(")", "", d_MMR$DateTime_start, fixed=TRUE))
  
  	if (date_format== "m/d/y"){
       d_MMR$DateTime_start<- strptime(d_MMR$DateTime_start, format="%m/%d/%y %H:%M:%OS") 
  	}
  	if (date_format== "d/m/y"){
       d_MMR$DateTime_start<- strptime(d_MMR$DateTime_start, format="%d/%m/%y %H:%M:%OS") 
  	}
  	if (date_format== "y-m-d"){
      # DateTime<- chron(dates=d_MMR$date,times=d_MMR$time,format=c('y-m-d','h:m:s')) 
  	   d_MMR$DateTime_start<- strptime(d_MMR$DateTime_start, format="%y-%m-%d %H:%M:%OS") 
  	}
		d_MMR$DateTime_start<-as.character(d_MMR$DateTime_start)

  	# drop any unwanted channels
  	if(!is.null(drop_ch[1])){
  	    n_ch_drop<-length(drop_ch)
  	     for(i in 1:n_ch_drop){
  	      d_MMR<-d_MMR[(substr(as.character(d_MMR$Ch), start=3, stop=3)!=drop_ch[i]),]
  	      }
  	  }
  	d_MMR$cycle_type<-factor(d_MMR$cycle_type)
  	d_MMR$Ch<-factor(d_MMR$Ch)
  
  	
  	d_MMR$cycle_mmr[d_MMR$cycle_mmr=="1" | d_MMR$cycle_mmr=="2"| d_MMR$cycle_mmr=="3"| d_MMR$cycle_mmr=="4"]<-1 # 1 is an arbritrary number indicating that this is a "full length MMR file slope"
  	d_MMR$cycle_mmr[(d_MMR$cycle_mmr=="1" | d_MMR$cycle_mmr=="2"| d_MMR$cycle_mmr=="3"| d_MMR$cycle_mmr=="4") & grepl("cycle", as.character(d_MMR$cycle_type))]<-10 # 10 is an arbitrary number that is not going to show up anywhere else/ needed to indicate teh cycle "full length" slope
  	
  	
  	
  	#### SELECT the type of MMR SLOPES as wanted 
  	# select either the absolute steepest slopes or the mean lowest slope
  	# See Zhang et al PEAk slopes JEB paper -- incorporate
  	if (mmr_type == "min"){
  	  d_MMR<-d_MMR[d_MMR$cycle_type!="Mean_1minMean", ]
  	  levels(droplevels(d_MMR$cycle_type))
  	}
  	  # }else{ # this will be the autocorrelation slope
  	  # d_MMR<-d_MMR[d_MMR$cycle_type!="Mean_1minSlopes", ]
  	  # levels(droplevels(d_MMR$cycle_type))
  	  # }
  	### 
  	
  	# print(d_MMR)
  	
  	d<-d_MMR %>%
  		group_by(Ch) %>%
  		filter(cycle_mmr== min_length_mmr| cycle_mmr== 1 | cycle_type=="cycle2" | cycle_type=="cycle3" | cycle_type=="cycle4")
  	d<-as.data.frame(d)
  	
  	d_MMR_list<-split(d_MMR, d_MMR$Ch)
    
  	d_list<-split(d, d$Ch)
  	# print(d)

    d_temp1<-as.data.frame(matrix(nrow=0, ncol=13))
    colnames(d_temp1)<-c("cycle_type", "cycle_start","cycle_end",  "cycle_mmr", "r2" ,"m", "b" , "t_min", "t_max", "t_mean", "Ch", "DateTime_start")	
    
  	for (i in 1:length(d_list)){
  	  d_ch<-as.data.frame(d_list[i])
  	  colnames(d_ch)<-c("cycle_type", "cycle_start","cycle_end", "cycle_mmr", "r2" ,"m", "b" , "t_min", "t_max", "t_mean", "Ch", "DateTime_start")	
  	  # print(d_ch)
  	  
  	  # d_temp<-d_ch
  	  # d_temp<-d_ch[grepl("MMR", as.character(d_ch$cycle_type)),]
  	  if((nrow(as.data.frame(d_ch[grepl("MMR", as.character(d_ch$cycle_type)),])))>1){
  	   
  	    d_ch<-d_ch[c(which((as.numeric(d_ch$cycle_mmr)>1 & grepl("MMR", as.character(d_ch$cycle_type))) | grepl("cycle", as.character(d_ch$cycle_type)))),]
  	  }
  	  d_temp1<-rbind(d_temp1, d_ch)

  	}
    
    
    d<-d_temp1
    # print(d)
   
  
  	if(any(d$r2<r2_threshold_mmr)){
  		d_new<-d_MMR[0,]
  		
  		for (i in 1:length(d_MMR_list)){
  			d_ch<-as.data.frame(d_MMR_list[i])
  			colnames(d_ch)<-c("cycle_type", "cycle_start","cycle_end", "cycle_mmr", "r2" ,"m", "b" , "t_min", "t_max", "t_mean", "Ch", "DateTime_start")	
  			
  			d_temp<-d_ch[grepl("MMR", as.character(d_ch$cycle_type)),]
  			
  			if (d_temp$r2[d_temp$cycle_mmr==min_length_mmr]<r2_threshold_mmr){
  				
  				if (all(d_temp$r2<r2_threshold_mmr)){
  						new_min_length_mmr<-d_temp$cycle_mmr[d_temp$r2==max(d_temp$r2)][1]
  						d_ch<-d_ch[c(which(grepl("cycle", as.character(d_ch$cycle_type)) | d_ch$cycle_mmr== new_min_length_mmr)),]
  						d_new<-rbind(d_new, d_ch)
  							message(paste(d_ch$Ch[1],": MMR measures all r2 below the set threshold: ", r2_threshold_mmr," / USE max r2 = ", d_ch$r2[1], sep="")) 
  							message(paste(d_ch$Ch[1],": MMR measure time extended from ", min_length_mmr ," to ", new_min_length_mmr , sep=""))
  					
  				}
  				
  				for(j in ntime:(length(times_list))){
  #~ 					
  					if(d_temp$r2[d_temp$cycle_mm==times_list[j]]>=r2_threshold_mmr){
  						new_min_length_mmr<-times_list[j]
  						d_ch<-d_ch[c(which(grepl("cycle", as.character(d_ch$cycle_type)) | d_ch$cycle_mmr== new_min_length_mmr)),]
  						d_new<-rbind(d_new, d_ch)
  						
  							if (new_min_length_mmr==1){
  								message(paste(d_ch$Ch[1],": MMR measure time extended from ", min_length_mmr ," to full file", sep=""))
  							}else{
  								message(paste(d_ch$Ch[1],": MMR measure time extended from ", min_length_mmr ," to ", new_min_length_mmr , sep=""))
  							}
  						break	
  					}
  					
  				}			
  				
  			}else{
  				d_ch<-d_ch[c(which(grepl("cycle", as.character(d_ch$cycle_type)) | d_ch$cycle_mmr==min_length_mmr)),]
  				d_new<-rbind(d_new, d_ch)
  			}	
  		}
  		
  		d_new$cycle_type[d_new$cycle_type=="MMR_slide"]<-"MMR"
  			
  	}else{
  		
  		d$cycle_type[d$cycle_type=="MMR_slide"]<-"MMR"
  		d_new<-d
  	}
  	
  	d_MMR<-as.data.frame(d_new)
  	# print(d_MMR)
  	### MMR data set established 

  	if (data.MMR!="none"){
  		d_MMR$bw<-NA
  		d_MMR$mo2<-NA
  		d_MMR$ID<-NA
      d_MMR$resp.V<-NA
      d_MMR$scaling_exponent<-scaling_exponent_mmr
      d_MMR$common_mass<-common_mass
  	}
  	
  	for(i in 1:4){
			if( any(grepl(as.character(i),as.character(d_MMR$Ch)))){
				nameCh<-paste("Ch",i,sep="")
				bw.val<-BW.animal[i]
				ID<-AnimalID[i]
				resp.Vol<-resp.V[i]
				n.row<-which(d_MMR$Ch==nameCh)
				d_MMR$bw[n.row]<-bw.val	
				d_MMR$ID[n.row]<-as.character(ID)
				d_MMR$resp.V[n.row]<-resp.Vol
			}
		}
		
  
  	
  	# **********************************************
    # START -- >>> background corrections MMR  
  	# no change with the linear background additions. MMR is the same since it is one measurement at the beginning.
  	# MMR end values MR in mgO2/min/kg
	  
	
    # mmr_background = "SAME_slope" (default is to take the same value as the entire smr), must specify mmr_background = "back_prior" to have a prior slope only, or add a specific value to be substracted for mmr background
	  
    
  	 d_MMR$DateTime_start<- strptime(d_MMR$DateTime_start, format="%Y-%m-%d %H:%M:%OS")
    	
	  
     if(mmr_background != "SAME_slope" & background_linear_gr==FALSE){
        
        # message("MMR slope corrected using the same background slope value and methods as for all SMR slopes")
       
        if(mmr_background == "back_prior"){
          
          if(match_background_Ch != TRUE){ # if NOT channel specific 
    
            if(!exists("prior_mean")) stop("Incomplete request: no prior-trail background respiration data to correct MMR measurement, must provide \"background_prior\"file")
            
              for (i in 1:nrow(d_MMR)){
          		  d_MMR$mo2[i]<-(( d_MMR$m[i]*(d_MMR$resp.V[i]-d_MMR$bw[i])) - (prior_mean * d_MMR$resp.V[i])) /(d_MMR$bw[i])#^scaling_exponent_mmr) # units mgO2 kg-1 min-1 -CORRECTED for back resp
              }
            
            message("MMR corrected for background: using a common background from a \"background_prior\" file only (same slope for all channels) | bacterial respiration slope: ", prior_mean)

            }else{ # this will be mmr slope different than smr, and is channel specific, i.e. match__background==TRUE
              # back_m_prior1
              # back_m_prior2
              # back_m_prior3
              # back_m_prior4
             message("MMR corrected for background: using Ch specific average background values from \"background_prior\"")
               for (i in 1:nrow(d_MMR)){
        
        		   # print(c(d_MMR$bw[1],d_MMR$Ch[i]))
        		    if(substr(d_MMR$Ch[i], start=3, stop=3) == "1"){
        		    d_MMR$mo2[i]<-(( d_MMR$m[i]*(d_MMR$resp.V[i]-d_MMR$bw[i])) - (back_m_prior1 * d_MMR$resp.V[i])) /(d_MMR$bw[i])#^scaling_exponent_mmr) # units mgO2 kg-1 min-1 -CORRECTED for back resp
        		    # message("Channel: 1,  bacterial respiration slope: ", back_m1)
        		    }
        		    
        		    if(substr(d_MMR$Ch[i], start=3, stop=3) == "2"){
        		      d_MMR$mo2[i]<-(( d_MMR$m[i]*(d_MMR$resp.V[i]-d_MMR$bw[i])) - (back_m_prior2 * d_MMR$resp.V[i])) /(d_MMR$bw[i])#^scaling_exponent_mmr) # units mgO2 kg-1 min-1 -CORRECTED for back resp
        		      # message("Channel: 2,  bacterial respiration slope: ", back_m2)
        		    }
        		    
        		    if(substr(d_MMR$Ch[i], start=3, stop=3) == "3"){
        		      d_MMR$mo2[i]<-(( d_MMR$m[i]*(d_MMR$resp.V[i]-d_MMR$bw[i])) - (back_m_prior3 * d_MMR$resp.V[i])) /(d_MMR$bw[i])#^scaling_exponent_mmr) # units mgO2 kg-1 min-1 -CORRECTED for back resp
        		      # message("Channel: 3,  bacterial respiration slope: ", back_m3)
        		    }
        		    
        		    if(substr(d_MMR$Ch[i], start=3, stop=3) == "4"){
        		      d_MMR$mo2[i]<-(( d_MMR$m[i]*(d_MMR$resp.V[i]-d_MMR$bw[i])) - (back_m_prior4 * d_MMR$resp.V[i])) /(d_MMR$bw[i])#^scaling_exponent_mmr) # units mgO2 kg-1 min-1 -CORRECTED for back resp
        		      # message("Channel: 4,  bacterial respiration slope: ", back_m4)
        		    }
        		  
        		  }

          }  
          
        }else{ # else from mmr = back prior
          
          if(!is.numeric(mmr_background)) stop("Incomplete request: must provide \"mmr_background\" slope value to manually MMR for background OR use mmr_background=\"SAME_slope\"")
         
           for (i in 1:nrow(d_MMR)){
        		  d_MMR$mo2[i]<-(( d_MMR$m[i]*(d_MMR$resp.V[i]-d_MMR$bw[i])) - (mmr_background * d_MMR$resp.V[i])) /(d_MMR$bw[i])#^scaling_exponent_mmr) # units mgO2 kg-1 min-1 -CORRECTED for back resp
           }
          message("MMR corrected for background: using specified slope value, same slope for all channels | bacterial respiration slope: ", mmr_background)
        }
       
        
		  }else{ # ALL OF THE CORRECTION BELOW IS WITH USING SAME SLOPE AS SMR
		    
		    if(background_linear_gr==TRUE){
		        if(match_background_Ch==TRUE){
		            for (i in 1:nrow(d_MMR)){
      
		              background_slopes<-data.frame(matrix(ncol=1, nrow=nrow(d_MMR)))
		              colnames(background_slopes)<-c("back_m")
		              
    		   # print(c(d_MMR$bw[1],d_MMR$Ch[i]))
          		    if(substr(as.character(d_MMR$Ch[i]), start=3, stop=3) == "1"){
          		      background_slopes$back_m[i]<-predict(back_regression1, data.frame(DateTime_start = d_MMR$DateTime_start[i]))
                    back_m1<-predict(back_regression1, data.frame(DateTime_start = d_MMR$DateTime_start[i]))
            		    d_MMR$mo2[i]<-(( d_MMR$m[i]*(d_MMR$resp.V[i]-d_MMR$bw[i])) - (back_m1 * d_MMR$resp.V[i])) /(d_MMR$bw[i])#^scaling_exponent_mmr) # units mgO2 kg-1 min-1 -CORRECTED for back resp
          		    # message("Channel: 1,  bacterial respiration slope: ", back_m1)
          		    }
          		    
          		    if(substr(as.character(d_MMR$Ch[i]), start=3, stop=3) == "2"){
          		      background_slopes$back_m[i]<-predict(back_regression2, data.frame(DateTime_start = d_MMR$DateTime_start[i]))
          		      back_m2<-predict(back_regression2, data.frame(DateTime_start = d_MMR$DateTime_start[i]))
          		      d_MMR$mo2[i]<-(( d_MMR$m[i]*(d_MMR$resp.V[i]-d_MMR$bw[i])) - (back_m2 * d_MMR$resp.V[i])) /(d_MMR$bw[i])#^scaling_exponent_mmr) # units mgO2 kg-1 min-1 -CORRECTED for back resp
          		      # message("Channel: 2,  bacterial respiration slope: ", back_m2)
          		    }
          		    
          		    if(substr(as.character(d_MMR$Ch[i]), start=3, stop=3) == "3"){
          		      background_slopes$back_m[i]<-predict(back_regression3, data.frame(DateTime_start = d_MMR$DateTime_start[i]))
          		      back_m3<-predict(back_regression3, data.frame(DateTime_start = d_MMR$DateTime_start[i]))
          		      d_MMR$mo2[i]<-(( d_MMR$m[i]*(d_MMR$resp.V[i]-d_MMR$bw[i])) - (back_m3 * d_MMR$resp.V[i])) /(d_MMR$bw[i])#^scaling_exponent_mmr) # units mgO2 kg-1 min-1 -CORRECTED for back resp
          		      # message("Channel: 3,  bacterial respiration slope: ", back_m3)
          		    }
          		    
          		    if(substr(as.character(d_MMR$Ch[i]), start=3, stop=3) == "4"){
                    background_slopes$back_m[i]<-predict(back_regression4, data.frame(DateTime_start = d_MMR$DateTime_start[i]))
          		      back_m4<-predict(back_regression4, data.frame(DateTime_start = d_MMR$DateTime_start[i]))
          		      d_MMR$mo2[i]<-(( d_MMR$m[i]*(d_MMR$resp.V[i]-d_MMR$bw[i])) - (back_m4 * d_MMR$resp.V[i])) /(d_MMR$bw[i])#^scaling_exponent_mmr) # units mgO2 kg-1 min-1 -CORRECTED for back resp
          		      # message("Channel: 4,  bacterial respiration slope: ", back_m4)
          		    }
          		  }# end of the loop 
		        }
            if(match_background_Ch==FALSE){
                background_slopes<-data.frame(DateTime_start = d_MMR$DateTime_start)
                background_slopes$back_m<-predict(back_regression, background_slopes)
          
            	 for (i in 1:nrow(d_MMR)){
            	  d_MMR$mo2[i]<-(( d_MMR$m[i]*(d_MMR$resp.V[i]-d_MMR$bw[i])) - (background_slopes$back_m[i] * d_MMR$resp.V[i])) /(d_MMR$bw[i])#^scaling_exponent_mmr) # units mgO2 kg-1 min-1 -CORRECTED for back resp
            	 }
		        }
		      # message("Correcting for average background from file - MMR corrected (same slope value as for SMR) ")
        		  # message("herehere")
		    }else{
            	# 1.1 if background files (either prior or post, or both) are provided and its one overall mean value (back_m)
        		if ((( !is.na(background_post) | !is.na(background_prior)) & match_background_Ch==FALSE) & is.null(background_slope) ){
        		  
        		  
        		  for (i in 1:nrow(d_MMR)){
        		    d_MMR$mo2[i]<-(( d_MMR$m[i]*(d_MMR$resp.V[i]-d_MMR$bw[i])) - (back_m * d_MMR$resp.V[i])) /(d_MMR$bw[i])#^scaling_exponent_mmr) # units mgO2 kg-1 min-1 -CORRECTED for back resp
        		  }
        		  # message("Correcting for average background from file - MMR corrected (same slope value as for SMR) ")
        		  message("MMR corrected for background: using a mean (prior and/or post) background measurements | mean bacterial respiration slope: ", back_m)
        		  
        		}	
        		
        		## if background slope and volume are specifically provided, then use those! this alos overrides the background prior and post argument. 
        		# all channels with the same slope 
        		if (!is.null(background_slope) ){
        		 
        		  for (i in 1:nrow(d_MMR)){
        		    d_MMR$mo2[i]<-(( d_MMR$m[i]*(d_MMR$resp.V[i]-d_MMR$bw[i])) - (background_slope * background.V)) /(d_MMR$bw[i])#^scaling_exponent_mmr) # units mgO2 kg-1 min-1 -CORRECTED for back resp
        		  }
        		  # message("Correcting for common background slope - only MMR corrected (same slope value as for SMR) ")
        		  message("MMR corrected for background: using a common manually provided background slope for all channels | bacterial respiration slope: ", background_slope)
        		}	
        		
        		# 1.2 if background files are provided and its channel specific - MEAN SLOPE OF PRIOR AND POST
        		if (( !is.na(background_post) | !is.na(background_prior)) & match_background_Ch==TRUE){
        		  message("MMR: correcting for Ch specific average background (using mean slope back prior and/or post), same slope value as for SMR")
               for (i in 1:nrow(d_MMR)){
        
        		   # print(c(d_MMR$bw[1],d_MMR$Ch[i]))
        		    if(substr(d_MMR$Ch[i], start=3, stop=3) == "1"){
        		    d_MMR$mo2[i]<-(( d_MMR$m[i]*(d_MMR$resp.V[i]-d_MMR$bw[i])) - (back_m1 * d_MMR$resp.V[i])) /(d_MMR$bw[i])#^scaling_exponent_mmr) # units mgO2 kg-1 min-1 -CORRECTED for back resp
        		    # message("Channel: 1,  bacterial respiration slope: ", back_m1)
        		    
        		    }
        		    
        		    if(substr(d_MMR$Ch[i], start=3, stop=3) == "2"){
        		      d_MMR$mo2[i]<-(( d_MMR$m[i]*(d_MMR$resp.V[i]-d_MMR$bw[i])) - (back_m2 * d_MMR$resp.V[i])) /(d_MMR$bw[i])#^scaling_exponent_mmr) # units mgO2 kg-1 min-1 -CORRECTED for back resp
        		      # message("Channel: 2,  bacterial respiration slope: ", back_m2)
        		    
        		    }
        		    
        		    if(substr(d_MMR$Ch[i], start=3, stop=3) == "3"){
        		      d_MMR$mo2[i]<-(( d_MMR$m[i]*(d_MMR$resp.V[i]-d_MMR$bw[i])) - (back_m3 * d_MMR$resp.V[i])) /(d_MMR$bw[i])#^scaling_exponent_mmr) # units mgO2 kg-1 min-1 -CORRECTED for back resp
        		      # message("Channel: 3,  bacterial respiration slope: ", back_m3)
        
        		    }
        		    
        		    if(substr(d_MMR$Ch[i], start=3, stop=3) == "4"){
        		      d_MMR$mo2[i]<-(( d_MMR$m[i]*(d_MMR$resp.V[i]-d_MMR$bw[i])) - (back_m4 * d_MMR$resp.V[i])) /(d_MMR$bw[i])#^scaling_exponent_mmr) # units mgO2 kg-1 min-1 -CORRECTED for back resp
        		      # message("Channel: 4,  bacterial respiration slope: ", back_m4)
        
        		    }
        		  }
        		}	
        		
        		
        		# 2. if background files are not provided 
        		if ((is.na(background_post) & is.na(background_prior)) & is.null(background_slope)){
        		  message("MMR: NO correction for background respiration")
        
          		for (i in 1:nrow(d_MMR)){
        		  	d_MMR$mo2[i]<-d_MMR$m[i]*(d_MMR$resp.V[i]-d_MMR$bw[i])/(d_MMR$bw[i])#^scaling_exponent_mmr) # units mgO2 kg-1 min-1
          		}
        		}	
		    }
		  }  		

    # END -- >>> background corrections MMR  
  	# **********************************************
  
		
  	# size correct each value for SMR calculations, lead with this into EPOC calculations
		d_MMR$mo2_individual_kg<-d_MMR$mo2 * d_MMR$bw
		# d_MMR$mo2_common_mass_kg<-exp( (scaling_exponent_mmr*(log(d_MMR$common_mass)-log(d_MMR$bw))) + log(d_MMR$mo2_individual_kg))
		d_MMR$mo2_common_mass_kg<-exp( (scaling_exponent_mmr*(log(d_MMR$common_mass)-log(d_MMR$bw))) + log(d_MMR$mo2_individual_kg))
		
    # ScalingSlope = (y2 - y1) / (x2 - x1) 
    # y2 = log (mo2) <<< current mass dependent value 
    # y1 = unknown <<< solve for; this will be log() values! use (exp()) to back-transform
    # x2 = log (mass kg) <<< current body mass in kg
    # x1 = log (common_mass) <<< common body mass for the correction 

    # solve for  
    # y1 = y2 - ( ScalingSlope * (x2 - x1) )
		# or solve for y2:
		# y2 = ScalingSlope * (x2 - x1) + y1
	  d_MMR$mo2_common_mass_kg <- exp( log(d_MMR$mo2_individual_kg) - (scaling_exponent_mmr * (log(d_MMR$bw) - log(d_MMR$common_mass)) ) )

	  # rename the mo2 to indicate that this is per kg fish 
	  names(d_MMR)[names(d_MMR) == 'mo2'] <- 'mo2_1kg'

    d_MMR$mo2<-d_MMR[,mo2_val_for_calc]

  }
	# END -- >>> MMR file available
 	# **********************************************

	
	
	# **********************************************
	# START -- >>> If SMR data IS available 
	# analyses MMR points for SMR value distributions and calculations too if available, no EPOC
	if(data.SMR!="none"){
			
		d_SMR<-read.csv(data.SMR)
			# drop any unwanted channels
	  if(!is.null(drop_ch[1])){
	    n_ch_drop<-length(drop_ch)
	     for(i in 1:n_ch_drop){
	      d_SMR<-d_SMR[(substr(as.character(d_SMR$Ch), start=3, stop=3)!=drop_ch[i]),]
	      }
	  }
    d_SMR$Ch<-factor(d_SMR$Ch)
		
	
  	if (path == "."){
  		plotname.freq<-paste( filename.SMR,"_PLOT_SMR_analyses.png", sep="")	
  		plotname.smr.meth<-paste( filename.SMR,"_PLOT_SMR_methodsALL.png", sep="")	

  	}else{
  		plotname.freq<-paste("../plots_min_values_SMR/", filename.SMR,"_PLOT_SMR_analyses.png", sep="")	
  		plotname.smr.meth<-paste("../plots_methods_sum_SMR/", filename.SMR,"_PLOT_SMR_methodsALL.png", sep="")	
  	
  	}
  		

		if(!colnames(d_SMR)[11]=="type"){
			d_SMR$type="SMR"
			d_SMR<-d_SMR[,c("time_frame", "min_start", "r2", "b", "m", "t_min", "t_max", "t_mean" ,"Ch", "DateTime_start", "type", "n_min", "ID_code" )]
		}
		
		
		## choose what to keep and what not for the SMR
		# 2) keep only > 1 min sections for SMR calculations any type  selected above (SMR, pre-, post-shallow slopes) and exclude "SMR-cut", "SMR-cut1", "SMR-cut2"
		d_SMR.1<-d_SMR[d_SMR$n_min>=1,]
		if (nrow(d_SMR.1)==0){
		  warning("!! All SMR measurements shorter than 1 min !!")
	   }else{
	    d_SMR<-d_SMR[d_SMR$n_min>=1,]
	    d_SMR.2<-d_SMR[d_SMR$n_min<=1,]
	      if(nrow(d_SMR.2)==0){
            message("All SMR/RMR measurements are > 1 min")
	        }else{
            message(paste("Using only SMR measurements >1 min: ", "Not using", nrow(d_SMR.2), " cycles", sep="")) 
        }
		}
		
		
		# 3) keep only sections with cycles above a set threshold of R2

		if(nrow(d_SMR[d_SMR$r2>=r2_threshold_smr,])<1){
		  message(paste("The R2 of SMR/RMR measurement slopes are below the specified threshold (", (r2_threshold_smr),") lower the R2 threshold and rerun the function. NOTE that the lowest slope is ", min(d_SMR$r2), sep=""))
	    }else{
	    d_SMR<-d_SMR[d_SMR$r2>=r2_threshold_smr,]
	  }

		# first get MO2 values in kg 
		d_SMR$bw<-NA
		d_SMR$mo2<-NA
		d_SMR$ID<-NA
		d_SMR$resp.V<-NA
		d_SMR$background_slope<-NA
		d_SMR$scaling_exponent<-scaling_exponent_smr
		d_SMR$common_mass<-common_mass
		
		for(i in 1:4){
			if(any(grepl(as.character(i),as.character(d_SMR$Ch)))){
				bw.val<-BW.animal[i]
				ID<-AnimalID[i]
				nameCh<-paste("Ch",i,sep="")
				resp.Vol<-resp.V[i]
				n.row<-which(d_SMR$Ch==nameCh)
				d_SMR$bw[n.row]<-bw.val
				d_SMR$ID[n.row]<-as.character(ID)
				d_SMR$resp.V[n.row]<-resp.Vol
			}
		}
		
		
    ## this is where lines indicated with NA in the individual animal argument are ditched
#     d_SMR<-d_SMR[d_SMR$ID!="NA",]
#     d_MMR<-d_MMR[d_MMR$ID!="NA",]
#     
# 		d_MMR<-d_MMR[as.character(d_MMR$ID)!="blank",]
# 		d_SMR<-d_SMR[as.character(d_SMR$ID)!="blank",]
# 		
#     d_SMR$ID<-factor(d_SMR$ID)
#     d_MMR$ID<-factor(d_MMR$ID)

    
		
		
		# **********************************************
    # START -- >>> background corrections SMR 
		# Jan 5 2020 change - account for background using linear regression 
    # making predictions 

		d_SMR$DateTime_start<-as.character(gsub("(", "", d_SMR$DateTime_start, fixed=TRUE))
  	d_SMR$DateTime_start<-as.character(gsub(")", "", d_SMR$DateTime_start, fixed=TRUE))

  	if (date_format== "m/d/y"){
	     d_SMR$DateTime_start<- strptime(d_SMR$DateTime_start, format="%m/%d/%y %H:%M:%OS")
  	}
  	if (date_format== "d/m/y"){
	     d_SMR$DateTime_start<- strptime(d_SMR$DateTime_start, format="%d/%m/%y %H:%M:%OS")
  	}
  	if (date_format== "y-m-d"){
      # DateTime<- chron(dates=back_$date,times=back_$time,format=c('y-m-d','h:m:s'))
	     d_SMR$DateTime_start<- strptime(d_SMR$DateTime_start, format="%y-%m-%d %H:%M:%OS")
  	}
		 
		if(background_linear_gr==TRUE & match_background_Ch==FALSE){
        background_slopes<-data.frame(DateTime_start = d_SMR$DateTime_start)
        background_slopes$back_m<-predict(back_regression, background_slopes)

  	   for (i in 1:nrow(d_SMR)){
  	    d_SMR$mo2[i]<-(( d_SMR$m[i]*(d_SMR$resp.V[i]-d_SMR$bw[i])) - (background_slopes$back_m[i] * d_SMR$resp.V[i])) /(d_SMR$bw[i])#^scaling_exponent_smr) # units mgO2 kg-1 min-1 -CORRECTED for back resp
  	   }
     
      background_slopes[,3:4]<-d_SMR[,c(5,9)]
      
      gannotations <- data.frame(
        xpos = c(background_slopes$DateTime_start[1]),
        ypos =  c(max(background_slopes$back_m)+0.5),
        annotateText = c("Used sections of predicted background: \n per L vol"),
        hjustvar = c(0,) ,
        vjustvar = c(1.0))
      
      background_slope_plot<-ggplot(data=back_all, aes(x=DateTime_start, y=m, colour=Ch, group=Ch))+
        geom_point()+
        geom_point(data= background_slopes, mapping = aes(x = DateTime_start, back_m), color = "grey30", size=1, pch=21)+
        geom_line(data= background_slopes, mapping = aes(x = DateTime_start, back_m), color = "grey30", size=1)+
        geom_text(data = gannotations, aes(x=xpos,y=ypos,hjust=hjustvar,
                                           vjust=vjustvar,label=annotateText), color="black")+
        # geom_point(aes(x=DateTime_start, back_m), colour="black", fill="grey", alpha=0.5, pch=19, size=1)+
        facet_grid(Ch~.)+
        # geom_smooth(method="lm", se=FALSE)+
        theme_bw()+
        ylab("Background respiration - mgO2/min/L")+
        theme(axis.text.x = element_text(angle = 45))+
        facet_grid(Ch~.)
      
		  
		  png(plotname.backgr, width=4, height=8, res=300, units="in")
		    print(background_slope_plot)
		  dev.off()
		  
		  d_SMR$background_slope<- background_slopes$back_m

		}
  	
		if(background_linear_gr==TRUE & match_background_Ch==TRUE){
		  
		  message("SMR/RMR corrected for background: using Ch specific average background")
		  background_slopes<-data.frame(DateTime_start = d_SMR$DateTime_start)
		  
		  # print(str(d_SMR))

		  
       for (i in 1:nrow(d_SMR)){
        
		   # print(c(d_SMR$bw[1],d_SMR$Ch[i]))
		    if(substr(as.character(d_SMR$Ch[i]), start=3, stop=3) == "1"){
		      background_slopes$back_m[i]<-predict(back_regression1, data.frame(DateTime_start = d_SMR$DateTime_start[i]))
          back_m1<-predict(back_regression1, data.frame(DateTime_start = d_SMR$DateTime_start[i]))
  		    d_SMR$mo2[i]<-(( d_SMR$m[i]*(d_SMR$resp.V[i]-d_SMR$bw[i])) - (back_m1 * d_SMR$resp.V[i])) /(d_SMR$bw[i])#^scaling_exponent_smr) # units mgO2 kg-1 min-1 -CORRECTED for back resp
		    # message("Channel: 1,  bacterial respiration slope: ", back_m1)
		    }
		    
		    if(substr(as.character(d_SMR$Ch[i]), start=3, stop=3) == "2"){
		      background_slopes$back_m[i]<-predict(back_regression2, data.frame(DateTime_start = d_SMR$DateTime_start[i]))
		      back_m2<-predict(back_regression2, data.frame(DateTime_start = d_SMR$DateTime_start[i]))
		      d_SMR$mo2[i]<-(( d_SMR$m[i]*(d_SMR$resp.V[i]-d_SMR$bw[i])) - (back_m2 * d_SMR$resp.V[i])) /(d_SMR$bw[i])#^scaling_exponent_smr) # units mgO2 kg-1 min-1 -CORRECTED for back resp
		      # message("Channel: 2,  bacterial respiration slope: ", back_m2)
		    }
		    
		    if(substr(as.character(d_SMR$Ch[i]), start=3, stop=3) == "3"){
		      background_slopes$back_m[i]<-predict(back_regression3, data.frame(DateTime_start = d_SMR$DateTime_start[i]))
		      back_m3<-predict(back_regression3, data.frame(DateTime_start = d_SMR$DateTime_start[i]))
		      d_SMR$mo2[i]<-(( d_SMR$m[i]*(d_SMR$resp.V[i]-d_SMR$bw[i])) - (back_m3 * d_SMR$resp.V[i])) /(d_SMR$bw[i]^scaling_exponent_smr) # units mgO2 kg-1 min-1 -CORRECTED for back resp
		      # message("Channel: 3,  bacterial respiration slope: ", back_m3)
		    }
		    
		    if(substr(as.character(d_SMR$Ch[i]), start=3, stop=3) == "4"){
          background_slopes$back_m[i]<-predict(back_regression4, data.frame(DateTime_start = d_SMR$DateTime_start[i]))
		      back_m4<-predict(back_regression4, data.frame(DateTime_start = d_SMR$DateTime_start[i]))
		      d_SMR$mo2[i]<-(( d_SMR$m[i]*(d_SMR$resp.V[i]-d_SMR$bw[i])) - (back_m4 * d_SMR$resp.V[i])) /(d_SMR$bw[i])#^scaling_exponent_smr) # units mgO2 kg-1 min-1 -CORRECTED for back resp
		      # message("Channel: 4,  bacterial respiration slope: ", back_m4)
		    }
		  }# end of the loop 
		  
		  background_slopes[,3:4]<-d_SMR[,c(5,9)]
		  
		  gannotations <- data.frame(
		    xpos = c(background_slopes$DateTime_start[1]),
		    ypos =  c(Inf),
		    annotateText = c("Predicted slopes for back: \n per L vol"),
		    hjustvar = c(0) ,
		    vjustvar = c(1.0))
		  
  		  background_slope_plot<-ggplot(data=back_all, aes(x=DateTime_start, y=m, colour=Ch, group=Ch))+
  		    geom_point()+
  		    geom_point(data= background_slopes, mapping = aes(x = DateTime_start, back_m), color = "grey30", size=1, pch=21)+
  		    geom_line(data= background_slopes, mapping = aes(x = DateTime_start, back_m), color = "grey30", size=1)+
  		    geom_text(data = gannotations, aes(x=xpos,y=ypos,hjust=hjustvar,
  		                                       vjust=vjustvar,label=annotateText), color="black")+
  		    # geom_point(aes(x=DateTime_start, back_m), colour="black", fill="grey", alpha=0.5, pch=19, size=1)+
  		    facet_grid(Ch~.)+
  		    # geom_smooth(method="lm", se=FALSE)+
  		    theme_bw()+
  		    ylab("Regression slope value- mgO2/min/L")+
  		    theme(axis.text.x = element_text(angle = 45))+
  		    facet_grid(Ch~.)
		  
		  
		  # WORK - SAVE this plot
		  # plotname.backgr<-paste( filename.SMR,"_PLOT_BACKGROUND.png", sep="")	
		  png(plotname.backgr, width=4, height=8, res=300, units="in")
		    print(background_slope_plot)
		  dev.off()
		  
		  d_SMR$background_slope<- background_slopes$back_m
		}
	
		# 1.1 if background files (either prior or post, or both) are provided and its one overall mean value (back_m)
		if ((( !is.na(background_post) | !is.na(background_prior)) & match_background_Ch==FALSE) & is.null(background_slope) & background_linear_gr==FALSE){
		  message("SMR/RMR corrected for background: used a mean (prior and/or post) background measurements | mean bacterial respiration slope: ", back_m, " ~" , round((back_m*100)/(mean(d_SMR[d_SMR$m <= quantile(d_SMR$m, 0.5, na.rm=TRUE), "m"], na.rm=TRUE)), 2), " %")

		  for (i in 1:nrow(d_SMR)){
		    d_SMR$mo2[i]<-(( d_SMR$m[i]*(d_SMR$resp.V[i]-d_SMR$bw[i])) - (back_m * d_SMR$resp.V[i])) /(d_SMR$bw[i])#^scaling_exponent_smr) # units mgO2 kg-1 min-1 -CORRECTED for back resp
		  }
		  
		  d_SMR$background_slope<- back_m
		}	
		
		## if background slope and volume are specifically provided, then use those! this alos overrides the background prior and post argument. 
		# all channels with the same slope 
		if (!is.null(background_slope)){
		  message("SMR/RMR corrected for background: used a common manually provided background slope for all channels | bacterial respiration slope: ", background_slope, " ~" , round((background_slope*100)/(mean(d_SMR[d_SMR$m <= quantile(d_SMR$m, 0.5, na.rm=TRUE), "m"], na.rm=TRUE)), 2), " %")
		  for (i in 1:nrow(d_SMR)){
		    d_SMR$mo2[i]<-(( d_SMR$m[i]*(d_SMR$resp.V[i]-d_SMR$bw[i])) - (background_slope * background.V)) /(d_SMR$bw[i])#^scaling_exponent_smr) # units mgO2 kg-1 min-1 -CORRECTED for back resp
		  }
		  d_SMR$background_slope<- paste(round(background_slope,2), "_BackVol=", background.V, sep="")
		}	
		
		# 1.2 if background files are provided and its channel specific 
		if ((!is.na(background_post) | !is.na(background_prior)) & match_background_Ch==TRUE & background_linear_gr==FALSE){
		  message("SMR/RMR: corrected for background: using Ch specific average background")
		
       for (i in 1:nrow(d_SMR)){

		   # print(c(d_SMR$bw[1],d_SMR$Ch[i]))
		    if(substr(d_SMR$Ch[i], start=3, stop=3) == "1"){
  		    d_SMR$mo2[i]<-(( d_SMR$m[i]*(d_SMR$resp.V[i]-d_SMR$bw[i])) - (back_m1 * d_SMR$resp.V[i])) /(d_SMR$bw[i])#^scaling_exponent_smr) # units mgO2 kg-1 min-1 -CORRECTED for back resp
  		    # message("Channel: 1,  bacterial respiration slope: ", back_m1)
  		    d_SMR$background_slope[i]<-back_m1
		    }
		    
		    if(substr(d_SMR$Ch[i], start=3, stop=3) == "2"){
		      d_SMR$mo2[i]<-(( d_SMR$m[i]*(d_SMR$resp.V[i]-d_SMR$bw[i])) - (back_m2 * d_SMR$resp.V[i])) /(d_SMR$bw[i])#^scaling_exponent_smr)# units mgO2 kg-1 min-1 -CORRECTED for back resp
		      # message("Channel: 2,  bacterial respiration slope: ", back_m2)
		      d_SMR$background_slope[i]<-back_m2
		    }
		    
		    if(substr(d_SMR$Ch[i], start=3, stop=3) == "3"){
		      d_SMR$mo2[i]<-(( d_SMR$m[i]*(d_SMR$resp.V[i]-d_SMR$bw[i])) - (back_m3 * d_SMR$resp.V[i])) /(d_SMR$bw[i])#^scaling_exponent_smr) # units mgO2 kg-1 min-1 -CORRECTED for back resp
		      # message("Channel: 3,  bacterial respiration slope: ", back_m3)
		      d_SMR$background_slope[i]<-back_m3
		    }
		    
		    if(substr(d_SMR$Ch[i], start=3, stop=3) == "4"){
		      d_SMR$mo2[i]<-(( d_SMR$m[i]*(d_SMR$resp.V[i]-d_SMR$bw[i])) - (back_m4 * d_SMR$resp.V[i])) /(d_SMR$bw[i])#^scaling_exponent_smr) # units mgO2 kg-1 min-1 -CORRECTED for back resp
		      # message("Channel: 4,  bacterial respiration slope: ", back_m4)
		      d_SMR$background_slope[i]<-back_m4
		    }
		  }# end of the loop 
		  
		}	
		
		# 2. if background files are not provided 
		if ((is.na(background_post) & is.na(background_prior)) & is.null(background_slope)){
		  message("SMR/RMR: NO correction for background respiration")

  		for (i in 1:nrow(d_SMR)){
  			d_SMR$mo2[i]<-d_SMR$m[i]*(d_SMR$resp.V[i]-d_SMR$bw[i])/(d_SMR$bw[i])#^scaling_exponent_smr) # units mgO2 kg-1 min-1
  		}
		}	
		## end of SMR corrections for body size and background 
    # END -- >>> background corrections SMR    	
		# **********************************************
	
  	
  	
  	
  	# size correct each value for SMR calculations, lead with this into EPOC calculations
		d_SMR$mo2_individual_kg<-d_SMR$mo2 * d_SMR$bw
		d_SMR$mo2_common_mass_kg<-exp( (scaling_exponent_smr*(log(d_SMR$common_mass)-log(d_SMR$bw))) + log(d_SMR$mo2_individual_kg))

    # ScalingSlope = (y2 - y1) / (x2 - x1) 
    # y2 = log (mo2) <<< current mass dependent value 
    # y1 = unknown <<< solve for; this will be log() values! use (exp()) to backtransform
    # x2 = log (mass kg) <<< current body mass in kg
    # x1 = log (common_mass) <<< common body mass for the correction 

    # solve for  
    # y1 = y2 - ( ScalingSlope * (x2 - x1) )
		# or solve for y2:
		# y2 = ScalingSlope * (x2 - x1) + y1
	  d_SMR$mo2_common_mass_kg <- exp( log(d_SMR$mo2_individual_kg) - (scaling_exponent_smr * (log(d_SMR$bw) - log(d_SMR$common_mass)) ) )
		# b <- exp( (scaling_exponent_smr * ( log(d_SMR$common_mass) - log(d_SMR$bw) ) ) + log(d_SMR$mo2_individual_kg))
		
	  # rename the mo2 to indicate that this is per kg fish 
	  names(d_SMR)[names(d_SMR) == 'mo2'] <- 'mo2_1kg'

    d_SMR$mo2<-d_SMR[,mo2_val_for_calc]
    
    if(mo2_val_for_calc == "mo2_common_mass_kg"){
       mo2_lab<-bquote(MO[2]~(mgO[2]~min^-1~ .(common_mass) ~ kg^ ~ - .(scaling_exponent_smr)))
       message(paste("Metabolic performance units: MR/min/ ", common_mass, "kg", sep=""))
      }else{
        if(mo2_val_for_calc == "mo2_individual_kg"){
            mo2_lab<-bquote(MO[2]~(mgO[2]~min^-1~per~individual))
            message(paste("Metabolic performance units: MR/min/animal"))

          }else{
          	mo2_lab<-bquote(MO[2]~(mgO[2]~min^-1~ kg^-1))
          	message(paste("Metabolic performance units: MR/min/kg"))
        }
    }

		# find SMR estimates using all methods provided by Chabot 
    # --- #
		# 1. frequency plot
		d_SMR$DateTime_start<-as.character(d_SMR$DateTime_start)

		# p_freq<-ggplot(d_SMR, aes(x=mo2))+
		# geom_histogram(bins = 60 , color = "black", fill = "gray") +
		# facet_grid(Ch~.)+
		# theme_classic()+
		# ggtitle(bquote(Datafile: ~ .(data.SMR)))+
		# xlab(mo2_lab)
				 
		# the lowest 10 values after removal of 5 lowest
		a0<-d_SMR[,2:ncol(d_SMR)] %>%
			group_by(Ch)%>%
			top_n(-15, mo2)%>%
			arrange(Ch,mo2)
			
		# the 5 lowest/ excluded	
		a00<-a0 %>%
			group_by(Ch)%>%
			top_n(-5, mo2)%>%
			arrange(Ch,mo2)	
		
		# the 10 lowest/ excluding the 5 lowest in the df	
		a<-a0 %>%
			group_by(Ch)%>%
			top_n(10, mo2)%>%
			arrange(Ch,mo2)
		
		min10_MO2<-as.data.frame(a)
		
		# detach(package:plyr)
		min10_mean<-min10_MO2%>%
		  group_by(Ch)%>%
			summarize(mean_temp=mean(t_mean), sd_temp=sd(t_mean), mean_mo2=mean(mo2), sd_mo2=sd(mo2), 
			cv_mo2 = sd(mo2)/(sqrt(10)), n=length(mo2))
		
		min10_plot<-ggplot(data=d_SMR, aes(x=min_start, y=mo2))+
			geom_point(size=1)+
			geom_point(data=a00, aes(x=min_start, y=mo2), color="red", pch=19, size=3)+
			geom_point(data=min10_MO2, aes(x=min_start, y=mo2), colour="green4",size=3, alpha=0.7)+
			geom_line(size=0.5, alpha=0.)+
		  theme_light()+
		  ggtitle("Lowest 10 values, excluding the 5 lowest values")+
			theme(legend.position="top")+
			facet_grid(Ch~.)+
		  ylab(mo2_lab)+
		  xlab("Time in trial (min)")

		
		a2<-as.data.frame(matrix(ncol=3, nrow=0))
    
		for (i in unique(d_SMR$Ch)){
		  split_temp<-as.data.frame(split(d_SMR, d_SMR$Ch)[i])
      colnames(split_temp)<-colnames(d_SMR)
      quant10<-quantile(split_temp$mo2, 0.1, na.rm=FALSE)
      quant15<-quantile(split_temp$mo2, 0.15, na.rm=FALSE)
      quant20<-quantile(split_temp$mo2, 0.2, na.rm=FALSE)
      
      # colnames(a2)<-c("Ch","quantiles", "mo2_perc")
      a2<-rbind(a2,as.data.frame(t(c(as.character(split_temp$Ch[1]), "10%", as.numeric(quant10)))))
      # colnames(a2)<-c("Ch","quantiles", "mo2_perc")
      a2<-rbind(a2,as.data.frame(t(c(as.character(split_temp$Ch[1]), "15%", as.numeric(quant15)))))
      # colnames(a2)<-c("Ch","quantiles", "mo2_perc")
      a2<-rbind(a2,as.data.frame(t(c(as.character(split_temp$Ch[1]), "20%", as.numeric(quant20)))))
    }
    
		# UPDATE 
    colnames(a2)<-c("Ch","quantiles", "mo2_perc")
    a2$mo2_perc<-as.numeric(as.character( a2$mo2_perc))
		
		quantile_smr<-spread(a2, key=quantiles, value=mo2_perc)
		a2$quantiles<-as.factor(a2$quantiles)
		
		row_ch1_10perc<-which(a2$Ch=="Ch1" & a2$quantiles=="10%")
		row_ch2_10perc<-which(a2$Ch=="Ch2" & a2$quantiles=="10%")
		row_ch3_10perc<-which(a2$Ch=="Ch3" & a2$quantiles=="10%")
		row_ch4_10perc<-which(a2$Ch=="Ch4" & a2$quantiles=="10%")
		
		row_ch1_15perc<-which(a2$Ch=="Ch1" & a2$quantiles=="15%")
		row_ch2_15perc<-which(a2$Ch=="Ch2" & a2$quantiles=="15%")
		row_ch3_15perc<-which(a2$Ch=="Ch3" & a2$quantiles=="15%")
		row_ch4_15perc<-which(a2$Ch=="Ch4" & a2$quantiles=="15%")
		
		row_ch1_20perc<-which(a2$Ch=="Ch1" & a2$quantiles=="20%")
		row_ch2_20perc<-which(a2$Ch=="Ch2" & a2$quantiles=="20%")
		row_ch3_20perc<-which(a2$Ch=="Ch3" & a2$quantiles=="20%")
		row_ch4_20perc<-which(a2$Ch=="Ch4" & a2$quantiles=="20%")
			
		min_percPlot<-ggplot(data=d_SMR, aes(x=min_start, y=mo2))+
			geom_point(size=1)+
		  geom_point(data=d_SMR[which(d_SMR$Ch=="Ch1" & d_SMR$mo2<=a2$mo2_perc[row_ch1_20perc]),], aes(x=min_start, y=mo2), colour="purple",size=3)+
		  geom_point(data=d_SMR[which(d_SMR$Ch=="Ch2" & d_SMR$mo2<=a2$mo2_perc[row_ch2_20perc]),], aes(x=min_start, y=mo2), colour="purple",size=3)+
		  geom_point(data=d_SMR[which(d_SMR$Ch=="Ch3" & d_SMR$mo2<=a2$mo2_perc[row_ch3_20perc]),], aes(x=min_start, y=mo2), colour="purple",size=3)+
		  geom_point(data=d_SMR[which(d_SMR$Ch=="Ch4" & d_SMR$mo2<=a2$mo2_perc[row_ch4_20perc]),], aes(x=min_start, y=mo2), colour="purple",size=3)+
		  geom_point(data=d_SMR[which(d_SMR$Ch=="Ch1" & d_SMR$mo2<=a2$mo2_perc[row_ch1_15perc]),], aes(x=min_start, y=mo2), colour="blue",size=3)+
		  geom_point(data=d_SMR[which(d_SMR$Ch=="Ch2" & d_SMR$mo2<=a2$mo2_perc[row_ch2_15perc]),], aes(x=min_start, y=mo2), colour="blue",size=3)+
		  geom_point(data=d_SMR[which(d_SMR$Ch=="Ch3" & d_SMR$mo2<=a2$mo2_perc[row_ch3_15perc]),], aes(x=min_start, y=mo2), colour="blue",size=3)+
		  geom_point(data=d_SMR[which(d_SMR$Ch=="Ch4" & d_SMR$mo2<=a2$mo2_perc[row_ch4_15perc]),], aes(x=min_start, y=mo2), colour="blue",size=3)+
		  geom_point(data=d_SMR[which(d_SMR$Ch=="Ch1" & d_SMR$mo2<=a2$mo2_perc[row_ch1_10perc]),], aes(x=min_start, y=mo2), colour="darkturquoise",size=3)+
		  geom_point(data=d_SMR[which(d_SMR$Ch=="Ch2" & d_SMR$mo2<=a2$mo2_perc[row_ch2_10perc]),], aes(x=min_start, y=mo2), colour="darkturquoise",size=3)+
		  geom_point(data=d_SMR[which(d_SMR$Ch=="Ch3" & d_SMR$mo2<=a2$mo2_perc[row_ch3_10perc]),], aes(x=min_start, y=mo2), colour="darkturquoise",size=3)+
		  geom_point(data=d_SMR[which(d_SMR$Ch=="Ch4" & d_SMR$mo2<=a2$mo2_perc[row_ch4_10perc]),], aes(x=min_start, y=mo2), colour="darkturquoise",size=3)+
		  # geom_p# geom_line(size=0.5, alpha=0.5)+
			theme_light()+
			# ggtitle("10th percentile")+
			theme(legend.position="top")+
		  ylab(mo2_lab)+
		  xlab("Time in trial (min)")+
			facet_grid(Ch~.)
			
		# min15_percPlot<-ggplot(data=d_SMR, aes(x=min_start, y=mo2))+
		# 	geom_point(size=1)+
		# 	geom_point(data=d_SMR[which(d_SMR$Ch=="Ch1" & d_SMR$mo2<=a2$mo2_perc[row_ch1_15perc]),], aes(x=min_start, y=mo2), colour="darkturquoise",size=3)+
		# 	geom_point(data=d_SMR[which(d_SMR$Ch=="Ch2" & d_SMR$mo2<=a2$mo2_perc[row_ch2_15perc]),], aes(x=min_start, y=mo2), colour="darkturquoise",size=3)+
		# 	geom_point(data=d_SMR[which(d_SMR$Ch=="Ch3" & d_SMR$mo2<=a2$mo2_perc[row_ch3_15perc]),], aes(x=min_start, y=mo2), colour="darkturquoise",size=3)+
		# 	geom_point(data=d_SMR[which(d_SMR$Ch=="Ch4" & d_SMR$mo2<=a2$mo2_perc[row_ch4_15perc]),], aes(x=min_start, y=mo2), colour="darkturquoise",size=3)+
		# 	# geom_line(size=0.5, alpha=0.5)+
		# 	theme_classic()+
		# 	ggtitle("15th percentile")+
		# 	theme(legend.position="top")+
		#   ylab(mo2_lab)+
		#   xlab("Time in trial (min)")+
		# 	facet_grid(Ch~.)
		# 	
		# min20_percPlot<-ggplot(data=d_SMR, aes(x=min_start, y=mo2))+
		# 	geom_point(size=1)+
		# 	geom_point(data=d_SMR[which(d_SMR$Ch=="Ch1" & d_SMR$mo2<=a2$mo2_perc[row_ch1_20perc]),], aes(x=min_start, y=mo2), colour="deepskyblue2",size=3)+
		# 	geom_point(data=d_SMR[which(d_SMR$Ch=="Ch2" & d_SMR$mo2<=a2$mo2_perc[row_ch2_20perc]),], aes(x=min_start, y=mo2), colour="deepskyblue2",size=3)+
		# 	geom_point(data=d_SMR[which(d_SMR$Ch=="Ch3" & d_SMR$mo2<=a2$mo2_perc[row_ch3_20perc]),], aes(x=min_start, y=mo2), colour="deepskyblue2",size=3)+
		# 	geom_point(data=d_SMR[which(d_SMR$Ch=="Ch4" & d_SMR$mo2<=a2$mo2_perc[row_ch4_20perc]),], aes(x=min_start, y=mo2), colour="deepskyblue2",size=3)+
		# 	# geom_line(size=0.5, alpha=0.5)+
		# 	theme_classic()+
		# 	ggtitle("20th percentile")+
		# 	theme(legend.position="top")+
		#   ylab(mo2_lab)+
		#   xlab("Time in trial (min)")+
		# 	facet_grid(Ch~.)
		# 
#     if(plot_smr_quantile==10){
# 		  percentile_plot<-min10_percPlot
# 		  }else{
# 		    if(plot_smr_quantile==15){
# 		      percentile_plot<-min15_percPlot
# 		    }else{
# 		      percentile_plot<-min20_percPlot}
# 		}	
		
		png(plotname.freq, width = 10, height = 10, units="in", res=200)
			grid.arrange( min10_plot, min_percPlot, ncol=1, nrow=2)
		dev.off()
		
		
		
		# MLND algorithm from suppl JFB issue 88 Chabot, Steffensen, and Farrell 2016
		# SMR and MMR data frames split based on the channel
		if (length(unique(d_SMR$Ch))>1){
			Ch.data.smr<-split(d_SMR, d_SMR$Ch)
			 	if (data.MMR!="none"){
		    	Ch.data.mmr<-split(d_MMR, d_MMR$Ch)
		  	}
		}else{
			Ch.data.smr<-1
		  	if (data.MMR!="none"){
		    	Ch.data.mmr<-1
		  	}
		}
		
		# MLND calculation loop
		for(i in 1:length(unique(d_SMR$Ch))){
  
  		if (length(unique(d_SMR$Ch))==1){
  			Y0<-d_SMR
  		}else{
  			Y0<-as.data.frame(Ch.data.smr[i])
  		}
  		
			
  		colnames(Y0)<-c("time_frame","min_start", "r2" ,"b", "m" , "t_min", "t_max", "t_mean", "Ch", "DateTime_start","mo2_type","n_min", "ID_code","bw", "mo2","ID")
  					
  		mo2<-Y0$mo2
  		BW<-Y0$bw[1]
  		ID<-Y0$ID[1]
  		t_min<-min(Y0$t_min) # min of min temp values
  		t_max<-max(Y0$t_max) # max of max temp values
  		t_mean<-mean(Y0$t_mean) # mean of mean temp values
  		N_mo2<-length(mo2)
  
  		Y.Ch<-as.character(Y0$Ch[1])
  		if (path == "."){
        plotname.mlnd<-paste( filename.SMR,"_", Y.Ch, "_MLND_SMR_analysed.png", sep="")	
  
    	}else{
        plotname.mlnd<-paste("../plots_mlnd_SMR/", filename.SMR,"_", Y.Ch, "_MLND_SMR_analysed.png", sep="")	
    	}
  		
    		
    # START -->>> MLND 
		# loop for MLND function 	
    if(MLND==TRUE){

    		mlnd.m1<-Mclust(mo2, G=1:4)
    		m1<-densityMclust(mo2, verbose=TRUE) # same as mlnd
    		clas.m1<-m1$classification # 
    		clas.m1.t <- as.data.frame(table(clas.m1))
    		clas.m1.t$clas.m1 <- as.numeric(levels(clas.m1.t$clas.m1))
    		boot1 <- MclustBootstrap(m1, nboot = 1000, type = "bs")
    		
    		
    		valid <- clas.m1.t$Freq>=0.1*length(mo2)   # assuming that the lower class contains 10% of the MO2 values recorded
    			
    		if(max(clas.m1)==1 ){
    		
    			message("ONLY ONE MLND")
    			if (valid[1]){ 
    					message("MLND class 1 = valid")
    				valid.clas<-1
    				}
    			png(plotname.mlnd, width = 12, height = 9, units="in", res=200)
    				par(mfrow=c(3,4))
    #~ 					plot(m1, what= "BIC")
    				plot(mlnd.m1, what="classification")
    				plot(m1, what = "density", data = mo2, breaks = 30)
    				plot(m1, what = "diagnostic", type = "cdf")
    				plot(m1, what = "diagnostic", type = "qq")
    				plot(Y0$mo2~Y0$min_start, cex=0.5)
    				points(Y0$mo2[which(clas.m1==1)]~Y0$min_start[which(clas.m1==1)], col="red", cex=1)
    #~ 				plot(boot1, what = "mean", show.confint=TRUE)	
    				plot(1, type="n", axes=F, xlab="", ylab="");
    				text(1, 1, Y.Ch ,cex = 3)
    #~ 				plot(boot1, what = "pro")
    			dev.off()
    			
    		}else{
    		
    			if (!is.na(boot1$variance[1])){
    				sum.boot1<-summary(boot1, what="ci")
    			}
    				
    			if(max(clas.m1)==2){
    				if (valid[1]){
    						message("MLND class 1 = valid")
    					valid.clas<-1
    					CIup.mlnd<-sum.boot1$mean[2]
    					CIlo.mlnd<-sum.boot1$mean[1]
    
    				}else{
    					valid.clas<- min(clas.m1.t$clas.m1[valid])
    					message(paste("MLND lowest valid class = ", valid.clas, sep=""))
    					
    					if(valid.clas==2){
    						CIup.mlnd<-sum.boot1$mean[4]
    						CIlo.mlnd<-sum.boot1$mean[3]
    					}
    				}
    					
    				png(plotname.mlnd, width = 12, height = 9, units="in", res=200)
    					par(mfrow=c(3,4))
    #~ 					plot(m1, what= "BIC")
    					plot(mlnd.m1, what="classification")
    					plot(m1, what = "density", data = mo2, breaks = 30)
    					plot(m1, what = "diagnostic", type = "cdf")
    					plot(m1, what = "diagnostic", type = "qq")
    					plot(Y0$mo2~Y0$min_start, cex=0.5)
    						points(Y0$mo2[which(clas.m1==1)]~Y0$min_start[which(clas.m1==1)], col="red", cex=1)
    					plot(boot1, what = "mean", show.confint=TRUE)	
    					plot(1, type="n", axes=F, xlab="", ylab="");
    						text(1, 1, "" ,cex = 3)
    					plot(1, type="n", axes=F, xlab="", ylab="");
    						text(1, 1, Y.Ch ,cex = 3)
    					plot(boot1, what = "pro")
    				dev.off()
    							
    			}
    				
    			if(max(clas.m1)>=3){
    				if (valid[1]){
    					message("MLND class 1 = valid")
    					valid.clas<-1
    					CIup.mlnd<-sum.boot1$mean[2]
    					CIlo.mlnd<-sum.boot1$mean[1]
    
    				}else{
    					valid.clas<- min(clas.m1.t$clas.m1[valid])
    					message(paste("MLND lowest valid class = ", valid.clas, sep=""))
    					
    					if(valid.clas==2){
    						CIup.mlnd<-sum.boot1$mean[4]
    						CIlo.mlnd<-sum.boot1$mean[3]
    					}else{
    						CIup.mlnd<-sum.boot1$mean[6]
    						CIlo.mlnd<-sum.boot1$mean[5]
    					}
    				}
    				
    				
    				png(plotname.mlnd, width = 12, height = 12, units="in", res=200)
    					par(mfrow=c(4,4))
    	#~ 					plot(m1, what= "BIC")
    					plot(mlnd.m1, what="classification")
    					plot(m1, what = "density", data = mo2, breaks = 30)
    					
    					if (!max(clas.m1)>3){
    						  plot(m1, what = "diagnostic", type = "cdf")
    					  	plot(m1, what = "diagnostic", type = "qq")
    						}else{
    						  message("More than 3 MLND classes, no diagnostic plots")
    					}
    		
    					plot(Y0$mo2~Y0$min_start, cex=0.5)
    						points(Y0$mo2[which(clas.m1==1)]~Y0$min_start[which(clas.m1==1)], col="red", cex=1)
    					plot(boot1, what = "mean", show.confint=TRUE)	
    					plot(1, type="n", axes=F, xlab="", ylab="");
    						text(1, 1, Y.Ch ,cex = 3)
    					plot(boot1, what = "pro")
    				dev.off()
    						
    			}
    		
    		}
    				
    		distr <- mo2[m1$classification==valid.clas]
    		mlnd <- m1$parameters$mean[valid.clas]
    		CVmlnd <- sd(distr)/mlnd * 100
    		Nmlnd<- length(distr)
  	
  	 }else{ # end of if(MLND=TRUE){
  	    
  	    if(substr(Y.Ch, start=3, stop=3)=="1"){
  	      message("MLND not calculated")
  	    }
  	    distr <- NA
    		mlnd <- 0
    		CVmlnd <- 0
    		Nmlnd <- 0
    		
  	 }
  		
    # END -- >>> MNLD
		# https://cran.r-project.org/web/packages/mclust/vignettes/mclust.html#classification
		# mclust: R package for model-based clustering, classification, and density estimation based on finite normal mixture modelling. It provides functions for parameter estimation via the EM algorithm for normal mixture models with a variety of covariance structures, and functions for simulation from these models.
			
		# read about EM http://www.statisticshowto.com/em-algorithm-expectation-maximization/
  		mmr_overall<-max(mo2)
  		AS_smr_mean10minVal_overall <- mmr_overall - min10_mean$mean_mo2[i]
  		AS_SMR_low10quant_overall <- mmr_overall - quantile_smr[i,2]
  		AS_SMR_low15quant_overall <- mmr_overall - quantile_smr[i,3]
  		AS_SMR_low20quant_overall <- mmr_overall - quantile_smr[i,4]
  		AS_smr_mlnd_overall <- mmr_overall - mlnd
  		
    	values_smr<-as.data.frame(t(c(filename.SMR, ID, Y.Ch, BW, t_min, t_max, t_mean, N_mo2,
  		min10_mean$mean_mo2[i], min10_mean$sd_mo2[i], min10_mean$cv_mo2[i], quantile_smr[i,2],  quantile_smr[i,3], quantile_smr[i,4],   
  		mlnd, CVmlnd, Nmlnd,
  		NA, mmr_overall,
  		NA, NA, NA, NA, NA,
  		AS_smr_mean10minVal_overall, AS_SMR_low10quant_overall, AS_SMR_low15quant_overall, AS_SMR_low20quant_overall, AS_smr_mlnd_overall, #5
  		NA, scaling_exponent_mmr, scaling_exponent_smr, common_mass)))#13
  		
  		colnames(values_smr)<-c("filename", "ID", "Ch", "BW","t_min","t_max", "t_mean", "N_mo2", #8
    	"smr_mean10minVal","smr_SD10minVal", "smr_CV10minVal", "SMR_low10quant","SMR_low15quant","SMR_low20quant", #6
    	"smr_mlnd", "smr_CVmlnd", "smr_Nmlnd", #3
    	"mmr", "mmr_overall", #2
    	"AS_smr_mean10minVal", "AS_SMR_low10quant", "AS_SMR_low15quant", "AS_SMR_low20quant", "AS_smr_mlnd", #5
    	"AS_smr_mean10minVal_overall", "AS_SMR_low10quant_overall", "AS_SMR_low15quant_overall", "AS_SMR_low20quant_overall", "AS_smr_mlnd_overall", #5
    	"mmr_length_cycle", "scaling_exponent_mmr", "scaling_exponent_smr", "common_mass")#1
  		newdata.smr<-rbind(newdata.smr, values_smr)
    		
		}# end of the loop for which MLND is a part of 
	
	
		
  	pd<-as.data.frame(newdata.smr[,c(3,9, 12,13,14,15, 8, 17)])
  	
  	
		df.1 <- data.frame(Ch=unlist(pd[,1], use.names=FALSE), smr_val=unlist(pd[,2], use.names=FALSE), smr_method="smr_mean10minVal", N_mo2=unlist(pd[,7], use.names=FALSE))
		df.2 <- data.frame(Ch=unlist(pd[,1], use.names=FALSE), smr_val=unlist(pd[,3], use.names=FALSE), smr_method="SMR_low10quant", N_mo2=unlist(pd[,7], use.names=FALSE))
		df.3 <- data.frame(Ch=unlist(pd[,1], use.names=FALSE), smr_val=unlist(pd[,4], use.names=FALSE), smr_method="SMR_low15quant", N_mo2=unlist(pd[,7], use.names=FALSE))
		df.4 <- data.frame(Ch=unlist(pd[,1], use.names=FALSE), smr_val=unlist(pd[,5], use.names=FALSE), smr_method="SMR_low20quant", N_mo2=unlist(pd[,7], use.names=FALSE))
		df.5 <- data.frame(Ch=unlist(pd[,1], use.names=FALSE), smr_val=unlist(pd[,6], use.names=FALSE), smr_method="smr_mlnd", N_mo2=unlist(pd[,8], use.names=FALSE))
		plot_d<-rbind(df.1, df.2, df.3, df.4, df.5 )
		
		plot_d$smr_val<-as.numeric(as.character(plot_d$smr_val))

		smr_meth_p<-
			ggplot(plot_d, aes(y=smr_val, x=factor(smr_method), colour=factor(Ch), group=factor(Ch), label=as.character(N_mo2)))+
				geom_point(size=7,pch=21)+
				geom_text(color="black")+
				geom_line()+
				theme_classic()+
		    scale_color_brewer(palette = "Set1")+
				ylab(mo2_lab)+
		    ggtitle("SMR/RMR; # in the circle is (N) mo2 values")+
				xlab("SMR calc type")+ 							
				theme(axis.text.y=element_text(size=20, colour= 'black'),
					axis.text.x=element_text(size=15, colour= 'black', angle=90, hjust=1),
					axis.line.y=element_line(colour = 'black',size=0.5),
					axis.line.x=element_line(colour = 'black',size=0.5),
					axis.ticks.y=element_line(size=0.5),
					axis.ticks.x=element_line(size=0), 
					legend.title = element_blank())+
				theme(axis.title.y=element_text(size=15),
					axis.title.x=element_text(size=15),
					panel.border = element_rect(size=0.9,linetype = "solid",fill=NA, colour = "black"))
		
		png(plotname.smr.meth, width=6, height=5, units="in", res=200)	
			print(smr_meth_p)
		dev.off()
		

	} 
  
	

	# EPOC focused part	
	if (data.SMR!="none" & data.MMR!="none"){
	
	  if (path == "."){
		  	EPOCdata_name<-paste( filename.MMR, "_EPOC_DATA.csv", sep='')
		  	# EPOCplot_name<-	paste( filename.MMR, "_", d$Ch[1], "_EPOC_PLOT.png", sep='')
		  	filename.smr<-paste( gsub('.{4}$', '', data.SMR), "SMR_analyzed.csv", sep='')
  	  	filename.mmr<-paste( gsub('.{4}$', '', data.MMR), "MMR_analyzed.csv", sep='')
	    	filename.MR<-paste( gsub('.{4}$', '', data.MMR), "MR_analyzed.csv", sep='')
		}else{
		  	EPOCdata_name<-paste("../csv_analyzed_EPOC/", filename.MMR, "_EPOC_DATA.csv", sep='')
		  	# EPOCplot_name<-	paste("../plots_ch_EPOC/", filename.MMR, "_", d$Ch[1], "_EPOC_PLOT.png", sep='')
		  	filename.smr<-paste("../csv_analyzed_SMR/",gsub('.{4}$', '', data.SMR), "SMR_analyzed.csv", sep='')
  	  	filename.mmr<-paste("../csv_analyzed_MMR/",gsub('.{4}$', '', data.MMR), "MMR_analyzed.csv", sep='')
	    	filename.MR<-paste("../csv_analyzed_MR/", gsub('.{4}$', '', data.MMR), "MR_analyzed.csv", sep='')
	    	
		}

	 
	  if(!(length(unique(d_SMR$Ch)) == length(unique(d_MMR$Ch)))){
	    stop_function <- TRUE
      if(stop_function) stop(paste("The number of channels in MMR and SMR do not match, check the input data and try again! | "))#, "Ch in MMR:", unique(d_MMR$Ch),  "Ch in SMR:", unique(d_SMR$Ch)))
	  }
	  
		mmr_Ch<-list()
		for(i in 1:4){
			if(any(grepl(as.character(i),as.character(d_MMR$Ch)))){
  
			  ##print(c(d_MMR$mo2[which(d_MMR$cycle_type=="MMR" & d_MMR$Ch==nameCh)], nameCh))
				name<-paste("mmr_Ch",i,sep="")
				nameCh<-paste("Ch",i,sep="")
				mmr_Ch0<-d_MMR$mo2[which(d_MMR$cycle_type=="MMR" & d_MMR$Ch==nameCh)]
				mmr_length_cycle0<-d_MMR$cycle_mmr[which(d_MMR$cycle_type=="MMR" & d_MMR$Ch==nameCh)]
				mmr_Ch02<-d_MMR$mo2[which(d_MMR$Ch==nameCh)]
				assign(name, mmr_Ch0)
				
				mmr_Ch[[i]]<-mmr_Ch0
				
				cols = c(4:33)
        newdata.smr[,cols] %<>% lapply(function(x) as.numeric(as.character(x)))
			
				newdata.smr$mmr[which(newdata.smr$Ch==nameCh)]<-mmr_Ch0
				newdata.smr$mmr_length_cycle[which(newdata.smr$Ch==nameCh)]<-mmr_length_cycle0
				newdata.smr$mmr_overall[which(newdata.smr$Ch==nameCh)]<-max(c(d_MMR$mo2[which(d_MMR$Ch==nameCh)], d_SMR$mo2[which(d_SMR$Ch==nameCh)]))
				
				# print(c(mmr_Ch0, 
				#         as.numeric(newdata.smr$mmr_overall[which(newdata.smr$Ch==nameCh)]),
				#         round(max(d_SMR$mo2[which(d_SMR$Ch==nameCh)]),2),round(max(d_MMR$mo2[which(d_MMR$Ch==nameCh)]),2)),
				#       newdata.smr$mmr_overall)
				
##				print(max(d_SMR$mo2[which(d_SMR$Ch==nameCh)]))
				if(mmr_Ch0 < as.numeric(newdata.smr$mmr_overall[which(newdata.smr$Ch==nameCh)])){
				
				  message(c("MMR 1st: ", round(mmr_Ch0, 2), " < MMR overall ", round(max(c(d_MMR$mo2[which(d_MMR$Ch==nameCh)], d_SMR$mo2[which(d_SMR$Ch==nameCh)])), 3)))
				}
			
			}
		}
	
		# these are lists, so fill in the entire row
		newdata.smr$AS_smr_mean10minVal<-as.numeric(as.character(newdata.smr$mmr))-as.numeric(as.character(newdata.smr$smr_mean10minVal))
		newdata.smr$AS_SMR_low10quant<-as.numeric(as.character(newdata.smr$mmr))-as.numeric(as.character(newdata.smr$SMR_low10quant))
		newdata.smr$AS_SMR_low15quant<-as.numeric(as.character(newdata.smr$mmr))-as.numeric(as.character(newdata.smr$SMR_low15quant))
		newdata.smr$AS_SMR_low20quant<-as.numeric(as.character(newdata.smr$mmr))-as.numeric(as.character(newdata.smr$SMR_low20quant))
		
		# these are lists, so fill in the entire row
		newdata.smr$AS_smr_mean10minVal_overall<-as.numeric(as.character(newdata.smr$mmr_overall)) -as.numeric(as.character(newdata.smr$smr_mean10minVal))
		newdata.smr$AS_SMR_low10quant_overall<-as.numeric(as.character(newdata.smr$mmr_overall)) -as.numeric(as.character(newdata.smr$SMR_low10quant))
		newdata.smr$AS_SMR_low15quant_overall<-as.numeric(as.character(newdata.smr$mmr_overall)) -as.numeric(as.character(newdata.smr$SMR_low15quant))
		newdata.smr$AS_SMR_low20quant_overall<-as.numeric(as.character(newdata.smr$mmr_overall)) -as.numeric(as.character(newdata.smr$SMR_low20quant))
	
		
    if (MLND==TRUE){
      newdata.smr$AS_smr_mlnd<-as.numeric(as.character(newdata.smr$mmr))-as.numeric(as.character(newdata.smr$smr_mlnd))
      newdata.smr$AS_smr_mlnd_overall<-as.numeric(as.character(newdata.smr$mmr_overall)) -as.numeric(as.character(newdata.smr$smr_mlnd))
    }else{
      newdata.smr$AS_smr_mlnd<-0
      newdata.smr$AS_smr_mlnd_overall<-0
    }
		
    cols = c(4:33)
    newdata.smr[,cols] %<>% lapply(function(x) as.numeric(as.character(x)))
    cols2 = c(1:3)
    newdata.smr[,cols2] %<>% lapply(function(x) as.character(x))
		

		# cobine the file specific file to the combined file
		# newdata.all<-rbind(newdata.all, newdata.smr)
  	# EPOC part -- ONLY IF both MMR and SMR are available 
  	### defining function first:
  	#	environment(plotEPOC.spar)<-environment()
    # when both MMR and SMR are available
    
		d_SMR$cycle_type<-"SMR"
		d_MMR$time_frame<-NA

		for(i in 1:nrow(d_MMR)){
			d_MMR$time_frame[i] <- paste("min",d_MMR$cycle_start[i],"_", d_MMR$cycle_end[i], sep="")
		}
		
		names(d_MMR)[names(d_MMR) == 'cycle_start'] <- 'min_start'

		dat_MMR<-d_MMR[,c("ID", "time_frame", "min_start", "r2", "b", "m", "t_min", "t_max", "t_mean", "Ch", "bw", "mo2", "cycle_type", "DateTime_start", "scaling_exponent", "common_mass")]
		dat_SMR<-d_SMR[,c("ID", "time_frame", "min_start", "r2", "b", "m", "t_min", "t_max", "t_mean", "Ch", "bw", "mo2", "cycle_type", "DateTime_start", "scaling_exponent", "common_mass")]
		
		if (length(unique(dat_SMR$Ch))>1){
			Ch.dat.smr<-split(dat_SMR, dat_SMR$Ch)
			Ch.dat.mmr<-split(dat_MMR, dat_MMR$Ch)
		}else{
			Ch.dat.smr<-1
			Ch.dat.mmr<-1
		}
#~ 		data<-dat_MMR[-c(1:nrow(dat_MMR)), ]

		EPOCdata<-matrix(ncol=26, nrow=0)
		colnames(EPOCdata) <- c("ID", "smr_type", "smr","spar", "EPOC_full", "end_EPOC_min", "SMR_intergral_full", "SMR_threshold", "EPOC_1hr", "MO2_1hr", "EPOC_2hr", "MO2_2hr", "EPOC_3hr", "MO2_3hr", "EPOC_4hr", "MO2_4hr",  "EPOC_5hr", "MO2_5hr", "end_EPOC.mmr", "EPOC_mmr", "MO2_mmr", "MMR", "MMR_percent", "scaling_exponent_mmr",  "scaling_exponent_smr", "common_mass")
		
    # apply EPOC.spar function on combined mmr smr data 
  	# loop through all available chanels 
		for(i in 1:length(unique(dat_SMR$Ch))){
  		
  		if (length(unique(dat_SMR$Ch))==1){
  			d.smr<-dat_SMR
  			d.mmr<-dat_MMR
  		}else{	
  			d.smr<-as.data.frame(Ch.dat.smr[i])
  			d.mmr<-as.data.frame(Ch.dat.mmr[i])
  		}
  	  
		
		  cols = c(1,2,5,13,14)
      d.mmr[,cols] %<>% lapply(function(x) (as.character(x)))
      d.smr[,cols] %<>% lapply(function(x) (as.character(x)))
      
  		d<-rbind(d.mmr, d.smr)
  		colnames(d)<-c("ID", "time_frame", "min_start", "r2", "b", "m", "t_min", "t_max", "t_mean", "Ch", "bw", "mo2", "cycle_type", "DateTime_start", "scaling_exponent", "common_mass")
  	
  		# mmr value for MMR 50 recovery calculations
  		mmr.val <- max(d$mo2)
  		message(paste(d$Ch[1], ": MMR = ", round(mmr.val,2) ,sep=""))
  		
  		# Ch specific name for epoc plot	
			if (path == "."){
	    	EPOCplot_name<-	paste( filename.MMR, "_", d$Ch[1], "_EPOC_PLOT.png", sep='')
    	}else{
	    	EPOCplot_name<-	paste("../plots_ch_EPOC/", filename.MMR, "_", d$Ch[1], "_EPOC_PLOT.png", sep='')
	    }
  
    	 # d$DateTime_start<- strptime(d$DateTime_start, format="%Y-%m-%d %H:%M:%S") # the default strptime, that was already used above
  		 d$time_mo2<-NA
  		 d$time_mo2[1]<-0
 
			for(j in 2:nrow(d)){
				d$time_mo2[j]<-difftime(d$DateTime_start[j], d$DateTime_start[1], units=c("mins"))
			}
			
		  end_EPOC<-end_EPOC_Ch[as.numeric(substr(d$Ch[1], start=3, stop=3))]
  		## The EPOC calculation
  		
		  spars <- spars_levels
  		
  			if(nrow(d) ==4 || nrow(d)>4){
  				for (n in 1:length(spars)){
  					if (n == 1) {
  					  # length(spars)
  						png(EPOCplot_name, width = 6, height = length(spars)*4, units="in", res=200)
  						par(mfrow=c(length(spars),1), mar=c(4,4,3,1)+0.1)
  					}

  					EPOCdata <- EPOC.spar(spars[n], d, EPOCdata, mmr.val, epoc_threshold, recovMMR_threshold, newdata.smr, MLND, end_EPOC, scaling_exponent_mmr, scaling_exponent_smr, common_mass)
  					
  					if (n == length(spars)){
  						dev.off()
  						write.csv(file=EPOCdata_name, EPOCdata, row.names=FALSE)
  					}
  				}
  				
  			}else{
  				message("Not enough points to do smoothing for EPOC (n < 4)")
  			}
	
	   }
	
	
  	if (epoc_threshold == 1){
  		  	message (paste("EPOC threshold = SMR"))
  		}else{
  		    message (paste("EPOC threshold = ", epoc_threshold, "* SMR ", sep = ""))
  		}
			
  	# print(newdata.smr)
  	# print(class(newdata.smr))
#   	newdata.smr<-as.data.frame(newdata.smr)
# 		newdata.smr<- apply(newdata.smr, 2, as.character)
# 
		write.csv(file=filename.smr, d_SMR, row.names=FALSE)
		write.csv(file=filename.mmr, d_MMR, row.names=FALSE)
		write.csv(file=filename.MR, newdata.smr, row.names=FALSE)
		
	# 	if(test=="preSDA"){
	#     	  filename.MR<-paste("../csv_input_files/", gsub('.{4}$', '', data.MMR), "MR_analyzed.csv", sep='')
	#     	  write.csv(file=filename.MR, newdata.smr, row.names=FALSE)
	#    }

		message("Save MMR, SMR, and MR files")
		
  }	

	

	# no MMR file 
  if(data.SMR!="none" & data.MMR=="none"){
	  
	 if (path == "." ){
	    filename.smr<-paste(gsub('.{4}$', '', data.SMR), "SMR_analyzed.csv", sep='')
		  filename.MR<-paste(gsub('.{4}$', '', data.SMR), "MR_analyzed.csv", sep='')
   }else{
	    filename.smr<-paste("../csv_analyzed_SMR/",gsub('.{4}$', '', data.SMR), "SMR_analyzed.csv", sep='')
		  filename.MR<-paste("../csv_analyzed_MR/", gsub('.{4}$', '', data.SMR), "MR_analyzed.csv", sep='')
	 }

    lst <- lapply(newdata.smr, unlist)
    newdata.smr <- (data.frame(lapply(lst, `length<-`, max(lengths(lst)))))

   # print(newdata.smr)  
		write.csv(file=filename.smr, d_SMR, row.names=FALSE)
		write.csv(file=filename.MR, newdata.smr, row.names=FALSE)
		
	   # if(test=="preSDA"){
	   #  	  filename.MR<-paste("../csv_input_files/", gsub('.{4}$', '', data.SMR), "MR_analyzed.csv", sep='')
	   #  	  write.csv(file=filename.MR, newdata.smr, row.names=FALSE)
	   # }
		message("Save SMR and MR files")

  }  # end of argument of 

  
  # no SMR file
  if(data.SMR=="none" & data.MMR!="none"){# if only MMR is analyzed
	
		# filename.MMR<-paste(gsub('.{3}$', '',data.MMR), "_MMR_", sep="")
	  if (path == "."){ 
	    filename.mmr<-paste( gsub('.{4}$', '', data.MMR), "MMR_analyzed.csv", sep='')
		  filename.MR<-paste( gsub('.{4}$', '', data.MMR), "MR_analyzed.csv", sep='')
	  
	   }else{
	    filename.mmr<-paste("../csv_analyzed_MMR/",gsub('.{4}$', '', data.MMR), "MMR_analyzed.csv", sep='')
		  filename.MR<-paste("../csv_analyzed_MR/", gsub('.{4}$', '', data.MMR), "MR_analyzed.csv", sep='')
    }
    
		# d_MMR$bw<-NA
		# d_MMR$mo2<-NA
		# d_MMR$ID<-NA
		# d_MMR$resp.V<-NA
		# 
		# for(i in 1:4){
		# 	if(any(grepl(as.character(i),as.character(d_MMR$Ch)))){
		# 		nameCh<-paste("Ch",i,sep="")
		# 		bw.val<-BW.animal[i]
		# 		ID<-AnimalID[i]
		# 		resp.Vol<-resp.V[i]
		# 		n.row<-which(d_MMR$Ch==nameCh)
		# 		d_MMR$bw[n.row]<-bw.val	
		# 		d_MMR$ID[n.row]<-ID
		#     d_MMR$resp.V[n.row]<-resp.Vol
		# 	}
		# }
		# 
	# ### CORERCT FOR background!!! 	
	# 	for (i in 1:nrow(d_MMR)){
	# 		d_MMR$mo2[i]<-d_MMR$m[i]*(d_MMR$resp.V[i]-d_MMR$bw[i])/(d_MMR$bw[i])#^scaling_exponent_mmr) # units mgO2 kg-1 min-1
	# 	}
				
		d_MMR<-d_MMR[as.character(d_MMR$ID)!="blank",]
	  d_MMR<-d_MMR[d_MMR$ID!="NA",]


		for (i in 1:nrow(d_MMR[d_MMR$cycle_type=="MMR",])){
		  d_MMR2<- d_MMR[d_MMR$cycle_type=="MMR",]# data frame without the cycles (e.g., the cycles only
		  
		  # temp min and max for MMR only 
			values<-as.data.frame(t(c(gsub('.{4}$', '', data.MMR),  d_MMR2$ID[i], d_MMR2$Ch[i],
			                          d_MMR2$bw[i], d_MMR2$t_min[i], d_MMR2$t_max[i], d_MMR2$t_mean[i], NA,
			NA,NA,NA,
			NA,NA,NA,
			NA,NA,NA,
			d_MMR2$mo2[i],NA,NA,NA,
			NA,NA,NA,
			NA,NA,NA,
			NA,NA, d_MMR2$cycle_mmr[i], 
			scaling_exponent_mmr, NA, d_MMR2$common_mass[i])))
			
			colnames(values)<-c("filename", "ID", "Ch",
			                    "BW","t_min","t_max", "t_mean", "N_mo2",
			"smr_mean10minVal","smr_SD10minVal", "smr_CV10minVal",
			"SMR_low10quant","SMR_low15quant",  "SMR_low20quant",
			"smr_mlnd", "smr_CVmlnd", "smr_Nmlnd", 
			"mmr" ,"mmr_overall" , "AS_smr_mean10minVal", "AS_SMR_low10quant",
			"AS_SMR_low15quant" , "AS_SMR_low20quant", "AS_smr_mlnd",
			"AS_smr_mean10minVal_overall","AS_SMR_low10quant_overall", "AS_SMR_low15quant_overall",
			"AS_SMR_low20quant_overall","AS_smr_mlnd_overall", "mmr_length_cycle",
			"scaling_exponent_mmr", "scaling_exponent_smr", "common_mass")
			
			newdata.smr<-rbind(newdata.smr, values)
		}
		



		write.csv(file=filename.mmr, d_MMR, row.names=FALSE)
		
		# newdata.smr<-as.data.frame(newdata.smr)
		# newdata.smr<- apply(newdata.smr, 2, as.character)
		lst <- lapply(newdata.smr, unlist)
    newdata.smr <- (data.frame(lapply(lst, `length<-`, max(lengths(lst)))))
    
		write.csv(file=filename.MR, newdata.smr, row.names=FALSE)
	# 	if(test=="preSDA"){
	#     	  filename.MR<-paste("../csv_input_files/", gsub('.{4}$', '', data.MMR), "MR_analyzed.csv", sep='')
	#     	  write.csv(file=filename.MR, newdata.smr, row.names=FALSE)
	#   }
		message("Saving MMR, and MR files")
  }
	
	# print(newdata.smr)
#~ 		smr.cols <- c(2, 9, 12,13,14,15)
#~ 		smr.only<-newdata.smr[,cols]
				# print(newdata.smr)

}


































# ##################################### DIFFERENT FUNCTION 

MMRslide_tunnel<-function(file){

	df<-matrix(nrow=0, ncol=16)
	colnames(df)<-c("filename","test","subset","date","time","r2","interc", "slope","temp_min", "temp_max","temp_mean", "mo2", "length_slide", "timeDiff_slide", "bw", "sex")

	df_set<-matrix(nrow=0, ncol=16)
	colnames(df_set)<-c("filename","test","subset","date","time","r2","interc", "slope","temp_min", "temp_max","temp_mean", "mo2", "length_slide", "timeDiff_slide", "bw", "sex")

	data<-read.csv(file)
	data$time_min<-data$time_sec/60
	
	test<-substr(file, start=11, stop=14) 
	name<-gsub('.{4}$', '', file)
	umax<-"umax"
	ucrit<-"swim"
	fatigue<-"fatigue"
		
		if(grepl(fatigue, name,fixed=TRUE)){
			
			if(data$time_min[nrow(data)]>10){
				data<-data[1:which(round(data$time_min,0)==10)[1],]
			}
		}
		
		
	
	n<-which(substr(data2$FilenameID, start=1, stop=10) == substr(file, start=1, stop=10))
	bw.test<-data2$BW.test[n]
	sex<-data2$sex[n]

	data$time_min<-round(data$time_min, 2)
	endtime<-max(data$time_sec)
	starttime<-min(data$time_sec)
	subset<-paste("min",round(data$time_sec[1]/60,2),"_",round(data$time_sec[nrow(data)]/60, 2), sep="")
	
	fit<-lm(data$Ch1_O2~data$time_min)
	lm_coef <- round(coef(fit), 5) # extract coefficients 
	
	r2<-round(summary(fit)$r.squared, 5)
	
	interc<-lm_coef[1]
	slope<-lm_coef[2]
	
	# mo2 on the whole data
	mo2<-slope*(425-bw.test)/bw.test
	mo2<-abs(mo2)
	
	#temperature
	temp_mean<-mean(data$Ch1_temp)
	temp_max<-max(data$Ch1_temp)	
	temp_min<-min(data$Ch1_temp)	
	temp_treatm<-data2$TempTest[n]

	date<-as.character(data[1,1])
	time<-as.character(data[1,2])
	
	values<-as.data.frame(t(c(name,test, subset, date, time, r2, interc, slope, temp_min, temp_max, temp_mean, mo2, "ALL","ALL", bw.test, sex )))	
	colnames(values)<-c("filename","test","subset","date","time","r2","interc", "slope","temp_min", "temp_max","temp_mean", "mo2", "length_slide", "timeDiff_slide", "bw", "sex")
	df<-rbind(df,values)	
	
	
	plotname<-gsub('.{4}$', '_PLOT.png', file)
	png(plotname, width=6, height=5, units="in",  res=200)  
	plot(data$Ch1_O2~data$time_min, ylab=expression(paste(O2~(mg~L^{-1}))),xlab="Time (min)", main=name)
	abline(lm(data$Ch1_O2~data$time_min), col="red",lwd=2)
	mtext(bquote(y == .(lm_coef[2])*x + .(lm_coef[1])), adj=1, padj=0, cex=0.8, line=0) # display equation 
	mtext(bquote(italic(R)^2 == .(format(r2, digits = 3))),adj=1, padj=0, cex=0.8, line=1)
	dev.off()
	
	#
		
		if(endtime>180){
			message("FILE > 3 min")
			
			length_slide<-c(10,20,30,60,90,120,180)
			timeDiff_slide<-c(1)

			for(k in 1:length(length_slide)){
							
								
				string_eval <- sprintf("
					for (i in seq(starttime,(endtime-%s), by=%s)){
						
						j<-i+%s
						test<-substr(file, start=11, stop=14) 
						data_set<-data[data$time_sec>i & data$time_sec<j, ]
						subset<-paste(\"min\",round(i/60,2),\"_\",round(j/60, 2), sep=\"\")
																	
						fit_set<-lm(data_set$Ch1_O2~data_set$time_min)
						lm_coef_set <- round(coef(fit_set), 5) 	
						r2_set<-as.character(summary(fit_set)$adj.r.squared)
					
						interc_set<-lm_coef_set[1]
						slope_set<-lm_coef_set[2]
						
						temp_mean_set<-mean(data_set$Ch1_temp)
						temp_max_set<-max(data_set$Ch1_temp)	
						temp_min_set<-min(data_set$Ch1_temp)	
						
						date<-as.character(data_set[1,1])
						time<-as.character(data_set[1,2])
								
						mo2_set<-slope_set*(425-bw.test)/bw.test
						mo2_set<-abs(mo2_set)
								
						values_set<-as.data.frame(t(c(name,test, subset, date, time, r2_set, interc_set, slope_set, temp_min_set, temp_max_set, temp_mean_set, mo2_set, %s,%s, bw.test, sex )))	
						colnames(values_set)<-c(\"filename\",\"test\",\"subset\",\"date\",\"time\",\"r2\",\"interc\", \"slope\",\"temp_min\", \"temp_max\",\"temp_mean\", \"mo2\", \"length_slide\", \"timeDiff_slide\", \"bw\", \"sex\")
			
					
						colnames(df_set)<-c(\"filename\",\"test\",\"subset\",\"date\",\"time\",\"r2\",\"interc\", \"slope\",\"temp_min\", \"temp_max\",\"temp_mean\", \"mo2\", \"length_slide\", \"timeDiff_slide\", \"bw\", \"sex\")

						df_set<-rbind(df_set, values_set)
											
					}
						
								
				", length_slide[k],1, length_slide[k],length_slide[k], 1 )
				eval(parse(text = string_eval)) 
			}
			
			for (i in 1:length(length_slide)){
							
				subsetName<-paste("s",length_slide[i],"_1", sep="")
				subsetD<-df_set[df_set$length_slide ==length_slide[i],]

				assign(subsetName, subsetD)
			
			}
			
				list<-list(s10_1, s20_1, s30_1, s60_1, s90_1, s120_1, s180_1)
		}
		
		if(endtime<180 && endtime>120){
			message("FILE > 2 min but less than 3 min")
			
			length_slide<-c(10,20,30,60,90,120)
			timeDiff_slide<-c(1)

			for(k in 1:length(length_slide)){
							
								
				string_eval <- sprintf("
					for (i in seq(starttime,(endtime-%s), by=%s)){
						
						j<-i+%s
						test<-substr(file, start=11, stop=14) 
						data_set<-data[data$time_sec>i & data$time_sec<j, ]
						subset<-paste(\"min\",round(i/60,2),\"_\",round(j/60, 2), sep=\"\")
																	
						fit_set<-lm(data_set$Ch1_O2~data_set$time_min)
						lm_coef_set <- round(coef(fit_set), 5) 	
						r2_set<-as.character(summary(fit_set)$adj.r.squared)
					
						interc_set<-lm_coef_set[1]
						slope_set<-lm_coef_set[2]
						
						temp_mean_set<-mean(data_set$Ch1_temp)
						temp_max_set<-max(data_set$Ch1_temp)	
						temp_min_set<-min(data_set$Ch1_temp)	
						
						date<-as.character(data_set[1,1])
						time<-as.character(data_set[1,2])
								
						mo2_set<-slope_set*(425-bw.test)/bw.test
						mo2_set<-abs(mo2_set)
						
						values_set<-as.data.frame(t(c(name,test, subset, date, time, r2_set, interc_set, slope_set, temp_min_set, temp_max_set, temp_mean_set, mo2_set, %s,%s, bw.test, sex )))	
						colnames(values_set)<-c(\"filename\",\"test\",\"subset\",\"date\",\"time\",\"r2\",\"interc\", \"slope\",\"temp_min\", \"temp_max\",\"temp_mean\", \"mo2\", \"length_slide\", \"timeDiff_slide\", \"bw\", \"sex\")
			
					
						colnames(df_set)<-c(\"filename\",\"test\",\"subset\",\"date\",\"time\",\"r2\",\"interc\", \"slope\",\"temp_min\", \"temp_max\",\"temp_mean\", \"mo2\", \"length_slide\", \"timeDiff_slide\", \"bw\", \"sex\")

						df_set<-rbind(df_set, values_set)
											
					}
						
								
				", length_slide[k],1, length_slide[k],length_slide[k], 1 )
				eval(parse(text = string_eval)) 
			}
			
			for (i in 1:length(length_slide)){
							
				subsetName<-paste("s",length_slide[i],"_1", sep="")
				subsetD<-df_set[df_set$length_slide ==length_slide[i],]

				assign(subsetName, subsetD)
			
			}
			
				list<-list(s10_1, s20_1,s30_1, s60_1, s90_1, s120_1)
			
		}
			
	df_set$length_slide<- as.numeric(as.character(df_set$length_slide))
	df_set$timeDiff_slide<- as.numeric(as.character(df_set$timeDiff_slide))

	filename_set<-paste("../channel_sliding_sets/", name,"_SLIDINGset.csv", sep="")
	write.csv(df_set,file=filename_set, row.names=FALSE)	


		
		
		for (i in 1:length(list)){
			
			df_s<-list[[i]]
			
			df_s$r2<-as.numeric(as.character(df_s$r2))
			df_s$slope<-as.numeric(as.character(df_s$slope))
			r2_set_max<-(which(df_s$r2 == max(df_s$r2)))
			slope_set_max<-(which(df_s$slope == min(df_s$slope)))# slope is negative steepest slope is the min(slope)
			df_s$r2<-as.character(df_s$r2)
			df_s$slope<-as.character(df_s$slope)
				
			if(length(slope_set_max)==1){
				mmrMax<-df_s[slope_set_max,]
			}else{
				# selecting the one with highest r2
				message(paste("Several MMR slopes: n = ", length(slope_set_max)))
				maxset<-df_s[c(slope_set_max),]
					if(var(maxset$r2)==0){
						message("Some slopes have the same R2 use the first slope recorded")
						mmrMax<-maxset[1,]
						# if the same them I am selecting the first one - that is also plotted 
												
					}else{
						message("Select slope with best R2")
						bestr2<-which(maxset$r2 == max(maxset$r2))
						mmrMax<-maxset[bestr2,]

					}
			}

			df<-rbind(df,mmrMax)
			
		}
		

		if(endtime<120){
			message("NOT ANALYSED/ FILE < 2 min")
		}
		
	filename_df<-paste("../sliding_set_analyzed/", name,"_analyzed.csv", sep="")
	write.csv(df,file=filename_df, row.names=FALSE)	

##	assign("df", df, envir=.GlobalEnv)	# df used for fatigue and also for max metabolic rates 
##	return(invisible(df))


}




# Chabot et al 2016 JFB SMR paper 
calcSMR = function(Y, q=c(0.1,0.15,0.2,0.25,0.3), G=1:4){
			u = sort(Y)
			the.Mclust <- Mclust(Y,  G=G)
			cl <- the.Mclust$classification
			# sometimes, the class containing SMR is not called 1
			# the following presumes that when class 1 contains > 10% of cases, 
			# it contains SMR, otherwise we take class 2
			cl2 <- as.data.frame(table(cl))
			cl2$cl <- as.numeric(levels(cl2$cl))
			valid <- cl2$Freq>=0.1*length(time)  
			the.cl <- min(cl2$cl[valid])
			left.distr <- Y[the.Mclust$classification==the.cl]
			mlnd = the.Mclust$parameters$mean[the.cl]
			CVmlnd = sd(left.distr)/mlnd * 100
			quant=quantile(Y, q)
			low10=mean(u[1:10])
			low10pc = mean(u[6:(5 + round(0.1*(length(u)-5)))])
			# remove 5 outliers, keep lowest 10% of the rest, average
			# Herrmann & Enders 2000
			return(list(mlnd=mlnd, quant=quant, low10=low10, low10pc=low10pc,
					  cl=cl, CVmlnd=CVmlnd))
}




