# Hourly MO2 values using analysed data from SMR_MMR_EPOC_AS function 

library(tidyverse)

# 1. test
# setwd("../Test/MMR_SMR_AS_EPOC/csv_analyzed_SMR/")
# data<-"jul04_2019_opasize_box4_smr_SMR_analyzed.csv"
# wd<-"../Test/MMR_SMR_AS_EPOC/csv_analyzed_SMR/"

# 1. get the mean of each hour
calc_hMO2<-function(data, wd = "./", export.wd = "./"){

  data.H<-read.csv(file = paste(wd, data, sep=""))
  data.H$hour<-as.factor(floor(data.H$min_start/60))
  # print(str(data.H))
  data.H$ID<-as.factor(as.character(data.H$ID))
  
  data.H.sum<-as.data.frame(data.H %>%
    group_by(Ch, ID, resp.V, bw, hour) %>%
    summarize(mo2_mean = mean(mo2, na.rm=TRUE), mo2_sd = sd(mo2, na.rm=TRUE), mo2_min = min(mo2, na.rm=TRUE), t_mean=mean(t_mean), n=length (mo2), mo2_max = max(mo2, na.rm=TRUE)))
  
  hr_plot<-ggplot(data=data.H.sum, aes(y=mo2_mean, x=hour))+
	  geom_point(size=2, pch=21, fill="grey", alpha=0.9, colour="black")+
	  geom_point(data=data.H.sum, aes(y=mo2_min, x=hour), pch=21, size=3, fill="black", alpha=0.7)+
  	geom_line(data=data.H.sum, aes(y=mo2_min, x=hour), size=1, alpha=0.7)+
	  geom_errorbar(ymin=data.H.sum$mo2_mean-data.H.sum$mo2_sd, ymax = data.H.sum$mo2_mean+data.H.sum$mo2_sd, alpha=0.5 )+
	  theme_classic()+
  	ylab("grey = MO2 mean +/- SEM, black = hourly MO2 min ")+
	  # geom_points(aes(x=
	  facet_wrap(.~ID, ncol=1,scales="free")
  
    filename.data<-	paste(export.wd, gsub('.{4}$', '', data),"_hourly.csv", sep='')
    filename.plot<-	paste(export.wd, gsub('.{4}$', '', data),"_hourly.png", sep='')
    write.csv(file=filename.data, data.H.sum, row.names=FALSE )
    ggsave(filename = filename.plot, hr_plot)
}

calc_hMO2(data="jul04_2019_opasize_box4_smr_SMR_analyzed.csv")
