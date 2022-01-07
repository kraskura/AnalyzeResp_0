


# last updated: Aug 23, 2021

# RESPIROMETRY ANALYSIS: ---------

# 1. import source code and setwd()
source("/Users/kristakraskura/Github_repositories/Respirometry_Performances/Codes/MR_firesting_v2.1.0.1.R")
source("/Users/kristakraskura/Github_repositories/Plots-formatting/ggplot_format.R")
source("/Users/kristakraskura/Github_repositories/Respirometry_Performances/Codes/construct_respo_runs.R")



# 2. create all working directories 
organize_MR_analysis(create = "Full")# bacterial respiration currently has similar format than that for auto cycles. 


# txt to csv -------------
# 3. convert all raw firesting data files into csv files and automatically save them in "csv_files" subfolders, either in AUTO or MANUAL or bacterial main folders, accordingly. 
# if path is not specified then just save converted files in the current directory

txt_csv_convert(txt_file = "./jun01_2021_box1_back-allRespos.txt",  path = "BACTERIAL_RESP" , N_Ch = 4)
txt_csv_convert(txt_file = "./jul04_2019_opasize_box4_mmr.txt",  path = "MANUAL" , N_Ch = 4)
txt_csv_convert(txt_file = "./jul04_2019_opasize_box4_smr.txt",  path = "AUTO" , N_Ch = 4)
txt_csv_convert(txt_file = "./jul04-2019-opasize-box4-smr.txt",  path = "AUTO" , N_Ch = 4)
txt_csv_convert(txt_file = "./jul04-2019-opasize-box4-mmr.txt",  path = "MANUAL" , N_Ch = 4)


# Convert all csv files from AS to mgO2.L ------------

# These will be saved in your current working directory with the 
# I am looping through all files to get this work. 
# For a single file example: 
convert.o2.Firesting(csv.data = "FileToCovert.csv", n_ch = 4, sal = 35)


setwd("/Users/kristakraskura/Desktop/BOX/UCSB/Research/Perch /data/Respiromtery data- preggo- acute/MANUAL/csv_files/")
n.mmr.files<-length(list.files("/Users/kristakraskura/Desktop/BOX/UCSB/Research/Perch /data/Respiromtery data- preggo- acute/MANUAL/csv_files/"))
mmr.files<-list.files("/Users/kristakraskura/Desktop/BOX/UCSB/Research/Perch /data/Respiromtery data- preggo- acute/MANUAL/csv_files/")

for (i in 1:n.mmr.files){
  
  mr.file<-mmr.files[i]
  convert.o2.Firesting(csv.data = mr.file, n_ch = 4, sal = 35)
}

setwd("/Users/kristakraskura/Desktop/BOX/UCSB/Research/Perch /data/Respiromtery data- preggo- acute/AUTO/csv_files/")
n.smr.files<-length(list.files("/Users/kristakraskura/Desktop/BOX/UCSB/Research/Perch /data/Respiromtery data- preggo- acute/AUTO/csv_files/"))
smr.files<-list.files("/Users/kristakraskura/Desktop/BOX/UCSB/Research/Perch /data/Respiromtery data- preggo- acute/AUTO/csv_files/")

for (i in 1:n.smr.files){
  
  mr.file<-smr.files[i]
  convert.o2.Firesting(csv.data = mr.file, n_ch = 4, sal = 35)
}


setwd("/Users/kristakraskura/Desktop/BOX/UCSB/Research/Perch /data/Respiromtery data- preggo- acute/BACTERIAL_RESP/csv_files/")
n.back.files<-length(list.files("/Users/kristakraskura/Desktop/BOX/UCSB/Research/Perch /data/Respiromtery data- preggo- acute/BACTERIAL_RESP/csv_files/"))
back.files<-list.files("/Users/kristakraskura/Desktop/BOX/UCSB/Research/Perch /data/Respiromtery data- preggo- acute/BACTERIAL_RESP/csv_files/")

for (i in 1:n.back.files){
  
  mr.file<-back.files[i]
  convert.o2.Firesting(csv.data = mr.file, n_ch = 4, sal = 35)
}

# 1. MMR functions. ---------


setwd("./MANUAL/csv_files/")
MMR(data.MMR = "./jul04_2019_opasize_box4_mmr.csv", 
    cycles = 2, 
    cycle_start = c(0, 11.02),
    cycle_end = c(4.0, 15.95),
    Ch1 = 1,
    Ch2 = 2,
    Ch3 = 1,
    Ch4 = 2,
    clean_Ch1=c(1.2,3.5),  # <<< this is for all channels, e.g. all channel cut offs 
    clean_Ch2=c(0,0),
    clean_Ch3=c(0,0),
    clean_Ch4=c(0,0),
    path ="Folders",
    N_Ch = 4,
    date_format = "m/d/y",
    inv.data = NA)

    # inv.data="/Users/kristakraskura/Github_repositories/Metabolic-scaling-fish/Data/Perch-scaling/MMRinventPerch2021.csv")
# the inv.data is specific for a channel (see the excel/csv attached)


# 2. SMR functions. ---------

setwd("./csv_files/")

SMR(data="jul04_2019_opasize_box4_smr.csv",
    inventory_data=NA,
    cycle_start=8,
    cycle_end=15,
    chop_start=10/60,
    chop_end=0,
    flush_plot="ON",
    N_Ch=4,
    path="UseFolders",
    date_format = "m/d/y")

# 3. SMR function for background. -------------------------------------

setwd("")

SMR(data="may25_box3_perch-back-pre_mgO2L.csv",
    inventory_data=NA,
    cycle_start=7,
    cycle_end=15,
    chop_start=30/60,
    chop_end=0,
    flush_plot="ON",
    N_Ch=4,
    path="UseFolders",
    date_format = "m/d/y")




# 4. SMR - combined auto files together. -------------

setwd("/Users/kristakraskura/Desktop/BOX/UCSB/Research/Perch /data/Respiromtery data- preggo- acute/AUTO/csv_analyzed/")
combine_smr(smr_files=c("jun01_2021_box1_smr1_mgO2L_analyzed.csv",
                        "jun01_2021_box1_smr2_mgO2L_analyzed.csv",
                        "jun01_2021_box1_smr3_mgO2L_analyzed.csv"),
            date_format = "m/d/y")


# 5. MMR_SMR_AS_EPOC functions. -------------

setwd("")

MMR_SMR_AS_EPOC(
  data.MMR = "jun01_2021_box1_mmr1_mgO2L_analyzed.csv",
  data.SMR = "jun01_2021_box1_smr1_mgO2L_analyzed_GLUED_n3.csv",
  AnimalID = c("GRTB1","GRDT1","RRTTB1","NA"),
  BW.animal = c(0.6765,0.3855,0.1495,0),
  resp.V = c(32.12,12.447,4.159,0),
  r2_threshold_mmr = 0.9,
  r2_threshold_smr = 0.9, 
  scaling_exponent_mmr = 1,
  scaling_exponent_smr = 1,
  epoc_threshold = 1,
  drop_ch = c(4),
  recovMMR_threshold = 0.7,
  plot_smr_quantile=10, 
  mo2_val_for_calc = "mo2_1kg",
  end_EPOC_Ch = c(NA, NA, NA, NA),
  # mmr_type = "mean",
  min_length_mmr = 90,
  background_prior = "jun01_2021_box1_back-allRespos_mgO2L_analyzed.csv" ,
  background_post = "jun02_2021_box1_back-PRE_mgO2L_analyzed.csv",
  background_slope = NULL,
  background.V = NULL,
  MLND=FALSE,
  match_background_Ch = TRUE,
  mmr_background = "SAME_slope",
  background_linear_gr = TRUE,
  path="foldering",
  date_format = "m/d/y",
  N_Ch = 4, 
  common_mass= 1)





### From Sam's data OLD !!!! Before Aug 23, 2021:  ----------------

## ---

# Example code to perform full MR analysis:
# The code includes the example to analyze:
# 1. Standard (or routine) metabolic rate, SMR
# 2. Maximum metabolic rate, MMR
# 3. Aerobic scope, AS, and Exess post exercise oxygen consumption, EPOC
# 4. Specific dynamic action, SDA

setwd("")

MMR_SMR_AS_EPOC(data.MMR = "Jul17_2018_box2_manual_analyzed.csv", #
    data.SMR = "Jul19_2018_box2_auto_analyzed.csv", #
    AnimalID = c("N18", NA, "L4", "N14"), #
    BW.animal = c(0.444, 0, 0.412, 0.323), #
    resp.V = c(17.889375, 17.889375, 17.889375, 17.889375), #
    r2_threshold_smr = 0.85, r2_threshold_mmr = 0.9, #
    min_length_mmr = 60, #
    epoc_threshold = 1.2, #
    drop_ch = 2,
    MLND = FALSE,
    recovMMR_threshold = 0.5, #
    end_EPOC_Ch = c(NA, NA, NA, NA),
    background_prior = "Jul17_2018_box2_blankbefore_SPLIT_analyzed.csv", #
    background_post = "Jul17_2018_box2_blankafter_SPLIT_analyzed.csv", #
    match_background_Ch = TRUE,
    path = ".",
    #date_format = "m/d/y",
    N_Ch = 4)



# ---- Creating folders in local directory &  conversion from raw txt --> csv file ----
#  set working directory with raw (.txt) Firesting data files. This is the location where all analysis output files will be saved and organized
setwd("/Users/kristakraskura/Github_repositories/Respirometry_Performances/Test/")

# create all working directories
organize_MR_analysis(create = "Full") # bacterial respiration currently has similar format than that for auto cycles.
organize_MR_analysis(create = "SDA") # bacterial respiration currently has similar format than that for auto cycles.


# convert all raw firesting data files into csv files and automatically save them in "csv_files" subfolders, either in AUTO or MANUAL or bacterial main folders, accordingly.
# if path is not specified then just save converted files in the current directory
txt_csv_convert(txt_file = "jul04_2019_opasize_box4_smr.txt", path = "AUTO", N_Ch = 4)
# txt_csv_convert(txt_file = "may14_2018_box2_blank_before.txt", path="BACTERIAL_RESP")
txt_csv_convert(txt_file = "jul04_2019_opasize_box4_mmr.txt", path = "MANUAL", N_Ch = 4)

# ---- Splitting files ----------
# setwd("/Users/kristakraskura/Desktop/BOX/UCSB/R/Kara")
# set a your working directory, this is a folder with files that will be split
setwd("/Users/kristakraskura/Github_repositories/Respirometry_Performances/Test/SplitFiles/")

# 4 channel Firesting


csv_file_split(data="sep12_2019_AS_box3_H2_auto.csv",
               split_data_name="TEST_split-jan",
               cycle_full_min=10,
               timeSplit=210,
               split=FALSE,
               N_Ch=4,
               date_format="m/d/y")

csv_file_split(data="Aug16_2017_Box1_fullfile.csv",
               split_data_name="Aug16_2017_Box1_SPLIT-TEST-4ch",
               cycle_full_min=10,
               timeSplit=58.5,
               split=FALSE,
               N_Ch=4,
               date_format="m/d/y")

# 8 ch Firesting example
csv_file_split(data="Sep24_2019_box0_fullfile.csv",
               split_data_name="Sep24_2019_box0_SPLIT-TEST-8ch",
               cycle_full_min=10,
               timeSplit=42,
               split=TRUE,
               N_Ch=8,
               date_format="d/m/y")


#
# # 8 channel firesting
# FirestingSplit(data="Sep24_2019_chsh_mmr.csv", split_data_name="TEST_split-8ch",
#                 t1=1, t2=120, cycle_full_min=10, timeSplit=3557/60, split=TRUE)
#
#

# ---- analyze MMR ------
# 8 channel - feb 2020
setwd("/Users/kristakraskura/Github_repositories/Respirometry_Performances/Test/MANUAL/csv_files")
# MMR(data.MMR="Oct28_2019_chch_box0_mmr.csv",
#     cycles = 4,
#     cycle_start = c(0, 10, 20, 29.1),
#     cycle_end = c(5, 15, 25, 32),
#     clean_Ch1=c(0,0), clean_Ch2=c(0,0), clean_Ch3=c(0,0), clean_Ch4=c(0,0),
#     Ch1=3, Ch2=3, Ch3=0, Ch4=3,
#     N_Ch=8, date_format = "m/d/y", path="folder")



# Use MMR function on MMR files
setwd("/Users/kristakraskura/Github_repositories/Respirometry_Performances/Test/MANUAL/csv_files")
MMR(data.MMR="jul04_2019_opasize_box4_mmr.csv",
        cycles = 2,
        cycle_start = c(0,9.85),
        cycle_end = c(3.8,14.1),
        mmr_Ch1=1, mmr_Ch2=2, mmr_Ch3=1, mmr_Ch4=2,
        clean_Ch1=c(0,0),
        clean_Ch2=c(0,0),
        clean_Ch3=c(0,0),
        clean_Ch4=c(0,0),
        path ="Foldering", # times in minutes (relative time)
        N_Ch = 4,
        date_format = "m/d/y",
        inv.data="MMR_template_TESTJan2022.csv")

data.MMR="jul04_2019_opasize_box4_mmr.csv"
cycles = 2
cycle_start = c(0,9.85)
cycle_end = c(3,14.1)
Ch1=1
Ch2=2
Ch3=1
Ch4=2
clean_Ch1=c(0,0)
clean_Ch2=c(0,0)
clean_Ch3=c(0,0)
clean_Ch4=c(0,0)
path ="Foldering" # times in minutes (relative time)
N_Ch = 4
date_format = "m/d/y"
inv.data="MMR_template_april202.csv"


# ---- analyze SMR ------
setwd("/Users/kristakraskura/Github_repositories/Respirometry_Performances/Test/AUTO/csv_files")
SMR(data="jul04_2019_opasize_box4_smr.csv",
    inventory_data=NA,
    cycle_start=7,
    cycle_end=15,
    chop_start=10/60,
    chop_end=0,
    flush_plot="OFF",
    N_Ch=4,
    path="UseFolders",
    date_format = "m/d/y", plot_temp = FALSE)

SMR(data="jul04_2019_opasize_box4_smr.csv",
    inventory_data= "smr_inventory_test.csv",
    cycle_start=8, # length of a full cycle, with open and closed phases together
    cycle_end=15, # this will take first 10 sec off as a wait period, can be decimal
    chop_start=10/60,
    chop_end=0,
    flush_plot="OFF",
    N_Ch=4,
    path="UseFolders",
    date_format = "m/d/y")

# 4 ch with cleaning
SMR(data="jul04_2019_opasize_box5_smr.csv",
        inventory_data="SMR_inventory_data_opa_smr1.csv",
        cycle_start=7, cycle_end=15, chop_start=10/60, chop_end=0, flush_plot="OFF",
        N_Ch=4, path="UseFolders", date_format = "m/d/y")


# 8 - channel 
SMR(data = "Oct09_2019_chch_box0_smr.csv", inventory_data = "SMR_inventory_chch.csv", 6, 10, 70/60, 20/60, N_Ch=8, path="foldering",  date_format = "y-m-d", flush_plot = "OFF")
# "SMR_inventory_chch.csv"

SMR(data = "Oct08_2019_chch_box0_back1.csv", inventory_data = NA, 6, 10, 60/60, 10/60, N_Ch=8, path="foldering",  date_format = "y-m-d", flush_plot = "OFF")
 SMR(data = "Oct08_2019_chch_box0_back1.csv", inventory_data = NA, 6, 10, 20/60, 10/60, N_Ch=8, path="foldering",  date_format = "y-m-d", flush_plot = "OFF")


# Need to do inventory? -- rerun all the file(s) but this time use the inventory file
# !!!! --- inventory file must be in the csv_files folder

# Need to combine several smr files to obtain one file that can be input in MMR_SMR_EPOC_AS function?
# the one combined file with name "GLUED_nX" (x = n of files) will be saved in the local directory and will be automatically placed in the folder "input_files" for full AS, EPOC etc. analysis (see below)

# Use SMR function on background respiration files as well.

 
 
# sum background  --------
# for system wide background resp estimates
 setwd("/Users/kristakraskura/Github_repositories/Respirometry_Performances/Test/MMR_SMR_AS_EPOC/csv_input_files")
 
 sum_backgr(data = "jul04_2019_box4_back_post_dummy_analyzed.csv",
            Ch = c(1,2,3,4),
            resp.ID = c("A", "B", "D", "E"),
            resp.Vol = c(1.5, 1.5, 1.5, 1.5))
            
 
 
 
# ---- analyze SMR, MMR, EPOC ------
#Get MO2 values, these are size adjusted (mgO2 kg-1 min-1)
setwd("/Users/kristakraskura/Github_repositories/Respirometry_Performances/Test/MMR_SMR_AS_EPOC/csv_input_files")

# MMR_SMR_AS_EPOC( #not working at the moment; 330 died shortly after chase
#       data.MMR = "Oct14_2019_chch_box1_mmr_analyzed.csv",
#       data.SMR = "Oct14_2019_chch_box1_smr_analyzed_GLUED_n2.csv",
#       AnimalID = c("CHCH329","CHCH330","NA","NA"),
#       BW.animal = c(2.439, 2.41, 0, 0),
#       drop_ch = c(3,4),
#       N_Ch = 4,
#       resp.V = c(54,98,0,0),
#       r2_threshold_smr = 0.85,
#       r2_threshold_mmr = 0.9,
#       min_length_mmr = 120,
#       epoc_threshold = 1.1,
#       recovMMR_threshold = 0.5,
#       end_EPOC_Ch = c(NA, NA, NA, NA), # must match how many channels are there, use NA otherwise
#       background_prior = NA, #no backg evident
#       background_post = NA,  #no backg evident
#       MLND=FALSE,
#       date_format = "m/d/y",
#       path="foldering")


MMR_SMR_AS_EPOC(
      	data.MMR = "jul04_2019_opasize_box4_mmr_analyzed.csv",
      	data.SMR = "jul04_2019_opasize_box4_smr_analyzed.csv",
        AnimalID = c("FISH1","FISH2","FISH3","FISH4"),
        BW.animal = c(0.065,0.068,0.061,0.062),
      	resp.V = c(1.9,1.9,1.5,1.5),
      	r2_threshold_smr = 0.85,
      	r2_threshold_mmr = 0.9,
        scaling_exponent_mmr = 1,
      	scaling_exponent_smr = 1,
        epoc_threshold = 1,
      	# drop_ch = 1,
      	recovMMR_threshold = 0.5,
      	plot_smr_quantile=10, 
      	mo2_val_for_calc = "mo2_1kg", # "mo2_1kg", mo2_common_mass_kg, mo2_per_individual_kg
        end_EPOC_Ch = c(NA, NA, NA, NA), # must match how many channels are there, use NA otherwise
      	# mmr_type = "mean",
      	min_length_mmr = 120,
      	spars_levels = c(0.1, 0.3),
        background_prior = "jul04_2019_box4_back_pre_dummy_analyzed.csv" ,
        background_post = "jul04_2019_box4_back_post_dummy_analyzed.csv" ,
        background_slope = NULL,
        background.V = NULL,
        MLND=TRUE,
        match_background_Ch = TRUE,
        mmr_background = "SAME_slope",
        background_linear_gr = TRUE,
        path="foldering",
      	date_format = "m/d/y",
      	N_Ch = 4, 
      	common_mass= 0.5) # in kg


data.MMR = "jul04_2019_opasize_box4_mmr_analyzed.csv"
data.SMR = "jul04_2019_opasize_box4_smr_analyzed.csv"
AnimalID = c("23-rbred", "27-ltred", "27-lbred", "27-rtred")
BW.animal = c(0.065, 0.068, 0.061, 0.062)
resp.V = c(1.890, 1.890, 1.4475, 1.4475)
r2_threshold_smr = 0.85
r2_threshold_mmr = 0.9
scaling_exponent_mmr = 1
scaling_exponent_smr = 0.89
epoc_threshold = 1
drop_ch = 1
recovMMR_threshold = 0.7
end_EPOC_Ch = c(NA, NA, NA, NA) # must match how many channels are there use NA otherwise
# mmr_type = "mean"
min_length_mmr = 120
# background_prior = "jul04_2019_opasize_box4_back_prior_dummy.csv" 
# background_post = "jul04_2019_opasize_box4_back_post_dummy.csv"
# background_slope = NULL
# background.V = NULL
MLND=FALSE
# match_background_Ch = TRUE
# mmr_background = "SAME_slope"
# background_linear_gr = FALSE
path="foldering"
date_format = "m/d/y"
N_Ch = 4 
common_mass= 0.05

MMR_SMR_AS_EPOC( data.MMR = "jul04_2019_opasize_box4_mmr_analyzed.csv",
                 data.SMR = "jul04_2019_opasize_box4_smr_analyzed.csv",
                 AnimalID = c("23-rbred","27-ltred","27-lbred","27-rtred"),
                 BW.animal = c(0.065,0.068,0.061,0.062),
                 resp.V = c(1.890,1.890,1.4475,1.4475),
                 r2_threshold_smr = 0.85,
                 r2_threshold_mmr = 0.9,
                 min_length_mmr = 120)

# ---- SDA analysis----
# 1. set working directory with raw (.txt) Firesting data files. This is the location where all analysis files will be organized
setwd("./Test/")
# 2. create all working directories
organize_MR_analysis(create = "SDA") # bacterial respiration currently has similar format than that for auto cycles.
# 3. convert the files
txt_csv_convert(txt_file = "Aug7_2019_PAAR17-20_box1_sda.txt", path = "SDA", N_Ch = 4)
txt_csv_convert(txt_file = "Aug8_2019_PAAR17-20_box1_sda2.txt", path = "SDA", N_Ch = 4)
txt_csv_convert(txt_file = "Aug9_2019_PAAR17-20_box1_sda3.txt", path = "SDA", N_Ch = 4)

# 4. run the SMR file on this
# change WD first
setwd("./SDA/csv_files/")
SMR(data="Aug7_2019_PAAR17-20_box1_sda.csv",
    inventory_data=NA,
    cycle_start=7,
    cycle_end=15,
    chop_start=10/60,
    chop_end=0,
    flush_plot="OFF",
    N_Ch=4,
    path="LocalSDAFolders",
    date_format = "m/d/y")

SMR(data="Aug8_2019_PAAR17-20_box1_sda2.csv",
    inventory_data=NA,
    cycle_start=7,
    cycle_end=15,
    chop_start=10/60,
    chop_end=0,
    flush_plot="OFF",
    N_Ch=4,
    path="LocalSDAFolders",
    date_format = "m/d/y")

SMR(data="Aug9_2019_PAAR17-20_box1_sda3.csv",
    inventory_data=NA,
    cycle_start=7,
    cycle_end=15,
    chop_start=10/60,
    chop_end=0,
    flush_plot="OFF",
    N_Ch=4,
    path="LocalSDAFolders",
    date_format = "m/d/y")

## -- > outcome - we have all slopes extrected. no MO2 values etc.

# 5. combine smr files now files  (if needed)
# change WD first
setwd("../csv_analyzed/")
combine_smr(smr_files=c("Aug7_2019_PAAR17-20_box1_sda_analyzed.csv", "Aug8_2019_PAAR17-20_box1_sda2_analyzed.csv", "Aug9_2019_PAAR17-20_box1_sda3_analyzed.csv"), date_format = "m/d/y")

# 6. Run the MMR_SMR_AS_EPOC function to get SMR values, and EPOC
MMR_SMR_AS_EPOC(data.SMR ="Aug7_2019_PAAR17-20_box1_sda_analyzed_GLUED_n3.csv",
                data.MMR ="none",
                AnimalID = c("hawk1","hawk2", "hawk3","hawky4"),
                BW.animal = c(0.005, 0.006, 0.01, 0.01),
                resp.V = c(0.300, 0.300, 0.300,0.300),
                r2_threshold_smr = 0.9,
                date_format = "m/d/y",
                drop_ch = NULL,
                N_Ch = 4,
                min_length_mmr = 60,
                MLND = FALSE,
                epoc_threshold = 1,## must have
                end_EPOC_Ch = c(NA, NA, NA, 1), # in minutes?
                recovMMR_threshold = 0.5,
                background_prior = "Aug7_2019_PAAR17-20_box1_sda_analyzed_BACKPRE_dummy.csv",
                background_post = "Aug7_2019_PAAR17-20_box1_sda_analyzed_BACKPOST_dummy.csv",
                background_linear_gr = TRUE,
                # background_slope = NULL, background.V = NULL,
                match_background_Ch = TRUE,
                path = "Foldering")

# 7. run thse SDA file to get SMR and the each hour lowest value, etc
# change wd first


# SDA fresh run - Feb 9 2020
setwd("/Users/kristakraskura/Github_repositories/Respirometry_Performances/Test/SDA/csv_input_files/")

SDA(data.SDA ="Jul20_2019_PAAR8-16_box1_sda_analyzed_GLUED_n4.csv",
    analyzed_MR="Jul19_2019_PAAR_box1_smr_analyzed_MR_analyzed.csv",
    AnimalID = c("PAAR9","PAAR10","PAAR11","PAAR12"),
    BW.animal = c(0.002,0.002,0.001,0.002),
    resp.V = c(0.76, 0.78, 0.78, 0.78),
    r2_threshold_smr = 0.9, date_format = "m/d/y",
    drop_ch = c(3,4), N_Ch = 4,
    SMR_calc = FALSE, # FALSE for no SMR calculations ## must have
   # SMR_vals = c(NA,NA,NA,NA), ## must have
    MLND = FALSE,
  #  end_SDA_Ch = c(26,26, NA, NA), # in h
    #sda_threshold = "SMR_mean10minVal",
    sda_threshold_level = c("SMR_mean10minVal", 1.15),## must have
    background_prior = "Jul19_2019_PAAR_box1_back2_analyzed.csv",
    background_post = "Jul22_2019_PAAR_box1_back3_analyzed.csv",
    background_linear_gr = TRUE,
    # background_slope = NULL, background.V = NULL,
    match_background_Ch = TRUE,
    path = "SDAfolders",
    handling_delay = 0,
    begin_smr_hr_zero=TRUE) # in minutes, currently not used





## config 1:
setwd("/Users/kristakraskura/Github_repositories/Respirometry_Performances/Test/Archived Test Jan 2020/SDA/csv_input_files/")
SDA(data.SDA ="Aug7_2019_PAAR17-20_box1_sda_analyzed_GLUED_n3.csv",
    # analyzed_MR="jul04_2019_opasize_box4_mmr_MR_analyzed.csv",
    AnimalID = c("hawk1","hawk2", "hawk3","hawky4"),
    BW.animal = c(0.005, 0.006, 0.01, 0.01),
    resp.V = c(0.300, 0.300, 0.300, 0.300),
    r2_threshold_smr = 0.9,
    date_format = "m/d/y",
    # drop_ch =,
    N_Ch = 4,
    SMR_calc = TRUE, # FALSE for no SMR calculations ## must have
    # SMR_vals = c(2,2.1, 2.2, 2.3), ## must have
    MLND = FALSE,
    end_SDA_Ch = c(NA), # in h
    # sda_threshold = "SMR_mean10minVal",
    sda_threshold_level = c("SMR_mean10minVal", 1.5),## must have
    background_prior = "Aug7_2019_PAAR17-20_box1_sda_analyzed_BACKPRE_dummy.csv",
    background_post = "Aug7_2019_PAAR17-20_box1_sda_analyzed_BACKPOST_dummy.csv",
    background_linear_gr = TRUE,
    # background_slope = NULL, background.V = NULL,
    match_background_Ch = TRUE,
    path = "SDAfolders",
    handling_delay = 0,
    begin_smr_hr_zero = TRUE) # in minutes, currently not used







# run smoothly:
# coditions:
# - it used the provided values, did not do SMR from the files
# - it still calculated the SMR etc using the individual data, respo volumes, and backgrounds
# - have SDA plots
# - dont have 'all together' file, etc as the SMR_calc = FALSE

# config 2:
 # "SMR_mean10minVal","SMR_low10quant", "SMR_low15quant", "SMR_low20quant", "smr_mlnd", "SMR_vals"),
  SDA(data.SDA ="Aug7_2019_PAAR17-20_box1_sda_analyzed_GLUED_n3.csv",
    analyzed_MR="jul04_2019_opasize_box4_mmr_MR_analyzed.csv",
    AnimalID = c("hawk1","hawk2", "hawk3","hawky4"),
    BW.animal = c(0.005, 0.006, 0.01, 0.01),
    resp.V = c(0.300, 0.300, 0.300,0.300),
    r2_threshold_smr = 0.9, date_format = "m/d/y",
    drop_ch = NULL, N_Ch = 4,
    SMR_calc = TRUE, # FALSE for no SMR calculations # and for using external SMR values
    SMR_vals = c(NA,NA,NA,NA,NA), ## must have
    MLND = FALSE,
    sda_threshold = "SMR_mean10minVal",
    # sda_threshold = "SMR_vals",## must have
    # background_prior = NA, background_post = NA,
    # background_slope = NULL, background.V = NULL,
    # match_background_Ch = FALSE,
    path = "SDAfolders",
    feeding_delay = 0) # in minutes

# SMR value for  Ch1  is:  2.68
# SMR value for  Ch2  is:  2.29
# SMR value for  Ch3  is:  0.77
# SMR value for  Ch4  is:  1.21

  # config 3, all same as # 2 but SMR_calc = FALSE
  SDA(data.SDA ="Aug7_2019_PAAR17-20_box1_sda_analyzed_GLUED_n3.csv",
    analyzed_MR="jul04_2019_opasize_box4_mmr_MR_analyzed.csv",
    AnimalID = c("hawk1","hawk2", "hawk3","hawky4"),
    BW.animal = c(0.005, 0.006, 0.01, 0.01),
    resp.V = c(0.300, 0.300, 0.300,0.300),
    r2_threshold_smr = 0.9, date_format = "m/d/y",
    drop_ch = NULL, N_Ch = 4,
    SMR_calc = TRUE, # FALSE for no SMR calculations # and for using external SMR values
    SMR_vals = c(NA,NA,NA,NA,NA), ## must have
    MLND = FALSE,
    sda_threshold = "SMR_low10quant",
    # sda_threshold = "SMR_vals",## must have
    background_prior = NA, background_post = NA,
    background_slope = NULL, background.V = NULL,
    match_background_Ch = FALSE,
    path = "SDAfolders",
    handling_delay = 3) # in h

# Use SMR values from previosly analyzed data
# SDA data: 4 MO2 measurements each hour;  42 total hours
# SMR value for  Ch1  is:  0.95
# Respiration post digestion does not reach chosen SMR levels: 0.95  - suggest changing the sda_threshold
# SMR value for  Ch2  is:  0.91
# Respiration post digestion does not reach chosen SMR levels: 0.91  - suggest changing the sda_threshold
# SMR value for  Ch3  is:  1.14
# SMR value for  Ch4  is:  0.91
# Respiration post digestion does not reach chosen SMR levels: 0.91  - suggest changing the sda_threshold



# SDA(data.SDA= analyzed_MR="Aug7_2019_PAAR17-20_box1_sda_analyzed_MR_analyzed.csv",
#     analyzed_SMR="Aug7_2019_PAAR17-20_box1_sda_analyzed_SMR_analyzed.csv",
#     SMR_calc = FALSE, # FALSE for no SMR calculations ## must have
#     SMR_vals = c(2,2.1, 2.2, 2.3), ## must have
#     sda_threshold = "SMR_vals",## must have
#     path = "SDAfolders",
#     feeding_delay = 0) # in minutes
# defaults
SDA(data.SDA=NULL, analyzed_MR=NULL, analyzed_SMR=NULL, AnimalID=NULL, BW.animal=NULL, resp.V=NULL,
                r2_threshold_smr, date_format = c("m/d/y","d/m/y","y-m-d"),
                drop_ch = NULL, N_Ch = 4,
                SMR_calc = TRUE,
                MLND = TRUE,
                SMR_vals = c(NA, NA, NA, NA),
                sda_threshold = c("SMR_mean10minVal","SMR_low10quant", "SMR_low15quant", "SMR_low20quant", "smr_mlnd", "SMR_vals"),
                background_prior = NA, background_post = NA,
                background_slope = NULL, background.V = NULL,
                match_background_Ch = FALSE,
                path = ".",
                feeding_delay = 0)




#___ end
