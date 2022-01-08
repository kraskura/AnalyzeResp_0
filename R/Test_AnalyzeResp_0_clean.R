


# last updated: Jun 8, 2021

# AQUATIC RESPIROMETRY ANALYSIS: ---------

# 1. import source code and setwd()
source("/Users/kristakraskura/Github_repositories/AnalyzeResp_0/R/AnalyzeResp_0.R")

#  set working directory with raw (.txt) Firesting data files. This is the location where all analysis output files will be saved and organized
setwd("/Users/kristakraskura/Github_repositories/AnalyzeResp_0/Test/")

# 2. create all working directories 
organize_MR_analysis(create = "Full")# bacterial respiration has a similar format than that for auto cycles. 

# Setup: convert txt files to csv ------------
# 3. convert all raw firesting data files into csv files and automatically save them in "csv_files" subfolders, either in AUTO or MANUAL or bacterial main folders, accordingly. 
# if path is not specified then just save converted files in the current directory

txt_csv_convert(txt_file = "./jul04_2019_opasize_box4_mmr.txt",  path = "MANUAL" , N_Ch = 4)
txt_csv_convert(txt_file = "./jul04_2019_opasize_box4_smr.txt",  path = "AUTO" , N_Ch = 4)

# Convert all csv files from air saturation (AS) to mgO2.L ------------
# The output files will be saved in the current working directory in a newdly created ./csv_files_mgO2L

# For a single file example: 
convert.o2.Firesting(csv.data = "FileToCovert.csv", n_ch = 4, sal = 35)

# creating a loop to do this quickly and efficiently: un-comment and use for any folder accordingly. 
    # setwd("")
    # n.mmr.files<-length(list.files(""))
    # mmr.files<-list.files("")
    # 
    # for (i in 1:n.mmr.files){
    #   
    #   mr.file<-mmr.files[i]
    #   convert.o2.Firesting(csv.data = mr.file, n_ch = 4, sal = 35)
    # }


# Split files: MMR and SMR recorded on one long file ----------

# The files can be split, best before running either MMR and SMR functions. 
# set a your working directory, this is a folder with files that will be split
# It is convenient and clean to keep the split files in a separate folder.
setwd("../../SplitFiles/") # all outputs are saved in local directory 

# 8 ch Firesting example
csv_file_split(data="Sep24_2019_box0_fullfile.csv",
               split_data_name="Sep24_2019_box0_SPLIT-TEST-8ch",
               cycle_full_min=10,
               timeSplit=42,
               split=TRUE,
               N_Ch=8,
               date_format="d/m/y")


# 1. MMR functions. ---------
setwd("../MANUAL/csv_files/")

MMR(data.MMR = "jul04_2019_opasize_box4_mmr.csv", 
    cycles = 2, 
    cycle_start = c(0,9.85),
    cycle_end = c(3.8,14.1),
    mmr_Ch1 = 1, mmr_Ch2 = 2, mmr_Ch3 = 1, mmr_Ch4 = 2,
    clean_Ch1=c(1.2,3.5), 
    clean_Ch2=c(0,0),
    clean_Ch3=c(0,0),
    clean_Ch4=c(0,0),
    path ="Folders",
    N_Ch = 4,
    date_format = "m/d/y",
    inv.data = NA)

# with inventory file added
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
# the inv.data is specific for a channel (see the excel/csv attached)

        
# 2. SMR functions. ---------

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
    inventory_data= "smr_inventory_test.csv", # inventory file must be in the dir with the data files indicated above
    cycle_start=8, # length of a full cycle, with open and closed phases together
    cycle_end=15, # this will take first 10 sec off as a wait period, can be decimal
    chop_start=10/60,
    chop_end=0,
    flush_plot="OFF",
    N_Ch=4,
    path="UseFolders",
    date_format = "m/d/y")

# 2.2 SMR- for background respiration. 
# Use SMR function on background respiration files as well.

# 2.3 SMR - combined auto files together. -------------

# Combine files:  -------------
# AUTO recorded files, but with different flush:measure cycle lengths. Combine them all to have one full file.
# the one combined file with name "GLUED_nX" (x = n of files) will be saved in the local directory and will be automatically placed in the folder "input_files" for full AS, EPOC etc. analysis (see below)

setwd() # set accordingly

combine_smr(smr_files=c("../../accessory_Combine/aug12_2021_box3_SMR1_mgO2L_analyzed.csv",
                        "../../accessory_Combine/aug12_2021_box3_SMR2_mgO2L_analyzed.csv"),
            date_format = "m/d/y")


# sum background  --------
# for system wide background resp estimates

sum_backgr(data = "jul04_2019_box4_back_post_dummy_analyzed.csv", # set the appropriate dir()
          Ch = c(1,2,3,4),
          resp.ID = c("A", "B", "D", "E"),
          resp.Vol = c(1.5, 1.5, 1.5, 1.5))
          
 
# Full analysis: SMR, MMR, AS, EPOC ------

# customized:
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

# relying on defaults
MMR_SMR_AS_EPOC( data.MMR = "jul04_2019_opasize_box4_mmr_analyzed.csv",
                 data.SMR = "jul04_2019_opasize_box4_smr_analyzed.csv",
                 AnimalID = c("23-rbred","27-ltred","27-lbred","27-rtred"),
                 BW.animal = c(0.065,0.068,0.061,0.062),
                 resp.V = c(1.890,1.890,1.4475,1.4475),
                 r2_threshold_smr = 0.85,
                 r2_threshold_mmr = 0.9,
                 min_length_mmr = 120)

