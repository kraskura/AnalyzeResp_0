



<div class="page-header">
  <h2 style="font-size: 30px; text-align:left;">function</h2>
  <p>Source: <a href = "#" style = "color:gray;">kraskura/AnalyzeResp_0</a></p>
</div>

<br>
`function`
<br

<h4 style="color:#495057; font-weight: bold;  text-align: left">About</h4>
<ul class="breadcrumb" style="background-color:#fff; border:none; color:#3e3f3a">
  <li><a href="#Arguments">Arguments</a></li>
  <li><a href="#Value">Value</a></li>
  <li><a href="#Details">Details</a></li>
  <li><a href="#Example">Example</a></li>
  <li><a href="#"><i class="fas fa-download"></i>&nbsp; See datafiles on GitHub</a></li>
</ul>


<h4 style="color:#495057; font-weight: bold;  text-align: left">Description</h4>

<!-- <h4 style="color:#495057; font-weight: bold;  text-align: left">Before running the SMR function </h4><br> -->

<h4><a style="color:#495057; font-weight: bold;  text-align: left" name="Arguments">Arguments</a></h4>



  Argument       | Description | Default
---: | :------------- | :---
            |  | 
            |  | 
            |  | 
            |  | 

<h4><a name="Value" style="color:#495057; font-weight: bold;  text-align: left">Value</a></h4> <br>

<h4><a name="Details" style="color:#495057; font-weight: bold;  text-align: left">Details</a></h4> <br>

<h5 style="color:#495057; font-weight: bold;  text-align: left; text-indent:+10px;"><i class="fas fa-ellipsis-h"></i>&nbsp; If enabling automatic output file distribution in local directories</h4>
<h5 style="color:#495057; font-weight: bold;  text-align: left; text-indent:+10px;"><i class="fas fa-ellipsis-h"></i>&nbsp; Data outputs</h4>

1. .csv files
2. .png files

<br>
<h5 style="color:#495057; font-weight: bold;  text-align: left; text-indent:+10px;"><i class="fas fa-ellipsis-h"></i>&nbsp; Messages on Console</h4>



<br>
<h5 style="color:#495057; font-weight: bold;  text-align: left; text-indent:+10px;"><i class="fas fa-ellipsis-h"></i>&nbsp; Support functions for SMR</h5>
- `` Arguments: <a style="color:grey">  </a>
- `` Arguments: <a style="color:grey"></a>
- `C` Arguments: <a style="color:grey">  </a>


<br>
<h4><a name="Example" style="color:#495057; font-weight: bold;  text-align: left">Example</a></h4> <br>


<pre class = "sourceCode yaml"> 


example
example
example
example




</pre>















#### 2. Convert raw FireSting data files to a usable format

**Function**: `txt_csv_convert`

The original FireSting datafiles are in _.txt_ format and cannot be used with this analysis package. Use this function to convert _.txt_ files into a _.csv_ format that can be used in the next steps. Note, this function only takes a portion of data available in the original file.  

Argument   | Description
----------- | -----------
txt_file    | The original _.txt_ file from FireSting. No default
N_Ch        | The number of FireSting channels. Options include 2, 4, 8. Note, if either a 2- or 4-Channel FireSting was used, this argument can be ignored. Default = `4`
path        | Specify the folder where all converted _.csv_ files will be saved. It can be either `AUTO`, `MANUAL`, `BACTERIAL_RESP`, or `SDA`. The default is a local directory (`"."`)




#### 3. Need splitting data files?
**Function**: `csv_file_split`

Use this function to split your files in 2 parts at a defined time point. Most commonly this is needed when MMR and SMR parts are together in one file. For example, when manually controlled part of the data recording (manually initiated flush:measure cycles; typically MMR) is immediately followed by automatic data recording (auto timer controlled flush:measure cycles; typically SMR/RMR). These two parts are analyzed using different functions and so need to be in different _.csv_ files.

Argument        |   Description
-----------     | -----------
data            | Data _.csv_ to be split in two (manual/MMR and auto/SMR _.csv_ files). Input must be a character.  No default
split_data_name | The name that will be used to save all output files. Input must be a character. No default
cycle_full_min  | cycle length (min) that the auto file was run on, this includes both flush + measure; no default. No default
timeSplit       | The time at which the file will be split in two (the split time (min)). This time will be shown in output plots. No default
date_format     | The date format. It must be provided, and can be either: `"m/d/y"`, `"d/m/y"`, `"y-m-d"`. The Default = `NULL`
split           | Logical argument. When `TRUE` the original file will be cut and 2 _.csv_ saved in the local directory. Default=FALSE
N_Ch            | The number of FireSting channels. Options include 2, 4, 8. Note, if either a 2- or 4-Channel FireSting was used, this argument can be ignored. Default = `4`         



#### 4. Combining multiple SMR or AUTO data files
**Function**: `combine_smr`

There may be more than one AUTO file recorded for one respirometry trial. Use this function to combine them all together to then calculate SMR/RMR using all measurement cycle values, and to measure recovery.

 Argument   | Description
----------- | -----------
smr_files   | A character vector of _..._analyzed.csv_ . These are output files from `SMR` function. 2 or more files must be specified. No default
date_format | The date format used in the original FireSting files. Must specify one of the following: `c("m/d/y","d/m/y","y-m-d")`
path        | Specify working directory for all output files. This argument has two options, i) files can be automatically organized in newly created folders, any character may be used, ii) the default is a local directory (`"."`).


#### 5. Understanding system-wide background respiration rates

**Function**: `sum_backgr`

This function summarises many background files. It is useful when trying to better understand system-wide respiration rate. For example, it may be used to explore the variability of background respiration rates between different respirometry chambers, or to explore overtime trends. This is shorthand way to get background MO2 <a style="color:grey">mgO2/L/h<a> through `MMR_SMR_AS_EPOC` function.
_Note: this is a developing function_

 Argument   | Description
----------- | -----------
data        | Data file _..._analyzed.csv_ of respiration . No default
Ch          | Specify Channels for analysis. A numeric vector. No default
resp.ID     | A character vector with respiromtery IDs. No default
resp.Vol    | A numeric vector of respirometry volumes. No default

<p>&nbsp;</p>
<h3 style="color:white; background-color: #4682B4; font-weight: bold;  text-align: center"> Maximum metabolic rate, MMR </h3>

#### 9. Estimating digestion costs.
**Function**: `SDA`

Clearly still working on this...

   Argument          | Description
-------------------  | -----------
data.SDA             | = `NULL`
analyzed_MR          | = NULL
SMR_calc             | = TRUE,
SMR_vals             | = c(NA, NA, NA, NA),
sda_threshold        | = c("SMR_mean10minVal","SMR_low10quant", "SMR_low15quant", "SMR_low20quant", "smr_mlnd", "SMR_vals"),
AnimalID             | No default
BW.animal            | No default
resp.V               | No default
r2_threshold_smr     | No default
date_format          | = c("m/d/y","d/m/y","y-m-d"),
drop_ch              | = NULL,
N_Ch                 | = 4,
MLND                 | = TRUE,
background_prior     | = NA,
background_post      | = NA,
background_slope     | = NULL,
background.V         | = NULL
background_linear_gr | = FALSE
match_background_Ch  | = FALSE,
path                 | = ".",
feeding_delay        | = 0)

 Supplementary to SDA function:
 - `SDA.spar` Arguments: spar,d, SDAdata, b, peak_SDA_all, time_peak_SDA_all, sda_threshold



<h3 style="color:white; background-color: #4682B4; font-weight: bold;  text-align: center">Miscellaneous functions, old work </h3>      

9. MMRslide_tunnel<-function(file)

10. calcSMR = function(Y, q=c(0.1,0.15,0.2,0.25,0.3), G=1:4) sourced from [Chabot et al 2016](https://doi.org/10.1111/jfb.12845)




