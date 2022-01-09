
<div class="page-header">
<h2 style="font-size: 30px; text-align:left;">Quality Control - our perspective</h2>
  <p>Source: <a href = "#" style = "color:gray;">https://github.com/kraskura/AnalyzeResp_0/blob/ccf6f96e629939be6b5f33f88e435a7b973e6fff/docs/QC_protocol.md</a></p>
</div>


<h4 style="color:#495057; font-weight: bold;  text-align: left">Data Analysis Pipeline</h4>
 
1. Visualize Data
    - Plot all MO2 data using AnalyzeResp code
    - Visually inspect all MO2 plots (all files - background, MMR, SMR)

2. Clean Data
    - Identify all clean MO~2~ data with linear decline - keep
    - Identify any plots with minor issues - correct
    - Identify erroneous MO~2~ plots - remove

3. Background
    - Assess if background is present
    - If background is present, correct all MO~2~ slopes for background respiration (see Rosewarne et al 2016 for guidance)
    - We generally assume a linear increase between the “before” and “after” background and apply that to the slopes.

4. Plot the data and calculate MR values
    - ALWAYS plot MO~2~ vs time and temperature vs time. ESSENTIAL for every fish.
    - Calculate SMR, RMR, MMR, EPOC, time to 50% MMR (Time[MMR50]) or 75% MMR (Time[MMR75])
    - Many ways to do this – talk to collogues and peers (i.e. which sliding measurement window; which SMR value; which EPOC threshold)

5. Assess metabolic scaling
    - Plot the data – SMR vs body mass; MMR vs body mass (log-log plots and raw data plots)
    - Adjust for scaling if needed (see above).

<h4 style="color:#495057; font-weight: bold;  text-align: left">Additional notes and suggestions from the Eliason Lab</h4>

1. Check for leaks
    - Plot flush plot ON
    - Are L shapes consistent across files? If yes, you might have a leak.

2. _Notes and suggestions:_ 
    - Do not cut the recovery or resting metabolic rate measurement down to below the “sliding window” length selected for MMR.
    - For cleaning: R^2^ below 0.7 are immediately rejected-- no cleaning
    - Experimental notes: If there are any measurements with a complementary note that anyone was messing with the respirometry chamber (relieving a bubble, etc) or where data is missing from the measurement (low signal in probe can cause this), it is aadvised to exclude the measurement. 

<h4 style="color:#495057; font-weight: bold;  text-align: left"> A sequence of our identified occasianal anomolies and our suggested solutions</h4>

<div style="background-color: #f8f5f0;  border: 2px solid #484f4f; padding: 10px">

<img src= "./files/resources/qc_Eliason_lab/slide5.png" alt="scatter"></img>
<img src= "./files/resources/qc_Eliason_lab/slide6.png" alt="scatter"></img>
<img src= "./files/resources/qc_Eliason_lab/slide7.png" alt="scatter"></img>
<img src= "./files/resources/qc_Eliason_lab/slide8.png" alt="scatter"></img>
<img src= "./files/resources/qc_Eliason_lab/slide9.png" alt="scatter"></img>
<img src= "./files/resources/qc_Eliason_lab/slide10.png" alt="scatter"></img>
<img src= "./files/resources/qc_Eliason_lab/slide11.png" alt="scatter"></img>
<img src= "./files/resources/qc_Eliason_lab/slide12.png" alt="scatter"></img>
<img src= "./files/resources/qc_Eliason_lab/slide13.png" alt="scatter"></img>
<img src= "./files/resources/qc_Eliason_lab/slide14.png" alt="scatter"></img>
<img src= "./files/resources/qc_Eliason_lab/slide15.png" alt="scatter"></img>


</div>
