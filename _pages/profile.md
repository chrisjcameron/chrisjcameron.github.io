# Christopher J. Cameron - Portfolio
--

### Writing

- [**Dissertation**](#dissertation)  
- [**Publications**](#publications)  
- [**Presentations**](#presentations)  
- [**Other Publications & Patents**](#other-publications-&-patents)  
- [**Working Papers**](#working-Papers)  

--  
### Selected projects and research code that support my research. 

- [**Respondent Driven Sampling Estimation and Simulation**](#1.-rds)  
- [**Real time contagion monitor for social media**](#2.-contagion-monitor)  
- [**Spatial Cluster Detection**](#3.-spatial-burst-detection)  

-- 

#### 1. RDS
**Respondent Driven Sampling (RDS) support code**  
[RDS Code ```[R][Python]```](RDS)

Respondent driven sampling is a methodology for the study of hidden populations that combines a peer referral sampling strategy with case weights derived from social network data to yield a probability sample and bias corrected estimates. I have several papers in this area, supported by R code I developed to produce RDS point and variance estimates and Python code to simulate the recruitment and sampling process on a variety of network topologies with different assumptions about the underlying population characteristics. 

The simulation process involves code written in R and Python. I designed the Python code to manage the simulation and then to automatically call R to process the simulation output and produce data tables. The simulation output is designed to be self documenting -- the python parameters used to generate the simulated data are encoded as comments in the final file. 


#### 2. Contagion Monitor  
**Real time burst detection for contagion monitoring**  
[Contagion Monitor ```[Python][Bash]```](contagion_monitor)

Research questions involving virality and trend detection require collecting data before the token becomes widely used. The vast majority of token never go viral, so collecting useful data requires sifting through a huge amount of live data and proactively collecting data on only a small number of candidate tokens. I developed this code as the initial filter for real-time monitoring of social media feeds. In this particular instance, it connects to a live global twitter feed. The design is based on shifted-wavelet trees to detect bursts of related tokens within an interval and includes aggressive culling of data in memory for efficiency.  I implemented the code using Python co-routines as way to familiarize myself with asynchronous co-routine logic (coroutines are now deprecated in favor of async). The data processing pipeline was composed of chains of co-routines that extracted and counted various types of tokens and fed the resulting tokens to their corresponding trackers. The trackers would emit reports to an accumulator function, which could connected to a persistent data store like MongoDB and trigger further data collection. 

This work supported my research in complex contagion on social media and is part of the work included in U.S. Provisional Application No. 62/592,554.

![Conceptual Illustration of process](contagion_monitor/swm_pipeline_and_composition.png)



#### 3. Spatial Cluster Detection  
[Burst Detection 2D ```[Python]```](burst_detection_2D)  
[Technical Writeup](burst_detection_2D/spatial-burst-detection-2D.pdf)

Despite its popularity in the literature, fixed grid counting is inappropriate for spatial burst detection. The arbitrary placement of the dividing lines can create counting issues for point clouds that are not centered at coordinates evenly divisible by .0005 degrees latitude and longitude. These off-center point clouds spill into neighboring grid cells, diluting the count. In the worst case, a point cloud divided over 4 cells would appear to be as significant as a well centered point cloud that was 4 times smaller.

![Worst case for fixed grid](burst_detection_2D/bad_case_400px.png)

As demonstrated in the [technical writeup](burst_detection_2D/spatial-burst-detection-2D.pdf), shifted wavelet trees can guarantee that all concentrations within an arbitrary sized region will be detected without significantly increasing computational load. The image below shows how the worst case example is handled by a SWT approach. 

![Worst case solved by SWT](burst_detection_2D/2x_box_400px.png)






-- 

### Dissertation

Theoretical models of social influence predict sudden outbreaks of viral spread when adoption requires reinforcement from multiple peers. My analysis demonstrates that viral outbreak of such complex contagions depends on both the number and the distribution of long range ties in the underlying social network. A survey of social networks revealed more robust connectivity than previously assumed and simulation showed contagion is more robust but less outbreak-prone on networks with shorter-range bridging ties. The empirically informed models I present in this work provide new insight about the
potential outcome and dynamics of complex contagion processes in human social networks.

[Sample - Chapter 3](publications/cameron_dissertation_sample_ch3.pdf)   
[Full Dissertation](publications/Cameron_dissertation_2016_Complex_contagion_in_social_networks.pdf)



### Publications

[Most articles are available as pdfs in the publications folder](publications)

Sirianni, A. D., Cameron, C. J., Shi, Y., Heckathorn, D. D. (2021) "Bias Decomposition and Estimator Performance in Respondent-Driven Sampling." *Social Networks*, 64 (January 1, 2021): 109–21. doi:10.1016/j.socnet.2020.08.002.

Barash V., Fink C., Cameron, C., Schmidt, A., Dong, W.,  Macy, M., Kelly J., \& Deshpande, A. "A Twitter Social Contagion Monitor." 2020 IEEE/ACM International Conference on Advances in Social Networks Analysis and Mining (ASONAM), *accepted*.

Berry, G., Cameron C, Park P, Macy M. (2019). "The Opacity Problem in Social Contagion." *Social Networks*, 56, 93–101. doi:10.1016/j.socnet.2018.09.001

Schmidt A., Fink C., Barash V., Cameron C., Macy M. (2018). "Using Spectral Clustering of Hashtag Adoptions to Find Interest-Based Communities" IEEE International Conference on Communications, 2018 doi:10.1109/ICC.2018.8422244 

Cameron, C. J., \& Macy, M. (2017). "The local dynamics of institutional change." *Rationality and Society*, 29(1), 69–79. doi:10.1177/1043463116685663

Heckathorn, D. D., \& Cameron, C. J. (2017). "Network sampling." *Annual Review of Sociology*, 43 (1). doi:10.1146/annurev-soc-060116-053556

Shi, Y., Cameron, C. J., \& Heckathorn, D. D. (2016). "Model-based and design-based inference reducing bias due to differential recruitment in respondent-driven sampling." *Sociological Methods \& Research*. doi:10.1177/0049124116672682

Fink, C., Schmidt, A., Barash, V., Cameron, C., \& Macy, M. (2016). "Complex contagions and the diffusion of popular twitter hashtags in Nigeria." *Social Network Analysis and Mining*, 6(1), 1. [https://rdcu.be/7BCh](https://rdcu.be/7BCh)

Fink, C., Schmidt, A., Barash, V., Kelly, J., Cameron, C., \& Macy, M. (2016). "Investigating the observability of complex contagion in empirical social networks." In *ICWSM* (pp. 121–130).  [https://www.aaai.org/ocs/index.php/ICWSM/ICWSM16/paper/view/13143](https://www.aaai.org/ocs/index.php/ICWSM/ICWSM16/paper/view/13143)

Barash, V., Cameron, C. J., Spiller, M. W., \& Heckathorn, D. D. (2016). "Respondent-driven sampling–testing assumptions: Sampling with replacement." *Journal of Official Statistics*, 32(1), 29–73. doi:10.1515/jos-2016-0002

Barash, V., Cameron, C. J., \& Macy, M. (2012). "Critical phenomena in complex contagions." *Social Networks*, 34(4), 451–461. doi:10.1016/j.socnet.2012.02.003

### Other Publications & Patents

U.S. Provisional Application No. 62/592,554. "Method and apparatus for monitoring complex contagion and critical mass in online social media" (jointly filed November 30 2017).

Spiller, M., Cameron, C., Heckathorn D. (2015) *RDSAT 8.1 User Manual*. [respondentdrivensampling.org](respondentdrivensampling.org)
    
Spiller, M., Cameron, C., Heckathorn D. (2012) *RDSAT 7.1 User Manual}. [respondentdrivensampling.org](respondentdrivensampling.org)  

### Presentations

"A New Method to Reduce Overestimation of Thresholds with Observational Network Data." George Berry and Christopher Cameron. American Sociological Association Annual Meeting. August 2017.

"Estimating Thresholds in Empirical Social Contagion." George Berry and Christopher Cameron. INSNA Sunbelt Conference April 2016. 
   
"Investigating the Observability of Complex Contagion in Empirical Social Networks." Clay Fink, Aurora Schmidt, Vladimir Barash, John Kelly, Christopher Cameron, Michael Macy. ICWSM May 2016.
      
"Respondent-Driven Sampling: Origins, Current Developments, and Alternative Estimators." Douglas D. Heckathorn, Christopher Cameron, and Yongren Shi. Proceedings of the International Methodology Symposium 2013: Producing reliable estimates from imperfect frames, Statistics Canada. Published October 21, 2014.

"Statistical and Sociological Development of Respondent-Driven Sampling." Douglas D. Heckathorn, Simon Frost, Christopher Cameron and Vladimir Barash. Facilitating Interdisciplinary Research: Methodological and Technological Innovation in the Behavioral and Social Sciences, Washington D.C., October 9, 2009.   

### Working Papers

R.H. Bernstein, M.W. Macy, C.J. Cameron, S. Williams-Ceci, W.M. Williams, and S.J. Ceci. "Assessing Gender Bias in Particle Physics and Social Science Recommendations for Academic Jobs."
    
Cameron, C. "The relative shortness of long ties: a common pattern in the empirical distribution of tie range"
    
Cameron, C. "Complex contagion and the distribution of long range ties"


