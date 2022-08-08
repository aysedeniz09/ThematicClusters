AN_Thematic_Github
================
Ayse D Lokmanoglu
2022-07-17

``` r
knitr::opts_chunk$set(echo = TRUE)
knitr::opts_chunk$set(message = FALSE)
knitr::opts_chunk$set(warning = FALSE, message = FALSE) 
```

### Supplementary Code for:

### Textual Messaging of ISIS’s al-Naba and the Context Drivers that Correspond to Strategic Changes

#### Authors: Ayse D. Lokmanoglu, PhD; Carol K. Winkler, PhD; Monerah AlMahmoud, PhD; Kayla McMinimy; Katherine Kountz
#### Citation: Lokmanoglu, A. D., Winkler, C. K., Al Mahmoud, M., McMinimy, K., & Kountz, K. (2022). Textual Messaging of ISIS’s al-Naba and the Context Drivers That Correspond to Strategic Changes. Studies in Conflict & Terrorism, 1–25. https://doi.org/10.1080/1057610X.2022.2109245


The code includes all the steps of the regression analysis, and the
visualizations.

The data set accompanying the code: <https://doi.org/10.7910/DVN/FAP771>

For questions, or more information on the code please contact: Ayse D.
Lokmanoglu  
ayse \[dot\] lokmanoglu \[at\] northwestern \[dot\] edu

``` r
library(forecast)
library(tseries)
library(dynlm)
library(vars)
library(jtools)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(modelsummary)
library(ggthemes)
library(wesanderson)
library(ggrepel)
library(ggpubr)
library(grid)
library(lubridate)
library(betareg)
library(sjPlot)
library(readr)
library(ggeffects)
library(stargazer)
```

##### Regression Analysis

Load dataset

``` r
mydata <- read.csv("https://dataverse.harvard.edu/api/access/datafile/5736741",
                   sep='\t',
                 header=TRUE)

## change date column to date and remove the index column from csv
mydata <- mydata %>%
  mutate(date=ymd(date)) %>%
  dplyr::select(-A)
glimpse(mydata)
```

    ## Rows: 63
    ## Columns: 14
    ## $ date                       <date> 2015-10-01, 2015-11-01, 2015-12-01, 2016-0~
    ## $ thememonth1                <dbl> 0.2586284, 0.2237901, 0.2326896, 0.3065204,~
    ## $ thememonth2                <dbl> 0.09373305, 0.11874316, 0.08649945, 0.12167~
    ## $ thememonth3                <dbl> 0.6476385, 0.6574667, 0.6808109, 0.5718064,~
    ## $ territorykm2               <dbl> 106399, 103453, 103486, 103518, 102745, 102~
    ## $ territorychange            <dbl> -2945, -2946, 33, 32, -773, -387, -387, -31~
    ## $ territoryloss              <dbl> 2945, 2946, 0, 0, 773, 387, 387, 314, 0, 31~
    ## $ territorygain              <dbl> 0, 0, 33, 32, 0, 0, 0, 0, 0, 0, 918, 918, 0~
    ## $ territorialmonthlychange   <dbl> -2.69333480, -2.76882302, 0.03189854, 0.030~
    ## $ religiousholidaysmuslim    <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0~
    ## $ religiousholidayschristian <dbl> 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1~
    ## $ religiousholidaysjewish    <dbl> 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0~
    ## $ airstrikes                 <dbl> 613, 771, 718, 725, 659, 593, 684, 672, 874~
    ## $ airstrikeslog              <dbl> 6.418365, 6.647688, 6.576470, 6.586172, 6.4~

``` r
mydata %>%
  dplyr::select(thememonth1:territorialmonthlychange, airstrikes:airstrikeslog) %>%
   summary()
```

    ##   thememonth1      thememonth2       thememonth3      territorykm2   
    ##  Min.   :0.1132   Min.   :0.04737   Min.   :0.4307   Min.   :  9132  
    ##  1st Qu.:0.1725   1st Qu.:0.11320   1st Qu.:0.5827   1st Qu.: 23460  
    ##  Median :0.2154   Median :0.16570   Median :0.6296   Median : 27561  
    ##  Mean   :0.2071   Mean   :0.17566   Mean   :0.6172   Mean   : 45651  
    ##  3rd Qu.:0.2377   3rd Qu.:0.23685   3rd Qu.:0.6680   3rd Qu.: 77741  
    ##  Max.   :0.3065   Max.   :0.38675   Max.   :0.7612   Max.   :106399  
    ##  territorychange    territoryloss     territorygain    territorialmonthlychange
    ##  Min.   :-12492.0   Min.   :    0.0   Min.   :   0.0   Min.   :-34.5023        
    ##  1st Qu.: -2762.8   1st Qu.:    0.0   1st Qu.:   0.0   1st Qu.: -7.6751        
    ##  Median :  -339.2   Median :  339.2   Median :   0.0   Median : -0.7467        
    ##  Mean   : -1361.0   Mean   : 2022.3   Mean   : 661.3   Mean   : -1.6328        
    ##  3rd Qu.:   691.4   3rd Qu.: 2762.8   3rd Qu.: 691.4   3rd Qu.:  1.9022        
    ##  Max.   :  9025.1   Max.   :12492.0   Max.   :9025.1   Max.   : 43.1997        
    ##    airstrikes     airstrikeslog  
    ##  Min.   :   0.0   Min.   :0.000  
    ##  1st Qu.:  28.0   1st Qu.:3.332  
    ##  Median : 271.0   Median :5.602  
    ##  Mean   : 409.9   Mean   :4.913  
    ##  3rd Qu.: 686.0   3rd Qu.:6.531  
    ##  Max.   :2817.0   Max.   :7.943

###### Ideology

``` r
gy_logit7 <- betareg(thememonth1 ~ religiousholidaysmuslim, data = mydata)
gy_logit8 <- betareg(thememonth2 ~ religiousholidaysmuslim, data = mydata)
gy_logit9 <- betareg(thememonth3 ~ religiousholidaysmuslim, data = mydata)
gy_logit10 <- betareg(thememonth1 ~ religiousholidayschristian, data = mydata)
gy_logit11 <- betareg(thememonth2 ~ religiousholidayschristian, data = mydata)
gy_logit12 <- betareg(thememonth3 ~ religiousholidayschristian, data = mydata)
gy_logit13 <- betareg(thememonth1 ~ religiousholidaysjewish, data = mydata)
gy_logit14 <- betareg(thememonth2 ~ religiousholidaysjewish, data = mydata)
gy_logit15 <- betareg(thememonth3 ~ religiousholidaysjewish, data = mydata)
```

Print tables for publication

``` r
stargazer(gy_logit7,gy_logit8, gy_logit9,
          align = T, df = F,
          star.char = c("*", "**", "***"),
          star.cutoffs = c(.05, .01, .001),
          ci = TRUE,
          single.row=TRUE,
          omit = c("Constant"),
          column.labels = c("Offline Caliphate", "Online Claiphate", "Regional Enemies"),
          covariate.labels = c("Religious Holidays (Islam)"),
          type = "html",
          digits = 3)
```

<table style="text-align:center">
<tr>
<td colspan="4" style="border-bottom: 1px solid black">
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td colspan="3">
<em>Dependent variable:</em>
</td>
</tr>
<tr>
<td>
</td>
<td colspan="3" style="border-bottom: 1px solid black">
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
thememonth1
</td>
<td>
thememonth2
</td>
<td>
thememonth3
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
Offline Caliphate
</td>
<td>
Online Claiphate
</td>
<td>
Regional Enemies
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
\(1\)
</td>
<td>
\(2\)
</td>
<td>
\(3\)
</td>
</tr>
<tr>
<td colspan="4" style="border-bottom: 1px solid black">
</td>
</tr>
<tr>
<td style="text-align:left">
Religious Holidays (Islam)
</td>
<td>
-0.149 (-0.332, 0.033)
</td>
<td>
0.121 (-0.200, 0.442)
</td>
<td>
0.027 (-0.162, 0.215)
</td>
</tr>
<tr>
<td colspan="4" style="border-bottom: 1px solid black">
</td>
</tr>
<tr>
<td style="text-align:left">
Observations
</td>
<td>
63
</td>
<td>
63
</td>
<td>
63
</td>
</tr>
<tr>
<td style="text-align:left">
R<sup>2</sup>
</td>
<td>
0.041
</td>
<td>
0.008
</td>
<td>
0.001
</td>
</tr>
<tr>
<td style="text-align:left">
Log Likelihood
</td>
<td>
108.986
</td>
<td>
78.651
</td>
<td>
82.082
</td>
</tr>
<tr>
<td colspan="4" style="border-bottom: 1px solid black">
</td>
</tr>
<tr>
<td style="text-align:left">
<em>Note:</em>
</td>
<td colspan="3" style="text-align:right">
<sup>*</sup>p\<0.05; <sup>**</sup>p\<0.01; <sup>***</sup>p\<0.001
</td>
</tr>
</table>

``` r
stargazer(gy_logit10,gy_logit11, gy_logit12,
          align = T, df = F,
          star.char = c("*", "**", "***"),
          star.cutoffs = c(.05, .01, .001),
                    ci = TRUE,
          single.row=TRUE,
          omit = c("Constant"),
                    column.labels = c("Offline Caliphate", "Online Claiphate", "Regional Enemies"),
          covariate.labels = c("Religious Holidays (Christianity)"),
          type = "html", digits = 3)
```

<table style="text-align:center">
<tr>
<td colspan="4" style="border-bottom: 1px solid black">
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td colspan="3">
<em>Dependent variable:</em>
</td>
</tr>
<tr>
<td>
</td>
<td colspan="3" style="border-bottom: 1px solid black">
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
thememonth1
</td>
<td>
thememonth2
</td>
<td>
thememonth3
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
Offline Caliphate
</td>
<td>
Online Claiphate
</td>
<td>
Regional Enemies
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
\(1\)
</td>
<td>
\(2\)
</td>
<td>
\(3\)
</td>
</tr>
<tr>
<td colspan="4" style="border-bottom: 1px solid black">
</td>
</tr>
<tr>
<td style="text-align:left">
Religious Holidays (Christianity)
</td>
<td>
0.048 (-0.124, 0.220)
</td>
<td>
-0.093 (-0.415, 0.229)
</td>
<td>
0.031 (-0.151, 0.212)
</td>
</tr>
<tr>
<td colspan="4" style="border-bottom: 1px solid black">
</td>
</tr>
<tr>
<td style="text-align:left">
Observations
</td>
<td>
63
</td>
<td>
63
</td>
<td>
63
</td>
</tr>
<tr>
<td style="text-align:left">
R<sup>2</sup>
</td>
<td>
0.005
</td>
<td>
0.005
</td>
<td>
0.002
</td>
</tr>
<tr>
<td style="text-align:left">
Log Likelihood
</td>
<td>
107.821
</td>
<td>
78.549
</td>
<td>
82.099
</td>
</tr>
<tr>
<td colspan="4" style="border-bottom: 1px solid black">
</td>
</tr>
<tr>
<td style="text-align:left">
<em>Note:</em>
</td>
<td colspan="3" style="text-align:right">
<sup>*</sup>p\<0.05; <sup>**</sup>p\<0.01; <sup>***</sup>p\<0.001
</td>
</tr>
</table>

``` r
stargazer(gy_logit13,gy_logit14, gy_logit15,
          align = T, df = F,
          star.char = c("*", "**", "***"),
          star.cutoffs = c(.05, .01, .001),
                    ci = TRUE,
          single.row=TRUE,
          omit = c("Constant"),
                    column.labels = c("Offline Caliphate", "Online Claiphate", "Regional Enemies"),
          covariate.labels = c("Religious Holidays (Judaism)"),
          type = "html", digits = 3)
```

<table style="text-align:center">
<tr>
<td colspan="4" style="border-bottom: 1px solid black">
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td colspan="3">
<em>Dependent variable:</em>
</td>
</tr>
<tr>
<td>
</td>
<td colspan="3" style="border-bottom: 1px solid black">
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
thememonth1
</td>
<td>
thememonth2
</td>
<td>
thememonth3
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
Offline Caliphate
</td>
<td>
Online Claiphate
</td>
<td>
Regional Enemies
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
\(1\)
</td>
<td>
\(2\)
</td>
<td>
\(3\)
</td>
</tr>
<tr>
<td colspan="4" style="border-bottom: 1px solid black">
</td>
</tr>
<tr>
<td style="text-align:left">
Religious Holidays (Judaism)
</td>
<td>
-0.017 (-0.199, 0.164)
</td>
<td>
0.024 (-0.303, 0.352)
</td>
<td>
0.022 (-0.166, 0.211)
</td>
</tr>
<tr>
<td colspan="4" style="border-bottom: 1px solid black">
</td>
</tr>
<tr>
<td style="text-align:left">
Observations
</td>
<td>
63
</td>
<td>
63
</td>
<td>
63
</td>
</tr>
<tr>
<td style="text-align:left">
R<sup>2</sup>
</td>
<td>
0.001
</td>
<td>
0.0003
</td>
<td>
0.001
</td>
</tr>
<tr>
<td style="text-align:left">
Log Likelihood
</td>
<td>
107.689
</td>
<td>
78.397
</td>
<td>
82.071
</td>
</tr>
<tr>
<td colspan="4" style="border-bottom: 1px solid black">
</td>
</tr>
<tr>
<td style="text-align:left">
<em>Note:</em>
</td>
<td colspan="3" style="text-align:right">
<sup>*</sup>p\<0.05; <sup>**</sup>p\<0.01; <sup>***</sup>p\<0.001
</td>
</tr>
</table>

###### Air Strikes

``` r
gy_logit4 <- betareg(thememonth1 ~ airstrikeslog, data = mydata)
gy_logit5 <- betareg(thememonth2 ~ airstrikeslog, data = mydata)
gy_logit6 <- betareg(thememonth3 ~ airstrikeslog, data = mydata)
```

Print for publication

``` r
stargazer(gy_logit4,gy_logit5, gy_logit6,
          align = T, df = F,
          star.char = c("*", "**", "***"),
                    ci = TRUE,
          star.cutoffs = c(.05, .01, .001),
          single.row=TRUE,
          omit = c("Constant"),
                    column.labels = c("Offline Caliphate", "Online Claiphate", "Regional Enemies"),
          covariate.labels = c("Air Strikes (log)"),
          type = "html", digits = 3)
```

<table style="text-align:center">
<tr>
<td colspan="4" style="border-bottom: 1px solid black">
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td colspan="3">
<em>Dependent variable:</em>
</td>
</tr>
<tr>
<td>
</td>
<td colspan="3" style="border-bottom: 1px solid black">
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
thememonth1
</td>
<td>
thememonth2
</td>
<td>
thememonth3
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
Offline Caliphate
</td>
<td>
Online Claiphate
</td>
<td>
Regional Enemies
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
\(1\)
</td>
<td>
\(2\)
</td>
<td>
\(3\)
</td>
</tr>
<tr>
<td colspan="4" style="border-bottom: 1px solid black">
</td>
</tr>
<tr>
<td style="text-align:left">
Air Strikes (log)
</td>
<td>
-0.055<sup>\*\*\*</sup> (-0.085, -0.026)
</td>
<td>
0.125<sup>\*\*\*</sup> (0.068, 0.181)
</td>
<td>
-0.038<sup>\*</sup> (-0.071, -0.004)
</td>
</tr>
<tr>
<td colspan="4" style="border-bottom: 1px solid black">
</td>
</tr>
<tr>
<td style="text-align:left">
Observations
</td>
<td>
63
</td>
<td>
63
</td>
<td>
63
</td>
</tr>
<tr>
<td style="text-align:left">
R<sup>2</sup>
</td>
<td>
0.167
</td>
<td>
0.294
</td>
<td>
0.077
</td>
</tr>
<tr>
<td style="text-align:left">
Log Likelihood
</td>
<td>
113.597
</td>
<td>
87.874
</td>
<td>
84.466
</td>
</tr>
<tr>
<td colspan="4" style="border-bottom: 1px solid black">
</td>
</tr>
<tr>
<td style="text-align:left">
<em>Note:</em>
</td>
<td colspan="3" style="text-align:right">
<sup>*</sup>p\<0.05; <sup>**</sup>p\<0.01; <sup>***</sup>p\<0.001
</td>
</tr>
</table>

###### Territorial Control

Theme 1

``` r
###theme1
testtheme1.1 <- dplyr::select(mydata,thememonth1,territorialmonthlychange)
testtheme1.1<- ts(testtheme1.1)
plot(testtheme1.1)
```

![](AN_github_v2_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
#Test for stationarity
Acf(testtheme1.1[,"thememonth1"])
Acf(testtheme1.1[,"territorialmonthlychange"])
adf.test(testtheme1.1[,"thememonth1"])
adf.test(testtheme1.1[,"territorialmonthlychange"])
adf.test(diff(testtheme1.1[,"thememonth1"]))
adf.test(diff(testtheme1.1[,"territorialmonthlychange"]))
coint1.1 <- dynlm(thememonth1~territorialmonthlychange, data=testtheme1.1)
ehat1.1 <- resid(coint1.1)
adf.test(ehat1.1)
```

``` r
fitvar1.1 <- VAR(testtheme1.1, p=3, type="both")
#summary(fitvar1.1) # theme1.1
causality(fitvar1.1, cause = "thememonth1")
```

    ## $Granger
    ## 
    ##  Granger causality H0: thememonth1 do not Granger-cause
    ##  territorialmonthlychange
    ## 
    ## data:  VAR object fitvar1.1
    ## F-Test = 3.1194, df1 = 3, df2 = 104, p-value = 0.02925
    ## 
    ## 
    ## $Instant
    ## 
    ##  H0: No instantaneous causality between: thememonth1 and
    ##  territorialmonthlychange
    ## 
    ## data:  VAR object fitvar1.1
    ## Chi-squared = 0.99832, df = 1, p-value = 0.3177

``` r
irf_theme1.1_territory <- irf(fitvar1.1, impulse = "territorialmonthlychange", response = "thememonth1", boot = TRUE)
plot(irf_theme1.1_territory)
```

![](AN_github_v2_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
plot(fevd(fitvar1.1))
```

![](AN_github_v2_files/figure-gfm/unnamed-chunk-9-2.png)<!-- --> Theme 2

``` r
testtheme2.1 <- dplyr::select(mydata, thememonth2,territorialmonthlychange)
testtheme2.1 <- ts(testtheme2.1)
plot(testtheme2.1)
```

![](AN_github_v2_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

``` r
#Test for stationarity
Acf(testtheme2.1[,"thememonth2"])
Acf(testtheme2.1[,"territorialmonthlychange"])
adf.test(testtheme2.1[,"thememonth2"])
adf.test(testtheme2.1[,"territorialmonthlychange"])
adf.test(diff(testtheme2.1[,"thememonth2"]))
adf.test(diff(testtheme2.1[,"territorialmonthlychange"]))
coint2.1 <- dynlm(thememonth2~territorialmonthlychange, data=testtheme2.1)
ehat2.1 <- resid(coint2.1)
adf.test(ehat2.1)
```

``` r
####VAR Theme 2
fitvar2.1 <- VAR(testtheme2.1, p=3, type="both")
#summary(fitvar2.1) # theme2.1
causality(fitvar2.1, cause = "thememonth2")
```

    ## $Granger
    ## 
    ##  Granger causality H0: thememonth2 do not Granger-cause
    ##  territorialmonthlychange
    ## 
    ## data:  VAR object fitvar2.1
    ## F-Test = 1.7622, df1 = 3, df2 = 104, p-value = 0.159
    ## 
    ## 
    ## $Instant
    ## 
    ##  H0: No instantaneous causality between: thememonth2 and
    ##  territorialmonthlychange
    ## 
    ## data:  VAR object fitvar2.1
    ## Chi-squared = 0.44802, df = 1, p-value = 0.5033

``` r
irf_theme2.1_territory <- irf(fitvar2.1, impulse = "territorialmonthlychange", response = "thememonth2", boot = TRUE)
plot(irf_theme2.1_territory)
```

![](AN_github_v2_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

``` r
plot(fevd(fitvar2.1))
```

![](AN_github_v2_files/figure-gfm/unnamed-chunk-12-2.png)<!-- --> Theme
3

``` r
testtheme3.1 <- dplyr::select(mydata, thememonth3,territorialmonthlychange)
testtheme3.1 <- ts(testtheme3.1)
plot(testtheme3.1)
```

![](AN_github_v2_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

``` r
#Test for stationarity
Acf(testtheme3.1[,"thememonth3"])
Acf(testtheme3.1[,"territorialmonthlychange"])
adf.test(testtheme3.1[,"thememonth3"])
adf.test(testtheme3.1[,"territorialmonthlychange"])
adf.test(diff(testtheme3.1[,"thememonth3"]))
adf.test(diff(testtheme3.1[,"territorialmonthlychange"]))
coint3.1 <- dynlm(thememonth3~territorialmonthlychange, data=testtheme3.1)
ehat3.1 <- resid(coint3.1)
adf.test(ehat3.1)
```

``` r
####VAR Theme 3
fitvar3.1 <- VAR(testtheme3.1, p=3, type="both")
#summary(fitvar3.1) # theme3.1
causality(fitvar3.1, cause = "thememonth3")
```

    ## $Granger
    ## 
    ##  Granger causality H0: thememonth3 do not Granger-cause
    ##  territorialmonthlychange
    ## 
    ## data:  VAR object fitvar3.1
    ## F-Test = 4.3332, df1 = 3, df2 = 104, p-value = 0.006402
    ## 
    ## 
    ## $Instant
    ## 
    ##  H0: No instantaneous causality between: thememonth3 and
    ##  territorialmonthlychange
    ## 
    ## data:  VAR object fitvar3.1
    ## Chi-squared = 1.4608, df = 1, p-value = 0.2268

``` r
irf_theme3.1_territory <- irf(fitvar3.1, impulse = "territorialmonthlychange", response = "thememonth3", boot = TRUE)
plot(irf_theme3.1_territory)
```

![](AN_github_v2_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

``` r
plot(fevd(fitvar3.1))
```

![](AN_github_v2_files/figure-gfm/unnamed-chunk-15-2.png)<!-- --> Write
the VAR results

``` r
vartheme1G <- fitvar1.1[["varresult"]]$territorialmonthlychange
vartheme2G <- fitvar2.1[["varresult"]]$territorialmonthlychange
vartheme3G <- fitvar3.1[["varresult"]]$territorialmonthlychange
coef_remove<-c("thememonth1.l1", 
               "thememonth1.l2",
               "thememonth1.l3",
               "const",
               "thememonth2.l1",
               "thememonth2.l2",
               "thememonth2.l3",
               "thememonth3.l1",
               "thememonth3.l2",
               "thememonth3.l3")
#plot
sjPlot::tab_model(vartheme1G, 
                  vartheme2G,
                  vartheme3G,
                  show.se = TRUE,
                  show.p = FALSE,
                  digits = 3,
                  wrap.labels = 25,
                  collapse.se = TRUE,
                  rm.terms = coef_remove,
                    pred.labels = c("Territorial Change (lag 1)", "Territorial Change (lag 2)", "Territorial Change (lag 3)", "trend"),
  dv.labels = c("Offline Caliphate", "Online Caliphate", "Regional Enemies"),
                  p.style = c("scientific_stars"))
```

<table style="border-collapse:collapse; border:none;">
<tr>
<th style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm;  text-align:left; ">
 
</th>
<th colspan="2" style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm; ">
Offline Caliphate
</th>
<th colspan="2" style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm; ">
Online Caliphate
</th>
<th colspan="2" style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm; ">
Regional Enemies
</th>
</tr>
<tr>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  text-align:left; ">
Predictors
</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">
Estimates
</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">
CI
</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">
Estimates
</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">
CI
</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">
Estimates
</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  col7">
CI
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
Territorial Change (lag 1)
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.290 <sup>\*</sup><br>(0.122)
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.044 – 0.536
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.378 <sup>\*\*</sup><br>(0.134)
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.110 – 0.647
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.245 <sup></sup><br>(0.130)
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">
-0.015 – 0.506
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
Territorial Change (lag 2)
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
-0.142 <sup></sup><br>(0.128)
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
-0.400 – 0.115
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
-0.093 <sup></sup><br>(0.135)
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
-0.363 – 0.177
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
-0.045 <sup></sup><br>(0.126)
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">
-0.298 – 0.208
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
Territorial Change (lag 3)
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.409 <sup>\*\*</sup><br>(0.124)
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.159 – 0.658
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.300 <sup>\*</sup><br>(0.129)
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.041 – 0.559
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.367 <sup>\*\*</sup><br>(0.119)
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">
0.128 – 0.607
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
trend
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.037 <sup></sup><br>(0.096)
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
-0.156 – 0.229
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.073 <sup></sup><br>(0.103)
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
-0.134 – 0.281
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.085 <sup></sup><br>(0.088)
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">
-0.092 – 0.262
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm; border-top:1px solid;">
Observations
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left; border-top:1px solid;" colspan="2">
60
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left; border-top:1px solid;" colspan="2">
60
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left; border-top:1px solid;" colspan="2">
60
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm;">
R<sup>2</sup> / R<sup>2</sup> adjusted
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="2">
0.361 / 0.275
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="2">
0.315 / 0.223
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="2">
0.396 / 0.315
</td>
</tr>
<tr>
<td colspan="7" style="font-style:italic; border-top:double black; text-align:right;">

-   p\<0.05   \*\* p\<0.01   \*\*\* p\<0.001
    </td>
    </tr>

</table>

``` r
#create function to retrieve IRF model info
getIRFPlotData <- function(impulse, response, list) {
  cbind.data.frame(Theme = 0:(nrow(list[[1]][[1]])-1),
                   Lower = list[[2]][names(list[[2]]) == impulse][[1]] %>% as.data.frame() %>% dplyr::select_(response) %>% pull(1),
                   irf = list[[1]][names(list[[1]]) == impulse][[1]] %>% as.data.frame() %>% dplyr::select_(response) %>% pull(1),
                   Upper = list[[3]][names(list[[3]]) == impulse][[1]] %>% as.data.frame() %>% dplyr::select_(response) %>% pull(1),
                   Impulse = impulse,
                   Response = response, stringsAsFactors = FALSE)
}

territorial_monthly_change1.1 <- getIRFPlotData("territorialmonthlychange", "thememonth1", irf_theme1.1_territory)
territorial_monthly_change2.1 <- getIRFPlotData("territorialmonthlychange", "thememonth2", irf_theme2.1_territory)
territorial_monthly_change3.1 <- getIRFPlotData("territorialmonthlychange", "thememonth3", irf_theme3.1_territory)

#Create singular data for all plots for ggplot plotting
plot_all <- rbind(territorial_monthly_change1.1, territorial_monthly_change2.1, territorial_monthly_change3.1)
names(plot_all)
```

    ## [1] "Theme"    "Lower"    "irf"      "Upper"    "Impulse"  "Response"

``` r
nrow(plot_all)
```

    ## [1] 33

``` r
plot_all <- plot_all %>% mutate(group = paste0(Impulse, Response))
plot_all<-as.data.frame(plot_all)
plot_all$Response<-dplyr::recode(plot_all$Response, 
                                 thememonth1 = "Offline Caliphate",
                                 thememonth2 = "Online Caliphate",
                                 thememonth3 = "Regional Enemies")
plot_all<- plot_all %>%
  mutate(across(group, factor, levels=c("territorialmonthlychangethememonth1","territorialmonthlychangethememonth2","territorialmonthlychangethememonth3")))
```

Plot

``` r
#create a labeler
to_string <- as_labeller(c("territorialmonthlychangethememonth1" = "Offline Caliphate", 
                           "territorialmonthlychangethememonth2" = "Online Caliphate",
                           "territorialmonthlychangethememonth3" = "Regional Enemies"))

ggplot(plot_all, aes(x = Theme, y = irf)) +
  geom_line(aes(x = Theme, y = irf), color = "red2", size = 1.5) + 
  geom_line(aes(x = Theme, y = Upper) , linetype = "dashed")+ 
  geom_line(aes(x = Theme, y = Lower), linetype = "dashed")+ 
  geom_hline(aes(yintercept=0), 
             linetype = "solid", color = "blue2") +
  facet_grid(.~ Response) + 
  facet_wrap(.~group, 
             labeller = to_string,
             nrow = 3,
             scales = "free_y") +
  scale_x_continuous(breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)) +
  labs(x = "time(months)",
       y= "Cluster Prominence") + 
  theme_wsj(color="white")+
  theme(text=element_text(size=10,family="sans"),
        title=element_text(size=8,family="sans"),
        axis.text.x=element_text(angle=60, hjust=1, family="sans"),
        axis.text.y=element_text(family="sans"),
        axis.title.x=element_text(vjust=-0.25, size=10, family="sans"),
        axis.title.y=element_text(vjust=-0.25, size=10, family="sans"),
        legend.position="none", legend.box="vertical", legend.margin=margin(),
        legend.key = element_rect(fill="white"), legend.background = element_rect(fill=NA),
        legend.text=element_text(size=8, family="sans")) 
```

![](AN_github_v2_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->

``` r
ggsave("AN_IRF_Function_DATE.tiff", width=10.5, height=5, dpi=300)
```

###### Temporary Population Control

Download daily thematic clusters data set

``` r
mydatalong <- read.csv("https://dataverse.harvard.edu/api/access/datafile/6388550")
## remove the index column from csv
mydatalong<-mydatalong %>%
  mutate(date = ymd(mydatalong$date)) %>%
  dplyr::select(-X)
glimpse(mydatalong)
```

    ## Rows: 792
    ## Columns: 3
    ## $ date  <date> 2015-10-16, 2015-10-16, 2015-10-16, 2015-10-23, 2015-10-23, 201~
    ## $ theme <chr> "Offline Caliphate", "Online Caliphate", "Regional Enemies", "Of~
    ## $ theta <dbl> 0.27596883, 0.02176216, 0.41277899, 0.22791331, 0.03410307, 0.39~

``` r
summary(mydatalong)
```

    ##       date               theme               theta        
    ##  Min.   :2015-10-16   Length:792         Min.   :0.00292  
    ##  1st Qu.:2017-02-06   Class :character   1st Qu.:0.04872  
    ##  Median :2018-05-26   Mode  :character   Median :0.20580  
    ##  Mean   :2018-05-25                      Mean   :0.23373  
    ##  3rd Qu.:2019-09-13                      3rd Qu.:0.39436  
    ##  Max.   :2020-12-16                      Max.   :0.74095

Load data set with dates of digital take down marked.

``` r
linedigital <- read.csv("https://dataverse.harvard.edu/api/access/datafile/6380746")
linedigital <- linedigital %>%
  mutate(date = ymd(linedigital$val)) %>%
  dplyr::select(-X)
glimpse(linedigital)
```

    ## Rows: 3
    ## Columns: 4
    ## $ ref   <chr> "a", "b", "c"
    ## $ event <chr> "Twitter Takedown", "Amaq Takedown", "Telegram takedown"
    ## $ val   <chr> "2017-04-01", "2018-04-01", "2019-11-01"
    ## $ date  <date> 2017-04-01, 2018-04-01, 2019-11-01

``` r
ggplot(mydatalong, aes(date, theta)) + 
  geom_line(size = 0.5) + 
  scale_x_date(date_breaks="2 months", date_labels = "%b-%Y")+
  geom_vline(mapping = aes(xintercept = date,
                           colour = ref),
             linetype="dashed", 
             size=0.5,
             data = linedigital,
             show.legend = FALSE)+
  geom_text(mapping = aes(x = date,
                          y=0,
                          label = ref,
                          hjust = 0.5,
                          vjust = 0),
            size=3,
            data = linedigital) +
  facet_wrap(~theme, 
             labeller = labeller(theme = 
                                           c("G1" = "Offline Caliphate",
                                             "G2" = "Online Caliphate",
                                             "G3" = "Regional Enemies")),
             dir = "v", 
             scales = "free_y")+
  labs(title = 'Temporary Population Control' ,
       y = "Cluster Prominence",
       x=" ") +
  theme_wsj(color="white") +
  theme(text=element_text(size=10,family="Sans"),
        title=element_text(size=10,family="Sans"),
        axis.text.x=element_text(size=8, angle=60, hjust=1, family="Sans"),
        axis.text.y=element_text(size=8, family="Sans"),
        axis.title.x=element_text(vjust=-0.25, size=8, family="Sans"),
        axis.title.y=element_text(vjust=-0.25, size=8, family="Sans"))
```

![](AN_github_v2_files/figure-gfm/unnamed-chunk-21-1.png)<!-- -->

``` r
ggsave("AN_PopulationControl_DATE.tiff", width=12.5, height=5, dpi=300)
```

###### Governance

Load data set with dates of leadership events marked.

``` r
ggplot(mydatalong, aes(date, theta)) + 
  geom_line(size = 0.5) + 
  scale_x_date(date_breaks="2 months", date_labels = "%b-%Y")+
  geom_vline(mapping = aes(xintercept = date,
                           colour = ref),
             linetype="dashed", 
             size=0.5,
             data = lineleader,
             show.legend = FALSE)+
  geom_text(mapping = aes(x = date,
                          y=0,
                          label = ref,
                          hjust = 0.5,
                          vjust = 0),
            size=3,
            data = lineleader) +
  facet_wrap(~theme, labeller = labeller(theme = 
                                           c("G1" = "Offline Caliphate",
                                             "G2" = "Online Caliphate",
                                             "G3" = "Regional Enemies")),
             dir = "v", scales = "free_y")+
  labs(title = 'Governance' ,
       y = "Cluster Prominence",
       x=" ") +
  theme_wsj(color="white") +
  theme(text=element_text(size=10,family="Sans"),
        title=element_text(size=10,family="Sans"),
        axis.text.x=element_text(size=8, angle=60, hjust=1, family="Sans"),
        axis.text.y=element_text(size=8, family="Sans"),
        axis.title.x=element_text(vjust=-0.25, size=8, family="Sans"),
        axis.title.y=element_text(vjust=-0.25, size=8, family="Sans"))
```

![](AN_github_v2_files/figure-gfm/unnamed-chunk-23-1.png)<!-- -->

``` r
ggsave("AN_Governance_DATE.tiff", width=12.5, height=5, dpi=300)
```
