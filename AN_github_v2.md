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

The code includes all the steps of the methodology, and the
visualizations.

The data set accompanying the code: <https://doi.org/10.7910/DVN/FAP771>

For questions, or more information on the code please contact: Ayse D.
Lokmanoglu  
ayse \[dot\] lokmanoglu \[at\] nortwestern \[dot\] edu

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

Load the dataset, cluster prominences by issue

``` r
mydatalong <- read.csv("https://dataverse.harvard.edu/api/access/datafile/6380742")
head(mydatalong)
```

    ##   X    Group.1 theme     theta
    ## 1 1 2015-10-16    G1 0.2759688
    ## 2 2 2015-10-23    G1 0.2279133
    ## 3 3 2015-10-30    G1 0.2594605
    ## 4 4 2015-11-06    G1 0.2336571
    ## 5 5 2015-11-14    G1 0.1802058
    ## 6 6 2015-11-21    G1 0.2326231

``` r
## remove the index column from csv
mydatalong<-mydatalong %>%
  mutate(date = ymd(mydatalong$Group.1)) %>%
  dplyr::select(-X, -Group.1)
```

##### Network Visualizations

``` r
prevalance<-ggplot(mydatalong,aes(x=date))+
  geom_col(aes(y=theta,fill=theme),position = "fill",width = 4)+
  scale_x_date(date_breaks="2 months", date_labels = "%b-%Y")+
  xlab("")+
  ylab("Prevalance")+
  theme_wsj(color="white")+
  #ggtitle("Graphs of Al Naba Prevalance and Prominence of Themes")+
  labs(fill = "CLUSTERS: ")+
  scale_fill_manual(values=c("orangered1", "purple4", "springgreen4"),
                    name="Clusters: ",
                    labels=c("Offline Caliphate", "Online Caliphate", "Regional Enemies"))+
  theme(text=element_text(size=10,family="Sans"),
        title=element_text(size=10,family="Sans"),
        axis.text.x=element_text(angle=60, size=8, hjust=1, family="Sans"),
        axis.text.y=element_text(size=8, family="Sans"),
        axis.title.x=element_text(vjust=-0.25, size=8, family="Sans"),
        axis.title.y=element_text(vjust=-0.25, size=8, family="Sans"),
        legend.position="none", legend.box="vertical", legend.margin=margin(),
        legend.key = element_rect(fill="white"), legend.background = element_rect(fill=NA),
        legend.text=element_text(size=8, family="Sans"))

line<-ggplot(mydatalong,aes(x=date, y=theta))+
  geom_line(aes(color=theme)) +
  scale_x_date(date_breaks="2 months", date_labels = "%b-%Y")+
  xlab(" ")+
  ylab("Prominence")+
  theme_wsj(color = "white")+
  theme(legend.position="none")+
  labs(fill = "CLUSTERS: ")+
  scale_color_manual(values=c("orangered1", "purple4", "springgreen4"),
                     name="Clusters: ",
                     labels=c("Offline Caliphate", "Online Caliphate", "Regional Enemies"))+
  theme(text=element_text(size=10,family="Sans"),
        title=element_text(size=10,family="Sans"),
        axis.text.x=element_text(angle=60, size=8, hjust=1, family="Sans"),
        axis.text.y=element_text(size=8, family="Sans"),
        axis.title.x=element_text(vjust=-0.25, size=8, family="Sans"),
        axis.title.y=element_text(vjust=-0.25, size=8, family="Sans"),
        legend.position="none", legend.box="vertical", legend.margin=margin(),
        legend.key = element_rect(fill="white"), legend.background = element_rect(fill=NA),
        legend.text=element_text(size=8, family="Sans"))

###Combine both graphs
ggarrange(prevalance +rremove("xlab"), line +rremove("xlab"), 
          ncol = 1, 
          nrow = 2, 
          common.legend = TRUE, legend = "bottom",
          align = c("h"))
```

![](AN_github_v2_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

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
```

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
          type = "html", digits = 2)
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
religiousholidaysmuslim
</td>
<td>
-0.15
</td>
<td>
0.12
</td>
<td>
0.03
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
(0.09)
</td>
<td>
(0.16)
</td>
<td>
(0.10)
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
</td>
<td>
</td>
<td>
</td>
</tr>
<tr>
<td style="text-align:left">
Constant
</td>
<td>
-1.32<sup>\*\*\*</sup>
</td>
<td>
-1.57<sup>\*\*\*</sup>
</td>
<td>
0.47<sup>\*\*\*</sup>
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
(0.04)
</td>
<td>
(0.07)
</td>
<td>
(0.04)
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
</td>
<td>
</td>
<td>
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
0.04
</td>
<td>
0.01
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
108.99
</td>
<td>
78.65
</td>
<td>
82.08
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
          type = "html", digits = 2)
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
religiousholidayschristian
</td>
<td>
0.05
</td>
<td>
-0.09
</td>
<td>
0.03
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
(0.09)
</td>
<td>
(0.16)
</td>
<td>
(0.09)
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
</td>
<td>
</td>
<td>
</td>
</tr>
<tr>
<td style="text-align:left">
Constant
</td>
<td>
-1.35<sup>\*\*\*</sup>
</td>
<td>
-1.53<sup>\*\*\*</sup>
</td>
<td>
0.47<sup>\*\*\*</sup>
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
(0.04)
</td>
<td>
(0.07)
</td>
<td>
(0.04)
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
</td>
<td>
</td>
<td>
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
0.01
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
107.82
</td>
<td>
78.55
</td>
<td>
82.10
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
          type = "html", digits = 2)
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
religiousholidaysjewish
</td>
<td>
-0.02
</td>
<td>
0.02
</td>
<td>
0.02
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
(0.09)
</td>
<td>
(0.17)
</td>
<td>
(0.10)
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
</td>
<td>
</td>
<td>
</td>
</tr>
<tr>
<td style="text-align:left">
Constant
</td>
<td>
-1.34<sup>\*\*\*</sup>
</td>
<td>
-1.55<sup>\*\*\*</sup>
</td>
<td>
0.47<sup>\*\*\*</sup>
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
(0.04)
</td>
<td>
(0.07)
</td>
<td>
(0.04)
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
</td>
<td>
</td>
<td>
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
107.69
</td>
<td>
78.40
</td>
<td>
82.07
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
          star.cutoffs = c(.05, .01, .001),
          type = "html", digits = 2, out = "Beta Regression Table4.html")
```

    ## 
    ## <table style="text-align:center"><tr><td colspan="4" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left"></td><td colspan="3"><em>Dependent variable:</em></td></tr>
    ## <tr><td></td><td colspan="3" style="border-bottom: 1px solid black"></td></tr>
    ## <tr><td style="text-align:left"></td><td>thememonth1</td><td>thememonth2</td><td>thememonth3</td></tr>
    ## <tr><td style="text-align:left"></td><td>(1)</td><td>(2)</td><td>(3)</td></tr>
    ## <tr><td colspan="4" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">airstrikeslog</td><td>-0.06<sup>***</sup></td><td>0.12<sup>***</sup></td><td>-0.04<sup>*</sup></td></tr>
    ## <tr><td style="text-align:left"></td><td>(0.02)</td><td>(0.03)</td><td>(0.02)</td></tr>
    ## <tr><td style="text-align:left"></td><td></td><td></td><td></td></tr>
    ## <tr><td style="text-align:left">Constant</td><td>-1.07<sup>***</sup></td><td>-2.18<sup>***</sup></td><td>0.66<sup>***</sup></td></tr>
    ## <tr><td style="text-align:left"></td><td>(0.08)</td><td>(0.16)</td><td>(0.09)</td></tr>
    ## <tr><td style="text-align:left"></td><td></td><td></td><td></td></tr>
    ## <tr><td colspan="4" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">Observations</td><td>63</td><td>63</td><td>63</td></tr>
    ## <tr><td style="text-align:left">R<sup>2</sup></td><td>0.17</td><td>0.29</td><td>0.08</td></tr>
    ## <tr><td style="text-align:left">Log Likelihood</td><td>113.60</td><td>87.87</td><td>84.47</td></tr>
    ## <tr><td colspan="4" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left"><em>Note:</em></td><td colspan="3" style="text-align:right"><sup>*</sup>p<0.05; <sup>**</sup>p<0.01; <sup>***</sup>p<0.001</td></tr>
    ## </table>

###### Territorial Control

Theme 1

``` r
###theme1
testtheme1.1 <- dplyr::select(mydata,thememonth1,territorialmonthlychange)
testtheme1.1<- ts(testtheme1.1)
plot(testtheme1.1)
```

![](AN_github_v2_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
#Test for stationarity
Acf(testtheme1.1[,"thememonth1"])
```

![](AN_github_v2_files/figure-gfm/unnamed-chunk-9-2.png)<!-- -->

``` r
Acf(testtheme1.1[,"territorialmonthlychange"])
```

![](AN_github_v2_files/figure-gfm/unnamed-chunk-9-3.png)<!-- -->

``` r
adf.test(testtheme1.1[,"thememonth1"])
```

    ## 
    ##  Augmented Dickey-Fuller Test
    ## 
    ## data:  testtheme1.1[, "thememonth1"]
    ## Dickey-Fuller = -3.9971, Lag order = 3, p-value = 0.01558
    ## alternative hypothesis: stationary

``` r
adf.test(testtheme1.1[,"territorialmonthlychange"])
```

    ## 
    ##  Augmented Dickey-Fuller Test
    ## 
    ## data:  testtheme1.1[, "territorialmonthlychange"]
    ## Dickey-Fuller = -2.4756, Lag order = 3, p-value = 0.3823
    ## alternative hypothesis: stationary

``` r
adf.test(diff(testtheme1.1[,"thememonth1"]))
```

    ## 
    ##  Augmented Dickey-Fuller Test
    ## 
    ## data:  diff(testtheme1.1[, "thememonth1"])
    ## Dickey-Fuller = -6.9784, Lag order = 3, p-value = 0.01
    ## alternative hypothesis: stationary

``` r
adf.test(diff(testtheme1.1[,"territorialmonthlychange"]))
```

    ## 
    ##  Augmented Dickey-Fuller Test
    ## 
    ## data:  diff(testtheme1.1[, "territorialmonthlychange"])
    ## Dickey-Fuller = -5.3789, Lag order = 3, p-value = 0.01
    ## alternative hypothesis: stationary

``` r
coint1.1 <- dynlm(thememonth1~territorialmonthlychange, data=testtheme1.1)
ehat1.1 <- resid(coint1.1)
adf.test(ehat1.1)
```

    ## 
    ##  Augmented Dickey-Fuller Test
    ## 
    ## data:  ehat1.1
    ## Dickey-Fuller = -3.9921, Lag order = 3, p-value = 0.0158
    ## alternative hypothesis: stationary

``` r
fitvar1.1 <- VAR(testtheme1.1, p=3, type="both")
summary(fitvar1.1) # theme1.1
```

    ## 
    ## VAR Estimation Results:
    ## ========================= 
    ## Endogenous variables: thememonth1, territorialmonthlychange 
    ## Deterministic variables: both 
    ## Sample size: 60 
    ## Log Likelihood: -105.437 
    ## Roots of the characteristic polynomial:
    ## 0.8466 0.6442 0.6442 0.4996 0.2099 0.2099
    ## Call:
    ## VAR(y = testtheme1.1, p = 3, type = "both")
    ## 
    ## 
    ## Estimation results for equation thememonth1: 
    ## ============================================ 
    ## thememonth1 = thememonth1.l1 + territorialmonthlychange.l1 + thememonth1.l2 + territorialmonthlychange.l2 + thememonth1.l3 + territorialmonthlychange.l3 + const + trend 
    ## 
    ##                               Estimate Std. Error t value Pr(>|t|)   
    ## thememonth1.l1               0.4530961  0.1392915   3.253  0.00201 **
    ## territorialmonthlychange.l1 -0.0002103  0.0003704  -0.568  0.57272   
    ## thememonth1.l2               0.1710991  0.1462426   1.170  0.24735   
    ## territorialmonthlychange.l2  0.0006648  0.0003882   1.713  0.09274 . 
    ## thememonth1.l3              -0.0308060  0.1341089  -0.230  0.81922   
    ## territorialmonthlychange.l3 -0.0001643  0.0003755  -0.438  0.66351   
    ## const                        0.0679570  0.0257077   2.643  0.01082 * 
    ## trend                        0.0004851  0.0002901   1.672  0.10052   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## 
    ## Residual standard error: 0.03457 on 52 degrees of freedom
    ## Multiple R-Squared: 0.4634,  Adjusted R-squared: 0.3912 
    ## F-statistic: 6.415 on 7 and 52 DF,  p-value: 1.858e-05 
    ## 
    ## 
    ## Estimation results for equation territorialmonthlychange: 
    ## ========================================================= 
    ## territorialmonthlychange = thememonth1.l1 + territorialmonthlychange.l1 + thememonth1.l2 + territorialmonthlychange.l2 + thememonth1.l3 + territorialmonthlychange.l3 + const + trend 
    ## 
    ##                              Estimate Std. Error t value Pr(>|t|)   
    ## thememonth1.l1              -65.60920   46.04360  -1.425  0.16015   
    ## territorialmonthlychange.l1   0.29007    0.12244   2.369  0.02158 * 
    ## thememonth1.l2               -1.69241   48.34134  -0.035  0.97221   
    ## territorialmonthlychange.l2  -0.14234    0.12831  -1.109  0.27236   
    ## thememonth1.l3              123.67368   44.33047   2.790  0.00735 **
    ## territorialmonthlychange.l3   0.40856    0.12413   3.291  0.00180 **
    ## const                       -13.44992    8.49783  -1.583  0.11954   
    ## trend                         0.03667    0.09591   0.382  0.70377   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## 
    ## Residual standard error: 11.43 on 52 degrees of freedom
    ## Multiple R-Squared: 0.3606,  Adjusted R-squared: 0.2745 
    ## F-statistic: 4.189 on 7 and 52 DF,  p-value: 0.001002 
    ## 
    ## 
    ## 
    ## Covariance matrix of residuals:
    ##                          thememonth1 territorialmonthlychange
    ## thememonth1                 0.001195                 -0.05138
    ## territorialmonthlychange   -0.051376                130.55679
    ## 
    ## Correlation matrix of residuals:
    ##                          thememonth1 territorialmonthlychange
    ## thememonth1                   1.0000                  -0.1301
    ## territorialmonthlychange     -0.1301                   1.0000

``` r
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

![](AN_github_v2_files/figure-gfm/unnamed-chunk-9-4.png)<!-- -->

``` r
plot(fevd(fitvar1.1))
```

![](AN_github_v2_files/figure-gfm/unnamed-chunk-9-5.png)<!-- --> Theme 2

``` r
testtheme2.1 <- dplyr::select(mydata, thememonth2,territorialmonthlychange)
testtheme2.1 <- ts(testtheme2.1)
plot(testtheme2.1)
```

![](AN_github_v2_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

``` r
#Test for stationarity
Acf(testtheme2.1[,"thememonth2"])
```

![](AN_github_v2_files/figure-gfm/unnamed-chunk-10-2.png)<!-- -->

``` r
Acf(testtheme2.1[,"territorialmonthlychange"])
```

![](AN_github_v2_files/figure-gfm/unnamed-chunk-10-3.png)<!-- -->

``` r
adf.test(testtheme2.1[,"thememonth2"])
```

    ## 
    ##  Augmented Dickey-Fuller Test
    ## 
    ## data:  testtheme2.1[, "thememonth2"]
    ## Dickey-Fuller = -2.8304, Lag order = 3, p-value = 0.2386
    ## alternative hypothesis: stationary

``` r
adf.test(testtheme2.1[,"territorialmonthlychange"])
```

    ## 
    ##  Augmented Dickey-Fuller Test
    ## 
    ## data:  testtheme2.1[, "territorialmonthlychange"]
    ## Dickey-Fuller = -2.4756, Lag order = 3, p-value = 0.3823
    ## alternative hypothesis: stationary

``` r
adf.test(diff(testtheme2.1[,"thememonth2"]))
```

    ## 
    ##  Augmented Dickey-Fuller Test
    ## 
    ## data:  diff(testtheme2.1[, "thememonth2"])
    ## Dickey-Fuller = -4.3526, Lag order = 3, p-value = 0.01
    ## alternative hypothesis: stationary

``` r
adf.test(diff(testtheme2.1[,"territorialmonthlychange"]))
```

    ## 
    ##  Augmented Dickey-Fuller Test
    ## 
    ## data:  diff(testtheme2.1[, "territorialmonthlychange"])
    ## Dickey-Fuller = -5.3789, Lag order = 3, p-value = 0.01
    ## alternative hypothesis: stationary

``` r
coint2.1 <- dynlm(thememonth2~territorialmonthlychange, data=testtheme2.1)
ehat2.1 <- resid(coint2.1)
adf.test(ehat2.1)
```

    ## 
    ##  Augmented Dickey-Fuller Test
    ## 
    ## data:  ehat2.1
    ## Dickey-Fuller = -2.7786, Lag order = 3, p-value = 0.2596
    ## alternative hypothesis: stationary

``` r
####VAR Theme 2
fitvar2.1 <- VAR(testtheme2.1, p=3, type="both")
summary(fitvar2.1) # theme2.1
```

    ## 
    ## VAR Estimation Results:
    ## ========================= 
    ## Endogenous variables: thememonth2, territorialmonthlychange 
    ## Deterministic variables: both 
    ## Sample size: 60 
    ## Log Likelihood: -123.828 
    ## Roots of the characteristic polynomial:
    ## 0.7896 0.7896 0.703 0.703 0.2377 0.2377
    ## Call:
    ## VAR(y = testtheme2.1, p = 3, type = "both")
    ## 
    ## 
    ## Estimation results for equation thememonth2: 
    ## ============================================ 
    ## thememonth2 = thememonth2.l1 + territorialmonthlychange.l1 + thememonth2.l2 + territorialmonthlychange.l2 + thememonth2.l3 + territorialmonthlychange.l3 + const + trend 
    ## 
    ##                               Estimate Std. Error t value Pr(>|t|)   
    ## thememonth2.l1               0.4388310  0.1341389   3.271  0.00190 **
    ## territorialmonthlychange.l1  0.0001238  0.0005106   0.242  0.80942   
    ## thememonth2.l2               0.1126466  0.1487024   0.758  0.45215   
    ## territorialmonthlychange.l2 -0.0006059  0.0005142  -1.178  0.24411   
    ## thememonth2.l3               0.0847953  0.1340416   0.633  0.52976   
    ## territorialmonthlychange.l3 -0.0006352  0.0004927  -1.289  0.20301   
    ## const                        0.0963938  0.0284058   3.393  0.00133 **
    ## trend                       -0.0009779  0.0003949  -2.476  0.01657 * 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## 
    ## Residual standard error: 0.04516 on 52 degrees of freedom
    ## Multiple R-Squared: 0.6628,  Adjusted R-squared: 0.6174 
    ## F-statistic:  14.6 on 7 and 52 DF,  p-value: 2.422e-10 
    ## 
    ## 
    ## Estimation results for equation territorialmonthlychange: 
    ## ========================================================= 
    ## territorialmonthlychange = thememonth2.l1 + territorialmonthlychange.l1 + thememonth2.l2 + territorialmonthlychange.l2 + thememonth2.l3 + territorialmonthlychange.l3 + const + trend 
    ## 
    ##                              Estimate Std. Error t value Pr(>|t|)   
    ## thememonth2.l1              -51.68241   35.12076  -1.472   0.1472   
    ## territorialmonthlychange.l1   0.37830    0.13368   2.830   0.0066 **
    ## thememonth2.l2               81.67624   38.93383   2.098   0.0408 * 
    ## territorialmonthlychange.l2  -0.09322    0.13464  -0.692   0.4918   
    ## thememonth2.l3              -12.59198   35.09527  -0.359   0.7212   
    ## territorialmonthlychange.l3   0.29969    0.12900   2.323   0.0241 * 
    ## const                        -6.30662    7.43730  -0.848   0.4003   
    ## trend                         0.07336    0.10339   0.710   0.4812   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## 
    ## Residual standard error: 11.83 on 52 degrees of freedom
    ## Multiple R-Squared: 0.3152,  Adjusted R-squared: 0.223 
    ## F-statistic: 3.419 on 7 and 52 DF,  p-value: 0.004429 
    ## 
    ## 
    ## 
    ## Covariance matrix of residuals:
    ##                          thememonth2 territorialmonthlychange
    ## thememonth2                  0.00204                 -0.04632
    ## territorialmonthlychange    -0.04632                139.83579
    ## 
    ## Correlation matrix of residuals:
    ##                          thememonth2 territorialmonthlychange
    ## thememonth2                  1.00000                 -0.08674
    ## territorialmonthlychange    -0.08674                  1.00000

``` r
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

![](AN_github_v2_files/figure-gfm/unnamed-chunk-10-4.png)<!-- -->

``` r
plot(fevd(fitvar2.1))
```

![](AN_github_v2_files/figure-gfm/unnamed-chunk-10-5.png)<!-- --> Theme
3

``` r
testtheme3.1 <- dplyr::select(mydata, thememonth3,territorialmonthlychange)
testtheme3.1 <- ts(testtheme3.1)
plot(testtheme3.1)
```

![](AN_github_v2_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

``` r
#Test for stationarity
Acf(testtheme3.1[,"thememonth3"])
```

![](AN_github_v2_files/figure-gfm/unnamed-chunk-11-2.png)<!-- -->

``` r
Acf(testtheme3.1[,"territorialmonthlychange"])
```

![](AN_github_v2_files/figure-gfm/unnamed-chunk-11-3.png)<!-- -->

``` r
adf.test(testtheme3.1[,"thememonth3"])
```

    ## 
    ##  Augmented Dickey-Fuller Test
    ## 
    ## data:  testtheme3.1[, "thememonth3"]
    ## Dickey-Fuller = -2.1786, Lag order = 3, p-value = 0.5026
    ## alternative hypothesis: stationary

``` r
adf.test(testtheme3.1[,"territorialmonthlychange"])
```

    ## 
    ##  Augmented Dickey-Fuller Test
    ## 
    ## data:  testtheme3.1[, "territorialmonthlychange"]
    ## Dickey-Fuller = -2.4756, Lag order = 3, p-value = 0.3823
    ## alternative hypothesis: stationary

``` r
adf.test(diff(testtheme3.1[,"thememonth3"]))
```

    ## 
    ##  Augmented Dickey-Fuller Test
    ## 
    ## data:  diff(testtheme3.1[, "thememonth3"])
    ## Dickey-Fuller = -4.5974, Lag order = 3, p-value = 0.01
    ## alternative hypothesis: stationary

``` r
adf.test(diff(testtheme3.1[,"territorialmonthlychange"]))
```

    ## 
    ##  Augmented Dickey-Fuller Test
    ## 
    ## data:  diff(testtheme3.1[, "territorialmonthlychange"])
    ## Dickey-Fuller = -5.3789, Lag order = 3, p-value = 0.01
    ## alternative hypothesis: stationary

``` r
coint3.1 <- dynlm(thememonth3~territorialmonthlychange, data=testtheme3.1)
ehat3.1 <- resid(coint3.1)
adf.test(ehat3.1)
```

    ## 
    ##  Augmented Dickey-Fuller Test
    ## 
    ## data:  ehat3.1
    ## Dickey-Fuller = -2.0368, Lag order = 3, p-value = 0.56
    ## alternative hypothesis: stationary

``` r
####VAR Theme 3
fitvar3.1 <- VAR(testtheme3.1, p=3, type="both")
summary(fitvar3.1) # theme3.1
```

    ## 
    ## VAR Estimation Results:
    ## ========================= 
    ## Endogenous variables: thememonth3, territorialmonthlychange 
    ## Deterministic variables: both 
    ## Sample size: 60 
    ## Log Likelihood: -127.063 
    ## Roots of the characteristic polynomial:
    ## 0.8295 0.8295 0.6655 0.6655 0.4244 0.4244
    ## Call:
    ## VAR(y = testtheme3.1, p = 3, type = "both")
    ## 
    ## 
    ## Estimation results for equation thememonth3: 
    ## ============================================ 
    ## thememonth3 = thememonth3.l1 + territorialmonthlychange.l1 + thememonth3.l2 + territorialmonthlychange.l2 + thememonth3.l3 + territorialmonthlychange.l3 + const + trend 
    ## 
    ##                               Estimate Std. Error t value Pr(>|t|)    
    ## thememonth3.l1               5.357e-01  1.376e-01   3.893 0.000283 ***
    ## territorialmonthlychange.l1 -5.216e-06  5.991e-04  -0.009 0.993088    
    ## thememonth3.l2               7.246e-02  1.597e-01   0.454 0.651903    
    ## territorialmonthlychange.l2  2.119e-05  5.815e-04   0.036 0.971065    
    ## thememonth3.l3               3.312e-02  1.500e-01   0.221 0.826120    
    ## territorialmonthlychange.l3  7.635e-04  5.503e-04   1.387 0.171277    
    ## const                        2.050e-01  7.328e-02   2.798 0.007198 ** 
    ## trend                        4.933e-04  4.074e-04   1.211 0.231426    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## 
    ## Residual standard error: 0.05123 on 52 degrees of freedom
    ## Multiple R-Squared: 0.5011,  Adjusted R-squared: 0.4339 
    ## F-statistic: 7.461 on 7 and 52 DF,  p-value: 3.348e-06 
    ## 
    ## 
    ## Estimation results for equation territorialmonthlychange: 
    ## ========================================================= 
    ## territorialmonthlychange = thememonth3.l1 + territorialmonthlychange.l1 + thememonth3.l2 + territorialmonthlychange.l2 + thememonth3.l3 + territorialmonthlychange.l3 + const + trend 
    ## 
    ##                              Estimate Std. Error t value Pr(>|t|)   
    ## thememonth3.l1               72.39466   29.81787   2.428  0.01868 * 
    ## territorialmonthlychange.l1   0.24533    0.12984   1.889  0.06441 . 
    ## thememonth3.l2              -59.21997   34.60794  -1.711  0.09301 . 
    ## territorialmonthlychange.l2  -0.04520    0.12602  -0.359  0.72127   
    ## thememonth3.l3              -55.95868   32.50387  -1.722  0.09109 . 
    ## territorialmonthlychange.l3   0.36728    0.11926   3.080  0.00331 **
    ## const                        22.82544   15.88039   1.437  0.15661   
    ## trend                         0.08524    0.08829   0.965  0.33880   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## 
    ## Residual standard error: 11.1 on 52 degrees of freedom
    ## Multiple R-Squared: 0.3964,  Adjusted R-squared: 0.3152 
    ## F-statistic: 4.879 on 7 and 52 DF,  p-value: 0.000277 
    ## 
    ## 
    ## 
    ## Covariance matrix of residuals:
    ##                          thememonth3 territorialmonthlychange
    ## thememonth3                 0.002624                  0.08984
    ## territorialmonthlychange    0.089836                123.24259
    ## 
    ## Correlation matrix of residuals:
    ##                          thememonth3 territorialmonthlychange
    ## thememonth3                    1.000                    0.158
    ## territorialmonthlychange       0.158                    1.000

``` r
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

![](AN_github_v2_files/figure-gfm/unnamed-chunk-11-4.png)<!-- -->

``` r
plot(fevd(fitvar3.1))
```

![](AN_github_v2_files/figure-gfm/unnamed-chunk-11-5.png)<!-- --> Write
the VAR results

``` r
vartheme1G <- fitvar1.1[["varresult"]]$territorialmonthlychange
vartheme2G <- fitvar2.1[["varresult"]]$territorialmonthlychange
vartheme3G <- fitvar3.1[["varresult"]]$territorialmonthlychange

#plot
sjPlot::tab_model(vartheme1G, 
                  vartheme2G,
                  vartheme3G,
                  show.se = TRUE,
                  show.p = FALSE,
                  digits = 3,
                  wrap.labels = 25,
                  collapse.se = TRUE,
                  p.style = c("scientific_stars"))
```

<table style="border-collapse:collapse; border:none;">
<tr>
<th style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm;  text-align:left; ">
 
</th>
<th colspan="2" style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm; ">
y
</th>
<th colspan="2" style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm; ">
y
</th>
<th colspan="2" style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm; ">
y
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
thememonth1 l1
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
-65.609 <sup></sup><br>(46.044)
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
-158.002 – 26.784
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
territorialmonthlychange<br>l1
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
thememonth1 l2
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
-1.692 <sup></sup><br>(48.341)
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
-98.696 – 95.312
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
territorialmonthlychange<br>l2
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
thememonth1 l3
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
123.674 <sup>\*\*</sup><br>(44.330)
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
34.718 – 212.629
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
territorialmonthlychange<br>l3
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
const
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
-13.450 <sup></sup><br>(8.498)
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
-30.502 – 3.602
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
-6.307 <sup></sup><br>(7.437)
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
-21.231 – 8.617
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
22.825 <sup></sup><br>(15.880)
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">
-9.041 – 54.692
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
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
thememonth2 l1
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
-51.682 <sup></sup><br>(35.121)
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
-122.157 – 18.793
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
thememonth2 l2
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
81.676 <sup>\*</sup><br>(38.934)
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
3.550 – 159.803
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
thememonth2 l3
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
-12.592 <sup></sup><br>(35.095)
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
-83.016 – 57.832
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
thememonth3 l1
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
72.395 <sup>\*</sup><br>(29.818)
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">
12.561 – 132.229
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
thememonth3 l2
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
-59.220 <sup></sup><br>(34.608)
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">
-128.666 – 10.226
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
thememonth3 l3
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
-55.959 <sup></sup><br>(32.504)
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">
-121.182 – 9.265
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

###### Temporary Population Control

``` r
linedigital <- read.csv("https://dataverse.harvard.edu/api/access/datafile/6380746")
linedigital <- linedigital %>%
  mutate(date = ymd(linedigital$val)) %>%
  dplyr::select(-X)
head(linedigital)
```

    ##   ref             event        val       date
    ## 1   a  Twitter Takedown 2017-04-01 2017-04-01
    ## 2   b     Amaq Takedown 2018-04-01 2018-04-01
    ## 3   c Telegram takedown 2019-11-01 2019-11-01

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

![](AN_github_v2_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

``` r
ggsave("RESULTS/AN_PopulationControl_071722.tiff", width=12.5, height=5, dpi=300)
```

###### Governance

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

![](AN_github_v2_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->

``` r
ggsave("RESULTS/AN_Governance_071722.tiff", width=12.5, height=5, dpi=300)
```
