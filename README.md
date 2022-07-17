### Supplementary Code for:
### Textual Messaging of ISIS’s al-Naba and the Context Drivers that Correspond to Strategic Changes
#### Authors: [Ayse D. Lokmanoglu, PhD](https://www.adenizlok.com/); [Carol K. Winkler, PhD](https://tcv.gsu.edu/profile/carol-winkler/); [Monerah Almahmoud, PhD](https://sites.google.com/view/almahmoudm/about); [Kayla McMinimy](https://tcv.gsu.edu/profile/kayla-mcminimy/); and, [Katherine Kountz](https://tcv.gsu.edu/profile/katherine-kountz/)

The code below includes all the steps of the methodology, and the visualizations. 

The data set accompanying the code: <https://doi.org/10.7910/DVN/FAP771>

For questions, or more information on the code please contact: 
Ayse D. Lokmanoglu\
ayse [dot] lokmanoglu [at] nortwestern [dot] edu

```{r setup, }
knitr::opts_chunk$set(echo = TRUE)
knitr::opts_chunk$set(message = FALSE)
knitr::opts_chunk$set(warning = FALSE, message = FALSE) 
```

```{r }
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
```{r }
mydatalong <- read.csv("https://dataverse.harvard.edu/api/access/datafile/6380742")
head(mydatalong)

## remove the index column from csv
mydatalong<-mydatalong %>%
  mutate(date = ymd(mydatalong$Group.1)) %>%
  dplyr::select(-X, -Group.1)
```

##### Network Visualizations
```{r}
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

##### Regression Analysis
Load dataset
```{r }
mydata <- read.csv("https://dataverse.harvard.edu/api/access/datafile/5736741",
                   sep='\t',
                 header=TRUE)

## change date column to date and remove the index column from csv
mydata <- mydata %>%
  mutate(date=ymd(date)) %>%
  dplyr::select(-A)
```

###### Ideology
```{r }
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
```{r, results='asis'}
stargazer(gy_logit7,gy_logit8, gy_logit9,
          align = T, df = F,
          star.char = c("*", "**", "***"),
          star.cutoffs = c(.05, .01, .001),
          type = "html", digits = 2)

stargazer(gy_logit10,gy_logit11, gy_logit12,
          align = T, df = F,
          star.char = c("*", "**", "***"),
          star.cutoffs = c(.05, .01, .001),
          type = "html", digits = 2)

stargazer(gy_logit13,gy_logit14, gy_logit15,
          align = T, df = F,
          star.char = c("*", "**", "***"),
          star.cutoffs = c(.05, .01, .001),
          type = "html", digits = 2)
```


###### Air Strikes
```{r }
gy_logit4 <- betareg(thememonth1 ~ airstrikeslog, data = mydata)
gy_logit5 <- betareg(thememonth2 ~ airstrikeslog, data = mydata)
gy_logit6 <- betareg(thememonth3 ~ airstrikeslog, data = mydata)
```
Print for publication
```{r, results='asis'}
stargazer(gy_logit4,gy_logit5, gy_logit6,
          align = T, df = F,
          star.char = c("*", "**", "***"),
          star.cutoffs = c(.05, .01, .001),
          type = "html", digits = 2)
```

###### Territorial Control
Theme 1
```{r }
###theme1
testtheme1.1 <- dplyr::select(mydata,thememonth1,territorialmonthlychange)
testtheme1.1<- ts(testtheme1.1)
plot(testtheme1.1)

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

fitvar1.1 <- VAR(testtheme1.1, p=3, type="both")
summary(fitvar1.1) # theme1.1
causality(fitvar1.1, cause = "thememonth1")
irf_theme1.1_territory <- irf(fitvar1.1, impulse = "territorialmonthlychange", response = "thememonth1", boot = TRUE)
plot(irf_theme1.1_territory)
plot(fevd(fitvar1.1))
```
Theme 2
```{r }
testtheme2.1 <- dplyr::select(mydata, thememonth2,territorialmonthlychange)
testtheme2.1 <- ts(testtheme2.1)
plot(testtheme2.1)

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

####VAR Theme 2
fitvar2.1 <- VAR(testtheme2.1, p=3, type="both")
summary(fitvar2.1) # theme2.1
causality(fitvar2.1, cause = "thememonth2")
irf_theme2.1_territory <- irf(fitvar2.1, impulse = "territorialmonthlychange", response = "thememonth2", boot = TRUE)
plot(irf_theme2.1_territory)
plot(fevd(fitvar2.1))
```
Theme 3
```{r }
testtheme3.1 <- dplyr::select(mydata, thememonth3,territorialmonthlychange)
testtheme3.1 <- ts(testtheme3.1)
plot(testtheme3.1)

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

####VAR Theme 3
fitvar3.1 <- VAR(testtheme3.1, p=3, type="both")
summary(fitvar3.1) # theme3.1
causality(fitvar3.1, cause = "thememonth3")
irf_theme3.1_territory <- irf(fitvar3.1, impulse = "territorialmonthlychange", response = "thememonth3", boot = TRUE)
plot(irf_theme3.1_territory)
plot(fevd(fitvar3.1))
```
Write the VAR results
```{r }
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
```{r }
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
nrow(plot_all)
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
```{r include = FALSE}
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
ggsave("RESULTS/IRF_Function.tiff", width=10.5, height=5, dpi=300)
```
###### Temporary Population Control
```{r }
linedigital <- read.csv("https://dataverse.harvard.edu/api/access/datafile/6380746")
linedigital <- linedigital %>%
  mutate(date = ymd(linedigital$val)) %>%
  dplyr::select(-X)
head(linedigital)
```

```{r }
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
ggsave("RESULTS/AN_PopulationControl_071722.tiff", width=12.5, height=5, dpi=300)
```
###### Governance
``` {r include = FALSE}
lineleader <- read.csv("https://dataverse.harvard.edu/api/access/datafile/6380745")
lineleader <- lineleader %>%
  mutate(date = ymd(lineleader$val)) %>%
  dplyr::select(-X)
head(lineleader)
```

```{r }
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
ggsave("RESULTS/AN_Governance_071722.tiff", width=12.5, height=5, dpi=300)

````

