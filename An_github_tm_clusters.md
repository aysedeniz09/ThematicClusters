Al Naba Topic Modeling and Networked Clusters
================
Ayse Deniz Lokmanoglu
7/20/2022

``` r
knitr::opts_chunk$set(echo = TRUE)
knitr::opts_chunk$set(message = FALSE)
knitr::opts_chunk$set(warning = FALSE, message = FALSE) 
knitr::opts_chunk$set(eval = FALSE)
```

### Supplementary Code for:

### Textual Messaging of ISIS’s al-Naba and the Context Drivers that Correspond to Strategic Changes

#### Authors: Ayse D. Lokmanoglu, PhD; Carol K. Winkler, PhD; Monerah AlMahmoud, PhD; Kayla McMinimy; Katherine Kountz

***We will not make the actual text publicly available due to its
violent extremist content. However, it can be made available upon
individual request to the corresponding author, Ayse D. Lokmanoglu.***

Import libraries

``` r
library(stringi) 
library(stringr)
library(qdap)
library(tm)
library(ggplot2)
library(lubridate)
library(irr)
library(quanteda)
library(ldatuning)
library(topicmodels)
library(textcat)
library(parallel)
library(RSQLite)
library(doParallel)
library(scales)
library(lsa)
library(igraph)
library(cld2) 
library(tidyverse)
library(tidytext)
library(dplyr)
library(rgexf)
library(openxlsx)
library(ggthemes)
```

Load pre-processed data frame with text column labelled as text. 1.
Tokenize it

``` r
toks <- tokens(mydata$text,
               remove_punct = TRUE,
               remove_symbols = TRUE,
               remove_numbers = TRUE,
               remove_url = TRUE,
               remove_separators = TRUE,
               split_hyphens = FALSE,
               include_docvars = TRUE,
               padding = FALSE) %>%
  tokens_remove(stopwords(language = "ar", source = "misc")) %>% #for this we used combined stopwords list from google with quanteda 
  tokens_select(min_nchar = 2)
```

2.  Change it into a [document-feature
    matrix](https://quanteda.io/reference/dfm.html)

``` r
dfm_counts<- dfm(toks) 
rm(toks) #remove unused files to save space
```

3.  Match your dfm object with your original data frame through index

``` r
docnames(dfm_counts)<-mydata$index
```

4.  Check for sparsity and trim accordingly

``` r
sparsity(dfm_counts)
dfm_counts2<-dfm_trim(dfm_counts, max_docfreq = 0.95, min_docfreq=0.05,docfreq_type="prop")
sparsity(dfm_counts2)
rm(dfm_counts) #remove for space
```

5.  Convert dfm object to an LDA object

``` r
dtm_lda <- convert(dfm_counts2, to = "topicmodels",docvars = dfm_counts2@docvars)
full_data<-dtm_lda
n <- nrow(full_data) #number of rows for cross-validation method
rm(dfm_counts2) #remove for space
```

6.  Run the cross-validation, save the results. The method is from the
    [supplemental
    code](https://github.com/aysedeniz09/AJPH2020/blob/master/AJPH%20GITupload.R),
    citation: Walter, D., Ophir, Y., & Jamieson, K. H. (2020). Russian
    Twitter Accounts and the Partisan Polarization of Vaccine Discourse,
    2015–2017. American Journal of Public Health, 110(5), 718–724.
    <https://doi.org/10.2105/AJPH.2019.305564>.

``` r
print(Sys.time())
# create container for results
MainresultDF<-data.frame(k=c(1),perplexity=c(1),myalpha=c("x"))
MainresultDF<-MainresultDF[-1,]
# set possible alpha and k values
candidate_alpha<- c(0.01, 0.05, 0.1, 0.2, 0.5) # candidates for alpha values
candidate_k <- c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100) # candidates for how many topics
# run the 10-fold cross validation
for (eachalpha in candidate_alpha) { 
  print ("now running ALPHA:")
  print (eachalpha)
  print(Sys.time())
  cluster <- makeCluster(detectCores(logical = TRUE) - 1) # We are leaving one Core spare. If number of corse on pc is 1, then -1 in this line should be removed.
  registerDoParallel(cluster)
  clusterEvalQ(cluster, {
    library(topicmodels)
  })
  folds <- 10
  splitfolds <- sample(1:folds, n, replace = TRUE)
  clusterExport(cluster, c("full_data", "splitfolds", "folds", "candidate_k"))
  system.time({
    results <- foreach(j = 1:length(candidate_k), .combine = rbind) %dopar%{
      k <- candidate_k[j]
      print(k)
      results_1k <- matrix(0, nrow = folds, ncol = 2)
      colnames(results_1k) <- c("k", "perplexity")
      for(i in 1:folds){
        train_set <- full_data[splitfolds != i , ]
        valid_set <- full_data[splitfolds == i, ]
        
        fitted <- LDA(train_set, k = k, method = "Gibbs",
                      control = list(alpha=eachalpha) )
        
        results_1k[i,] <- c(k, perplexity(fitted, newdata = valid_set))
      }
      return(results_1k)
    }
  })
  stopCluster(cluster)
  results_df <- as.data.frame(results)
  results_df$myalpha<-as.character(eachalpha)
  MainresultDF<-rbind(MainresultDF,results_df)
}
print ("DONE!")
print(Sys.time())
save(MainresultDF, file="Main_Results_DF_DATE.Rda")
```

7.  Examine the output by visualizing

``` r
MainresultDF$kalpha=paste0(as.character(MainresultDF$k),MainresultDF$myalpha) 
ggplot(MainresultDF) +geom_boxplot(aes(x=k, y=perplexity, group=kalpha,color=myalpha))

ggplot(MainresultDF) +
  geom_boxplot(aes(x=k, y=perplexity, group=kalpha,color=myalpha))+
  geom_hline(yintercept=min(MainresultDF$perplexity[which(MainresultDF$myalpha==0.5)]),linetype = "dotted")+
  geom_hline(yintercept=min(MainresultDF$perplexity[which(MainresultDF$myalpha==0.2)]),linetype = "dotted")+
  geom_hline(yintercept=min(MainresultDF$perplexity[which(MainresultDF$myalpha==0.1)]),linetype = "dotted")+
  geom_hline(yintercept=min(MainresultDF$perplexity[which(MainresultDF$myalpha==0.05)]),linetype = "dotted")+
  geom_hline(yintercept=min(MainresultDF$perplexity[which(MainresultDF$myalpha==0.01)]),linetype = "dotted")

ggplot(MainresultDF)+geom_line(aes(x=k, y=mean(perplexity),color=myalpha))
ggplot(MainresultDF)+geom_smooth(se = FALSE, aes(x=k, y=perplexity,color=myalpha))
ggsave("Alpha and Perplexity.jpg")

ggplot(MainresultDF) +
  geom_boxplot(aes(x=k, y=perplexity, group=kalpha,color=myalpha))+
  geom_smooth(se = TRUE, aes(x=k, y=perplexity,color=myalpha))


p<-ggplot(MainresultDF, aes(x = k, y = perplexity))
Alpha<-MainresultDF$myalpha
p+geom_point(aes(color=Alpha),size=0.1)+geom_smooth(se = FALSE, aes(color=Alpha))+
  ggtitle("5-fold cross-validation of topic modelling (5% of data)",
          "(ie five different models fit for each candidate number of topics)") +
  labs(x = "Candidate number of topics", y = "Perplexity when fitting the trained model to the hold-out set")
ggsave("Alpha and Perplexity.jpg")

ggplot(MainresultDF) +
  geom_boxplot(aes(x=k, y=perplexity, group=kalpha,color=myalpha))+
  scale_color_discrete(name = "Alpha Levels")+
  xlab("K (Number of Topics)")+
  ylab("Perplexity")
b<-ggplot(MainresultDF) +
  geom_smooth(se = FALSE, aes(x=k, y=perplexity,color=myalpha)) +  scale_x_continuous(breaks = seq(from = 0, to = 150, by = 20)) +
  xlab("Topics (k)")+
  ylab("Perplexity")+
  theme_wsj(color="white") +
  theme(text=element_text(size=10,family="Times New Roman"),
        title=element_text(size=12,family="Times New Roman"),
        axis.text.x=element_text(angle=60, hjust=1, family="Times New Roman"),
        axis.text.y=element_text(family="Times New Roman"),
        axis.title.x=element_text(vjust=-0.25, size=10, family="Times New Roman"),
        axis.title.y=element_text(vjust=-0.25, size=10, family="Times New Roman"),
        legend.position="top", legend.box="vertical", legend.margin=margin(),
        legend.key = element_rect(fill="white"), legend.background = element_rect(fill=NA),
        legend.text=element_text(size=8, family="Times New Roman"))
```

8.  Choose the alpha and test approximate topic numbers, in our case it
    was alpha=0.05 and k=30

``` r
# Identify 2nd derivative max point on perplexity  
MainresultDF_MYALPHA<-MainresultDF[MainresultDF$myalpha==0.05,]
cars.spl <- with(MainresultDF_MYALPHA, smooth.spline(k, perplexity, df = 3))
plot(with(cars, predict(cars.spl, x = MainresultDF_MYALPHA$k, deriv = 2)), type = "l",
     #main = "My title",
     #sub = "My subtitle",
     xlab = "Topics (k)",
     ylab = "Perplexity Second Derivative") + abline(v=30)
data<-data.frame(with(cars, predict(cars.spl, x = MainresultDF_MYALPHA$k, deriv = 2)))

a<- ggplot(data,aes(x=x,y=y))+
  geom_line(color = "grey11")+
  geom_vline(xintercept=30) +
  scale_x_continuous(breaks = seq(from = 0, to = 150, by = 20)) +
  ggtitle(" ")+
  xlab("Topics (k)")+
  ylab("Perplexity Second Derivative")+
  theme_wsj(color="white")+
  theme(text=element_text(size=10,family="Times New Roman"),
        title=element_text(size=12,family="Times New Roman"),
        axis.text.x=element_text(angle=60, hjust=1, family="Times New Roman"),
        axis.text.y=element_text(family="Times New Roman"),
        axis.title.x=element_text(vjust=-0.25, size=10, family="Times New Roman"),
        axis.title.y=element_text(vjust=-0.25, size=10, family="Times New Roman"),
        legend.position="bottom", legend.box="vertical", legend.margin=margin(),
        legend.key = element_rect(fill="white"), legend.background = element_rect(fill=NA),
        legend.text=element_text(size=8, family="Times New Roman"))
ggpubr::ggarrange(b, a, ncol = 1, nrow = 2)
```

![Al-naba Topic Number Perplexity
Graph](https://github.com/aysedeniz09/ThematicClusters/blob/main/Cross_Validation_Results.jpg?raw=true)

### Topic Modeling

1.  Run the topic model for the identified k and alpha

``` r
lda.30.05 <- LDA(dtm_lda,30,
                 method = "Gibbs",
           control = list(alpha=0.05,seed=125231)) #set a random seed 
save(lda.30.05, file="AN_k30_DATE.Rda")

LDAfit<-lda #copy the object it with a different name for backup
```

2.  Extract top words from the topic model

``` r
#mark text column
datacolnum=which( colnames(mydata)=="text")

myw=0.3
word_beta_sums<-rowSums(mybeta)
my_beta_for_frex<-mybeta
for (m in 1:ncol(my_beta_for_frex)) {
  for (n in 1:nrow(my_beta_for_frex)) {
    my_beta_for_frex[n,m]<-1/(myw/(my_beta_for_frex[n,m]/word_beta_sums[n])+((1-myw)/my_beta_for_frex[n,m]))
  }
  print (m)
}
nwords=100
topwords <- my_beta_for_frex[1:nwords,]
for (i in 1:LDAfit@k) {
  tempframe <- my_beta_for_frex[order(-my_beta_for_frex[,i]),]
  tempframe <- tempframe[1:nwords,]
  tempvec<-as.vector(rownames(tempframe))
  topwords[,i]<-tempvec
}
rownames(topwords)<-c(1:nwords)
topwords<-data.frame(topwords)
openxlsx::write.xlsx(topwords, file="AN_k30_topwords_DATE.xlsx")
```

3.  Extract top articles

``` r
metadf<-mydata #your original dataframe
meta_theta_df<-cbind(metadf[,"text"],LDAfit@gamma)
ntext=30
toptexts <- mybeta[1:ntext,]
for (i in 1:LDAfit@k) {
  print(i)
  tempframe <- meta_theta_df[order(-as.numeric(meta_theta_df[,i+1])),]
  tempframe <- tempframe[1:ntext,]
  tempvec<-as.vector(tempframe[,1])
  toptexts[,i]<-tempvec
}
rownames(toptexts)<-c(1:ntext)
toptexts<-data.frame(toptexts)
openxlsx::write.xlsx(toptexts, file="AN_k30_toptexts_DATE.xlsx")
```

4.  Extract issue and page for top articles

``` r
meta_theta_df2<-cbind(metadf[,"issuepage"],LDAfit@gamma)
ntext=30
toptextspage <- mybeta[1:ntext,]
for (i in 1:LDAfit@k) {
  print(i)
  tempframe <- meta_theta_df2[order(-as.numeric(meta_theta_df2[,i+1])),]
  tempframe <- tempframe[1:ntext,]
  tempvec<-as.vector(tempframe[,1])
  toptextspage[,i]<-tempvec
}
rownames(toptextspage)<-c(1:ntext)
toptextspage<-data.frame(toptextspage)
openxlsx::write.xlsx(toptextspage, file="AN_k30_topissuepage_DATE.xlsx")
```

5.  Save the complete file with topic loadings and all information

``` r
meta_theta_df3<-cbind(metadf,LDAfit@gamma)
first<-which(colnames(meta_theta_df3)=="1")
last<-ncol(meta_theta_df3)
colnames(meta_theta_df3)[first:last] <- paste("X", colnames(meta_theta_df3[,c(first:last)]), sep = "_")
save(meta_theta_df3, file="AN_k30_MetaThetaDF_DATE.Rda")
```

### Networked Clusters

The method is from the [supplemental
code](https://github.com/DrorWalt/ANTMN), citation: Walter, D., & Ophir,
Y. (2019). News Frame Analysis: An Inductive Mixed-Method Computational
Approach. Communication Methods and Measures.
<https://doi.org/10.1080/19312458.2019.1639145>.

1.  Load topic names assigned by researchers

``` r
mynames<-c('Negotiations', 
           'Conspiracy', 
           'Jihad', 
           'Territory', 
           'Faith', 
           'The Five Pillars of Islam', 
           'Death', 
           'War with Enemies',
           'War in Syria', 
           'War Equipment', 
           'Media in Africa', 
           'Shiite', 
           'Duty', 
           'Money and Currency', 
           'Technology', 
           'Vision',
           'Prophetic Times', 
           'Shiite (The Twelver)',
           'Shiite Conspiracy',
           'The War with Rafidi',
           'Recruitments',
           'Nusayri Army',
           'Social Services',
           'Apostates',
           'Foreign Policy',
           'Mockery',
           'Mujahideen & Apostates',
           'Muslim Women')
```

2.  Calculate the topic sizes

``` r
#use meta_theta_df3
first<-which(colnames(meta_theta_df3)=="X_1")
last<-which(colnames(meta_theta_df3)=="X_30")
topicsize<-colMeans(meta_theta_df3[,first:last])
```

3.  Create [ANTMN](https://github.com/DrorWalt/ANTMN) function

``` r
# load libraries
library(igraph)
library(corpustools)
library(lsa)

network_from_LDA<-function(LDAobject,deleted_topics=c(),topic_names=c(),save_filename="",topic_size=c(),bbone=FALSE) {
  # Importing needed packages
  require(lsa) # for cosine similarity calculation
  require(dplyr) # general utility
  require(igraph) # for graph/network managment and output
  require(corpustools)
  
  print("Importing model")
  
  # first extract the theta matrix form the topicmodel object
  theta<-LDAobject@gamma
  # adding names for culumns based on k
  colnames(theta)<-c(1:LDAobject@k)
  
  # calculate the adjacency matrix using cosine similarity on the theta matrix
  mycosine<-cosine(as.matrix(theta))
  colnames(mycosine)<-colnames(theta)
  rownames(mycosine)<-colnames(theta)
  
  # Convert to network - undirected, weighted, no diagonal
  
  print("Creating graph")
  
  topmodnet<-graph.adjacency(mycosine,mode="undirected",weighted=T,diag=F,add.colnames="label") # Assign colnames
  # add topicnames as name attribute of node - importend from prepare meta data in previous lines
  if (length(topic_names)>0) {
    print("Topic names added")
    V(topmodnet)$name<-topic_names
  } 
  # add sizes if passed to funciton
  if (length(topic_size)>0) {
    print("Topic sizes added")
    V(topmodnet)$topic_size<-topic_size
  }
  newg<-topmodnet
  
  # delete 'garbage' topics
  if (length(deleted_topics)>0) {
    print("Deleting requested topics")
    
    newg<-delete_vertices(topmodnet, deleted_topics)
  }
  
  # Backbone
  if (bbone==TRUE) {
    print("Backboning")
    
    nnodesBASE<-length(V(newg))
    for (bbonelvl in rev(seq(0,1,by=0.05))) {
      #print (bbonelvl)
      nnodes<-length(V(backbone_filter(newg,alpha=bbonelvl)))
      if(nnodes>=nnodesBASE) {
        bbonelvl=bbonelvl
        #  print ("great")
      }
      else{break}
      oldbbone<-bbonelvl
    }
    
    newg<-backbone_filter(newg,alpha=oldbbone)
    
  }
  
  # run community detection and attach as node attribute
  print("Calculating communities")
  
  mylouvain<-(cluster_louvain(newg)) 
  mywalktrap<-(cluster_walktrap(newg)) 
  myspinglass<-(cluster_spinglass(newg)) 
  myfastgreed<-(cluster_fast_greedy(newg)) 
  myeigen<-(cluster_leading_eigen(newg)) 
  
  V(newg)$louvain<-mylouvain$membership 
  V(newg)$walktrap<-mywalktrap$membership 
  V(newg)$spinglass<-myspinglass$membership 
  V(newg)$fastgreed<-myfastgreed$membership 
  V(newg)$eigen<-myeigen$membership 
  V(newg)$degree <- degree(newg)                        # Degree centrality
  V(newg)$eig <- evcent(newg)$vector                    # Eigenvector centrality
  V(newg)$hubs <- hub.score(newg)$vector                # "Hub" centrality
  V(newg)$authorities <- authority.score(newg)$vector   # "Authority" centrality
  V(newg)$closeness <- closeness(newg)                  # Closeness centrality
  V(newg)$betweenness <- betweenness(newg)  
  # if filename is passsed - saving object to graphml object. Can be opened with Gephi.
  if (nchar(save_filename)>0) {
    print("Writing graph")
    write.graph(newg,paste0(save_filename,".graphml"),format="graphml")
  }
  
  # graph is returned as object
  return(newg)
}
```

4.  Run the ANTMN function with your lda object

``` r
mynewnet<-network_from_LDA(LDAobject=LDAfit,
                           topic_names=mynames,
                           topic_size=topicsize,
                           deleted_topics=deleted_topics=c(8, 26),                     save_filename="AN_antmn_DATE",
                           bbone=TRUE)

save(mynewnet, file="AN_antmn_DATE.Rda")
```

5.  Open file in [Gephi](https://gephi.org/) to visualize it with Louvain algorithm.![Network
    Graph](https://github.com/aysedeniz09/ThematicClusters/blob/main/AN%20Network%208.8.21.png?raw=true)
6.  Calculate networked clusters

``` r
## Orange is 1 Offline Caliphate
## Purple is 2 Online Caliphate
## Green is 3 Regional Enemies
meta_theta_df_comm_lv<-meta_theta_df3
which(colnames(meta_theta_df_comm_lv)=="X1") #### -1 for rowsums
meta_theta_df_comm_lv$OrangeG1<-rowSums(meta_theta_df_comm_lv[,(as.numeric(V(mynewnet)$label[which(V(mynewnet)$louvain == 1)]))+7]) 
meta_theta_df_comm_lv$PurpleG2<-rowSums(meta_theta_df_comm_lv[,(as.numeric(V(mynewnet)$label[which(V(mynewnet)$louvain == 2)]))+7]) 
meta_theta_df_comm_lv$GreenG3<-rowSums(meta_theta_df_comm_lv[,(as.numeric(V(mynewnet)$label[which(V(mynewnet)$louvain == 3)]))+7]) 
save(meta_theta_df_comm_lv, file="AN_k30_Meta_Theta_Df_Louvain_DATE.Rda")
```

7.  Calculate networked clusters daily, the dataset
    [downloadurl](https://dataverse.harvard.edu/api/access/datafile/6388550)

``` r
# color codes orangered1, purple4, springgreen4
mydatatemp<-aggregate(x=meta_theta_df_comm_lv[,which(colnames(meta_theta_df_comm_lv)=="OrangeG1"):which(colnames(meta_theta_df_comm_lv)=="GreenG3")],by=list(meta_theta_df_comm_lv$Date),FUN="mean")

mydatalong<-mydatatemp %>% 
  mutate(date = ymd(Group.1)) %>%
  dplyr::select(date,
                OrangeG1,
                PurpleG2,
                GreenG3) %>%
  rename('Offline Caliphate' = 'OrangeG1',
         'Online Caliphate' = 'PurpleG2',
         'Regional Enemies' = 'GreenG3') %>%
  pivot_longer(!date, names_to = "theme", values_to = "theta") 
```

8.  Graph

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
ggarrange(prevalance + rremove("xlab"), line +rremove("xlab"), 
          ncol = 1, 
          nrow = 2, 
          common.legend = TRUE, legend = "bottom",
          align = c("h"))
```

![Image](https://github.com/aysedeniz09/ThematicClusters/blob/main/Prevalance_Prominence.jpg?raw=true)
