###################### Part 2 - Plotting ######################

##############################Install and Load the Packages

#2.1 - install.packages("tidyverse"),install.packages('rworldmap',dependencies=TRUE) ---

library(wordcloud)
library(tm)
library(tidytext)
library(tidyverse)
library(tidyr)
library(ggplot2)
library(rworldmap)
library(ggraph)
library(igraph)
library(dplyr)
library(gapminder)
library(widyr)
library(gridExtra)
library(grid)
library(ggstatsplot)
library(plotly)

#2.2-Load the finalDataset into the working area--- 

#2.3- Save finalDataset into new Dataframe along with the line sequence - 1172 observations---
text_tb <- tibble(Line = seq_along(finalDataset$TextForAnalyze),
                  text = finalDataset$TextForAnalyze,
                  Name = finalDataset$Name,
                  Size = finalDataset$Size,
                  Country = finalDataset$Country,
                  Region = finalDataset$Region,
                  Title = finalDataset$Title,
                  Type = finalDataset$Type,
                  Year = finalDataset$`Publication Year`,
                  Statuses = finalDataset$Status
)


#2.4- Retrieve positive and negative sentiments for words (All_counts- 965 observations)

bing <- get_sentiments("bing")
All_counts <- na.omit(text_tb) %>%
  select(everything()) %>%
  group_by(Country,Region,Size,Title,Type) %>%
  unnest_tokens(word, text)%>%
  inner_join(bing) %>%
  count(word, sentiment, sort = TRUE)%>%
  ungroup()

All_counts_Countrywisw <- na.omit(text_tb) %>%
   group_by(Country) %>%
  unnest_tokens(word, text)%>%
  inner_join(bing) %>%
  count(word, sentiment, sort = TRUE)

# Aggregation for Countrywise Sentiment Analysis
Aggregatecoutrywise<-aggregate(All_counts_Countrywisw$n, by=list(Category=All_counts_Countrywisw$sentiment,All_counts_Countrywisw$Country), FUN=sum)

#2.5- All Country - Sentiment Analysis with scatter plot
png("SentimentAnalysis.png")
ggplot( na.omit(text_tb) %>%
          select(everything()) %>%
          unnest_tokens(word, text)%>%
          inner_join(bing) %>%
          count(word, sentiment, sort = TRUE)%>%
          ungroup()
        , aes(x=word, y=n, color=as.factor(sentiment) )) + 
  geom_point(alpha = .85,size=1)+  
  facet_wrap(~sentiment) +
  theme(legend.title=element_blank()) +
  xlab("Words")+
  ylab("Occurences")
#dev.off()

#2.6- Word Cloud


wordcloud(words = unique(All_counts$word), freq = All_counts$n, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

#2.7 countrywise Sentiment Analysis plot

#create a map-shaped window
mapDevice('x11')
#join to a coarse resolution map
spdf <- joinCountryData2Map(All_counts_Countrywisw, joinCode="NAME", nameJoinColumn="Country")
colourPalette <- c( 'red'
                   ,'darkgreen')
                   
mapCountryData(spdf, 
               nameColumnToPlot="sentiment"
               ,catMethod="diverging"
               ,mapTitle ="Countrywise Sentiment Analysis"
               ,numCats = length(table(All_counts_Countrywisw$sentiment))
               ,colourPalette=colourPalette
               
               )




#2.8 Word Clustering - taken only more than 0.65 corelated values

#png("wordclustering.png") #- to save image with highresolution

set.seed(123)
(word_pairs <- All_counts %>%
    group_by(Country) %>%
    pairwise_cor(word, Country, sort = TRUE)%>%
    filter(!is.na(correlation),correlation > .65)%>%
    graph_from_data_frame()%>%
    ggraph(layout = "fr") +
    geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
    geom_node_point(color = "lightblue", size = 5) +
    geom_node_text(aes(label = name), repel = TRUE) +
    theme_void()
)

#dev.off()
############################################create sub Dataset


CountDataset<- text_tb %>%
  count(Region,Year,Size,Statuses, sort = TRUE) 

##Scales
CountDatasetForScale<- text_tb %>%
  count(Region,Year,Size, sort = TRUE) 

#2.9 Regionwise Facetting

#png("regionwisefacet.png") #To save image

ggplot(CountDataset, aes(Year, n, size = Region, colour = Region)) +
  geom_point(alpha = 0.5) +
  #scale_colour_manual(values = country_colors) +
  theme(legend.position = "none") +
  facet_wrap(~ Region)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#dev.off() #To save image

#3.0 Company Scale Facetting

#png("SizPlot.png") 
ggplot(CountDatasetForScale, aes(Year, n, size = Size, colour = Size)) +
  geom_point(alpha = 0.5) +
  #scale_colour_manual(values = country_colors) +
  theme(legend.position = "none") +
  facet_wrap(~ Size)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#dev.off()


#3.1 Histogram with GRI Statuses

CountDatasetHist <-as.data.frame(na.omit(CountDataset))

#png("StatusHistWithNA.png")  # with Na values

ggplot(CountDataset,aes(as.numeric(Year),fill=as.factor(Statuses)))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(fill="Statuses")+
  scale_x_continuous(name="Year", labels = 2000:2019, breaks =  2000:2019)+ 
  geom_histogram(alpha=0.5) 
  
#dev.off()

#png("StatusHistWithoutNA.png")  # without Na values

ggplot(CountDatasetHist,aes(as.numeric(Year),fill=as.factor(Statuses)))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(fill="Statuses")+
  # theme(legend.title=element_blank()) +
  scale_x_continuous(name="Year", labels = 2000:2019, breaks =  2000:2019)+ 
  geom_histogram(alpha=0.5) 

#dev.off()


# 3.2 pair Frequencies


(word_pairs_Corelation <- unique(All_counts) %>%
                    select(everything()) %>%
                    group_by(Country) %>%
                    pairwise_cor(word, Country, sort = TRUE)%>%
                    ungroup()
  
  )


write.csv(word_pairs_Corelation,"Corelationpair.csv")#save the dataframe values into csv



#Created by Uthistra Karungaran




