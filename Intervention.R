# Intervention LDA
#set working directory
setwd('C:/Users/Admin/Documents/PROJECTS/HIV_Text')

library(dplyr) # manipulating df
library(tidyverse) # df exploration
library(tidyr) # create tidy df
library(ggplot2) # df visualization
library(wordcloud) # word clouds
library("XML") # read xml files
library(lubridate) # manipulate dates
library(naniar) # missing values
library(janitor) # df cleaning and tables
library(stringr) # work with strings
library(quanteda) # qualitative df analysis
library(ggthemes) #themes
library(extrafont)# import system fonts
extrafont::loadfonts(device = "win")


df <- read.csv('./Intervention.csv')

df$clean_text <- tolower(df$clean_text)
df$clean_text <- gsub("[[:punct:]]"," ", df$clean_text)


df[51, ]

data_corp <- corpus(df, text_field = 'clean_text') #initialize corpus 

#create a new id for the variables
docid <- paste(data_corp$NCTNumber,
               sep= " ")
docnames(data_corp) <- docid
head(docvars(data_corp))

# create tokens
corp <- tokens(data_corp, what='word',
               remove_punct = TRUE, remove_numbers = TRUE,
               remove_symbols = TRUE, remove_separators = TRUE)


# remove stopwords 
corp <- tokens_select(corp, stopwords(),selection = 'remove')
corp[[24]]
#construct a Document Feature Matrix
corp_dfm <- dfm(corp)


## Creating Topic Models
library(topicmodels)
library(ldatuning)
library(tm)
k_metrics <- ldatuning::FindTopicsNumber(
  corp_dfm,
  topics = seq(5, 50, by = 5),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 77),
  mc.cores = NA,
  return_models = FALSE,
  verbose = FALSE,
  libpath = NULL
)
FindTopicsNumber_plot(k_metrics)
#The best number of topics shows low values for CaoJuan2009 and high values for Deveaud2014
#k= number of topics

# LDA is a mathematical method for estimating the mixture of words that is 
# associated with each topic, while also determining the mixture of topics that describes each document 
tds_lda <- LDA(corp_dfm, 
               k = 8,
               method="Gibbs",
               control = list(seed = 588))# set random number generator seed

terms(tds_lda, 5)

# have a look a some of the results (posterior distributions)
tmResult <- posterior(tds_lda)
attributes(tmResult) #names -> "terms"  "topics"

# topics are probability distributions over the entire vocabulary
beta <- tmResult$terms   # get beta from results
dim(beta)                # K distributions over nTerms(DTM) terms

# for every document we have a probability distribution of its contained topics
theta <- tmResult$topics 
dim(theta)               # nDocs(DTM) distributions over K topics

topicNames <- apply(terms(tds_lda, 5), 2, paste, collapse = " ")
topicNames
