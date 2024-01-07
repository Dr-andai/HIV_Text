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


#BTM
# The Biterm Topic Model (BTM) is a word co-occurrence based topic 
# model that learns topics by modeling word-word co-occurrences patterns
# Tag parts of speech
library(udpipe)
library(data.table)
colnames(df)[4]  <- "doc_id"

anno    <- udpipe(df, "english", trace = 8)
biterms <- as.data.table(anno)
biterms <- biterms[, cooccurrence(x = lemma,
                                  relevant = upos %in% c("NOUN",
                                                         "ADJ",
                                                         "PROPN"),
                                  skipgram = 5),
                   by = list(doc_id)]

# Build BTM
library(BTM)
set.seed(588)
traindata <- subset(anno, upos %in% c("NOUN", "ADJ", "PROPN"))
traindata <- traindata[, c("doc_id", "lemma")]
model <- BTM(traindata, k = 8, 
             beta = 0.01, 
             iter = 500,
             biterms = biterms, 
             trace = 100)

# Plot Model Results (do not run when knitting)
library(ggraph)
library(textplot)
library(concaveman)
plot(model,
     top_n = 8,
     title = "BTM model",
     subtitle = "K = 8, 500 Training Iterations",
     labels = c("0", "1", "2", "3", "4", "5", "6", "7", "8"))
