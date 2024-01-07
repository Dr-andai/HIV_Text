#Clinical trials text analysis
#set working directory
setwd('C:/Users/Admin/Documents/PROJECTS/HIV_Text')


library(dplyr) # manipulating data
library(tidyverse) # data exploration
library(tidyr) # create tidy data
library(ggplot2) # data visualization
library(wordcloud) # word clouds
library("XML") # read xml files
library(lubridate) # manipulate dates
library(naniar) # missing values
library(janitor) # data cleaning and tables
library(stringr) # work with strings
library(quanteda) # qualitative data analysis
library(ggthemes) #themes
library(extrafont)# import system fonts
extrafont::loadfonts(device = "win")


#Load Clinical Trials in Kenya Data Set
# clinicals <- xmlToDataFrame("KenyaTrials.xml")
KE_clinicals <- xmlToDataFrame("KenyaTrials.xml")
TZ_clinicals <- xmlToDataFrame("TanzaniaTrials.xml")
UG_clinicals <- xmlToDataFrame("UgandaTrials.xml")


# # bind data frame
clinicals <- bind_rows(KE_clinicals,TZ_clinicals,UG_clinicals)


#Transform Date Column from character to Date variable
class(clinicals$Date_registration) ##'character'
#using the lubridate library
clinicals <- clinicals %>%
  mutate(Date_registration = lubridate::dmy(Date_registration))

class(clinicals$Date_registration)##'Date'

# change column names to lower case
names(clinicals)
clinicals <- janitor::clean_names(clinicals)



#What to do?
## analysis on titles
## extract topics that focus on children


# -- Data was uploaded on 14th/06/2023

# Create a data frame for Clinical Trials conducted over the last 5 years and 20 years
# Obtain the following columns: Trial ID, Date of Registration(year and month),Condition and Scientific Title

fiveyears <- clinicals[clinicals$date_registration >= "2019-01-01" & clinicals$date_registration <= "2023-06-01", ]
twentyyears <- clinicals[clinicals$date_registration >= "2003-01-01" & clinicals$date_registration <= "2023-12-31", ]

# create year and month date variables - method 1
years_date <- twentyyears %>%
  mutate(date_registration = ymd(date_registration)) %>%
  mutate_at(vars(date_registration),list(year, month))
names(years_date)

#Create Year and Month date variables - method 2
years_m <- twentyyears %>% 
  mutate(year = year(date_registration),
         month = month(date_registration))
names(years_m)
class(years_m$year)



# data frame Containing Clinical Trials from 2003.

data <- years_m[ , c('trial_id','date_registration','phase','primary_sponsor','condition','scientific_title',
                     'intervention','countries','target_size','primary_outcome','secondary_outcome','web_address')]



## Data Cleaning
# Look for missing values
pct_miss(data$condition) ## 3.921569% of the data set don't have conditions
# drop na
data$condition [data$condition ==""] <- NA
data <- data %>%
  drop_na(condition)

# Remove symbols, duplicates... etc from conditions column
# data <- unique( data[ , c('trial_id','years_a','condition','scientific_title') ] )
data <- unique( data[ , c('trial_id','date_registration','phase','primary_sponsor','condition',
                          'scientific_title','intervention','countries','target_size','primary_outcome','secondary_outcome','web_address') ] )

data$condition <- tolower(data$condition)
data$condition <- gsub("<br>","", data$condition)
data$condition <- gsub(";\\s",";", data$condition)
data$condition <- gsub("[.,;:-]"," ", data$condition)
data$condition <- gsub("\\(|\\)", "", data$condition)
data$condition <- gsub("coronavirus|coronavirus 2|sars cov 2|covid\\s+19","covid", data$condition)
data$condition <- gsub("health condition 1"," ", data$condition)
data$condition <- gsub("hiv/aids|hiv 1|aids","hiv", data$condition)
data$condition <- gsub("sickle cell disease|scd","sickle", data$condition)
data$condition <- gsub("infections","infection", data$condition)
data$condition <- gsub("diseases","disease", data$condition)
data$condition <- gsub("disorders","disorder", data$condition)
data$condition <- gsub("[0-9]+"," ", data$condition)
data$condition <- gsub("\\b[a-zA-Z]\\b", " ", data$condition)
# Function to remove duplicate words in a string
remove_duplicates <- function(string) {
  string <- trimws(string)
  words <- strsplit(string, " ")[[1]]
  unique_words <- unique(words)
  result <- paste(unique_words, collapse = " ")
  return(result)
}
# Apply the function to the 'conditions' column
data$condition <- sapply(data$condition, remove_duplicates)

# clean scientific title column
data$scientific_title <- tolower(data$scientific_title)
data$scientific_title <- gsub("pahse", "phase",data$scientific_title)
data$scientific_title <- gsub("phase 1|phase i", "phasei",data$scientific_title)
data$scientific_title <- gsub("phase 2|phase ii", "phaseii",data$scientific_title)
data$scientific_title <- gsub("phase 3|phase iii", "phaseiii",data$scientific_title)
data$scientific_title <- gsub("phase 4|phase iv", "phaseiv",data$scientific_title)
data$scientific_title <- gsub("coronavirus|coronavirus 2|sars cov 2|covid-19|covid\\s+19","covid", data$scientific_title)
data$scientific_title <- gsub("hiv/aids|hiv 1|aids","hiv", data$scientific_title)
data$scientific_title <- gsub("[.,;:-]"," ", data$scientific_title)

#clean intervention column
data$intervention <- tolower(data$intervention)
data$intervention <- gsub("<br>","", data$intervention)
data$intervention <- gsub("[.,;:-]"," ", data$intervention)
data$intervention <- gsub("\\(|\\)", "", data$intervention)
data$intervention <- gsub("/", "", data$intervention)

#clean sponsor column
#clean intervention column
data$primary_sponsor <- tolower(data$primary_sponsor)

#clean countries column
data$countries <- tolower(data$countries)
data$countries <- gsub("[.,;:-]"," ", data$countries)
# Function to remove duplicate words in a string
remove_duplicates <- function(string) {
  string <- trimws(string)
  words <- strsplit(string, " ")[[1]]
  unique_words <- unique(words)
  result <- paste(unique_words, collapse = " ")
  return(result)
}
# Apply the function to the 'conditions' column
data$countries <- sapply(data$countries, remove_duplicates)

data[31, ]


# text column: scientific title column
data$text <- paste(data$scientific_title, sep=" ")
row.names(data) <- NULL

data_1 <- data[ , c('trial_id','year','month','text')]












## Text Mining for Biterm Modelling
# Create a corpus of the document

data_corp <- corpus(data_1, text_field = 'text') #initialize corpus for scientific title

summary(data_corp,3)
#create a new id for the variables
docid <- paste(data_corp$NCTNumber,
               data_corp$year,
               data_corp$month,
               sep= " ")
docnames(data_corp) <- docid
head(docvars(data_corp))


# create tokens
corp <- tokens(data_corp, what='word',
               remove_punct = TRUE, remove_numbers = TRUE,
               remove_symbols = TRUE, remove_separators = TRUE)
# stem
corp <- tokens_wordstem(corp, language = 'en')

# remove stopwords 
corp <- tokens_select(corp, stopwords(),selection = 'remove')
corp[[24]]
#construct a Document Feature Matrix
corp_dfm <- dfm(corp)



# Data visualization
# Visualize a wordcloud of the Conditions and Scientific titles
# Create a bigram using tokens_ngram
toks_ngram <- tokens_ngrams(corp, n = 2, concatenator = " ")
head(toks_ngram[[1]], 30)

# wordcloud plot
# load quanteda packages
library("quanteda.textplots")
library("quanteda.textstats")
bigram <- dfm(toks_ngram)
bigram_freq<-textstat_frequency(bigram)

textplot_wordcloud(bigram,max_words = 100,
                   ordered_color = TRUE)


# Observe the distribution of date of registration over the years
years_m %>%
  ggplot(aes(x=date_registration, color= factor(month)))+
  geom_bar(show.legend = FALSE)+
  labs(y="Date",
       x= "Article counts",
       title = "Scientific titles")

# Asses the frequency distribution of the bigrams, data grouped per year
word_freq <- textstat_frequency(bigram, groups = data$year)

# frequency plot of the top 10 most frequent word
features_corpdfm <- textstat_frequency(bigram, n=10)
features_corpdfm$feature <- with(features_corpdfm, reorder(feature, -frequency))
ggplot(features_corpdfm, aes(feature, frequency))+
  geom_col(fill="#0B1354")+
  labs (title = "Top 10 Frequent bigrams",
        subtitle = "2018-2023",
        y = "",
        x = "")+
  theme_few()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size=17, face="bold"))


# top frequent bigrams occurring each year
top_words <- word_freq %>%
  group_by (group) %>%
  top_n(15, frequency) %>% # Change the number to show more or fewer words
  ungroup ()

top_words %>%
  mutate(group = as.factor(group)) %>% 
  ggplot(aes(feature,frequency, fill=group)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~group, scales = "free_y") +
  coord_flip()+
  scale_y_continuous(expand = c(0,0)) +
  labs(x = "", y = "", title = "Conditions Spread Over 5 years") 


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
               k = 11,
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


# Visualizing topics based on the one interested
topicToViz <- 3
# select to 40 most probable terms from the topic by sorting the term-topic-probability vector in decreasing order
top40terms <- sort(tmResult$terms[topicToViz,], decreasing=TRUE)[1:40]
words <- names(top40terms)
# extract the probabilities of each of the 40 terms  
probabilities <- sort(tmResult$terms[topicToViz,], decreasing=TRUE)[1:40]
# visualize the terms as wordcloud
mycolors <- brewer.pal(8, "Dark2")
wordcloud(words, probabilities, random.order = FALSE, color = mycolors)





#BTM
# The Biterm Topic Model (BTM) is a word co-occurrence based topic 
# model that learns topics by modeling word-word co-occurrences patterns
# Tag parts of speech
library(udpipe)
library(data.table)
colnames(data_1)[1]  <- "doc_id"

anno    <- udpipe(data_1, "english", trace = 11)
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
model <- BTM(traindata, k = 11, 
             beta = 0.01, 
             iter = 500,
             biterms = biterms, 
             trace = 100)

# Plot Model Results (do not run when knitting)
library(ggraph)
library(textplot)
library(concaveman)
plot(model,
     top_n = 11,
     title = "BTM model",
     subtitle = "K = 11, 500 Training Iterations",
     labels = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9",
                "10","11"))




# pic one group from BTM 
sickle <- data[grep("sickl", data$text), ]
row.names(sickle) <- NULL

### sickle_data <- clinicals[grep("sickl", clinicals$Condition), ]

## A case of Sickle cell in Kenya

## sample size
class(sickle$target_size)
sickle$target_size <- as.numeric(as.character(sickle$target_size))
# summary
summary(sickle$target_size)
# summary(sickle$target_size)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 8.0    80.0   224.0   239.8   344.0   600.0 

# Barplot
library(forcats)
sickle %>% 
  mutate(name = fct_reorder(trial_id, desc(target_size))) %>%
  ggplot(aes(x=trial_id,y= target_size)) +
  geom_bar(stat='identity')








Observational <- clinicals[grep("Observational", clinicals$study_type), ]












#BITERM MOdelling for the Scientific Title
data$text <- paste(data$primary_outcome, data$scientific_title, sep=" ")
row.names(data) <- NULL  


## Analyze all HIV clinical trials

hiv_data <- data[grep("hiv", data$condition), ]

# behavioral study types
hiv_behavioral <- hiv_data[grep("behavioral", hiv_data$intervention), ]

class(hiv_behavioral$target_size)

hiv_behavioral$target_size <- as.numeric(hiv_behavioral$target_size)

rownames(hiv_behavioral)<- NULL


# Visualize sponsorshttp://127.0.0.1:46929/graphics/plot_zoom_png?width=1536&height=912

#Data Visualization: Primary Sponsors of on going Phase 3 Clinical Trials
ggplot(data = hiv_behavioral,
       aes((y = reorder(primary_sponsor, -target_size)), x = target_size, fill = target_size))+
  geom_bar(stat = "identity", show.legend = FALSE)+
  labs (title = "Sponsors",
        subtitle = "Behavioural interventions",
        y = "Primary Sponsors",
        x = "Population Target size")+
  theme_few()

hiv_behavioral[10, ]




# Visualization on Target size
# visualize according to groups

hiv_behavioral$countriesgroup <- factor(hiv_behavioral$countries)

ggplot(hiv_behavioral, aes(x = trial_id, y = target_size, fill = countries)) + 
  
  geom_bar(stat = "identity") +
  ylim(-max(hiv_behavioral$target_size)*0.4, max(hiv_behavioral$target_size)) +
  #scale_fill_manual(values = c('kenya'='blue','tanzania'='red','uganda'='green'))+
  
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-2,4), "cm")     # This remove unnecessary margin around plot
  ) +
  #coord_polar(ylim = c(0, max(data$Value) * 0.5))+
  coord_polar(start = 0) +
  labs(title = "Circular Bar Graph of Target SIze")










data_2 <- hiv_behavioral[ , c('trial_id','date_registration','text')]


## Text Mining
# Create a corpus of the document

data_corp <- corpus(data_2, text_field = 'text') #initialize corpus for scientific title

summary(data_corp,3)
#create a new id for the variables
docid <- paste(data_corp$trial_id,
               data_corp$date_registration,
               data_corp$primary_outcome,
               sep= " ")
docnames(data_corp) <- docid
head(docvars(data_corp))


# create tokens
corp <- tokens(data_corp, what='word',
               remove_punct = TRUE, remove_numbers = TRUE,
               remove_symbols = TRUE, remove_separators = TRUE)
# stem
#corp <- tokens_wordstem(corp, language = 'en') # don't run this

# remove stopwords 
corp <- tokens_select(corp, stopwords(),selection = 'remove')
corp[[20]]
#construct a Document Feature Matrix
corp_dfm <- dfm(corp)



# Data visualization
# Visualize a wordcloud of the Primary outcome and Scientific titles
# Create a bigram using tokens_ngram
toks_ngram <- tokens_ngrams(corp, n = 2, concatenator = " ")
head(toks_ngram[[1]], 30)

# wordcloud plot
# load quanteda packages
library("quanteda.textplots")
library("quanteda.textstats")
bigram <- dfm(toks_ngram)
bigram_freq<-textstat_frequency(bigram)

textplot_wordcloud(bigram,max_words = 100,
                   ordered_color = TRUE)


# Observe the distribution of date of registration over the years
years_m %>%
  ggplot(aes(x=date_registration, color= factor(month)))+
  geom_bar(show.legend = FALSE)+
  labs(y="Date",
       x= "Article counts",
       title = "Scientific titles")

# Asses the frequency distribution of the bigrams, data grouped per year
word_freq <- textstat_frequency(bigram, groups = data$date_registration) # error: groups must have length ndoc(x)

# frequency plot of the top 10 most frequent word
features_corpdfm <- textstat_frequency(bigram, n=10)
features_corpdfm$feature <- with(features_corpdfm, reorder(feature, -frequency))
ggplot(features_corpdfm, aes(feature, frequency))+
  geom_col(fill="#0B1354")+
  labs (title = "Top 10 Frequent bigrams",
        subtitle = "2003-2023",
        y = "",
        x = "")+
  theme_few()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size=17, face="bold"))


# top frequent bigrams occurring each year
top_words <- word_freq %>%
  group_by (group) %>%
  top_n(15, frequency) %>% # Change the number to show more or fewer words
  ungroup ()

top_words %>%
  mutate(group = as.factor(group)) %>% 
  ggplot(aes(feature,frequency, fill=group)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~group, scales = "free_y") +
  coord_flip()+
  scale_y_continuous(expand = c(0,0)) +
  labs(x = "", y = "", title = "Conditions Spread Over 5 years") 


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
               k = 10,
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


# Visualizing topics based on the one interested
topicToViz <- 3
# select to 40 most probable terms from the topic by sorting the term-topic-probability vector in decreasing order
top40terms <- sort(tmResult$terms[topicToViz,], decreasing=TRUE)[1:40]
words <- names(top40terms)
# extract the probabilities of each of the 40 terms  
probabilities <- sort(tmResult$terms[topicToViz,], decreasing=TRUE)[1:40]
# visualize the terms as wordcloud
mycolors <- brewer.pal(8, "Dark2")
wordcloud(words, probabilities, random.order = FALSE, color = mycolors)





#BTM
# The Biterm Topic Model (BTM) is a word co-occurrence based topic 
# model that learns topics by modeling word-word co-occurrences patterns
# Tag parts of speech
library(udpipe)
library(data.table)
colnames(data_2)[2]  <- "doc_id"

anno    <- udpipe(data_2, "english", trace = 10)
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
model <- BTM(traindata, k = 10, 
             beta = 0.01, 
             iter = 500,
             biterms = biterms, 
             trace = 100)

# Plot Model Results (do not run when knitting)
library(ggraph)
library(textplot)
library(concaveman)
plot(model,
     top_n = 10,
     title = "BTM model",
     subtitle = "K = 10, 500 Training Iterations",
     labels = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9","10"))







## Making maps
library(tmap)
library(sf)
library(rnaturalearth)

world <- ne_countries(scale = "medium", returnclass = "sf")
target_countries <- c("Uganda", "Kenya", "Tanzania")
east_africa <- world[world$name %in% target_countries, ]

tm_shape(east_africa) +
  tm_borders() +
  tm_fill(col = "name") +
  tm_text("name", size = 0.8) +
  tm_layout(title = "Map of Uganda, Kenya, and Tanzania")

east_africa$name <- tolower(east_africa$name)


library(dplyr)

merged_data <- left_join(east_africa, hiv_behavioral, by = c("name" = "countries"))

tm_shape(merged_data) +
  tm_borders() +
  tm_fill(col = "target_size", style="quantile", palette="Blues") +
  tm_text("name", size = 0.8) +
  tm_layout(title = "Map of Uganda, Kenya, and Tanzania with Data")




## Time Series analysis/ Years
## I need to clean the sponsors column 
hiv_behavioral$primary_sponsor <- gsub("[.,;:-]","", hiv_behavioral$primary_sponsor)
hiv_behavioral$institution_type <- ifelse(grepl("university", hiv_behavioral$primary_sponsor), "University", "Research Institution")

class(hiv_behavioral$date_registration)

hiv_behavioral$constant_value <- 1

ggplot(hiv_behavioral, aes(x = date_registration, y = primary_sponsor, size= target_size, color= institution_type )) +
  geom_point(shape = 16 ) +
  labs(x = "Date", y = "Primary_Sponsor", title = "Behavioural Intervention Clinical Trials: 2003-2023")




ggplot(hiv_behavioral, aes(x = date_registration, y = primary_sponsor, color = institution_type)) +
  geom_point(shape = 16) +
  labs(x = "Date of Registration", y = "Primary Sponsor", 
       title = "Scatter Plot Distinguishing Universities from Research Institutions")

ggplot(hiv_behavioral, aes(x = institution_type)) + 
  geom_bar() +
  labs(x = "Institution Type", y = "Count", 
       title = "Distribution of Primary Sponsors by Institution Type")


# extracting trial ID code

# Function to extract the last part of the URL
extract_last_part <- function(url) {
  # Extract the part after the last slash
  result <- gsub("^.*/", "", url)
  return(result)
}

# Apply the function to each URL in the column
hiv_behavioral$extracted_part <- sapply(hiv_behavioral$web_address, extract_last_part)

## remove the space after each digit
hiv_behavioral$extracted_part <- trimws(hiv_behavioral$extracted_part)


print(hiv_behavioral$extracted_part)

trial_id <- hiv_behavioral$extracted_part
print(trial_id)


extracted_parts_string <- paste(sapply(hiv_behavioral$extracted_part, trimws), collapse = ",")
print(extracted_parts_string)



#########
# World map is available in the maps package
library(maps)

# No margin
par(mar=c(0,0,0,0))

# World map
map('world',
    col="#f2f2f2", fill=TRUE, bg="white", lwd=0.05,
    mar=rep(0,4),border=0, ylim=c(-80,80) 
)


library(ggmap)

register_google(key = "AIzaSyADngLnfcUhOTVqk1euPiz530YAjDlVKiI")

hiv_behavioral <- hiv_behavioral %>%
  mutate(GeocodeResult = geocode(primary_sponsor))

hiv_behavioral$Latitude <- hiv_behavioral$GeocodeResult$lat
hiv_behavioral$Longitude <- hiv_behavioral$GeocodeResult$lon

names(hiv_behavioral)

map('world',
    col="#f2f2f2", fill=TRUE, bg="white", lwd=0.05,
    mar=rep(0,4),border=0, ylim=c(-80,80) 
)
points(x=hiv_behavioral$Longitude, y=hiv_behavioral$Latitude, col="slateblue", cex=1, pch=20)










































## Text Mining
# Create a corpus of the document

sickle_corp <- corpus(sickle, text_field = 'countries') #initialize corpus for countries

summary(sickle_corp,3)
#create a new id for the variables
docid <- paste(sickle_corp$trial_id,
               sickle_corp$target_size,
               sickle_corp$study_design,
               sep= " ")
docnames(sickle_corp) <- docid
head(docvars(sickle_corp))


# create tokens
scorp <- tokens(sickle_corp, what='word',
                remove_punct = TRUE, remove_numbers = TRUE,
                remove_symbols = TRUE, remove_separators = TRUE)
# stem
scorp <- tokens_wordstem(scorp, language = 'en')

# remove stopwords 
scorp <- tokens_select(scorp, stopwords(),selection = 'remove')
scorp[[24]]
#construct a Document Feature Matrix
scorp_dfm <- dfm(scorp)



# Data visualization
# Visualize a wordcloud of the Conditions and Scientific titles
# Create a bigram using tokens_ngram
stoks_ngram <- tokens_ngrams(scorp, n = 2, concatenator = " ")
head(stoks_ngram[[1]], 30)

# wordcloud plot
# load quanteda packages
library("quanteda.textplots")
library("quanteda.textstats")
sbigram <- dfm(stoks_ngram)
sbigram_freq<-textstat_frequency(sbigram)

textplot_wordcloud(sbigram,max_words = 100,
                   ordered_color = TRUE)































# Creating Word Clouds
library("quanteda.textplots")
col <- sapply(seq(0.1, 1, 0.1), function(x) adjustcolor("#0B2545", x))
par(bg = "#98BAE3") # set background color
textplot_wordcloud(corp_dfm, font="Helvetica 65 Medium", rotation = 0, color = col)

# How the distribution of Condition happened across the 5 years
# 5 years data
word_freq <- textstat_frequency(corp_dfm, groups = data$years_a)

# Plot the frequency text plot
library(quanteda.textstats)
# frequency plot of the top 10 most frequent word
features_corpdfm <- textstat_frequency(corp_dfm, n=10)
features_corpdfm$feature <- with(features_corpdfm, reorder(feature, -frequency))

ggplot(features_corpdfm, aes(feature, frequency))+
  geom_point(size=5, color="#0B1354")+
  labs (title = "Top 10 Word Frequency Plot",
        subtitle = "Conditions 2018-2023",
        y = "",
        x = "")+
  theme_few()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size=17, face="bold"))

# frequency plot of most occurring words each year
top_words <- word_freq %>%
  group_by (group) %>%
  top_n(5, frequency)  # Change the number to show more or fewer words

# Create scatter plot
par(bg = "#BACAD5")
ggplot(data = top_words, aes(x = feature, y = frequency)) +
  geom_point(size = 3, fill="#BACAD5") +
  labs(x = "", y = "", title = "Conditions Spread Over 5 years") +
  facet_wrap(~ group, ncol = 3, scales = 'free')+
  theme_few()+
  theme(plot.background = element_rect(fill = "#BACAD5"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size=15, face="bold"))# Creates a grid of plots, 2 columns






## Group data ocurring from different Trials Registry based on Trial ID
pattern <- "PACTR"
PACTR <- data[str_detect(data$trial_id, pattern), ]

























