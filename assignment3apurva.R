install.packages("quanteda")
install.packages("devtools")
install.packages("readtext")
install.packages("spacyr")
install.packages("newsmap")
devtools::install_github("quanteda/quanteda.corpora")
library(quanteda)
library(readtext)
library(readtext)
library("jsonlite")
library(ggplot2)



URL = "http://www.cs.cornell.edu/people/pabo/movie-review-data/rt-polaritydata.tar.gz"
download.file(URL, "rt-polaritydata.tar.gz")
untar("rt-polaritydata.tar.gz")
head("rt-polaritydata.tar.gz")
Data <- data.frame(sentence = readLines("./rt-polaritydata/rt-polarity.tar.gz"), 
                   stringsAsFactors = FALSE)
df_neg <- data.frame(sentence = readLines("./rt-polaritydata/rt-polarity.neg"), 
                     stringsAsFactors = FALSE)
df_neg['sentiment'] <- "neg"
View(df_neg)


df_pos <- data.frame(sentence = readLines("./rt-polaritydata/rt-polarity.pos"), 
                     stringsAsFactors = FALSE)
df_pos['sentiment'] <- "pos"
View(df_pos)
CorpusMovies <- corpus(rbind(df_neg, df_pos), text_field = 'sentence')
summary(CorpusMovies)
View(CorpusMovies)
CorpusMovies <- tokens(CorpusMovies, remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE, remove_separators = TRUE)
CorpusMovies <- tokens_select(CorpusMovies, pattern = stopwords('en'), selection = 'remove')
View(CorpusMovies)
dfmat_CorpusMovies <- dfm(CorpusMovies)

ggplot(textstat_frequency(dfmat_CorpusMovies, n = 15),
       aes(x = reorder(feature, frequency), y = frequency)) +
  geom_point() +
  coord_flip() +
  labs(x = NULL, y = "Frequency") +
  theme_minimal()



devtools::install_github("lepennec/ggwordcloud") 
install.packages("lepennec/ggwordcloud")
install.packages("ggwordcloud")
install.packages("tm")
install.packages("SnowballC")
install.packages("dplyr")
library(ggwordcloud)
library(tm)
library(SnowballC)
library(ggplot2)
library(dplyr)
set.seed(1234)
textplot_wordcloud(dfmat_CorpusMovies,min_size = 1,
                   max_size = 4, color = brewer.pal(8, "Set1"),
                   font = "sans",max_words = 50)

#wordcloud(words = d$word, freq = d$freq, min.freq = 1,
    #      max.words=50, random.order=FALSE, rot.per=0.35, 
   #       colors = brewer.pal(8, "Dark2") , vfont=c("sans serif","plain"))

names(docvars(CorpusMovies))
docvars(CorpusMovies, "sentiment") <- 
  factor(ifelse(docvars(CorpusMovies, "sentiment") == "neg",
                "neg",
                "pos"))

dfmat_corp_senti <- dfm(CorpusMovies, 
                           groups = "sentiment")
textplot_wordcloud(dfmat_corp_senti,min_size = 1,
                   max_size = 6, comparison = TRUE, max_words = 100,
                   color = c('red', 'green'))

# Lexical Diversity of Random 20 Sentances
lex_div <- tokens(CorpusMovies)
lex_inaug <- dfm(lex_div, remove = stopwords('en'))
tstat <- textstat_lexdiv(lex_inaug) 
rand_20 <- tstat[sample(nrow(tstat), 20, replace = FALSE, prob = NULL),]
plot(rand_20$TTR, type = 'l', xaxt = 'n', xlab = NULL, ylab = "TTR")
grid()
axis(1, at = seq_len(nrow(rand_20)), labels = lex_inaug$Reviews)

# Plot Dendogram of 20 random sentences
install.packages("dendextend")
library(dendextend)
random <- dfm_sample(dfmat_CorpusMovies,20, replace = FALSE)
View(random)
tstat_dist <- as.dist(textstat_dist(random))
clust <- hclust(tstat_dist)
View(clust)
avg_dend_obj <- as.dendrogram(clust)x
avg_col_dend <- color_branches(avg_dend_obj, h = 8)
plot(avg_col_dend)








devtools::install_github("quanteda/quanteda.corpora")
install.packages("quanteda.corpora")
install.packages("quanteda.textmodels")
install.packages("caret")
library(quanteda.corpora)
library(caret)
library(quanteda)
library(caret)
require(quanteda.textmodels)
corp_movies <- CorpusMovies
summary(corp_movies, 5)
set.seed(300)
id_train <- sample(1:10662, size = round(0.7*10662) , replace = FALSE)
head(id_train, 10)
corp_movies$id_numeric <- 1:ndoc(corp_movies)
dfmat_training <- corpus_subset(corp_movies, id_numeric %in% id_train) %>%
  dfm(stem = TRUE)
dfmat_test <- corpus_subset(corp_movies, !id_numeric %in% id_train) %>%
  dfm(stem = TRUE)
tmod_nb <- textmodel_nb(dfmat_training, dfmat_training$sentiment)
summary(tmod_nb)
dfmat_matched <- dfm_match(dfmat_test, features = featnames(dfmat_training))
actual_class <- dfmat_matched$sentiment
predicted_class <- predict(tmod_nb, newdata = dfmat_matched)
tab_class <- table(actual_class, predicted_class)
tab_class
install.packages('e1071', dependencies=TRUE)
confusionMatrix(tab_class, mode = "everything")





install.packages("caret")
library(caret)
head(CorpusMovies,2)
lengths(data_dictionary_LSD2015)
CorpusMovies_lsd <- tokens_lookup(CorpusMovies,dictionary = data_dictionary_LSD2015[1:2])
dfm_CorpusMovies_lsd <- dfm(CorpusMovies_lsd)
dfm_CorpusMovies_set <- as.data.frame(dfm_CorpusMovies_lsd)
dfm_CorpusMovies_set$sentiment <- ifelse(dfm_CorpusMovies_set$negative >= dfm_CorpusMovies_set$positive,"neg","pos")
head(dfm_CorpusMovies_set)
head(dfm_CorpusMovies_lsd)
tab_class <- table(dfm_CorpusMovies_set$sentiment,dfmat_CorpusMovies@docvars$sentiment)
View(tab_class)
tab_class
confusionMatrix(tab_class, mode = "everything")
