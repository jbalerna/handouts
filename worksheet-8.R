 ## RegEx

library(stringr)

str_extract_all(
  'Email info@sesync.org or tweet @SESYNC for details!',
  '\\b\\S+@\\S+\\b')

library(tm)

enron <- VCorpus(DirSource("~/data/enron"))
email <- enron[[1]]
content(email)

match <- str_match(content(email), '^From: (.*)')
match


## Data Extraction

enron <- tm_map(enron, function(email) {
  body <- content(email)
  match <- str_match(body, '^From: (.*)')
  match <- na.omit(match)
  meta(email, 'author') <- match[[1, 2]]
  return(email)
})

email <- enron[[1]]
meta(email)


## Relational Data Extraction

get_to <- function(email) {
  body <- content(email)
  match <- str_detect(body, '^To:')
  if (any(match)) {
    to_start <- which(match)[[1]]
    match <- str_detect(body, '^Subject:')
    to_end <- which(match)[[1]] - 1
    to <- paste(body[to_start:to_end], collapse = '')
    to <- str_extract_all(to, '\\b\\S+@\\S+\\b')
    return(unlist(to))
  } else {
    return(NA)
  }
}

get_to(email) #double check expression

edges <- lapply(enron, FUN = function(email) {
  from <- meta(email, 'author')
  to <- get_to(email)
  return(cbind(from, to))
})

#looping thru e-mails in enron corpus and extracts from & to lines

edges <- do.call(rbind, edges)
edges <- na.omit(edges)
attr(edges, 'na.action') <- NULL

dim(edges)

library(network)

g <- network(edges)
plot(g) #this is a crazy plot

## Cleaning Text

enron <- tm_map(enron, function(email) {
  body <- content(email)
  match <- str_detect(body, '^X-FileName:')
  begin <- which(match)[[1]] + 1
  match <- str_detect(body, '^[>\\s]*[_\\-]{2}')
  match <- c(match, TRUE)
  end <- which(match)[[1]] - 1
  content(email) <- body[begin:end]
  return(email)
})

email <- enron[[2]]
content(email)

library(magrittr)

#using pre-defined functions for cleaning
enron_words <- enron %>%
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>%
  tm_map(stripWhitespace)

email <- enron_words[[2]]
content(email)


#creating a new function for cleaning links
remove_links <- function(body) {
  match <- str_detect(body, '(http|www|mailto)')
  body[!match]
}


enron_words <- enron_words %>%
  tm_map(content_transformer(remove_links))


## Stopwords and Stems - removes an, the, etc. 
#as well as "ing" at the end of words

enron_words <- enron_words %>%
  tm_map(stemDocument) %>%
  tm_map(removeWords, stopwords("english"))

## Bag-of-Words

dtm <- DocumentTermMatrix(enron_words)

## Long Form

library(tidytext)
library(dplyr)

dtt <- tidy(dtm)
words <- dtt %>%
  group_by(term) %>%
  summarise(
    n = n(),
    total = sum(count)) %>%
  mutate(nchar = nchar(term))

library(ggplot2)

#plot of frequency of nchar in each word in emails
#why would you want to know this lmao?!
ggplot(words, aes(x = nchar)) +
  geom_histogram(binwidth = 1)

#looking at characters can show whether words got stuck together
#this filters out those words
dtt_trimmed <- words %>%
  filter(
    nchar < 16,
    n > 1,
    total > 3) %>%
  select(term) %>%
  inner_join(dtt)

dtm_trimmed <- dtt_trimmed %>%
  cast_dtm(document, term, count)

## Term Correlations

word_assoc <- findAssocs(dtm_trimmed, "ken", 0.6)

word_assoc <- data.frame(
  word = names(word_assoc[[1]]),
  assoc = word_assoc,
  row.names = NULL)

library(ggwordcloud)

ggplot(data = word_assoc,
       aes(label = word, size = ken)) +
  geom_text_wordcloud_area()

## Latent Dirichlet allocation

library(topicmodels)

#creates five topic areas with all the terms that fall into them
seed = 12345
fit = LDA(dtm_trimmed, k = 5, control = list(seed=seed))

email_topics <- as.data.frame(
  posterior(fit, dtm_trimmed)$topics)

head(email_topics)

library(ggwordcloud)

topics <- tidy(fit) %>%
  filter(beta > 0.004)

ggplot(data = topics,
  aes(size = beta, label = term)) +
  geom_text_wordcloud_area(rm_outside = TRUE) +
  facet_wrap(vars(topic))
