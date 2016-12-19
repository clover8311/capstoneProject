loadDataAndClean <- function(samplePercentage) {
  
  require(stringi)
  require(tm)
  require(ngram)
  require(quanteda)
  #load data
  setwd("/Users/yanze/github/capstone project/final/en_US/");
  blogs_file <- "en_US.blogs.txt";
  blogs_con <- file(blogs_file, "r");
  blogs <- readLines(blogs_con, encoding = "UTF-8");
  
  news_file <- "en_US.news.txt";
  news_con <- file(news_file, "r");
  news <- readLines(news_con, encoding = "UTF-8");
  
  twitter_file <- "en_US.twitter.txt";
  twitter_con <- file(twitter_file, "r");
  twitter <- readLines(twitter_con, encoding = "UTF-8");
  
  close(blogs_con)
  close(news_con)
  close(twitter_con)
  
  set.seed(1234);
  blogs_sample<- sample(blogs, length(blogs)*samplePercentage, replace=F);
  twitter_sample<- sample(twitter, length(twitter)*samplePercentage, replace=F);
  news_sample<- sample(news, length(news)*samplePercentage, replace=F);
  data_sample<- c(blogs_sample, twitter_sample, news_sample);
  
  # clean data, remove stemming, stop words, number, white spaces.
  writeLines(data_sample, "sampleData.txt");
  rm(blogs_sample, twitter_sample, news_sample, data_sample);
  
  words <- readLines('sampleData.txt', encoding = 'UTF-8');
  myCorpus <- Corpus(VectorSource(words));
  toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x));
  myCorpus <- tm_map(myCorpus, toSpace,"\"|/|@|#|\\|");
  myCorpus <- tm_map(myCorpus, content_transformer(tolower));
  myCorpus <- tm_map(myCorpus, removeNumbers);
  myCorpus <- tm_map(myCorpus,removePunctuation);
  myCorpus <- tm_map(myCorpus, stripWhitespace);
  data_corpus<-corpus(myCorpus);
  return(data_corpus);
}

getOneGrams <- function(data_corpus) {
  tkn_dfm_1<- dfm(data_corpus, ngrams=1, verbose= T, concatenator=" ", stopwords=T);
  one_gram<-as.data.frame(as.matrix(docfreq(tkn_dfm_1)));
  colnames(one_gram) <- 'frequency'
  one_gram$words<-rownames(one_gram)
  # only get the word which shows more thatn 200 times
  one_gram <- one_gram[one_gram$frequency > 200,]
  one_gram <- one_gram[with(one_gram, order(-frequency)), ]
  #one_gram_list <- split(one_gram, list(one_gram$words, one_gram$frequency))
  return(one_gram)
}

getTwoGrams <- function(data_corpus) {
  tkn_dfm_2<- dfm(data_corpus, ngrams=2, verbose= T, concatenator=" ", stopwords=T);
  two_gram<-as.data.frame(as.matrix(docfreq(tkn_dfm_2)))
  colnames(two_gram) <- 'frequency'
  two_gram$words<-rownames(two_gram)
  two_gram <- two_gram[with(two_gram, order(-frequency)), ]
  #two_gram_list <- split(two_gram, list(two_gram$words, two_gram$frequency))
  return(two_gram)
}

getThreeGrams <- function(data_corpus) {
  tkn_dfm_3<- dfm(data_corpus, ngrams=3, verbose= T, concatenator=" ", stopwords=T);
  three_gram<-as.data.frame(as.matrix(docfreq(tkn_dfm_3)))
  colnames(three_gram) <- 'frequency'
  three_gram$words<-rownames(three_gram)
  #three_gram_list <- split(three_gram, list(three_gram$words, three_gram$frequency))
  three_gram <- three_gram[with(three_gram, order(-frequency)), ]
  return(three_gram)
}

getFourGrams <- function(data_corpus) {
  tkn_dfm_4<- dfm(data_corpus, ngrams=4, verbose= T, concatenator=" ", stopwords=T);
  four_gram<-as.data.frame(as.matrix(docfreq(tkn_dfm_4)))
  colnames(four_gram) <- 'frequency'
  four_gram$words<-rownames(four_gram)
  four_gram <- four_gram[with(four_gram, order(-frequency)), ]
  return(four_gram)
}

saveNGramsData <- function(nGramDataPath, nGramDataFrame) {
  setwd("/Users/yanze/github/capstone project/final/");
  oneGramDataPath <- "oneGramData.RData";
  twoGramDataPath <- "twoGramData.RData";
  threeGramDataPath <- "threeGramData.RData";
  fourGramDataPath <- "fourGramData.RData";
  print(oneGramDataPath)
  save(nGramDataFrame, file = nGramDataPath);
  
}

getNGramsData <- function(samplePercentage) {
  # get the data load and clean
  dat <- loadDataAndClean(samplePercentage);
  # get one, two, three and four grams
  setwd("/Users/yanze/github/capstone project/final/");
  oneGramDataPath <- "oneGramData.RData";
  oneGram <- getOneGrams(dat);
  save(oneGram, file = oneGramDataPath);
  rm(oneGram);
  twoGramDataPath <- "twoGramData.RData";
  twoGram <- getTwoGrams(dat);
  save(twoGram, file = twoGramDataPath);
  rm(twoGram);
  threeGramDataPath <- "threeGramData.RData";
  threeGram <- getThreeGrams(dat);
  save(threeGram, file = threeGramDataPath);
  rm(threeGram);
  fourGramDataPath <- "fourGramData.RData";
  fourGram <- getFourGrams(dat);
  save(fourGram, file = fourGramDataPath);
  rm(fourGram);
}