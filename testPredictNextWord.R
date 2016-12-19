predictNextWord <- function(inputQuery) {
  require(tau)
  setwd("/Users/yanze/github/capstone project/final/");
  # change the query input vector
  cleanQuery <- unlist(strsplit(inputQuery, split=" "));
  print(cleanQuery);
  queryLen <- length(cleanQuery);
  nextNGramFlag <- FALSE;
  supportMessage <- "";
  returnNextWord <- "";
  print(queryLen)
  if (queryLen >=3 & nextNGramFlag == FALSE) {
    load("fourGramData.RData");
    # already sorted when load
    queryString <- paste(cleanQuery[(queryLen-2):queryLen], collapse=" ");
    print(queryString)
    print(paste("^", queryString, sep = ""));
    #print(grep(paste("^", queryString, sep = ""), fourGram$words))
    nextWords <- fourGram[grep(paste("^", queryString, sep = ""), fourGram$words),];
    print(nextWords)
    if (length(nextWords[,1]) >= 1) {
      returnNextWord <- nextWords$words[1];
      returnNextWord <- tail(unlist(strsplit(returnNextWord, split=" ")), n = 1);
      nextNGramFlag <- TRUE;
      supportMessage <- "Predicted by four Gram.";
    } 
  } else if (queryLen >=2 & nextNGramFlag == FALSE) {
    load("threeGramData.RData");
    # already sorted when load
    queryString <- paste(cleanQuery[(queryLen-1):queryLen], collapse=" ");
    print(queryString)
    nextWords <- threeGram[grep(paste("^", queryString, sep = ""), threeGram$words),];
    if (length(nextWords[,1]) >= 1) {
      returnNextWord <- nextWords$words[1];
      returnNextWord <- tail(unlist(strsplit(returnNextWord, split=" ")), n = 1);
      nextNGramFlag <- TRUE;
      supportMessage <- "Predicted by three Gram.";
    } 
  } else if (queryLen >=1 & nextNGramFlag == FALSE) {
    load("twoGramData.RData");
    queryString <- cleanQuery[queryLen];
    print(queryString)
    # already sorted when load
    nextWords <- twoGram[grep(paste("^", queryString, sep = ""), twoGram$words),];
    if (length(nextWords[,1]) >= 1) {
      returnNextWord <- nextWords$words[1];
      returnNextWord <- tail(unlist(strsplit(returnNextWord, split=" ")), n = 1);
      nextNGramFlag <- TRUE;
      supportMessage <- "Predicted by two Gram.";
    } 
  } else {
    load("oneGramData.RData");
    returnNextWord <- oneGram$words[1];
    print(oneGram$words[1]);
    supportMessage <- "We cannot predict your words, just return the most common word from our database.";
  }
  
  returnDF <- data.frame(nextWordPredicted = returnNextWord, supportMessage = supportMessage);
  return(returnDF) ;
}