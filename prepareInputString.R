prepareInputString <- function(inputString) {
  # clean the white spaces, punctuations, and numbers, the same as clean the n gram data
  # First remove the non-alphabatical characters
  inputStr <- iconv(inputString, "latin1", "ASCII", sub=" ");
  inputStr <- trimws(inputStr, which = c("both", "left", "right"))
  
  # Convert the input string to a Corpus and do the same clean as n gram
  inStrCorpus <- VCorpus(VectorSource(inputStr))
  toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x));
  inStrCorpus <- tm_map(inStrCorpus, toSpace,"\"|/|@|#|\\|");
  inStrCorpus <- tm_map(inStrCorpus, content_transformer(tolower));
  inStrCorpus <- tm_map(inStrCorpus, removeNumbers);
  inStrCorpus <- tm_map(inStrCorpus,removePunctuation);
  inStrCorpus <- tm_map(inStrCorpus, stripWhitespace);
  print(inStrCorpus)
  cleanInputString <- as.character(inStrCorpus[[1]])
  print(cleanInputString)
  
  
  # Return the cleaned input sentense
  if (nchar(cleanInputString) > 0) {
    return(cleanInputString); 
  } else {
    return("");
  }
  
  return(cleanInputString);
}