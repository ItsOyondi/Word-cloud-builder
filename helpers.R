word_freq <- function(textfile){
  text <- readLines(textfile) #text is a vector with one element for each line of the input file.
  text <- str_c(text, collapse = " ") #text is now a single element
  docs <- VCorpus(VectorSource(text)) #tm library
  #VCorpus instead of Corpus removes warnings
  # inspect(docs) to look at text
  toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x)) #makes function toSpace()
  docs <- tm_map(docs, toSpace, "/")
  docs <- tm_map(docs, toSpace, "@")
  docs <- tm_map(docs, toSpace, "\\|")
  # Convert the text to lower case
  docs <- tm_map(docs, content_transformer(tolower))
  
  # Remove english common stopwords
  docs <- tm_map(docs, removeWords, stopwords("english"))
  # Remove punctuations
  docs <- tm_map(docs, removePunctuation)
  # Eliminate extra white spaces
  docs <- tm_map(docs, stripWhitespace)
  
  dtm <- TermDocumentMatrix(docs)
  m <- as.matrix(dtm)
  v <- sort(rowSums(m),decreasing=TRUE)
  df <- data.frame(word = names(v),freq=v) #create a data frame from the uploaded file.
  as_tibble(df)
}