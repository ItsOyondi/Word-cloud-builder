word_freq <- function(textfile, stp_val, excludes=NULL){
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
  
  #check if user has selected include stopwords or not
  if(stp_val == FALSE){ 
    # Remove english common stopwords
    docs <- tm_map(docs, removeWords, stopwords("english"))
  }
  #remove user specified words
  if(!is.null(excludes)){
    docs <- tm_map(docs, removeWords, c(excludes))
  }
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

get_word_in_cloud <- function(cloud_file){
  #getting the words using Tesseract OCR engine
  eng <- tesseract("eng") #get engine
  
  extracted_txt <- tesseract::ocr_data(cloud_file, engine = eng)
  extracted_txt
  
}
