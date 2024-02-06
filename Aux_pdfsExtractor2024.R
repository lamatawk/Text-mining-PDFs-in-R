# Load necessary packages and install if necessary
required_packages <- c("pdftools", "tm", "dplyr", "tibble", "openxlsx", "tidytext", "ggplot2", 
                       "topicmodels", "reshape2", "tidyr", "tidyverse", "igraph", "ggraph", 
                       "RColorBrewer", "readxl", "scales", "crayon", "viridis")

install_and_load_packages <- function(packages) {
  for (package in packages) {
    if (!require(package, character.only = TRUE)) {
      install.packages(package, repos = "http://cran.us.r-project.org")
      library(package, character.only = TRUE)
    }
  }
}

install_and_load_packages(required_packages)

# Function to read a PDF, extract words, remove stopwords, and find frequency
freqFullWords <- function(pdffile = NULL, language = NULL) {
  suppressMessages(data.pdf <- pdftools::pdf_text(pdffile))
  dt.lower <- tolower(data.pdf)
  dt.sym <- gsub("\r?\n|\r", " ", dt.lower)
  dt.punc <- gsub("[[:punct:]]* *(\\w+[&'-]\\w+)|[[:punct:]]+ *| {2,}", " \\1", dt.sym)
  dt.split <- strsplit(dt.punc, " ")
  dt.vec <- unlist(dt.split)
  dt.num <- gsub("\\b\\d+\\b", "", dt.vec)
  dt.rom <- dt.num[-grep(paste0("\\b(M{1,4}(CM|CD|D?C{0,3})(XC|XL|L?X{0,3})(IX|IV|V?I{0,3})",
                                "|M{0,4}(CM|C?D|D?C{1,3})(XC|XL|L?X{0,3})(IX|IV|V?I{0,3})",
                                "|M{0,4}(CM|CD|D?C{0,3})(XC|X?L|L?X{1,3})(IX|IV|V?I{0,3})",
                                "|M{0,4}(CM|CD|D?C{0,3})(XC|XL|L?X{0,3})(IX|I?V|V?I{1,3}))\\b"),
                        dt.num, ignore.case = TRUE)]
  dt.ltr <- dt.rom[-grep("\\b([a-z])\\b", dt.rom, ignore.case = TRUE)]
  dt.ltr.temp <- dt.ltr[-grep('[^[:alnum:]]', dt.ltr)]
  dt.stop <- removeWords(dt.ltr.temp, stopwords(language))
  dt.nohy <- dt.stop[-grep("-", dt.stop)]
  dt.noam <- dt.nohy[-grep("&", dt.nohy)]
  dt.clean <- dt.noam[-which(dt.noam == "")]
  freq.all <- table(dt.clean)
  freq.sorted <- sort(freq.all, decreasing = TRUE)
  outFull <- data.frame(Word = names(freq.sorted), Freq = freq.sorted, stringsAsFactors = FALSE)
  return(outFull)
}

# Function to read a PDF, remove stopwords, extract specified keywords, and find frequency
freqKeywords <- function(pdffile = NULL, csvfile = NULL, language = NULL) {
  suppressMessages(data.pdf <- pdftools::pdf_text(pdffile))
  dt.lower <- tolower(data.pdf)
  dt.sym <- gsub("\r?\n|\r", " ", dt.lower)
  dt.punc <- gsub("[[:punct:]]* *(\\w+[&'-]\\w+)|[[:punct:]]+ *| {2,}", " \\1", dt.sym)
  dt.split <- strsplit(dt.punc, " ")
  dt.vec <- unlist(dt.split)
  dt.num <- gsub("\\b\\d+\\b", "", dt.vec)
  dt.rom <- dt.num[-grep(paste0("\\b(M{1,4}(CM|CD|D?C{0,3})(XC|XL|L?X{0,3})(IX|IV|V?I{0,3})",
                                "|M{0,4}(CM|C?D|D?C{1,3})(XC|XL|L?X{0,3})(IX|IV|V?I{0,3})",
                                "|M{0,4}(CM|CD|D?C{0,3})(XC|X?L|L?X{1,3})(IX|IV|V?I{0,3})",
                                "|M{0,4}(CM|CD|D?C{0,3})(XC|XL|L?X{0,3})(IX|I?V|V?I{1,3}))\\b"),
                        dt.num, ignore.case = TRUE)]
  dt.ltr <- dt.rom[-grep("\\b([a-z])\\b", dt.rom, ignore.case = TRUE)]
  dt.ltr.temp <- dt.ltr[-grep('[^[:alnum:]]', dt.ltr)]
  dt.stop <- removeWords(dt.ltr.temp, stopwords(language))
  dt.nohy <- dt.stop[-grep("-", dt.stop)]
  dt.noam <- dt.nohy[-grep("&", dt.nohy)]
  dt.clean <- dt.stop[-which(dt.stop == "")]
  keywords <- tolower(read.csv(csvfile)[[1]])
  check.match <- dt.clean %in% keywords
  dt.clean <- dt.clean[which(check.match)]
  freq.all <- table(dt.clean)
  new.freq.all <- data.frame(Word = names(freq.all), Freq = as.vector(freq.all), stringsAsFactors = FALSE)
  return(new.freq.all)
}

# Function to read a PDF, extract all words, remove symbols/numbers, and return remaining ones
fullCleanWords_stopWRemoved <- function(pdffile = NULL, language = NULL, dataExc = NULL) {
  suppressMessages(data.pdf <- pdftools::pdf_text(pdffile))
  dt.lower <- tolower(data.pdf)
  dt.sym <- gsub("\r?\n|\r", " ", dt.lower)
  dt.punc <- gsub("[[:punct:]]* *(\\w+[&'-]\\w+)|[[:punct:]]+ *| {2,}", " \\1", dt.sym)
  dt.split <- strsplit(dt.punc, " ")
  dt.vec <- unlist(dt.split)
  dt.num <- gsub("\\b\\d+\\b", "", dt.vec)
  dt.rom <- dt.num[-grep(paste0("\\b(M{1,4}(CM|CD|D?C{0,3})(XC|XL|L?X{0,3})(IX|IV|V?I{0,3})",
                                "|M{0,4}(CM|C?D|D?C{1,3})(XC|XL|L?X{0,3})(IX|IV|V?
