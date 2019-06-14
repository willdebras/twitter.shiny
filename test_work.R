
library(stringr)


test <- "word, other words, singular, more things"


test2 <- strsplit(test, ",") 

test3 <- as.vector(test2[[1]]) %>% trimws(which = "both")

test4 <- sapply(test3, function(x) ifelse(str_detect(x, "[:blank:]"), paste("\\\"", x, "\\\"", sep = ""), paste(x)))


paste("(", paste(test4, sep = " ", collapse = " OR "), ")", sep = "")


urlterms <- "here we really fucking go, here another set"

terms <- strsplit(urlterms, ",") 

terms2 <- as.vector(terms[[1]]) %>% trimws(which = "both") %>%
  strsplit(" ")

terms3 <- sapply(terms2, function(x) paste("url_contains:", x, "OR", collapse = " "))

terms4 <- sapply(terms3, function(x) str_sub(x, 1, str_length(x)-3))

terms5 <- sapply(terms4, function(x) paste("(", x, ")", sep = ""))


url1 <- "apnorc.org/blahblah, apnews.com/blahblah"

url2 <- strsplit(url1, ",")

url3 <- as.vector(url2[[1]]) %>% trimws(which = "both")

url4 <- test4 <- sapply(url3, function(x) paste("url_contains:", "\\\"", x, "\\\"", sep = ""))

url5 <- paste(url4, sep = " ", collapse = " OR ")

              