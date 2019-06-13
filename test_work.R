
library(stringr)


test <- "word, other words, singular, more things"


test2 <- strsplit(test, ",") 

test3 <- as.vector(test2[[1]]) %>% trimws(which = "both")

test4 <- sapply(test3, function(x) ifelse(str_detect(x, "[:blank:]"), paste("\\\"", x, "\\\"", sep = ""), paste(x)))


paste(test2, sep = "", collapse)
