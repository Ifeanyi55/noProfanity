profanities <- readLines("profanities.txt")

# no profanity
noProfanity <- function(text, block) {
  text_split <- c(unlist(strsplit(text, split = " ")))
  
  post_n <- c()
  for (i in text_split) {
    post_n <- c(
      post_n,
      if (i %in% c(
        profanities
      )) {
        tolower(i)
      } else {
        i
      }
    )
  }
  
  clean_text <- gsub("\\b(fuck|shit|bullshit|damn|crap|fucking|bs)\\b", block, post_n)
  
  clean_text <- gsub("\n", "", clean_text)
  
  return(paste(clean_text, collapse = " "))
}
