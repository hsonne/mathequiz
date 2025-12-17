raw_text <- readLines("~/../Desktop/verben_praeteritum.Rmd", warn = FALSE)

headers <- grep("#", text)
text <- raw_text[(headers[1] + 2):(headers[2] - 2)]
text <- trimws(gsub("^.|.$", "", text[-2]))
verbforms <- read.table(text = text, sep = "|", header = TRUE, check.names = FALSE)
verbforms[] <- lapply(verbforms, trimws)
str(verbforms)

names(verbforms)
n_rows <- nrow(verbforms)
n_forms <- ncol(verbforms) - 1L
n <- n_rows * n_forms

index_matrix <- cbind(
  i = sample(seq_len(n_rows), n, replace = TRUE),
  j = sample(seq_len(n_forms), n, replace = TRUE)
)

first_upper <- function(x) {
  paste0(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x)))
}

quiz <- do.call(rbind, lapply(seq_len(nrow(index_matrix)), function(row) {
  #row <- 1
  i <- index_matrix[row, 1]
  form <- index_matrix[row, 2]
  verbform <- verbforms[i, form + 1]
  
  question <- sprintf(
    "%d. Person %s Präteritum von '%s'",
    ((form - 1) %% 3L) + 1L,
    ifelse(form > 3, "Plural", "Singular"),
    verbforms$Infinitiv[i]
  )
  
  solution <- sprintf(
    "%s %s.", 
    first_upper(names(verbforms)[form + 1]), 
    verbform
  )

  data.frame(n = 1, thema = "Präteritum", frage = question, antwort = solution)
}))

#readLines("data/jeder-gegen-jeden-fragen.txt", 2)
write.table(quiz, "data/questions_praeteritum.txt", sep = ";", row.names = FALSE, quote = FALSE)
readLines("data/questions_praeteritum.txt", 2)
