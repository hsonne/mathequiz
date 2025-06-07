file <- "data/jeder-gegen-jeden-fragen.txt"

handle_placeholders <- function(data) {
  has_option_placeholder <- function(x) {
    grepl("\\[\\[", x)
  }
  select_option <- function(x) {
    sapply(strsplit(x, "\\|"), function(options) {
      sample(options, 1L)    
    })
  }
  resolve_option_placeholders <- function(x) {
    token_list <- strsplit(x, "(\\[|\\]){2}")
    token_list <- lapply(token_list, function(tokens) {
      is_option <- grepl("\\|", tokens)
      tokens[is_option] <- select_option(tokens[is_option])
      tokens
    })
    sapply(token_list, paste, collapse = "")
  }
  questions <- data$frage
  has_options <- has_option_placeholder(questions)
  questions[has_options] <- resolve_option_placeholders(questions[has_options])
  data$frage <- questions
  data
}

randomly_sorted <- function(data) {
  data[sample(nrow(data)), ]
}

add_empty <- function(x) c(x, "")

create_slides_markdown <- function(topic, question, answer) {
  rep_str <- function(x, n) paste(rep(x, n), collapse = "")
  h <- function(n, x) add_empty(paste(rep_str("#", n), x))
  p <- add_empty
  text_lines <- c(
    #h(3, "Thema"), 
    #p(topic),
    h(2, "Frage"), 
    p(question),
    h(2, "Antwort"), 
    p(answer)
  )
  paste(text_lines, collapse = "\n")
}

rmd_header <- function(title, author, output, date) {
  c(
    "---",
    paste("title:", title),
    paste("author:", author),
    paste("date:", date),
    paste("output:", output),
    "---"
  )
}

create_isoslides_markdown <- function(records) {
  quiz_header <- rmd_header(
    title = "Mathequiz", 
    author = "Hauke Sonnenberg", 
    date = format(Sys.Date(), "%d.%m.%Y"),
    output = "ioslides_presentation"
  )
  records <- split(data, seq_len(nrow(data)))
  slides <- lapply(records, function(record) {
    create_slides_markdown(record$thema, record$frage, record$antwort)
  })
  c(add_empty(quiz_header), paste(slides, collapse = "\n\n"))
}

{
  set.seed(123)
  
  data <- read.table(file, sep = ";", header = TRUE)
  data <- handle_placeholders(data)
  data <- randomly_sorted(data)
  
  output_dir <- "./quiz"
  dir.create(output_dir, showWarnings = FALSE)
  rmd_file <- file.path(output_dir, "quiz.rmd")
  writeLines(create_isoslides_markdown(data), rmd_file)
  
  rmarkdown::render(
    rmd_file, 
    output_format = rmarkdown::ioslides_presentation(widescreen = TRUE),
    output_file = "quiz.html", 
    output_dir = output_dir, 
    quiet = TRUE
  )
  
  file.copy(file.path(output_dir, "quiz.html"), "~/index.html")
  
  # Checkout gh-pages, then 
  #file.copy("~/index.html", ".", overwrite = TRUE)
}
