#install.packages("xaringan")

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

create_slides_markdown <- function(question, answer) {
  rep_str <- function(x, n) paste(rep(x, n), collapse = "")
  h <- function(n, x) add_empty(paste(rep_str("#", n), x))
  p <- add_empty
  slide_class <- add_empty("class: middle,center")
  text_lines <- c(
    # Slide with question
    slide_class,
    #h(1, "Frage"), 
    h(1, question),
    # Slide with question and answer
    "---",
    slide_class,
    #h(1, "Antwort"),
    h(1, answer),
    p("Antwort auf die Frage:"),
    p(dQuote(question))
  )
  paste(text_lines, collapse = "\n")
}

rmd_header <- function(title, author, date, output) {
  c(
    "---",
    paste("title:", title),
    paste("author:", author),
    paste("date:", date),
    paste("output:", output),
    "---"
  )
}

create_quiz_markdown <- function(records) {
  quiz_header <- rmd_header(
    title = "Mathequiz", 
    author = "Hauke Sonnenberg", 
    date = format(Sys.Date(), "%d.%m.%Y"),
    output = "xaringan::moon_reader" # "ioslides_presentation"
  )
  records <- split(data, seq_len(nrow(data)))
  slides <- lapply(records, function(record) {
    create_slides_markdown(record$frage, record$antwort)
  })
  c(add_empty(quiz_header), paste(slides, collapse = "\n---\n"))
}

{
  #set.seed(123)
  
  data <- read.table(file, sep = ";", header = TRUE)
  data <- handle_placeholders(data)
  data <- randomly_sorted(data)
  
  output_dir <- "./quiz"
  dir.create(output_dir, showWarnings = FALSE)
  rmd_file <- file.path(output_dir, "quiz.rmd")
  writeLines(create_quiz_markdown(data), rmd_file)
  
  output_file <- file.path(output_dir, "index.html")
  rmarkdown::render(
    rmd_file, 
    output_file = basename(output_file), 
    output_dir = dirname(output_file), 
    quiet = TRUE
  )

  if (FALSE) {
    deploy_dir <- "~/mathequiz"
    #unlink(deploy_dir, recursive = TRUE)
    dir.create(deploy_dir, recursive = TRUE, showWarnings = FALSE)
    dir(deploy_dir, recursive = TRUE) 
    from_rel <- dir(output_dir, recursive = TRUE)
    from_rel <- grep("\\.rmd$", from_rel, value = TRUE, invert = TRUE)
    to <- file.path(deploy_dir, from_rel)
    kwb.utils::createDirectories(unique(dirname(to)), dbg = FALSE) 
    file.copy(file.path(output_dir, from_rel), to, overwrite = TRUE)
  }
  
  # Checkout gh-pages, then 
  #file.copy(dir("~/mathequiz", recursive = TRUE, full.names = TRUE), ".", overwrite = TRUE)
}
