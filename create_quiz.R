#install.packages("xaringan")

file <- "data/jeder-gegen-jeden-fragen.txt"

handle_placeholders <- function(data) {
  
  rnd <- function(...) {
    sample(do.call(c, list(...)), 1L)
  }

  de_formatted <- function(x, fmt) {
    gsub("\\.", ",", sprintf(fmt, x))
  }
  
  de_to_num <- function(x) {
    as.numeric(gsub(",", ".", x))
  }

  is_ticked <- function(x) {
    startsWith(x, "`")
  }
  
  split_into_tokens <- function(x) {
    p <- "\\[+[^]]*\\]+|`[^`]*`|[^][`]*"
    matches <- gregexpr(p, x)
    regmatches(x, matches)
  }
  
  eval_tokens <- function(tokens) {
    is_expr <- is_ticked(tokens)
    values <- sapply(tokens[is_expr], USE.NAMES = FALSE, function(e) {
      eval(parse(text = gsub("`", "", e)))
    })
    tokens[is_expr] <- values
    list(tokens = tokens, indices = which(is_expr))
  }
  
  question_tokens <- split_into_tokens(data$frage)
  answer_tokens <- split_into_tokens(data$antwort)
  
  indices <- which(sapply(question_tokens, function(x) sum(is_ticked(x)) > 0L))

  for (index in indices) {
    result <- eval_tokens(tokens = question_tokens[[index]])
    question_tokens[[index]] <- result$tokens
    x <- result$tokens[result$indices]
    result <- eval_tokens(answer_tokens[[index]])
    answer_tokens[[index]] <- result$tokens
  }
  
  data$frage <- sapply(question_tokens, paste, collapse = "")
  data$antwort <- sapply(answer_tokens, paste, collapse = "")
  
  data
}

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

add_empty <- function(x) {
  c(x, "")
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

data <- read.table(file, sep = ";", header = TRUE)

if (TRUE)
{
  #set.seed(123)
  
  # Duplicate rows with n > 1
  data$n[is.na(data$n)] <- 1L
  i <- which(data$n > 1L)
  data <- rbind(data, data[rep(i, data$n[i] - 1L), ])

  data <- handle_placeholders(data)
  data <- data[sample(nrow(data)), ]
  
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
