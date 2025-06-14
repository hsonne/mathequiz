#install.packages("xaringan")
if (FALSE) {
romans <- c("I", "V", "X", "L", "C", "D", "M", "-", "-")

lmu <- sapply(2*(1:4) - 1, function(i) {
  paste(romans[i:(i+2)], collapse = "")
})

a <- 999:1001

n <- length(a)
ma <- matrix(a, n, 4)
md <- t(matrix(10^(3:0), 4, n))
digits <- ma %/% md %% 10

# l = lower, m = middle, u = upper
pattern <- c(
    "-",                # 0
    "l",   "ll", "lll", # 1, 2, 3
   "lm",    "m",  "ml", # 4, 5, 6
  "mll", "mlll",  "lu"  # 7, 8, 9
)

chr <- function(s, i) substr(s, i ,i)
lookups <- sapply(lmu, function(x) {
  res <- sapply(pattern, function(p) {
    p <- gsub("l", chr(x, 1), p)
    p <- gsub("m", chr(x, 2), p)
    p <- gsub("u", chr(x, 3), p)
    p
  })
  stats::setNames(res, pattern)
})

lookups
dimnames(lookups)
digits
p <- matrix(pattern[digits + 1], n)
p


}

file <- "data/jeder-gegen-jeden-fragen.txt"

rom_lookup <- c(
  I = 1L, 
  V = 5L,
  X = 10L,
  L = 50L, 
  C = 100L, 
  D = 500L, 
  M = 1000L
)

rom_to_dec <- function(r) {
  sapply(strsplit(r, ''), function(x) {
    values <- rom_lookup[x]
    if (length(i <- which(diff(values) > 0))) {
      values[i] <- values[i+1] - values[i]
      values <- values[-(i+1)]
    }
    sum(values)
  })
}

dec_to_rom <- function(a) {
  
  stopifnot(all(a < 4000L))
  a <- as.integer(a)
  
  if (length(a) > 1L) {
    return(sapply(a, dec_to_rom))
  }
  
  lookup <- matrix(
    data = c(
      1, 0, 0, 0,
      2, 0, 0, 0,
      3, 0, 0, 0,
      1, 1, 0, 0,
      0, 1, 0, 0,
      1, 1, 0, 1,
      2, 1, 0, 1,
      3, 1, 0, 1,
      1, 0, 1, 0
    ),
    ncol = 4L, 
    byrow = TRUE, 
    dimnames = list(NULL, c("lower", "middle", "upper", "swap"))
  )
  
  symbols_for_exponent <- function(e) {
    symbols <- names(rom_lookup)
    result <- symbols[seq.int(2*e + 1, min(2*e + 3L, length(symbols)))]
    names(result) <- c("lower", "middle", "upper")[seq_along(result)]
    result
  }
  
  collapse <- function(x) paste(x, collapse = "")
  
  collapse(sapply(3:0, function(e) {
    
    potence <- 10^e
    times <- a %/% potence
    
    if (times == 0L) {
      return("")
    }
    
    stimes <- if (lookup[times, "swap"]) {
      lookup[times, 3:1]
    } else {
      lookup[times, 1:3]
    }
    
    a <<- a - times * potence
    collapse(rep(symbols_for_exponent(e)[names(stimes)], stimes))
  }))
}

stopifnot(identical(rom_to_dec(dec_to_rom(a <- 1:3999)), a))

handle_placeholders <- function(data) {
  
  rnd <- function(...) {
    sample(do.call(c, list(...)), 1L)
  }
  
  int <- as.integer
  
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
