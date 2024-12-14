library(flashr)
library(tidyverse)
library(here)

# Create version of extract_code() that ignores eval: false
extract_code2 <- function(file,
                          empty = TRUE,
                          comments = TRUE) {
  stopifnot(
    "'file' should be a character string with one element" =
      typeof(file) == "character" & length(file) == 1
  )
  res <- litedown::crack(file)
  code <- unlist(lapply(res, function(el) {
    if (is.expression(el$options$eval)) {
      if (el$options$engine == "r" && el$type == "code_chunk") el$source
    } else {
      if (el$options$engine == "r" && el$type == "code_chunk" && all(el$options$eval != FALSE) && all(el$options$echo != FALSE) && all(el$options$include != FALSE)) el$source
    }
  }))
  if (!empty) {
    code <- code[code != ""]
  }
  if (!comments) {
    code <- gsub(" ", "", code)
    code <- code[!grepl("^#", code)]
  }
  code
}


# Create version of extract_function() that includes packages
extract_functions2 <- function(code,
                               duplicates = TRUE,
                               package = TRUE) {
  stopifnot("'code' should be a character vector" = typeof(code) == "character")
  d <- getParseData(x = parse(text = code, keep.source = TRUE))
  f <- d[d$token == "SYMBOL_FUNCTION_CALL", "text"]
  p <- d[d$token == "SYMBOL_PACKAGE", "text"]
  for (s in d[d$token == "SYMBOL", "text"]) {
    tryCatch(
      {
        ev <- eval(as.symbol(s), parent.frame())
        if (is.function(ev)) f <- c(f, s)
      },
      error = function(e) NULL
    )
  }
  if (package && !rlang::is_empty(p)) {
    f <- paste(p, f, sep = "::")
  }
  if (duplicates) {
    f
  } else {
    unique(f)
  }
}

# Create version of build_functions_df() that includes packages
build_functions_df2 <- function(file = NULL,
                                fs = NULL,
                                title,
                                desc = TRUE,
                                omit = TRUE) {
  # Validate arguments
  stopifnot(
    "Needs argument for either file or fs but not both" =
      (is.null(file) & !is.null(fs)) | (!is.null(file) & is.null(fs))
  )
  if (!is.null(file)) {
    stopifnot(
      "'file' should be a character string with one element" =
        typeof(file) == "character" & length(file) == 1
    )
  }
  if (!is.null(fs)) stopifnot("'fs' should be a character vector" = typeof(fs) == "character")
  stopifnot("'title' should be a character vector" = typeof(title) == "character")
  stopifnot("'desc' should be a logical" = typeof(desc) == "logical")
  
  # Extract functions from files
  if (!is.null(file)) fs <- extract_functions(extract_code(file))
  
  # Create vector of operators
  operators_csv <- "https://raw.githubusercontent.com/JeffreyRStevens/flashr/refs/heads/main/inst/extdata/operators.csv"
  fail_gracefully(operators_csv)
  operators <- utils::read.csv(operators_csv)$term
  
  # Create vector unique functions with appropriate ()
  fsplit <- stringr::str_split(fs, pattern = "::")
  
  split_package <- function(x) {
    if (length(x) == 1L) {
      p <- ""
      f <- x
    } else if (length(x) == 2L) {
      p <- x[1]
      f <- x[2]
    } else {
      stop("Too many elements.")
    }
    data.frame(package = p, func = f)
  }
  func_pkg <- map(fsplit, split_package) |> 
    list_rbind() |> 
    mutate(combined = fs) |> 
    rowwise() |> 
    mutate(url = ifelse(package != "", downlit::href_topic(func, package), ""))
  
  unique_functions <- func_pkg |> 
    ungroup() |> 
    slice_head(n = 1, by = func) |> 
    mutate(term = ifelse(func %in% operators, func, paste0(func, "()")),
           description = NA,
           title = title)
  
  # Initiate descrips and pkgs
  descrips <- pkgs <- url <- NA_character_
  
  # Pull descriptions and packages from flashr_decks function CSV
  if (desc) {
    functions_csv <- "https://raw.githubusercontent.com/JeffreyRStevens/flashr_decks/refs/heads/main/data/functions.csv"
    fail_gracefully(functions_csv)
    all_functions <- utils::read.csv(functions_csv)
    for (i in seq_along(unique_functions$term)) {
      unique_functions$description[i] <- ifelse(unique_functions$term[i] %in% all_functions$term,
                            all_functions[all_functions$term == unique_functions$term[i], ]$description,
                            NA_character_
      )
      unique_functions$package[i] <- ifelse(unique_functions$term[i] %in% all_functions$term,
                        all_functions[all_functions$term == unique_functions$term[i], ]$package,
                        NA_character_
      )
      unique_functions$url[i] <- ifelse(unique_functions$term[i] %in% all_functions$term,
                       all_functions[all_functions$term == unique_functions$term[i], ]$url,
                       NA_character_
      )
    }
  }
  # df <- data.frame(term = unique_functions$func, description = descrips, url = url, package = pkgs, title = title)
  if (omit) {
    unique_functions <- unique_functions[!is.na(unique_functions$description), ]
  }
  unique_functions |> 
    select(term, description, url, package, title)
}


fail_gracefully <- function(remote_file, maxtime = 10) {
  try_GET <- function(x, ...) {
    tryCatch(
      httr::GET(url = x, httr::timeout(maxtime), ...),
      error = function(e) conditionMessage(e),
      warning = function(w) conditionMessage(w)
    )
  }
  is_response <- function(x) {
    class(x) == "response"
  }
  
  # First check internet connection
  if (!curl::has_internet()) {
    message("No internet connection.")
    return(invisible(NULL))
  }
  # Then try for timeout problems
  resp <- try_GET(remote_file)
  if (!is_response(resp)) {
    message(resp)
    return(invisible(NULL))
  }
  # Then stop if status > 400
  if (httr::http_error(resp)) {
    httr::message_for_status(resp)
    return(invisible(NULL))
  }
}

# Create decks from book GitHub
extract_book <- function(repo, input, path, download = TRUE, startzero = FALSE, number = FALSE, omitdesc = FALSE) {
  if (download) {
    usethis::use_github_file(repo, path = input, save_as = paste0(path, input))
  }
  book <- sub("books/", "", path) |> 
    sub("/", "", x = _)
  if (book == "r4ds-2e") book <- "R4DS-2e"
  if (number) {
    num <- stringr::str_split_1(input, "-")[1]
  } else {
    num <- which(input == chapter_names)
    if (startzero) num <- (num - 1)
  }
  num <- stringr::str_pad(num, 2, pad = "0")
  title <- paste0(book, " Ch. ", num)
  code <- extract_code2(here(paste0(path, input)))
  if (!is.null(code)) {
    deck <- build_functions_df2(fs = extract_functions2(code), 
                                title = title,
                                omit = omitdesc)
    write.csv(deck, paste0("decks/", tolower(book), num, ".csv"))
  }
}

extract_files <- function(repo, path = "", type = "qmd") {
  get_repo <- function(repo_text) {
    gh::gh(repo_text) |>
      vapply("[[", "", "name")
  }
  get_repo_mem <- memoise::memoise(get_repo)
  repo_text <- paste0("GET /repos/", repo, "/contents/", path)
  files <- get_repo_mem(repo_text)
  files[which(grepl(type, files))]
}


# R for Data Science (2nd edition) ----
# https://github.com/hadley/r4ds


chapter_names <- c("intro.qmd", "data-visualize.qmd", "workflow-basics.qmd", "data-transform.qmd", "workflow-style.qmd", "data-tidy.qmd", "workflow-scripts.qmd", "data-import.qmd", "workflow-help.qmd", "layers.qmd", "EDA.qmd", "communication.qmd", "logicals.qmd", "numbers.qmd", "strings.qmd", "regexps.qmd", "factors.qmd", "datetimes.qmd", "missing-values.qmd", "joins.qmd",  "spreadsheets.qmd", "databases.qmd", "arrow.qmd", "rectangling.qmd", "webscraping.qmd", "functions.qmd", "iteration.qmd", "base-R.qmd", "quarto.qmd", "quarto-formats.qmd")

repo <- "hadley/r4ds"
path <- "books/r4ds-2e/"
purrr::walk(chapter_names, ~ extract_book(repo, input = .x, path, download = FALSE, startzero = TRUE))



# PsyTeachR Fundamentals of Quantitative Analysis ----
repo <- "PsyTeachR/quant-fun-v3"
all_files <- extract_files(repo)
chapter_names <- all_files[c(2:14, 18:19)]

path <- "books/psyteachr_fqa/"
purrr::walk(chapter_names, ~ extract_book(repo, input = .x, path, download = FALSE))




# PsyTeachR Applied Data Skills ----
repo <- "PsyTeachR/ads-v3"
all_files <- extract_files(repo)
chapter_names <- all_files[c(1:4, 7:10)]

path <- "books/psyteachr_ads/"
purrr::walk(chapter_names, ~ extract_book(repo, input = .x, path, download = FALSE, number = TRUE))




# PsyTeachR Data Skills for Reproducible Research ----
repo <- "PsyTeachR/reprores-v4"
all_files <- extract_files(repo)
chapter_names <- all_files[c(1:10)]

path <- "books/psyteachr_dsrr/"
purrr::walk(chapter_names, ~ extract_book(repo, input = .x, path, download = FALSE, number = TRUE))


all_files <- list.files("decks", pattern = "*.csv", full.names = TRUE, include.dirs = TRUE)
deck_files <- all_files[-1]
combined_decks <- deck_files |> 
  map(~read_csv(.x, col_select = term:title, show_col_types = FALSE)) |> 
  list_rbind()

missing_descriptions <- combined_decks |>  
  filter(is.na(description)) |> 
  slice_head(n = 1, by = term)

missing_r4ds2e <- missing_descriptions |> 
  filter(grepl("R4DS-2e", title)) |> 
  arrange(title, term)
write_csv(missing_r4ds2e, here("data/r4ds2e_empty.csv"))
