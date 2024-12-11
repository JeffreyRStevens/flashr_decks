
library(flashr)
library(here)

# Get file names in repository
get_repo <- function(repo_text) {
  gh::gh(repo_text) |>
    vapply("[[", "", "name")
}
get_repo_mem <- memoise::memoise(get_repo)

# Extract title from CSV
get_title <- function(x) {
  data <- utils::read.csv(x)
  data$title[1]
}
get_title_mem <- memoise::memoise(get_title)

repo <- "JeffreyRStevens/flashr_decks"
repo_text <- paste0("GET /repos/", repo, "/contents/decks")
deckfiles <- get_repo_mem(repo_text)
deckfiles <- deckfiles[which(deckfiles != "00_all_decks.csv")]

# Create labels, paths, and titles for decks
deckpaths <-
  paste0("https://raw.githubusercontent.com/", repo, "/main/decks/", deckfiles)
decklabels <- gsub(".csv", "", deckfiles)
titles <- vapply(deckpaths, get_title_mem, character(1))
decks <- paste0(titles, " (", decklabels, ")") |>
  gtools::mixedsort()

all_decks <- data.frame(deck = decks, title = titles, decklabel = decklabels)

write.csv(all_decks, here("data/all_decks.csv"), row.names = FALSE)
write.csv(all_decks, here("decks/00_all_decks.csv"), row.names = FALSE)


# Create HTML decks for website ------------------------------------------------

# for (i in seq_along(all_decks$decklabel)) {
#   deck <- all_decks$decklabel[i]
#   flashcard(deck, file = here(paste0("html/", deck, ".html")))
# }
all_decks <- read.csv(here("data/all_decks.csv"))
purrr::walk(all_decks$decklabel, ~ flashr::flashcard(.x, file = here(paste0("html/", .x, ".html"))))

# Run quarto render then copy slides from html to _site/decks (may have to create directory)