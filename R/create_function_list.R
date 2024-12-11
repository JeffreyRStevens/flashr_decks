

#  Do not use this script. Do not rewrite function list. Edit by hand.


library(tidyverse)
library(here)

# Get file names in repository
get_repo <- function(repo_text) {
  gh::gh(repo_text) |>
    vapply("[[", "", "name")
}
get_repo_mem <- memoise::memoise(get_repo)

read_csv_mem <- memoise::memoise(read_csv)

repo <- "JeffreyRStevens/flashr_decks"
repo_text <- paste0("GET /repos/", repo, "/contents/decks")
deckfiles <- get_repo_mem(repo_text)
deckfiles <- deckfiles[which(deckfiles != "00_all_decks.csv" & !grepl("psyteachr", deckfiles))]
deckpaths <-
  paste0("https://raw.githubusercontent.com/", repo, "/main/decks/", deckfiles)

all_operators <- c(":", "::", "%>%", "%<>%", "%T>%", "%$%", "[]", "=", "<-", "<<-", "|>", "+", "-", "*", "/", "%%", "%/%", "^", ">", "<", ">=", "<=", "==", "!=", "&", "|", "!", ":", "%in%", "%*%")

all_functions <- deckpaths |> 
  map_dfr(read_csv_mem) |> 
  filter(grepl("\\(\\)", term) | term %in% all_operators) |> 
  distinct(term, package, .keep_all = TRUE) |> 
  select(-title) |> 
  relocate(package, .after = term) |> 
  mutate(function_name = ifelse(term %in% all_operators, term, paste0(package, "::", term)), .after = package) |> 
  arrange(term)

# Do not rewrite function list
# write_csv(all_functions, here("data/functions.csv"))   
