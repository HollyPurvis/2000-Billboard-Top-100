# tidyVerse
install.packages("tidyverse")
library(tidyverse)

# Tidyr
install.packages("tidyr")
library(tidyr)

# billboard data packag
install.packages("dplyr")
library(dplyr)

# Plot Title
install.packages("glue")
library(glue)

# Plot Title
install.packages("stringr")
library(stringr)


# gets billboard dataset
data(billboard)

#Tidy Data
billboard_tidy = pivot_longer(
  billboard,
  col=starts_with("wk"),
  names_to = "week",
  values_to="rank",
  values_drop_na = TRUE
) %>% mutate(week = as.numeric(str_replace(week, "wk", "")))

# gets only the artists that were in the top 10 and removes duplicates
top_10_artists = billboard_tidy %>% filter(rank <= 10) %>% select(artist) %>% distinct  %>% pull(artist)

# function that gets the artist name from the user
getArtist=function()
{
  
  # shows user their options of artists that had top 10 hits
  for (i in seq_along(top_10_artists)) {
    cat(i, ": ", top_10_artists[i], "\n", sep = "")
  }
  
  # gets input from user and converts it to an int
  artist_index = readline(prompt = "Enter Number Of Artist:") %>% as.integer
  
  return (top_10_artists[artist_index])
  
}
artist_name = getArtist()

# gets all the songs that the artist had in the top billboard 100
artist_hits = billboard_tidy %>% filter(artist %in% artist_name)

# if the artist's name is formatted last, first reformat it
artist_title = str_replace(artist_name, "^(.+),\\s(.+)$", "\\2 \\1")

# gets title for plot
plot_title = glue("{artist_title}'s Billboard 100 Hits in 2000")

# generates line plot to show how the artist's songs ranked throughout the year
artist_hits %>%
  ggplot( 
    aes(
      x = week,
      y = rank,
      group = track,
      color = track,
    )
  ) + ggtitle(plot_title) + scale_y_continuous(
    trans = scales::trans_new(
      name = "reverse",
      transform = function(x) -x,
      inverse = function(x) -x
    ),
    breaks = seq(1, max(artist_hits$rank), by = 2),
    labels = scales::label_number()
  ) + scale_x_continuous(breaks = seq(1, max(artist_hits$week), by = 1)) +  geom_line()
