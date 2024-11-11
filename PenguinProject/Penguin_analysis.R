library(tidyverse)
library(palmerpenguins)
library(here)
library(janitor)
library(ragg)
library(svglite)
source(here("functions", "cleaning.R"))
here::here()

#Load libraries 

#show first 6 rows 

head(penguins)

#show column names 

colnames(penguins)

#write data using a csv using here

write.csv(penguins_raw, here("data", "penguins_raw.csv"))

#this put the penguin data in data folder we made previsously 

#removing comments column (BAD PRACTICE)
colnames(penguins_raw)
penguins_raw <- select(penguins_raw, -Comments) #bad practice if
# we run this line again it wont work because penguins raw doesnt have
# comments anymore 
colnames(penguins_raw)
# fixing it -getting back to safe file
penguins_raw <- read.csv(here("data","penguins_raw.csv"))
#using piping to remove comments and delta columns 
penguins_clean <- penguins_raw %>%
  select(-Comments) %>%
  select(-starts_with("Delta")) #removes tow columns called delta

colnames(penguins_clean)

#using piping to remove comments and delta columns 

penguins_clean <- penguins_raw %>%
  select(-Comments) %>%
  select(-starts_with("Delta")) %>%
  clean_names()

#save it
write.csv(penguins_clean, here("data","penguins_clean.csv"))

#making a function 
cleaning_penguin_columns <- function(raw_data){
  raw_data %>%
    clean_names() %>%
    select(-comments) %>%
    select(-starts_with("delta")) #removes two columns called delta}
}

#exploratory data

penguins_raw <- read_csv(here("data", "penguins_raw.csv"),
                         penguins_clean <- penguins_raw %>%
                           clean_column_names() %>%
                           remove_columns(c("comments", "delta")) %>%
                           shorten_species() %>%
                           remove_empty_columns_rows(),
#a place to start 
penguins_clean <- read_csv(here("data","penguins_clean.csv")),

species_colours <- c("Adelie" = "darkorange",
                     "Chinstrap" = "purple",
                     "Gentoo" = "cyan4"),

flipper_boxplot <- ggplot(
  data = penguins_flippers,
  aes(x = species,
      y = flipper_length_mm)) +
  geom_boxplot(aes(color = species),
               width = 0.3,
               show.legend = FALSE) + 
  geom_jitter(aes(color = species),
              alpha = 0.3,
              show.legend = FALSE,
              position = position_jitter(
                width = 0.2,
                seed = 0)) +
  scale_color_manual(values = species_colours) +
                labs(x = "Penguin species",
                     y = "Flipper Length (mm)") +
  theme_bw()
flipper_boxplot


              

#subset the columns
penguins_flippers <- penguins_clean %>% 
  select(c("species","flipper_length_mm")) %>%
  drop_na()
colnames(penguins_flippers)

# Update renv
renv::snapshot()

