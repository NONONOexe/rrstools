file_path <- here::here("data-raw", "competition_maps.csv")
competition_maps <- read.csv(file_path)
usethis::use_data(competition_maps)
