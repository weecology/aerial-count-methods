`%>%` <- magrittr::`%>%`

data_path <- "count_comparisons.csv"

data_raw <- read_csv(data_path, col_names = TRUE) %>%
  dplyr::rename_with(tolower) %>%
  tidyr::pivot_longer(cols = !c(date,colony,image_type,counter,behavior), 
                      names_to = "species",
                      values_to = "count")

drone_counts <- data_raw %>%
  dplyr::group_by(date,colony,image_type,counter,species,behavior) %>%
  dplyr::summarise(count = mean(count)) %>%
  dplyr::group_by(date,colony,image_type,counter,species) %>%
  dplyr::summarise(count = sum(count)) %>%
  dplyr::filter(image_type=="Drone") %>%
  dplyr::ungroup() %>%
  dplyr::rename(drone_count=count) %>%
  dplyr::mutate(order = dplyr::case_when(
    date %in% c("3/2/22","3/3/22","3/4/22","3/9/22","3/14/22") ~ 1,
    date %in% c("3/21/22","3/22/22","3/24/22","3/28/22","3/29/22","3/30/22","4/6/22","4/7/22") ~ 2,
    TRUE ~ 3)) %>%
  dplyr::select(-c(image_type,date)) 
  
airplane_counts <- data_raw %>%
  dplyr::group_by(date,colony,image_type,counter,species,behavior) %>%
  dplyr::summarise(count = mean(count)) %>%
  dplyr::group_by(date,colony,image_type,counter,species) %>%
  dplyr::summarise(count = sum(count)) %>%
  dplyr::filter(image_type=="Airplane") %>%
  dplyr::ungroup() %>%
  dplyr::rename(airplane_count=count) %>%
  dplyr::mutate(order = dplyr::case_when(
    date %in% c("3/2/22","3/3/22","3/4/22") ~ 1,
    date %in% c("3/28/22", "3/29/22") ~ 2,
    TRUE ~ 3)) %>%
  dplyr::select(-c(image_type,date)) 

total_counts <- merge(drone_counts,airplane_counts)

all_airplane <- merge(drone_counts,dplyr::select(airplane_counts,-counter))

plot(total_counts$drone_count,total_counts$airplane_count, xlim = c(0,5000), ylim = c(0,5000))
abline(a = 0, b=1, col="red")          

library(ggplot2)
ggplot(total_counts, aes(drone_count, airplane_count, col=species)) +
  geom_point() +
  geom_abline(slope=1, intercept = 0, col = "red") +
  facet_wrap(vars(species))

ggplot(total_counts, aes(drone_count, airplane_count, col=species)) +
  geom_point() +
  geom_abline(slope=1, intercept = 0, col = "red") +
  facet_wrap(vars(counter), nrow = 2, scales = "free")

dplyr::filter(all_airplane,counter=="LG") %>%
ggplot(aes(drone_count, airplane_count, col=species)) +
  geom_point() +
  geom_abline(slope=1, intercept = 0, col = "red") +
  facet_wrap(vars(order), nrow = 2, scales = "free")


## Drone double counts
lg_double <- data_raw %>%
  dplyr::filter(image_type=="Drone", counter=="LG", behavior=="nesting") %>%
  dplyr::group_by(date,colony,species) %>%
  dplyr::mutate(countid = paste0('count', dplyr::row_number())) %>%
  tidyr::spread(countid, count) %>%
  dplyr::filter(colony!="Horus")

  ggplot(lg_double, aes(count1, count2, col=species)) +
  xlim(0,300) +
  ylim(0,300) +
  geom_point() +
  geom_abline(slope=1, intercept = 0, col = "red")
  