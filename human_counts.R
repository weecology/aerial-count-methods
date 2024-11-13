`%>%` <- magrittr::`%>%`

data_path <- "count_comparisons.csv"

data_raw <- read.csv(data_path) %>%
  dplyr::rename_with(tolower) %>%
  mutate(colony = gsub(" ", "", colony),
         whib = whib + sneg + smwh) %>%
  select(-c(sneg,smwh)) %>%
  tidyr::pivot_longer(cols = !c(date,colony,image_type,counter,behavior), 
                      names_to = "species",
                      values_to = "count") %>%
  mutate(date = as.Date(date, format = "%m/%d/%Y"))

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

counter_comparison <- data_raw %>%
  dplyr::group_by(date,colony,image_type,counter,species,behavior) %>%
  dplyr::summarise(count = mean(count)) %>%
  dplyr::group_by(date,colony,image_type,counter,species) %>%
  dplyr::summarise(count = sum(count)) 


library(ggplot2)
ggplot(total_counts, aes(drone_count, airplane_count, col=species)) +
  geom_point(cex=2) +
  geom_abline(slope=1, intercept = 0, col = "red") +
  facet_wrap(vars(species), scales = "free") +
  theme_minimal(base_size=12) +
  labs(title="Human Counts -  UAV vs Airplane")

ggplot(total_counts, aes(drone_count, airplane_count, col=colony)) +
  geom_point(cex=2) +
  geom_abline(slope=1, intercept = 0, col = "red") +
  facet_wrap(vars(species), scales = "free") +
  theme_minimal(base_size=12) +
  labs(title="Human Counts -  UAV vs Airplane")

ggplot(total_counts, aes(drone_count, airplane_count, col=species)) +
  geom_point(cex=2) +
  geom_abline(slope=1, intercept = 0, col = "red") +
  facet_wrap(vars(counter), nrow = 2, scales = "free") +
  theme_minimal(base_size=12)

dplyr::filter(all_airplane,counter=="LG") %>%
  mutate(airplane_count = airplane_count+1, drone_count = drone_count+1) %>%
  ggplot(aes(drone_count, airplane_count, col=species)) +
  geom_point(cex=2) +
  coord_trans(x="log10", y="log10") +
  geom_abline(slope=1, intercept = 0, col = "red") +
  facet_wrap(vars(order), nrow = 2, scales = "free") +
  theme_minimal(base_size=12)


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
  
## Violin plots
  total_counts %>% mutate(error = drone_count-airplane_count) %>%
  ggplot(aes(counter, error)) +
  geom_violin() +
  theme_minimal(base_size=12) +
    labs(title="Error (Drone-Airplane) by counter")
  