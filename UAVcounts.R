library(sf)
library(dplyr)

shape <- read_sf(dsn = "../Downloads/PredictedBirds", layer = "PredictedBirds") %>%
         rename(species = label, colony = Site, date = Date) %>%
         as.data.frame() %>%
         select(colony, date, species) %>%
         group_by(colony, date, species) %>%
         summarize(count = n()) %>%
         mutate(image_type = "Drone_AI", 
                counter = "AI", 
                date = as.Date(date, format = "%m_%d_%Y"),
                species = replace(species, species=="Great Blue Heron", "gbhe"),
                species = replace(species, species=="Great Egret" , "greg"),
                species = replace(species, species=="Roseate Spoonbill", "rosp"),
                species = replace(species, species=="White Ibis", "whib"),
                species = replace(species, species=="Wood Stork", "wost"),
                species = replace(species, species=="Snowy Egret", "sneg"),
                colony = replace(colony, colony=="HiddenMain", "Hidden"),
                colony = replace(colony, colony=="3BRamp080", "3BRamp"),
                colony = replace(colony, colony=="Forsetti", "Forseti")) %>%
                dplyr::mutate(order = dplyr::case_when(
                  date %in% seq(as.Date("2022-03-01"), by = "day", length.out = 14) ~ 1,
                  date %in% seq(as.Date("2022-03-21"), by = "day", length.out = 18) ~ 2,
                  date %in% seq(as.Date("2022-04-25"), by = "day", length.out = 8) ~ 3,
                  TRUE ~ NA)) %>%
        filter(species != "sneg")
                                                                
data_raw <- read.csv("count_comparisons.csv") %>%
  rename_with(tolower) %>%
  mutate(colony = gsub(" ", "", colony)) %>%
  select(-c(sneg,smwh)) %>%
  tidyr::pivot_longer(cols = !c(date,colony,image_type,counter,behavior), 
                      names_to = "species",
                      values_to = "count") %>%
  mutate(date = as.Date(date, format = "%m/%d/%Y")) %>%
  group_by(date,colony,image_type,counter,species,behavior) %>%
  summarise (count = mean(count)) %>%
  group_by(date,colony,image_type,counter,species) %>%
  summarise(count = sum(count)) %>%
  dplyr::mutate(order = dplyr::case_when(
    date %in% seq(as.Date("2022-03-01"), by = "day", length.out = 14) ~ 1,
    date %in% seq(as.Date("2022-03-21"), by = "day", length.out = 18) ~ 2,
    date %in% seq(as.Date("2022-04-25"), by = "day", length.out = 8) ~ 3,
    TRUE ~ NA))

all_data <- bind_rows(data_raw,shape)

totals <- all_data %>%
          group_by(date,colony,image_type,counter,order) %>%
          summarise(count = sum(count)) %>%
          mutate(species = " total")

all_data <- bind_rows(all_data,totals)

airplane_dates <- data_raw %>%
                  ungroup() %>%
                  filter(image_type=="Airplane") %>%
                  select(date,colony,order) %>%
                  unique() %>%
                  rename(airplane_date = date) %>%
                   right_join(all_data) %>%
                   select(date,colony,order,airplane_date) %>%
                   unique() %>%
                   mutate(lag = as.numeric(airplane_date-date))

airplane_compare1 <- all_data %>%
                      ungroup() %>%
                      filter(image_type == "Drone_AI", !is.na(order)) %>%
                      left_join(airplane_dates) %>%
                      rename(Drone_AI = count) %>%
                      select(-c(date,counter,image_type)) 
airplane_compare2 <- all_data %>%
                     ungroup() %>%
                     filter(image_type == "Airplane", !is.na(order)) %>%
                     rename(Airplane = count) %>%
                     select(-c(date,image_type)) 

airplane_compare <- merge(airplane_compare1,airplane_compare2, by = c("colony","order","species"))

  
drone_compare <- all_data %>%
                ungroup() %>%
                filter(counter %in% c("LG","AI"), image_type != "Airplane", !is.na(order)) %>%
                select(-c(counter)) %>%
                tidyr::pivot_wider(names_from = image_type, values_from = count)


library(ggplot2)

ggplot(drone_compare, aes(Drone, Drone_AI, col=colony)) +
  geom_point(cex=2) +
  geom_abline(slope=1, intercept = 0, col = "red") +
  facet_wrap(vars(species), scales = "free") +
  theme_minimal(base_size=12)

ggplot(airplane_compare, aes(Airplane, Drone_AI, col=colony)) +
  geom_point(cex=2) +
  geom_abline(slope=1, intercept = 0, col = "red") +
  facet_wrap(vars(species), scales = "free") +
  theme_minimal(base_size=12)

airplane_compare %>% filter(lag <= 2, lag >= -2, counter=="LG") %>%

ggplot(aes(Airplane, Drone_AI, col=colony)) +
  geom_point(cex=2) +
  geom_abline(slope=1, intercept = 0, col = "red") +
  facet_wrap(vars(species), scales = "free") +
  theme_minimal(base_size=12)

airplane_compare %>% filter(counter=="LG") %>%
  
  ggplot(aes(Airplane, Drone_AI, col=colony)) +
  geom_point(cex=2) +
  geom_abline(slope=1, intercept = 0, col = "red") +
  facet_wrap(vars(species), scales = "free") +
  theme_minimal(base_size=12)

airplane_compare %>% mutate(error = abs(Airplane-Drone_AI), lag = abs(lag)) %>%
  ggplot(aes(lag, error, col=colony)) +
  geom_point(cex=2) +
  facet_wrap(vars(species), scales = "free") +
  theme_minimal(base_size=12)

airplane_compare %>% mutate(error = Drone_AI-Airplane) %>%
  ggplot(aes(counter, error)) +
  geom_violin() +
  theme_minimal(base_size=12)
