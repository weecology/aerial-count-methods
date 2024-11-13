library(sf)
library(dplyr)

### Human counted data from the airplane and drone images
# 1. sum whib, sneg, and smwh into one count
# 2. take the mean of multiple counts on the same images
# 3. sum roosting and nesting into one count
# 4. add order column to assign multiple drone flights one different dates to one airplane flight 

counted <- read.csv("count_comparisons.csv") %>%
            rename_with(tolower) %>%
            mutate(colony = gsub(" ", "", colony),
                   whib = whib + sneg + smwh) %>%
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

### Predicted counts from EverWatch
# 1. sum whib and sneg into one count (the model does not predict smwh)
# 2. add order column to assign multiple drone flights one different dates to one airplane flight 
predicted <- read_sf(dsn = "../Downloads/PredictedBirds", layer = "PredictedBirds") %>%
             rename(species = label, colony = Site, date = Date) %>%
             as.data.frame() %>%
             filter(score >=0.4) %>%
             select(colony, date, species) %>%
             mutate(date = as.Date(date, format = "%m_%d_%Y"),
                    species = replace(species, species=="Great Blue Heron", "gbhe"),
                    species = replace(species, species=="Great Egret" , "greg"),
                    species = replace(species, species=="Roseate Spoonbill", "rosp"),
                    species = replace(species, species=="White Ibis", "whib"),
                    species = replace(species, species=="Wood Stork", "wost"),
                    species = replace(species, species=="Snowy Egret", "whib"),
                    colony = replace(colony, colony=="HiddenMain", "Hidden"),
                    colony = replace(colony, colony=="3BRamp080", "3BRamp"),
                    colony = replace(colony, colony=="Forsetti", "Forseti")) %>%
             group_by(colony, date, species) %>%
             summarize(count = n()) %>%
             mutate(image_type = "Drone_AI", 
                    counter = "AI", 
                    order = case_when(date %in% seq(as.Date("2022-03-01"), by = "day", length.out = 14) ~ 1,
                                      date %in% seq(as.Date("2022-03-21"), by = "day", length.out = 18) ~ 2,
                                      date %in% seq(as.Date("2022-04-25"), by = "day", length.out = 8) ~ 3,
                                      TRUE ~ NA))

all_data <- bind_rows(counted,predicted)

totals <- all_data %>%
  group_by(date,colony,image_type,counter,order) %>%
  summarise(count = sum(count)) %>%
  mutate(species = " total")

all_data <- bind_rows(all_data,totals)

### Organize airplane comparisons
# here we need to take all 2022 drone flights and assign them to the closest airplane flight
# only use Lindsey counts to compare
airplane_dates <- counted %>%
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

### Organize drone human count comparisons
# here we need only the dates that have actually been human counted, 
# and compare them to the exact same predicted counts (no need for order and lag)
# only use Lindsey counts to compare
# be sure to include zero counts
flights_counted <- counted %>% ungroup() %>% filter(image_type=="Drone") %>% select(date,colony) %>% unique()

drone_compare <- all_data %>%
                 ungroup() %>%
                 filter(counter %in% c("LG","AI"), 
                        image_type != "Airplane", 
                        !is.na(order)) %>%
                 select(-c(counter)) %>%
                 right_join(flights_counted) %>%
                 tidyr::pivot_wider(names_from = image_type, values_from = count, values_fill = 0)

# library(ggplot2)

drone_compare  %>% mutate(Drone = Drone+1, Drone_AI = Drone_AI+1) %>%
ggplot(aes(Drone, Drone_AI, col=colony)) +
  geom_point(cex=2) +
  coord_trans(x="log10", y="log10") +
  geom_abline(slope=1, intercept = 0, col = "red") +
  facet_wrap(vars(species), scales = "free") +
  theme_minimal(base_size=12) +
  labs(title="UAV - Human vs MLM")

airplane_compare %>% filter(counter=="LG") %>% mutate(Airplane = Airplane+1, Drone_AI = Drone_AI+1) %>%

ggplot(aes(Airplane, Drone_AI, col=colony)) +
  geom_point(cex=2) +
  coord_trans(x="log10", y="log10") +
  geom_abline(slope=1, intercept = 0, col = "red") +
  facet_wrap(vars(species), scales = "free") +
  theme_minimal(base_size=12) +
  labs(title="Airplane+Human vs UAV+MLM")

airplane_compare %>% filter(lag <= 2, lag >= -2, counter=="LG") %>% 
  mutate(Airplane = Airplane+1, Drone_AI = Drone_AI+1) %>%

ggplot(aes(Airplane, Drone_AI, col=colony)) +
  geom_point(cex=2) +
  coord_trans(x="log10", y="log10") +
  geom_abline(slope=1, intercept = 0, col = "red") +
  facet_wrap(vars(species), scales = "free") +
  theme_minimal(base_size=12) +
  labs(title="Airplane+Human vs UAV+MLM, lag<=2")

airplane_compare %>% filter(counter=="LG") %>% 
  mutate(Airplane = Airplane+1, Drone_AI = Drone_AI+1) %>%
  
  ggplot(aes(Airplane, Drone_AI, col=colony)) +
  geom_point(cex=2) +
  coord_trans(x="log10", y="log10") +
  geom_abline(slope=1, intercept = 0, col = "red") +
  facet_wrap(vars(species), scales = "free") +
  theme_minimal(base_size=12) +
  labs(title="Airplane+LG vs UAV+MLM")

airplane_compare %>% mutate(error = abs(Airplane-Drone_AI), lag = abs(lag)) %>% filter(counter=="LG") %>%
  ggplot(aes(lag, error, col=colony)) +
  geom_point(cex=2) +
  facet_wrap(vars(species), scales = "free") +
  theme_minimal(base_size=12) +
  labs(title="Lag (days btw images) vs error (abs(Airplane_LG-Drone_AI))")

airplane_compare %>% mutate(error = Drone_AI-Airplane) %>%
  ggplot(aes(counter, error)) +
  geom_violin() +
  theme_minimal(base_size=12) +
  labs(title="Error (Drone_AI-Airplane) by counter")

just_drone <- all_data %>%
  ungroup() %>%
  filter(image_type == "Drone", 
         !is.na(order)) %>%
  right_join(flights_counted) %>%
  tidyr::pivot_wider(names_from = image_type, values_from = count, values_fill = 0)

just_ai <- all_data %>%
  ungroup() %>%
  filter(image_type == "Drone_AI", 
         !is.na(order)) %>%
  select(-"counter") %>%
  right_join(flights_counted) %>%
  tidyr::pivot_wider(names_from = image_type, values_from = count, values_fill = 0)

merge(just_drone,just_ai) %>% mutate(error = Drone_AI-Drone) %>%
  ggplot(aes(counter, error)) +
  geom_violin() +
  theme_minimal(base_size=12) +
  labs(title="Error (Drone_AI-Drone_Human) by counter")

##No log scale
drone_compare  %>% 
  ggplot(aes(Drone, Drone_AI, col=colony)) +
  geom_point(cex=2) +
  geom_abline(slope=1, intercept = 0, col = "red") +
  facet_wrap(vars(species), scales = "free") +
  theme_minimal(base_size=12) +
  labs(title="UAV - Human vs MLM")

airplane_compare %>% filter(counter=="LG") %>%
  
  ggplot(aes(Airplane, Drone_AI, col=colony)) +
  geom_point(cex=2) +
  geom_abline(slope=1, intercept = 0, col = "red") +
  facet_wrap(vars(species), scales = "free") +
  theme_minimal(base_size=12) +
  labs(title="Airplane+Human vs UAV+MLM")
