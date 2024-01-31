## ----setup, include=FALSE----------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----------------------------------------------------------------------
library(tidyverse)


## ----------------------------------------------------------------------
panteon <- read_csv("https://raw.githubusercontent.com/Tomasz-Olczyk/wizualizacjaR/main/podstawy/panteon_s.csv")


## ----------------------------------------------------------------------
glimpse(panteon)


## ----------------------------------------------------------------------


kolumny <- panteon %>%
  select(name, 
         countryCode3, 
         birthyear)



## ----------------------------------------------------------------------
mtcars_bez_kolumn_zaczynających_się_od_d <-select(mtcars, -starts_with("d"))


## ----------------------------------------------------------------------

names(mtcars_bez_kolumn_zaczynających_się_od_d) #funkcja names() zwraca nazwy kolumn(zmiennych) w ramce danych



## ----------------------------------------------------------------------
wzrost_waga <- select(starwars, wzrost = height, waga = mass)


## ----------------------------------------------------------------------

starwars_pl <- rename(starwars,  
                      wzrost = height,
                      waga = mass) # drugi argument jest nową nazwą kolumny/zmiennej



## ----------------------------------------------------------------------


View(starwars)



## ----------------------------------------------------------------------
glimpse(starwars)


## ----------------------------------------------------------------------

dwumetrowcy_GW <- filter(starwars, height > 200) 



## ----------------------------------------------------------------------

range(starwars$mass, na.rm = TRUE)



## ----------------------------------------------------------------------

mean(starwars$mass, na.rm = TRUE)



## ----------------------------------------------------------------------
fem <- filter(panteon, gender == "Female") 



## ----------------------------------------------------------------------

?grepl




## ----------------------------------------------------------------------

?str_detect()




## ----------------------------------------------------------------------

Green_and_grepl <-  filter(starwars, grepl("green", 
                                     skin_color))


## ----------------------------------------------------------------------

?stringr::str_detect()



## ----------------------------------------------------------------------

Green_and_stringr <-  filter(starwars, 
                             str_detect(skin_color, "green"))


## ----------------------------------------------------------------------

?contains
?str_detect



## ----------------------------------------------------------------------
marie <- filter(panteon, str_detect(name, "Mari"))


## ----------------------------------------------------------------------
marie_kob <- filter(panteon, 
                    str_detect(name, "Mari") & gender != "Male")


## ----------------------------------------------------------------------

kraje_marie <- marie %>% 
  group_by(countryName) %>% 
  count()



## ----------------------------------------------------------------------

urodzeni_w_roku_wojny <- filter(panteon, birthyear %in% c(1914, 1918, 1939, 1945))



## ----------------------------------------------------------------------

?is.na()



## ----------------------------------------------------------------------

is.na(starwars)



## ----------------------------------------------------------------------

waga_GW_bez_brakujacych  <- filter(starwars, 
                                   !is.na(mass))


## ----------------------------------------------------------------------

wzrost_GW_bez_brakujących <- filter(starwars, 
                                   !is.na(height))



## ----------------------------------------------------------------------

?drop_na()




## ----------------------------------------------------------------------
starwars_drop_na <- drop_na(starwars)


## ----------------------------------------------------------------------
?na.omit


## ----------------------------------------------------------------------

starwars_na.omit <- na.omit(starwars)



## ----------------------------------------------------------------------

GW_drop_na <- starwars %>%
  drop_na()



## ----------------------------------------------------------------------

GW_na.omit <- na.omit(starwars)



## ----------------------------------------------------------------------

?between() # dla wektora liczb



## ----------------------------------------------------------------------

boomerzy <-   filter(panteon, between(birthyear, 1945, 1960))



## ----------------------------------------------------------------------

arrange(starwars, height) # 



## ----------------------------------------------------------------------

arrange(starwars, desc(height))



## ----------------------------------------------------------------------

wg_masy <- arrange(starwars, mass)



## ----------------------------------------------------------------------

wg_wzrostu <-  arrange(starwars, height)



## ----------------------------------------------------------------------

kobiety_po1900 <- panteon %>% 
  filter(gender == "Female" & birthyear >= 1900) 



## ----------------------------------------------------------------------
panteon %>% 
  filter(gender == "Female" & birthyear >= 1900 & countryCode3 == "POL") %>%
  ggplot() +
  geom_col(aes(x = reorder(name, AverageViews), y = AverageViews)) +
  coord_flip() +
  labs(title = "Średnia liczba wyświetleń kobiet z Polski w zbiorze Panteon 1.0") +
  xlab("") +
  ylab("wyświetlenia")


## ----------------------------------------------------------------------

panteon %>% 
  group_by(continentName) %>%
  summarise(sławni = n()) 



## ----------------------------------------------------------------------

panteon %>% 
  group_by(continentName, gender) %>%
  summarise(sławni = n()) 



## ----------------------------------------------------------------------
panteon %>% 
  group_by(continentName, gender) %>%
  summarise(sławni = n()) %>%
  ggplot() +
  geom_col(aes(x = gender, 
               y = sławni,
               fill = gender)) +
  facet_wrap(~continentName)


## ----------------------------------------------------------------------

panteon %>%
  group_by(domain, continentName) %>%
  summarise(średnie = mean(AverageViews)) %>%
  ggplot() +
  geom_col(aes(x = domain, 
               y = średnie)) +
  coord_flip() +
  facet_wrap(~continentName)



## ----------------------------------------------------------------------

ggplot(panteon) +
  geom_boxplot(aes(x = domain, y = AverageViews)) +
  scale_y_log10() +
  coord_flip() +
  facet_wrap(~continentName)



## ----------------------------------------------------------------------

masa_rangowanie <- starwars %>%
  mutate(rangi = min_rank(mass))



## ----------------------------------------------------------------------

masa_rangowanie_procent  <- starwars %>%
  mutate(rangi = percent_rank(mass))



## ----------------------------------------------------------------------

numer_wiersza <- starwars %>%
  mutate(numer_wiersza = row_number(mass))



## ----------------------------------------------------------------------

?sample_n()



## ----------------------------------------------------------------------

?slice_sample()



## ----------------------------------------------------------------------

GW_random_ten <- starwars %>%
  slice_sample(n= 10)




## ----------------------------------------------------------------------

tail_5_GW_masa <- starwars %>%
  slice_min(n = 5, mass)



## ----------------------------------------------------------------------

x <- slice_max(starwars, n= 7, height) %>%
  select(name, height)



## ----------------------------------------------------------------------

y <- starwars %>%
  filter(!is.na(mass)) %>%
  slice_max(n= 7, height) %>%
  select(name, height)



## ----------------------------------------------------------------------



## ----------------------------------------------------------------------

złączona <-  inner_join(x, y)


