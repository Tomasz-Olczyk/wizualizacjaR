library(tidyverse)


panteon <- read_csv("https://raw.githubusercontent.com/Tomasz-Olczyk/wizualizacjaR/main/podstawy/panteon_s.csv")


kob_polskie <- filter(panteon, gender == "Female" & countryCode3 == "POL")

ggplot(kob_polskie) +
  geom_col(aes(x = reorder(name, -AverageViews), y = AverageViews)) +
  coord_flip()


gw_100_200 <- filter(gw, wzrost > 100 & wzrost < 200)

sw_100_200 <- filter(starwars, height > 100, height <200)


# operator pipe %>% potoki


kobiety_po_1900 <- panteon %>%
  filter(gender == "Female" & birthyear > 1900)

kobiety_1900_cont_domain <- panteon %>%
  filter(gender == "Female" & birthyear > 1900) %>%
  group_by(continentName, domain) %>%
  summarize(sławne = n())


panteon %>% 
  filter(gender == "Male" & birthyear > 1950) %>%
  group_by(continentName) %>%
  summarize(sławne = n()) %>%
  ggplot() +
  geom_col(aes(x= reorder(continentName, sławne), y = sławne)) +
  coord_flip()
  



panteon %>% 
  filter (gender == "Female" & birthyear > 1950 ) %>%
  group_by(continentName, domain) %>%
  summarize(sławne = n()) %>%
  ggplot() +
  geom_col(aes(x = continentName, y = sławne)) +
  facet_wrap(~domain)

# nowa kolumna mutate

# sumujemy liczbę osób sławnych urodzonych po 1990 według kontynentu


kontynenty <- panteon %>%
  filter(birthyear > 1990) %>%
  group_by(continentName) %>%
  summarise(sławni = n())


kontynenty_proc <- kontynenty %>%
  mutate(procent = sławni/sum(kontynenty$sławni)*100)

ggplot(kontynenty_proc) +
  geom_col(aes(x = continentName, y = procent))


# wartości brakujące

view(starwars)


wzrost_bez_wartości_brakujących <- starwars %>%
  filter(!is.na(height))

bez_na_wzrost <- drop_na(starwars, height)

zielonoskórzy <- starwars %>%
  filter(skin_color == "green")

zielonkawi <- starwars %>%
  filter(str_detect(skin_color, "green"))


# weźmy panteon i znajdźmy wszystki osoby, które mają Mari w name
# ile jest kobiet a ile mężczyzn z takim ciągiem znaków w name


Mari <- panteon %>%
  filter(str_detect(name, "Mari")) %>%
  group_by(płeć = gender) %>%
  summarise(liczba = n())


Marie_n <- panteon %>%
  filter(str_detect(name, "Mari")) 

lata <- c(1914, 1918, 1939, 1945)

RON <- c("POL", "LIT", "UKR", "BLR")

#%in%
  
  
urodzone_w_wojenne <- panteon %>%
  filter(birthyear %in% lata)

sławni_ron <- panteon %>%
  filter(countryCode3 %in% RON)

sławni_ron2 <- panteon %>%
  filter(countryCode3 %in% c("POL", "LIT", "UKR", "BLR"))


?slice_sample

d

tysiąc_sławnych <- panteon %>%
  slice_sample(n=1000)


slice_max

# themes
#install.packages("ggthemes")

library(ggthemes)


theme_set(ggthemes::theme_wsj())


ggplot(mpg) +
  geom_point(aes(x = hwy, y = cty)) 

ggplot(kob_polskie) +
  geom_point(aes(x = L_star, y = HPI))


wykres1 <- ggplot(kob_polskie) +
  geom_point(aes(x = L_star, 
                 y = HPI, 
                 size = AverageViews))

wykres1 + theme(legend.position = "bottom",
                legend.text = element_text(angle = 45),
                legend.title = element_text(angle = 45, 
                                            size = 12,
                                            colour = "salmon",
                                            family = "Arial",
                                            face = "italic"))


  
