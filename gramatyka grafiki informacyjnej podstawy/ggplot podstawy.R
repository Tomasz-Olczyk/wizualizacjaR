## ----setup, include=FALSE------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ------------------------------------------------------------------------------------------------------

library(tidyverse)



## ------------------------------------------------------------------------------------------------------
ggplot2::mpg


## ------------------------------------------------------------------------------------------------------
?mpg  



## ------------------------------------------------------------------------------------------------------
ggplot(data = mpg)



## ------------------------------------------------------------------------------------------------------
ggplot(mpg) +
  geom_point(aes(x = displ, y = hwy))


## ------------------------------------------------------------------------------------------------------
ggplot(mpg) +
  geom_point(aes(x = displ, y = hwy, alpha = class))



## ------------------------------------------------------------------------------------------------------

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, size = class))



## ------------------------------------------------------------------------------------------------------

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy,  alpha = class))



## ------------------------------------------------------------------------------------------------------

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, shape = class))



## ------------------------------------------------------------------------------------------------------


ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, 
                           y = hwy, 
                           alpha = 0.3,
                           color = "blue"))



## ------------------------------------------------------------------------------------------------------

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, 
                           y = hwy), 
             alpha = 0.3,
             color = "blue")



## ------------------------------------------------------------------------------------------------------
 
ggplot(data = mpg,  mapping = aes(x = displ, y = hwy)) + 
  geom_point() +
  geom_smooth()



## ------------------------------------------------------------------------------------------------------
ggplot(faithful, 
       aes(x= eruptions, y = waiting)) +
  geom_density2d() +
  geom_point()


## ------------------------------------------------------------------------------------------------------
ggplot() + 
  geom_point(data = mpg,  mapping = aes(x = displ, y = hwy)) +
  geom_smooth()


## ------------------------------------------------------------------------------------------------------


ggplot(faithful, 
       aes(x= eruptions, y = waiting)) +
  geom_point() +
  geom_density2d()


## ------------------------------------------------------------------------------------------------------

ggplot(data = faithful) +
  geom_histogram(aes(x = eruptions))



## ----eval = FALSE--------------------------------------------------------------------------------------
## 
## 
## geom_point() understands the following aesthetics (required aesthetics are in bold):
## 
## x
## 
## y
## 
## alpha
## 
## colour
## 
## fill
## 
## group
## 
## shape
## 
## size
## 
## stroke
## 
## 
## 


## ------------------------------------------------------------------------------------------------------
bar <- ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = cut), 
           show.legend = FALSE, 
           width = 1 ) + 
  theme(aspect.ratio = 1) + 
  labs(x = NULL, y = NULL) 


## ------------------------------------------------------------------------------------------------------
bar


## ------------------------------------------------------------------------------------------------------
bar + coord_flip() 


## ------------------------------------------------------------------------------------------------------
bar + coord_polar()


## ------------------------------------------------------------------------------------------------------
ggplot( map_data("world", region = 'Poland'), aes(long, lat, group = group)) + geom_polygon(fill = "white", color = "black")



## ------------------------------------------------------------------------------------------------------
ggplot( map_data("world", region = 'Poland'), aes(long, lat, group = group)) + geom_polygon(fill = "white", color = "black") + coord_quickmap()


## ------------------------------------------------------------------------------------------------------

ggplot( map_data("world", region = 'Russia'), aes(long, lat, group = group)) + geom_polygon(fill = "white", color = "black")



## ------------------------------------------------------------------------------------------------------
ggplot(map_data("world", region = 'Russia'), aes(long, lat, group = group)) + geom_polygon(fill = "white", color = "black") +
  coord_quickmap()


## ------------------------------------------------------------------------------------------------------

ggplot(data = mpg) + 
  geom_point( mapping = aes(x = displ, y = hwy, color = class)) +
  scale_color_brewer(palette ="Set3") # zmienia z domyślnego koloru na wybraną skalę  paletę 


## ------------------------------------------------------------------------------------------------------



## ------------------------------------------------------------------------------------------------------

ggplot(mpg) + 
  geom_point(aes(x = displ, y = hwy, color = class)) +
  scale_color_brewer(palette ="BrBG") +
  scale_x_continuous(breaks = (c(2, 5.5, 7))) # ustalamy własne przedziały tworząc wektor liczb funkcją c()


## ------------------------------------------------------------------------------------------------------

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, colour = class))



## ------------------------------------------------------------------------------------------------------

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_wrap(~ class)



## ------------------------------------------------------------------------------------------------------

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, 
                           y = hwy))  +
  facet_grid(cyl ~ class){r}



## ------------------------------------------------------------------------------------------------------

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  labs(title = "Wydajność na autostradzie a pojemność",
       caption = "źródło: dane mpg",
       x = "pojemność", 
       y = "wydajność na autostradzie",
       colour = "klasa")



## ------------------------------------------------------------------------------------------------------

#install.packages("ggthemes")
library(ggthemes)



## ------------------------------------------------------------------------------------------------------
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  labs(title = "Wydajność na autostradzie a pojemność",
       caption = "źródło: dane mpg",
       x = "pojemność", 
       y = "wydajność na autostradzie",
       colour = "klasa") + # theme jest kolejną warstwą którą dodajemy plusem
       theme_economist()

