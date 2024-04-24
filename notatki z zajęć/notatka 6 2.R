
library(tidyverse)

panteon <- read_csv("https://raw.githubusercontent.com/Tomasz-Olczyk/wizualizacjaR/main/podstawy/panteon_s.csv")

# rgb

?rgb


kolor1 <- rgb(63, 74, 84, maxColorValue = 255)

kolor2 <- rgb(155, 133, 172, maxColorValue = 255)

print(kolor1)

ggplot(mtcars, aes(x = drat)) +
  geom_density(color = kolor1,
               fill = kolor2,
               linewidth = 2)


ggplot(mtcars, aes(x = drat)) +
  geom_density(color = "salmon",
               fill = "gray",
               linewidth = 2)


ggplot(mtcars, aes(x = drat)) +
  geom_density(color = "darkorange",
               fill = "darkslategray",
               linewidth = 2)

#install.packages("RColorBrewer")

library(RColorBrewer)


sp <- ggplot(mtcars) +
  geom_point(aes(x = qsec, y = drat, color= factor(cyl)))

             sp

sp + scale_color_brewer(palette = "Set2")


sp + scale_color_manual(values = c("darkorange", "darkblue", "black"))


display.brewer.all(colorblindFriendly = TRUE)


#install.packages("wesanderson")
library(wesanderson)

sp+ scale_color_manual(values = wes_palette(n=3, name="GrandBudapest1")) +
 theme_minimal()

?scale_color_gradient


sp2 <- ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point(aes(color = qsec))

sp2

sp2 + scale_color_gradient(low = "blue", high = "red")

?scale_color_gradient2

mid <- mean(mtcars$qsec)

sp2 + scale_color_gradient2(midpoint = mid, 
                            low = "blue",
                            high = "red")

