## ----setup, include=FALSE----------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----------------------------------------------------------
library(tidyverse)


## ----------------------------------------------------------
ToothGrowth$dose <- as.factor(ToothGrowth$dose)

p <- ggplot(ToothGrowth, aes(x=dose, y=len, fill=dose))+
  geom_boxplot()


## ----------------------------------------------------------
p + theme(legend.position="bottom")


## ----------------------------------------------------------
p + theme(legend.position="none")


## ----------------------------------------------------------
levels(iris$Species)


## ----------------------------------------------------------
iris$Species <- factor(iris$Species, levels = rev(levels(iris$Species)))



## ----------------------------------------------------------
iris$Species <- factor(iris$Species, levels = c("virginica", "versicolor", "setosa"))


## ----------------------------------------------------------
(p1 <- ggplot(iris, aes(x = Species, y = Sepal.Length)) +
  geom_boxplot(aes(color = Species)) +
  scale_color_manual(values = c("#E7B800", "#2E9FDF", "#FC4E07")))


## ----------------------------------------------------------
p1 +
  guides(color = guide_legend(reverse = TRUE)) # 


## ----------------------------------------------------------

ggplot(iris, aes(x = Species, y = Sepal.Length)) +
  geom_boxplot(aes(color = Species)) +
  scale_color_manual(values = c("#E7B800", "#2E9FDF", "#FC4E07"), guide = guide_legend(reverse=TRUE))



## ----------------------------------------------------------

ggplot(iris, aes(x = Species, y = Sepal.Length)) +
  geom_boxplot(aes(color = Species)) +
  scale_color_manual(values = c("#E7B800", "#2E9FDF", "#FC4E07")) +
  guides(color = guide_legend(reverse=TRUE))



## ----------------------------------------------------------


ggplot(iris, aes(x = Species, y = Sepal.Length)) +
  geom_boxplot(aes(color = Species)) +
  scale_color_manual(values = c("#E7B800", "#2E9FDF", "#FC4E07")) +
  guides(color = guide_legend(reverse=TRUE))



## ----------------------------------------------------------
# usuwa leendę kształu
p + scale_shape(guide="none") 
# usuwa legendę rozmiaru
p + scale_size(guide="none") 



## ----------------------------------------------------------

# usuwa legendę koloru

p + scale_fill_manual(values=c('#999999','#E69F00','#56B4E9'), 
                       guide = "none") #


## ---- eval = FALSE-----------------------------------------
## 
##  p + guides(guides(fill = guide_legend(reverse = TRUE,
##                                     title.position = "top",
##                                     label.position = "bottom",
##                                     keywidth = 1,
##                                     nrow = 1)))
## 


## ----------------------------------------------------------
panteon %>%
  ggplot() +
  geom_boxplot(aes(x = gender, y = AverageViews))



## ----------------------------------------------------------
panteon %>%
  ggplot() +
  geom_boxplot(aes(x = gender, y = AverageViews)) +
  scale_y_log10()



## ----------------------------------------------------------
options(scipen =999)


## ----------------------------------------------------------

panteon %>%
  ggplot() +
  geom_boxplot(aes(x = gender, y = AverageViews)) +
  scale_y_log10(breaks = c(0, 100000, 1000000), labels = c("", "100 tys.", "1 mln")) +
  scale_x_discrete(labels = c("kobiety", "mężczyźni")) +
  labs(y = "średnia wyświetleń",
       x = "",
       title = "Biografie kobiet są średnio częściej wyświetlane niż biografie mężczyzn",
       subtitle = "Rozkład średniej wyświetleń biografii postaci z Panteon 1.0")



## ----------------------------------------------------------
w1 <- panteon %>%
  ggplot() +
  geom_boxplot(aes(x = gender, y = AverageViews)) +
  scale_y_log10(breaks = c(0, 100000, 1000000), labels = c("", "100 tys.", "1 mln")) +
  scale_x_discrete(labels = c("kobiety", "mężczyźni")) +
  labs(y = "średnia wyświetleń",
       x = "",
       title = "Biografie kobiet są średnio częściej wyświetlane niż biografie mężczyzn",
       subtitle = "Rozkład średniej wyświetleń biografii postaci z Panteon 1.0")



## ----------------------------------------------------------

ggplot(mpg, aes(x = hwy, y = cty)) +
  geom_point() +
  geom_text(aes(label = manufacturer))


## ----------------------------------------------------------
panteon %>%
  filter(countryCode3 == "POL" & gender == "Female") %>%
  ggplot(aes(x = L_star, y = HPI)) +
  geom_point(aes(size = AverageViews)) +
  geom_text(aes(label = name), hjust = 0)


## ----------------------------------------------------------
#install.packags("ggrepel")
library(ggrepel)


## ----------------------------------------------------------

panteon %>%
  filter(countryCode3 == "POL" & gender == "Female") %>%
  ggplot(aes(x = L_star, y = HPI)) +
  geom_point(aes(size = AverageViews)) +
  geom_text_repel(aes(label = name))



## ----------------------------------------------------------
panteon %>%
  filter(countryCode3 == "POL" & gender == "Female") %>%
  ggplot(aes(x = L_star, y = HPI)) +
  geom_point(aes(size = AverageViews)) +
  geom_text_repel(aes(label = name)) +
  annotate(geom = "text", x=8, y=30, label = "Jedną z bardziej znanych 'Polek' \n okazuje się Katarzyna II",)


## ----------------------------------------------------------
panteon %>%
  filter(countryCode3 == "POL" & gender == "Female") %>%
  ggplot(aes(x = L_star, y = HPI)) +
  geom_point(aes(size = AverageViews)) +
  geom_text_repel(aes(label = name)) +
  annotate(geom = "text", x=8, y=30, label = "Jedną z bardziej znanych 'Polek' \n okazuje się Katarzyna II",) +
  
  annotate(geom = "rect", xmin = 7.5, xmax = 11, ymin = 27, ymax = 29, fill = "red", alpha = 0.2)


## ----------------------------------------------------------
polki <- panteon %>%
  filter(countryCode3 == "POL" & gender == "Female")


## ----------------------------------------------------------
ggplot(polki, aes(x = L_star, y = HPI)) +
  geom_point(aes(size = AverageViews)) +
  geom_text_repel(data = polki %>% filter(AverageViews > mean(polki$AverageViews)), aes(label = name), nudge_y = 1.5) +
  annotate(geom = "text", x=8, y=30, label = "Jedną z bardziej znanych 'Polek' \n okazuje się Katarzyna II",) +
  
  annotate(geom = "rect", xmin = 7.5, xmax = 11, ymin = 27, ymax = 29, fill = "red", alpha = 0.2) +
  guides(size = "none")


## ----------------------------------------------------------
panteon %>%
  filter(countryCode3 == "POL" & gender == "Female") %>%
  ggplot(aes(x = L_star, y = HPI)) +
  geom_point(aes(size = AverageViews)) +
  geom_text_repel(aes(label = name)) +
  geom_vline(xintercept = mean(panteon$L_star)) +
  geom_hline(yintercept = mean(panteon$HPI))


## ----message=TRUE------------------------------------------
?rgb()


## ----------------------------------------------------------

kolor1 <- rgb(144, 26, 40, maxColorValue = 255) # domyślnie jest na skali intensywności 0-1

kolor2 <- rgb(200, 131, 84, maxColorValue = 255)
  
print(kolor1)
print(kolor2)


## ----------------------------------------------------------
ggplot(mtcars, aes(x=drat)) +
  geom_density(color= kolor2, fill=
kolor1, linewidth=2 ) 



## ----------------------------------------------------------

ggplot(mtcars, aes(x=drat)) + 
  geom_density( color = "#C88354", 
                fill="#901A28", 
                linewidth=2 )



## ----------------------------------------------------------
ggplot(mtcars, aes(x=drat)) + 
  geom_density(color=(rgb(200, 131, 84, maxColorValue = 255)), 
               fill= (rgb(63, 74, 84, maxColorValue = 255)), linewidth=2 )


## ---- fig.dim= c(8, 8)-------------------------------------

plot(0, 0, type = "n", xlim = c(0, 1), ylim = c(0, 1), axes = FALSE, xlab = "", ylab = "")
# Settings
line <- 25
col <- 5

# Add color background
rect(  
  rep((0:(col - 1)/col),line) ,  
  sort(rep((0:(line - 1)/line),col),decreasing=T),   
  rep((1:col/col),line) , 
  sort(rep((1:line/line),col),decreasing=T),  
  border = "white" , 
  col=colors()[seq(1,line*col)])

# Color names
text(  
  rep((0:(col - 1)/col),line)+0.1 ,  
  sort(rep((0:(line - 1)/line),col),decreasing=T)+0.015 , 
  colors()[seq(1,line*col)]  , 
  cex=0.6)



## ----------------------------------------------------------
ggplot(mtcars, aes(x=drat)) + 
  geom_density(color= "darkorange1", 
               fill= "darkslategray",
               linewidth=2 )


## ----------------------------------------------------------
kolor3  = colors()[143] 


## ---- numery-kolorów---------------------------------------


#numery kolor

par(mar=c(0,0,0,0))
plot(0, 0, type = "n", xlim = c(0, 1), ylim = c(0, 1), axes = FALSE, xlab = "", ylab = "")

# Parameters
line <- 31
col <- 21

# Rectangles
rect( rep((0:(col - 1)/col),line) ,  sort(rep((0:(line - 1)/line),col),decreasing=T) , rep((1:col/col),line) , sort(rep((1:line/line),col),decreasing=T),  
      border = "light gray" , col=colors()[seq(1,651)])

# Text
text( rep((0:(col - 1)/col),line)+0.02 ,  sort(rep((0:(line - 1)/line),col),decreasing=T)+0.01 , seq(1,651)  , cex=0.5)



## ----------------------------------------------------------
ggplot(mtcars, aes(x=drat)) + 
  geom_density(color= colors()[53],
               fill= colors()[593],
               linewidth=2 )



## ----------------------------------------------------------

library(RColorBrewer) 
par(mfrow=c(1,1))



## ----fig.dim= c(12, 12)------------------------------------

display.brewer.all()


## ----fig.dim= c(12, 12)------------------------------------
display.brewer.all(colorblindFriendly = TRUE)


## ----------------------------------------------------------
#install.packages("viridis")
library(viridis)


## ----eval = FALSE------------------------------------------
## scale_fill_viridis(discrete = TRUE)


## ----------------------------------------------------------
glimpse(ToothGrowth)


## ----------------------------------------------------------
ToothGrowth$dose <- as.factor(ToothGrowth$dose)
mtcars$cyl <- as.factor(mtcars$cyl)

#boxplot
bp <- ggplot(ToothGrowth, aes(x=dose, y=len))

# scatter plot

sp <- ggplot(mtcars, aes(x=wt, y=mpg))


## ----------------------------------------------------------
bp + geom_boxplot(fill = "steelblue", color = "red")



## ----------------------------------------------------------
sp + geom_point(color = 'darkblue')


## ----------------------------------------------------------

bp <- bp + geom_boxplot(aes(fill = dose)) 



## ----------------------------------------------------------
sp <- sp + geom_point(aes(color = cyl))



## ----------------------------------------------------------
bp + scale_fill_hue(l=40, c=35) 



## ----------------------------------------------------------
# Scatter plot

sp + scale_color_hue(l=40, c=35) 


## ----------------------------------------------------------
bp + scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))



## ----------------------------------------------------------
sp + scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))


## ----------------------------------------------------------
sp + scale_color_brewer(palette="Dark2")



## ----------------------------------------------------------

#install.packages("wesanderson") 

library(wesanderson)



## ----------------------------------------------------------
bp + scale_fill_manual(values=wes_palette(n=3, name="GrandBudapest1"))


## ----------------------------------------------------------
sp+scale_color_manual(values=wes_palette(n=3, name="GrandBudapest1"))



## ----------------------------------------------------------
sp2 <- ggplot(mtcars, aes(x = wt, y = mpg)) + 
  geom_point(aes(color = qsec)) 


sp2 
# Change the low and high colors


## ----------------------------------------------------------
# sekwencyjna
sp2+scale_color_gradient(low="blue", high="red")


## ----------------------------------------------------------
# Ddywergentna
mid <- mean(mtcars$qsec) 
sp2 + scale_color_gradient2(midpoint = mid, low = "blue", 
                            mid = "white", 
                            high = "red", 
                            space = "Lab" )


## ----------------------------------------------------------
sp3 <- ggplot(mtcars, aes(x = wt, y = mpg)) + 
  geom_point(aes(color = mpg)) 

sp3


## ----------------------------------------------------------

theme_set(theme_bw())



## ----------------------------------------------------------
sp3 + theme(legend.position = "bottom")


## ----------------------------------------------------------
w1 +
  theme(plot.title = element_text(size = rel(0.9)),
        plot.subtitle = element_text(size = rel(0.4)))



## ----------------------------------------------------------

w1 +
  theme(plot.title = element_text(size = rel(0.9),
                                  family = "Times",
                                  face = "bold.italic",
                                  colour = "salmon"),
        plot.subtitle = element_text(size = rel(0.4)))



