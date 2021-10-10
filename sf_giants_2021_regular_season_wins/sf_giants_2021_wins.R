library(ggplot2)
library(gganimate)
library(gifski)
library(png)

p <- ggplot(mtcars) + 
      geom_boxplot(aes(factor(cyl), mpg)) + 
      transition_manual(gear)

animate(p)
anim_save('test.gif')

