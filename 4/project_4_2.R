library(ggplot2)
library(shiny)
library(gganimate)
library(patchwork)

heart = read.csv('heart.csv')
age <- heart[order(heart$age),]
p = ggplot(age, aes(factor(sex), chol)) + geom_boxplot() + labs(title = 'Mean cholesterole with age', x = 'Sex', y = 'Cholesterole') +
  transition_states(
    age,
    transition_length = 5,
    state_length = 1
  ) +
  
  enter_fade() + 
  exit_shrink() +
  ease_aes('sine-in-out')


anim_save("outfile.gif", animate(p)) # New


