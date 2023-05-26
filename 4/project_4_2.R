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



gg_1 <- ggplot(heart) + geom_point_interactive (aes (x = age,
                                                     y = chol,
                                                     
                                                     tooltip = age
)) 

x = girafe(ggobj = gg_1)
x <- girafe_options(x,opts_hover(css = "fill:wheat;stroke:orange;r:5pt;") )
print(x)
gg_2 <- ggplot(heart) + geom_point_interactive (aes (x = age,
                                                     y = trestbps,
                                                     color = factor(sex),
                                                     tooltip = age
)) +theme_bw()
girafe(code = print(gg_1 + gg_2), width_svg = 8, height_svg = 4) %>% girafe_options(opts_hover(css = 'fill:black'))






library(ggplot2)

dataset <- mtcars
dataset$carname = row.names(mtcars)

gg <- ggplot(
  data = heart,
  mapping = aes(x = age, y = trestbps, color = factor(sex),
                tooltip = age, data_id = cp) ) +
  geom_point_interactive() + theme_minimal()

x <- girafe(ggobj = gg)
x <- girafe_options(x,
                    opts_hover(css = "fill:wheat;stroke:orange;r:5pt;") )
if( interactive() ) print(x)
