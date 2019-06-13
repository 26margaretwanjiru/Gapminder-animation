
#The required packages

library(tidyverse)
library(gganimate)
library(ggthemes)
library(gapminder)
library(viridis)
library(ggrepel)
library(gapminder)

#Structure of the code
data(gapminder)

str(gapminder)

attach(gapminder)

#The code below changes big numbers from scientific notation to real numbers

options(scipen = 999)


#Let's do a simple scatter plot

ggplot(gapminder, aes(lifeExp, gdpPercap,
                size = pop, color = country))+
  geom_point()+
  theme(legend.position = "none")


##Since there are many countries, we can't label all of them
#Hence, i choose to label some of them
#I use the median, 1st quartile and 3rd quartile to give me a range of these case_when() labels

gap <- gapminder %>%
  mutate(
    annotation = case_when(
      lifeExp > 71 & gdpPercap > 20000 ~ "yes",
      lifeExp < 30 ~ "yes",
      gdpPercap < 500 ~ "yes"
    )
  ) %>%
  arrange(desc(pop)) %>%
  mutate(country = factor(country))


##And now....The animation

gap_anim <- ggplot( gap, aes(x= lifeExp, 
                                  y=gdpPercap, 
                                  size = pop, color = country)) +
  geom_point(alpha = 0.2) +
  scale_size(range = c(1.4, 19), name="Population") +
  theme_stata() +
  geom_text_repel(data= gap%>% filter(annotation=="yes"), 
                  aes(label= country), size=4 )+
  transition_states(year, transition_length = 3, state_length = 1) +
  theme(text=element_text(size=15),
        plot.caption =element_text(size=10, hjust=0.5, face="italic", color="blue"))+
  theme(legend.position = "none")+
  labs(title = 'Year : {closest_state}')+
  labs(caption  = "Data Source: R data sets_gapminder package")


animate(gap_anim, fps = 10, width = 900, height = 600)
anim_save("gap_anim.gif")


