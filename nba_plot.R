library(tidyverse)
library(scales)


df19 <- readxl::read_excel('data/nba_data.xlsx', sheet='2018-2019')
df20 <- readxl::read_excel('data/nba_data.xlsx', sheet='2019-2020')

num_teams <- nrow(df19)
# Check
num_teams == nrow(df20)

team_rank <- function(x)factor(x, levels = num_teams:1)

dat <- 
  df19 %>%
  select(Team, PCT) %>%
  rename(PCT19=PCT) %>%
  mutate(rank19 = 1:num_teams)%>%
  full_join(df20 %>%
              select(Team, PCT, AKA) %>%
              mutate(rank20 = 1:num_teams)%>%
              rename(PCT20=PCT), by="Team") %>%
  mutate(rank19 = team_rank(rank19),
        rank20 = factor(rank20),
        is_gsw = AKA == 'GSW',
        diff_pct = PCT20 - PCT19)



dat %>%
  ggplot()+
  geom_segment(aes(x=PCT19, xend=PCT20, 
                   y=reorder(AKA, diff_pct), yend=reorder(AKA, diff_pct)))+
  geom_point(aes(x=PCT19, y=AKA, col='2019'))+
  geom_point(aes(x=PCT20, y=AKA, col='2020'))+
  scale_x_continuous(labels = percent)+
  labs(title='NBA Seasons: 2019 vs 2020',
       x='Percentage of Games Won', y='', 
       col='Year')+
  theme(panel.background = element_blank(),
        legend.background = element_blank())
  

dat %>%
  ggplot()+
  geom_segment(arrow = arrow(length=unit(0.20,"cm"), ends="last", type = "closed"),
               aes(x=PCT19, xend=PCT20, 
                   y=reorder(AKA, diff_pct), yend=reorder(AKA, diff_pct)))+
  geom_point(aes(x=PCT19, y=AKA))+
  #geom_point(aes(x=PCT20, y=AKA, col='2020'))+
  scale_x_continuous(labels = percent, position = 'top')+
  labs(title='Percentage of Games Won from NBA Seasons 2019 to 2020',
       x='', y='', 
       col='Year')+
  theme(panel.background = element_blank())




cpt <- "Golden State's Fall from 2019 to 2020 was steep, but there's nowhere to go but up"
plt <-
  dat %>%
  ggplot()+
  geom_segment(aes(x=1, xend=2, y=rank19, yend=rank20,  col=is_gsw)) +
  xlim(0.5, 2.5)+
  geom_text(label=dat$AKA, x=rep(2.2, num_teams), y=dat$rank20, hjust=1.1, size=3.5)+
  geom_text(label=dat$AKA, x=rep(0.9, num_teams), y=dat$rank19, hjust=1.1, size=3.5)+
  geom_point(aes(x=rep(1, num_teams), y=rank19, alpha=0.1, fill=is_gsw, col=is_gsw))+
  geom_point(aes(x=rep(2, num_teams), y=rank20, alpha=0.1, fill=is_gsw, col=is_gsw))+
  geom_segment(data=filter(dat, is_gsw), 
               aes(x=1, xend=2, y=rank19, yend=rank20),
               arrow = arrow(length=unit(0.10,"cm"), ends="last", type = "closed"),
               col=I('#0000ff'))+
  scale_color_manual(values = c('#cccccc', '#0000ff'))+
  scale_fill_manual(values = c('#cccccc', '#0000ff'))+
  labs(x='', y='', title='NBA Team Rankings',
        caption = cpt)+
  theme(panel.background = element_blank(), 
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        legend.position = 'none',
        plot.caption =  element_text(hjust = 0.5, size = 9, face='bold'),
        plot.title =  element_text(hjust=0.5, size = 18, face='bold'))

plt


 
plt +
  geom_text(x=0.6, y=team_rank(15), label="2019", fontface='bold',
            col='black')+
  geom_text(x=2.3, y=team_rank(15), label="2020", fontface='bold',
            col='black')+
  theme(axis.text.y = element_blank())

