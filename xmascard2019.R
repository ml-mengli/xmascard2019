library(tidyverse)

tibble(pos=c(0,-1,0,1),level=c(1,2,2,2)) -> data

expand.tree <- function (data) {
    data %>%
      filter(level==max(level)) %>%
        mutate(level = level+1) -> next.branch
  
    next.branch %>%
      filter(abs(pos) == max(pos)) %>%
        mutate(pos = if_else(pos>0, pos+1, pos-1)) %>%
          bind_rows(next.branch) %>%
            bind_rows(data) %>%
              return()
}

data %>% 
  expand.tree() %>% 
    expand.tree() %>% 
      expand.tree() %>% 
        expand.tree() %>%
          mutate(colour = as.factor(pos%%4 + level%%3))-> data

data %>%
  ggplot(aes(pos,level,colour=colour)) + 
  geom_polygon(data=tibble(pos=c(-5,5,0),level=c(6,6,1)),colour="green3",fill="green4") +
  geom_point(size=7, show.legend = F) + 
  geom_point(shape=8, size=4, colour="white") + 
  scale_y_reverse() +
  theme_dark() +
  ggtitle("Happy Christmas!", "from Babraham Bioinformatics") +
  theme(plot.title=element_text(hjust=0.5),plot.subtitle=element_text(hjust=0.5)) +
  xlab("www.bioinformatics.babraham.ac.uk") +
  ylab("")

ggsave("c:/Users/andrewss/Desktop/xmas.svg",device = "svg", width = 4, height = 5)
