require(librarian)
librarian::shelf(plyr, tidytext, tidyverse,
                 widyr,igraph, ggraph,
                 wordcloud, reshape2, graphlayouts,
                 pluralize, quanteda, qgraph, cowplot, readr,
                 ggwordcloud)

t_df <- as_tibble(read_csv("assets/data/wildfires_survey_all_answers.csv",show_col_types = FALSE))


aq_tokens <- t_df %>% 
  ungroup() %>% 
  unnest_tokens(output = word, input = answers, drop = FALSE)%>%
  anti_join(stop_words, by = "word")%>%
  filter(str_detect(word,"[:alpha:]"))%>%
  rowwise() %>% mutate(word = if_else(word!="data",singularize(word),"data")) %>%
  distinct() %>% 
  group_by(question) %>% 
  count(word, sort = FALSE) %>% 
  mutate(length = nchar(word)) 

head(aq_tokens)
summary(aq_tokens)

# Using ggwordclouds:
# https://cran.r-project.org/web/packages/ggwordcloud/vignettes/ggwordcloud.html

p <- ggplot(filter(aq_tokens,n>2), 
            aes(label = word, 
                size = n, 
                color = question)) +
  geom_text_wordcloud(area_corr_power = 1) +
  scale_radius(range = c(0, 20),
               limits = c(0, NA)) +
  facet_wrap(~question) 
p

