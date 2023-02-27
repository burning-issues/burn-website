require(librarian)
librarian::shelf(plyr, tidytext, tidyverse,
                 widyr,igraph, ggraph,
                 wordcloud, reshape2, graphlayouts,
                 pluralize, quanteda, qgraph, cowplot, readr)

t_df <- as_tibble(read_csv("assets/data/wildfires_survey_all_answers.csv",show_col_types = FALSE))


aq_tokens <- t_df %>% 
  ungroup() %>% 
  unnest_tokens(output = word, input = answers, drop = FALSE)%>%
  anti_join(stop_words, by = "word")%>%
  filter(str_detect(word,"[:alpha:]"))%>%
  rowwise() %>% mutate(word = if_else(word!="data",singularize(word),"data")) %>%
  distinct() %>% 
  count(word, sort = FALSE) %>% 
  mutate(length = nchar(word)) 

head(aq_tokens)


aq_tokens1 <- t_df %>% 
select(everything()) %>% 
  ungroup() %>% 
  mutate(text = unnest_tokens(output = word, input = answers, drop = FALSE)) %>% 
  anti_join(stop_words, by = "word")%>%
  filter(str_detect(text,"[:alpha:]"))%>%
  rowwise() %>% mutate(text = if_else(word!="data",singularize(word),"data")) %>%
  distinct() %>% 
  count(text, sort = FALSE) %>% 
  mutate(length = nchar(text)) 

head(aq_tokens1)


%>% 
  with(wordcloud(word,n, scale = c(4, .05)))
head(aq_tokens)