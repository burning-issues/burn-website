###############################################################################
#Text Analysis of a Research Article
###############################################################################

# Setting up libraries
require(librarian)
librarian::shelf(dplyr, tidytext, tidyverse,stringr,stringi,
                 widyr,igraph, ggraph,
                 wordcloud, reshape2, graphlayouts,
                 pluralize, quanteda, qgraph, cowplot, readr, pdftools)

# Downloading the file
download.file("https://www.nature.com/articles/s41467-021-22747-3.pdf", 
              destfile = "~/GitHub/burn_iss-website/assets/wq_burning1.pdf", mode = "wb")

# Reading our file
ppr <- pdf_text("assets/wq_burning1.pdf")
info <- pdf_info("assets/wq_burning1.pdf")
ppr_txt <- data.frame(line = 1:8, text = ppr)

a <- as.vector(unlist(strsplit(info$keys$Subject," ")))

ppr_cln %>% ppr_txt %>% 
  sub(paste(a, collapse = '|'),'',ppr_txt$text)


ppr_txt$text <- sub(paste(a, collapse = '|'),'',ppr_txt)


ppr_cln <- ppr_txt %>% 
  unnest_tokens(output = word, input = text)


subject <- info$keys$Subject
domain <- info$keys$`CrossMarkDomains[1]`
creator <- info$keys$Creator
doi <- info$keys$doi
cdomain <- info$keys$`CrossMarkDomains[2]`

a <- as.vector(unlist(strsplit(info$keys$Subject," ")))
b <- strsplit(info$keys$Subject," ")


# Cleanning up the file
ppr_txt1 <- ppr_txt %>% 
  filter(str_remove_all(text,paste(b,collapse = "|")))
 
ppr_txt2 <- ppr_txt %>%
  filter(str_detect(text,paste(b,collapse = "|")))


ppr_cln <- ppr_txt %>% 
  # unnest_tokens(output = word, input = text) %>% 
  filter(str_detect(ppr_txt,paste(a,collapse = "|")),negate = TRUE)
%>% 
  filter(str_detect(word,paste(domain))) %>% 
  filter(str_detect(word,paste(creator))) %>% 
  filter(str_detect(word,paste(doi))) %>% 
  filter(str_detect(word,paste(cdomain))) 
  
  
  str_replace(ppr_cln$word,paste(domain),"hey")
  
  
  anti_join(stop_words, by = "word") %>% 
  filter(str_detect(word,"[:alpha:]")) %>% 
  count(word, sort = TRUE) %>% 
  mutate(length = nchar(word)) %>% 
  select(word, n) %>% 
  mutate(rank = row_number(),
         total=sum(n),
         t_freq = n/total)

ppr_cln %>% with(wordcloud(word,n,max.words = 400))








ppr_cln <- pdftools::pdf_text(pdf = ppr) %>% 
  str_to_lower() #%>% 
  
unnest_tokens(subject,txt)
  
a <- paste(subject, collapse = ' ')  
  
  
  str_split("\n") %>% 
  unlist() %>% 
  str_to_lower() %>%
  str_replace_all("\n"," ") %>%
  str_remove_all(",") %>% 
  str_replace_all("\\s{2,}", " ") %>%
  str_replace_all("[:digit:]", " ") %>%
  str_replace_all("[:punct:]", " ") %>%
  str_trim()


ppr_txt <- data.frame(line = 1:721, text = ppr_cln) %>% 
  filter(text != "") %>% 
  filter(str_length(text)>2)
  

