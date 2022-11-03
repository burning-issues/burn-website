###############################################################################
#Text Analysis of a Research Article
###############################################################################

# Setting up libraries
require(librarian)
librarian::shelf(dplyr, tidytext, tidyverse,
                 widyr,igraph, ggraph,
                 wordcloud, reshape2, graphlayouts,
                 pluralize, quanteda, qgraph, cowplot, readr, pdftools)

# Downloading the file
download.file("https://www.nature.com/articles/s41467-021-22747-3.pdf", 
              destfile = "~/GitHub/burn_iss-website/assets/wq_burning1.pdf", mode = "wb")

# Reading our file
ppr <- pdf_text("assets/wq_burning1.pdf")

# Cleanning up the file

ppr_cln <- ppr %>% 
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
  

