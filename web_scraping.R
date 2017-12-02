library(rvest)
library(magrittr)
library(stringr)




site_url1 = "https://www.apartmentratings.com/nc/durham/heights-at-meridian_9199332346275157194/"

review_count = site_url1 %>%
  read_html() %>%
  html_nodes('script[type="application/ld+json"]') %>% 
  html_text() %>% 
  str_extract_all(., "reviewRating") %>% 
  unlist() %>% 
  length()


review_score = site_url1 %>%
  read_html() %>%
  html_nodes('script[type="application/ld+json"]') %>% 
  html_text()




