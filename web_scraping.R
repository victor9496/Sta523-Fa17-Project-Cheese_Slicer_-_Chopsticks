library(rvest)
library(magrittr)
library(stringr)




site_url1 = "https://www.apartmentratings.com/nc/durham/heights-at-meridian_9199332346275157194/"
site_url1 = "https://www.apartmentratings.com/nc/durham/heights-at-meridian_9199332346275157194/"



site = "example.htm"

#total cout
review_count = site %>%
  read_html() %>%
  html_nodes('script[type="application/ld+json"]') %>% 
  html_text() %>% 
  str_extract_all(., "reviewRating") %>% 
  unlist() %>% 
  length()

#count and average score
review_score = site %>%
  read_html() %>%
  html_nodes('script[type="application/ld+json"]') %>% 
  html_text() %>% 
  str_extract_all('"reviewCount": "\\d+|"ratingValue": "\\d\\.\\d') %>% 
  str_extract_all("\\d\\.\\d|\\d+") %>% 
  unlist()

#lon and lat
lon_lat = site %>%
  read_html() %>%
  html_text() %>% 
  str_extract_all("\\[\\'longitude\\'\\] = '-\\d+\\.\\d+|\\[\\'latitude\\'\\] = '\\d+\\.\\d+") %>%  
  str_extract_all("-?\\d+\\.\\d+") %>% 
  unlist()

#apartment name
apt_name = site %>%
  read_html() %>%
  html_nodes('.last span') %>% 
  html_text() 



#floor plan
floor_plan = site %>%
  read_html() %>%
  html_nodes('a[href="#floorplans"]') %>% 
  html_text() %>% 
  .[str_detect(., "\\d")]

#rent
rent = site %>%
  read_html() %>%
  html_nodes('div[class="floor-detail-row-rent"]') %>% 
  html_text() %>% 



#different score

# dif_score = site %>%
#   read_html() %>%
#   html_nodes('#content_PropertyBreakdown .score') %>% 
#   html_text() 


