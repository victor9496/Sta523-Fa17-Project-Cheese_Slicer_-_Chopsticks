library(rvest)
library(magrittr)
library(stringr)
library(purrr)
library(geosphere)
library(dplyr)


site = "example.htm"

#total count
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
  unlist() %>% 
  t()

#lon and lat
lon_lat = site %>%
  read_html() %>%
  html_text() %>% 
  str_extract_all("\\[\\'longitude\\'\\] = '-\\d+\\.\\d+|\\[\\'latitude\\'\\] = '\\d+\\.\\d+") %>%  
  str_extract_all("-?\\d+\\.\\d+") %>% 
  unlist() %>% 
  as.numeric()

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
  html_text()

#image 
image_url = site %>%
  read_html() %>%
  html_nodes('.gallery-image') %>% 
  html_attrs() %>% 
#only include first image
  .[[1]] %>% 
  .[str_detect(.,"^/.*g$")] %>% 
  paste0("http:", .)

#distance to chapel
chapel = c(-78.9424706, 36.0018988)
distance = distm(lon_lat, chapel, fun = distHaversine)[1]

df.int = as.data.frame(cbind(apt_name, review_score, distance, image_url))

df.final = df.int %>% 
  slice(rep(1:n(), length(floor_plan))) %>% 
  cbind(floor_plan, rent)

colnames(df.final) = c("name", "count", "score", "image", "plan", "rent")

#different score

# dif_score = site %>%
#   read_html() %>%
#   html_nodes('#content_PropertyBreakdown .score') %>% 
#   html_text() 


