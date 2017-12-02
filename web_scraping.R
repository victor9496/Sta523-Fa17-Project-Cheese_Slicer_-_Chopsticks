library(rvest)
library(magrittr)
library(stringr)
library(purrr)
library(geosphere)
library(dplyr)


site = "sample2.htm"

apartment_finder = function(site) {

                    extract_info = function(node) {
                      site %>% 
                        read_html() %>% 
                        html_node(node) %>% 
                        html_text()
                    }
#total count
review_count = extract_info('script[type="application/ld+json"]') %>% 
               str_extract_all(., "reviewRating") %>% 
               unlist() %>% 
               length() %>% 
               as.numeric()

#count and average score
review_score = extract_info('script[type="application/ld+json"]') %>% 
  str_extract_all('"reviewCount": "\\d+|"ratingValue": "\\d\\.\\d') %>% 
  str_extract_all("\\d\\.\\d|\\d+") %>% 
  unlist() %>% 
  as.numeric() %>% 
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
apt_name = extract_info('.last span')

#floor plan(change)
floor_plan = site %>%
  read_html() %>%
  html_nodes('h3[class="link1"]') %>% 
  html_text()

#rent
rent = site %>%
  read_html() %>%
  html_nodes('div[class="floor-detail-row-rent"]') %>% 
  html_text() %>% 
  as.numeric()

#image 
image_url = site %>%
  read_html() %>%
  html_nodes('.gallery-image') %>% 
  html_attrs() %>% 
  unlist() %>% 
  str_extract("^/.*g$") %>% 
  .[!is.na(.)] %>% 
#only need the first image
  .[1] %>% 
  paste0("http:", .)

#distance to chapel
chapel = c(-78.9424706, 36.0018988)
distance = round(distm(lon_lat, chapel, fun = distHaversine)[1]) %>% 
  as.numeric()

#calculate average floor size
floor = site %>%
  read_html() %>%
  html_nodes('#floorplans .widget') %>% 
  html_text()

split_floor = unlist(str_split(floor, "\\d Bedrooms, \\d Bathroom"), recursive = FALSE)

num_floor = unlist(lapply(split_floor, 
                     function(i) str_extract_all(str_replace_all(i, ",", ""), "\\d{3,}")), 
              recursive = FALSE)

floor_mean = unlist(lapply(num_floor, function(i) mean(as.numeric(i))))
floor_mean_clean = floor_mean[!is.nan(floor_mean)]

#generate final dataframe
df.final =  as.data.frame(cbind(apt_name, image_url), 
                          stringsAsFactors = FALSE) %>% 
  slice(rep(1:n(), length(floor_plan))) %>% 
  cbind(floor_plan, rent, floor_mean_clean, review_score, distance)

colnames(df.final) = c("name", "image", "plan", "rent", 
                       "size", "review_count", "avg_review","distance")

#different score

# dif_score = site %>%
#   read_html() %>%
#   html_nodes('#content_PropertyBreakdown .score') %>% 
#   html_text() 

return(df.final)

}





image_url = site %>%
  read_html() %>%
  html_nodes('.gallery-image') %>% 
  html_attrs() %>% 
  #only include first image
  .[[2]] %>% 
  .[str_detect(.,"^/.*g$")] %>% 
  paste0("http:", .)




image_url = site %>%
  read_html() %>%
  html_nodes('.gallery-image') %>% 
  html_attrs() %>% 
  unlist() %>% 
  str_extract("^/.*g$") %>% 
  .[!is.na(.)] %>% 
  .[1] %>% 
  paste0("http:", .)

