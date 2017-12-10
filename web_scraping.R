library(rvest)
library(magrittr)
library(stringr)
library(purrr)
library(geosphere)
library(dplyr)

#for test only
site = "webURLs/web24.htm"

#create function for scraping for all 145 websites
apartment_finder = function(site) {

                   #create another small function to extract test from html
                    extract_info = function(node) {
                      site %>% 
                        read_html() %>% 
                        html_node(node) %>% 
                        html_text()
                    }


#count and average score for each individual review
review_score = extract_info('script[type="application/ld+json"]') %>% 
  str_match_all('ratingValue":\\s.(\\d)"')%>% 
  .[[1]]%>% # we know that it will only return one list from each page
  .[,2]%>% # second column is the part inside bracket we need
  as.numeric()

#lon and lat for apartment site
lon_lat = site %>%
  read_html() %>%
  html_text() %>% 
  str_extract_all("\\[\\'longitude\\'\\] = '-\\d+\\.\\d+|\\[\\'latitude\\'\\] = '\\d+\\.\\d+") %>%  
  str_extract_all("-?\\d+\\.\\d+") %>% 
  unlist() %>% 
  as.numeric() %>% 
  t()
#if this information is not complete, either miss longitude or latitude, we treat them as NA for both
if(length(lon_lat) != 2) lon_lat = rep(NA,2)


#apartment name
apt_name = extract_info('.last span')

#floor plan aviable in this apartment
floor_plan = site %>%
  read_html() %>%
  html_nodes('h3[class="link1"]') %>% 
  html_text()

#rent for each floor plan
rent.raw = site %>%
  read_html() %>%
  html_nodes('#floorplans .widget') %>% 
  html_text()
  
#if differnt sizes of room for each floor plan, calculate the average rent for each floor plan
split_rent = unlist(str_split(rent.raw, "Bathroom"), recursive = FALSE) %>% 
  .[str_detect(.,"Price")]

num_rent = unlist(lapply(split_rent,
                          function(i) str_extract_all(str_replace_all(i, ",", ""), "\\$\\d{3,}")),
                   recursive = FALSE) %>% 
  lapply(., function(i) str_replace_all(i, "\\$", ""))

rent_mean = unlist(lapply(num_rent, function(i) mean(as.numeric(i)))) %>% 
  .[!is.nan(.)]

#the length of floor plan and rent not the same, we treat them as NA
#since information is not complete
if(length(rent_mean) != length(floor_plan)) {
  rent = rep(NA, length(floor_plan))
} else {
  rent = rent_mean
}



#extract url first image aviable on apartment rating website(could be NA) 
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

if(str_detect(image_url, "A$")) image_url = NA

#distance to chapel(Duke University) unit is meter by default
chapel = c(-78.9424706, 36.0018988)
#since the result return as a 1*1 matrix, we call it by [1]
distance = round(distm(lon_lat, chapel, fun = distHaversine)[1]) %>% 
  as.numeric()

#oringally look for the information for size for each floor plan (sq^2)
#but since this information is not avaible for many apartment
#decide to abandon this process
                          
# #calculate average floor size
# floor = site %>%
#   read_html() %>%
#   html_nodes('#floorplans .widget') %>% 
#   html_text()
# 
# split_floor = unlist(str_split(floor, "\\d Bathroom"), recursive = FALSE)
# 
# num_floor = unlist(lapply(split_floor,
#                           function(i) str_extract_all(str_replace_all(i, ",", ""), "\\.\\d{3,}")),
#                    recursive = FALSE) %>% 
#   lapply(., function(i) str_replace_all(i, "\\.", ""))
# 
# floor_mean = unlist(lapply(num_floor, function(i) mean(as.numeric(i)))) %>% 
#   .[!is.na(.)]
#   
# 
# if(length(floor_mean) != length(floor_plan)) {
#  floor_mean_clean = rep(NA, length(floor_plan))
# } else {
#   floor_mean_clean = floor_mean
# }

#combine all the information as list
info_list = list(apt_name, distance, rent, review_score,floor_plan)#, floor_mean_clean, floor_plan, floor,)

#use if statement to filter out NA info, like some zero length vector or NA element 
if(any(lengths(info_list) == 0 | sapply(info_list, function(i) any(is.na(i))))|length(review_score)==0) {
  df.final = NA
} else {

#generate final dataframe
df.final =  as.data.frame(cbind(apt_name, image_url), 
                          stringsAsFactors = FALSE) %>% 
  slice(rep(1:n(), length(floor_plan))) %>% 
  slice(rep(1:n(),length(review_score)))%>%
  cbind( rent, floor_plan,
        #floor_mean_clean,
        review_score, distance, lon_lat)

colnames(df.final) = c("name", "image", "rent","plan",
                       #"size", 
                       "review","distance", "lon", "lat")
}

#different score

# dif_score = site %>%
#   read_html() %>%
#   html_nodes('#content_PropertyBreakdown .score') %>% 
#   html_text() 

return(df.final)
}

#loading data for web getting process
load("urls.Rdata")
apt.df = data.frame()

for (i in seq_len(133)) {
  df = apartment_finder(paste0("webURLs/web", i, ".htm"))
  if(!is.na(df)){
    apt.df = rbind(apt.df,cbind(df,urls[i]))
  }
}

test.df = apt.df %>% 
  select(-image)

#again filter out the observation with NA inside except the image column
df.complete = apt.df[!rowSums(is.na(test.df)) > 0,]

save(df.complete, file="df_complete.Rdata")
