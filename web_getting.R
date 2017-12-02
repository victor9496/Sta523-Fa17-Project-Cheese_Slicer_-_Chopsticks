library(rvest)
library(dplyr)
library(magrittr)
library(stringr)
library(purrr)

base_url0 = "https://www.apartmentratings.com"

base_url1 = "https://www.apartmentratings.com/nc/durham/"
base_url2 = "https://www.apartmentratings.com/nc/durham/page=1/"
base_url3 = "https://www.apartmentratings.com/nc/durham/page=2/"
base_url4 = "https://www.apartmentratings.com/nc/durham/page=3/"
base_url5 = "https://www.apartmentratings.com/nc/durham/page=4/"
base_url6 = "https://www.apartmentratings.com/nc/durham/page=5/"
base_url = c(base_url1, base_url2, base_url3, base_url4, base_url5, base_url6)

apt_url = c()

for (i in 1:6) {
  apt_url0 = base_url[i] %>% read_html() %>%
    html_nodes('a[class="communityLink"]') %>% html_attr("href")
  apt_url = c(apt_url, apt_url0)
}

urls0 = apt_url %>% unlist() %>% unique() %>% paste0(base_url0, .)
url = urls0[-which(urls0 %in% "")]