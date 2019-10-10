# Loading the required libraries
library(splashr)
library(rvest)
library(httr)
library(dplyr)
library(stringr)


# Scrape Brand URLs -------------------------------------------------------

## URL from Sephora 
URL = "https://www.sephora.com/brands-list"
html_brandnames = read_html(URL)
  
## Function uses the HTML and creates URL to the skincare page
## for each brand
get_brand_urls = function(list_html){
  URL_prefix = "https://www.sephora.com"
  URL_suffix = "/skincare"
  
  brand_urls = list_html %>%
    html_nodes(".css-kxa5od") %>%
    html_attr("href")
  
  brand_urls_complete = paste(URL_prefix, brand_urls, URL_suffix, sep = "")
  return(brand_urls_complete)
}

urls_skincare_brand = get_brand_urls(html_brandnames)

## Loops through all the URLs and scrapes their skincare page. 
html_skincare_pages = vector(mode = "list", length = length(urls_skincare_brand))
                               
for(i in urls_skincare_brand[1:6]){ # Update this to be all when want to scrape
  html_skincare_pages[i] = list(read_html(i))
  Sys.sleep(5) ## Adding in a delay for good scraping practices
}

html_skincare_pages[[1]] = NULL

# Scrape Brand Product Information -------------------------------------------------------

## Function that pulls item and price for each skincare item
get_item_price = function(list_html){
  item = list_html %>%
    ## Selects span elements that have the attribute data-at whose value begins with sku_item_name
    html_nodes("span[data-at^=sku_item_name]") %>% 
    html_text() %>%
    str_trim()
  
  price = list_html %>%
    ## Selects span elements that have the attribute data-at whose value begins with sku_item_price_list
    html_nodes("span[data-at^=sku_item_price_list]") %>% 
    html_text() %>%
    str_trim()
  
  return(c(item, price))
}

skincare_products_details = lapply(html_skincare_pages, get_item_price)

# Count Products -------------------------------------------------------

## Function that counts how many times a pattern appears
count_products = function(list, pattern){
  list %>%
    str_count(pattern) %>%
    sum()
}

## Regex expressions for products
## Product list: serum, toner, mask, oil cleanser
pattern = c("(S|s)erum(|s)", "(T|t)oner(|s)|(T|t)onic(|s)", "(M|m)ask(|s)|(M|m)asque(|s)",
            "(O|o)il(\\s\\w*)*(C|c)leanser(|s)")


# List that will track product counts
test_counts = vector(mode = "list", length = 1)

for(i in seq_along(pattern)){
  test_counts[[i]] = lapply(skincare_products_details, count_products, pattern[i])
}



# Creating Dataframe ------------------------------------------------------

brand_names = all_brand_pages_html %>%
  html_nodes(".css-kxa5od") %>%
  html_text() %>%
  str_trim()


df_products_info = data.frame(brand_names[1:6], stringsAsFactors = FALSE)
df_products_info$serum_counts = test_counts[[1]]
df_products_info$toner_counts = 
df_products_info$mask_counts = 
df_products_info$oil_cleanser_counts = 







# Code Graveyard ----------------------------------------------------------

# Micellar Cleanser
# Cleanser --> cleanser, wash, 
# Treatment --> treatment, solution, concentrate, spot, peel
# Moisturizer --> Cream, balm
# Lip -> lip
# SPF --> SPF






