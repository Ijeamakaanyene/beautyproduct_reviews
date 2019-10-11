# Loading the required libraries
library(splashr)
library(rvest)
library(httr)
library(dplyr)
library(stringr)
library(RSelenium)


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
urls_skincare_brand = urls_skincare_brand[1:15] # Comment out when scraping all!

## Loops through all the URLs and scrapes their skincare page. 
html_skincare_pages = vector(mode = "list", length = 0)
                               
for(i in seq_along(urls_skincare_brand)){ # Update this to be all when want to scrape
  html_skincare_pages[i] = list(read_html(i))
  Sys.sleep(5) ## Adding in a delay for good scraping practices
}

# Creates vector of all the brand names
brand_names = html_brandnames %>%
  html_nodes(".css-kxa5od") %>%
  html_text() %>%
  str_trim()

names(html_skincare_pages) = brand_names

# Scrape Brand Product Information -------------------------------------------------------

## Function that pulls item and price for each skincare item
get_item_price = function(list_html){
  item = list_html %>%
    ## Selects span elements that have the attribute data-at whose value 
    ## begins with sku_item_name
    html_nodes("span[data-at^=sku_item_name]") %>% 
    html_text() %>%
    str_trim()
  
  price = list_html %>%
    ## Selects span elements that have the attribute data-at whose value 
    ## begins with sku_item_price_list
    html_nodes("span[data-at^=sku_item_price_list]") %>% 
    html_text() %>%
    str_trim()
  
  return(c(item, price))
}

skincare_products_details = lapply(html_skincare_pages, get_item_price)

# Count Products -------------------------------------------------------

# Dataframe to store counts 
df_products_info = data.frame(brand_names[1:15], stringsAsFactors = FALSE)


## Function that counts how many times a pattern appears
count_products = function(list, pattern){
  list %>%
    str_count(pattern) %>%
    sum()
}

## Regex expressions for products
pattern = c("(S|s)erum(|s)", 
            "(T|t)oner(|s)|(T|t)onic(|s)", 
            "(M|m)ask(|s)|(M|m)asque(|s)",
            "(O|o)il(\\s\\w*)*(C|c)leanser(|s)",
            "((M|m)icellar)|(Cleans(er|ing)(\\s\\w*)*((W|w)ater))",
            "(C|c)leanser|(W|w)ash",
            "(S|s)heet (M|m)ask(|s)",
            "(V|v)itamin (C|c)",
            "(R|r)etinol",
            "(E|e)ye(\\s\\w*)*((C|c)ream|(T|t)reatment|(S|s)erum|(C|c)oncentrate|(B|b)alm|(G|g)el)",
            "(M|m)ake(U|u)p(\\s\\w*)*((R|r)emov(er|ing)|(S|s)olvent|(C|c)leans(er|ing))",
            "(S|s)unscreen|(S|s)un (S|s)creen")

products_types = c("serum", "toner", "mask", "oil_cleanser", "micella_water", "cleanser", 
                   "sheet_mask", "vitamin_c", "retinol", "eye_cream", "makeup_remover",
                   "sunscreen")

## List that will track product counts
product_counts = vector(mode = "list", length = 1)

for(i in seq_along(pattern)){
  product_counts[[i]] = lapply(skincare_products_details, count_products, pattern[i])
  df_products_info[[i+1]] = product_counts[[i]]
}

## Updating column names to be product type counting
colnames(df_products_info) = c("brand_name", paste(products_types, "count", sep = "_"))


# Calculate Price -------------------------------------------------------

## Function that locates pattern 
locate_product = function(list, pattern){
  list %>%
    str_which(pattern)
}

## Function that extracts all strings with prices 
extract_price = function(list){
  list %>%
    str_remove_all("\\s-\\s\\$\\d{1,3}\\.\\d{1,2}") %>% # Removes second part of range
    str_subset("\\$\\d{1,3}\\.\\d{1,2}") %>%
    str_remove_all("\\$")
  }

## Each initial item in the list is each product, then within it is a list 
## with the index location of the product
product_locate = vector(mode = "list", length = 1)

for(i in seq_along(pattern)){
  product_locate[[i]] = lapply(skincare_products_details, locate_product, pattern[i])

}

## List with price for each product 
price_list = lapply(skincare_products_details, extract_price)

## Converting each item in the list to numeric if there is an item
## in that list
for(i in seq_along(price_list)){
  if(length(price_list[[i]])>0){
    price_list[[i]] = as.numeric(price_list[[i]])
  }
}

df_products_info$average_price = sapply(price_list, mean, na.rm = TRUE)



# Code Graveyard ----------------------------------------------------------


# Treatment --> treatment, solution, concentrate, spot, peel
# Facial Moisturizer --> Cream, balm
# Exfoliator
 # Wipes











