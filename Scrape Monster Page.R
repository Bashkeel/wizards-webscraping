library(RSelenium)
library(rvest)
library(tidyverse)
library(knitr)
library(kableExtra)

# Start automated browsing
rD[["server"]]$stop() 
rD <- rsDriver(browser = "chrome")
remdr <- rD[['client']]

url <- "https://www.dndbeyond.com/login"
remdr$navigate(url)
remdr$findElement(using = "css selector", value = ".twitch-button")$clickElement()

# Replace USERNAME/PASSWORD, or input manually
  remdr$findElement(using = "css selector", value = ".text")$sendKeysToElement(list("USERNAME", key = "tab", "PASSWORD", "\uE007"))
Sys.sleep(1) # Allows page to load before clicking authorize button
remdr$findElement(using = 'css selector', value = '.js-authorize-text')$clickElement()

#############
# Functions #
#############
get_monster_url <- function(url) {
  page <- read_html(url)
  abs_links <- page %>%
    html_nodes(".link") %>%
    html_attr("href") %>%
    str_subset("/monsters/") %>%
    str_replace_all("/monsters/", "https://www.dndbeyond.com/monsters/")
}


replace_if_empty <- function(text, replacement) {
  if (length(text)==0) {text <- replacement}
  else if (text == "") {text <- replacement}
  text
}

select_text <- function(xml, selector, trim = TRUE) {
  text <- xml %>% 
    html_nodes(selector) %>%
    html_text %>% trimws()
  text
}

scrape_monster_page <- function(page){
  # Overview
  monster_name <- select_text(page, ".mon-stat-block__name-link")
  monster_portrait <- page %>% html_node('.detail-content') %>% html_node('img') %>% html_attr('src')
  meta_text <- select_text(page, ".mon-stat-block__meta")
  meta_split1 <- str_split(meta_text, ", ")[[1]]
  meta_split2 <- str_split(meta_split1, " ")[[1]]
  monster_size <- tools::toTitleCase(meta_split2[1])
  monster_type <- tools::toTitleCase(meta_split2[2])
  monster_alignment <- tools::toTitleCase(meta_split1[2])
  monster_overview <- tibble(Label = c('Name', "Size", "Type", "Alignment"),
                             Content = c(monster_name, monster_size, monster_type, monster_alignment))
  
  # Monster Attributes
  monster_attributes_node <- page %>% html_nodes('.mon-stat-block__attribute')
  monster_attributes <- do.call(rbind, lapply(monster_attributes_node, function(node) {
    attribute_label <- node %>% select_text(".mon-stat-block__attribute-label")
    attribute_value <- node %>% select_text(".mon-stat-block__attribute-data-value")
    attribute_extra <- node %>% select_text(".mon-stat-block__attribute-data-extra") %>% replace_if_empty("")
    attribute_content <- paste(attribute_value, attribute_extra)
    tibble(Label = attribute_label, Content = attribute_content)
  }))
  
  # Ability Score
  ability_score_node <- page %>% html_nodes(".ability-block__stat")
  monster_ability_scores <- do.call(rbind, lapply(ability_score_node, function(node) {
    ability <- node %>% select_text(".ability-block__heading")
    score <- node %>% select_text('.ability-block__score') %>% as.integer()
    modifier <- node %>% select_text('.ability-block__modifier')
    score_content <- paste(score, modifier)
    tibble(Label = ability, Content = score_content)
  }))
  
  #Tidbits
  tidbits_node <- page %>% html_nodes('.mon-stat-block__tidbits')
  tidbits <- do.call(rbind, lapply(tidbits_node, function(node) {
    label <- node %>% select_text(".mon-stat-block__tidbit-label")
    value <- node %>% select_text(".mon-stat-block__tidbit-data")
    tibble(Label = label, Content = value)
  }))
  
  #Description
  description_node <- page %>% html_nodes('.mon-stat-block__description-block')
  description_sections <- do.call(rbind, lapply(description_node, function(node) {
    header <- node %>% select_text(".mon-stat-block__description-block-heading") %>% replace_if_empty("Misc Features")
    content <- node %>% select_text(".mon-stat-block__description-block-content p")
    tibble(Label = header, Content = content)
  }))
  
  # Extra Info
  extra_info <- page %>% select_text(".mon-details__description-block-content")
  
  # Extended Info
  
  extended_info <- tibble(Label = c("Tags", "Environment", "Source"), 
                          Content = c(page %>% select_text(".monster-tag") %>% paste(collapse = ",") %>% replace_if_empty("Unknown"), 
                                      page %>% select_text(".environment-tag") %>% paste(collapse = ",") %>% replace_if_empty("Unknown"), 
                                      page %>% select_text(".monster-source") %>% paste(collapse = ",") %>% replace_if_empty("Unknown")))
  
  tibble(Portrait = monster_portrait,
         Name =   monster_name,
         Overview = list(monster_overview),
         Attributes = list(monster_attributes),
         Ability_Scores = list(monster_ability_scores),
         Tidbits = list(tidbits),
         Description= list(description_sections),
         Extra_Info = (extra_info),
         Extended_info = list(extended_info))
}


#######################
# Scrape Monster Info #
#######################

num_pages <- monsters %>% 
  html_nodes(".b-pagination-item") %>% 
  html_text() %>% 
  as.integer() %>% 
  max(na.rm = TRUE)

all_monster_urls <- lapply(seq_len(num_pages), function(i) {
  url = paste0("https://www.dndbeyond.com/monsters?page=", i)
  get_monster_url(url)
}) %>% unlist

monster_info <- vector("list", length(all_monster_urls))
for (i in seq_along(all_monster_urls)){
  url <- all_monster_urls[i]
  remdr$navigate(url)
  page <- read_html(remdr$getPageSource()[[1]])
  current_url <- remdr$getCurrentUrl()[[1]]
  if (current_url == url) {
    monster_info[[i]] <- scrape_monster_page(page)
    cat(sprintf("Monster Found! - Progress: %s/%s\n", i, length(all_monster_urls)))
  }
  else {cat(sprintf("Content Unavailable - Progress: %s/%s\n", i, length(all_monster_urls)))}
  Sys.sleep(1)
}

monster_info <- do.call(rbind, monster_info)
saveRDS(monster_info, file = "monster_info.rds")


##############
# HTML Table #
##############

tibble_length <- function(tibs) {
  (monster_table %>% .[[tibs]] %>% unlist %>% length)/2
}

find_monster <- function(monster){
  monster_table <- monster_info %>% filter(Name == monster)
  monster_portrait <- monster_table$Portrait
  portrait_row <- tibble("Label" = " ", "Content" = sprintf("<img src=%s>)", monster_portrait))
  monster_data <- unlist(monster_table[,c(3:6,9,7)], recursive = FALSE)
  monster_data <- do.call("rbind", monster_data)
  monster_data <- rbind(portrait_row, monster_data[2:nrow(monster_data),])
  
  monster_summary <- kable(monster_data, row.names = F, booktabs=T, col.names=c("", "")) %>%
    kable_styling("striped") %>%
    group_rows(monster_table$Name, 1, 4) %>%
    group_rows(index = c(" " = 4,
                         "Attributes" = tibble_length("Attributes"),
                         "Ability Score" = tibble_length("Ability_Scores"),
                         "Tidbits" = tibble_length("Tidbits"),
                         "Tags" = tibble_length("Extended_info"),
                         "Description" = tibble_length('Description')))
  
  description <- monster_table[[8]] %>% 
    str_split("\n") %>% 
    simplify() %>%
    str_replace("Lair Actions", "<b>Lair Actions</b>") %>%
    str_replace("Regional Effects", "<b>Regional Effects</b>") %>%
    kable(col.names=c("Extended Description"), escape = F, booktabs = T, format="html") %>%
    kable_styling('striped', full_width = F)
  
  print(monster_summary)
  print(description)
  
}

find_monster("Adult Black Dragon")
