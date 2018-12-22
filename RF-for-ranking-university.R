

#===========================================================================
#   State 1: Stage 1: Scraping Data For The Best 300 Global Universities
#  from https://www.usnews.com/education/best-global-universities/rankings
#===========================================================================

# Loading packages 

library(rvest)
library(tidyverse)
library(magrittr)



# Get data from the links

get_link_from_page <- function(x){
  
  x %>% 
    read_html(x) %>% 
    html_nodes("a") %>% 
    html_attr("href") -> k
  
  k1 <- k[str_detect(k, "https://www.usnews.com/education/best-global-universities")]
  return(k1[str_detect(k1, "[0-9$]")])
    
}


# Get all links from 30 pages: 

all_links <- lapply(paste0("https://www.usnews.com/education/best-global-universities/rankings?page=", 1:30),get_link_from_page) %>% 
  unlist()



# A function collects data for an university: 

get_data_for_university <- function(x) {
  
  x %>% 
    read_html() -> html_content
  
  html_content %>% 
    html_nodes('.t-slack.sep div') %>% 
    html_text() %>% 
    str_squish() -> my_raw_data
  
  my_raw_data %>% 
    matrix(ncol = 2, byrow = TRUE) %>% 
    as.data.frame() %>% 
    mutate_all(as.character) -> raw_df1
  
  
  html_content %>% 
    html_nodes('.thumb-left') %>% 
    html_text() %>% 
    str_squish() -> rank_field
  
  rank_field[-c(1:2)] -> rank_field
  
  html_content %>% 
    html_nodes('.t-large a') %>% 
    html_text() %>% 
    str_squish() -> field_name
  
  raw_df2 <- data_frame(field = field_name, rank = rank_field)
  raw_df2 %>% 
    spread(value = "rank", key = "field") -> raw_df2_wide
  
  raw_df1 %>%  
    spread(value = "V1", key = "V2") -> raw_df1_wide
  
  # Get Uni Name + address: 
  
  html_content %>% 
    html_nodes('.h-biggest') %>% 
    html_text() %>% 
    str_squish() -> uni_name
  
  html_content %>% 
    html_nodes('.clearfix .t-slack:nth-child(3)') %>% 
    html_text() %>% 
    str_squish() -> uni_add
  
  full_join(raw_df1_wide %>% mutate(uni_name = uni_name), 
            raw_df2_wide %>% mutate(uni_name = uni_name, uni_add = uni_add), 
            by = "uni_name") -> df_for_university
  
  return(df_for_university)
  
}

# Use above function for collecting data for 300 universities: 
all_data_for_uni <- lapply(all_links[1:300], get_data_for_university)


#====================================
#   State 2: Data Preprocessing
#====================================
# Use Reduce() in conjunction with intersect() for identifying common columns: 

common_columns <- Reduce(intersect, lapply(all_data_for_uni, names))

# Final data frame: 
final_df <- do.call("bind_rows", 
                    lapply(all_data_for_uni, function (x) {x %>% select(common_columns)}))

# Rename for columns and convert some non-numeric ones to numeric: 

names(final_df) <- str_replace_all(names(final_df), " ", "_")

final_df %<>%  
  select(-uni_add, -Best_Global_Universities) %>% 
  mutate(uni_name = str_replace_all(uni_name, "--", "-") %>% as.factor(), Global_score = as.factor(Global_score)) %>% 
  mutate_if(is.character, function(x) {x %>% str_replace_all("[^0-9]", "") %>% as.numeric()}) %>% 
  mutate(Global_score = as.character(Global_score) %>% as.numeric(), uni_name = as.character(uni_name))




## Random Forest for Ranking Universities

# Make a draft plot: 

library(extrafont)
library(ggrepel)
my_font <- "OfficinaSansITC"

my_gray_theme <- function(...) {
  theme_minimal() + 
    theme(plot.background = element_rect(fill = "#f5f5f2", color = NA)) + 
    theme(plot.title = element_text(family = my_font, size = 17)) +
    theme(plot.subtitle = element_text(family = my_font, size = 15, colour = "gray40")) + 
    theme(plot.caption = element_text(family = my_font, size = 15, color = "gray50")) + 
    theme(axis.text = element_text(family = my_font, size = 13, face = "bold", color = "gray50")) + 
    theme(axis.title = element_text(family = my_font, size = 15, face = "bold", colour = "gray20"))
  
}

final_df %>% 
  ggplot(aes(Number_of_publications_that_are_among_the_10_percent_most_cited, Global_score)) + 
  geom_point(alpha = 0.5, size = 5, color = "firebrick") + 
  geom_text_repel(data = final_df %>% slice(1:20),
                  aes(label = uni_name), force = 19, size = 4, color = "gray35", family = my_font) +
  geom_smooth(method = "lm", fill = "orange", color = "gray50", alpha = 0.2) + 
  scale_x_log10() + 
  my_gray_theme() + 
  labs(x = "Publication Rank", y = "Global Score",
       title = "The Relationship Between Ranking and Publication Rank for The 10 Percent Most Cited Research Papers",
       caption = "Source: https://www.usnews.com")


# A negative correlation between the two variables: 
cor(final_df$Number_of_publications_that_are_among_the_10_percent_most_cited, final_df$Global_score)



#=========================================================
#  Stage 3: Use Random Forest for ranking universities
#=========================================================

# Split data: 

set.seed(123)
train_df <- final_df %>% sample_frac(0.7, replace = FALSE)
test_df <- dplyr::setdiff(final_df, train_df)

# Set conditions for training RF model: 
library(caret)
set.seed(1)
train.control <- trainControl(method = "repeatedcv", 
                              number = 5,
                              repeats = 5)

# Function for training RF: 
rf_with_inputs <- function(train_df_selected) {
  
  set.seed(1)
  my_rf <- train(Global_score ~., 
                 data = train_df_selected %>% select(-uni_name), 
                 method = "rf", 
                 metric = "RMSE", 
                 importance = TRUE, 
                 trainControl = train.control)
  return(my_rf)
  
}


# Use this function: 
rf_13 <- rf_with_inputs(train_df)




# Function for predicting rankings and comparing: 

show_results <- function(model, test_df_selected) {
  
  # Create a data frame for comparing actuals and predictions: 
  df_comp <- data_frame(uni_name = test_df_selected$uni_name, 
                        Actual_scores = test_df_selected$Global_score, 
                        Predicted_scores = predict(model, test_df_selected %>% select(-uni_name)), 
                        Error = Actual_scores - Predicted_scores, 
                        Error_percent = 100*Error / Actual_scores) %>% 
    mutate_if(is.numeric, function(x) {round(x, 1)})
  
  return(df_comp)
}

# Fuction for show results by table: 

my_table <- function(df) {
  df %>% 
    rename(University = uni_name) %>% 
    select(-Error) %>% 
    knitr::kable()
}

# Actual and Predicted Rankings:  
df_comp <- show_results(rf_13, test_df) 
df_comp %>% my_table()





# Agreement between actuals and predictions by using scatter plot: 
df_comp %>% 
  ggplot(aes(Actual_scores, Predicted_scores)) + 
  geom_point(alpha = 0.5, color = "firebrick", size = 3) + 
  theme_minimal() + 
  labs(x = "Actual Scores", y = "Predicted Scores", 
       title = "Figure 1: Concordance between Actuals and Predictions, 13 Variables Used", 
       caption = "Data Source: https://www.usnews.com") + 
  my_gray_theme()






