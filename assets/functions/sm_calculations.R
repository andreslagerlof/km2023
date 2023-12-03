
# Set competition category ------------------------------------------------

set_comp_category <- function(df, comp_group)


# Calculate points --------------------------------------------------------

# This function takes a dataframe with a variable called "place" and 
# a variable called "comp_category" and creates (calculates) a new variable 
# called "points"

calculate_points_cat1 <- function(df, comp_category, place) {
  df |> 
    mutate(
        points = case_when(
          comp_category == 1 & place == 1 ~ 30,
          comp_category == 1 & place == 2 ~ 26,
          comp_category == 1 & place == 3 ~ 22,
          comp_category == 1 & between(place, 5,8)  ~ 18,
          comp_category == 1 & between(place, 9,16) ~ 14,
          comp_category == 1 & between(place, 17,32)  & is.na(poule) ~ 10,
          comp_category == 1 & between(place, 33,64)  & is.na(poule) ~ 6,
          comp_category == 1 & between(place, 65,96)  & is.na(poule) ~ 2,
          comp_category == 1 & between(place, 97,128) & is.na(poule) ~ 1,
          comp_category == 1 & poule != NA ~ 0
          )
        )
}
  
  
calculate_points_cat2 <- function(df, comp_category, place) { 
  df |> 
    mutate(
        points = case_when(
          comp_category == 2 & place == 1 ~ 50,
          comp_category == 2 & place == 2 ~ 42,
          comp_category == 2 & place == 3 ~ 34,
          comp_category == 2 & between(place, 5,8)  ~ 27,
          comp_category == 2 & between(place, 9,16) ~ 20,
          comp_category == 2 & between(place, 17,32)   & is.na(poule) ~ 15,
          comp_category == 2 & between(place, 33,64)   & is.na(poule) ~ 10,
          comp_category == 2 & between(place, 65,96)   & is.na(poule) ~ 5,
          comp_category == 2 & between(place, 97,128)  & is.na(poule) ~ 3,
          comp_category == 2 & between(place, 129,256) & is.na(poule) ~ 1,
          comp_category == 2 & poule != NA ~ 0
        )
      )
}


calculate_grand_tot <- function(df){
  df |>  
    group_by(name) |> 
    summarise(sum_points = sum(points, na.rm = TRUE)) |>  
    arrange(desc(sum_points)) |> 
    ungroup()
}


# Calculate points for the 2 best competitions
calculate_top_2 <- function(df){
  df %>% 
    group_by(name) %>% 
    slice_max(points, 
              n = 2,
              with_ties = FALSE) %>% 
    summarise(sum_top2_points = sum(points, na.rm = TRUE)) %>%  
    arrange(desc(sum_top2_points)) %>% 
    ungroup()
}
