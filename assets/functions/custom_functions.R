
# Calculate points --------------------------------------------------------

# This function takes a dataframe with a variable called "place" and calculates points

calculate_points <- function(df, place) {
  tot <- df %>% 
    mutate(points = case_match(place,
                               1 ~ 25,
                               2 ~ 22,
                               3 ~ 19,
                               4 ~ 17,
                               5 ~ 15,
                               6 ~ 13,
                               7 ~ 12,
                               8 ~ 11,
                               9 ~ 10,
                               10 ~ 9,
                               11 ~ 8,
                               12 ~ 7,
                               13 ~ 6,
                               14 ~ 5,
                               15 ~ 4,
                               16 ~ 3,
                               17 ~ 2,
                               between(place, 18, 40) ~ 1
    ))
}

# Make pivot_longer -------------------------------------------------------
# reshapes a tibble to long format
to_long_format <- function(df){
  df %>%
    # make long dataframe
    pivot_longer(cols = starts_with("km_"), 
                 names_to = "comp_no",
                 names_prefix = "km_",
                 values_to = "place")
}

# Create competition results table ---------------------------------------

# Prepare table
results_table <- function(df){
  df |> 
    filter(comp_no == 4) %>% 
    select(-c(gender, comp_no, points)) %>%
    rename(Namn = name, Placering = place) %>% 
    na.omit(Placering) %>% 
    arrange(Placering) |> 
    gt() |> 
    tab_header(
      title = paste0("Resultat från KM, deltävling ", 
                     current_comp_no)
    )
}

# Calculate grand total ---------------------------------------------------

## Calculate grand total points for all competitions (1-3)
calculate_grand_tot <- function(df){
  df |>  
    group_by(name) |> 
    summarise(sum_points = sum(points, na.rm = TRUE)) |>  
    arrange(desc(sum_points))
}

# Crete standings table including rank and totals -------------------------

# modify data to produce all necessary variables
# in correct output format
return_res <- function(df){
  # prepare standings table
  df |> 
    select(-place) |>  
    pivot_wider(
      names_from = comp_no, 
      values_from = points) |> 
    
    # Create new piv df with totals column
    full_join(grand_tot, by = "name") |>  
    arrange(desc(sum_points)) |> 
    
    # Add ranking
    mutate(rank = min_rank(desc(sum_points))) |> 
    relocate(rank, everything())
}

# Prep data and split by gender as input for at gt() table
prep_total_table<- function(df, selected_gender) {
  df |> 
    filter(gender == selected_gender) |>
    #select(-gender) |> 
    rename(Nr = rank, Namn = name, Totalt = sum_points) 
}

# Female totals table
create_totals_table_f <- function(df, current_comp_no){
  df %>% 
    select(-gender) %>% 
    gt() %>% 
    tab_spanner(
      label = "Deltävling nr.",
      columns = -c(Nr, Namn, Totalt)
    ) %>% 
    tab_header(
      title = "Resultat i damklassen",
      subtitle = paste0("Efter ", current_comp_no ," deltävlingar")
    )
}

# Male totals table
create_totals_table_m <- function(df, current_comp_no){
  df %>% 
    select(-gender) %>% 
    gt() %>% 
    tab_spanner(
      label = "Deltävling nr.",
      columns = -c(Nr, Namn, Totalt)
    ) %>% 
    tab_header(
      title = "Resultat i herrklassen",
      subtitle = paste0("Efter ", current_comp_no ," deltävlingar")
    )
}

