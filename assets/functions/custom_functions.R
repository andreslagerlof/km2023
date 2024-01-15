
# Calculate points --------------------------------------------------------

# This function takes a dataframe with a variable called "place" and calculates points

calculate_points <- function(df, place) {
  tot <- df |> 
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
                               18:50 ~ 1
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

results_table <- function(df){
  df |> 
    filter(comp_no == current_comp_no) |> 
    select(-c(gender, comp_no)) |>
    arrange(place) |> 
    na.omit(place) |> 
    rename(Namn = name,
           Placering = place,
           Poäng = points) |>  
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


# Calculate top 4 results -------------------------------------------------

# Calculate points for the 4 best competitions
calculate_top_4 <- function(df){
  df |> 
    group_by(name) |> 
    slice_max(points, 
              n = 4,
              with_ties = FALSE) |> 
    summarise(sum_top4_points = sum(points, na.rm = TRUE)) |> 
    filter(sum_top4_points > 0) |> 
    arrange(desc(sum_top4_points))
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


# Create final standings table including rank and totals ------------------
return_res_final <- function(df){
  # prepare standings table
  df |> 
    select(-place) |>  
    pivot_wider(
      names_from = comp_no, 
      values_from = points) |> 
    
    # Create new piv df with totals column
    full_join(grand_tot, by = "name") |>  
    full_join(best_4, by = "name") |> 
    arrange(desc(sum_top4_points)) |> 
    
    # Add ranking
    mutate(rank = min_rank(desc(sum_top4_points))) |> 
    relocate(rank, everything())
}


# Male and female standings -----------------------------------------------
create_totals_gender_table <- function(df, selected_gender, selected_class){
  df |> 
    filter(gender == selected_gender) |>
    filter(sum_points > 0) |> 
    select(-c(rank, gender)) |> 
    # Add ranking
    mutate(rank = min_rank(desc(sum_points))) |> 
    relocate(rank, everything()) |> 
    arrange(rank) |> 
    rename( "#" = rank,
            Namn = name,
            Totalt = sum_points) |> 
    gt() |> 
    tab_spanner(
      label = "Deltävling nr.",
      columns = -c("#", Namn, Totalt)
    ) |> 
    tab_header(
      title = paste0("Resultat i ", selected_class),
      subtitle = paste0("Efter ", current_comp_no ," deltävlingar"))
}


# Total standings ---------------------------------------------------------
create_totals_table <- function(df){
  df |> 
    filter(sum_points > 0) |> 
    select(-c(rank, gender)) |> 
    # Add ranking
    mutate(rank = min_rank(desc(sum_points))) |> 
    relocate(rank, everything()) |> 
    arrange(rank) |> 
    rename( "#" = rank,
            Namn = name,
            Totalt = sum_points) |> 
    gt() |> 
    tab_spanner(
      label = "Deltävling nr.",
      columns = -c("#", Namn, Totalt)
    ) |> 
    tab_header(
      title = "Totalsammanställning ",
      subtitle = paste0("Efter ", current_comp_no ," deltävlingar"))
}

