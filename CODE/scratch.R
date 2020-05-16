mutate(
  form = case_when(rownames(.) == "1" ~ "IT-49-Home",
                   T ~ NA_character_),
  n = case_when(rownames(.) == "1" ~ nrow(IT_49_Home_Stand_T_scores),
                T ~ NA_integer_)
) %>%
  select(form, n, scale, scale_order)
