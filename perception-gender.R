##### Indicator 2: Perception of Gender #####
## Loading necessary files
source("main.R")

## Across Councils
all_perception <- rbind(ind(kishangarh, perc_gender),
                        ind(beawar, perc_gender),
                        ind(bhilwara, perc_gender),
                        ind(nagaur, perc_gender),
                        ind(tonk, perc_gender),
                        ind(makrana, perc_gender))

## Focus on Gender
focus_gender_summary <- all_perception %>%
    group_by(council, gender_focus) %>%
    summarize(count = n()) %>%
    ungroup() %>%
    pivot_wider(names_from = c(gender_focus),
                values_from = count, values_fill = 0)

focus_gender_table <- flextable(focus_gender_summary) %>%
    theme_zebra() %>%
    set_table_properties(width = .8) %>%
    set_header_labels(council = "Council",
                      "0" = "No",
                      "1" = "Yes") %>%
    merge_v(j = "council")

save_as_image(focus_gender_table, "tables/focus-gender-table.png")
