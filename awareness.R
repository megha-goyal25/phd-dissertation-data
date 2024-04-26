##### Indicator 1: Awareness #####
## Loading necessary files
source("main.R")
## source("socio-economic.R")

## Across Coucils
all_awareness <- rbind(ind(kishangarh, awareness),
                       ind(beawar, awareness),
                       ind(bhilwara, awareness),
                       ind(nagaur, awareness),
                       ind(tonk, awareness),
                       ind(makrana, awareness))

## Dividing the questions

aw_1 <- awareness[1:5] # Powers 1
aw_2 <- awareness[6:9] # Powers 2
aw_3 <- awareness[10:12] # Policies
aw_4 <- awareness[13:16] # Duties
aw_5 <- awareness[17:18] # Funding


## Stacked Bar Plots for Awareness of Powers
awareness_data <- all_awareness %>%
                  mutate_all(as.factor)  # Ensure columns are factors

# Reshape data for stacked bars
awareness_plot_data <- awareness_data %>%
             select(council, awareness_power:awareness_waste) %>%
             tidyr::pivot_longer(cols = starts_with("awareness_"),
                                 names_to = "question", values_to = "response")


label_map <- c("awareness_roads" = "Road",
               "awareness_women" = "Women",
               "awareness_power" = "Power",
               "awareness_health" = "Health",
               "awareness_law" = "Law",
               "awareness_water" = "Water",
               "awareness_urban" = "Urban Housing",
               "awareness_education" = "Education",
               "awareness_waste" = "Waste Mgmt"
               )

awareness_overall_plot <- ggplot(awareness_plot_data, aes(x = question, fill = response)) +
  geom_bar(position = "fill") +
  facet_wrap(~ council) +
  labs(x = "Awareness Area", y = "Proportion of Responses",
       title = "Awareness Distribution Across Councils") +
  scale_fill_manual(values = c("grey", "pink"), labels = c("Ignorant", "Aware")) +
  scale_x_discrete(labels = label_map) +  # Customize x-axis labels
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("plots/awareness-overall-plot.png", awareness_overall_plot, dpi=300)

## Awareness of Policies
aw_policies <- subset(all_awareness, select = c("awareness_amendment", "awareness_twochild", "council"))

aw_policies$awareness_amendment <- factor(aw_policies$awareness_amendment, levels = c(0, -50, 50, 100),

                                                     labels = c("Ignorant", "Not Essential", "Essential", "Aware"))

aw_policies$awareness_twochild <- factor(aw_policies$awareness_twochild, levels = c(0, -50, 50, 100),

                                                     labels = c("Ignorant", "Not Essential", "Essential", "Aware"))



awareness_amendment_summary <- aw_policies %>%
    group_by(council, awareness_amendment) %>%
    summarize(count = n()) %>%
    ungroup() %>%
    pivot_wider(names_from = c(awareness_amendment),
                values_from = count, values_fill = 0)

awareness_twochild_summary <- aw_policies %>%
    group_by(council, awareness_twochild) %>%
    summarize(count = n()) %>%
    ungroup() %>%
    pivot_wider(names_from = c(awareness_twochild),
                values_from = count, values_fill = 0)

## Create a flextable
awareness_amendment_table <- flextable(awareness_amendment_summary) %>%
    theme_zebra() %>%
    set_table_properties(width = .8) %>%
    set_header_labels(council = "Council") %>%
    merge_v(j = "council")

awareness_twochild_table <- flextable(awareness_twochild_summary) %>%
    theme_zebra() %>%
    set_table_properties(width = .8) %>%
    set_header_labels(council = "Council") %>%
    merge_v(j = "council")

save_as_image(awareness_twochild_table, "tables/awareness-twochild-table.png")
save_as_image(awareness_amendment_table, "tables/awareness-amendment-table.png")

### Awareness of Duties
awareness_duties_summary <- all_awareness %>%
    group_by(council, awareness_ulb) %>%
    summarize(count = n()) %>%
    ungroup() %>%
    pivot_wider(names_from = c(awareness_ulb),
                values_from = count, values_fill = 0)

awareness_duties_table <- flextable(awareness_duties_summary) %>%
    theme_zebra() %>%
    set_table_properties(width = .8) %>%
    set_header_labels(council = "Council",
                      "-50" = "Not Essential",
                      "50" = "Essential") %>%
    merge_v(j = "council")

save_as_image(awareness_duties_table, width = 2, height = 2, "tables/awareness-duties-table.png")

### Awareness of Resources
awareness_grants_summary <- all_awareness %>%
    group_by(council, awareness_grant) %>%
    summarize(count = n()) %>%
    ungroup() %>%
    pivot_wider(names_from = c(awareness_grant),
                values_from = count, values_fill = 0)

awareness_grants_table <- flextable(awareness_grants_summary) %>%
    theme_zebra() %>%
    ## set_table_properties(width = .8) %>%
    set_header_labels(council = "Council",
                      "0" = "No",
                      "1" = "Yes") %>%
    merge_v(j = "council")

save_as_image(awareness_grants_table, width = 2, height = 2, "tables/awareness-grants-table.png")
