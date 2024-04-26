## Loading the main file
source("main.R")

## Across councils
all_participation <- rbind(ind(kishangarh, part_dec),
                        ind(beawar, part_dec),
                        ind(bhilwara, part_dec),
                        ind(nagaur, part_dec),
                        ind(tonk, part_dec),
                        ind(makrana, part_dec))

meeting_levels <- c("Always", "Sometimes", "Never")

all_councils$councillor_age <- factor(all_councils$councillor_age,
         levels = c("A", "B", "C", "D", "E"),
                                      labels = age_groups)

age_summary <- all_councils %>%
  group_by(council) %>%
  count(councillor_age) %>%
  mutate(percent = n / sum(n) * 100)

age_plot <- ggplot(age_summary, aes(x = council, y = percent, fill = councillor_age)) +
    geom_bar(stat = "identity") +
    labs(title = "Age Distribution of Councillors",
         x = "Council",
         y = "Percentage of Councillors",
         fill = "Age Groups") +
    scale_fill_brewer(palette = "Set3") +  # Use a color palette for age groups
    theme_minimal()

ggsave("plots/age-plot.png", age_plot, width = 6, height = 4, dpi=300)
