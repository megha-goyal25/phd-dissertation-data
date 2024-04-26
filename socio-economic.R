### Analysis of Socio-Economic Conditions of Women Councilors in Ajmer ###
# loading the main file
source("main.R")

## Age
age_groups <- c("Below 30", "30-40", "40-50", "50-60", "Above 60")

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


## Education

all_councils$councillor_education <- factor(all_councils$councillor_education, levels = c(0, 1, 2, 3, 4),
                                            labels = c("Illiterate", "Primary", "Secondary", "Higher secondary", "Graduate"))

# Count the number of councillors in each education level for each council

education_summary <- all_councils %>%
  group_by(council, councillor_education) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  spread(councillor_education, count, fill = 0)  # Spread the data to wide format

                                        # Create a flextable
education_table <- flextable(education_summary) %>%
    theme_zebra() %>%
    set_caption(
        align_with_table = TRUE,
        caption = "Education Levels of Municipal Councillors in Ajmer") %>%
    set_table_properties(width = .8) %>%
    ## add_header_row(values = "No. of Councillors") %>%
    set_header_labels(council = "Council",
                      Illiterate = "Illiterate",
                      Primary = "Primary",
                      Secondary = "Secondary",
                      `Higher secondary` = "Higher Secondary",
                      Graduate = "Graduate") %>%
    merge_v(j = "council")

save_as_image(education_table, "tables/education-table.png")

# Education and Age
education_age <- subset(all_councils, select=c("councillor_age", "councillor_education"))
education_age_summary <- education_age %>%
    group_by(councillor_age, councillor_education) %>%
    summarise(count = n()) %>%
    ungroup() %>%
    spread(councillor_education, count, fill = 0)

education_age_table <- flextable(education_age_summary) %>%
    theme_zebra() %>%
    set_caption(
        align_with_table = TRUE,
        caption = "Correlating Education Level with Age") %>%
    set_table_properties(width = .9) %>%
    ## add_header_row(values = "No. of Councillors") %>%
    set_header_labels(
                      councillor_age = "Age") %>%
    merge_v(j = "council_age")

save_as_image(education_age_table, "tables/education-age-table.png")



## Marital Status
all_councils$councillor_councillor_marital <- factor(all_councils$councillor_marital, levels = c("M", "U", "W", "D"),
                                                     labels = c("Married", "Unmarried", "Widow", "Divorced"))

marital_summary <- all_councils %>%
  group_by(council, councillor_marital) %>%
  summarise(count = n()) %>%
  spread(key = councillor_marital, value = count, fill = 0)  # Spread the data to wide format

# Create a flextable
marital_table <- flextable(marital_summary) %>%
  theme_zebra() %>%
  set_table_properties(width = .8) %>%
    set_header_labels(council = "Council",
                      M = "Married",
                      U = "Unmarried",
                      W = "Widow",
                      D = "Divorced") %>%
  merge_v(j = "council")

save_as_image(marital_table, "tables/marital-table.png")


## Religion
religious_groups <- c("Hindu", "Muslim", "Christian", "Jain", "Others")

all_councils$councillor_religion <- factor(all_councils$councillor_religion, levels = c("H", "M", "C", "J", 11),
                                      labels = religious_groups)

religion_summary <- all_councils %>%
  group_by(council) %>%
  count(councillor_religion) %>%
  mutate(percent = n / sum(n) * 100)

religion_plot <- ggplot(religion_summary, aes(x = council, y = percent, fill = councillor_religion)) +
    geom_bar(stat = "identity") +
    labs(title = "Religious Distribution of Municipal Councillors",
         fill = "Religion",
         x = "Council",
         y = "Percentage of Councillors") +
    ## scale_fill_brewer(palette = "Set4") +  # Use a color palette for age groups
    theme_minimal()

ggsave("plots/religion-plot.png", religion_plot, width = 6, height = 4, dpi=300)

## Caste
caste_groups <- c("Scheduled Tribe", "Scheduled Caste", "OBCs", "General")

all_councils$councillor_caste <- factor(all_councils$councillor_caste, levels = c("ST", "SC", "OBC", "GN"),
                                      labels = caste_groups)

caste_summary <- all_councils %>%
  group_by(council) %>%
  count(councillor_caste) %>%
  mutate(percent = n / sum(n) * 100)

caste_plot <- ggplot(caste_summary, aes(x = council, y = percent, fill = councillor_caste)) +
    geom_bar(stat = "identity") +
    labs(title = "Caste Distribution of Municipal Councillors",
         fill = "Castes",
         x = "Council",
         y = "Percentage of Councillors") +
    scale_fill_brewer(palette = "Spectral") +  # Use a color palette for age groups
    theme_minimal()

ggsave("plots/caste-plot.png", caste_plot, dpi=300)

## Caste and Religion

caste_religion <- subset(all_councils, select=c("councillor_religion", "councillor_caste"))
caste_religion_summary <- caste_religion %>%
    group_by(councillor_religion, councillor_caste) %>%
    summarise(count = n()) %>%
    ungroup() %>%
    spread(councillor_caste, count, fill = 0)

caste_religion_table <- flextable(caste_religion_summary) %>%
    theme_zebra() %>%
    set_caption(
        align_with_table = TRUE,
        caption = "Correlating Caste Level with Religion") %>%
    set_table_properties(width = .9) %>%
    ## add_header_row(values = "No. of Councillors") %>%
    set_header_labels(
                      councillor_religion = "Religion") %>%
    merge_v(j = "council_religion")

save_as_image(caste_religion_table, "tables/caste-religion-table.png")



## Occupation
occupation_groups <- c("Housewife", "Agriculture", "Self-Employed", "Teaching/Education", "Industrial Labour", "Others")

all_councils$councillor_occupation <- factor(all_councils$councillor_occupation, levels = c("HW", "AH", "SE", "TE", "IL", 99),
                                            labels = occupation_groups)

# Count the number of councillors in each education level for each council
occupation_summary <- all_councils %>%
  group_by(council, councillor_occupation) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  spread(councillor_occupation, count, fill = 0)  # Spread the data to wide format

# Create a flextable
occupation_table <- flextable(occupation_summary) %>%
  theme_zebra() %>%
  set_table_properties(width = .8) %>%
  set_header_labels(council = "Council") %>%
  merge_v(j = "council")

save_as_image(occupation_table, "tables/occupation-table.png")

## Occupation and Age
occupation_age <- subset(all_councils, select=c("councillor_age", "councillor_occupation"))
occupation_age_summary <- occupation_age %>%
    group_by(councillor_age, councillor_occupation) %>%
    summarise(count = n()) %>%
    ungroup() %>%
    spread(councillor_age, count, fill = 0)

occupation_age_table <- flextable(occupation_age_summary) %>%
    theme_zebra() %>%
    set_caption(
        align_with_table = TRUE,
        caption = "Correlating Occupation with Age") %>%
    set_table_properties(width = .9) %>%
    ## add_header_row(values = "No. of Councillors") %>%
    set_header_labels(councillor_occupation = "Occupation",
                      councillor_age = "Age") %>%
    merge_v(j = "council_age")

save_as_image(occupation_age_table, "tables/occupation-age-table.png")



## Annual Income
all_councils$councillor_income <- factor(all_councils$councillor_income, levels = c(0, 1, 2, 3, 4),
                                          labels = c("<25,000", "25,000-50,000", "51,000-75,000", "76,000-1,00,000", "1,00,000+"))

# Create a summary table
income_summary <- all_councils %>%
  group_by(council, councillor_income) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from = councillor_income, values_from = count, values_fill = 0)  # Spread the data to wide format

# Create a flextable
income_table <- flextable(income_summary) %>%
  theme_zebra() %>%
  set_table_properties(width = .8) %>%
  set_header_labels(council = "Council") %>%
  merge_v(j = "council")

save_as_image(income_table, "tables/income-table.png")

# Print the flextable
#income_table

## Family Income
all_councils$husband_income <- factor(all_councils$husband_income, levels = c(0, 1, 2, 3, 4),
                                          labels = c("<25,000", "25,000-50,000", "51,000-75,000", "76,000-1,00,000", "1,00,000+"))

# Create a summary table
family_income_summary <- all_councils %>%
  group_by(council, husband_income) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from = husband_income, values_from = count, values_fill = 0)  # Spread the data to wide format

# Create a flextable
family_income_table <- flextable(family_income_summary) %>%
  theme_zebra() %>%
  set_table_properties(width = .8) %>%
  set_header_labels(council = "Council") %>%
  merge_v(j = "council")

save_as_image(family_income_table, "tables/family-income-table.png")

## Councillor Property
all_councils$councillor_property <- factor(all_councils$councillor_property, levels = c(0, 1),
                                          labels = c("No", "Yes"))

councillor_property_summary <- all_councils %>%
    group_by(council, councillor_property) %>%
    summarise(count = n()) %>%
    ungroup () %>%
    spread(councillor_property, count, fill=0)

councillor_property_table <- flextable(councillor_property_summary) %>%
    theme_zebra() %>%
    set_table_properties(width = .8) %>%
    add_header_row(values = c("", "Do you own any property?", "")) %>%   # Title row
    merge_h(i = 1, part = "header") %>%
    set_header_labels(council = "Council") %>%
    merge_v(j = "council")

save_as_image(councillor_property_table, width = 1, height = 1, "tables/councillor-property-table.png")


## Political Motivation
all_councils$councillor_motivation <- factor(all_councils$councillor_motivation, levels = c(1, 2, 3),
                                          labels = c("Party's Decision", "Other's Decision", "Own Decision"))

councillor_motivation_summary <- all_councils %>%
    group_by(council, councillor_motivation) %>%
    summarise(count = n()) %>%
    ungroup () %>%
    spread(councillor_motivation, count, fill=0)

councillor_motivation_table <- flextable(councillor_motivation_summary) %>%
  theme_zebra() %>%
  set_table_properties(width = .8) %>%
  set_header_labels(council = "Council") %>%
  merge_v(j = "council")

save_as_image(councillor_motivation_table, "tables/councillor-motivation-table.png")

## Political Positions

all_councils$councillor_prior_org <- factor(all_councils$councillor_prior_org, levels = c("PP", "SO", 22, 11),
                                          labels = c("Political Party", "Social Organization", "Both", "Neither"))

councillor_position_summary <- all_councils %>%
    group_by(council, councillor_prior_org) %>%
    summarise(count = n()) %>%
    ## ungroup () %>%
    spread(councillor_prior_org, count, fill=0)

councillor_position_table <- flextable(councillor_position_summary) %>%
  theme_zebra() %>%
  set_table_properties(width = .8) %>%
  set_header_labels(council = "Council") %>%
  merge_v(j = "council")

save_as_image(councillor_position_table, "tables/councillor-position-table.png")

## Political Inheritance

## Family Inheritance
all_councils$councillor_family <- factor(all_councils$councillor_family, levels = c(0, 1),
                                          labels = c("No", "Yes"))

councillor_family_summary <- all_councils %>%
    group_by(council, councillor_family) %>%
    summarise(count = n()) %>%
    ungroup () %>%
    spread(councillor_family, count, fill=0)

councillor_family_table <- flextable(councillor_family_summary) %>%
    theme_zebra() %>%
    set_table_properties(width = .8) %>%
    ## add_header_row(values = c("", "Family member in Politics", "")) %>%   # Title row
    ## merge_h(i = 1, part = "header") %>%
    set_header_labels(council = "Council") %>%
    merge_v(j = "council")

save_as_image(councillor_family_table, width = 6, height = 4, "tables/councillor-family-table.png")

## Active Relatives

all_councils$councillor_relatives <- factor(all_councils$councillor_relatives,
                                            levels = c("R0", "R1", -99),
                                            labels = c("Inactive Relatives", "Active Relatives", "No Relatives"))

councillor_relative_summary <- all_councils %>%
    group_by(council, councillor_relatives) %>%
    summarise(count = n()) %>%
    ungroup () %>%
    spread(councillor_relatives, count, fill=0)

councillor_relative_table <- flextable(councillor_relative_summary) %>%
  theme_zebra() %>%
  set_table_properties(width = .8) %>%
    add_header_row(values = c("", "Relatives in Politics", ""), colwidths = c(1,2,1)) %>%   # Title row
    merge_h(i = 1, part = "header") %>%
  set_header_labels(council = "Council") %>%
  merge_v(j = "council")

save_as_image(councillor_relative_table, "tables/councillor-relative-table.png")

## Political Party
party_groups <- c("Congress", "BJP", "RLP", "Independent")

all_councils$councillor_party <- factor(all_councils$councillor_party, levels = c("C", "B", "R", "I"),
                                      labels = party_groups)

party_summary <- all_councils %>%
  group_by(council) %>%
  count(councillor_party) %>%
  mutate(percent = n / sum(n) * 100)

party_plot <- ggplot(party_summary, aes(x = council, y = percent, fill = councillor_party)) +
    geom_bar(stat = "identity") +
    labs(title = "Political Distribution of Women Municipal Councillors",
         fill = "Political Party",
         x = "Council",
         y = "Percentage of Councillors") +
    ## scale_fill_brewer(palette = "Set3") +  # Use a color palette for age groups
    theme_minimal()

ggsave("plots/party-plot.png", party_plot, width = 6, height = 4, dpi=300)

## Political Experience
## Whether they were elected before
all_councils$councillor_prior_elected <- factor(all_councils$councillor_prior_elected, levels = c(0, 1),
                                          labels = c("No", "Yes"))

councillor_elected_summary <- all_councils %>%
    group_by(council, councillor_prior_elected) %>%
    summarise(count = n()) %>%
    ungroup () %>%
    spread(councillor_prior_elected, count, fill=0)

councillor_elected_table <- flextable(councillor_elected_summary) %>%
    theme_zebra() %>%
    set_table_properties(width = .8) %>%
    add_header_row(values = c("", "Were you elected before?", "")) %>%   # Title row
    merge_h(i = 1, part = "header") %>%
    set_header_labels(council = "Council") %>%
    merge_v(j = "council")

save_as_image(councillor_elected_table, width = 6, height = 4, "tables/councillor-elected-table.png")

## Children

children <- all_councils %>%
    group_by(council, councillor_children) %>%
    mutate(percentage = (n() / nrow(all_councils)) * 100)


children_plot <- ggplot(children, aes(x = councillor_children, y = percentage, color = council)) +
    geom_line() +  # Line graph
    labs(x = "Number of Children", y = "Percentage of Councillors",
         title = "Councillor Children Distribution by Council")

ggsave("plots/children-plot.png", children_plot, width = 6, height = 4, dpi=300)
