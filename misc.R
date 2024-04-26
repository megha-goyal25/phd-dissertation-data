library(flextable)

## Table for indicators as used in methodology chapter

indicators <- c("Socio-Economic Information", "Awareness", "Perception of Gender",
                "Gender-Sensitive Considerations", "Effectiveness of National Policies",
                "Participation & Decision-Making", "Capacity Building",
                "Critical Experiences", "Workplace Experiences", "Gender Dynamics",
                "Challenges in Power Execution", "Challenges from Family",
                "Support in Fieldwork", "Resource Allocation", "Scope for Growth")

descriptions <- c("Data on income, education, living standards, etc.",
                  "Knowledge of gender roles, rights, and inequalities",
                  "Attitudes and beliefs about gender stereotypes",
                  "Integration of gender awareness in policies and programs",
                  "Assessment of policy impact on gender equality",
                  "Level of women's involvement in political decision-making",
                  "Skills and knowledge development related to gender issues",
                  "Criticisms experienced by the councillor",
                  "Experiences of discrimination or support in the workplace",
                  "Relationships between men and women in the council",
                  "Obstacles to women exercising poliitcal authority",
                  "Constraints imposed by family expectations",
                  "Assistance received during fieldwork",
                  "Availability of funding and gender budgeting",
                  "Further growth of council towards gender equality")

num_questions <- c(38, 19, 13, 9, 21, 62, 6, 3, 6, 4, 4, 5, 3, 4, 10)

ind_data <- data.frame(indicators, descriptions, num_questions)
ind_table <- flextable(ind_data) %>%
    theme_zebra() %>%
    set_table_properties(width = 1.0) %>%
    autofit() %>%
    set_header_labels(indicators = "Indicators",
                      descriptions = "Description",
                      num_questions = "No. of Questions")

save_as_image(ind_table, "tables/indicator-table.png")
