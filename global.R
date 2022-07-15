# Load librabries
pacman::p_load(shiny, shinydashboard, DT, plotly, ggplot2, 
               tidyverse, data.table, shinyWidgets, 
               shinythemes, shinycssloaders, 
               shinydashboardPlus)



# Import data
Uganda <- data.table(read_csv("data/Uganda.csv"))

Uganda$Year <- factor(Uganda$Year, levels = c("BAU", "Year 1", "Year 2", "Year 3",
                                              "Year 4", "Year 5", "Year 6", "Year 7",
                                              "Year 8", "Year 9", "Year 10", "Year 20",
                                              "Year 30"), 
                      ordered = T)


ug.type.of.investment <- unique(Uganda$`Type of investment`)

Uganda$`Type of investment` <- factor(Uganda$`Type of investment`,
                                      levels = ug.type.of.investment,
                                      ordered = T)


Rwanda <- data.table(read_csv("data/Rwanda.csv"))

Rwanda$Year <- factor(Rwanda$Year, levels = c("BAU", "Year 1", "Year 2", "Year 3",
                                              "Year 4", "Year 5", "Year 6", "Year 7",
                                              "Year 8", "Year 9", "Year 10", "Year 20",
                                              "Year 30"), 
                      ordered = T)


rw.type.of.investment <- unique(Rwanda$`Type of investment`)

Rwanda$`Type of investment` <- factor(Rwanda$`Type of investment`, 
                                      levels = rw.type.of.investment, 
                                      ordered = T)


Individual_farmer_costs <- c("Labor costs", 
                             "Input costs", 
                             "Total individual costs to farmer")

Incremental_farmer_costs <- c("Incremental labor costs", 
                              "Incremental input costs", 
                              "Incremental total individual costs to farmer")

Incremental_percent_costs <- c( "Labor costs percent increase", 
                                "Input costs percent increase",
                                "Total individual costs to farmer percent increase" )


Gross_benefits <- c("Gross value of annual crops of USD",
                    "Gross value of perrenial crops of USD",
                    "Gross value of tree species USD",
                    "Gross value")


Net_values <- c("Net values to farmers in USD/Ha",
                "Net Present Values (NPV) to farmers in USD/Ha",
                "Net Present Values (NPV) in USD/person/day/Ha")

NPC_NPGV_NPV <- c("Net Present Costs (NPC) to farmers in USD/Ha",
                  "Net Present Gross Values (NPGV) to farmers in USD/Ha",
                  "Net Present Values (NPV) in USD/person/day/Ha _ 1")

BCR <- "Benefit-Cost-Ratio (BCR)"
