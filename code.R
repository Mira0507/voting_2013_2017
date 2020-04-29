library(tidyverse)
library(stringr)
library(ggplot2)
library(ggrepel)
library(formattable)
h <- head
s <- summary
g <- glimpse
t <- tail

# ns = a master data frame
ns <- read.csv("Nation_State_combined.csv", stringsAsFactors = FALSE)

# primary data cleaning
ns <- ns[, -1]
drop <- c("TOT_MOE", "ADU_MOE", "CIT_MOE", "CVAP_MOE")
ns1 <- ns %>% select(-drop)

# mapping total population by state
library(choroplethr)
library(choroplethrMaps)
ns_pop <- ns1 %>% filter(GEONAME != "United States", LNTITLE == "Total")
ns_pop1 <- data.frame(region = tolower(ns_pop$GEONAME), value = ns_pop$TOT_EST)
ns_pop2 <- ns1 %>% filter(GEONAME != "United States", 
                          LNTITLE == "Total") %>%
        mutate(Percent_pop = TOT_EST / sum(TOT_EST) * 100)

mapping_table <- function(df, val) {
        df %>% transmute(region = tolower(GEONAME), value = val)
}
ns_pop3 <- mapping_table(ns_pop2, ns_pop2$Percent_pop)


# draw_map = a function for mapping
draw_map <- function(df, tit, leg, clr) {
        state_choropleth(df, 
                         title = tit,
                         legend = leg, 
                         num_colors = clr)
}

# map_total_pop = total population map over the country
map_total_pop <- draw_map(ns_pop1, 
                          "State Population Estimate (2013-2017)",
                          "Population", 1)

# TOT = total
# ADU = adult (18 or older)
# CIT = US citizen
# CVAP = adult citizen
# data cleaning for getting proportion
ns2 <- ns1 %>% mutate(Percent_ADU = ADU_EST / TOT_EST * 100,
                      Percent_CIT = CIT_EST / TOT_EST * 100,
                      Percent_CVAP = CVAP_EST / TOT_EST * 100)
ns3 <- ns2 %>%
        filter(LNTITLE != "Total", GEONAME == "United States", LNTITLE != "Not Hispanic or Latino")

# data cleaning for plotting demographics
ns4 <- ns3 %>%
        mutate(Percent_of_Total = round(TOT_EST / sum(TOT_EST) * 100, digits = 1),
               Percent_of_Voter_Total = round(CVAP_EST / sum(TOT_EST) * 100, digits = 1),
               Percent_of_Adult = round(CVAP_EST / ADU_EST * 100, digits = 1))


ns_pop4 <- ns4 %>%
        select(GEONAME, LNTITLE, TOT_EST:CVAP_EST) %>% 
        rename(Race = LNTITLE, 
               Total = TOT_EST,
               Adult = ADU_EST,
               Citizen = CIT_EST,
               Adult_Citizen = CVAP_EST) 

ns_pop5 <- gather(ns_pop4, Population_Category, Population, -c(GEONAME, Race))
ns_pop6 <- ns_pop5 %>%
        filter(Population_Category != "Citizen")


ns_pop7 <- ns4 %>% 
        select(GEONAME, LNTITLE, Percent_of_Total:Percent_of_Adult) %>%
        rename(Race = LNTITLE, 
               Race_Proportion_over_Total_Population = Percent_of_Total,
               Adult_Citizen_Proportion_over_Total_Population = Percent_of_Voter_Total,
               Adult_Citizen_Proportion_over_Adult_Population = Percent_of_Adult)
ns_pop8 <- gather(ns_pop7, Proportion_Category, Proportion, -c(GEONAME, Race))


# plots
# nation_population_plot: Nation-wide population by race
nation_population_plot <- ggplot(ns_pop6, aes(x = Race, y = Population, fill = Race)) + 
        ggtitle("Population of Individual Race") + 
        facet_grid(.~ Population_Category) + 
        geom_bar(stat = "identity") + 
        theme_grey() +
        theme(axis.text.x=element_blank())

# nation_proportion_plot1: Proportion of individual race 
nation_proportion_plot1 <- ggplot(ns_pop8[ns_pop8$Proportion_Category == "Race_Proportion_over_Total_Population", ],
                                  aes(x = "", y = Proportion, fill = Race)) +
        geom_bar(stat = "identity", width = 1, color = "white") +
        xlab("Race Population / Total Population (%)") +
        ylab(" ") +
        ggtitle("Proportion of Individual Race (%)") + 
        theme_grey() +
        theme(axis.text.x=element_blank(), panel.background = element_blank(),
              axis.text.y=element_blank(), axis.ticks = element_blank()) + 
        coord_polar(theta = "y", start = 0) + 
        geom_label_repel(aes(label = Proportion), position = position_stack(vjust = 0.5)) 

# Eligible voter proportion over Adult population in individual race
nation_proportion_plot2 <- ggplot(ns_pop8[ns_pop8$Proportion_Category == "Adult_Citizen_Proportion_over_Adult_Population", ], 
                                  aes(x = Race, y = Proportion, fill = Race)) +
        ylab("Adult Citizen / Total Adult (%)") +
        ggtitle("Adult Citizen over Total Adult (%) in Individual Race") +
        geom_bar(stat = "identity") + 
        theme_grey() +
        theme(axis.text.x=element_blank())

# Race proportion ranking table
proportion_table_df <- data.frame(Race = ns_pop8[ns_pop8$Proportion_Category == "Race_Proportion_over_Total_Population", ]$Race,
                                  Proportion = ns_pop8[ns_pop8$Proportion_Category == "Race_Proportion_over_Total_Population", ]$Proportion) %>%
        arrange(desc(Proportion))
proportion_table <- formattable(proportion_table_df,
                                list(Proportion = color_tile("lightblue", "lightpink")))

# data cleaning for asian statistics
asian <- c("Asian Alone", "Asian and White")
ns5 <- ns1 %>%
        filter(GEONAME != "United States", LNTITLE != "Total", 
               LNTITLE != "Not Hispanic or Latino", GEONAME != "Puerto Rico") 


ns6 <- ns5 %>%
        group_by(GEONAME) %>%
        summarize(State_Population = sum(TOT_EST))

ns7 <- ns5 %>% 
        left_join(ns6, by = "GEONAME") %>%
        mutate(Race_Proportion = round(TOT_EST / State_Population * 100, digits = 1))

# ns8: table for asian proportion by state
ns8 <- ns7 %>%
        filter(LNTITLE %in% asian) %>%
        transmute(State = GEONAME, Race = LNTITLE, Asian_Proportion = Race_Proportion) %>%
        group_by(State) %>%
        summarize(Asian_Proportion = sum(Asian_Proportion)) 

ns9 <- ns7 %>% 
        filter(LNTITLE %in% asian) %>% 
        select(GEONAME, LNTITLE, TOT_EST:Race_Proportion) %>% 
        group_by(GEONAME) %>%
        summarize(Total_Asian_Population = sum(TOT_EST), 
                  Asian_Adult_Population = sum(ADU_EST),
                  Asian_Citizen_Population = sum(CIT_EST), 
                  Asian_Adult_Citizen_Population = sum(CVAP_EST))

# ns10: table for asian population and proportion by state
ns10 <- ns9 %>% 
        left_join(ns6, by = "GEONAME") %>%
        rename(State = GEONAME) %>% 
        left_join(ns8, by = "State") %>%
        mutate(Asian_Voter_Proportion = 
                       round(Asian_Adult_Citizen_Population / State_Population * 100, 
                             digits = 1))



# plotting for asian population by state
total_pop_vs_asian_pop <- ggplot(ns10, 
                                 aes(x = State_Population, y = Total_Asian_Population)) + 
        geom_jitter(alpha = 0.6, size = 2) + 
        geom_smooth(col = "blue", se = FALSE) + 
        theme(panel.background = element_rect(fill = "white"), 
              axis.ticks = element_line(color = "black"), 
              axis.line = element_line(color = "black")) +
        ggtitle("State Total Population vs State Asian Population") + 
        xlab("State Total Population") +
        ylab("State Asian Population")

total_pop_vs_asian_pop_cor <- cor(ns10$State_Population, 
                                         ns10$Total_Asian_Population)

total_pop_vs_asian_proportion <- 
        ggplot(ns10, 
               aes(x = State_Population, y = Asian_Proportion)) + 
        geom_jitter(alpha = 0.6, size = 2, col = "#FF0000") + 
        geom_smooth(col = "blue", se = FALSE) + 
        theme(panel.background = element_rect(fill = "white"), 
              axis.ticks = element_line(color = "black"), 
              axis.line = element_line(color = "black")) +
        ggtitle("State Total Population vs State Asian Proportion") + 
        xlab("State Total Population") +
        ylab("State Asian Proportion (% of State Population)")

# correlation
total_pop_vs_asian_proportion_cor <- cor(ns10$State_Population, 
                                         ns10$Asian_Proportion)


total_pop_vs_asian_voter_proportion <- ggplot(ns10, 
       aes(x = State_Population, 
           y = Asian_Voter_Proportion)) + 
        geom_jitter(alpha = 0.6, size = 2, col = "#006600") + 
        geom_smooth(col = "blue", se = FALSE) + 
        theme(panel.background = element_rect(fill = "white"), 
              axis.ticks = element_line(color = "black"), 
              axis.line = element_line(color = "black")) +
        ggtitle("State Total Population vs Asian Adult Citizen Proportion") + 
        xlab("State Total Population") +
        ylab("State Asian Adult Proportion (% of State Population)")

total_pop_vs_asian_voter_proportion_cor <- cor(ns10$State_Population, 
                                               ns10$Asian_Voter_Proportion)