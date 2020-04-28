library(tidyverse)
library(stringr)

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
map_proportion_pop <- draw_map(ns_pop3, "State Population Estimate (2013-2017): Proportion",
                               "Proportion (% of Total Population)",
                               1)

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


library(ggplot2)

# nation_plotting = a function for plotting 
nation_plotting <- function(df, ycol, title) {
        ggplot(df, aes(x = LNTITLE, y = ycol, fill = LNTITLE)) +
                geom_col(aes(color = LNTITLE)) + 
                ylab("% of Each Population") +
                xlab("Ethnic Group") + 
                ggtitle(title) + 
                theme(legend.title = element_blank(), axis.text.x=element_blank())
}

# plotting for entire country
nation_plot_adult <- nation_plotting(ns3, ns3$Percent_ADU, "Adult (18 or older) in the US") +
        geom_hline(yintercept = ns2$Percent_ADU[1], size = 1)
nation_plot_citizen <- nation_plotting(ns3, ns3$Percent_CIT, "US Citizen in the US") +
        geom_hline(yintercept = ns2$Percent_CIT[1], size = 1)
nation_plot_ad_cit <- nation_plotting(ns3, ns3$Percent_CVAP, "Adult (18 or older) US Citizen in the US") +
        geom_hline(yintercept = ns2$Percent_CVAP[1], size = 1) + 
        theme(panel.background = element_rect(fill = "white"))


# Prep for getting proportion of adult/citizen/adult&citizen in each state
ns4 <- ns2 %>% filter(GEONAME != "United States", LNTITLE == "Total") 

# data cleaning and mapping for adult proprotion
ns_ADU <- mapping_table(ns4, ns4$Percent_ADU)
map_state_adu <- draw_map(ns_ADU, 
                          "Adult Proportion Estimate (2013-2017)",
                          "Proportion (% of State Population)", 
                          1)
# data cleaning and mapping for citizen proportion
ns_CIT <- mapping_table(ns4, ns4$Percent_CIT)
map_state_cit <- draw_map(ns_CIT, 
                          "US Citizen Proportion Estimate (2013-2017)", 
                          "Proportion (% of State Population)", 
                          1)

# data cleaning and mapping for adult citizen 
ns_ADUCIT <- mapping_table(ns4, ns4$Percent_CVAP)
map_state_ADUCIT <- draw_map(ns_ADUCIT, 
                             "US Adult Citizen Proportion Estimate (2013-2017)", 
                             "Proportion (% of State Population)", 
                             1)
ns_ADUCIT_US <- mapping_table(ns4, ns4$CVAP_EST)
map_total_ADUCIT <- draw_map(ns_ADUCIT_US, 
                             "US Adult Citizen Population Estimate (2013-2017)", 
                             "Population)", 
                             1)

# data cleaning and tables 
library(formattable)
ns5 <- ns3 %>% 
        transmute(Ethnic_Group = LNTITLE, Percent_of_Population = Percent_CVAP) %>% 
        arrange(desc(Percent_of_Population))
adult_us_citizen_table <- formattable(ns5, 
                                      list(Percent_of_Population = color_tile("lightblue", "lightpink")))
ns_pop4 <- ns_pop1 %>% 
        arrange(desc(value)) %>% 
        transmute(States = toupper(region), Population = value)

pop_summary <- data.frame(Top_5_States = ns_pop4$States[1:5], 
                          Top_5_Population = ns_pop4$Population[1:5],
                          Bottom_5_States = ns_pop4$States[48:52],
                          Bottom_5_Population = ns_pop4$Population[48:52])

pop_table <- formattable(pop_summary, 
                         list(Top_5_Population = color_tile("transparent", "#CCCC00"),
                              Bottom_5_Population = color_tile("transparent", "#999900")))
ns_ADUCIT1 <- ns_ADUCIT %>% 
        arrange(desc(value)) %>%
        transmute(States = toupper(region), Percent_of_State_Population = value) %>% 
        filter(States != "PUERTO RICO")

ns_adult_prop_summary<- data.frame(Top_5_States = ns_ADUCIT1$States[1:5],
                                   Top_5_Proportion = ns_ADUCIT1$Percent_of_State_Population[1:5],
                                   Bottom_5_States = ns_ADUCIT1$States[47:51],
                                   Bottom_5_Proportion = ns_ADUCIT1$Percent_of_State_Population[47:51])
ns_adult_prop_table <- formattable(ns_adult_prop_summary,
                                   list(Top_5_Proportion = color_tile("transparent", "#FFCC00"),
                                        Bottom_5_Proportion = color_tile("transparent", "#FFCC99")))

# asian population map
asian <- c("Asian Alone", "Asian and White")
ns6 <- ns2 %>%
        filter(GEONAME != "United States", LNTITLE %in% asian) 
ns7 <- ns6 %>% 
        group_by(GEONAME) %>%
        summarize(Asian_Population = sum(TOT_EST)) %>% 
        transmute(region = tolower(GEONAME), value = Asian_Population)

asian_pop_map <- draw_map(ns7, 
                          "Asian Population Estimate (2013-2017)", 
                          "Population", 
                          1)

ns8 <- ns4 %>% 
        mutate(GEONAME = tolower(GEONAME)) %>% 
        inner_join(ns7, by = c("GEONAME" = "region")) %>% 
        rename(Asian_Population = value) %>% 
        mutate(Asian_Percent = Asian_Population / TOT_EST * 100)

ns9 <- ns6 %>% 
        group_by(GEONAME) %>%
        summarize(Asian_Ault_Population = sum(CVAP_EST)) %>% 
        mutate(GEONAME = tolower(GEONAME)) %>% 
        inner_join(ns8, by = "GEONAME") %>% 
        mutate(Percent_Asian_Adult = Asian_Ault_Population / CVAP_EST * 100)

ns_asian_percent <- ns9 %>%
        transmute(region = GEONAME, value = Asian_Percent)
ns_asian_adult_percent <- ns9 %>% 
        transmute(region = GEONAME, value = Percent_Asian_Adult)

asian_percent_map <- draw_map(ns_asian_percent, 
                          "Asian Proportion Estimate (2013-2017)", 
                          "Proportion (% of State Population", 
                          1)

asian_adult_percent_map <- draw_map(ns_asian_percent, 
                              "Asian Proportion Estimate (2013-2017)", 
                              "Proportion (% of State Population", 
                              9)

ns_asian_percent1 <- ns_asian_percent %>%
        arrange(desc(value))