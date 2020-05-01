library(tidyverse)
library(stringr)
library(ggplot2)
library(ggrepel)
library(formattable)
library(choroplethr)
library(choroplethrMaps)
h <- head
s <- summary
g <- glimpse
t <- tail

# ns = a master data frame
ns <- read.csv("Nation_State_combined.csv", stringsAsFactors = FALSE)

# ec: a data frame for electoral college
# st: a data frame for state name and codes 
ec <- read.csv("electoral college.txt", header = FALSE)
names(ec) <- c("st", "Electoral_College")
st <- read.csv("st_name_code.csv")
st1 <- st %>% 
        left_join(ec, by = "st")

draw_map <- function(df, tit, leg, clr) {
        state_choropleth(df, 
                         title = tit,
                         legend = leg, 
                         num_colors = clr)
}

# primary data cleaning
ns <- ns[, -1]
drop <- c("TOT_MOE", "ADU_MOE", "CIT_MOE", "CVAP_MOE")

nss <- ns %>% 
        select(-drop)


# nss1: cleaned data for state population map
# state_population: state population map
nss1 <- nss %>%
        filter(LNTITLE == "Total", GEONAME != "United States", GEONAME != "Puerto Rico") %>% 
        transmute(region = tolower(GEONAME), value = TOT_EST)

state_population <- draw_map(nss1, "State Population (2013-2017)", "Population", 1)


# nss2: cleaned data for voter proportion map
# state_voter_proportion: proportion of adult citizen by state
nss2 <- nss %>%
        filter(GEONAME != "United States", GEONAME != "Puerto Rico", LNTITLE == "Total") %>%
        transmute(region = tolower(GEONAME), value = round(CVAP_EST / TOT_EST * 100, digits = 1))

state_voter_proportion <- draw_map(nss2, 
                                   "Proportion of Adult Citizen by State (2013-2017)", 
                                   "Proportion (% of Population)", 
                                   1)

# data cleaning for plotting relationship  btw state population and voter proportion
nss3 <- nss1 %>% 
        inner_join(nss2, by = "region") 
nss4 <- nss3 %>%
        mutate(region = toupper(region)) %>%
        rename(State = region, 
               Population = value.x, 
               Adult_Citizen_Proportion = value.y)

# pop_vs_voter_plot: scatter plot for relationship  btw state population and voter proportion
# pop_vs_voter_cor = R^2 = -0.55
pop_vs_voter_plot <- ggplot(nss4, aes(x = Population, y = Adult_Citizen_Proportion)) + 
        geom_point(alpha = 0.5, size = 3, col = "#990000") + 
        geom_smooth(col = "blue", se = FALSE) + 
        theme(panel.background = element_rect(fill = "white"), 
              axis.ticks = element_line(color = "black"), 
              axis.line = element_line(color = "black")) + 
        ggtitle("Relationship between Population and Proportion of Adult Citizen by State") +
        ylab("Proportion of Adult Citizen (% of Population)") 

pop_vs_voter_cor <- cor(nss4$Population, nss4$Adult_Citizen_Proportion)

# data cleaning for tables 
nss5_1 <- nss4 %>%
        arrange(desc(Population)) 
nss5_2 <- nss4 %>% 
        arrange(desc(Adult_Citizen_Proportion))

pop_table <- data.frame(Top_States = nss5_1$State[1:10], 
                        Top_Population = nss5_1$Population[1:10],
                        Bottom_States = nss5_1$State[42:51], 
                        Bottom_Population = nss5_1$Population[42:51])

voter_prop_table <- data.frame(Top_States = nss5_2$State[1:10],
                               Top_Proportion = nss5_2$Adult_Citizen_Proportion[1:10],
                               Bottom_States = nss5_2$State[42:51],
                               Bottom_Proportion = nss5_2$Adult_Citizen_Proportion[42:51])

# tables 
pop_table_display <- formattable(pop_table, list(Top_Population = color_tile("#FFFFCC", "#FFCC00"),
                                                 Bottom_Population = color_tile("#CCFFFF", "#3399FF")))

voter_prop_table_display <- formattable(voter_prop_table, list(Top_Proportion = color_tile("#CCFF66", "#99CC33"),
                                                               Bottom_Proportion = color_tile("#99FFCC", "#00CC66")))

# electoral college map 
st2 <- data.frame(region = tolower(st1$stname), value = st1$Electoral_College)
elec_col_map <- draw_map(st2, "Electoral College by State", "Electoral Votes", 1)

# data cleaning for electoral college per state population
nss6 <- nss %>%
        filter(GEONAME != "United States", LNTITLE == "Total", GEONAME != "Puerto Rico") %>% 
        select(GEONAME, TOT_EST, CVAP_EST) %>% 
        mutate(GEONAME = tolower(GEONAME)) %>%
        inner_join(st2, by = c("GEONAME" = "region")) 

nss7 <- nss6 %>%
        mutate(Per_Total = round(value / TOT_EST * 1000000, digits = 2),
               Per_Adult_Citizen = round(value / CVAP_EST * 1000000, digits = 2))

# mapping electoral college
elec_col_per_total_pop <- data.frame(
        region = nss7$GEONAME,
        value = nss7$Per_Total)

elec_col_per_voter <- data.frame(region = nss7$GEONAME,
                                 value = nss7$Per_Adult_Citizen)

elec_col_per_total_pop_map <- draw_map(elec_col_per_total_pop, 
                                       "Electoral College per State Total Population",
                                       "Electoral Votes \n (per Million People)",
                                       1)

elec_col_per_voter_map <- draw_map(elec_col_per_voter,
                                   "Electoral College per State Adult Citizen",
                                   "Electoral Votes \n (per Million People)",
                                   1)

# data cleaning for plotting relationship btw state population and electoral college
nss8 <- nss7 %>% 
        rename(State = GEONAME, 
               Total_Population = TOT_EST,
               Adult_Citizen_Population = CVAP_EST,
               Electoral_College = value,
               Electoral_College_Per_Total_Population = Per_Total,
               Electoral_College_Per_Adult_Citizen_Population = Per_Adult_Citizen)

# plotting 

elect_col_vs_elect_col_per_voters_plot <- ggplot(nss8, 
                                                 aes(x = Adult_Citizen_Population,
                                                     y = Electoral_College_Per_Adult_Citizen_Population)) +
        geom_point(size = 3, col = "#006600", alpha = 0.5) + 
        geom_smooth(col = "blue", se = FALSE) + 
        theme(panel.background = element_rect(fill = "white"), 
              axis.ticks = element_line(color = "black"), 
              axis.line = element_line(color = "black")) + 
        ggtitle("Relationship between Adult Citizen Population \n and Relative Number of Electoral Votes by State") +
        xlab("Number of Adult Citizens") +
        ylab("Electoral Votes \n(per Million Adult Citizens)") 
        

elec_col_vs_voter_plot <- ggplot(nss8, aes(x = Adult_Citizen_Population, y = Electoral_College)) + 
        geom_point(size = 3, col = "#9900CC", alpha = 0.5) + 
        geom_smooth(col = "blue", se = FALSE) + 
        theme(panel.background = element_rect(fill = "white"), 
              axis.ticks = element_line(color = "black"), 
              axis.line = element_line(color = "black")) + 
        ggtitle("Relationship between Adult Citizen Population \n and Absolute Number of Electoral Votes by State") +
        xlab("Number of Adult Citizens") +
        ylab("Electoral Votes") 

# correlation btw adult citizen and electoral college = 0.997
elec_col_vs_voter_cor <- cor(nss8$Adult_Citizen_Population, nss8$Electoral_College)
