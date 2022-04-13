library(tidyverse)
library(lubridate)              #Date Library
library(treemapify)             #Library for Treemap
library(ggplotify)
library(forcats)                #Arranging Data frame
library(readxl)                 #Reading Excel files
library(ggthemes)               #theme for solarized
library(extrafont)              #font
font_import()
loadfonts(device = "win")

diseases <- read.csv("C:/Users/ishan/OneDrive/Desktop/Data Viz/disease.csv")
View(diseases)

#Summarising every disease vaccinations by count
diseases_count <- diseases %>% 
  group_by(disease) %>% 
  summarise(Frequency = sum(count))


ggplot(diseases_count,
       aes(x = Frequency, y = disease)) + 
  geom_col(fill = "magenta1") + 
  scale_y_discrete(limits = rev) +
  theme_bw() + 
  labs(title = "Disease VS Number of vaccinations: 2001-2018\n", 
       x = "\nNumber of Vaccinations") + 
  theme(panel.grid.major.y = element_blank(),
        plot.background = element_rect(fill="#111111"),
        axis.text = element_text(color= "white"),
        axis.title.y = element_blank(),
        axis.title = element_text(color = "white"),
        plot.title = element_text(color = "white")
        ) +
  theme(legend.position ="none") 



#Using Log scale to understand the data better

#Arranging in descending order
diseases_count1 <- diseases_count %>%
  mutate(disease = fct_reorder(disease, Frequency, .desc = TRUE))


ggplot(diseases_count1,
       aes(x = log10(Frequency), xend = 0 , y = disease, 
           yend = disease, colour = disease)) + 
  geom_vline(xintercept = 4, linetype = 2, colour = "white") +
  geom_text(x = 4.05, y = 10.5,color = "white", label = "Exponential Scale", 
            hjust = 0, size = 11 * 0.8 / .pt) + 
  geom_segment(size = 0.8) +
  geom_point(size = 1.8) +
  scale_y_discrete(limits = rev) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.1))) +
  theme_bw() + 
  labs(title = "Disease VS Number of vaccinations: 2001-2018\n", 
       x = "\nNumber of Vaccinations (Scaled)") + 
  theme(panel.grid.major = element_blank(),
        axis.title.y = element_blank(),
        legend.position ="none",
        panel.background = element_rect(fill = "black")
  ) 


#Visualisation 2
#Finding which county has had the most vaccinations in total for these 
#11 diseases for 18 years
county_count <- diseases %>% 
  group_by(county) %>% 
  summarise(Frequency = sum(count))
View(county_count)

#Sorting counties with more vaccinations
county_count <- county_count %>% 
  arrange(desc(Frequency))

top9 <- county_count[1:9,]
View(top9)


ggplot(top9, aes(area = Frequency, fill = Frequency, label = county)) +
  labs(title = "Vaccines by County (Top 9)")+
  geom_treemap() + 
  geom_treemap_text(place = "centre", color = "white",
                    size = 15,
                    grow = TRUE, angle = 30) +
  theme(plot.title = element_text(hjust = 0.5), legend.position ="none")
  
#Visualisation 3 
#We focus on vaccines in California, Los Angeles and San Diego
#We also find that Pertussis, IMD and Hepatitis A are the more prominent
#diseases that are vaccinated against

county_3 <- subset(diseases, subset = (county %in% c("California",
                                                  "Los Angeles",
                                                  "San Diego")))
View(county_3)


#For pertussis, IMD and Hepatitis-A across top 3 counties
pertussis <- subset(county_3, subset = (disease %in% c("Pertussis",
                                                       "Invasive Meningococcal Disease",
                                                       "Hepatitis A")) &
                       (county %in% c("California","Los Angeles",
                                       "San Diego")))
View(pertussis)

plotall1 <- pertussis %>%
  ggplot( aes(x = year, y = (count), color = county)) +
  geom_path(size = 1.2) + 
  theme(legend.position ="none", axis.title = element_blank(),
        strip.background = element_rect(colour="black", fill="skyblue", 
                                        size=1.4, linetype="solid"),
        plot.background = element_rect(fill = "#BFD5E3"),
        panel.grid.major = element_blank(),
        panel.spacing = unit(1, "lines")
        ) +
  ggtitle("Vaccinations - Hepatitis A, I.M.D., Pertussis across California, Los Angeles and San Diego") 
plotall1 + facet_grid(disease ~ county, scales = 'free')



#Visualisation 4
cases_pertussis <- read_excel("C:/Users/ishan/OneDrive/Desktop/Data Viz/pertussis.xlsx")

cali_pertussis <- subset(pertussis, subset = (county == "California") & 
                           (disease == "Pertussis"))
View(cali_pertussis)
pert2000 <- subset(cases_pertussis, subset = (year >= 2001)&(year != 2019))
View(pert2000)

plotcalipert <- full_join(cali_pertussis, pert2000, by="year")
View(plotcalipert$Cases)

ggplot(plotcalipert, aes(x = year)) +
  geom_line(size = 2, colour = "red", aes(y = Cases)) + 
  geom_area(aes(y = Cases), fill = "red",alpha = 0.8,) + 
  geom_line(size = 2, colour = "chartreuse", 
                                         aes(y = count)) +
  geom_area(aes(y = count), fill = "chartreuse", alpha = 0.8) + 
  theme_solarized_2(light = FALSE) + 
  labs(title = "Pertussis- Cases Vs Vaccines in California",
       y = " ", x = "\nYear") +
  geom_text(x = 2014.5, y = 11000,color = "white", label = "Vaccines", 
            hjust = 0, size = 11 * 0.8 / .pt) +
  geom_text(x = 2014.2, y = 34200,color = "white", label = "Cases", 
            hjust = 0, size = 11 * 0.8 / .pt) + 
  theme(text = element_text(colour = "#EEEEEE"),
        title = element_text(colour = "#EEEEEE"),
        axis.title.x = element_text(colour = "#EEEEEE"),
        panel.background = element_rect(fill = NA),
        plot.background = element_rect(fill = "#111111"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5)) 


#Visualisation 4B
#Plotting Cases vs Vaccines Overall and California

#Joining plotallpert and plotcalipert
ploteverythingpert <- full_join(plotallpert, plotcalipert, by="year")
View(ploteverythingpert)


ggplot(ploteverythingpert, aes(x = year)) +
  geom_line(size = 2, colour = "red", aes(y = Cases.x)) + 
  geom_area(aes(y = Cases.x), fill = "red", alpha = 0.75) + 
  geom_line(size = 2, colour = "blue", aes(y = Frequency)) +
  geom_area(aes(y = Frequency),fill = "blue", alpha = 0.75) + 
  geom_line(size = 2, colour = "chartreuse", aes(y = count)) + 
  geom_area(aes(y = count), fill = "chartreuse") + 
  
  theme_solarized_2(light = FALSE) + 
  labs(title = "Pertussis- Cases Vs Vaccines- B",
       y = " ", x = "\nYear") +
  geom_text(x = 2015.3, y = 11000,color = "white", label = "Vaccines", 
            hjust = 0, size = 11 * 0.8 / .pt) +
  geom_text(x = 2014.2, y = 34200,color = "white", label = "Cases", 
            hjust = 0, size = 11 * 0.8 / .pt) + 
  geom_text(x = 2013.4, y = 4500,color = "black", label = "Vaccine- \nCalifornia", 
            hjust = 0, size = 11 * 0.8 / .pt) + 
  theme(text = element_text(colour = "#EEEEEE"),
        title = element_text(colour = "#EEEEEE"),
        axis.title.x = element_text(colour = "#EEEEEE"),
        panel.background = element_rect(fill = NA),
        plot.background = element_rect(fill = "#111111"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5)) 



#Visualisation 5
#Invasive Meningococcal Disease

cases_meningo <- read_excel("C:/Users/ishan/OneDrive/Desktop/Data Viz/meningococcal.xlsx")
View(cases_meningo)
#Number of cases calculated by multiplying the rate (per 100,000) * the population that year
cases_meningo$Cases <- cases_meningo$rate * cases_meningo$population / 100000

#Counting Vaccines for California for I.M.D from vaccine dataset
imdcali <- subset(diseases, subset = (county == "California") &
                    (disease == "Invasive Meningococcal Disease"))
View(imdcali)

#Joining the total IMD cases to California vaccines
imdcali1 <- full_join(cases_meningo, imdcali, by = "year")
View(imdcali1)

#Plotting Cases vs Count for IMD in California
ggplot(imdcali1, aes(x = year)) +
  geom_line(size = 2, colour = "yellow", aes(y = Cases)) + 
  geom_area(aes(y = Cases), fill = "yellow",alpha = 1) + 
  geom_line(size = 2, colour = "limegreen", aes(y = count)) +
  geom_area(aes(y = count), fill = "limegreen", alpha = 1) + 
  theme_solarized_2(light = FALSE) + 
  labs(title = "I.M.D.- Cases Vs Vaccines in California",
       y = " ", x = "\nYear") +
  geom_text(x = 2005, y = 1400,color = "yellow", label = "Cases", 
            hjust = 0, size = 5) +
  geom_text(x = 2005, y = 370,color = "limegreen", label = "Vaccines", 
            hjust = 0, size = 5) + 
  theme(text = element_text(colour = "#EEEEEE"),
        title = element_text(colour = "#EEEEEE"),
        axis.title.x = element_text(colour = "#EEEEEE"),
        panel.background = element_rect(fill = NA),
        plot.background = element_rect(fill = "#111111"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5)) 



#Plotting Cases vs Vaccine for California AND Other counties
imdcounty <- subset(diseases, subset = (disease == "Invasive Meningococcal Disease"))
imdcounty1 <- imdcounty %>%
  group_by(year) %>% 
  summarise(Frequency = sum(count))
View(imdcounty1)

imdall <- full_join(imdcounty1, imdcali1, by = "year")
View(imdall)

#Plotting all 3 
ggplot(imdall, aes(x = year),) +
  geom_line(size = 2, colour = "orangered", aes(y = Cases)) + 
  geom_area(aes(y = Cases), fill = "orangered", alpha = 0.75) + 
  geom_line(size = 2, colour = "purple1", aes(y = Frequency)) +
  geom_area(aes(y = Frequency),fill = "purple1", alpha = 0.9) + 
  geom_line(size = 2, colour = "palegreen", aes(y = count)) + 
  geom_area(aes(y = count), fill = "palegreen") + 
  
  theme_solarized_2(light = FALSE) + 
  labs(title = "I.M.D- Cases Vs Vaccines- B",
       y = " ", x = "\nYear") +
  theme(text = element_text(colour = "#EEEEEE"),
        title = element_text(colour = "#EEEEEE"),
        axis.title.x = element_text(colour = "#EEEEEE"),
        panel.background = element_rect(fill = NA),
        plot.background = element_rect(fill = "#111111"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5)) 
