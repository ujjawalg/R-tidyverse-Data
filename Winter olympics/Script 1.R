library(tidyverse)
library(readr)
library(ggthemes)
library(sqldf)
library(manipulateWidget)
library(ggplot2)
library(gridExtra)


Winter <- read.csv("winter.csv")

curr_country <- read.csv("dictionary.csv")

teams <- merge(x = Winter, y = curr_country, by = "Code", all = TRUE)


totalGold <- sqldf("select code, count(Medal)as totalG from teams where Medal = 'Gold' group by code")

totalSilver <- sqldf("select code, count(Medal)as totalS from teams where Medal = 'Silver' group by code")

totalBronze <- sqldf("select code, count(Medal)as totalB from teams where Medal = 'Bronze' group by code")

totalMedal <- merge(x = curr_country, y = totalGold, by = "Code", all = TRUE)

totalMedal <- merge(x = totalMedal, y = totalSilver, by = "Code", all = TRUE)

totalMedal <- merge(x = totalMedal, y = totalBronze, by = "Code", all = TRUE)

#Ques 1
 
  TempGold <- count(totalMedal,Code) %>% filter(totalMedal$totalG > 25)
  TGold <- filter(totalMedal, Code %in% TempGold$Code)
  
  subplot1 <- ggplot(data = TGold, aes(x = reorder(Code, -totalG), y = totalG, na.rm = FALSE))+
    geom_bar(stat= "identity",show.legend = FALSE, fill = "Gold")+
    xlab("")+
    ylab("Total number of Medals")+
  theme_bw()+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text.x = element_text(angle=90, size=rel(0.8), hjust=1))+ ggtitle("Top Gold Medal Winners")
  subplot1

  TempSilver <- count(totalMedal,Code) %>% filter(totalMedal$totalS > 25)
  TSilver <- filter(totalMedal, Code %in% TempSilver$Code)
  
  subplot2 <- ggplot(data = TSilver, aes(x = reorder(Code, -totalS), y = totalS, na.rm = FALSE))+
    geom_bar(stat= "identity",show.legend = FALSE, fill = "#bdbdbd")+
    xlab("")+
    ylab("")+
    theme_bw()+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text.x = element_text(angle=90, size=rel(0.8), hjust=1))+ ggtitle("Top Silver Medal Winners")
  subplot2
  
  TempBronze <- count(totalMedal,Code) %>% filter(totalMedal$totalB > 25)
  TBronze <- filter(totalMedal, Code %in% TempBronze$Code)
  
  subplot3 <- ggplot(data = TBronze, aes(x = reorder(Code, -totalB), y = totalB, na.rm = FALSE))+
    geom_bar(stat= "identity",show.legend = FALSE, fill = "#f03b20")+
    xlab("")+
    ylab("")+
    theme_bw()+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text.x = element_text(angle=90, size=rel(0.8), hjust=1))+ ggtitle("Top Bronze Medal Winners")
  subplot3
  
  Ans1 <- grid.arrange(subplot1, subplot2, subplot3, ncol=3, top = "Medal Distribution Winter olympics 1924 - 2014")
  
  #Ques 2
  
  totalMedal$Points <- ((totalMedal$totalG*3)+(totalMedal$totalS*2)+(totalMedal$totalB*1))
  
  totalMedal$GDPpPoints <- (totalMedal$Points/totalMedal$GDP.per.Capita)
  
  TempPoints <- count(totalMedal,Code) %>% filter(totalMedal$Points > 500)
  TPoints <- filter(totalMedal, Code %in% TempPoints$Code)
  
  plot2 <- ggplot(data = TPoints, aes(x = reorder(Code, -Points), y = Points, na.rm = FALSE))+
    geom_bar(stat= "identity",show.legend = FALSE, fill = "Blue" )+
    xlab("")+
    ylab("Points")+
    theme_economist()+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text.x = element_text(angle=90, size=rel(0.8), hjust=1))+ ggtitle("Most Successful Countries based on Points", subtitle = "Points = Gold*3 + Silver*2 + Bronze*1")
  plot2
  
  TempGDP <- count(totalMedal,Code) %>% filter(totalMedal$GDPpPoints > 0.01)
  TGDP <- filter(totalMedal, Code %in% TempGDP$Code)
  
  plot3 <- ggplot(data = TGDP, aes(x = reorder(Code, -GDPpPoints), y = GDPpPoints, na.rm = FALSE))+
    geom_bar(stat= "identity",show.legend = FALSE, fill = "Gold" )+
    xlab("")+
    ylab("Points/GDP per capita")+
    theme_economist()+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text.x = element_text(angle=90, size=rel(0.8), hjust=1))+ ggtitle("Most Successful Countries based on Points/GDP per capita")
  plot3
  
  totalMedal$PoppPoints <- (totalMedal$Points/totalMedal$Population)
  
  TempPop <- count(totalMedal,Code) %>% filter(totalMedal$PoppPoints > 0)
  TPop <- filter(totalMedal, Code %in% TempPop$Code)
  
  plot4 <- ggplot(data = TPop, aes(x = reorder(Code, -PoppPoints), y = PoppPoints, na.rm = FALSE))+
    geom_bar(stat= "identity",show.legend = FALSE, fill = "Green" )+
    xlab("")+
    ylab("Points/Population")+
    theme_economist()+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text.x = element_text(angle=90, size=rel(0.8), hjust=1))+ ggtitle("Most Successful Countries based on Points/Population")
  plot4
  
  Ans2 <- grid.arrange(plot2, plot3, plot4, nrow=3, top = "Most Successful Countries Winter olympics 1924 - 2014")
  
  #Ques 3
  
  
  library(rvest)
  library(stringr)
  wiki_hosts <- read_html("https://en.wikipedia.org/wiki/Winter_Olympic_Games")
  hosts <- html_table(html_nodes(wiki_hosts, "table")[[5]], fill=TRUE)
  hosts <- hosts[-1,1:3]
  hosts$city <- str_split_fixed(hosts$Host, n=2, ",")[,1]
  hosts$country <- str_split_fixed(hosts$Host, n=2, ",")[,2]
  
  HostCountry <- merge(x = Winter, y = hosts, by = "Year", all = TRUE)
  
  HostCountry$city <- NULL
  
  HostMedals <- sqldf("select Year,code,country, count(Medal)as totalH from HostCountry group by Year,code,country")
  
  HostMedals1 <- sqldf("select Year, Count(Medal)as Total from HostCountry group by Year")
  
  HostMedals <- merge(x = HostMedals, y = HostMedals1, by = "Year", all = TRUE)
  
  HostMedals$Percentage <- (HostMedals$totalH/HostMedals$Total)*100
  
  curr_country <- read.csv("dictionary.csv")
  
  curr_country$Hostcountry <- curr_country$Country
  
  HostMedals <- merge(x = HostMedals, y = curr_country, by = "Code", all = TRUE)
  
  HostMedals$Population <- NULL
  
  HostMedals$GDP.per.Capita <- NULL
  
  HostMedals$Country <- NULL
  
  TempHost <- read.csv("Host.csv")
  THostMedals <- filter(HostMedals, Code %in% TempHost$Code)
  
  plot5 <- ggplot(data=THostMedals, aes(colour = Hostcountry, x=Year, y=Percentage, group = Hostcountry)) + 
    geom_point(size = 1.5) +
    geom_line()+
    ylab("Percentage of Medals Won") +
    xlab("") + 
    theme_bw()+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text.x = element_text(angle=90, size=rel(0.8), hjust=1))+ ggtitle("Host Nations Medal Percentage share trend", sub = "France 1924,1968,1992 Switzerland 1928,1948 USA 1932,1960,1982,2002 Germany 1936 Norway 1952,1994 Italy 1956,2006 Austria 1964,1976 Japan 1972,1998 Canada 1988,2010 Russia 2014") 
  
  plot5
  
  #Ques 4
  
  Event <- sqldf("Select Year,Sport,Event,Code,Medal,Gender,Count(Medal)as MedalsWon from Winter group by Year,Sport,Event,Code,Medal,Gender")
  
  Event$MedalsWon <- NULL
  
  Eventdis <- sqldf("Select Code,Sport, Gender, Count(Medal) as Countwon
                     from Event group by Code,Sport,Gender")
  
  Icehockey <- sqldf("Select Code, Gender,Countwon
                     from Eventdis where Sport = 'Ice Hockey'")
  
  Ans4 <- ggplot(Icehockey, aes(fill=Gender, y=Countwon, x= reorder(Code, -Countwon))) +
    geom_bar(stat="identity")+xlab("")+
    ylab("Total Medals Won")+
    theme_bw()+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text.x = element_text(angle=90, size=rel(1), hjust=0.5))+ ggtitle("Most Successful countries in Ice Hockey Winter Olympics")
  
  Ans4
  
  Eventtotal <- sqldf("Select Code,Sport, Count(Medal) as Countwon
                     from Event group by Code,Sport")
  
  Totalmed <- sqldf("Select Code, Count(Medal) as Countwon
                     from Event group by Code order by Countwon DESC limit 10")
  Top10 <- filter(Eventtotal, Code %in% Totalmed$Code)
  
  Ans4_2 <- ggplot(Top10, aes(fill=Sport, y=Countwon, x= reorder(Code, -Countwon))) +
    geom_bar(stat="identity")+xlab("")+
    ylab("Total Medals Won")+
    theme_bw()+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text.x = element_text(angle=90, size=rel(1), hjust=0.5))+ ggtitle("Sportwise Medal Distribution of 10 Most Successful countries in Winter Olympics")
  
  Ans4_2
  
  #Ques 5
  SuccessAthlete <- sqldf("Select Athlete,Gender,Sport,Code,Count(Athlete)as MedalsWon from Winter group by Athlete")
  
   SuccessMale <- sqldf("Select Athlete,Gender,Sport,Code,MedalsWon from SuccessAthlete where Gender='Men' order by MedalsWon DESC limit 10")
   
   SuccessFemale <- sqldf("Select Athlete,Gender,Sport,Code,MedalsWon from SuccessAthlete where Gender='Women' order by MedalsWon DESC limit 10")
   
   plot6 <- ggplot(data = SuccessMale, aes(x = reorder(Athlete, -MedalsWon), y = MedalsWon)) +
     geom_segment( aes(x= reorder(Athlete, -MedalsWon), xend=Athlete, y=0, yend=MedalsWon, color = Code, size = 0.1)) +
     geom_point( size=4, color="Black", fill=alpha("Black", 0.3), alpha=0.5, shape=21, stroke=1, show.legend = FALSE)+
     xlab("")+
     ylab("Total Medals Won")+
     theme_classic()+
     theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text.x = element_text(angle=90, size=rel(1), hjust=0.5))+ ggtitle("Top 10 Most Successful Male Athletes of Winter Olympics")
   
   plot6
   
   plot7 <- ggplot(data = SuccessFemale, aes(x = reorder(Athlete, -MedalsWon), y = MedalsWon, fill = Sport)) +
     geom_segment( aes(x= reorder(Athlete, -MedalsWon), xend=Athlete, y=0, yend=MedalsWon, color = Sport, size = 0.1)) +
     geom_point( size=4, color="Black", fill=alpha("Black", 0.3), alpha=0.5, shape=21, stroke=1)+
     xlab("")+
     ylab("Total Medals Won")+
     theme_classic()+
     theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text.x = element_text(angle=90, size=rel(1), hjust=0.5))+ ggtitle("Top 10 Most Successful Female Athletes of Winter Olympics")
   
   plot7
   
   
  #Ques 6
   
  library(plotly)
  
  ggplotly(plot5)
  
  ggplotly(plot6)
  
  ggplotly(plot7)
  
  
  #Ques 7
  
  library(DT)
  
  table1 <- Event %>%
    select(Year, Sport, Event, Gender, Code, Medal) %>%
    arrange(Year,Sport)  %>%
    datatable(rownames = FALSE,
              colnames = c("Year", "Sport", "Subcategory", "Participant", "Winning_Country", "Medal"), 
              caption =  "Winter Olympics Winners 1924 - 2014", 
              filter = "top")
  
  table1
  