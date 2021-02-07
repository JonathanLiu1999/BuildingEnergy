library(dplyr)

ebewe<- read.csv("Existing_Buildings_Energy___Water_Efficiency__EBEWE__Program.csv")
colnames(ebewe)[3]<- "CO2"
ebewe$CO2<- as.numeric(ebewe$CO2)
ebewe$LADBS.Building.Category<- as.factor(ebewe$LADBS.Building.Category)
ebewe<- ebewe[which(is.na(ebewe$CO2) != T),]
colnames(ebewe)
ebewe<- ebewe[,-c(1,2,4,9,10,11,19,27)]

in1<- which(ebewe$PROPERTY.TYPE == "Multifamily Housing")
house.energy<- ebewe[in1, c(12,14)]
house.energy<- house.energy[-which(house.energy$SITE.ENERGY.USE.INTENSITY..EUI...kBtu.ft.. == "Not Available"),]
house.energy<- as.data.frame(tapply(as.numeric(house.energy$SITE.ENERGY.USE.INTENSITY..EUI...kBtu.ft..),  house.energy$POSTAL.CODE, median))
house.energy[,2]<- row.names(house.energy)
colnames(house.energy)<- c("Average Energy Use", "Zipcode")


year<- read.csv("Year.csv")
year<- year[-1,c(2,3)]
year$NAME<- substr(year$NAME, 7, 13)
colnames(year)<- c("Zipcode", "Median Year")
house.energy2<- left_join(house.energy, year, by = "Zipcode")
plot(x = house.energy2$`Median Year`, house.energy2$`Average Energy Use`)
house.energy2$`Median Year`<- substr(house.energy2$`Median Year`, 1, 4)
cor.test(house.energy2$`Average Energy Use`, as.numeric(house.energy2$`Median Year`))

assesor<- read.csv("parcels.csv")

aa<- as.data.frame(tapply(assesor$netTaxableValue, assesor$ZIPcode5, mean))
aa[,2]<- row.names(aa)
colnames(aa)<- c("total val", "Zipcode")
house.energy3<- left_join(house.energy, aa, by = "Zipcode")
house.energy3<- house.energy3 %>% filter(`total val` < 2*10^6)
plot(x=house.energy3$`total val`, y=house.energy3$`Average Energy Use`)
cor.test(x=house.energy3$`total val`, y=house.energy3$`Average Energy Use`)

plants<- ebewe %>%
  filter(PROPERTY.TYPE == "Manufacturing/Industrial Plant" ) %>%
  select(POSTAL.CODE, SITE.ENERGY.USE.INTENSITY..EUI...kBtu.ft.., POSTAL.CODE) %>%
  group_by(POSTAL.CODE) %>%
  summarise(mean.consumption = mean(SITE.ENERGY.USE.INTENSITY..EUI...kBtu.ft.., na.rm = T))

indus$naics<- substr(indus$naics, 1, 2)
indus1<- indus %>%
  group_by(zip) %>%
  summarise(bussiness.number = sum(est)) %>%
  filter(zip %in% office$POSTAL.CODE) %>% 
  rename("POSTAL.CODE" = "zip")
indus2<- indus %>%
  filter(naics %in% c( "31", "32", "33") )%>%
  group_by(zip) %>%
  summarise(bussiness.number = sum(est)) %>%
  filter(zip %in% office$POSTAL.CODE) %>% 
  rename("POSTAL.CODE" = "zip")
indus1<- left_join(indus1, indus2, "POSTAL.CODE")
indus1<- indus1 %>%
  mutate(percentage = bussiness.number.y/bussiness.number.x)
indu.office<- left_join(plants, indus1, "POSTAL.CODE")  
indu.office<- indu.office %>% filter(mean.consumption < 1000)
cor.test(indu.office$percentage, indu.office$mean.consumption)

ebewe %>%
  group_by(PROPERTY.TYPE) %>%
  summarise(a = n()) %>% 
  arrange(desc(a))

demo<- read.csv("demo.csv")
indus<- read.csv("indu.txt")

pop.den<- ebewe %>%
  filter(PROPERTY.TYPE == "Office" ) %>%
  select(POSTAL.CODE, SITE.ENERGY.USE.INTENSITY..EUI...kBtu.ft.., POSTAL.CODE) %>%
  group_by(POSTAL.CODE) %>%
  summarise(mean.consumption = mean(SITE.ENERGY.USE.INTENSITY..EUI...kBtu.ft.., na.rm = T))
pop.den$POSTAL.CODE<- as.character(pop.den$POSTAL.CODE)
aa<- aa %>%
  rename("POSTAL.CODE" = "Zipcode")
pop<-left_join(pop.den, aa, "POSTAL.CODE") %>%
  filter(mean.consumption < 300)

pop<- pop %>% filter(`total val`<1000000)

cor.test(pop$`total val`, pop$mean.consumption) # significant

library(rvest)
library(xml2)
scrape<- read_html("http://www.usa.com/rank/california-state--median-household-income--zip-code-rank.htm") %>%
  html_nodes("td") %>%
  html_text()
scrape<- scrape[-1:-5]
income<- scrape[seq(2, length(scrape), 3)]
zippop<- scrape[seq(3, length(scrape), 3)]

zipcode<- str_extract(zippop, "\\d+")
pop<- str_extract(zippop, "\\d+.\\d+$")
pop<- gsub(",", "", pop)
income<- gsub(",", "", income)
income<- gsub("\\$", "", income)

demo<- as.data.frame(cbind(as.numeric(income), as.character(zipcode), as.numeric(pop))) 
colnames(demo)<- c("income", "POSTAL.CODE", "population")

multi<- ebewe %>%
  filter(PROPERTY.TYPE == "Multifamily Housing") %>%
  select(POSTAL.CODE, SITE.ENERGY.USE.INTENSITY..EUI...kBtu.ft.., GROSS.BUILDING.FLOOR.AREA..ft..) %>%
  group_by(POSTAL.CODE) %>%
  summarise(energy.consumption = mean(SITE.ENERGY.USE.INTENSITY..EUI...kBtu.ft..*GROSS.BUILDING.FLOOR.AREA..ft.., na.rm = T))
multi$POSTAL.CODE<- as.character(multi$POSTAL.CODE)
com<- left_join(multi, demo, "POSTAL.CODE")
com$income<- as.numeric(com$income)
com<- com %>% filter(energy.consumption < 8000000)
com$population<- as.numeric(com$population)
cor.test(com$income, com$energy.consumption)


