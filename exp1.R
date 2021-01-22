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
house.energy<- as.data.frame(tapply(as.numeric(house.energy$SITE.ENERGY.USE.INTENSITY..EUI...kBtu.ft..),  house.energy$POSTAL.CODE, mean))
house.energy[,2]<- row.names(house.energy)
colnames(house.energy)<- c("Average Energy Use", "Zipcode")


year<- read.csv("Year.csv")
year<- year[-1,c(2,3)]
year$NAME<- substr(year$NAME, 7, 13)
colnames(year)<- c("Zipcode", "Median Year")
house.energy2<- left_join(house.energy, year, by = "Zipcode")
plot(x = house.energy2$`Median Year`, house.energy2$`Average Energy Use`)

house.energy2<- house.energy2 %>% filter(`Average Energy Use` < 200)
house.energy2$`Median Year`<- substr(house.energy2$`Median Year`, 1, 4)


cor.test(house.energy2$`Average Energy Use`, as.numeric(house.energy2$`Median Year`))


as.numeric(house.energy2$`Median Year`)
