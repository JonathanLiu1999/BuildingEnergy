library(dplyr)

# Setting up the EBEWE Dataset

ebewe<- read.csv("Existing_Buildings_Energy___Water_Efficiency__EBEWE__Program.csv")
colnames(ebewe)[3]<- "CO2"
ebewe$CO2<- as.numeric(ebewe$CO2)
ebewe$LADBS.Building.Category<- as.factor(ebewe$LADBS.Building.Category)
ebewe<- ebewe[which(is.na(ebewe$CO2) != T),]
ebewe<- ebewe[,-c(1,2,4,9,10,11,19,27)]

in1<- which(ebewe$PROPERTY.TYPE == "Multifamily Housing")
house.energy<- ebewe[in1, c(12,14)]
house.energy<- house.energy[-which(house.energy$SITE.ENERGY.USE.INTENSITY..EUI...kBtu.ft.. == "Not Available"),]
house.energy<- as.data.frame(tapply(as.numeric(house.energy$SITE.ENERGY.USE.INTENSITY..EUI...kBtu.ft..),  house.energy$POSTAL.CODE, median))
house.energy[,2]<- row.names(house.energy)
colnames(house.energy)<- c("Average Energy Use", "Zipcode")

# Importing Assesor Dataset

assesor<- read.csv("parcels.csv")
aa<- as.data.frame(tapply(assesor$netTaxableValue, assesor$ZIPcode5, mean))
aa[,2]<- row.names(aa)
colnames(aa)<- c("total val", "Zipcode")
house.energy3<- left_join(house.energy, aa, by = "Zipcode")
house.energy3<- house.energy3 %>% filter(`total val` < 2*10^6)
plot(x=house.energy3$`total val`, y=house.energy3$`Average Energy Use`)
cor.test(x=house.energy3$`total val`, y=house.energy3$`Average Energy Use`) # Significant
m1<- lm(house.energy3$`Average Energy Use`~log(house.energy3$`total val`))
summary(m1)
# Scraping Income 

library(rvest)
webpage<- "http://www.laalmanac.com/employment/em12c.php"
income<- read_html(webpage) %>%
  html_nodes("td:nth-child(3) , td:nth-child(1)") %>%
  html_text()

income.zipcode<- income[seq(1,length(income),2)] %>% as.numeric()
income.income<- income[seq(2,length(income),2)]
income.income<- gsub("\\$","",income.income)
income.income<- gsub(",","",income.income)
income<- as.data.frame(cbind(income.zipcode, income.income))
colnames(income)<- c("Zipcode", "Income")
  
house.energy3<- left_join(house.energy3, income, "Zipcode")


summary(house.energy3$`Average Energy Use`)

boxplot(house.energy3$`Average Energy Use`)
find.range<- function(data){
  a<- IQR(data)
  summar<- unname(summary(data))
  c(summar[2]-1.5*a, summar[5]+1.5*a)
}
find.range(house.energy3$`Average Energy Use`)

# Saving Data
write.csv(house.energy3, "house.csv")
