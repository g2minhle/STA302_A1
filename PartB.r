marriage.csv <- read.table("marriage.csv", head=TRUE, sep=",")
marriageWithDateBreakDown <- within(marriage.csv, {
  Year <- as.numeric(substr(TIME_PERIOD , 1, 4))
  Month <- month.abb[as.numeric(substr(TIME_PERIOD , 6, 7))]
})
totalLicenses <- with(marriage.csv, sum(MARRIAGE_LICENSES))
save(totalLicenses ,file="data.Rda")
marriageByCivicCentre <- with(marriageWithDateBreakDown, tapply(MARRIAGE_LICENSES , CIVIC_CENTRE, sum))
marriageByMonthAndYear <- with(marriageWithDateBreakDown, tapply(MARRIAGE_LICENSES , list(Month,Year) , sum))[month.abb,]
marriageByCivicCenterAndMonth <- with(marriageWithDateBreakDown, tapply(MARRIAGE_LICENSES , list(CIVIC_CENTRE,Month) , sum))

barplot(marriageByCivicCentre[attr(marriageByCivicCentre, "dimnames")[[1]][order(-marriageByCivicCentre)]])
barplot(marriageByCivicCenterAndMonth[,month.abb])

businessLicences.csv <- read.csv("businessLicences.csv", head=T, stringsAsFactors = F)
businessLicencesCleaned <- within(businessLicences.csv, {
  Issued <- as.Date(Issued, format = "%d/%m/%y")
})

businessLicencesWithDateBreakDown <- within(businessLicencesCleaned, {
  Year <- as.numeric(substr(as.character(Issued), 1, 4))
  Month <- month.abb[as.numeric(substr(as.character(Issued) , 6, 7))]
})

businessLicencesWithDateOnly <- businessLicencesWithDateBreakDown[,c('Month','Year')]
marriageWithDateOnly <- marriageWithDateBreakDown[,c('Month','Year')]

businessLicencesWithDataDate <- within(businessLicencesWithDateOnly,{
  YYYYMM <- Year* 100 + match(Month, month.abb)
})

marriageDataDate <- unique(within(marriageWithDateOnly, {
  YYYYMM <- Year* 100 + match(Month, month.abb)
})["YYYYMM"])

businessLicencesWithDataDateLeaned <- 
  businessLicencesWithDataDate[businessLicencesWithDataDate$YYYYMM %in% marriageDataDate$YYYYMM,]

businessLicencesWithDataDateLeanedWithOnes <- within(businessLicencesWithDataDateLeaned,{
  Count <- Year/Year
})

businessLicencesByMonthAndYear <- 
  with(businessLicencesWithDataDateLeanedWithOnes, tapply(Count , list(Month,Year) , sum))[month.abb,]

combineData <- cbind(c(businessLicencesByMonthAndYear), c(marriageByMonthAndYear))
#colnames(combineData) <- c("Business Licenses (X)","Marriage Licenses (Y)")
colnames(combineData) <- c("X","Y")
plot(combineData)
lmModel = lm(Y ~ X , as.data.frame(combineData))
abline()

#predict(lmModel)