##Install the packages
{
  #install.packages("keras")
  library(keras)
  #install_keras()
  
  #install.packages("tensorflow")
  library(tensorflow)
  #install_tensorflow()
  
  library(tensorflow)
  tf$constant("Hellow Tensorflow")
  
  
  #install.packages("magrittr")
  library(magrittr) 
  library(foreign)
  library(dplyr)
  library(dummy)
}


normalize<-function(x) {
  y<-as.double(x)
  #z<-(y-min(y))/(max(y)-min(y))
  return(y)
}

##Dummy variable
make_dummy <- function(a_vector){ 
  temp <- as.integer(as.factor(a_vector))
  temp <- temp - min(temp, na.rm=TRUE)
  count_values <- length(names(table(temp)))
  temp[is.na(temp)] <- count_values
  temp <- as.factor(temp)
  nubble <- as.data.frame(lapply(dummy(data.frame(temp),verbose=T),as.double))
  normalized_nubble <-apply(nubble,2,normalize)
  return(normalized_nubble)
} 



##G4 data
Country_G_4_dummy <- function(x){
  Country_books_4_dummy <- make_dummy(x$ASBG04)
  Country_gender_4_dummy <- make_dummy(x$ASBG01)
  Country_language_4_dummy <- make_dummy(x$ASBG03)
  Country_bq1_4_dummy <- make_dummy(x$ASBG05A)
  Country_bq2_4_dummy <- make_dummy(x$ASBG05C)
  Country_bq3_4_dummy <- make_dummy(x$ASBG05D)
  Country_bq4_4_dummy <- make_dummy(x$ASBG05E)
  Country_bq5_4_dummy <- make_dummy(x$ASBG12B)
  Country_bq6_4_dummy <- make_dummy(x$ASBG12C)
  Country_bq7_4_dummy <- make_dummy(x$ASBG12D)
  Country_bq8_4_dummy <- make_dummy(x$ASBG12E)
  Country_bq9_4_dummy <- make_dummy(x$ASBG12G)
  Country_bq10_4_dummy <- make_dummy(x$ASBG12H)
  Country_bq11_4_dummy <- make_dummy(x$ASBG11A)
  Country_bq12_4_dummy <- make_dummy(x$ASBG11B)
  Country_bq13_4_dummy <- make_dummy(x$ASBG11C)
  Country_bq14_4_dummy <- make_dummy(x$ASBG11E)
  Country_bq15_4_dummy <- make_dummy(x$ASBG11F)
  Country_bq16_4_dummy <- make_dummy(x$ASBM01A)
  Country_bq17_4_dummy <- make_dummy(x$ASBM01B)
  Country_bq18_4_dummy <- make_dummy(x$ASBM01C)
  Country_bq19_4_dummy <- make_dummy(x$ASBM01D)
  Country_bq20_4_dummy <- make_dummy(x$ASBM01E)
  Country_bq21_4_dummy <- make_dummy(x$ASBM01F)
  Country_bq22_4_dummy <- make_dummy(x$ASBM01G)
  Country_bq23_4_dummy <- make_dummy(x$ASBM01H)
  Country_bq24_4_dummy <- make_dummy(x$ASBM01I)
  Country_bq25_4_dummy <- make_dummy(x$ASBM02A)
  Country_bq26_4_dummy <- make_dummy(x$ASBM02B)
  Country_bq27_4_dummy <- make_dummy(x$ASBM02E)
  Country_bq28_4_dummy <- make_dummy(x$ASBM02F)
  Country_bq29_4_dummy <- make_dummy(x$ASBM02H)
  Country_bq30_4_dummy <- make_dummy(x$ASBM03A)
  Country_bq31_4_dummy <- make_dummy(x$ASBM03B)
  Country_bq32_4_dummy <- make_dummy(x$ASBM03C)
  Country_bq33_4_dummy <- make_dummy(x$ASBM03D)
  Country_bq34_4_dummy <- make_dummy(x$ASBM03E)
  Country_bq35_4_dummy <- make_dummy(x$ASBM03F)
  Country_bq36_4_dummy <- make_dummy(x$ASBM03G)
  Country_bq37_4_dummy <- make_dummy(x$ASBM03H)
  Country_bq38_4_dummy <- make_dummy(x$ASBM03I)
  
  Country_4_matM <- (as.double(as.vector(x$ASMMAT01))
                     +as.double(as.vector(x$ASMMAT02))
                     +as.double(as.vector(x$ASMMAT03))
                     +as.double(as.vector(x$ASMMAT04))
                     +as.double(as.vector(x$ASMMAT05)))/5000

  Country_grade_indicator <- rep(c(0),nrow(Country_bq30_4_dummy))
  return(data.frame(cbind(Country_books_4_dummy,Country_gender_4_dummy,Country_language_4_dummy,Country_bq1_4_dummy,Country_bq2_4_dummy,Country_bq3_4_dummy,Country_bq4_4_dummy,Country_bq5_4_dummy,
                          Country_bq6_4_dummy,Country_bq7_4_dummy,Country_bq8_4_dummy,Country_bq9_4_dummy,Country_bq10_4_dummy,Country_bq11_4_dummy,Country_bq12_4_dummy,Country_bq13_4_dummy,
                          Country_bq14_4_dummy,Country_bq15_4_dummy,Country_bq16_4_dummy,Country_bq17_4_dummy,Country_bq18_4_dummy,Country_bq19_4_dummy,Country_bq20_4_dummy,Country_bq21_4_dummy,
                          Country_bq22_4_dummy,Country_bq23_4_dummy,Country_bq24_4_dummy,Country_bq25_4_dummy,Country_bq26_4_dummy,Country_bq27_4_dummy,Country_bq28_4_dummy,Country_bq29_4_dummy,
                          Country_bq30_4_dummy,Country_bq31_4_dummy,Country_bq32_4_dummy,Country_bq33_4_dummy,Country_bq34_4_dummy,Country_bq35_4_dummy,Country_bq36_4_dummy,Country_bq37_4_dummy,
                          Country_bq38_4_dummy,Country_grade_indicator,Country_4_matM)))
}



##G8 data
Country_G_8_dummy <- function(y){
  Country_books_8_dummy <- make_dummy(y$BSBG04)
  Country_gender_8_dummy <- make_dummy(y$BSBG01)
  Country_language_8_dummy <- make_dummy(y$BSBG03)
  Country_bq1_8_dummy <- make_dummy(y$BSBG05A)
  Country_bq2_8_dummy <- make_dummy(y$BSBG05B)
  Country_bq3_8_dummy <- make_dummy(y$BSBG05C)
  Country_bq4_8_dummy <- make_dummy(y$BSBG05D)
  Country_bq5_8_dummy <- make_dummy(y$BSBG14M)
  Country_bq6_8_dummy <- make_dummy(y$BSBG14B)
  Country_bq7_8_dummy <- make_dummy(y$BSBG14F)
  Country_bq8_8_dummy <- make_dummy(y$BSBG14L)
  Country_bq9_8_dummy <- make_dummy(y$BSBG14J)
  Country_bq10_8_dummy <- make_dummy(y$BSBG14K)
  Country_bq11_8_dummy <- make_dummy(y$BSBG13A)
  Country_bq12_8_dummy <- make_dummy(y$BSBG13B)
  Country_bq13_8_dummy <- make_dummy(y$BSBG13C)
  Country_bq14_8_dummy <- make_dummy(y$BSBG13D)
  Country_bq15_8_dummy <- make_dummy(y$BSBG13E)
  Country_bq16_8_dummy <- make_dummy(y$BSBM16A)
  Country_bq17_8_dummy <- make_dummy(y$BSBM16B)
  Country_bq18_8_dummy <- make_dummy(y$BSBM16C)
  Country_bq19_8_dummy <- make_dummy(y$BSBM16D)
  Country_bq20_8_dummy <- make_dummy(y$BSBM16E)
  Country_bq21_8_dummy <- make_dummy(y$BSBM16F)
  Country_bq22_8_dummy <- make_dummy(y$BSBM16G)
  Country_bq23_8_dummy <- make_dummy(y$BSBM16H)
  Country_bq24_8_dummy <- make_dummy(y$BSBM16I)
  Country_bq25_8_dummy <- make_dummy(y$BSBM17A)
  Country_bq26_8_dummy <- make_dummy(y$BSBM17B)
  Country_bq27_8_dummy <- make_dummy(y$BSBM17C)
  Country_bq28_8_dummy <- make_dummy(y$BSBM17D)
  Country_bq29_8_dummy <- make_dummy(y$BSBM17E)
  Country_bq30_8_dummy <- make_dummy(y$BSBM19A)
  Country_bq31_8_dummy <- make_dummy(y$BSBM19B)
  Country_bq32_8_dummy <- make_dummy(y$BSBM19C)
  Country_bq33_8_dummy <- make_dummy(y$BSBM19D)
  Country_bq34_8_dummy <- make_dummy(y$BSBM19E)
  Country_bq35_8_dummy <- make_dummy(y$BSBM19F)
  Country_bq36_8_dummy <- make_dummy(y$BSBM19G)
  Country_bq37_8_dummy <- make_dummy(y$BSBM19H)
  Country_bq38_8_dummy <- make_dummy(y$BSBM19I)
  
  
  Country_8_matM <- (as.double(as.vector(y$BSMMAT01))
                     +as.double(as.vector(y$BSMMAT02))
                     +as.double(as.vector(y$BSMMAT03))
                     +as.double(as.vector(y$BSMMAT04))
                     +as.double(as.vector(y$BSMMAT05)))/5000
  

  Country_grade_indicator <- rep(c(1),nrow(Country_bq30_8_dummy))
  return(data.frame(cbind(Country_books_8_dummy,Country_gender_8_dummy,Country_language_8_dummy,Country_bq1_8_dummy,Country_bq2_8_dummy,Country_bq3_8_dummy,Country_bq4_8_dummy,Country_bq5_8_dummy,
                          Country_bq6_8_dummy,Country_bq7_8_dummy,Country_bq8_8_dummy,Country_bq9_8_dummy,Country_bq10_8_dummy,Country_bq11_8_dummy,Country_bq12_8_dummy,Country_bq13_8_dummy,
                          Country_bq14_8_dummy,Country_bq15_8_dummy,Country_bq16_8_dummy,Country_bq17_8_dummy,Country_bq18_8_dummy,Country_bq19_8_dummy,Country_bq20_8_dummy,Country_bq21_8_dummy,
                          Country_bq22_8_dummy,Country_bq23_8_dummy,Country_bq24_8_dummy,Country_bq25_8_dummy,Country_bq26_8_dummy,Country_bq27_8_dummy,Country_bq28_8_dummy,Country_bq29_8_dummy,
                          Country_bq30_8_dummy,Country_bq31_8_dummy,Country_bq32_8_dummy,Country_bq33_8_dummy,Country_bq34_8_dummy,Country_bq35_8_dummy,Country_bq36_8_dummy,Country_bq37_8_dummy,
                          Country_bq38_8_dummy,Country_grade_indicator,Country_8_matM)))
}

##G4 and G8 data
Country_dummy<-function(x,y){
  Country_4_dummy <- Country_G_4_dummy(x)
  Country_8_dummy <- Country_G_8_dummy(y)
  names(Country_4_dummy) <- names(Country_8_dummy)
  return(rbind(Country_4_dummy,Country_8_dummy))
}



##############################################################################################################
library(tidyr)
# Input data
{
  Abu_4 <- as.data.frame(read.spss(file="C:/Users/G4 2015 and G8 2019/Abu_G4_2015.sav",header=TRUE,to.data.frame=TRUE,sep=","))
  Abu_4 <- Abu_4[complete.cases(Abu_4$ASBG01),]
  Australia_4 <- as.data.frame(read.spss(file="C:/Users/G4 2015 and G8 2019/Australia_G4_2015.sav",header=TRUE,to.data.frame=TRUE,sep=","))
  Australia_4 <- Australia_4[complete.cases(Australia_4$ASBG01),]
  Bahrain_4 <- as.data.frame(read.spss(file="C:/Users/G4 2015 and G8 2019/Bahrain_G4_2015.sav",header=TRUE,to.data.frame=TRUE,sep=","))
  Bahrain_4 <- Bahrain_4[complete.cases(Bahrain_4$ASBG01),]
  Chile_4 <- as.data.frame(read.spss(file="C:/Users/G4 2015 and G8 2019/Chile_G4_2015.sav",header=TRUE,to.data.frame=TRUE,sep=","))
  Chile_4 <- Chile_4[complete.cases(Chile_4$ASBG01),]
  Dubai_4 <- as.data.frame(read.spss(file="C:/Users/G4 2015 and G8 2019/Dubai_G4_2015.sav",header=TRUE,to.data.frame=TRUE,sep=","))
  Dubai_4 <- Dubai_4[complete.cases(Dubai_4$ASBG01),]
  England_4 <- as.data.frame(read.spss(file="C:/Users/G4 2015 and G8 2019/England_G4_2015.sav",header=TRUE,to.data.frame=TRUE,sep=","))
  England_4 <- England_4[complete.cases(England_4$ASBG01),]
  Georgia_4 <- as.data.frame(read.spss(file="C:/Users/G4 2015 and G8 2019/Georgia_G4_2015.sav",header=TRUE,to.data.frame=TRUE,sep=","))
  Georgia_4 <- Georgia_4[complete.cases(Georgia_4$ASBG01),]
  HongKong_4 <- as.data.frame(read.spss(file="C:/Users/G4 2015 and G8 2019/HongKong_G4_2015.sav",header=TRUE,to.data.frame=TRUE,sep=","))
  HongKong_4 <- HongKong_4[complete.cases(HongKong_4$ASBG01),]
  Hungary_4 <- as.data.frame(read.spss(file="C:/Users/G4 2015 and G8 2019/Hungary_G4_2015.sav",header=TRUE,to.data.frame=TRUE,sep=","))
  Hungary_4 <- Hungary_4[complete.cases(Hungary_4$ASBG01),]
  Iran_4 <- as.data.frame(read.spss(file="C:/Users/G4 2015 and G8 2019/Iran_G4_2015.sav",header=TRUE,to.data.frame=TRUE,sep=","))
  Iran_4 <- Iran_4[complete.cases(Iran_4$ASBG01),]
  Ireland_4 <- as.data.frame(read.spss(file="C:/Users/G4 2015 and G8 2019/Ireland_G4_2015.sav",header=TRUE,to.data.frame=TRUE,sep=","))
  Ireland_4 <- Ireland_4[complete.cases(Ireland_4$ASBG01),]
  Italy_4 <- as.data.frame(read.spss(file="C:/Users/G4 2015 and G8 2019/Italy_G4_2015.sav",header=TRUE,to.data.frame=TRUE,sep=","))
  Italy_4 <- Italy_4[complete.cases(Italy_4$ASBG01),]
  Japan_4 <- as.data.frame(read.spss(file="C:/Users/G4 2015 and G8 2019/Japan_G4_2015.sav",header=TRUE,to.data.frame=TRUE,sep=","))
  Japan_4 <- Japan_4[complete.cases(Japan_4$ASBG01),]
  Kazakhstan_4 <- as.data.frame(read.spss(file="C:/Users/G4 2015 and G8 2019/Kazakhstan_G4_2015.sav",header=TRUE,to.data.frame=TRUE,sep=","))
  Kazakhstan_4 <- Kazakhstan_4[complete.cases(Kazakhstan_4$ASBG01),]
  Korea_4 <- as.data.frame(read.spss(file="C:/Users/G4 2015 and G8 2019/Korea_G4_2015.sav",header=TRUE,to.data.frame=TRUE,sep=","))
  Korea_4 <- Korea_4[complete.cases(Korea_4$ASBG01),]
  Kuwait_4 <- as.data.frame(read.spss(file="C:/Users/G4 2015 and G8 2019/Kuwait_G4_2015.sav",header=TRUE,to.data.frame=TRUE,sep=","))
  Kuwait_4 <- Kuwait_4[complete.cases(Kuwait_4$ASBG01),]
  Lithuania_4 <- as.data.frame(read.spss(file="C:/Users/G4 2015 and G8 2019/Lithuania_G4_2015.sav",header=TRUE,to.data.frame=TRUE,sep=","))
  Lithuania_4 <- Lithuania_4[complete.cases(Lithuania_4$ASBG01),]
  Morocco_4 <- as.data.frame(read.spss(file="C:/Users/G4 2015 and G8 2019/Morocco_G4_2015.sav",header=TRUE,to.data.frame=TRUE,sep=","))
  Morocco_4 <- Morocco_4[complete.cases(Morocco_4$ASBG01),]
  NewZealand_4 <- as.data.frame(read.spss(file="C:/Users/G4 2015 and G8 2019/NewZealand_G4_2015.sav",header=TRUE,to.data.frame=TRUE,sep=","))
  NewZealand_4 <- NewZealand_4[complete.cases(NewZealand_4$ASBG01),]
  Oman_4 <- as.data.frame(read.spss(file="C:/Users/G4 2015 and G8 2019/Oman_G4_2015.sav",header=TRUE,to.data.frame=TRUE,sep=","))
  Oman_4 <- Oman_4[complete.cases(Oman_4$ASBG01),]
  Ontario_4 <- as.data.frame(read.spss(file="C:/Users/G4 2015 and G8 2019/Ontario_G4_2015.sav",header=TRUE,to.data.frame=TRUE,sep=","))
  Ontario_4 <- Ontario_4[complete.cases(Ontario_4$ASBG01),]
  Quebec_4 <- as.data.frame(read.spss(file="C:/Users/G4 2015 and G8 2019/Quebec_G4_2015.sav",header=TRUE,to.data.frame=TRUE,sep=","))
  Quebec_4 <- Quebec_4[complete.cases(Quebec_4$ASBG01),]
  Russia_4 <- as.data.frame(read.spss(file="C:/Users/G4 2015 and G8 2019/Russia_G4_2015.sav",header=TRUE,to.data.frame=TRUE,sep=","))
  Russia_4 <- Russia_4[complete.cases(Russia_4$ASBG01),]
  Singapore_4 <- as.data.frame(read.spss(file="C:/Users/G4 2015 and G8 2019/Singapore_G4_2015.sav",header=TRUE,to.data.frame=TRUE,sep=","))
  Singapore_4 <- Singapore_4[complete.cases(Singapore_4$ASBG01),]
  Sweden_4 <- as.data.frame(read.spss(file="C:/Users/G4 2015 and G8 2019/Sweden_G4_2015.sav",header=TRUE,to.data.frame=TRUE,sep=","))
  Sweden_4 <- Sweden_4[complete.cases(Sweden_4$ASBG01),]
  Taipei_4 <- as.data.frame(read.spss(file="C:/Users/G4 2015 and G8 2019/Taipei_G4_2015.sav",header=TRUE,to.data.frame=TRUE,sep=","))
  Taipei_4 <- Taipei_4[complete.cases(Taipei_4$ASBG01),]
  Turkey_4 <- as.data.frame(read.spss(file="C:/Users/G4 2015 and G8 2019/Turkey_G4_2015.sav",header=TRUE,to.data.frame=TRUE,sep=","))
  Turkey_4 <- Turkey_4[complete.cases(Turkey_4$ASBG01),]
  UAE_4 <- as.data.frame(read.spss(file="C:/Users/G4 2015 and G8 2019/UAE_G4_2015.sav",header=TRUE,to.data.frame=TRUE,sep=","))
  UAE_4 <- UAE_4[complete.cases(UAE_4$ASBG01),]
  US_4 <- as.data.frame(read.spss(file="C:/Users/G4 2015 and G8 2019/US_G4_2015.sav",header=TRUE,to.data.frame=TRUE,sep=","))
  US_4 <- US_4[complete.cases(US_4$ASBG01),]
  
  Abu_8 <- as.data.frame(read.spss(file="C:/Users/G4 2015 and G8 2019/Abu_G8_2019.sav",header=TRUE,to.data.frame=TRUE,sep=","))
  Abu_8 <- Abu_8[complete.cases(Abu_8$BSBG01),]
  Australia_8 <- as.data.frame(read.spss(file="C:/Users/G4 2015 and G8 2019/Australia_G8_2019.sav",header=TRUE,to.data.frame=TRUE,sep=","))
  Australia_8 <- Australia_8[complete.cases(Australia_8$BSBG01),]
  Bahrain_8 <- as.data.frame(read.spss(file="C:/Users/G4 2015 and G8 2019/Bahrain_G8_2019.sav",header=TRUE,to.data.frame=TRUE,sep=","))
  Bahrain_8 <- Bahrain_8[complete.cases(Bahrain_8$BSBG01),]
  Chile_8 <- as.data.frame(read.spss(file="C:/Users/G4 2015 and G8 2019/Chile_G8_2019.sav",header=TRUE,to.data.frame=TRUE,sep=","))
  Chile_8 <- Chile_8[complete.cases(Chile_8$BSBG01),]
  Dubai_8 <- as.data.frame(read.spss(file="C:/Users/G4 2015 and G8 2019/Dubai_G8_2019.sav",header=TRUE,to.data.frame=TRUE,sep=","))
  Dubai_8 <- Dubai_8[complete.cases(Dubai_8$BSBG01),]
  England_8 <- as.data.frame(read.spss(file="C:/Users/G4 2015 and G8 2019/England_G8_2019.sav",header=TRUE,to.data.frame=TRUE,sep=","))
  England_8 <- England_8[complete.cases(England_8$BSBG01),]
  Georgia_8 <- as.data.frame(read.spss(file="C:/Users/G4 2015 and G8 2019/Georgia_G8_2019.sav",header=TRUE,to.data.frame=TRUE,sep=","))
  Georgia_8 <- Georgia_8[complete.cases(Georgia_8$BSBG01),]
  HongKong_8 <- as.data.frame(read.spss(file="C:/Users/G4 2015 and G8 2019/HongKong_G8_2019.sav",header=TRUE,to.data.frame=TRUE,sep=","))
  HongKong_8 <- HongKong_8[complete.cases(HongKong_8$BSBG01),]
  Hungary_8 <- as.data.frame(read.spss(file="C:/Users/G4 2015 and G8 2019/Hungary_G8_2019.sav",header=TRUE,to.data.frame=TRUE,sep=","))
  Hungary_8 <- Hungary_8[complete.cases(Hungary_8$BSBG01),]
  Iran_8 <- as.data.frame(read.spss(file="C:/Users/G4 2015 and G8 2019/Iran_G8_2019.sav",header=TRUE,to.data.frame=TRUE,sep=","))
  Iran_8 <- Iran_8[complete.cases(Iran_8$BSBG01),]
  Ireland_8 <- as.data.frame(read.spss(file="C:/Users/G4 2015 and G8 2019/Ireland_G8_2019.sav",header=TRUE,to.data.frame=TRUE,sep=","))
  Ireland_8 <- Ireland_8[complete.cases(Ireland_8$BSBG01),]
  Italy_8 <- as.data.frame(read.spss(file="C:/Users/G4 2015 and G8 2019/Italy_G8_2019.sav",header=TRUE,to.data.frame=TRUE,sep=","))
  Italy_8 <- Italy_8[complete.cases(Italy_8$BSBG01),]
  Japan_8 <- as.data.frame(read.spss(file="C:/Users/G4 2015 and G8 2019/Japan_G8_2019.sav",header=TRUE,to.data.frame=TRUE,sep=","))
  Japan_8 <- Japan_8[complete.cases(Japan_8$BSBG01),]
  Kazakhstan_8 <- as.data.frame(read.spss(file="C:/Users/G4 2015 and G8 2019/Kazakhstan_G8_2019.sav",header=TRUE,to.data.frame=TRUE,sep=","))
  Kazakhstan_8 <- Kazakhstan_8[complete.cases(Kazakhstan_8$BSBG01),]
  Korea_8 <- as.data.frame(read.spss(file="C:/Users/G4 2015 and G8 2019/Korea_G8_2019.sav",header=TRUE,to.data.frame=TRUE,sep=","))
  Korea_8 <- Korea_8[complete.cases(Korea_8$BSBG01),]
  Kuwait_8 <- as.data.frame(read.spss(file="C:/Users/G4 2015 and G8 2019/Kuwait_G8_2019.sav",header=TRUE,to.data.frame=TRUE,sep=","))
  Kuwait_8 <- Kuwait_8[complete.cases(Kuwait_8$BSBG01),]
  Lithuania_8 <- as.data.frame(read.spss(file="C:/Users/G4 2015 and G8 2019/Lithuania_G8_2019.sav",header=TRUE,to.data.frame=TRUE,sep=","))
  Lithuania_8 <- Lithuania_8[complete.cases(Lithuania_8$BSBG01),]
  Morocco_8 <- as.data.frame(read.spss(file="C:/Users/G4 2015 and G8 2019/Morocco_G8_2019.sav",header=TRUE,to.data.frame=TRUE,sep=","))
  Morocco_8 <- Morocco_8[complete.cases(Morocco_8$BSBG01),]
  NewZealand_8 <- as.data.frame(read.spss(file="C:/Users/G4 2015 and G8 2019/NewZealand_G8_2019.sav",header=TRUE,to.data.frame=TRUE,sep=","))
  NewZealand_8 <- NewZealand_8[complete.cases(NewZealand_8$BSBG01),]
  Oman_8 <- as.data.frame(read.spss(file="C:/Users/G4 2015 and G8 2019/Oman_G8_2019.sav",header=TRUE,to.data.frame=TRUE,sep=","))
  Oman_8 <- Oman_8[complete.cases(Oman_8$BSBG01),]
  Ontario_8 <- as.data.frame(read.spss(file="C:/Users/G4 2015 and G8 2019/Ontario_G8_2019.sav",header=TRUE,to.data.frame=TRUE,sep=","))
  Ontario_8 <- Ontario_8[complete.cases(Ontario_8$BSBG01),]
  Quebec_8 <- as.data.frame(read.spss(file="C:/Users/G4 2015 and G8 2019/Quebec_G8_2019.sav",header=TRUE,to.data.frame=TRUE,sep=","))
  Quebec_8 <- Quebec_8[complete.cases(Quebec_8$BSBG01),]
  Russia_8 <- as.data.frame(read.spss(file="C:/Users/G4 2015 and G8 2019/Russia_G8_2019.sav",header=TRUE,to.data.frame=TRUE,sep=","))
  Russia_8 <- Russia_8[complete.cases(Russia_8$BSBG01),]
  Singapore_8 <- as.data.frame(read.spss(file="C:/Users/G4 2015 and G8 2019/Singapore_G8_2019.sav",header=TRUE,to.data.frame=TRUE,sep=","))
  Singapore_8 <- Singapore_8[complete.cases(Singapore_8$BSBG01),]
  Sweden_8 <- as.data.frame(read.spss(file="C:/Users/G4 2015 and G8 2019/Sweden_G8_2019.sav",header=TRUE,to.data.frame=TRUE,sep=","))
  Sweden_8 <- Sweden_8[complete.cases(Sweden_8$BSBG01),]
  Taipei_8 <- as.data.frame(read.spss(file="C:/Users/G4 2015 and G8 2019/Taipei_G8_2019.sav",header=TRUE,to.data.frame=TRUE,sep=","))
  Taipei_8 <- Taipei_8[complete.cases(Taipei_8$BSBG01),]
  Turkey_8 <- as.data.frame(read.spss(file="C:/Users/G4 2015 and G8 2019/Turkey_G8_2019.sav",header=TRUE,to.data.frame=TRUE,sep=","))
  Turkey_8 <- Turkey_8[complete.cases(Turkey_8$BSBG01),]
  UAE_8 <- as.data.frame(read.spss(file="C:/Users/G4 2015 and G8 2019/UAE_G8_2019.sav",header=TRUE,to.data.frame=TRUE,sep=","))
  UAE_8 <- UAE_8[complete.cases(UAE_8$BSBG01),]
  US_8 <- as.data.frame(read.spss(file="C:/Users/G4 2015 and G8 2019/US_G8_2019.sav",header=TRUE,to.data.frame=TRUE,sep=","))
  US_8 <- US_8[complete.cases(US_8$BSBG01),]
}


#dummy matrix for each country
{
  Country_1 <- Country_dummy(Abu_4,Abu_8)
  Country_2 <- Country_dummy(Australia_4,Australia_8)
  Country_3 <- Country_dummy(Bahrain_4,Bahrain_8)
  Country_4 <- Country_dummy(Chile_4,Chile_8)
  Country_5 <- Country_dummy(Dubai_4,Dubai_8)
  Country_6 <- Country_dummy(England_4,England_8)
  Country_7 <- Country_dummy(Georgia_4,Georgia_8)
  Country_8 <- Country_dummy(HongKong_4,HongKong_8)
  Country_9 <- Country_dummy(Hungary_4,Hungary_8)
  Country_10 <- Country_dummy(Iran_4,Iran_8)
  Country_11 <- Country_dummy(Ireland_4,Ireland_8)
  Country_12 <- Country_dummy(Italy_4,Italy_8)
  Country_13 <- Country_dummy(Japan_4,Japan_8)
  Country_14 <- Country_dummy(Kazakhstan_4,Kazakhstan_8)
  Country_15 <- Country_dummy(Korea_4,Korea_8)
  Country_16 <- Country_dummy(Kuwait_4,Kuwait_8)
  Country_17 <- Country_dummy(Lithuania_4,Lithuania_8)
  Country_18 <- Country_dummy(Morocco_4,Morocco_8)
  Country_19 <- Country_dummy(NewZealand_4,NewZealand_8)
  Country_20 <- Country_dummy(Oman_4,Oman_8)
  Country_21 <- Country_dummy(Ontario_4,Ontario_8)
  Country_22 <- Country_dummy(Quebec_4,Quebec_8)
  Country_23 <- Country_dummy(Russia_4,Russia_8)
  Country_24 <- Country_dummy(Singapore_4,Singapore_8)
  Country_25 <- Country_dummy(Sweden_4,Sweden_8)
  Country_26 <- Country_dummy(Taipei_4,Taipei_8)
  Country_27 <- Country_dummy(Turkey_4,Turkey_8)
  Country_28 <- Country_dummy(UAE_4,UAE_8)
  Country_29 <- Country_dummy(US_4,US_8)
}

#Country indicators
{
  Country_indicator_1 <- matrix(rep(c(rep(0,28),1),nrow(Country_1)),ncol=29,nrow=nrow(Country_1),byrow=T)
  Country_indicator_2 <- matrix(rep(c(rep(0,27),1,rep(0,1)),nrow(Country_2)),ncol=29,nrow=nrow(Country_2),byrow=T)
  Country_indicator_3 <- matrix(rep(c(rep(0,26),1,rep(0,2)),nrow(Country_3)),ncol=29,nrow=nrow(Country_3),byrow=T)
  Country_indicator_4 <- matrix(rep(c(rep(0,25),1,rep(0,3)),nrow(Country_4)),ncol=29,nrow=nrow(Country_4),byrow=T)
  Country_indicator_5 <- matrix(rep(c(rep(0,24),1,rep(0,4)),nrow(Country_5)),ncol=29,nrow=nrow(Country_5),byrow=T)
  Country_indicator_6 <- matrix(rep(c(rep(0,23),1,rep(0,5)),nrow(Country_6)),ncol=29,nrow=nrow(Country_6),byrow=T)
  Country_indicator_7 <- matrix(rep(c(rep(0,22),1,rep(0,6)),nrow(Country_7)),ncol=29,nrow=nrow(Country_7),byrow=T)
  Country_indicator_8 <- matrix(rep(c(rep(0,21),1,rep(0,7)),nrow(Country_8)),ncol=29,nrow=nrow(Country_8),byrow=T)
  Country_indicator_9 <- matrix(rep(c(rep(0,20),1,rep(0,8)),nrow(Country_9)),ncol=29,nrow=nrow(Country_9),byrow=T)
  Country_indicator_10 <- matrix(rep(c(rep(0,19),1,rep(0,9)),nrow(Country_10)),ncol=29,nrow=nrow(Country_10),byrow=T)
  Country_indicator_11 <- matrix(rep(c(rep(0,18),1,rep(0,10)),nrow(Country_11)),ncol=29,nrow=nrow(Country_11),byrow=T)
  Country_indicator_12 <- matrix(rep(c(rep(0,17),1,rep(0,11)),nrow(Country_12)),ncol=29,nrow=nrow(Country_12),byrow=T)
  Country_indicator_13 <- matrix(rep(c(rep(0,16),1,rep(0,12)),nrow(Country_13)),ncol=29,nrow=nrow(Country_13),byrow=T)
  Country_indicator_14 <- matrix(rep(c(rep(0,15),1,rep(0,13)),nrow(Country_14)),ncol=29,nrow=nrow(Country_14),byrow=T)
  Country_indicator_15 <- matrix(rep(c(rep(0,14),1,rep(0,14)),nrow(Country_15)),ncol=29,nrow=nrow(Country_15),byrow=T)
  Country_indicator_16 <- matrix(rep(c(rep(0,13),1,rep(0,15)),nrow(Country_16)),ncol=29,nrow=nrow(Country_16),byrow=T)
  Country_indicator_17 <- matrix(rep(c(rep(0,12),1,rep(0,16)),nrow(Country_17)),ncol=29,nrow=nrow(Country_17),byrow=T)
  Country_indicator_18 <- matrix(rep(c(rep(0,11),1,rep(0,17)),nrow(Country_18)),ncol=29,nrow=nrow(Country_18),byrow=T)
  Country_indicator_19 <- matrix(rep(c(rep(0,10),1,rep(0,18)),nrow(Country_19)),ncol=29,nrow=nrow(Country_19),byrow=T)
  Country_indicator_20 <- matrix(rep(c(rep(0,9),1,rep(0,19)),nrow(Country_20)),ncol=29,nrow=nrow(Country_20),byrow=T)
  Country_indicator_21 <- matrix(rep(c(rep(0,8),1,rep(0,20)),nrow(Country_21)),ncol=29,nrow=nrow(Country_21),byrow=T)
  Country_indicator_22 <- matrix(rep(c(rep(0,7),1,rep(0,21)),nrow(Country_22)),ncol=29,nrow=nrow(Country_22),byrow=T)
  Country_indicator_23 <- matrix(rep(c(rep(0,6),1,rep(0,22)),nrow(Country_23)),ncol=29,nrow=nrow(Country_23),byrow=T)
  Country_indicator_24 <- matrix(rep(c(rep(0,5),1,rep(0,23)),nrow(Country_24)),ncol=29,nrow=nrow(Country_24),byrow=T)
  Country_indicator_25 <- matrix(rep(c(rep(0,4),1,rep(0,24)),nrow(Country_25)),ncol=29,nrow=nrow(Country_25),byrow=T)
  Country_indicator_26 <- matrix(rep(c(rep(0,3),1,rep(0,25)),nrow(Country_26)),ncol=29,nrow=nrow(Country_26),byrow=T)
  Country_indicator_27 <- matrix(rep(c(rep(0,2),1,rep(0,26)),nrow(Country_27)),ncol=29,nrow=nrow(Country_27),byrow=T)
  Country_indicator_28 <- matrix(rep(c(rep(0,1),1,rep(0,27)),nrow(Country_28)),ncol=29,nrow=nrow(Country_28),byrow=T)
  Country_indicator_29 <- matrix(rep(c(1,rep(0,28)),nrow(Country_29)),ncol=29,nrow=nrow(Country_29),byrow=T)
}

######################################################################################################################
##The whole matrix
Country_indicators <- rbind(Country_indicator_1,Country_indicator_2,Country_indicator_3,Country_indicator_4,
                            Country_indicator_5,Country_indicator_6,Country_indicator_7,Country_indicator_8,
                            Country_indicator_9,Country_indicator_10,Country_indicator_11,Country_indicator_12,
                            Country_indicator_13,Country_indicator_14,Country_indicator_15,Country_indicator_16,
                            Country_indicator_17,Country_indicator_18,Country_indicator_19,Country_indicator_20,
                            Country_indicator_21,Country_indicator_22,Country_indicator_23,Country_indicator_24,
                            Country_indicator_25,Country_indicator_26,Country_indicator_27,Country_indicator_28,
                            Country_indicator_29)
Country_variables <- rbind(Country_1,Country_2,Country_3,Country_4,Country_5,Country_6,Country_7,Country_8,
                           Country_9,Country_10,Country_11,Country_12,Country_13,Country_14,Country_15,Country_16,
                           Country_17,Country_18,Country_19,Country_20,Country_21,Country_22,Country_23,Country_24,
                           Country_25,Country_26,Country_27,Country_28,Country_29)
Dataset <- cbind(Country_indicators,Country_variables)

names(Dataset) <- c("i1","i2","i3","i4","i5","i6","i7","i8","i9","i10","i11","i12","i13","i14","i15","i16","i17","i18","i19","i20",
                    "i21","i22","i23","i24","i25","i26","i27","i28","i29","i30",
                    "i31","i32","i33","i34","i35","i36","i37","i38","i39","i40",
                    "i41","i42","i43","i44","i45","i46","i47","i48","i49","i50",
                    "i51","i52","i53","i54","i55","i56","i57","i58","i59","i60",
                    "i61","i62","i63","i64","i65","i66","i67","i68","i69","i70",
                    "i71","i72","i73","i74","i75","i76","i77","i78","i79","i80",
                    "i81","i82","i83","i84","i85","i86","i87","i88","i89","i90",
                    "i91","i92","i93","i94","i95","i96","i97","i98","i99","i100",
                    "i101","i102","i103","i104","i105","i106","i107","i108","i109","i110",
                    "i111","i112","i113","i114","i115","i116","i117","i118","i119","i120",
                    "i121","i122","i123","i124","i125","i126","i127","i128","i129","i130",
                    "i131","i132","i133","i134","i135","i136","i137","i138","i139","i140",
                    "i141","i142","i143","i144","i145","i146","i147","i148","i149","i150",
                    "i151","i152","i153","i154","i155","i156","i157","i158","i159","i160",
                    "i161","i162","i163","i164","i165","i166","i167","i168","i169","i170",
                    "i171","i172","i173","i174","i175","i176","i177","i178","i179","i180",
                    "i181","i182","i183","i184","i185","i186","i187","i188","i189","i190",
                    "i191","i192","i193","i194","i195","i196","i197","i198","i199","i200",
                    "i201","i202","i203","i204","i205","i206","i207","i208","i209","i210",
                    "i211","i212","i213","i214","i215","i216","i217","i218","i219","i220",
                    "i221","i222","i223","i224","i225","i226")

sample_size <- floor(0.8*nrow(Dataset))
set.seed(123)
train_index <- sample (seq_len(nrow(Dataset)),size=sample_size)

train_dataset <- Dataset[train_index,]
test_dataset <- Dataset[-train_index,]

train_data <- train_dataset %>% dplyr::select(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14,i15,i16,i17,i18,
                                              i19,i20,i21,i22,i23,i24,i25,i26,i27,i28,i29,i30,i31,i32,i33,i34,
                                              i35,i36,i37,i38,i39,i40,i41,i42,i43,i44,i45,i46,i47,i48,i49,i50,
                                              i51,i52,i53,i54,i55,i56,i57,i58,i59,i60,i61,i62,i63,i64,i65,i66,
                                              i67,i68,i69,i70,i71,i72,i73,i74,i75,i76,i77,i78,i79,i80,i81,i82,
                                              i83,i84,i85,i86,i87,i88,i89,i90,i91,i92,i93,i94,i95,i96,i97,i98,
                                              i99,i100,i101,i102,i103,i104,i105,i106,i107,i108,i109,i110,i111,
                                              i112,i113,i114,i115,i116,i117,i118,i119,i120,i121,i122,i123,
                                              i124,i125,i126,i127,i128,i129,i130,i131,i132,i133,i134,i135,
                                              i136,i137,i138,i139,i140,i141,i142,i143,i144,i145,i146,i147,
                                              i148,i149,i150,i151,i152,i153,i154,i155,i156,i157,i158,i159,
                                              i160,i161,i162,i163,i164,i165,i166,i167,i168,i169,i170,i171,
                                              i172,i173,i174,i175,i176,i177,i178,i179,i180,i181,i182,i183,
                                              i184,i185,i186,i187,i188,i189,i190,i191,i192,i193,i194,i195,
                                              i196,i197,i198,i199,i200,i201,i202,i203,i204,i205,i206,i207,i208,i209,i210,
                                              i211,i212,i213,i214,i215,i216,i217,i218,i219,i220,i221,i222,i223,i224,i225)
train_labels <- dplyr::select(train_dataset,c(i226))
test_data <- test_dataset %>% dplyr::select(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14,i15,i16,i17,i18,
                                            i19,i20,i21,i22,i23,i24,i25,i26,i27,i28,i29,i30,i31,i32,i33,i34,
                                            i35,i36,i37,i38,i39,i40,i41,i42,i43,i44,i45,i46,i47,i48,i49,i50,
                                            i51,i52,i53,i54,i55,i56,i57,i58,i59,i60,i61,i62,i63,i64,i65,i66,
                                            i67,i68,i69,i70,i71,i72,i73,i74,i75,i76,i77,i78,i79,i80,i81,i82,
                                            i83,i84,i85,i86,i87,i88,i89,i90,i91,i92,i93,i94,i95,i96,i97,i98,
                                            i99,i100,i101,i102,i103,i104,i105,i106,i107,i108,i109,i110,i111,
                                            i112,i113,i114,i115,i116,i117,i118,i119,i120,i121,i122,i123,
                                            i124,i125,i126,i127,i128,i129,i130,i131,i132,i133,i134,i135,
                                            i136,i137,i138,i139,i140,i141,i142,i143,i144,i145,i146,i147,
                                            i148,i149,i150,i151,i152,i153,i154,i155,i156,i157,i158,i159,
                                            i160,i161,i162,i163,i164,i165,i166,i167,i168,i169,i170,i171,
                                            i172,i173,i174,i175,i176,i177,i178,i179,i180,i181,i182,i183,
                                            i184,i185,i186,i187,i188,i189,i190,i191,i192,i193,i194,i195,
                                            i196,i197,i198,i199,i200,i201,i202,i203,i204,i205,i206,i207,i208,i209,i210,
                                            i211,i212,i213,i214,i215,i216,i217,i218,i219,i220,i221,i222,i223,i224,i225)
test_labels <- dplyr::select(test_dataset,c(i226))

# Normalize the data by subtracting the mean and dividing by the standard deviation
normalize<-function(x) {
  y<-as.double(x)
  #z<-(y-min(y))/(max(y)-min(y))
  return(y)
}

normalized_train_data <-apply(train_data,2,normalize)

# Convert to matrix
train_labels <- as.matrix(train_labels)
normalized_test_data <- apply(test_data,2,normalize)
test_labels <- as.matrix(test_labels)

model <- keras::keras_model_sequential()
model %>% 
  keras::layer_dense(units = 32, activation ='relu', kernel_initializer='normal', input_shape = dim(normalized_train_data)[2]) %>% 
  keras::layer_dense(units = 64, activation = 'relu', kernel_initializer='normal') %>%
  keras::layer_dense(units = 16, activation = 'relu',kernel_initializer='normal') %>%
  keras::layer_dense(units = 1, activation = 'relu',kernel_initializer='normal')

# Set the metrics required to be Mean Absolute Error and Mean Squared Error.For regression, the loss is 
# mean_squared_error
model %>% keras::compile(
  loss = 'mean_squared_error',
  #loss = 'huber_loss',
  #loss = 'mean_absolute_error',
  optimizer = keras::optimizer_sgd(lr=0.05,momentum=0.003),
  #optimizer = keras::optimizer_rmsprop(),
  metrics = c('mean_absolute_error','mean_squared_error')
)

# Fit the model
# Use the test data for validation
history <- model %>% keras::fit(
  normalized_train_data, train_labels, 
  epochs = 350, batch_size = 128, 
  validation_data = list(normalized_test_data,test_labels)
)
plot(history)

test_predictions <- model %>% predict(normalized_test_data)

# See how the prediction and the actual value of the training set.
plot(x=test_labels,y=test_predictions)
abline(a=0,b=1,untf=FALSE,col="red")

# Prepare for the dataset for G4 to G8 prediction. Math score * 1000
normalized_Allset_data <- apply(Dataset,2,as.double)[,1:225]
All_predictions <- model %>% predict(normalized_Allset_data)
All_predictions <- 1000*All_predictions
Dataset[,226]=1000*Dataset[,226]

### Sample size for each sample
s <- c(nrow(Abu_4),nrow(Abu_8),nrow(Australia_4),nrow(Australia_8),nrow(Bahrain_4),nrow(Bahrain_8),nrow(Chile_4),nrow(Chile_8),
       nrow(Dubai_4),nrow(Dubai_8),nrow(England_4),nrow(England_8),nrow(Georgia_4),nrow(Georgia_8),nrow(HongKong_4),nrow(HongKong_8),
       nrow(Hungary_4),nrow(Hungary_8),nrow(Iran_4),nrow(Iran_8),nrow(Ireland_4),nrow(Ireland_8),nrow(Italy_4),nrow(Italy_8),
       nrow(Japan_4),nrow(Japan_8),nrow(Kazakhstan_4),nrow(Kazakhstan_8),nrow(Korea_4),nrow(Korea_8),nrow(Kuwait_4),nrow(Kuwait_8),
       nrow(Lithuania_4),nrow(Lithuania_8),nrow(Morocco_4),nrow(Morocco_8),nrow(NewZealand_4),nrow(NewZealand_8),
       nrow(Oman_4),nrow(Oman_8),nrow(Ontario_4),nrow(Ontario_8),nrow(Quebec_4),nrow(Quebec_8),nrow(Russia_4),nrow(Russia_8),
       nrow(Singapore_4),nrow(Singapore_8),nrow(Sweden_4),nrow(Sweden_8),nrow(Taipei_4),nrow(Taipei_8),nrow(Turkey_4),nrow(Turkey_8),
       nrow(UAE_4),nrow(UAE_8),nrow(US_4),nrow(US_8))
S <- rep(0,58)
for (i in 1:58){
  S[i]=sum(s[1:i])
}

M<-matrix(0,ncol=1,nrow=29)
for (i in 1:29){
  M[i,1]=sd(Dataset[(S[2*i-1]+1):S[2*i],][,226])
}
write.table(M, file = "data.csv",sep = "\t", row.names = F)

for (i in 1:28){
  Test<-cbind(Dataset[(S[2*i]+1):S[2*i+1],][,1:224],rep(c(1),s[2*i+1]))
  names(Test) <-  c("i1","i2","i3","i4","i5","i6","i7","i8","i9","i10","i11","i12","i13","i14","i15","i16","i17","i18","i19","i20",
                    "i21","i22","i23","i24","i25","i26","i27","i28","i29","i30",
                    "i31","i32","i33","i34","i35","i36","i37","i38","i39","i40",
                    "i41","i42","i43","i44","i45","i46","i47","i48","i49","i50",
                    "i51","i52","i53","i54","i55","i56","i57","i58","i59","i60",
                    "i61","i62","i63","i64","i65","i66","i67","i68","i69","i70",
                    "i71","i72","i73","i74","i75","i76","i77","i78","i79","i80",
                    "i81","i82","i83","i84","i85","i86","i87","i88","i89","i90",
                    "i91","i92","i93","i94","i95","i96","i97","i98","i99","i100",
                    "i101","i102","i103","i104","i105","i106","i107","i108","i109","i110",
                    "i111","i112","i113","i114","i115","i116","i117","i118","i119","i120",
                    "i121","i122","i123","i124","i125","i126","i127","i128","i129","i130",
                    "i131","i132","i133","i134","i135","i136","i137","i138","i139","i140",
                    "i141","i142","i143","i144","i145","i146","i147","i148","i149","i150",
                    "i151","i152","i153","i154","i155","i156","i157","i158","i159","i160",
                    "i161","i162","i163","i164","i165","i166","i167","i168","i169","i170",
                    "i171","i172","i173","i174","i175","i176","i177","i178","i179","i180",
                    "i181","i182","i183","i184","i185","i186","i187","i188","i189","i190",
                    "i191","i192","i193","i194","i195","i196","i197","i198","i199","i200",
                    "i201","i202","i203","i204","i205","i206","i207","i208","i209","i210",
                    "i211","i212","i213","i214","i215","i216","i217","i218","i219","i220",
                    "i221","i222","i223","i224","i225")
  test_dataset <- Test
  test_data <- test_dataset %>% dplyr::select(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14,i15,i16,i17,i18,
                                              i19,i20,i21,i22,i23,i24,i25,i26,i27,i28,i29,i30,i31,i32,i33,i34,
                                              i35,i36,i37,i38,i39,i40,i41,i42,i43,i44,i45,i46,i47,i48,i49,i50,
                                              i51,i52,i53,i54,i55,i56,i57,i58,i59,i60,i61,i62,i63,i64,i65,i66,
                                              i67,i68,i69,i70,i71,i72,i73,i74,i75,i76,i77,i78,i79,i80,i81,i82,
                                              i83,i84,i85,i86,i87,i88,i89,i90,i91,i92,i93,i94,i95,i96,i97,i98,
                                              i99,i100,i101,i102,i103,i104,i105,i106,i107,i108,i109,i110,i111,
                                              i112,i113,i114,i115,i116,i117,i118,i119,i120,i121,i122,i123,
                                              i124,i125,i126,i127,i128,i129,i130,i131,i132,i133,i134,i135,
                                              i136,i137,i138,i139,i140,i141,i142,i143,i144,i145,i146,i147,
                                              i148,i149,i150,i151,i152,i153,i154,i155,i156,i157,i158,i159,
                                              i160,i161,i162,i163,i164,i165,i166,i167,i168,i169,i170,i171,
                                              i172,i173,i174,i175,i176,i177,i178,i179,i180,i181,i182,i183,
                                              i184,i185,i186,i187,i188,i189,i190,i191,i192,i193,i194,i195,
                                              i196,i197,i198,i199,i200,i201,i202,i203,i204,i205,i206,i207,i208,i209,i210,
                                              i211,i212,i213,i214,i215,i216,i217,i218,i219,i220,i221,i222,i223,i224,i225)
  normalized_test_data <- apply(test_data,2,normalize)
  test_predictions <- model %>% predict(normalized_test_data)
  print(1000*sd(test_predictions))
}

Test<-cbind(Dataset[1:S[1],][,1:224],rep(c(1),s[1]))
names(Test) <-  c("i1","i2","i3","i4","i5","i6","i7","i8","i9","i10","i11","i12","i13","i14","i15","i16","i17","i18","i19","i20",
                  "i21","i22","i23","i24","i25","i26","i27","i28","i29","i30",
                  "i31","i32","i33","i34","i35","i36","i37","i38","i39","i40",
                  "i41","i42","i43","i44","i45","i46","i47","i48","i49","i50",
                  "i51","i52","i53","i54","i55","i56","i57","i58","i59","i60",
                  "i61","i62","i63","i64","i65","i66","i67","i68","i69","i70",
                  "i71","i72","i73","i74","i75","i76","i77","i78","i79","i80",
                  "i81","i82","i83","i84","i85","i86","i87","i88","i89","i90",
                  "i91","i92","i93","i94","i95","i96","i97","i98","i99","i100",
                  "i101","i102","i103","i104","i105","i106","i107","i108","i109","i110",
                  "i111","i112","i113","i114","i115","i116","i117","i118","i119","i120",
                  "i121","i122","i123","i124","i125","i126","i127","i128","i129","i130",
                  "i131","i132","i133","i134","i135","i136","i137","i138","i139","i140",
                  "i141","i142","i143","i144","i145","i146","i147","i148","i149","i150",
                  "i151","i152","i153","i154","i155","i156","i157","i158","i159","i160",
                  "i161","i162","i163","i164","i165","i166","i167","i168","i169","i170",
                  "i171","i172","i173","i174","i175","i176","i177","i178","i179","i180",
                  "i181","i182","i183","i184","i185","i186","i187","i188","i189","i190",
                  "i191","i192","i193","i194","i195","i196","i197","i198","i199","i200",
                  "i201","i202","i203","i204","i205","i206","i207","i208","i209","i210",
                  "i211","i212","i213","i214","i215","i216","i217","i218","i219","i220",
                  "i221","i222","i223","i224","i225")
test_dataset <- Test
test_data <- test_dataset %>% dplyr::select(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14,i15,i16,i17,i18,
                                            i19,i20,i21,i22,i23,i24,i25,i26,i27,i28,i29,i30,i31,i32,i33,i34,
                                            i35,i36,i37,i38,i39,i40,i41,i42,i43,i44,i45,i46,i47,i48,i49,i50,
                                            i51,i52,i53,i54,i55,i56,i57,i58,i59,i60,i61,i62,i63,i64,i65,i66,
                                            i67,i68,i69,i70,i71,i72,i73,i74,i75,i76,i77,i78,i79,i80,i81,i82,
                                            i83,i84,i85,i86,i87,i88,i89,i90,i91,i92,i93,i94,i95,i96,i97,i98,
                                            i99,i100,i101,i102,i103,i104,i105,i106,i107,i108,i109,i110,i111,
                                            i112,i113,i114,i115,i116,i117,i118,i119,i120,i121,i122,i123,
                                            i124,i125,i126,i127,i128,i129,i130,i131,i132,i133,i134,i135,
                                            i136,i137,i138,i139,i140,i141,i142,i143,i144,i145,i146,i147,
                                            i148,i149,i150,i151,i152,i153,i154,i155,i156,i157,i158,i159,
                                            i160,i161,i162,i163,i164,i165,i166,i167,i168,i169,i170,i171,
                                            i172,i173,i174,i175,i176,i177,i178,i179,i180,i181,i182,i183,
                                            i184,i185,i186,i187,i188,i189,i190,i191,i192,i193,i194,i195,
                                            i196,i197,i198,i199,i200,i201,i202,i203,i204,i205,i206,i207,i208,i209,i210,
                                            i211,i212,i213,i214,i215,i216,i217,i218,i219,i220,i221,i222,i223,i224,i225)
normalized_test_data <- apply(test_data,2,normalize)
test_predictions <- model %>% predict(normalized_test_data)
print(1000*sd(test_predictions))

M<-matrix(0,ncol=1,nrow=29)
ASBS04A <- function(x,y){ 
  A=make_dummy(x$ASBG05E) 
  B=make_dummy(y$BSBG06E)
  mean_1=3*mean(A[,1])+2*mean(A[,2])+1*mean(A[,3])
  mean_2=3*mean(B[,1])+2*mean(B[,2])+1*mean(B[,3])
  #mean_1=mean(A[,1])
  #mean_2=mean(B[,1])
  return(mean_2-mean_1)
} 
BSBS21A <- function(y){
  A = make_dummy(y$BSBS21D) 
  mean = 3*mean(A[,1])+2*mean(A[,2])+1*mean(A[,3])
  return(mean)
}

COU<-list(Abu_4,Abu_8,Australia_4,Australia_8,Bahrain_4,Bahrain_8,Chile_4,Chile_8,Dubai_4,Dubai_8,England_4,England_8,
          Georgia_4,Georgia_8,HongKong_4,HongKong_8,Hungary_4,Hungary_8,Iran_4,Iran_8,Ireland_4,Ireland_8,Italy_4,Italy_8,
          Japan_4,Japan_8,Kazakhstan_4,Kazakhstan_8,Korea_4,Korea_8,Kuwait_4,Kuwait_8,Lithuania_4,Lithuania_8,
          Morocco_4,Morocco_8,NewZealand_4,NewZealand_8,Oman_4,Oman_8,Ontario_4,Ontario_8,Quebec_4,Quebec_8,Russia_4,Russia_8,
          Singapore_4,Singapore_8,Sweden_4,Sweden_8,Taipei_4,Taipei_8,Turkey_4,Turkey_8,UAE_4,UAE_8,US_4,US_8)


for (i in 1:25){
  M[i,1]=ASBS04A(COU[[2*i-1]],COU[[2*i]])
}
for (i in 1:25){
  M[i,1]=BSBS21A(COU[[2*i]])
}
M
write.table(M, file = "data.csv",sep = "\t", row.names = F)
