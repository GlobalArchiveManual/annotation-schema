

# This script shows you how to produce a species file and extract information from FishBase to use for checking future projects. 
# 1. If the project is the first in a region create a species list using EventMeasure
# 2. Select Program -> Generate Species File -> Locate the EMObs files for the project and save as an appropriate name
# 3. Check this list manually and remove any rows with unidentified species, or species that shouldn't be there. 
# Compare to other lists in the literature if they exist. This list must be 100% accurate as it will be used to check future projects.
rm(list=ls())


# Load packages----
library("rfishbase")
library("dplyr")
library("tidyr")
library("stringr")
library("readr")
library("googlesheets")

# Set working directory----
# setwd("C:/Users/JordanG/ownCloud/Post-Doc/Geographe Paper/Data")
# work.dir<-("C:/Users/Anne/Google Drive/Analysis_GlobalArchive_Australia")
work.dir<-("C:/Users/00095130/Google Drive/MEGFISH/Projects/Analysis_GlobalArchive_Australia")
work.dir<-("C:/Users/21301367/Google Drive/Work/Analysis_GlobalArchive_Australia") # Brooke

data.dir=paste(work.dir,"Data working",sep="/")

name<-"Au.check.taxa.missing"
name<-"Life.history.length.weight.conversions"

setwd(data.dir)
dir()

#Load your .csv file created during the data formatting process; note length is the only one needed
specieslist = read.csv("Au.check.taxa.missing.lw.from.life.history.csv")%>%
  distinct(Genus_species)

#load life.history
gs_ls()
LH <- gs_title("Australia.life.history")%>%#register a sheet
  gs_read_csv(ws = "australia.life.history")

LHist<-LH%>%
  select(c(Scientific,Length.measure,a,b,aLL,bLL))

# Read in country codes
country.codes = read.csv("all.country.codes.csv", colClasses = rep("character", 9)) # Convert numbers to strings


# Create a list of just the Genus_species to look up in FishBase
listforFB.all = LHist$Scientific
listforFB = listforFB.all[which(!grepl("spp", listforFB.all))] # Remove spp


# Check that your species list matches those in FishBase. Rename any that are not correct----
# The misapplied warning can generally be ignored
validatedlist = validate_names(listforFB)
warnings()

write.csv(validatedlist, paste(name, "validated.csv", sep = "."), row.names = FALSE)
# validatedlist = read.csv(paste(name, "validated.csv", sep = "."))

# Rename the species that are not correct
not.validated = as.data.frame(setdiff(listforFB, validatedlist)) # 46

test<-rfishbase::taxonomy(validatedlist)


test<-rfishbase::country(validatedlist)
names(test)

endemic<-test%>%
  dplyr::rename(country.code=C_Code)

endemic.list<-left_join(endemic,country.codes,by="country.code")

aus.list<-endemic.list%>%
  filter(name=="Australia")

all.countries<-endemic.list%>%
  filter(!is.na(name))%>%
  group_by(sciname)%>%
  dplyr::summarise(countries=paste(name,collapse="/"))

combined<-left_join(aus.list,all.countries,by="sciname")%>%
  select(sciname,countries,Status)

unique(test$Status)
write.csv(not.validated, paste(name, "not.validated.csv", sep = "."), row.names=FALSE)


# Extract length-weight information from the main page----
# We need to decide what we want and figure out a way to decide which a and b values to use to proceed from here.
#info1 = species(validatedlist[1:20],fields = c("FBname","DemersPelag","DepthRangeShallow","DepthRangeDeep","Vulnerability","Length","LTypeMaxM","LengthFemale","LTypeMaxF","CommonLength","Importance","PriceCateg","PriceReliability","Comments","SpecCode"))
LW = length_weight(validatedlist)%>%
  dplyr::rename(Scientific = sciname)

LL = length_length(validatedlist)%>%
  dplyr::rename(Scientific = sciname)

LL.dat<-info2%>%
  select(Scientific,Length1,Length2,a,b)%>%
  filter(Length2=="FL")%>%
  rename(aLL=a)%>%
  rename(bLL=b)%>%
  rename(Type=Length1)%>%
  group_by(Scientific,Type)%>%
  dplyr::summarise(aLL=mean(aLL),bLL=mean(bLL))

# info<-rename(info1, Genus_species = sciname)

write.csv(info1, paste(name, "validated.has.lw.csv", sep = "."), row.names = FALSE)
# info = read.csv(paste(name, "validated.has.lw.csv", sep = "."))

# Extract the fish without length-weight data on FishBase
# You can substitute the length-weight data with data from a similar species, or you can go hunting through scientific papers
has.lw = unique(as.character(LW$Scientific)) # 690
no.lw = as.data.frame(setdiff(validatedlist, has.lw)) # 902
write.csv(no.lw, paste(name, "no.lw.csv", sep = "."), row.names = FALSE)


# Clean up the FishBase data and join with country codes----
clean.info = info1%>%#change to LW
  select(Scientific, Type, C_Code, Locality, LengthMax, LengthMin, Number, Sex, CoeffDetermination, a, aTL, b)%>%
  mutate_at(vars(c(C_Code, Sex)), tolower)%>%
  mutate(C_Code = ifelse(grepl("Australia", Locality), "036", as.character(C_Code)))%>%
  mutate(C_Code = str_replace_all(C_Code, "a|b|c|d", ""))%>%
  left_join(select(country.codes, country.code, sub.region), by = c("C_Code" = "country.code"))%>%
  group_by(Scientific,Type)%>%
  left_join(LL.dat,by=c("Scientific","Type"))%>%
  mutate(aLL=ifelse(Type=="FL",0,aLL))%>%
  mutate(bLL=ifelse(Type=="FL",1,bLL))


# # Rank the data based on what you think is important for the life history sheet----

ranked.info = clean.info%>%
  mutate(CountryRank = ifelse(C_Code == "036", 1, ifelse(sub.region == "Australia and New Zealand", 2, ifelse(sub.region%in%c("South-Eastern Asia", "Melanesia", "Micronesia","Polynesia"), 3,ifelse(sub.region%in%c("Eastern Asia","Southern Asia"), 4,ifelse(sub.region%in%c("Eastern Africa","Western Asia"),5,6))))))%>%
  mutate(CountryRank = ifelse(is.na(sub.region), 6, CountryRank))%>%
  mutate(NumberRank = ifelse(Number>=100,1,ifelse(Number>=20&Number<100,2,ifelse(Number>=10&Number<20,3,ifelse(Number>1&Number<10,4,5)))))%>%
  mutate(NumberRank = ifelse(is.na(Number), 5, NumberRank))%>%
  # mutate(CoeffProp = CoeffDetermination/max(CoeffDetermination, na.rm = TRUE))%>%
  mutate(TypeRank = ifelse(Type=="FL",1,ifelse(Type=="TL",2,ifelse(Type%in%c("SL","WD","OT","PC","NG","AF","LP"),6,6))))%>% #ifelse(Type=="SL",3,
  mutate(TypeRank = ifelse(is.na(Type),7,TypeRank))%>%
  mutate(SexRank = ifelse(Sex%in%c("mixed","unsexed"),1,ifelse(is.na(Sex),2,ifelse(Sex=="juvenile",8,2))))%>%
  group_by(Scientific)%>%
  # mutate(MaxNumber = max(Number,na.rm = TRUE))%>%
  # mutate(MaxCoeff = max(CoeffDetermination,na.rm = TRUE))%>%
  # mutate(NumberRank = order(Number, decreasing = T))%>%
  mutate(CoeffRank = order(CoeffDetermination, decreasing = T))%>%
  mutate(ConvRank=ifelse(!is.na(aLL)&!is.na(bLL),1,8))%>%
  mutate(FinalRank = TypeRank+SexRank+CountryRank*0.5+NumberRank+CoeffRank+ConvRank)%>%
  # mutate(FinalRank2 = TypeRank+SexRank+CountryRank+NumberRank+CoeffRank)%>%
  arrange(Scientific,FinalRank)

# Extract the top ranks----
top.ranks<-ranked.info%>%
  slice(1L)%>%
  ungroup()

top.ranks2<-ranked.info2%>%
  slice(1L)%>%
  ungroup()

# Or

top.ranks.summed<-ranked.info%>%
  arrange(Scientific, FinalRank, desc(Number), CountryRank)%>%
  slice(1L)%>%
  ungroup()


check.ranking.difference<-setdiff(top.ranks,top.ranks.summed)
check.ranking.difference.2<-setdiff(top.ranks.summed,top.ranks)


# Check the top ranks
wrong.sex = filter(top.ranks, !Sex%in%c("mixed","unsexed"))
wrong.sex2 = filter(top.ranks.summed, !Sex%in%c("mixed","unsexed"))
wrong.number = filter(top.ranks, NumberRank !=1)
wrong.number2 = filter(top.ranks.summed, NumberRank!=1)
# etc.


# This is Anne checking one of the species that gets mucked up due to synonyms
# hm<-length_length("Pagrus auratus")
# hmm<-length_weight("Pagrus auratus")
# 
# hm
# hmm
# 
# hmmm<-filter(clean.info, !is.na(aTL))
# unique(hmmm$Type)


# Save the data once you are satisfied with it----
# You can now directly join with and edit the life history sheet
getwd()
write.csv(top.ranks, paste(name, "completed.lw.csv", sep="."), row.names = FALSE)

#get length-length data
# ranked.spp<-unique(top.ranks$Scientific)
# 
# top.ll <- length_length(ranked.spp)
# 
# warnings()
# 
# # add ll to top LW - average those that have multiple equations for same processs
# LL.list<-top.ll%>%
#   select(-c(Type))%>%
#   filter(Length2=="FL")%>%
#   rename(Scientific=sciname)%>%
#   rename(aLL=a)%>%
#   rename(bLL=b)%>%
#   rename(Type=Length1)%>%
#   select(Scientific,Type,aLL,bLL)%>%
#   group_by(Scientific,Type)%>%
#   dplyr::summarise(aLL=mean(aLL),bLL=mean(bLL))

#List and summarise Max lengths and create test len2gth for all species
LH.lengths<-LH%>%
  select(Scientific,FBLength_MAX)%>%
  # left_join(LMinMax,by="Scientific")%>%
  # select(Scientific,Max_length,Min_length,LengthMax,LengthMin)%>%
  mutate(Test_Length=FBLength_MAX/10*0.8)%>%
  mutate(Test_Length=ifelse(is.na(Test_Length),25,Test_Length))
  
unique(LL.list$Type)

# FIX ME ------
#instead of making test lengths here make it from the full list of species and use LH data as well

#join LL to top LW based on scientific name and measurement type - also creat a test length from min and max length
lw.ll.top<-top.ranks%>%
  select(Scientific,Type,a,b)%>%
  left_join(.,LL.dat,by=c("Scientific","Type"))%>%
  mutate(aLL=ifelse(Type=="FL",0,aLL))%>%
  mutate(bLL=ifelse(Type=="FL",1,bLL))%>%
  select(Scientific,Type,a,b,aLL,bLL)


# read in equations from Life History sheet and add
LHist.edit<-LHist%>%
  rename(Type_LH=Length.measure)%>%
  select(Scientific,Type_LH,a,b,aLL,bLL)%>%
  rename(a_LH=a)%>%
  rename(b_LH=b)%>%
  rename(aLL_LH=aLL)%>%
  rename(bLL_LH=bLL)


lw.ll.FB_LH<-LHist.edit%>% 
  left_join(lw.ll.top,by="Scientific")%>%
  left_join(LH.lengths, by = "Scientific")
  

#read in equations from Reef Life Survey and add
RLS <-read.csv("RLS master B coeffs 210917.csv")%>%
  rename(Scientific=TAXONOMIC_NAME)%>%
  rename(a_RLS=a)%>%
  rename(b_RLS=b)%>%
  rename(bLL_RLS=CF)%>%
  rename(RLS_source=B.coefficients.derived.)%>%
  mutate(aLL_RLS=0)%>%
  select(Scientific,RLS_source,a_RLS,b_RLS,aLL_RLS,bLL_RLS)

lw.ll.FB_LH_RLS<-lw.ll.FB_LH%>%
  left_join(RLS,by="Scientific")%>%
  rename(Type_FB=Type)%>%
  rename(a_FB=a)%>%
  rename(b_FB=b)%>%
  rename(aLL_FB=aLL)%>%
  rename(bLL_FB=bLL)%>%
  select(Scientific,Type_LH,a_LH,b_LH,aLL_LH,bLL_LH,Type_FB,a_FB,b_FB,aLL_FB,bLL_FB,a_RLS,b_RLS,aLL_RLS,bLL_RLS,Test_Length)



# fill out aLL and bLL for equations where the same as on other sheets
lw.comp<-lw.ll.FB_LH_RLS%>%
  mutate(aLL_FB=ifelse(is.na(aLL_FB)&a_FB==a_LH&b_FB==b_LH&!is.na(aLL_LH),aLL_LH,ifelse(is.na(aLL_FB)&a_FB==a_RLS&b_FB==b_RLS&!is.na(aLL_RLS),aLL_RLS,aLL_FB)))%>%
  mutate(bLL_FB=ifelse(is.na(bLL_FB)&a_FB==a_LH&b_FB==b_LH&!is.na(bLL_LH),bLL_LH,ifelse(is.na(bLL_FB)&a_FB==a_RLS&b_FB==b_RLS&!is.na(aLL_RLS),bLL_RLS,bLL_FB)))

lw.fix<-lw.comp%>%
  filter(is.na(aLL_FB))%>%
  filter(!is.na(a_FB))%>%
  mutate(aLL_FB=ifelse(b_FB==b_LH,aLL_LH,aLL_FB))%>%
  mutate(bLL_FB=ifelse(b_FB==b_LH,bLL_LH,bLL_FB))%>%
  mutate(aLL_FB=ifelse(b_FB==b_RLS,aLL_RLS,aLL_FB))%>%
  mutate(bLL_FB=ifelse(b_FB==b_RLS,bLL_RLS,bLL_FB))

bio.calc.comp<-lw.comp%>%
  mutate(AdjLength_LH = ((Test_Length*bLL_LH)+aLL_LH)) %>% # Adjusted length  accounts for a b not coming from for Fork length
  mutate(Mass_LH = (AdjLength_LH^b_LH)*a_LH)%>%
  mutate(AdjLength_FB = ((Test_Length*bLL_FB)+aLL_FB)) %>% # Adjusted length  accounts for a b not coming from for Fork length
  mutate(Mass_FB = (AdjLength_FB^b_FB)*a_FB)%>%
  mutate(AdjLength_RLS = ((Test_Length*bLL_RLS)+aLL_RLS)) %>% # Adjusted length  accounts for a b not coming from for Fork length
  mutate(Mass_RLS = (AdjLength_RLS^b_RLS)*a_RLS)%>%
  mutate(Mass_LH=format(Mass_LH,scientific=FALSE))%>%
  mutate(Mass_FB=format(Mass_FB,scientific=FALSE))%>%
  mutate(Mass_RLS=format(Mass_RLS,scientific=FALSE))#%>%
  # mutate(Mass_LH=ifelse(Mass_LH=="NA",0,Mass_LH))%>%
  # mutate(Mass_LH=ifelse(is.na(Mass_LH),0,Mass_LH))#%>%
  # # mutate(Mass_LH ==funs(round(Mass_LH,digits=3)))



# getting a list of species family and genus names from LH, RLS and FB
LH.names<-LH%>%
  select(Scientific,Family,Genus,Species)%>%
  Rename(LH_Family=Family)%>%
  rename(LH_Genus=Genus)%>%
  rename(LH_Species=Species)

RLS.names<-read.csv("RLS master B coeffs 210917.csv")%>%
  select(TAXONOMIC_NAME,FAMILY)%>%
  rename(Scientific=TAXONOMIC_NAME)%>%
  rename(RLS_Family=FAMILY)%>%
  mutate(GS=Scientific)%>%
  tidyr::separate(GS,into=c("RLS_Genus","RLS_Species"),sep=" ")%>%
  filter(RLS_Species!=("spp."))

FB.names<-as.data.frame(validatedlist)

FB.info<-species(validatedlist,fields = c("sciname", "FamCode","Genus","Species"))
                       