rm(list=ls())
options(scipen=999)
setwd("~/Desktop/India 2019/")
library(data.table)
library(reshape2)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # HISTORICAL RESULTS# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Load historical data 
GE = read.csv("SF Data/Historical_Election_Data.csv")

# Standardize constituency and state names 
GE$State_Name =  trimws(tolower(as.character(unlist(gsub("\\_"," ",GE$State_Name)))),which = "both")

# states taht have changed name or disappeared
GE$State_Name[GE$State_Name=="madras"]="tamil nadu" # since 1969
GE = GE[-which(GE$State=="goa, daman & diu"),] # divided in goa and damn+diu after 1984

# rename constituency for simplicity
names(GE)[which(names(GE)=="Constituency_Name")] = "PC_name"
names(GE)[which(names(GE)=="State_Name")] = "State"

GE$PC_name = tolower(GE$PC_name)

# standardize state names to fit constituency standardizer applied to old Francesca dataset
GE$PC_name[which(GE$PC_name=="l m and a islands")]="laccadive minicoy and amindivi"
GE$PC_name[which(GE$PC_name=="andaman & nicobar islan ( total electors")]="andaman & nicobar islands"
GE$PC_name[which(GE$PC_name=="andaman nicobar")]="andaman & nicobar islands"
GE$PC_name[which(GE$PC_name=="a and n islands")]="andaman & nicobar islands"

GE$PC_name[which(GE$PC_name=="autonomous district")]="autonomous districts"
GE$PC_name[which(GE$PC_name=="autonomous dists")]="autonomous districts"

# bring state names back to first dataset standardization to recycle PC code 
GE$State[which(GE$State=="chhattisgarh")] = "chattisgarh"
GE$State[which(GE$State=="odisha")] = "orissa"
GE$State[which(GE$State=="puducherry")] = "pondicherry"
GE$State[which(GE$State=="arunachal pradesh")] = "arunachal pradesh"
GE$State[which(GE$State=="gujarat")] = "gujrat"
GE$State[which(GE$State=="dadra nagar & haveli")] = "dadra & nagar haveli"
GE$State[which(GE$State=="mysore")] = "karnataka"

sort(unique(GE$State))

# PC name cleanup
# some initial ones
GE$PC_name[which(GE$PC_name=="amalapruam")] = "amalapuram"
GE$PC_name[which(GE$PC_name=="anakapalle")] = "anakapalli"
GE$PC_name[which(GE$PC_name=="anantapur")] = "anantpur"
GE$PC_name[which(GE$PC_name=="araku")] = "aruku"
GE$PC_name[which(GE$PC_name=="chelvella")] = "chevella"
GE$PC_name[which(GE$PC_name=="chittor")] = "chittoor"
GE$PC_name[which(GE$PC_name=="hyderabad")] = "hydrabad"
GE$PC_name[which(GE$PC_name=="mahbubabad")] = "mahabubabad"
GE$PC_name[which(GE$PC_name=="mahabubnagar")] = "mahbubnagar"
GE$PC_name[which(GE$PC_name=="padapalli")] = "peddapalli"

# andhra pradesh
sort(unique(GE$State))
sort(unique(GE$PC_name[GE$State=="andhra pradesh"]))
unique(GE$PC_name[GE$State=="andhra pradesh" & GE$Year>1999])
GE$PC_name[which(GE$PC_name=="vijaywada")] = "vijayawada"
GE$PC_name[which(GE$PC_name=="warrangal")] = "warangal"
GE$PC_name[which(GE$PC_name=="tirupathi")] = "tirupati"
GE$PC_name[which(GE$PC_name=="secundrabad")] = "secunderabad"
GE$PC_name[which(GE$PC_name=="peddapalli")] = "peddapalle"
GE$PC_name[which(GE$PC_name=="paravathipuram")] = "parvathipuram"
GE$PC_name[which(GE$PC_name=="narasapur")] = "narsapuram"
GE$PC_name[which(GE$PC_name=="cuddapah")] = "kadapa"
GE$PC_name[which(GE$PC_name=="sinddipet(sc)")] = "siddipet"
GE$PC_name[which(GE$PC_name=="warangal(sc)")] = "warangal"


# arunachal pradesh
sort(unique(GE$State))
unique(GE$PC_name[GE$State=="arunachal pradesh"])
# assam
sort(unique(GE$State))
sort(unique(GE$PC_name[GE$State=="assam"]))
unique(GE$PC_name[GE$State=="assam" & GE$Year==2014])
GE$PC_name[which(GE$PC_name=="mangaldai")] = "mangaldoi"
GE$PC_name[which(GE$PC_name=="lakhimpur , district, lakhimpur")] = "lakhimpur"
GE$PC_name[which(GE$PC_name=="diphu(st)")] = "diphu"

#GE$PC_name[which(GE$PC_name=="cachar")] = "silchar"
#GE$PC_name[which(GE$PC_name=="darrang")] = "mangaldoi"
#GE$PC_name[which(GE$PC_name=="goalpara")] = "dhubri"
#GE$PC_name[which(GE$PC_name=="sibsagar")] = "jorhat"

# bihar
sort(unique(GE$State))
sort(unique(GE$PC_name[GE$State=="bihar"]))
sort(unique(GE$PC_name[GE$State=="bihar" & GE$Year>2013]))
GE$PC_name[which(GE$PC_name=="kessaria")] = "kesaria"
GE$PC_name[which(GE$PC_name=="madhipura")] = "madhepura"
GE$PC_name[which(GE$PC_name=="monghyr")] = "munger"
GE$PC_name[which(GE$PC_name=="monger")] = "munger"
GE$PC_name[which(GE$PC_name=="palamu")] = "palamau"
GE$PC_name[which(GE$PC_name=="patna")] = "patna sahib"
GE$PC_name[which(GE$PC_name=="hazari bagh")] = "hazaribagh"
GE$PC_name[which(GE$PC_name=="pupari")] = "pupri"
GE$PC_name[which(GE$PC_name=="purnea")] = "purnia"
GE$PC_name[which(GE$PC_name=="ranchi east")] = "ranchi"
GE$PC_name[which(GE$PC_name=="ranchi west")] = "ranchi"

GE$PC_name[which(GE$PC_name=="gopalganj (sc)")] = "gopalganj"
GE$PC_name[which(GE$PC_name=="gaya (sc)")] = "gaya"
GE$PC_name[which(GE$PC_name=="hajipur (sc)")] = "hajipur"
GE$PC_name[which(GE$PC_name=="jamui (sc)")] = "jamui"
GE$PC_name[which(GE$PC_name=="lohardaga(st)")] = "lohardaga"
GE$PC_name[which(GE$PC_name=="samastipur (sc)")] = "samastipur"
GE$PC_name[which(GE$PC_name=="sasaram (sc)")] = "sasaram"


# Jharkhand recently split from Bihar (2000) 
sort(unique(GE$State))
sort(unique(GE$PC_name[GE$State=="bihar"]))
sort(unique(GE$PC_name[GE$State=="bihar" & GE$Year>2013]))
GE$PC_name[which(GE$PC_name=="dumka(st)")] = "dumka"
GE$State[which(GE$PC_name=="chatra")] = "jharkhand"
GE$State[which(GE$PC_name=="dhanbad")] = "jharkhand"
GE$State[which(GE$PC_name=="dumka")] = "jharkhand"
GE$State[which(GE$PC_name=="giridih")] = "jharkhand"
GE$State[which(GE$PC_name=="godda")] = "jharkhand"
GE$State[which(GE$PC_name=="hazaribagh")] = "jharkhand"
GE$State[which(GE$PC_name=="jamshedpur")] = "jharkhand"
GE$State[which(GE$PC_name=="khunti")] = "jharkhand"
GE$State[which(GE$PC_name=="kodarma")] = "jharkhand"
GE$State[which(GE$PC_name=="lohardaga")] = "jharkhand"
GE$State[which(GE$PC_name=="palamau")] = "jharkhand"
GE$State[which(GE$PC_name=="rajmahal")] = "jharkhand"
GE$State[which(GE$PC_name=="ranchi")] = "jharkhand"
GE$State[which(GE$PC_name=="singhbhum")] = "jharkhand"

sort(unique(GE$PC_name[GE$State=="jharkhand"]))


# Chhattisgarh
sort(unique(GE$State))
sort(unique(GE$PC_name[GE$State=="chattisgarh"]))
sort(unique(GE$PC_name[GE$State=="chattisgarh" & GE$Year>1992]))
GE$PC_name[which(GE$PC_name=="janjgir-champa")] = "janjgir"
GE$PC_name[which(GE$PC_name=="surguja")] = "sarguja"

# goa
sort(unique(GE$State))
sort(unique(GE$PC_name[GE$State=="goa"]))
sort(unique(GE$PC_name[GE$State=="goa" & GE$Year>1999]))
GE$PC_name[which(GE$PC_name=="mormugao")] = "south goa"
GE$PC_name[which(GE$PC_name=="panaji")] = "north goa"
# gujrat
sort(unique(GE$State))
sort(unique(GE$PC_name[GE$State=="gujrat"]))
sort(unique(GE$PC_name[GE$State=="gujrat" & GE$Year>1999]))
GE$PC_name[which(GE$PC_name=="panchmahals")] = "panchmahal"
GE$PC_name[which(GE$PC_name=="broach")] = "bharuch"
GE$PC_name[which(GE$PC_name=="baroda")] = "vadodara"
GE$PC_name[which(GE$PC_name=="bulsar")] = "valsad"
GE$PC_name[which(GE$PC_name=="dohad")] = "dahod"
GE$PC_name[which(GE$PC_name=="kaira")] = "kheda"
GE$PC_name[which(GE$PC_name=="kutch")] = "kachchh"
GE$PC_name[which(GE$PC_name=="mehsana")] = "mahesana"
# haryana
sort(unique(GE$State))
sort(unique(GE$PC_name[GE$State=="haryana"]))
sort(unique(GE$PC_name[GE$State=="haryana" & GE$Year>1999]))
GE$PC_name[which(GE$PC_name=="hisar")] = "hissar"
GE$PC_name[which(GE$PC_name=="sonepat")] = "sonipat"
GE$PC_name[which(GE$PC_name=="rohtak(pc)")] = "rohtak"
GE$PC_name[which(GE$PC_name=="sirsa(sc)")] = "sirsa"

# himachal pradesh
sort(unique(GE$State))
sort(unique(GE$PC_name[GE$State=="himachal pradesh"]))
sort(unique(GE$PC_name[GE$State=="himachal pradesh" & GE$Year>1999]))
GE$PC_name[which(GE$PC_name=="simla")] = "shimla"
# jammu & kashmir
sort(unique(GE$State))
sort(unique(GE$PC_name[GE$State=="jammu & kashmir"]))
sort(unique(GE$PC_name[GE$State=="jammu & kashmir" & GE$Year>1999]))
# jharkhand
sort(unique(GE$State))
sort(unique(GE$PC_name[GE$State=="jharkhand"]))
sort(unique(GE$PC_name[GE$State=="jharkhand" & GE$Year>1999]))
# karnataka
sort(unique(GE$State))
sort(unique(GE$PC_name[GE$State=="karnataka"]))
sort(unique(GE$PC_name[GE$State=="karnataka" & GE$Year>=2014]))
GE$PC_name[which(GE$PC_name=="banglore")] = "bangalore"
GE$PC_name[which(GE$PC_name=="bangalore city")] = "bangalore north"
GE$PC_name[which(GE$PC_name=="belgaum")] = "belagavi"
GE$PC_name[which(GE$PC_name=="chikkballapur")] = "chikballapur"
GE$PC_name[which(GE$PC_name=="chikodi")] = "chikkodi"
GE$PC_name[which(GE$PC_name=="davangere")] = "davanagere"
GE$PC_name[which(GE$PC_name=="dharwar north")] = "dharward north"
GE$PC_name[which(GE$PC_name=="dharwar south")] = "dharward south"
GE$PC_name[which(GE$PC_name=="kanara")] = "uttara kannada"
GE$PC_name[which(GE$PC_name=="udipi")] = "udupi chikmagalur"
GE$PC_name[which(GE$PC_name=="udupi")] = "udupi chikmagalur"

# kerala
sort(unique(GE$State))
sort(unique(GE$PC_name[GE$State=="kerala"]))
sort(unique(GE$PC_name[GE$State=="kerala" & GE$Year>=2014]))
GE$PC_name[which(GE$PC_name=="alleppey")] = "alappuzha"
GE$PC_name[which(GE$PC_name=="badagara")] = "vadakara"
GE$PC_name[which(GE$PC_name=="calicut")] = "kozhikode"
GE$PC_name[which(GE$PC_name=="cannanore")] = "kannur"
GE$PC_name[which(GE$PC_name=="kasargod")] = "kasaragod"
GE$PC_name[which(GE$PC_name=="kasergod")] = "kasaragod"
GE$PC_name[which(GE$PC_name=="manjeri")] = "malappuram"
GE$PC_name[which(GE$PC_name=="mavelikkara")] = "mavelikara"
GE$PC_name[which(GE$PC_name=="mavilekara")] = "mavelikara"
GE$PC_name[which(GE$PC_name=="muvattupuzha")] = "muvattpuzha"
GE$PC_name[which(GE$PC_name=="muvattu puzha")] = "muvattpuzha"
GE$PC_name[which(GE$PC_name=="palghat")] = "pathanamthitta"
GE$PC_name[which(GE$PC_name=="poonani")] = "ponnani"
GE$PC_name[which(GE$PC_name=="qullon")] = "quilon"
GE$PC_name[which(GE$PC_name=="telli cherry")] = "tellicherry"
GE$PC_name[which(GE$PC_name=="trichur")] = "thrissur"
GE$PC_name[which(GE$PC_name=="trivandrum")] = "trivanduram"
GE$PC_name[which(GE$PC_name=="ottappalam(sc)")] = "ottapalam"
GE$PC_name[which(GE$PC_name=="mukundapuram")] = "mukandapuram"
GE$PC_name[which(GE$PC_name=="malappuram , district, malappuram")] = "malappuram"



# madhya pradesh
sort(unique(GE$State))
sort(unique(GE$PC_name[GE$State=="madhya pradesh"]))
sort(unique(GE$PC_name[GE$State=="madhya pradesh" & GE$Year>=2014]))
GE$PC_name[which(GE$PC_name=="jhabua")] = "ratlam"
GE$PC_name[which(GE$PC_name=="mandsour")] = "mandsaur"
GE$PC_name[which(GE$PC_name=="shahjapur")] = "shajapur"
GE$PC_name[which(GE$PC_name=="ratlam(st) jhabua")] = "ratlam"
GE$PC_name[which(GE$PC_name=="sagar(sc)")] = "sagar"
GE$PC_name[which(GE$PC_name=="shahdol(st) , district, anooppur")] = "shahdol"
GE$PC_name[which(GE$PC_name=="siddhi")] = "sidhi"
GE$PC_name[which(GE$PC_name=="sindhi")] = "sidhi"
GE$PC_name[which(GE$PC_name=="khargon")] = "khargone"
GE$PC_name[which(GE$PC_name=="mahasamud")] = "mahasamund"


# Chhattisgarh recently split from "madhya pradesh" (2000)
sort(unique(GE$State))
sort(unique(GE$PC_name[GE$State=="madhya pradesh"]))
sort(unique(GE$PC_name[GE$State=="madhya pradesh" & GE$Year>=2014]))
GE$State[which(GE$PC_name=="bastar")] = "chattisgarh"
GE$State[which(GE$PC_name=="bilaspur")] = "chattisgarh"
GE$State[which(GE$PC_name=="durg")] = "chattisgarh"
GE$State[which(GE$PC_name=="janjgir")] = "chattisgarh"
GE$State[which(GE$PC_name=="kanker")] = "chattisgarh"
GE$State[which(GE$PC_name=="kanker")] = "chattisgarh"
GE$State[which(GE$PC_name=="raigarh")] = "chattisgarh"
GE$State[which(GE$PC_name=="raipur")] = "chattisgarh"
GE$State[which(GE$PC_name=="rajnandgaon")] = "chattisgarh"
GE$State[which(GE$PC_name=="shajapur")] = "chattisgarh"

# maharashtra
sort(unique(GE$State))
sort(unique(GE$PC_name[GE$State=="maharashtra"]))
sort(unique(GE$PC_name[GE$State=="maharashtra" & GE$Year>=2014]))
GE$PC_name[which(GE$PC_name=="ahmadnagar")] = "ahmednagar"
GE$PC_name[which(GE$PC_name=="bhandara - gondiya")] = "bhandara-gondiya"
GE$PC_name[which(GE$PC_name=="bhir")] = "beed"
GE$PC_name[which(GE$PC_name=="bombay central")] = "mumbai central"
GE$PC_name[which(GE$PC_name=="bombay central south")] = "mumbai south central"
GE$PC_name[which(GE$PC_name=="bombay city central north")] = "mumbai north central"
GE$PC_name[which(GE$PC_name=="bombay city central south")] = "mumbai south central"
GE$PC_name[which(GE$PC_name=="bombay city north")] = "mumbai north"
GE$PC_name[which(GE$PC_name=="bombay city south")] = "mumbai south"
GE$PC_name[which(GE$PC_name=="bombay north")] = "mumbai north"
GE$PC_name[which(GE$PC_name=="bombay north central")] = "mumbai north central"
GE$PC_name[which(GE$PC_name=="bombay north east")] = "mumbai north east"
GE$PC_name[which(GE$PC_name=="bombay north west")] = "mumbai north west"
GE$PC_name[which(GE$PC_name=="bombay south")] = "mumbai south"
GE$PC_name[which(GE$PC_name=="bombay south central")] = "mumbai south central"
GE$PC_name[which(GE$PC_name=="buldhana")] = "buldana"
GE$PC_name[which(GE$PC_name=="hatkanangale")] = "hatkanangle"
GE$PC_name[which(GE$PC_name=="miraj")] = "sangli"
GE$PC_name[which(GE$PC_name=="mumbai")] = "mumbai central"
GE$PC_name[which(GE$PC_name=="nasik")] = "nashik"
GE$PC_name[which(GE$PC_name=="poona")] = "pune"
GE$PC_name[which(GE$PC_name=="ratnagiri sindhudurg")] = "ratnagiri-sindhudurg"
GE$PC_name[which(GE$PC_name=="ratnagiri - sindhudurg")] = "ratnagiri-sindhudurg"
GE$PC_name[which(GE$PC_name=="sholapur")] = "solapur"
GE$PC_name[which(GE$PC_name=="thana")] = "thane"
GE$PC_name[which(GE$PC_name=="yeotmal")] = "yavatmal"


GE$PC_name[which(GE$PC_name=="bombay north-east")] = "mumbai north east"
GE$PC_name[which(GE$PC_name=="mumbainorthwest")] = "mumbai north west"
GE$PC_name[which(GE$PC_name=="mumbai   south")] = "mumbai south"
GE$PC_name[which(GE$PC_name=="nandrbar(st)")] = "nandurbar"


# manipur
sort(unique(GE$State))
sort(unique(GE$PC_name[GE$State=="manipur"]))
sort(unique(GE$PC_name[GE$State=="manipur" & GE$Year>=2014]))
GE$PC_name[which(GE$PC_name=="outer manipur (st)")] = "outer manipur"

# meghalaya
sort(unique(GE$State))
sort(unique(GE$PC_name[GE$State=="meghalaya"]))
sort(unique(GE$PC_name[GE$State=="meghalaya" & GE$Year>=2014]))
GE$PC_name[which(GE$PC_name=="tura(st), district,west garo hills")] = "tura"

# mizoram
sort(unique(GE$State))
sort(unique(GE$PC_name[GE$State=="mizoram"]))
sort(unique(GE$PC_name[GE$State=="mizoram" & GE$Year>=2014]))
# nagaland
sort(unique(GE$State))
sort(unique(GE$PC_name[GE$State=="nagaland"]))
sort(unique(GE$PC_name[GE$State=="nagaland" & GE$Year>=2014]))
# odisha
sort(unique(GE$State))
sort(unique(GE$PC_name[GE$State=="orissa"]))
sort(unique(GE$PC_name[GE$State=="orissa" & GE$Year>=2014]))
GE$PC_name[which(GE$PC_name=="bhanjanagar")] = "aska"
GE$PC_name[which(GE$PC_name=="nowrangpur")] = "nabarangpur"
GE$PC_name[which(GE$PC_name=="sundergarh")] = "sundargarh"
# punjab
sort(unique(GE$State))
sort(unique(GE$PC_name[GE$State=="punjab"]))
sort(unique(GE$PC_name[GE$State=="punjab" & GE$Year>=2014]))
GE$PC_name[which(GE$PC_name=="bhatinda")] = "bathinda"
GE$PC_name[which(GE$PC_name=="ferozepur")] = "firozpur"
GE$PC_name[which(GE$PC_name=="ferozpur")] = "firozpur"
GE$PC_name[which(GE$PC_name=="jullundur")] = "jalandhar"
GE$PC_name[which(GE$PC_name=="rupar")] = "ropar"
GE$PC_name[which(GE$PC_name=="jalandhar p.c.")] = "jalandhar"

# Himchal Pradesh was created out of bits of Punjab in 1971 (1966)
# Haryana split from Pubjab in on November 1st 1966

sort(unique(GE$State))
sort(unique(GE$PC_name[GE$State=="punjab"]))
sort(unique(GE$PC_name[GE$State=="punjab" & GE$Year>=2014]))
GE$State[which(GE$PC_name=="ambala")] = "haryana"
GE$State[which(GE$PC_name=="mahendragarh")] = "haryana"
GE$State[which(GE$PC_name=="gurgaon")] = "haryana"
GE$State[which(GE$PC_name=="hissar")] = "haryana"
GE$State[which(GE$PC_name=="karnal")] = "haryana"
GE$State[which(GE$PC_name=="rohtak")] = "haryana"


# rajasthan
sort(unique(GE$State))
sort(unique(GE$PC_name[GE$State=="rajasthan"]))
sort(unique(GE$PC_name[GE$State=="rajasthan" & GE$Year>=2014]))
GE$PC_name[which(GE$PC_name=="chitorgarh")] = "chittorgarh"
GE$PC_name[which(GE$PC_name=="jhun jhunu")] = "jhunjhunu"
GE$PC_name[which(GE$PC_name=="kotah")] = "kota"
GE$PC_name[which(GE$PC_name=="bayana(sc)")] = "bayana"
GE$PC_name[which(GE$PC_name=="tonk(sc)")] = "tonk"

# sikkim
sort(unique(GE$State))
sort(unique(GE$PC_name[GE$State=="sikkim"]))
sort(unique(GE$PC_name[GE$State=="sikkim" & GE$Year>=2014]))
# tamil nadu
sort(unique(GE$State))
sort(unique(GE$PC_name[GE$State=="tamil nadu"]))
sort(unique(GE$PC_name[GE$State=="tamil nadu" & GE$Year>=2014]))
GE$PC_name[which(GE$PC_name=="gohichettipalayam")] = "gobichettipalayam"
GE$PC_name[which(GE$PC_name=="madras central")] = "chennai central"
GE$PC_name[which(GE$PC_name=="madras north")] = "chennai north"
GE$PC_name[which(GE$PC_name=="madras south")] = "chennai south"
GE$PC_name[which(GE$PC_name=="mayiladuthurai")] = "mayiladuturai"
GE$PC_name[which(GE$PC_name=="mayuram")] = "mayiladuturai"
#GE$PC_name[which(GE$PC_name=="palani")] = "theni"
GE$PC_name[which(GE$PC_name=="periakulam")] = "periyakulam"
GE$PC_name[which(GE$PC_name=="sivakasi")] = "virudhunagar"
GE$PC_name[which(GE$PC_name=="tenkasai")] = "tenkasi"
GE$PC_name[which(GE$PC_name=="tindivanam")] = "viluppuram"
GE$PC_name[which(GE$PC_name=="tiruchirapali")] =  "tiruchirapalli"
GE$PC_name[which(GE$PC_name=="tiruchirappalli")] ="tiruchirapalli"
GE$PC_name[which(GE$PC_name=="tiruppattur")] = "tirupattur"
GE$PC_name[which(GE$PC_name=="wandiwash")] = "vandavasi"
GE$PC_name[which(GE$PC_name=="tiruchirapalli")] = "thiruchirapalli"


# telangana
sort(unique(GE$State))
sort(unique(GE$PC_name[GE$State=="andhra pradesh"]))
sort(unique(GE$PC_name[GE$State=="andhra pradesh" & GE$Year>=2014]))
# Telangana has recently split from Andhra Pradesh (2014) - let's re-describe the constituencies 
GE$State[which(GE$PC_name=="adilabad")] = "telangana"
GE$State[which(GE$PC_name=="bhongir")] = "telangana"
GE$State[which(GE$PC_name=="chevella")] = "telangana"
GE$State[which(GE$PC_name=="hydrabad")] = "telangana"
GE$State[which(GE$PC_name=="karimnagar")] = "telangana"
GE$State[which(GE$PC_name=="khammam")] = "telangana"
GE$State[which(GE$PC_name=="mahabubabad")] = "telangana"
GE$State[which(GE$PC_name=="mahbubnagar")] = "telangana"
GE$State[which(GE$PC_name=="malkajgiri")] = "telangana"
GE$State[which(GE$PC_name=="medak")] = "telangana"
GE$State[which(GE$PC_name=="nagarkurnool")] = "telangana"
GE$State[which(GE$PC_name=="nalgonda")] = "telangana"
GE$State[which(GE$PC_name=="nizamabad")] = "telangana"
GE$State[which(GE$PC_name=="peddapalle")] = "telangana"
GE$State[which(GE$PC_name=="secunderabad")] = "telangana"
GE$State[which(GE$PC_name=="warangal")] = "telangana"
GE$State[which(GE$PC_name=="zahirabad")] = "telangana"

# tripura 
sort(unique(GE$State))
sort(unique(GE$PC_name[GE$State=="tripura"]))
sort(unique(GE$PC_name[GE$State=="tripura" & GE$Year>=2014]))
GE$PC_name[which(GE$PC_name=="tripurawest")] = "tripura west"

# uttar pradesh
sort(unique(GE$State))
sort(unique(GE$PC_name[GE$State=="uttar pradesh"]))
sort(unique(GE$PC_name[GE$State=="uttar pradesh" & GE$Year>=2014]))
GE$PC_name[which(GE$PC_name=="bara banki")] = "barabanki"
GE$PC_name[which(GE$PC_name=="budaun")] = "badaun"
GE$PC_name[which(GE$PC_name=="bulandsahar")] = "bulandshahr"
GE$PC_name[which(GE$PC_name=="domariaganj")] = "domariyaganj"
GE$PC_name[which(GE$PC_name=="gautam buddh nagar")] = "gautam budhha nagar"
GE$PC_name[which(GE$PC_name=="gazipur")] = "ghazipur"
GE$PC_name[which(GE$PC_name=="hatras")] = "hathras"
GE$PC_name[which(GE$PC_name=="kaisarganj")] = "kaiserganj"
GE$PC_name[which(GE$PC_name=="machhalishahr")] = "machhlishahr"
GE$PC_name[which(GE$PC_name=="merut")] = "meerut"
GE$PC_name[which(GE$PC_name=="mishrikh")] = "misrikh"
GE$PC_name[which(GE$PC_name=="marodabad")] = "moradabad"
GE$PC_name[which(GE$PC_name=="musafirkhana")] = "muzaffarnagar"
GE$PC_name[which(GE$PC_name=="naini tal")] = "nainital"
GE$PC_name[which(GE$PC_name=="rasra")] = "ballia"
GE$PC_name[which(GE$PC_name=="robertganj")] = "robertsganj"
GE$PC_name[which(GE$PC_name=="tehri garhawal")] = "tehri garhwal"
GE$PC_name[which(GE$PC_name=="tehri garwal")] = "tehri garhwal"
GE$PC_name[which(GE$PC_name=="bijnor(sc)")] = "bijnor"
GE$PC_name[which(GE$PC_name=="bilhaur")] = "bilhour"
GE$PC_name[which(GE$PC_name=="bijnor(sc)")] = "bijnor"
GE$PC_name[which(GE$PC_name=="fatehpur")] = "fathepur"
GE$PC_name[which(GE$PC_name=="gautam buddha nagar")] = "gautam budhha nagar"
GE$PC_name[which(GE$PC_name=="hardwar(sc)")] = "hardwar"
GE$PC_name[which(GE$PC_name=="mainpuri(gen)")] = "mainpuri"
GE$PC_name[which(GE$PC_name=="mathur")] = "mathura"
GE$PC_name[which(GE$PC_name=="rae bareli")] = "raebareli"

# uttarkhand
sort(unique(GE$State))
sort(unique(GE$PC_name[GE$State=="uttarakhand"]))
sort(unique(GE$PC_name[GE$State=="uttarakhand" & GE$Year>=2014]))
sort(unique(GE$PC_name[GE$State=="uttar pradesh"]))
GE$State[which(GE$PC_name=="almora")] = "uttarakhand"
GE$State[which(GE$PC_name=="garhwal")] = "uttarakhand"
GE$State[which(GE$PC_name=="hardwar")] = "uttarakhand"
GE$State[which(GE$PC_name=="nainital")] = "uttarakhand"
GE$State[which(GE$PC_name=="tehri garhwal")] = "uttarakhand"
GE$PC_name[which(GE$PC_name=="total electors")] = NA
GE$PC_name[which(GE$PC_name=="nainital")] = "nainital-udhamsingh nagar"
GE$PC_name[which(GE$PC_name=="tehrigharwal")] = "tehri garhwal"

# west bengal 
sort(unique(GE$State))
sort(unique(GE$PC_name[GE$State=="west bengal"]))
sort(unique(GE$PC_name[GE$State=="west bengal" & GE$Year>=2014]))
GE$PC_name[which(GE$PC_name=="arambagh")] = "arambag"
GE$PC_name[which(GE$PC_name=="ausgram")] = "durgapur"
GE$PC_name[which(GE$PC_name=="barrackpore")] = "barrackpur"
GE$PC_name[which(GE$PC_name=="berhampore")] = "baharampur"
GE$PC_name[which(GE$PC_name=="berhampur")] = "baharampur"
GE$PC_name[which(GE$PC_name=="bholpur")] = "bolpur"
GE$PC_name[which(GE$PC_name=="burdwan")] = "bardhaman"
GE$PC_name[which(GE$PC_name=="burdwan - durgapur")] = "bardhaman-durgapur"
GE$PC_name[which(GE$PC_name=="calcutta south")] = "kolkata dakshin" # direct translation here - other calcullta are bits and pieces 
GE$PC_name[which(GE$PC_name=="contai")] = "kanthi"
GE$PC_name[which(GE$PC_name=="coochbehar")] = "cooch behar"

GE$PC_name[which(GE$PC_name=="mathurapur(sc)")] = "mathurapur"


GE$PC_name[which(GE$PC_name=="daimond harbour")] = "diamond harbour"
GE$PC_name[which(GE$PC_name=="durgapur")] = "durgapur"
GE$PC_name[which(GE$PC_name=="joynagar")] = "jaynagar"
GE$PC_name[which(GE$PC_name=="krishnagar")] = "krishnanagar"
GE$PC_name[which(GE$PC_name=="malda")] = "maldaha"
GE$PC_name[which(GE$PC_name=="midnapore")] = "medinipur"
GE$PC_name[which(GE$PC_name=="midnapur")] = "medinipur"
GE$PC_name[which(GE$PC_name=="nebadwip")] = "nabadwip"
GE$PC_name[which(GE$PC_name=="serampore")] = "srerampur"
GE$PC_name[which(GE$PC_name=="sreerampur")] = "srerampur"
GE$PC_name[which(GE$PC_name=="vishnupur")] = "bishnupur"

GE$PC_name[which(GE$PC_name=="coochbehar(sc), district, coochbehar")] = "cooch behar"
GE$PC_name[which(GE$PC_name=="nawadwip")] = "nabadwip"
GE$PC_name[which(GE$PC_name=="perulia")] = "purulia"
GE$PC_name[which(GE$PC_name=="tamluk(gen), district, purba medinipur")] = "tamluk"



# andaman & nicobar islands
sort(unique(GE$State))
sort(unique(GE$PC_name[GE$State=="andaman & nicobar islands"]))
sort(unique(GE$PC_name[GE$State=="andaman & nicobar islands" & GE$Year>=2014]))
# chandigarh
sort(unique(GE$State))
sort(unique(GE$PC_name[GE$State=="chandigarh"]))
sort(unique(GE$PC_name[GE$State=="chandigarh" & GE$Year>=2014]))
# dadra & nagar haveli
sort(unique(GE$State))
sort(unique(GE$PC_name[GE$State=="dadra & nagar haveli"]))
sort(unique(GE$PC_name[GE$State=="dadra & nagar haveli" & GE$Year>=2014]))
GE$PC_name[which(GE$PC_name=="dadar & nagar haveli")] = "dadra & nagar haveli"
GE$PC_name[which(GE$PC_name=="dadra nagar haveli")] = "dadra & nagar haveli"
GE$PC_name[which(GE$PC_name=="d and n haveli")] = "dadra & nagar haveli"
# daman & diu
sort(unique(GE$State))
sort(unique(GE$PC_name[GE$State=="daman & diu"]))
sort(unique(GE$PC_name[GE$State=="daman & diu" & GE$Year>=2014]))
GE$PC_name[which(GE$PC_name=="daman and diu")] = "daman & diu"
GE$PC_name[which(GE$PC_name=="diu")] = "daman & diu"
# lakshadweep
sort(unique(GE$State))
sort(unique(GE$PC_name[GE$State=="lakshadweep"]))
sort(unique(GE$PC_name[GE$State=="lakshadweep" & GE$Year>=2014]))
# delhi
sort(unique(GE$State))
sort(unique(GE$PC_name[GE$State=="delhi"]))
sort(unique(GE$PC_name[GE$State=="delhi" & GE$Year>=2014]))
GE$PC_name[which(GE$PC_name=="eastdelhi")] = "east delhi"
GE$PC_name[which(GE$PC_name=="north west  delhi")] = "north west delhi"
GE$PC_name[which(GE$PC_name=="west  delhi")] = "west delhi"

# pondicherry
sort(unique(GE$State))
sort(unique(GE$PC_name[GE$State=="pondicherry"]))
sort(unique(GE$PC_name[GE$State=="pondicherry" & GE$Year>=2014]))
GE$PC_name[which(GE$PC_name=="puducherry")] = "pondicherry"


# Telangana has recently split from Andhra Pradesh (2014) - let's re-describe the constituencies 
# Jharkhand recently split from Bihar (2000) 
# Chhattisgarh recently split from "madhya pradesh" (2000)
# Haryana split from Pubjab in on November 1st 1966
# Himchal Pradesh was created out of bits of Punjab in 1971 (1966)
# Uttarakhand split from Uttar Pradesh in November 2000
dim(unique(GE[GE$Year>=2014,c("State","PC_name")]))
# check that number of constituencies agrees with the wikipedia page 
# 25
length(unique(GE$PC_name[GE$State=="andhra pradesh" & GE$Year>=2014]))
# 2
length(unique(GE$PC_name[GE$State=="arunachal pradesh" & GE$Year>=2014]))
# 14
length(unique(GE$PC_name[GE$State=="assam" & GE$Year>=2014]))
# 40
length(unique(GE$PC_name[GE$State=="bihar" & GE$Year>=2014]))
# 11
length(unique(GE$PC_name[GE$State=="chattisgarh" & GE$Year>=2014]))
# 2
length(unique(GE$PC_name[GE$State=="goa" & GE$Year>=2014]))
# 26
length(unique(GE$PC_name[GE$State=="gujrat" & GE$Year>=2014]))
# 10
length(unique(GE$PC_name[GE$State=="haryana" & GE$Year>=2014]))
# 4
length(unique(GE$PC_name[GE$State=="himachal pradesh" & GE$Year>=2014]))
# 6
length(unique(GE$PC_name[GE$State=="jammu & kashmir" & GE$Year>=2014]))
# 14
length(unique(GE$PC_name[GE$State=="jharkhand" & GE$Year>=2014]))
# 28
length(unique(GE$PC_name[GE$State=="karnataka" & GE$Year>=2014])) # missing one here 
# 20
length(unique(GE$PC_name[GE$State=="kerala" & GE$Year>=2014]))
# 29
length(unique(GE$PC_name[GE$State=="madhya pradesh" & GE$Year>=2014])) # missing one here
# 48
length(unique(GE$PC_name[GE$State=="maharashtra" & GE$Year>=2014]))
# 2
length(unique(GE$PC_name[GE$State=="manipur" & GE$Year>=2014]))
# 2
length(unique(GE$PC_name[GE$State=="meghalaya" & GE$Year>=2014]))
# 1
length(unique(GE$PC_name[GE$State=="mizoram" & GE$Year>=2014]))
# 1
length(unique(GE$PC_name[GE$State=="nagaland" & GE$Year>=2014]))
# 21
length(unique(GE$PC_name[GE$State=="orissa" & GE$Year>=2014]))
# 13
length(unique(GE$PC_name[GE$State=="punjab" & GE$Year>=2014]))
# 25
length(unique(GE$PC_name[GE$State=="rajasthan" & GE$Year>=2014]))
# 1
length(unique(GE$PC_name[GE$State=="sikkim" & GE$Year>=2014]))
# 39
length(unique(GE$PC_name[GE$State=="tamil nadu" & GE$Year>=2014]))
# 17
length(unique(GE$PC_name[which(GE$State=="telangana")])) 
# 2
length(unique(GE$PC_name[GE$State=="tripura" & GE$Year>=2014]))
# 80
length(unique(GE$PC_name[GE$State=="uttar pradesh" & GE$Year>=2014]))
# 5
length(unique(GE$PC_name[GE$State=="uttarakhand" & GE$Year>=2014]))
# 42
length(unique(GE$PC_name[GE$State=="west bengal" & GE$Year>=2014]))
# 1
length(unique(GE$PC_name[GE$State=="andaman & nicobar islands" & GE$Year>=2014]))
# 1
length(unique(GE$PC_name[GE$State=="chandigarh" & GE$Year>=2014]))
# 1
length(unique(GE$PC_name[GE$State=="dadra & nagar haveli" & GE$Year>=2014]))
# 1
length(unique(GE$PC_name[GE$State=="daman & diu" & GE$Year>=2014]))
# 1
length(unique(GE$PC_name[GE$State=="lakshadweep" & GE$Year>=2014]))
# 7
length(unique(GE$PC_name[GE$State=="delhi" & GE$Year>=2014]))
# 1
length(unique(GE$PC_name[GE$State=="pondicherry" & GE$Year>=2014]))

dim(unique(GE[GE$Year==2014,c("State","PC_name")]))

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # The following alliance members reflect onnly 2014 MEMBERSHIP
GE$Party.Alliance.fourteen = NA
GE$Party.Alliance.fourteen[which(GE$Party=="AINRC")] = "NDA"
GE$Party.Alliance.fourteen[which(GE$Party=="AD")] = "NDA"
GE$Party.Alliance.fourteen[which(GE$Party=="BJP")] = "NDA"
GE$Party.Alliance.fourteen[which(GE$Party=="DMDK")] = "NDA"
#GE$Party.Alliance.fourteen[which(GE$Party=="GVP")] = "NDA"
GE$Party.Alliance.fourteen[which(GE$Party=="JaSPa")] = "NDA"
GE$Party.Alliance.fourteen[which(GE$Party=="HJCBL")] = "NDA"
GE$Party.Alliance.fourteen[which(GE$Party=="LJP")] = "NDA"
GE$Party.Alliance.fourteen[which(GE$Party=="MPP")] = "NDA"
GE$Party.Alliance.fourteen[which(GE$Party=="MDMK")] = "NDA"
GE$Party.Alliance.fourteen[which(GE$Party=="MNF")] = "NDA"
GE$Party.Alliance.fourteen[which(GE$Party=="NPF")] = "NDA"
GE$Party.Alliance.fourteen[which(GE$Party=="NPP")] = "NDA"
GE$Party.Alliance.fourteen[which(GE$Party=="PMK")] = "NDA"
GE$Party.Alliance.fourteen[which(GE$Party=="BLSP")] = "NDA"
GE$Party.Alliance.fourteen[which(GE$Party=="RPI(A)")] = "NDA"
GE$Party.Alliance.fourteen[which(GE$Party=="RSPK(B)")] = "NDA"
GE$Party.Alliance.fourteen[which(GE$Party=="SAD")] = "NDA"
GE$Party.Alliance.fourteen[which(GE$Party=="SHS")] = "NDA"
GE$Party.Alliance.fourteen[which(GE$Party=="SWP")] = "NDA"
GE$Party.Alliance.fourteen[which(GE$Party=="TDP")] = "NDA"
GE$Party.Alliance.fourteen[which(GE$Party=="NPEP")] = "NDA"
# missing Gorkha Janmukti Morcha,  Indhiya Jananayaga Katchi, Kerala Congress (Nationalist), Kongunadu Makkal Desia Katchi,
# Maharashtrawadi Gomantak Party, New Justice Party, Rashtriya Samaj Paksha,
unique(data.frame(GE$Party,GE$State,GE$Year)[GE$State=="telangana" & GE$Year==2014,])
table(as.character(unlist(GE$Party[GE$State=="meghalaya" & GE$Position==1 & GE$Year==2014])))
# now parties in the UPA
GE$Party.Alliance.fourteen[which(GE$Party=="INC")] = "UPA" 
GE$Party.Alliance.fourteen[which(GE$Party=="RJD")] = "UPA" 
GE$Party.Alliance.fourteen[which(GE$Party=="NCP")] = "UPA" 
GE$Party.Alliance.fourteen[which(GE$Party=="RLD")] = "UPA" 
GE$Party.Alliance.fourteen[which(GE$Party=="JMM")] = "UPA" 
GE$Party.Alliance.fourteen[which(GE$Party=="MD")] = "UPA" 
GE$Party.Alliance.fourteen[which(GE$Party=="IUML")] = "UPA" 
GE$Party.Alliance.fourteen[which(GE$Party=="KEC(M)")] = "UPA" 
GE$Party.Alliance.fourteen[which(GE$Party=="RSP")] = "UPA" 
GE$Party.Alliance.fourteen[which(GE$Party=="BOPF")] = "UPA" 
#GE$Party.Alliance.fourteen[which(GE$Party=="CPI")] = "UPA" 
# Jammu & Kashmir National Conference, Socialist Janata (Democratic)
unique(data.frame(GE$Party,GE$State,GE$Year)[GE$State=="telangana" & GE$Year==2014,])
table(as.character(unlist(GE$Party[ GE$State=="kerala" & GE$Position!=1 & GE$Year==2014])))
# remaining = OTHER 
GE$Party.Alliance.fourteen = ifelse(is.na(GE$Party.Alliance.fourteen),"OTHER",GE$Party.Alliance.fourteen)
# check 2014 results - discrepancy of 1 UPA is CPI which defected 
table(as.character(unlist(GE$Party.Alliance.fourteen[GE$Position==1 & GE$Year==2014 & !is.na(GE$month)])))
table(as.character(unlist(GE$Party[GE$Position==1 & GE$Year==2014 & !is.na(GE$month) & GE$Party.Alliance.fourteen=="UPA"])))
table(as.character(unlist(GE$Party[GE$Position==1 & GE$Year==2014 & !is.na(GE$month) & GE$Party.Alliance.fourteen=="NDA"])))
# vote share according to this were: 
c(
  sum(GE$Votes[GE$Party.Alliance.fourteen=="NDA" & GE$Year==2014 & !is.na(GE$month) ])/sum(sum(GE$Votes[GE$Party.Alliance.fourteen=="NDA" & GE$Year==2014 & !is.na(GE$month) ]),
                                                                                           sum(GE$Votes[GE$Party.Alliance.fourteen=="UPA" & GE$Year==2014 & !is.na(GE$month)]),
                                                                                           sum(GE$Votes[GE$Party.Alliance.fourteen=="OTHER" & GE$Year==2014 & !is.na(GE$month)])),
  sum(GE$Votes[GE$Party.Alliance.fourteen=="UPA" & GE$Year==2014 & !is.na(GE$month)])/sum(sum(GE$Votes[GE$Party.Alliance.fourteen=="NDA" & GE$Year==2014 & !is.na(GE$month) ]),
                                                                                          sum(GE$Votes[GE$Party.Alliance.fourteen=="UPA" & GE$Year==2014 & !is.na(GE$month)]),
                                                                                          sum(GE$Votes[GE$Party.Alliance.fourteen=="OTHER" & GE$Year==2014 & !is.na(GE$month)])),
  sum(GE$Votes[GE$Party.Alliance.fourteen=="OTHER" & GE$Year==2014 & !is.na(GE$month)])/sum(sum(GE$Votes[GE$Party.Alliance.fourteen=="NDA" & GE$Year==2014 & !is.na(GE$month) ]),
                                                                                            sum(GE$Votes[GE$Party.Alliance.fourteen=="UPA" & GE$Year==2014 & !is.na(GE$month)]),
                                                                                            sum(GE$Votes[GE$Party.Alliance.fourteen=="OTHER" & GE$Year==2014 & !is.na(GE$month)]))
)
# this is correct, which means the data is not faulty.
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # The following alliance members reflect onnly the current membership -
# hence aggregate results derived from this alliance may differ from official 
# ones with the members of the alliance at the time. 
# # # # # # # Now highlight members of the NDA: 

GE$Party.Alliance = NA
GE$Party.Alliance[which(GE$Party=="BJP")] = "NDA"
GE$Party.Alliance[which(GE$Party=="ADMK")] = "NDA"

#GE[which(GE$Party=="ADMK"),c("Year")]

GE$Party.Alliance[which(GE$Party=="SHS")] = "NDA"
GE$Party.Alliance[which(GE$Party=="JD(U)")] = "NDA"
GE$Party.Alliance[which(GE$Party=="LJP")] = "NDA"
GE$Party.Alliance[which(GE$Party=="SAD")] = "NDA"
GE$Party.Alliance[which(GE$Party=="AD")] = "NDA"
GE$Party.Alliance[which(GE$Party=="PMK")] = "NDA"
GE$Party.Alliance[which(GE$Party=="DMDK")] = "NDA"
GE$Party.Alliance[which(GE$Party=="RPI(A)")] = "NDA"
GE$Party.Alliance[which(GE$Party=="BOPF")] = "NDA"
GE$Party.Alliance[which(GE$Party=="AINRC")] = "NDA"
GE$Party.Alliance[which(GE$Party=="NPEP")] = "NDA"
GE$Party.Alliance[which(GE$Party=="MNF")] = "NDA"
GE$Party.Alliance[which(GE$Party=="MGP")] = "NDA"
GE$Party.Alliance[which(GE$Party=="AJSUP")] = "NDA"
GE$Party.Alliance[which(GE$Party=="IPFT")] = "NDA"
GE$Party.Alliance[which(GE$Party=="UDP")] = "NDA"
GE$Party.Alliance[which(GE$Party=="SBSP")] = "NDA"
GE$Party.Alliance[which(GE$Party=="AGP")] = "NDA"
GE$Party.Alliance[which(GE$Party=="SDF")] = "NDA"
GE$Party.Alliance[which(GE$Party=="SKM")] = "NDA"


# National Democratic Progressive Party,Shiv Sangram, Maharashtrawadi Gomantak Party,Goa Forward Party,Goa Vikas Party,Manipur Peoples Party,Kamtapur People's Party
# 	Hill State People's Democratic Party, Kerala Congress (Thomas), Bharath Dharma Jana Sena, kerala kamaraj congress, Praja Socialist Party, 	Democratic Labor Party (Kerala)
# Kerala Vikas Congress, # Pravasi Nivasi party, Kerala Congress (Nationalist), People's Democratic Front
# now parties in the UPA
GE$Party.Alliance[which(GE$Party=="INC")] = "UPA" #
GE$Party.Alliance[which(GE$Party=="NCP")] = "UPA" #
GE$Party.Alliance[which(GE$Party=="RJD")] = "UPA" #
GE$Party.Alliance[which(GE$Party=="IUML")] = "UPA" #
GE$Party.Alliance[which(GE$Party=="JMM")] = "UPA" #
GE$Party.Alliance[which(GE$Party=="JD(S)")] = "UPA" #
GE$Party.Alliance[which(GE$Party=="AD")] = "UPA" #
GE$Party.Alliance[which(GE$Party=="KEC(M)")] = "UPA" #
#GE$Party.Alliance[which(GE$Party=="RLD")] = "UPA" #
GE$Party.Alliance[which(GE$Party=="RSP")] = "UPA" #
GE$Party.Alliance[which(GE$Party=="SWP")] = "UPA" #
GE$Party.Alliance[which(GE$Party=="JVM")] = "UPA" #
GE$Party.Alliance[which(GE$Party=="MDMK")] = "UPA" #

GE$Party.Alliance[which(GE$Party=="DMK")] = "UPA"
GE$Party.Alliance[which(GE$Party=="PPI")] = "UPA" #
GE$Party.Alliance[which(GE$Party=="MD")] = "UPA" #
GE$Party.Alliance[which(GE$Party=="CPI")] = "UPA" #
GE$Party.Alliance[which(GE$Party=="CPIM")] = "UPA" #
GE$Party.Alliance[which(GE$Party=="VCK")] = "UPA" #
GE$Party.Alliance[which(GE$Party=="CPI(ML)(L)")] = "UPA" #
GE$Party.Alliance[which(GE$Party=="BVA")] = "UPA" #
GE$Party.Alliance[which(GE$Party=="VIP")] = "UPA" #
GE$Party.Alliance[which(GE$Party=="JAP")] = "UPA" #


# Members of the 'Grand Coalition' have to be merged together  so that when we take the 'challenger' for this election in uttar pradesh, it is the sum of their votes
GE$Party.Alliance[which(GE$Party=="BSP")] = "Mahagathbandhan"
GE$Party.Alliance[which(GE$Party=="SP")] = "Mahagathbandhan"
GE$Party.Alliance[which(GE$Party=="RLD")] = "Mahagathbandhan"

# Jammu & Kashmir National Conference, #¢Bharatiya Tribal Party, Hindustani Awam Morcha,Kerala Congress (Jacob),Kongunadu Makkal Desia Katchi,
# Karnataka Pragnyavantha Janatha Party, Telangana Jana Samithi, Loktantrik Janata Dal, Loktantrik Janata Dal, Indigenous Nationalist Party of Twipra
# Yuva Swabhiman Party
table(as.character(unlist(GE$Party[GE$Position!=1 & GE$Year==2014 & !is.na(GE$month) & GE$State=="maharashtra"])))
# remaining = OTHER 
GE$Party.Alliance = ifelse(is.na(GE$Party.Alliance),"OTHER",GE$Party.Alliance)
sum(table(GE$Party.Alliance[GE$Position==1 & GE$Year==2014 & !is.na(GE$month)]))
c(
  sum(GE$Votes[GE$Party.Alliance=="NDA" & GE$Year==2014 & !is.na(GE$month) ])/sum(sum(GE$Votes[GE$Party.Alliance=="NDA" & GE$Year==2014 & !is.na(GE$month) ]),
                                                                                  sum(GE$Votes[GE$Party.Alliance=="UPA" & GE$Year==2014 & !is.na(GE$month)]),
                                                                                  sum(GE$Votes[GE$Party.Alliance=="OTHER" & GE$Year==2014 & !is.na(GE$month)]),
                                                                                  sum(GE$Votes[GE$Party.Alliance=="Mahagathbandhan" & GE$Year==2014 & !is.na(GE$month)]    
                                                                                  )),
  sum(GE$Votes[GE$Party.Alliance=="UPA" & GE$Year==2014 & !is.na(GE$month)])/sum(sum(GE$Votes[GE$Party.Alliance=="NDA" & GE$Year==2014 & !is.na(GE$month) ]),
                                                                                 sum(GE$Votes[GE$Party.Alliance=="UPA" & GE$Year==2014 & !is.na(GE$month)]),
                                                                                 sum(GE$Votes[GE$Party.Alliance=="OTHER" & GE$Year==2014 & !is.na(GE$month)]),
                                                                                 sum(GE$Votes[GE$Party.Alliance=="Mahagathbandhan" & GE$Year==2014 & !is.na(GE$month)] 
                                                                                 )),
  sum(GE$Votes[GE$Party.Alliance=="Mahagathbandhan" & GE$Year==2014 & !is.na(GE$month)])/sum(sum(GE$Votes[GE$Party.Alliance=="NDA" & GE$Year==2014 & !is.na(GE$month) ]),
                                                                                             sum(GE$Votes[GE$Party.Alliance=="UPA" & GE$Year==2014 & !is.na(GE$month)]),
                                                                                             sum(GE$Votes[GE$Party.Alliance=="OTHER" & GE$Year==2014 & !is.na(GE$month)]),
                                                                                             sum(GE$Votes[GE$Party.Alliance=="Mahagathbandhan" & GE$Year==2014 & !is.na(GE$month)] 
                                                                                             )),
  
  sum(GE$Votes[GE$Party.Alliance=="OTHER" & GE$Year==2014 & !is.na(GE$month)])/sum(sum(GE$Votes[GE$Party.Alliance=="NDA" & GE$Year==2014 & !is.na(GE$month) ]),
                                                                                   sum(GE$Votes[GE$Party.Alliance=="UPA" & GE$Year==2014 & !is.na(GE$month)]),
                                                                                   sum(GE$Votes[GE$Party.Alliance=="OTHER" & GE$Year==2014 & !is.na(GE$month)]),
                                                                                   sum(GE$Votes[GE$Party.Alliance=="Mahagathbandhan" & GE$Year==2014 & !is.na(GE$month)]
                                                                                   )))

unique(GE[GE$Party.Alliance!=GE$Party.Alliance.fourteen,c("Party","Party.Alliance","Party.Alliance.fourteen")])
# both major parties poached 'Other' parties, with only a few notabvle switching between the two.


# only deal with years since 1989 
#GE = GE[which(GE$Year>=1989),]
# Derive national results by alliance for non by-election years 
GE_noby = GE[which(GE$Poll_No==0),]
# the pubjab polls of 1992 are to be counted with the 1991 election
GE_noby[which(GE_noby$Year == 1992),"Year"] = 1991

NAT_vote = 
  aggregate(data.frame(NAT_Votes = GE_noby$Votes),
            by = list(Year =GE_noby$Year,
                      Party.Alliance = GE_noby$Party.Alliance),
            FUN = function(x){sum(x)})
NAT_turnout = 
  aggregate(data.frame(NAT_Turnout = GE_noby$Votes),
            by = list(Year =GE_noby$Year),
            FUN = function(x){sum(x)})
NAT_GE = merge(NAT_vote,NAT_turnout,by = "Year",all=TRUE)
NAT_GE$NAT_Percentage.Votes = round(100*NAT_GE$NAT_Votes/NAT_GE$NAT_Turnout,1)

#png(filename = "ELECTION MODEL/NAT_VOTES.png",width = 500, height = 400,res = 100)
png(filename = "Plots/HIST_ALLIANCE_NAT_VOTES.png",width = 750, height = 750,res = 100)
plot(NAT_GE$Year,NAT_GE$NAT_Percentage.Votes,
     bty = "n",
     xlab = "year", xlim = c(1989,2019),
     ylab = "% vote", ylim = c(0,50),
     pch = NA,
     main = "India National Trend"
)
legend("topright",legend = c("UPA","NDA","OTHER","Mahagathbandhan"),col = c("blue","orange","darkgrey","brown"),lty = 1,bty = "n")
lines(NAT_GE$Year[which(NAT_GE$Party.Alliance=="NDA")],NAT_GE$NAT_Percentage.Votes[which(NAT_GE$Party.Alliance=="NDA")],col = "orange")
lines(NAT_GE$Year[which(NAT_GE$Party.Alliance=="UPA")],NAT_GE$NAT_Percentage.Votes[which(NAT_GE$Party.Alliance=="UPA")],col = "blue")
lines(NAT_GE$Year[which(NAT_GE$Party.Alliance=="OTHER")],NAT_GE$NAT_Percentage.Votes[which(NAT_GE$Party.Alliance=="OTHER")],col = "darkgrey")
lines(NAT_GE$Year[which(NAT_GE$Party.Alliance=="Mahagathbandhan")],NAT_GE$NAT_Percentage.Votes[which(NAT_GE$Party.Alliance=="Mahagathbandhan")],col = "brown")
dev.off()

#GE_noby[GE_noby$Year=="1962" &  GE_noby$State=="uttar pradesh" & GE_noby$PC_name=="muzaffarnagar" & GE_noby$Party.Alliance=="UPA",]


# Now derive state level results 
tmxp = unique(GE_noby[,c("Year","month","State","Electors","Valid_Votes")])
tmxp$State = ifelse(tmxp$State == "telangana","andhra pradesh",tmxp$State)
ST_turnout = aggregate(tmxp[,c("Electors","Valid_Votes")],
                       by = list(Year = tmxp$Year,
                                 State = tmxp$State),
                       FUN = sum)
ST_turnout$pct_T = (100*ST_turnout$Valid_Votes/ST_turnout$Electors)

tmxp = GE_noby
tmxp$Party.Alliance = ifelse(tmxp$Party.Alliance=="Mahagathbandhan","OTHER",tmxp$Party.Alliance)
tmxp$State = ifelse(tmxp$State == "telangana","andhra pradesh",tmxp$State)

ST_vote = 
  aggregate(data.frame(ST_Votes = tmxp$Votes),
            by = list(Year =tmxp$Year,
                      State = tmxp$State,
                      Party.Alliance = tmxp$Party.Alliance),
            FUN = function(x){sum(x)})

ST_GE = merge(ST_vote,ST_turnout,by = c("Year","State"),all=TRUE)

ST_GE$pct_V = (100*ST_GE$ST_Votes/ST_GE$Valid_Votes)
# remove turnout = 0 cases
ST_GE = ST_GE[ST_GE$pct_T!=0,]
ST_GE[ST_GE$Year==2014 & ST_GE$State=="uttar pradesh",]
# now at the end of the constituency-level clean up, merge with this and have state-level lags 

png(filename = "Plots//HIST_ALLIANCE_STATE_VOTES.png",width = 1000, height = 1000,res = 100)

par(mfrow = c(6,6), mar = c(2,2,2,1))
for(i in 1:length(unique(ST_GE$State))){
  plot(ST_GE$Year[which(ST_GE$State==unique(ST_GE$State)[1])],
       ST_GE$pct_V[which(ST_GE$State==unique(ST_GE$State)[1])],
       bty = "n",
       xlab = "year", xlim = c(1989,2019),
       ylab = "% vote", ylim = c(0,100),
       pch = NA,
       main = unique(ST_GE$State)[i]
  )
  #legend("topright",legend = c("UPA","NDA","OTHER"),col = c("blue","orange","darkgrey"),lty = 1,bty = "n")
  lines(ST_GE$Year[which(ST_GE$Party.Alliance=="NDA" & ST_GE$State==unique(ST_GE$State)[i])],
        ST_GE$pct_V[which(ST_GE$Party.Alliance=="NDA" & ST_GE$State==unique(ST_GE$State)[i])],
        col = "orange")
  lines(ST_GE$Year[which(ST_GE$Party.Alliance=="UPA" & ST_GE$State==unique(ST_GE$State)[i])],
        ST_GE$pct_V[which(ST_GE$Party.Alliance=="UPA" & ST_GE$State==unique(ST_GE$State)[i])],
        col = "blue")
  lines(ST_GE$Year[which(ST_GE$Party.Alliance=="OTHER" & ST_GE$State==unique(ST_GE$State)[i])],
        ST_GE$pct_V[which(ST_GE$Party.Alliance=="OTHER" & ST_GE$State==unique(ST_GE$State)[i])],
        col = "darkgrey")
  lines(ST_GE$Year[which(ST_GE$Party.Alliance=="Mahagathbandhan" & ST_GE$State==unique(ST_GE$State)[i])],
        ST_GE$pct_V[which(ST_GE$Party.Alliance=="Mahagathbandhan" & ST_GE$State==unique(ST_GE$State)[i])],
        col = "brown")
}
dev.off()

# stick Mahagathbandhan in 'other' category to keep in line with previous code

NAT_GE$Party.Alliance[which(NAT_GE$Party.Alliance=="Mahagathbandhan")] = "OTHER"

NAT_GE_temp = aggregate(data.frame(NAT_Votes = NAT_GE[,c("NAT_Votes")]),by = list(Year = NAT_GE$Year,Party.Alliance = NAT_GE$Party.Alliance),FUN = sum)

NAT_GE = unique(merge(NAT_GE_temp,NAT_GE[,c("Year","Party.Alliance","NAT_Turnout")],by=c("Year","Party.Alliance"),all=TRUE))
NAT_GE$NAT_Percentage.Votes = round(100*NAT_GE$NAT_Votes/NAT_GE$NAT_Turnout,1)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# ------------------------------------------------------------------
# Set up the data
# National Votes
V_NAT = reshape(NAT_GE[,-which(colnames(NAT_GE)=="NAT_Votes"|colnames(NAT_GE)=="NAT_Turnout")],timevar = "Party.Alliance",direction = "wide",idvar = "Year")
# order NDDA,UPA,OTHER
V_NAT = V_NAT[,c("Year","NAT_Percentage.Votes.NDA","NAT_Percentage.Votes.UPA","NAT_Percentage.Votes.OTHER")]
# State Votes 
V = reshape(ST_GE[,-which(colnames(ST_GE)=="ST_Votes"|colnames(ST_GE)=="Electors"|colnames(ST_GE)=="Valid_Votes"|colnames(ST_GE)=="pct_T")],
            timevar = "Party.Alliance",direction = "wide",idvar = c("Year","State"))
# order NDA,UPA,OTHER
V = V[,c("Year","State","pct_V.NDA","pct_V.UPA","pct_V.OTHER")]
# NAs are 0 votes 
for(j in 1:3){
  V[is.na(V[,j+2]),j+2] = 0
}

V_temp = 
  merge(expand.grid(Year = unique(V$Year), State = unique(V$State)),
        V,by = c("State","Year"), all = TRUE)
V_temp_long = melt(V_temp,id.vars = c("State","Year"))
V_temp_long$variable = gsub("pct_V.","",V_temp_long$variable)
table(V_temp_long$variable)
names(V_temp_long)[3] = "Party.Alliance"
V_temp_long = V_temp_long[order(V_temp_long$Year,V_temp_long$State,V_temp_long$Party.Alliance),]

#GE_win = GE_noby[GE_noby$Position==1,]
GE_win = GE_noby
# limit to constituencies available in 2014
GE_win = GE_win[paste(GE_win$State,GE_win$PC_name,sep=" | ") %in% unique(paste(GE_win$State,GE_win$PC_name,sep=" | ")[which(GE_win$Year==2014)]),]

N.y = length(unique(GE_win$Year))
N.s = length(unique(GE_win$State))
N.j = length(unique(GE_win$Party.Alliance))

GE_win = unique(GE_win)

N.c = max(as.integer(as.factor(paste(GE_win$State,GE_win$PC_name,sep=" | "))))

length(which(GE_win$Position==1 & GE_win$Year==2014 & GE_win$Party.Alliance=="NDA"))
length(which(GE_win$Position==1 & GE_win$Year==2014 & GE_win$Party.Alliance=="UPA"))
length(which(GE_win$Position==1 & GE_win$Year==2014 & GE_win$Party.Alliance=="OTHER"))
length(which(GE_win$Position==1 & GE_win$Year==2014 & GE_win$Party.Alliance=="Mahagathbandhan"))

GE_win = data.table(GE_win)

head(GE_win)
unique(GE_win$State)
# CONSIDER TELANGANA ANDRHA PRADESH FOR PURPOSES INCLUDING HISTORICAL VOTE INFO AT ZONE LEVEL 
GE_win$State[which(GE_win$State=="telangana")]="andhra pradesh"
GE_win$State = 
  ifelse(GE_win$State==tolower("andaman & nicobar islands"),"(35) Anadman/Nicobar 35",
         ifelse(GE_win$State==tolower("chattisgarh"),"(22) Chhattisgarh 22",
                ifelse(GE_win$State==tolower("jammu & kashmir"),"(01) Jammu & Kashmir 01",
                       ifelse(GE_win$State==tolower("madhya pradesh"),"(23) Madhya Pradesh 23",
                              ifelse(GE_win$State==tolower("dadra & nagar haveli"),"(26) Dadra+Nagar Haveli 26",
                                     ifelse(GE_win$State==tolower("gujrat"),"(24) Gujarat 24",
                                            ifelse(GE_win$State==tolower("andhra pradesh"),"(28) Andhra Pradesh 28",
                                                   ifelse(GE_win$State==tolower("daman & diu"),"(25) Daman & Diu 25",
                                                          ifelse(GE_win$State==tolower("lakshadweep"),"(31) Lakshadweep 31",
                                                                 ifelse(GE_win$State==tolower("Andra Pradesh"),"(28) Andhra Pradesh 28",
                                                                        ifelse(GE_win$State==tolower("Mizoram"),"(15) Mizoram 15",
                                                                               ifelse(GE_win$State==tolower("Andaman and Nicobar Islands"),"(35) Anadman/Nicobar 35",
                                                                                      ifelse(GE_win$State==tolower("Telangana"),"(28) Andhra Pradesh 28",
                                                                                             ifelse(GE_win$State==tolower("Arunachal Pradesh"),"(12) Arunachal Pradesh 12",
                                                                                                    ifelse(GE_win$State==tolower("Assam"),"(18) Assam 18",
                                                                                                           ifelse(GE_win$State==tolower("Bihar"),"(10) Bihar 10",
                                                                                                                  ifelse(GE_win$State==tolower("Chandigarh"),"(04) Chandigarh 04",
                                                                                                                         ifelse(GE_win$State==tolower("Chhattisgarh"),"(22) Chhattisgarh 22",
                                                                                                                                ifelse(GE_win$State==tolower("Delhi"),"(07) Delhi 07",
                                                                                                                                       ifelse(GE_win$State==tolower("Goa"),"(30) Goa 30",
                                                                                                                                              ifelse(GE_win$State==tolower("Gujarat"),"(24) Gujarat 24",
                                                                                                                                                     ifelse(GE_win$State==tolower("Haryana"),"(06) Haryana 06",
                                                                                                                                                            ifelse(GE_win$State==tolower("Himachal Pradesh"),"(02) Himachal Pradesh 02",
                                                                                                                                                                   ifelse(GE_win$State==tolower("Jammu and Kashmir"),"(01) Jammu & Kashmir 01",
                                                                                                                                                                          ifelse(GE_win$State==tolower("Jharkhand"),"(20) Jharkhand 20",
                                                                                                                                                                                 ifelse(GE_win$State==tolower("Karnataka"),"(29) Karnataka 29",
                                                                                                                                                                                        ifelse(GE_win$State==tolower("Kerala"),"(32) Kerala 32",
                                                                                                                                                                                               ifelse(GE_win$State==tolower("Madya Pradesh"),"(23) Madhya Pradesh 23",
                                                                                                                                                                                                      ifelse(GE_win$State==tolower("Maharashtra"),"(27) Maharashtra 27",
                                                                                                                                                                                                             ifelse(GE_win$State==tolower("Manipur"),"(14) Manipur 14",
                                                                                                                                                                                                                    ifelse(GE_win$State==tolower("Meghalaya"),"(17) Meghalaya 17",
                                                                                                                                                                                                                           ifelse(GE_win$State==tolower("Nagaland"),"(13) Nagaland 13",
                                                                                                                                                                                                                                  ifelse(GE_win$State==tolower("Orissa"),"(21) Orissa 21",
                                                                                                                                                                                                                                         ifelse(GE_win$State==tolower("Pondicherry"),"(34) Pondicherry 34",
                                                                                                                                                                                                                                                ifelse(GE_win$State==tolower("Punjab"),"(03) Punjab 03",
                                                                                                                                                                                                                                                       ifelse(GE_win$State==tolower("Rajasthan"),"(08) Rajasthan 08",
                                                                                                                                                                                                                                                              ifelse(GE_win$State==tolower("Sikkim"),"(11) Sikkim 11",
                                                                                                                                                                                                                                                                     ifelse(GE_win$State==tolower("Tamil Nadu"),"(33) Tamil Nadu 33",
                                                                                                                                                                                                                                                                            ifelse(GE_win$State==tolower("Tripura"),"(16) Tripura 16",
                                                                                                                                                                                                                                                                                   ifelse(GE_win$State==tolower("Uttar Pradesh"),"(09) Uttar Pradesh 09",
                                                                                                                                                                                                                                                                                          ifelse(GE_win$State==tolower("West Bengal"),"(19) West Bengal 19",
                                                                                                                                                                                                                                                                                                 ifelse(GE_win$State==tolower("Uttarakhand"),"(05) Uttarakhand 05",GE_win$State
                                                                                                                                                                                                                                                                                                 ))))))))))))))))))))))))))))))))))))))))))
GE_win$Zones = ifelse(GE_win$State=="(04) Chandigarh 04"|GE_win$State=="(06) Haryana 06"|GE_win$State=="(02) Himachal Pradesh 02"|GE_win$State=="(01) Jammu & Kashmir 01"|GE_win$State=="(03) Punjab 03"|GE_win$State=="(08) Rajasthan 08","North",
                      ifelse(GE_win$State=="(10) Bihar 10"|GE_win$State=="(07) Delhi 07"|GE_win$State== "(23) Madhya Pradesh 23"|GE_win$State=="(09) Uttar Pradesh 09"|GE_win$State=="(05) Uttarakhand 05","North-Central",
                             ifelse(GE_win$State=="(18) Assam 18"|GE_win$State=="(12) Arunachal Pradesh 12"|GE_win$State=="(14) Manipur 14"|GE_win$State=="(17) Meghalaya 17"|GE_win$State=="(15) Mizoram 15"|GE_win$State=="(13) Nagaland 13"|GE_win$State=="(16) Tripura 16","North-Eastern",
                                    ifelse(GE_win$State=="(35) Anadman/Nicobar 35"|GE_win$State=="(22) Chhattisgarh 22"|GE_win$State=="(20) Jharkhand 20"|GE_win$State=="(21) Orissa 21"|GE_win$State=="(11) Sikkim 11"|GE_win$State=="(19) West Bengal 19","Eastern",
                                           ifelse(GE_win$State=="(26) Dadra+Nagar Haveli 26"|GE_win$State=="(25) Daman & Diu 25"|GE_win$State=="(30) Goa 30"|GE_win$State=="(24) Gujarat 24"|GE_win$State=="(27) Maharashtra 27","Western",
                                                  ifelse(GE_win$State=="(28) Andhra Pradesh 28"|GE_win$State=="(29) Karnataka 29"|GE_win$State=="(32) Kerala 32"|GE_win$State=="(31) Lakshadweep 31"|GE_win$State=="(34) Pondicherry 34"|GE_win$State=="(33) Tamil Nadu 33"|
                                                           GE_win$State=="(33) Telangana 33","Southern",GE_win$State))))))



GE_win = data.table(GE_win)
######################
# do the same for state-level dataframe 

ST_GE$State[which(ST_GE$State=="telangana")]="andhra pradesh"
ST_GE$State = 
  ifelse(ST_GE$State==tolower("andaman & nicobar islands"),"(35) Anadman/Nicobar 35",
         ifelse(ST_GE$State==tolower("chattisgarh"),"(22) Chhattisgarh 22",
                ifelse(ST_GE$State==tolower("jammu & kashmir"),"(01) Jammu & Kashmir 01",
                       ifelse(ST_GE$State==tolower("madhya pradesh"),"(23) Madhya Pradesh 23",
                              ifelse(ST_GE$State==tolower("dadra & nagar haveli"),"(26) Dadra+Nagar Haveli 26",
                                     ifelse(ST_GE$State==tolower("gujrat"),"(24) Gujarat 24",
                                            ifelse(ST_GE$State==tolower("andhra pradesh"),"(28) Andhra Pradesh 28",
                                                   ifelse(ST_GE$State==tolower("daman & diu"),"(25) Daman & Diu 25",
                                                          ifelse(ST_GE$State==tolower("lakshadweep"),"(31) Lakshadweep 31",
                                                                 ifelse(ST_GE$State==tolower("Andra Pradesh"),"(28) Andhra Pradesh 28",
                                                                        ifelse(ST_GE$State==tolower("Mizoram"),"(15) Mizoram 15",
                                                                               ifelse(ST_GE$State==tolower("Andaman and Nicobar Islands"),"(35) Anadman/Nicobar 35",
                                                                                      ifelse(ST_GE$State==tolower("Telangana"),"(28) Andhra Pradesh 28",
                                                                                             ifelse(ST_GE$State==tolower("Arunachal Pradesh"),"(12) Arunachal Pradesh 12",
                                                                                                    ifelse(ST_GE$State==tolower("Assam"),"(18) Assam 18",
                                                                                                           ifelse(ST_GE$State==tolower("Bihar"),"(10) Bihar 10",
                                                                                                                  ifelse(ST_GE$State==tolower("Chandigarh"),"(04) Chandigarh 04",
                                                                                                                         ifelse(ST_GE$State==tolower("Chhattisgarh"),"(22) Chhattisgarh 22",
                                                                                                                                ifelse(ST_GE$State==tolower("Delhi"),"(07) Delhi 07",
                                                                                                                                       ifelse(ST_GE$State==tolower("Goa"),"(30) Goa 30",
                                                                                                                                              ifelse(ST_GE$State==tolower("Gujarat"),"(24) Gujarat 24",
                                                                                                                                                     ifelse(ST_GE$State==tolower("Haryana"),"(06) Haryana 06",
                                                                                                                                                            ifelse(ST_GE$State==tolower("Himachal Pradesh"),"(02) Himachal Pradesh 02",
                                                                                                                                                                   ifelse(ST_GE$State==tolower("Jammu and Kashmir"),"(01) Jammu & Kashmir 01",
                                                                                                                                                                          ifelse(ST_GE$State==tolower("Jharkhand"),"(20) Jharkhand 20",
                                                                                                                                                                                 ifelse(ST_GE$State==tolower("Karnataka"),"(29) Karnataka 29",
                                                                                                                                                                                        ifelse(ST_GE$State==tolower("Kerala"),"(32) Kerala 32",
                                                                                                                                                                                               ifelse(ST_GE$State==tolower("Madya Pradesh"),"(23) Madhya Pradesh 23",
                                                                                                                                                                                                      ifelse(ST_GE$State==tolower("Maharashtra"),"(27) Maharashtra 27",
                                                                                                                                                                                                             ifelse(ST_GE$State==tolower("Manipur"),"(14) Manipur 14",
                                                                                                                                                                                                                    ifelse(ST_GE$State==tolower("Meghalaya"),"(17) Meghalaya 17",
                                                                                                                                                                                                                           ifelse(ST_GE$State==tolower("Nagaland"),"(13) Nagaland 13",
                                                                                                                                                                                                                                  ifelse(ST_GE$State==tolower("Orissa"),"(21) Orissa 21",
                                                                                                                                                                                                                                         ifelse(ST_GE$State==tolower("Pondicherry"),"(34) Pondicherry 34",
                                                                                                                                                                                                                                                ifelse(ST_GE$State==tolower("Punjab"),"(03) Punjab 03",
                                                                                                                                                                                                                                                       ifelse(ST_GE$State==tolower("Rajasthan"),"(08) Rajasthan 08",
                                                                                                                                                                                                                                                              ifelse(ST_GE$State==tolower("Sikkim"),"(11) Sikkim 11",
                                                                                                                                                                                                                                                                     ifelse(ST_GE$State==tolower("Tamil Nadu"),"(33) Tamil Nadu 33",
                                                                                                                                                                                                                                                                            ifelse(ST_GE$State==tolower("Tripura"),"(16) Tripura 16",
                                                                                                                                                                                                                                                                                   ifelse(ST_GE$State==tolower("Uttar Pradesh"),"(09) Uttar Pradesh 09",
                                                                                                                                                                                                                                                                                          ifelse(ST_GE$State==tolower("West Bengal"),"(19) West Bengal 19",
                                                                                                                                                                                                                                                                                                 ifelse(ST_GE$State==tolower("Uttarakhand"),"(05) Uttarakhand 05",ST_GE$State
                                                                                                                                                                                                                                                                                                 ))))))))))))))))))))))))))))))))))))))))))
ST_GE$Zones = ifelse(ST_GE$State=="(04) Chandigarh 04"|ST_GE$State=="(06) Haryana 06"|ST_GE$State=="(02) Himachal Pradesh 02"|ST_GE$State=="(01) Jammu & Kashmir 01"|ST_GE$State=="(03) Punjab 03"|ST_GE$State=="(08) Rajasthan 08","North",
                      ifelse(ST_GE$State=="(10) Bihar 10"|ST_GE$State=="(07) Delhi 07"|ST_GE$State== "(23) Madhya Pradesh 23"|ST_GE$State=="(09) Uttar Pradesh 09"|ST_GE$State=="(05) Uttarakhand 05","North-Central",
                             ifelse(ST_GE$State=="(18) Assam 18"|ST_GE$State=="(12) Arunachal Pradesh 12"|ST_GE$State=="(14) Manipur 14"|ST_GE$State=="(17) Meghalaya 17"|ST_GE$State=="(15) Mizoram 15"|ST_GE$State=="(13) Nagaland 13"|ST_GE$State=="(16) Tripura 16","North-Eastern",
                                    ifelse(ST_GE$State=="(35) Anadman/Nicobar 35"|ST_GE$State=="(22) Chhattisgarh 22"|ST_GE$State=="(20) Jharkhand 20"|ST_GE$State=="(21) Orissa 21"|ST_GE$State=="(11) Sikkim 11"|ST_GE$State=="(19) West Bengal 19","Eastern",
                                           ifelse(ST_GE$State=="(26) Dadra+Nagar Haveli 26"|ST_GE$State=="(25) Daman & Diu 25"|ST_GE$State=="(30) Goa 30"|ST_GE$State=="(24) Gujarat 24"|ST_GE$State=="(27) Maharashtra 27","Western",
                                                  ifelse(ST_GE$State=="(28) Andhra Pradesh 28"|ST_GE$State=="(29) Karnataka 29"|ST_GE$State=="(32) Kerala 32"|ST_GE$State=="(31) Lakshadweep 31"|ST_GE$State=="(34) Pondicherry 34"|ST_GE$State=="(33) Tamil Nadu 33"|
                                                           ST_GE$State=="(33) Telangana 33","Southern",ST_GE$State))))))

#####################

GE_win[,sum_votes:=sum(Votes),by = c("State","PC_name","Year","month")]

# average over months too
GE_win_ST_T  = unique(GE_win[,c("State","Year","month","PC_name","Turnout_Percentage")])[,AV_ST_Turnout_Percentage:=mean(Turnout_Percentage,na.rm=TRUE),by = c("State","Year")]
names(GE_win_ST_T )[which(names(GE_win_ST_T )=="State")] ="State_Name"
GE_win_ST_T = unique(GE_win_ST_T[,c("State_Name","Year","AV_ST_Turnout_Percentage")])

# Here we sum over relevant coalitions - the idea being colaition members are not going to compete against each other in compatitive seats
GE_win$Party = ifelse(GE_win$Party=="BSP","Mahagathbandhan",as.character(unlist(GE_win$Party)))
GE_win$Party = ifelse(GE_win$Party=="SP","Mahagathbandhan",as.character(unlist(GE_win$Party)))
GE_win$Party = ifelse(GE_win$Party=="RLD","Mahagathbandhan",as.character(unlist(GE_win$Party)))

GE_win[,sum_votes:=sum(Votes),by = c("State","PC_name","Year","month")]
which(GE_win$sum_votes!=GE_win$Valid_Votes)
head(GE_win)
# sum mebers of Mahagathbandhan, make them 'other' and re-calculate percentages (they are fielding only 1 candidate)
# I'm losing 'IND' c andidates which get very little votes because they get similar amount of votes 
# need to rename inds with id so it's unique
GE_win$Party = ifelse(GE_win$Party=="IND",paste("IND",GE_win$Candidate,sep="-"),GE_win$Party)


GE_win[GE_win$Party.Alliance=="Mahagathbandhan",Votes:=sum(Votes),by = c("State","PC_name","Year","month","Party.Alliance","Party")]
GE_win = GE_win[!duplicated(GE_win[,
                                   c("State","Year","month","Assembly_No","Poll_No","DelimID","Constituency_No",
                                     "Valid_Votes","Electors","PC_name","Constituency_Type","Sub_Region","N_Cand",
                                     "ENOP","Zones","Party","Votes")
                                   
                                   ]),]
GE_win[GE_win$Party.Alliance=="Mahagathbandhan",] 
GE_win$Party.Alliance[GE_win$Party.Alliance=="Mahagathbandhan"] = "OTHER"
GE_win$Vote_Share_Percentage = round((GE_win$Votes/GE_win$Valid_Votes)*100,10)

GE_win[,sum_votes:=sum(Votes),by = c("State","PC_name","Year","month")]
GE_win[which(GE_win$sum_votes!=GE_win$Valid_Votes),]

head(GE_win)
# need to do this summing over for NDA and UPA as well in Tamil Nadu and elsewhere, but not for the 'Others'
library(data.table)
GE_win = data.table(GE_win)
GE_win = GE_win[GE_win$Party.Alliance=="UPA",Votes:=sum(Votes),by = c("State","PC_name","Year","month","Party.Alliance")]
GE_win = GE_win[!duplicated(GE_win[,
                                   c("State","Year","month","Assembly_No","Poll_No","DelimID","Constituency_No",
                                     "Valid_Votes","Electors","PC_name","Constituency_Type","Sub_Region","N_Cand",
                                     "ENOP","Zones","Party","Votes")
                                   
                                   ]),]
GE_win[GE_win$Party.Alliance=="UPA",] 
GE_win$Vote_Share_Percentage = round((GE_win$Votes/GE_win$Valid_Votes)*100,2)
GE_win[GE_win$State=="(33) Tamil Nadu 33" & GE_win$Year==2014,c("PC_name","Vote_Share_Percentage","Party","Party.Alliance")]

GE_win = GE_win[GE_win$Party.Alliance=="NDA",Votes:=sum(Votes),by = c("State","PC_name","Year","month","Party.Alliance")]
GE_win = GE_win[!duplicated(GE_win[,
                                   c("State","Year","month","Assembly_No","Poll_No","DelimID","Constituency_No",
                                     "Valid_Votes","Electors","PC_name","Constituency_Type","Sub_Region","N_Cand",
                                     "ENOP","Zones","Party","Votes")
                                   
                                   ]),]
GE_win[GE_win$Party.Alliance=="NDA",] 
GE_win$Vote_Share_Percentage = round((GE_win$Votes/GE_win$Valid_Votes)*100,2)
GE_win[GE_win$State=="(33) Tamil Nadu 33" & GE_win$Year==2014,c("PC_name","Vote_Share_Percentage","Party","Party.Alliance")]

# take top party from each alliance
GE_win_AL =  GE_win[,AL.Rank:= rank(-Vote_Share_Percentage,ties.method = "first"),by = c("State","PC_name","Year","Party.Alliance")]
GE_win_AL = GE_win_AL[GE_win_AL$AL.Rank==1,]
GE_win_AL = GE_win_AL[,c("Year","State","Zones","PC_name","Party.Alliance","Vote_Share_Percentage","Turnout_Percentage")]
GE_win_AL = reshape(GE_win_AL,idvar = c("State","Zones","Year","PC_name","Turnout_Percentage"),timevar = "Party.Alliance",direction = "wide")

# put a zero wher the party did not compete 
GE_win_AL = GE_win_AL[,Vote_Share_Percentage.NDA :=ifelse(is.na(Vote_Share_Percentage.NDA),0,Vote_Share_Percentage.NDA),]
GE_win_AL = GE_win_AL[,Vote_Share_Percentage.UPA :=ifelse(is.na(Vote_Share_Percentage.UPA),0,Vote_Share_Percentage.UPA),]
GE_win_AL = GE_win_AL[,Vote_Share_Percentage.OTHER :=ifelse(is.na(Vote_Share_Percentage.OTHER),0,Vote_Share_Percentage.OTHER),]

#  do these sum to 100? all do but a couple, which do not because thre is a discrepancy in the PC_names back in 1962; 
# for 2014, all seems fine, so we can safely ignore that discrepancy. 
GE_win_AL[rowSums(GE_win_AL[,c("Vote_Share_Percentage.NDA","Vote_Share_Percentage.UPA","Vote_Share_Percentage.OTHER")])>100.5]

# now derive quantities needed for the model:
# number of 2014/2019 constituencies
N.c = length(unique(paste(GE_win_AL$State,GE_win_AL$PC_name)))
# number of states
N.s = length(unique(GE_win_AL $State))
# number of years going back
N.y = length(unique(GE_win_AL$Year))

# PC identifier (some PCs have same name in different states so have to paste state name in )
ST_PC_id = as.integer(as.factor(paste(GE_win_AL $State,GE_win_AL $PC_name)))
# get state id
ST_id = as.integer(as.factor(paste(GE_win_AL $State)))
# get YR id
YR_id = as.integer(as.factor(paste(GE_win_AL $Year)))

# get historical vote for the three parties 
V_H = GE_win_AL[,c("Vote_Share_Percentage.NDA","Vote_Share_Percentage.UPA","Vote_Share_Percentage.OTHER")]

# Get National historical vote for the three parties 
NAT_GE = reshape(NAT_GE[,-which(names(NAT_GE)=="NAT_Turnout"|names(NAT_GE)=="NAT_Votes")],idvar = "Year",timevar = c("Party.Alliance"),direction = "wide")[,c("Year","NAT_Percentage.Votes.NDA","NAT_Percentage.Votes.UPA","NAT_Percentage.Votes.OTHER")]
NAT_V_H = NAT_GE[,-1]

# should be equal to N.y
dim(NAT_V_H)[1]==N.y


# NEXT CLEAN UP THE HISTORICAL DATA NEEDED TO CALCULATE THE UNIFORM SWING (get lags and deltas)
# lag function: 
lg <- function(x)c(NA, x[1:(length(x)-1)])

HIST = merge(GE_win_AL,NAT_GE,by = "Year",all=TRUE)
HIST [which(HIST $Year==2014),]
HIST [which(HIST $Year==2009),]

HIST = HIST[,LAG_Vote_Share_Percentage.NDA :=lg(Vote_Share_Percentage.NDA),by = c("Zones","State","PC_name")]
HIST = HIST[,LAG_Vote_Share_Percentage.UPA:=lg(Vote_Share_Percentage.UPA),by = c("Zones","State","PC_name")]
HIST = HIST[,LAG_Vote_Share_Percentage.OTHER:=lg(Vote_Share_Percentage.OTHER),by = c("Zones","State","PC_name")]

HIST = HIST[,LAG_Turnout_Percentage :=lg(Turnout_Percentage),by = c("Zones","State","PC_name")]

HIST = HIST[,LAG2_Vote_Share_Percentage.NDA :=lg(LAG_Vote_Share_Percentage.NDA),by = c("Zones","State","PC_name")]
HIST = HIST[,LAG2_Vote_Share_Percentage.UPA:=lg(LAG_Vote_Share_Percentage.UPA),by = c("Zones","State","PC_name")]
HIST = HIST[,LAG2_Vote_Share_Percentage.OTHER:=lg(LAG_Vote_Share_Percentage.OTHER),by = c("Zones","State","PC_name")]

HIST = HIST[,LAG2_Turnout_Percentage :=lg(LAG_Turnout_Percentage),by = c("Zones","State","PC_name")]

HIST = HIST[,LAG3_Vote_Share_Percentage.NDA :=lg(LAG2_Vote_Share_Percentage.NDA),by = c("Zones","State","PC_name")]
HIST = HIST[,LAG3_Vote_Share_Percentage.UPA:=lg(LAG2_Vote_Share_Percentage.UPA),by = c("Zones","State","PC_name")]
HIST = HIST[,LAG3_Vote_Share_Percentage.OTHER:=lg(LAG2_Vote_Share_Percentage.OTHER),by = c("Zones","State","PC_name")]

HIST = HIST[,LAG3_Turnout_Percentage :=lg(LAG2_Turnout_Percentage),by = c("Zones","State","PC_name")]

HIST = HIST[,LAG_NAT_Percentage.Votes.NDA:=lg(NAT_Percentage.Votes.NDA),by = c("Zones","State","PC_name")]
HIST = HIST[,LAG_NAT_Percentage.Votes.UPA:=lg(NAT_Percentage.Votes.UPA),by = c("Zones","State","PC_name")]
HIST = HIST[,LAG_NAT_Percentage.Votes.OTHER:=lg(NAT_Percentage.Votes.OTHER),by = c("Zones","State","PC_name")]

HIST = HIST[,LAG2_NAT_Percentage.Votes.NDA:=lg(LAG_NAT_Percentage.Votes.NDA),by = c("Zones","State","PC_name")]
HIST = HIST[,LAG2_NAT_Percentage.Votes.UPA:=lg(LAG_NAT_Percentage.Votes.UPA),by = c("Zones","State","PC_name")]
HIST = HIST[,LAG2_NAT_Percentage.Votes.OTHER:=lg(LAG_NAT_Percentage.Votes.OTHER),by = c("Zones","State","PC_name")]

HIST = HIST[,LAG3_NAT_Percentage.Votes.NDA:=lg(LAG2_NAT_Percentage.Votes.NDA),by = c("Zones","State","PC_name")]
HIST = HIST[,LAG3_NAT_Percentage.Votes.UPA:=lg(LAG2_NAT_Percentage.Votes.UPA),by = c("Zones","State","PC_name")]
HIST = HIST[,LAG3_NAT_Percentage.Votes.OTHER:=lg(LAG2_NAT_Percentage.Votes.OTHER),by = c("Zones","State","PC_name")]



# state level results  
ST_GE = ST_GE[,-which(names(ST_GE)=="Electors"|names(ST_GE)=="Valid_Votes"|names(ST_GE)=="ST_Votes")]
names(ST_GE)[which(names(ST_GE)=="pct_T")] = "AV_ST_Turnout_Percentage"
names(ST_GE)[which(names(ST_GE)=="pct_V")] = "AV_ST_Vote_Share_Percentage"
ST_GE = ST_GE[,-which(names(ST_GE)=="Zones")]

ST_GE = reshape(ST_GE,idvar = c("Year","State","AV_ST_Turnout_Percentage"),
        timevar = "Party.Alliance",direction = 'wide'
        )
ST_GE = as.data.table(ST_GE)

ST_GE = ST_GE[,AV_LAG_ST_Vote_Share_Percentage.NDA :=lg(AV_ST_Vote_Share_Percentage.NDA),by = c("State")]
ST_GE = ST_GE[,AV_LAG_ST_Vote_Share_Percentage.UPA:=lg(AV_ST_Vote_Share_Percentage.UPA),by = c("State")]
ST_GE = ST_GE[,AV_LAG_ST_Vote_Share_Percentage.OTHER:=lg(AV_ST_Vote_Share_Percentage.OTHER),by = c("State")]

ST_GE = ST_GE[,AV_LAG_ST_Turnout_Percentage :=lg(AV_ST_Turnout_Percentage),by = c("State")]

ST_GE = ST_GE[,AV_LAG2_ST_Vote_Share_Percentage.NDA :=lg(AV_LAG_ST_Vote_Share_Percentage.NDA),by = c("State")]
ST_GE = ST_GE[,AV_LAG2_ST_Vote_Share_Percentage.UPA:=lg(AV_LAG_ST_Vote_Share_Percentage.UPA),by = c("State")]
ST_GE = ST_GE[,AV_LAG2_ST_Vote_Share_Percentage.OTHER:=lg(AV_LAG_ST_Vote_Share_Percentage.OTHER),by = c("State")]

ST_GE = ST_GE[,AV_LAG2_ST_Turnout_Percentage :=lg(AV_LAG_ST_Turnout_Percentage),by = c("State")]

ST_GE = ST_GE[,AV_LAG3_ST_Vote_Share_Percentage.NDA :=lg(AV_LAG2_ST_Vote_Share_Percentage.NDA),by = c("State")]
ST_GE = ST_GE[,AV_LAG3_ST_Vote_Share_Percentage.UPA:=lg(AV_LAG2_ST_Vote_Share_Percentage.UPA),by = c("State")]
ST_GE = ST_GE[,AV_LAG3_ST_Vote_Share_Percentage.OTHER:=lg(AV_LAG2_ST_Vote_Share_Percentage.OTHER),by = c("State")]

ST_GE = ST_GE[,AV_LAG3_ST_Turnout_Percentage :=lg(AV_LAG2_ST_Turnout_Percentage),by = c("State")]



temp = merge(as.data.frame(HIST),ST_GE,by = c("Year","State"),all=TRUE)
HIST = temp[-which(is.na(temp$PC_name)),]

HIST = as.data.table(HIST)

# HIST zone level
# we calculate zone level turnout and vote share lags.
GE_zones = GE_win
# zone turnout: 
keep =c("Zones","State","PC_name","Year","Valid_Votes","Electors") # unique instances to calculate unique turnout numbers per constituency, then sum up over zones
GE_zones_T = unique(GE_zones[,..keep])
GE_zones_T[,Valid_Votes:= sum(Valid_Votes),by = c("Zones","Year")]
GE_zones_T[,Electors:= sum(Electors),by = c("Zones","Year")]
GE_zones_T$Zone_T = GE_zones_T$Valid_Votes/GE_zones_T$Electors
keep = c("Zones","Year","Zone_T","Valid_Votes","Electors")
GE_zones_T = unique(GE_zones_T[,..keep])

# zone vote shares party alliances
GE_zones_V = GE_win
# zone turnout: 
keep =c("Zones","State","PC_name","Year","Votes","Party.Alliance") # unique instances to calculate unique turnout numbers per constituency, then sum up over zones
GE_zones_V = unique(GE_zones_V[,..keep])
GE_zones_V = unique(GE_zones_V [,Votes := sum(Votes),by = c("Zones","Year","Party.Alliance")])
keep = c("Zones","Year","Votes","Party.Alliance")
GE_zones_V = unique(GE_zones_V[,..keep])

GE_zones = merge(GE_zones_V,GE_zones_T,by = c("Zones","Year"),all=TRUE)
GE_zones$Zone_V = GE_zones$Votes/GE_zones$Valid_Votes

keep = c("Zones","Year","Party.Alliance","Zone_T","Zone_V")
HIST_zones = GE_zones[,..keep]

names(HIST_zones)[which(names(HIST_zones)=="Zone_T")] = "Z_Turnout_Percentage"
names(HIST_zones)[which(names(HIST_zones)=="Zone_V")] = "Z_Vote_Share_Percentage"

HIST_zones = reshape(HIST_zones,idvar = c("Year","Zones","Z_Turnout_Percentage"),
                     timevar = "Party.Alliance",direction = 'wide'
                     )


HIST_zones = HIST_zones[,LAG_Z_Vote_Share_Percentage.NDA :=lg(Z_Vote_Share_Percentage.NDA),by = c("Zones")]
HIST_zones = HIST_zones[,LAG_Z_Vote_Share_Percentage.UPA:=lg(Z_Vote_Share_Percentage.UPA),by = c("Zones")]
HIST_zones = HIST_zones[,LAG_Z_Vote_Share_Percentage.OTHER:=lg(Z_Vote_Share_Percentage.OTHER),by = c("Zones")]

HIST_zones = HIST_zones[,LAG_Z_Turnout_Percentage :=lg(Z_Turnout_Percentage),by = c("Zones")]

HIST_zones = HIST_zones[,LAG2_Z_Vote_Share_Percentage.NDA :=lg(LAG_Z_Vote_Share_Percentage.NDA),by = c("Zones")]
HIST_zones = HIST_zones[,LAG2_Z_Vote_Share_Percentage.UPA:=lg(LAG_Z_Vote_Share_Percentage.UPA),by = c("Zones")]
HIST_zones = HIST_zones[,LAG2_Z_Vote_Share_Percentage.OTHER:=lg(LAG_Z_Vote_Share_Percentage.OTHER),by = c("Zones")]

HIST_zones = HIST_zones[,LAG2_Z_Turnout_Percentage :=lg(LAG_Z_Turnout_Percentage),by = c("Zones")]

HIST_zones = HIST_zones[,LAG3_Z_Vote_Share_Percentage.NDA :=lg(LAG2_Z_Vote_Share_Percentage.NDA),by = c("Zones")]
HIST_zones = HIST_zones[,LAG3_Z_Vote_Share_Percentage.UPA:=lg(LAG2_Z_Vote_Share_Percentage.UPA),by = c("Zones")]
HIST_zones = HIST_zones[,LAG3_Z_Vote_Share_Percentage.OTHER:=lg(LAG2_Z_Vote_Share_Percentage.OTHER),by = c("Zones")]

HIST_zones = HIST_zones[,LAG3_Z_Turnout_Percentage :=lg(LAG2_Z_Turnout_Percentage),by = c("Zones")]



# number of observations per constituency
hist(table(HIST$PC_name))

HIST[which(HIST$Year=="2014"),][which(!complete.cases(HIST[which(HIST$Year=="2014"),])),]

fwrite(HIST,"Generated Quantities/HIST.csv",row.names=FALSE)
fwrite(HIST_zones,"Generated Quantities/HIST_Zones.csv",row.names=FALSE)

