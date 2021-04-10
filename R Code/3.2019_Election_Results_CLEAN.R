rm(list=ls())
options(scipen=999)
setwd("~/Desktop/India 2019/")
library(data.table)
library(xtable)
# analyize new results
res_2019 = read.csv('Election Data/India_2019_Results.csv')[,-1]
# first harmonize with historical results dataset
HIST = read.csv("Generated Quantities/HIST.csv")
# harmonize states 
levels(res_2019$state) = c("(35) Anadman/Nicobar 35",
                           "(28) Andhra Pradesh 28",
                           "(12) Arunachal Pradesh 12",
                           "(18) Assam 18",
                           "(10) Bihar 10",
                           "(04) Chandigarh 04",
                           
                           "(22) Chhattisgarh 22",
                           "(26) Dadra+Nagar Haveli 26",
                           "(25) Daman & Diu 25",
                           "(30) Goa 30",
                           "(24) Gujarat 24",
                           "(06) Haryana 06",
                           
                           "(02) Himachal Pradesh 02",
                           "(01) Jammu & Kashmir 01",
                           "(20) Jharkhand 20",
                           "(29) Karnataka 29",
                           "(32) Kerala 32" ,
                           "(31) Lakshadweep 31",
                           
                           "(23) Madhya Pradesh 23",
                           "(27) Maharashtra 27",
                           "(14) Manipur 14",
                           "(17) Meghalaya 17",
                           "(15) Mizoram 15",
                           "(13) Nagaland 13",
                           
                           "(07) Delhi 07",
                           "(21) Orissa 21",
                           "(34) Pondicherry 34",
                           "(03) Punjab 03",
                           "(08) Rajasthan 08",
                           "(11) Sikkim 11",
                           
                           "(33) Tamil Nadu 33",
                           "(28) Andhra Pradesh 28",
                           "(16) Tripura 16",
                           "(05) Uttarakhand 05",
                           "(09) Uttar Pradesh 09",
                           "(19) West Bengal 19"
)
# harmonize PC_name
res_2019$PC_name = tolower(as.character(unlist(res_2019$PC_name)))
res_2019$PC_name[which(res_2019$state == "(35) Anadman/Nicobar 35")]

#(35) Anadman/Nicobar 35 PCs
res_2019$PC_name[which(res_2019$state == "(35) Anadman/Nicobar 35" )]

# all clear for this state? - any non-congruencies between the two datasets? 
res_2019$PC_name[which(res_2019$state == "(35) Anadman/Nicobar 35")][which(is.na(
  match(res_2019$PC_name[which(res_2019$state == "(35) Anadman/Nicobar 35")],
        unique(HIST$PC_name[which(HIST$State=="(35) Anadman/Nicobar 35")])
  )))]
sort(unique(HIST$PC_name[which(HIST$State=="(35) Anadman/Nicobar 35")]))

# (28) Andhra Pradesh 28 PCs
res_2019$PC_name[which(res_2019$state == "(28) Andhra Pradesh 28" & res_2019$PC_name=="hyderabad" )]="hydrabad"
res_2019$PC_name[which(res_2019$state == "(28) Andhra Pradesh 28" & res_2019$PC_name=="secundrabad" )]="secunderabad"
res_2019$PC_name[which(res_2019$state == "(28) Andhra Pradesh 28" & res_2019$PC_name=="anantapur" )]="anantpur"

# all clear for this state? - any non-congruencies between the two datasets? 
res_2019$PC_name[which(res_2019$state == "(28) Andhra Pradesh 28")][which(is.na(
  match(res_2019$PC_name[which(res_2019$state == "(28) Andhra Pradesh 28")],
        unique(HIST$PC_name[which(HIST$State=="(28) Andhra Pradesh 28")])
  )))]
sort(unique(HIST$PC_name[which(HIST$State=="(28) Andhra Pradesh 28")]))

# (12) Arunachal Pradesh 12 PCs
res_2019$PC_name[which(res_2019$state == "(12) Arunachal Pradesh 12" )]
# all clear for this state? - any non-congruencies between the two datasets? 
res_2019$PC_name[which(res_2019$state == "(12) Arunachal Pradesh 12")][which(is.na(
  match(res_2019$PC_name[which(res_2019$state == "(12) Arunachal Pradesh 12")],
        unique(HIST$PC_name[which(HIST$State=="(12) Arunachal Pradesh 12")])
  )))]
sort(unique(HIST$PC_name[which(HIST$State=="(12) Arunachal Pradesh 12")]))


# (18) Assam 18 PCs
res_2019$PC_name[which(res_2019$state == "(18) Assam 18" & res_2019$PC_name=="autonomous district")] = "autonomous districts"
# all clear for this state? - any non-congruencies between the two datasets? 
res_2019$PC_name[which(res_2019$state == "(18) Assam 18")][which(is.na(
  match(res_2019$PC_name[which(res_2019$state == "(18) Assam 18")],
        unique(HIST$PC_name[which(HIST$State=="(18) Assam 18")])
  )))]
sort(unique(HIST$PC_name[which(HIST$State=="(18) Assam 18")]))

# (10) Bihar 10 PCs
res_2019$PC_name[which(res_2019$state == "(10) Bihar 10" & res_2019$PC_name=="jamui (sc)")] ="jamui"
res_2019$PC_name[which(res_2019$state == "(10) Bihar 10" & res_2019$PC_name=="gaya (sc)")] ="gaya"
res_2019$PC_name[which(res_2019$state == "(10) Bihar 10" & res_2019$PC_name=="sasaram (sc)")] = "sasaram"
res_2019$PC_name[which(res_2019$state == "(10) Bihar 10" & res_2019$PC_name=="samastipur (sc)")] ="samastipur"
res_2019$PC_name[which(res_2019$state == "(10) Bihar 10" & res_2019$PC_name=="hajipur (sc)")] = "hajipur"
res_2019$PC_name[which(res_2019$state == "(10) Bihar 10" & res_2019$PC_name=="gopalganj (sc)")] = "gopalganj"

# all clear for this state? - any non-congruencies between the two datasets? 
res_2019$PC_name[which(res_2019$state == "(10) Bihar 10")][which(is.na(
  match(res_2019$PC_name[which(res_2019$state == "(10) Bihar 10")],
        unique(HIST$PC_name[which(HIST$State=="(10) Bihar 10")])
  )))]
sort(unique(HIST$PC_name[which(HIST$State=="(10) Bihar 10")]))

# (04) Chandigarh 04 PCs
res_2019$PC_name[which(res_2019$state == "(04) Chandigarh 04" )] 
# all clear for this state? - any non-congruencies between the two datasets? 
res_2019$PC_name[which(res_2019$state == "(04) Chandigarh 04")][which(is.na(
  match(res_2019$PC_name[which(res_2019$state == "(04) Chandigarh 04")],
        unique(HIST$PC_name[which(HIST$State=="(04) Chandigarh 04")])
  )))]
sort(unique(HIST$PC_name[which(HIST$State=="(04) Chandigarh 04")]))

# (22) Chhattisgarh 22 PCs
res_2019$PC_name[which(res_2019$state == "(22) Chhattisgarh 22" & res_2019$PC_name=="champa")] = "janjgir"
# all clear for this state? - any non-congruencies between the two datasets? 
res_2019$PC_name[which(res_2019$state == "(22) Chhattisgarh 22")][which(is.na(
  match(res_2019$PC_name[which(res_2019$state == "(22) Chhattisgarh 22")],
        unique(HIST$PC_name[which(HIST$State=="(22) Chhattisgarh 22")])
  )))]
sort(unique(HIST$PC_name[which(HIST$State=="(22) Chhattisgarh 22")]))

# (26) Dadra+Nagar Haveli 26 PCs
res_2019$PC_name[which(res_2019$state == "(26) Dadra+Nagar Haveli 26")] = "dadra & nagar haveli"
# all clear for this state? - any non-congruencies between the two datasets? 
res_2019$PC_name[which(res_2019$state == "(26) Dadra+Nagar Haveli 26")][which(is.na(
  match(res_2019$PC_name[which(res_2019$state == "(26) Dadra+Nagar Haveli 26")],
        unique(HIST$PC_name[which(HIST$State=="(26) Dadra+Nagar Haveli 26")])
  )))]
sort(unique(HIST$PC_name[which(HIST$State=="(26) Dadra+Nagar Haveli 26")]))

# (25) Daman & Diu 25 PCs
res_2019$PC_name[which(res_2019$state == "(25) Daman & Diu 25")] 
# all clear for this state? - any non-congruencies between the two datasets? 
res_2019$PC_name[which(res_2019$state == "(25) Daman & Diu 25")][which(is.na(
  match(res_2019$PC_name[which(res_2019$state == "(25) Daman & Diu 25")],
        unique(HIST$PC_name[which(HIST$State=="(25) Daman & Diu 25")])
  )))]
sort(unique(HIST$PC_name[which(HIST$State=="(25) Daman & Diu 25")]))

# (30) Goa 30 PCs
res_2019$PC_name[which(res_2019$state == "(30) Goa 30")]
# all clear for this state? - any non-congruencies between the two datasets? 
res_2019$PC_name[which(res_2019$state == "(30) Goa 30")][which(is.na(
  match(res_2019$PC_name[which(res_2019$state == "(30) Goa 30")],
        unique(HIST$PC_name[which(HIST$State=="(30) Goa 30")])
  )))]
sort(unique(HIST$PC_name[which(HIST$State=="(30) Goa 30")]))


# (24) Gujarat 24 PCs
res_2019$PC_name[which(res_2019$state == "(24) Gujarat 24")]
# all clear for this state? - any non-congruencies between the two datasets? 
res_2019$PC_name[which(res_2019$state == "(24) Gujarat 24")][which(is.na(
  match(res_2019$PC_name[which(res_2019$state == "(24) Gujarat 24")],
        unique(HIST$PC_name[which(HIST$State=="(24) Gujarat 24")])
  )))]
sort(unique(HIST$PC_name[which(HIST$State=="(24) Gujarat 24")]))

# (06) Haryana 06 PCs
res_2019$PC_name[which(res_2019$state == "(06) Haryana 06" & res_2019$PC_name=="mahendragarh")] = "bhiwani-mahendragarh"
res_2019$PC_name[which(res_2019$state == "(06) Haryana 06" & res_2019$PC_name=="hisar")] = "hissar"

# all clear for this state? - any non-congruencies between the two datasets? 
res_2019$PC_name[which(res_2019$state == "(06) Haryana 06")][which(is.na(
  match(res_2019$PC_name[which(res_2019$state == "(06) Haryana 06")],
        unique(HIST$PC_name[which(HIST$State=="(06) Haryana 06")])
  )))]
sort(unique(HIST$PC_name[which(HIST$State=="(06) Haryana 06")]))


# (02) Himachal Pradesh 02 PCs
res_2019$PC_name[which(res_2019$state == "(02) Himachal Pradesh 02")]
# all clear for this state? - any non-congruencies between the two datasets? 
res_2019$PC_name[which(res_2019$state == "(02) Himachal Pradesh 02")][which(is.na(
  match(res_2019$PC_name[which(res_2019$state == "(02) Himachal Pradesh 02")],
        unique(HIST$PC_name[which(HIST$State=="(02) Himachal Pradesh 02")])
  )))]
sort(unique(HIST$PC_name[which(HIST$State=="(02) Himachal Pradesh 02")]))

# (01) Jammu & Kashmir 01 PCs
res_2019$PC_name[which(res_2019$state == "(01) Jammu & Kashmir 01")]
# all clear for this state? - any non-congruencies between the two datasets? 
res_2019$PC_name[which(res_2019$state == "(01) Jammu & Kashmir 01")][which(is.na(
  match(res_2019$PC_name[which(res_2019$state == "(01) Jammu & Kashmir 01")],
        unique(HIST$PC_name[which(HIST$State=="(01) Jammu & Kashmir 01")])
  )))]
sort(unique(HIST$PC_name[which(HIST$State=="(01) Jammu & Kashmir 01")]))

# (20) Jharkhand 20 PCs
res_2019$PC_name[which(res_2019$state == "(20) Jharkhand 20")]
# all clear for this state? - any non-congruencies between the two datasets? 
res_2019$PC_name[which(res_2019$state == "(20) Jharkhand 20")][which(is.na(
  match(res_2019$PC_name[which(res_2019$state == "(20) Jharkhand 20")],
        unique(HIST$PC_name[which(HIST$State=="(20) Jharkhand 20")])
  )))]
sort(unique(HIST$PC_name[which(HIST$State=="(20) Jharkhand 20")]))

# (29) Karnataka 29 PCs
res_2019$PC_name[which(res_2019$state == "(29) Karnataka 29"  & res_2019$PC_name=="chikkballapur")] = "chikballapur"
res_2019$PC_name[which(res_2019$state == "(29) Karnataka 29"  & res_2019$PC_name=="belgaum")] = "belagavi"

# all clear for this state? - any non-congruencies between the two datasets? 
res_2019$PC_name[which(res_2019$state == "(29) Karnataka 29")][which(is.na(
  match(res_2019$PC_name[which(res_2019$state == "(29) Karnataka 29")],
        unique(HIST$PC_name[which(HIST$State=="(29) Karnataka 29")])
  )))]
sort(unique(HIST$PC_name[which(HIST$State=="(29) Karnataka 29")]))

# (32) Kerala 32 PCs
res_2019$PC_name[which(res_2019$state == "(32) Kerala 32"  & res_2019$PC_name=="mavelikkara")] = "mavelikara"
# all clear for this state? - any non-congruencies between the two datasets? 
res_2019$PC_name[which(res_2019$state == "(32) Kerala 32")][which(is.na(
  match(res_2019$PC_name[which(res_2019$state == "(32) Kerala 32")],
        unique(HIST$PC_name[which(HIST$State=="(32) Kerala 32")])
  )))]
sort(unique(HIST$PC_name[which(HIST$State=="(32) Kerala 32")]))

# (31) Lakshadweep 31 PCs
res_2019$PC_name[which(res_2019$state == "(31) Lakshadweep 31")]
# all clear for this state? - any non-congruencies between the two datasets? 
res_2019$PC_name[which(res_2019$state == "(31) Lakshadweep 31")][which(is.na(
  match(res_2019$PC_name[which(res_2019$state == "(31) Lakshadweep 31")],
        unique(HIST$PC_name[which(HIST$State=="(31) Lakshadweep 31")])
  )))]
sort(unique(HIST$PC_name[which(HIST$State=="(31) Lakshadweep 31")]))

# (23) Madhya Pradesh  23 PCs
res_2019$PC_name[which(res_2019$state == "(23) Madhya Pradesh 23"  & res_2019$PC_name=="mandsour")] = "mandsaur"
# all clear for this state? - any non-congruencies between the two datasets? 
res_2019$PC_name[which(res_2019$state == "(23) Madhya Pradesh 23")][which(is.na(
  match(res_2019$PC_name[which(res_2019$state == "(23) Madhya Pradesh 23")],
        unique(HIST$PC_name[which(HIST$State=="(23) Madhya Pradesh 23")])
  )))]
sort(unique(HIST$PC_name[which(HIST$State=="(23) Madhya Pradesh 23")]))

# (27) Maharashtra 27 PCs
res_2019$PC_name[which(res_2019$state == "(27) Maharashtra 27" & res_2019$PC_name==" sindhudurg" )] = "ratnagiri-sindhudurg"
res_2019$PC_name[which(res_2019$state == "(27) Maharashtra 27" & res_2019$PC_name=="ahmadnagar" )] = "ahmednagar"
res_2019$PC_name[which(res_2019$state == "(27) Maharashtra 27" & res_2019$PC_name=="mumbai   south" )] = "mumbai south"
res_2019$PC_name[which(res_2019$state == "(27) Maharashtra 27" & res_2019$PC_name=="washim" )] = "yavatmal-washim"
res_2019$PC_name[which(res_2019$state == "(27) Maharashtra 27" & res_2019$PC_name=="chimur" )] = "gadchiroli-chimur"
res_2019$PC_name[which(res_2019$state == "(27) Maharashtra 27" & res_2019$PC_name==" gondiya" )] = "bhandara-gondiya"
res_2019$PC_name[which(res_2019$state == "(27) Maharashtra 27" & res_2019$PC_name=="buldhana" )] = "buldana"

# all clear for this state? - any non-congruencies between the two datasets? 
res_2019$PC_name[which(res_2019$state == "(27) Maharashtra 27")][which(is.na(
  match(res_2019$PC_name[which(res_2019$state == "(27) Maharashtra 27")],
        unique(HIST$PC_name[which(HIST$State=="(27) Maharashtra 27")])
  )))]
sort(unique(HIST$PC_name[which(HIST$State=="(27) Maharashtra 27")]))

# (14) Manipur 14 PCs
res_2019$PC_name[which(res_2019$state == "(14) Manipur 14" )] 
# all clear for this state? - any non-congruencies between the two datasets? 
res_2019$PC_name[which(res_2019$state == "(14) Manipur 14")][which(is.na(
  match(res_2019$PC_name[which(res_2019$state == "(14) Manipur 14")],
        unique(HIST$PC_name[which(HIST$State=="(14) Manipur 14")])
  )))]
sort(unique(HIST$PC_name[which(HIST$State=="(14) Manipur 14")]))

# (17) Meghalaya 17 PCs
res_2019$PC_name[which(res_2019$state == "(17) Meghalaya 17" )] 
# all clear for this state? - any non-congruencies between the two datasets? 
res_2019$PC_name[which(res_2019$state == "(17) Meghalaya 17")][which(is.na(
  match(res_2019$PC_name[which(res_2019$state == "(17) Meghalaya 17")],
        unique(HIST$PC_name[which(HIST$State=="(17) Meghalaya 17")])
  )))]
sort(unique(HIST$PC_name[which(HIST$State=="(17) Meghalaya 17")]))

# (15) Mizoram 15 PCs
res_2019$PC_name[which(res_2019$state == "(15) Mizoram 15" )] 
# all clear for this state? - any non-congruencies between the two datasets? 
res_2019$PC_name[which(res_2019$state == "(15) Mizoram 15")][which(is.na(
  match(res_2019$PC_name[which(res_2019$state == "(15) Mizoram 15")],
        unique(HIST$PC_name[which(HIST$State=="(15) Mizoram 15")])
  )))]
sort(unique(HIST$PC_name[which(HIST$State=="(15) Mizoram 15")]))

# (13) Nagaland 13 PCs
res_2019$PC_name[which(res_2019$state == "(13) Nagaland 13" )] 
# all clear for this state? - any non-congruencies between the two datasets? 
res_2019$PC_name[which(res_2019$state == "(13) Nagaland 13")][which(is.na(
  match(res_2019$PC_name[which(res_2019$state == "(13) Nagaland 13")],
        unique(HIST$PC_name[which(HIST$State=="(13) Nagaland 13")])
  )))]
sort(unique(HIST$PC_name[which(HIST$State=="(13) Nagaland 13")]))

# (07) Delhi 07 PCs
res_2019$PC_name[which(res_2019$state == "(07) Delhi 07" )] 
# all clear for this state? - any non-congruencies between the two datasets? 
res_2019$PC_name[which(res_2019$state == "(07) Delhi 07")][which(is.na(
  match(res_2019$PC_name[which(res_2019$state == "(07) Delhi 07")],
        unique(HIST$PC_name[which(HIST$State=="(07) Delhi 07")])
  )))]
sort(unique(HIST$PC_name[which(HIST$State=="(07) Delhi 07")]))

# (21) Orissa 21 PCs
res_2019$PC_name[which(res_2019$state == "(21) Orissa 21" & res_2019$PC_name=="berhampur" )] = "baharampur"
# all clear for this state? - any non-congruencies between the two datasets? 
res_2019$PC_name[which(res_2019$state == "(21) Orissa 21")][which(is.na(
  match(res_2019$PC_name[which(res_2019$state == "(21) Orissa 21")],
        unique(HIST$PC_name[which(HIST$State=="(21) Orissa 21")])
  )))]
sort(unique(HIST$PC_name[which(HIST$State=="(21) Orissa 21")]))

# (34) Pondicherry 34 PCs
res_2019$PC_name[which(res_2019$state == "(34) Pondicherry 34")] = "pondicherry"
# all clear for this state? - any non-congruencies between the two datasets? 
res_2019$PC_name[which(res_2019$state == "(34) Pondicherry 34")][which(is.na(
  match(res_2019$PC_name[which(res_2019$state == "(34) Pondicherry 34")],
        unique(HIST$PC_name[which(HIST$State=="(34) Pondicherry 34")])
  )))]
sort(unique(HIST$PC_name[which(HIST$State=="(34) Pondicherry 34")]))


# (03) Punjab 03 PCs
res_2019$PC_name[which(res_2019$state == "(03) Punjab 03")]
# all clear for this state? - any non-congruencies between the two datasets? 
res_2019$PC_name[which(res_2019$state == "(03) Punjab 03")][which(is.na(
  match(res_2019$PC_name[which(res_2019$state == "(03) Punjab 03")],
        unique(HIST$PC_name[which(HIST$State=="(03) Punjab 03")])
  )))]
sort(unique(HIST$PC_name[which(HIST$State=="(03) Punjab 03")]))

# Rajasthan PCs
res_2019$PC_name[which(res_2019$state == "(08) Rajasthan 08"& res_2019$PC_name=="baran")] = "jhalawar-baran"
res_2019$PC_name[which(res_2019$state == "(08) Rajasthan 08"& res_2019$PC_name=="sawai madhopur")] = "tonk-sawai madhopur"
res_2019$PC_name[which(res_2019$state == "(08) Rajasthan 08"& res_2019$PC_name=="dholpur")] = "karauli-dholpur"
res_2019$PC_name[which(res_2019$state == "(08) Rajasthan 08"& res_2019$PC_name=="bikaner (sc)")] = "bikaner"
# all clear for this state? - any non-congruencies between the two datasets? 
res_2019$PC_name[which(res_2019$state == "(08) Rajasthan 08")][which(is.na(
  match(res_2019$PC_name[which(res_2019$state == "(08) Rajasthan 08")],
        unique(HIST$PC_name[which(HIST$State=="(08) Rajasthan 08")])
  )))]
sort(unique(HIST$PC_name[which(HIST$State=="(08) Rajasthan 08")]))

# Sikkim PCs
res_2019$PC_name[which(res_2019$state == "(11) Sikkim 11")]
# all clear for this state? - any non-congruencies between the two datasets? 
res_2019$PC_name[which(res_2019$state == "(11) Sikkim 11")][which(is.na(
  match(res_2019$PC_name[which(res_2019$state == "(11) Sikkim 11")],
        unique(HIST$PC_name[which(HIST$State=="(11) Sikkim 11")])
  )))]
sort(unique(HIST$PC_name[which(HIST$State=="(11) Sikkim 11")]))

# Tamil Nadu PCs
res_2019$PC_name[which(res_2019$state == "(33) Tamil Nadu 33"& res_2019$PC_name=="mayiladuthurai")] = "mayiladuturai"
res_2019$PC_name[which(res_2019$state == "(33) Tamil Nadu 33"& res_2019$PC_name=="tiruchirappalli")] = "thiruchirapalli"
# all clear for this state? - any non-congruencies between the two datasets? 
res_2019$PC_name[which(res_2019$state == "(33) Tamil Nadu 33")][which(is.na(
  match(res_2019$PC_name[which(res_2019$state == "(33) Tamil Nadu 33")],
        unique(HIST$PC_name[which(HIST$State=="(33) Tamil Nadu 33")])
  )))]
sort(unique(HIST$PC_name[which(HIST$State=="(33) Tamil Nadu 33")]))

# Tripura PCs
res_2019$PC_name[which(res_2019$state == "(16) Tripura 16")]
# all clear for this state? - any non-congruencies between the two datasets? 
res_2019$PC_name[which(res_2019$state == "(16) Tripura 16")][which(is.na(
  match(res_2019$PC_name[which(res_2019$state == "(16) Tripura 16")],
        unique(HIST$PC_name[which(HIST$State=="(16) Tripura 16")])
  )))]
sort(unique(HIST$PC_name[which(HIST$State=="(16) Tripura 16")]))

# Uttarakhand PCs
res_2019$PC_name[which(res_2019$state == "(05) Uttarakhand 05"& res_2019$PC_name=="udhamsingh nagar")] = "nainital-udhamsingh nagar"
# all clear for this state? - any non-congruencies between the two datasets? 
res_2019$PC_name[which(res_2019$state == "(05) Uttarakhand 05")][which(is.na(
  match(res_2019$PC_name[which(res_2019$state == "(05) Uttarakhand 05")],
        unique(HIST$PC_name[which(HIST$State=="(05) Uttarakhand 05")])
  )))]
sort(unique(HIST$PC_name[which(HIST$State=="(05) Uttarakhand 05")]))


# Uttar Pradesh PCs
res_2019$PC_name[which(res_2019$state == "(09) Uttar Pradesh 09"& res_2019$PC_name=="fatehpur")] = "fathepur"
res_2019$PC_name[which(res_2019$state == "(09) Uttar Pradesh 09"& res_2019$PC_name=="rae bareli")] = "raebareli"
res_2019$PC_name[which(res_2019$state == "(09) Uttar Pradesh 09"& res_2019$PC_name=="gautam buddha nagar")] = "gautam budhha nagar"

# all clear for this state? - any non-congruencies between the two datasets? 
res_2019$PC_name[which(res_2019$state == "(09) Uttar Pradesh 09")][which(is.na(
  match(res_2019$PC_name[which(res_2019$state == "(09) Uttar Pradesh 09")],
        unique(HIST$PC_name[which(HIST$State=="(09) Uttar Pradesh 09")])
  )))]
sort(unique(HIST$PC_name[which(HIST$State=="(09) Uttar Pradesh 09")]))


# West Bengal PCs 
res_2019$PC_name[which(res_2019$state == "(19) West Bengal 19" & res_2019$PC_name=="bardhaman durgapur")]="bardhaman-durgapur"
res_2019$PC_name[which(res_2019$state == "(19) West Bengal 19" & res_2019$PC_name=="arambagh")]="arambag"
res_2019$PC_name[which(res_2019$state == "(19) West Bengal 19" & res_2019$PC_name=="joynagar")]="jaynagar"
res_2019$PC_name[which(res_2019$state == "(19) West Bengal 19" & res_2019$PC_name=="barrackpore")]="barrackpur"
# all clear for this state? - any non-congruencies between the two datasets? 
res_2019$PC_name[which(res_2019$state == "(19) West Bengal 19")][which(is.na(
  match(res_2019$PC_name[which(res_2019$state == "(19) West Bengal 19")],
        unique(HIST$PC_name[which(HIST$State=="(19) West Bengal 19")])
  )))]

# party -> alliance
alliance_parties = read.csv("Election Data/Alliance_parties.csv")
alliance_parties$Party = tolower(as.character(unlist(alliance_parties$Party)))
res_2019$Party = tolower(as.character(unlist(res_2019$Party)))

alliance_parties$Party[which(alliance_parties$Party=="shiv sena")] = "shivsena"
alliance_parties$Party[which(alliance_parties$Party=="janata dal (united)")] = "janata dal  (united)"
alliance_parties$Party[which(alliance_parties$Party=="lok janshakti party")] = "lok jan shakti party"
alliance_parties$Party[which(alliance_parties$Party=="apna dal (sonelal)")] = "apna dal (soneylal)"
alliance_parties$Party[which(alliance_parties$Party=="bodoland people's front")] = "bodoland peoples front"
alliance_parties$Party[which(alliance_parties$Party=="naga people's front")] = "naga peoples front"
alliance_parties$Party[which(alliance_parties$Party=="shiv sangram")]
alliance_parties$Party[which(alliance_parties$Party=="goa forward party")]
alliance_parties$Party[which(alliance_parties$Party=="all jharkhand students union")] = "AJSU Party"
alliance_parties$Party[which(alliance_parties$Party=="manipur peoples party")] = "manipur people’s party"
alliance_parties$Party[which(alliance_parties$Party=="kamtapur people's party")]
alliance_parties$Party[which(alliance_parties$Party=="hill state people's democratic party")]
alliance_parties$Party[which(alliance_parties$Party=="kerala kamaraj congress")]
alliance_parties$Party[which(alliance_parties$Party=="praja socialist party")]
alliance_parties$Party[which(alliance_parties$Party=="democratic labor party (kerala)")]
alliance_parties$Party[which(alliance_parties$Party=="tamil maanila congress")] = "tamil maanila congress  (moopanar)"
alliance_parties$Party[which(alliance_parties$Party=="puthiya tamilagam")]

alliance_parties$Party[which(alliance_parties$Party=="kerala congress (nationalist)")]
alliance_parties$Party[which(alliance_parties$Party=="people's democratic front")]
alliance_parties$Party[which(alliance_parties$Party=="kerala congress (thomas)")]
alliance_parties$Party[which(alliance_parties$Party=="nishad party")]
alliance_parties$Party[which(alliance_parties$Party=="kerala janapaksham (secular)")]
alliance_parties$Party[which(alliance_parties$Party=="kerala congress (m)")] = "kerala congress  (m)"
alliance_parties$Party[which(alliance_parties$Party=="apna dal")]
alliance_parties$Party[which(alliance_parties$Party=="bharatiya tribal party")]
alliance_parties$Party[which(alliance_parties$Party=="hindustani awam morcha")] = "hindustani awam morcha (secular)"
alliance_parties$Party[which(alliance_parties$Party=="kerala congress (jacob)")]
alliance_parties$Party[which(alliance_parties$Party=="marumalarchi dravida munnetra kazhagam")]
alliance_parties$Party[which(alliance_parties$Party=="kongunadu makkal desia katchi")]
alliance_parties$Party[which(alliance_parties$Party=="communist marxist party (john)")]
alliance_parties$Party[which(alliance_parties$Party=="peace party of india")] = "peace party"
alliance_parties$Party[which(alliance_parties$Party=="mahan dal")]
alliance_parties$Party[which(alliance_parties$Party=="communist party of india (marxist)")] = "communist party of india  (marxist)"
alliance_parties$Party[which(alliance_parties$Party=="loktantrik janata dal")] 
alliance_parties$Party[which(alliance_parties$Party=="indhiya jananayaga katchi")]
alliance_parties$Party[which(alliance_parties$Party=="indigenous nationalist party of twipra")] = "indigenous people's front of tripura"
alliance_parties$Party[which(alliance_parties$Party=="communist party of india (marxist–leninist) liberation")]="communist party of india  (marxist-leninist)  (liberation)"
alliance_parties$Party[which(alliance_parties$Party=="yuva swabhiman party")]

cbind(tolower(alliance_parties$Party),tolower(alliance_parties$Base.State))[
  which(is.na(
    match(
      tolower(alliance_parties$Party),
      tolower(levels(res_2019$Party))
    ) ) ),]

sort(tolower(unique(res_2019$Party[which(res_2019$state=="(20) Jharkhand 20")])))
unique(res_2019$Party[res_2019$state=="(32) Kerala 32"])
unique(res_2019$state)

# assign alliance
res_2019$alliance = 
  ifelse(is.na(
    alliance_parties$Alliance[
      match(
        tolower(res_2019$Party),
        tolower(alliance_parties$Party)
      )]),"OTHER",as.character(unlist(
        alliance_parties$Alliance[
          match(
            tolower(res_2019$Party),
            tolower(alliance_parties$Party)
          )]))
  )

res_2019$alliance[which(res_2019$Party=="janata dal  (secular)")] = "UPA"
res_2019$alliance[which(res_2019$Party=="sikkim krantikari morcha")] = "OTHER" # (https://timesofindia.indiatimes.com/india/skm-parts-ways-with-bjp-in-sikkim/articleshow/68432996.cms)
res_2019$alliance[which(res_2019$Party=="telugu desam")] = "UPA" # https://en.wikipedia.org/wiki/Telugu_Desam_Party
unique(res_2019$Party)

# print party-alliance breakdown
tmx = res_2019[order(res_2019$alliance,res_2019$Party),]
print(xtable(unique(tmx[!tmx$alliance=="OTHER",c("Party","alliance")])),include.rownames=FALSE)

# winner 

res_2019 = as.data.table(res_2019)
res_2019[,NA.:=NULL,]
res_2019[,NA..1:=NULL,]
res_2019[,rank := rank(-Pct.of.Votes),by = c("state","PC_name")]

# party breakdown
table(res_2019$Party,res_2019$alliance,res_2019$rank==1)[
  rowSums(table(res_2019$Party,res_2019$alliance,res_2019$rank==1)[,,2])!=0
  ,,2][rev(order(table(res_2019$Party,res_2019$alliance,res_2019$rank==1)[
    rowSums(table(res_2019$Party,res_2019$alliance,res_2019$rank==1)[,,2])!=0
    ,,2][,1],table(res_2019$Party,res_2019$alliance,res_2019$rank==1)[
      rowSums(table(res_2019$Party,res_2019$alliance,res_2019$rank==1)[,,2])!=0
      ,,2][,2],table(res_2019$Party,res_2019$alliance,res_2019$rank==1)[
        rowSums(table(res_2019$Party,res_2019$alliance,res_2019$rank==1)[,,2])!=0,,2][,3])),]

#alliance breakdown - seats
table(res_2019$alliance,res_2019$rank==1)[,2]

# national alliance vote shares 
res_2019_AL = res_2019

res_2019_AL[,State.Total.Votes := sum(Total.Votes),by = c("state")]
res_2019_AL[,State.Alliance.Total.Votes := sum(Total.Votes),by = c("state","alliance")]
res_2019_AL$State.Alliance.Pct.of.Votes = round(100*res_2019_AL$State.Alliance.Total.Votes/res_2019_AL$State.Total.Votes,2)

res_2019_AL[,Nat.Total.Votes := sum(Total.Votes)]
res_2019_AL[,Nat.Alliance.Total.Votes := sum(Total.Votes),by = c("alliance")]
res_2019_AL$Nat.Alliance.Pct.of.Votes = round(100*res_2019_AL$Nat.Alliance.Total.Votes/res_2019_AL$Nat.Total.Votes,2)


fwrite(res_2019_AL,"Generated Quantities/2019_Results.csv")


state_T_res = read.csv (file = "Election Data/India_2019_Turnout.csv")[-c(1:2),][1:36,c("State.Name","Total.Electors","X.5")]
names(state_T_res) = c("State","Electors","Total_Votes")
state_T_res$Electors = as.numeric(as.character(unlist(state_T_res$Electors)))
state_T_res$Total_Votes = as.numeric(as.character(unlist(state_T_res$Total_Votes)))

state_T_res$State = 
  ifelse(state_T_res$State=="Andaman & Nicobar Islands","(35) Anadman/Nicobar 35",
         ifelse(state_T_res$State=="Andhra Pradesh*","(28) Andhra Pradesh 28",
                ifelse(state_T_res$State=="Arunachal Pradesh","(12) Arunachal Pradesh 12",
                       ifelse(state_T_res$State=="Assam","(18) Assam 18",
                              ifelse(state_T_res$State=="Bihar","(10) Bihar 10",
                                     ifelse(state_T_res$State=="Chandigarh","(04) Chandigarh 04",
                                            ifelse(state_T_res$State=="Chhattisgarh","(22) Chhattisgarh 22",
                                                   ifelse(state_T_res$State=="Dadra & Nagar Haveli","(26) Dadra+Nagar Haveli 26",
                                                          ifelse(state_T_res$State=="Daman & Diu","(25) Daman & Diu 25",
                                                                 ifelse(state_T_res$State=="Goa","(30) Goa 30",
                                                                        ifelse(state_T_res$State=="Gujarat*","(24) Gujarat 24",
                                                                               ifelse(state_T_res$State=="Haryana","(06) Haryana 06",
                                                                                      ifelse(state_T_res$State=="Himachal Pradesh","(02) Himachal Pradesh 02",
                                                                                             ifelse(state_T_res$State=="Jammu & Kashmir","(01) Jammu & Kashmir 01",
                                                                                                    ifelse(state_T_res$State=="Jharkhand","(20) Jharkhand 20",
                                                                                                           ifelse(state_T_res$State=="Karnataka","(29) Karnataka 29",
                                                                                                                  ifelse(state_T_res$State=="Kerala","(32) Kerala 32",
                                                                                                                         ifelse(state_T_res$State=="Lakshadweep","(31) Lakshadweep 31",
                                                                                                                                ifelse(state_T_res$State=="Madhya Pradesh","(23) Madhya Pradesh 23",
                                                                                                                                       ifelse(state_T_res$State=="Maharashtra","(27) Maharashtra 27",
                                                                                                                                              ifelse(state_T_res$State=="Manipur","(14) Manipur 14",
                                                                                                                                                     ifelse(state_T_res$State=="Meghalaya*","(17) Meghalaya 17",
                                                                                                                                                            ifelse(state_T_res$State=="Mizoram","(15) Mizoram 15",
                                                                                                                                                                   ifelse(state_T_res$State=="Nagaland","(13) Nagaland 13",
                                                                                                                                                                          ifelse(state_T_res$State=="NCT OF Delhi","(07) Delhi 07",
                                                                                                                                                                                 ifelse(state_T_res$State=="Odisha","(21) Orissa 21",
                                                                                                                                                                                        ifelse(state_T_res$State=="Puducherry","(34) Pondicherry 34",
                                                                                                                                                                                               ifelse(state_T_res$State=="Punjab","(03) Punjab 03",
                                                                                                                                                                                                      ifelse(state_T_res$State=="Rajasthan","(08) Rajasthan 08",
                                                                                                                                                                                                             ifelse(state_T_res$State=="Sikkim","(11) Sikkim 11",
                                                                                                                                                                                                                    ifelse(state_T_res$State=="Tamil Nadu","(33) Tamil Nadu 33",
                                                                                                                                                                                                                           ifelse(state_T_res$State=="Telangana*","(28) Andhra Pradesh 28",
                                                                                                                                                                                                                                  ifelse(state_T_res$State=="Tripura","(16) Tripura 16",
                                                                                                                                                                                                                                         ifelse(state_T_res$State=="Uttar Pradesh","(09) Uttar Pradesh 09",
                                                                                                                                                                                                                                                ifelse(state_T_res$State=="Uttarakhand","(05) Uttarakhand 05",
                                                                                                                                                                                                                                                       ifelse(state_T_res$State=="West Bengal","(19) West Bengal 19",
                                                                                                                                                                                                                                                              state_T_res$State
                                                                                                                                                                                                                                                       ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) )

state_T_res = aggregate(state_T_res[,c("Electors","Total_Votes")],
                        by = list(State = state_T_res$State),sum
)
state_T_res$T = state_T_res$Total_Votes/state_T_res$Electors

fwrite(state_T_res,file="Generated Quantities/Turnout_2019_state.csv")
