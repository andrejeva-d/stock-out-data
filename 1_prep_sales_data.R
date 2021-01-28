


######################################################################################
# Get and prepare XML file
######################################################################################

library(readxl)
library(stringr)
All <- read_excel("~/R_projects/Sandoz_DA/Data/Send to candidates/All product sales.xlsx")
All<-data.frame(apply(All,2,tolower))

tab<-data.frame(table(All$Company))
All$Company<-str_replace_all(All$Company,"pfizer oy anim","pfizer")
All$Company<-str_replace_all(All$Company,"fresenius k de","fresenius")
All$Company<-str_replace_all(All$Company,"fresen med","fresenius")
All$Company<-str_replace_all(All$Company,"braun medical","b. braun b.")
All$Company<-str_replace_all(All$Company,"mundipharma cor","mundipharma")
All$Company<-str_replace_all(All$Company,"mundipharma a/s","mundipharma")
All$Company<-str_replace_all(All$Company,"substipharm lim","substipharm")
All$Company<-str_replace_all(All$Company,"vetpharma anima","vetpharma a/s")
All$Company<-str_replace_all(All$Company,"cross vetpharm","vetpharma a/s")
All$Company<-str_replace_all(All$Company,"fresenius k de","fresenius")
All$Company<-str_replace_all(All$Company,"eurocept international b.v.","eurocept")
All$Company<-str_replace_all(All$Company,"b. braun medical","b. braun b.")
All$Company<-str_replace_all(All$Company,"ceva santÉ","ceva sante anim")
All$Company<-str_replace_all(All$Company,"lundbeck pharma a/s","lundbeck")
All$Company<-str_replace_all(All$Company,"allergan norden","allergan")
All$Company<-str_replace_all(All$Company,"shire orphan th","shire")
All$Company<-str_replace_all(All$Company,"shire p contrac","shire")
All$Company<-str_replace_all(All$Company,"merial sas","merial")
All$Company<-str_replace_all(All$Company,"merial norden","merial")
All$Company<-str_replace_all(All$Company,"biomarin pharma","biomarin")
All$Company<-str_replace_all(All$Company,"biomarin intern","biomarin")
All$Company<-str_replace_all(All$Company,"amneal pharma europe limited","amneal pharm")
All$Company<-str_replace_all(All$Company,"amring pharma","amring s.a.s.")
All$Company<-str_replace_all(All$Company,"ceva animal","ceva sante anim")
All$Company<-str_replace_all(All$Company,"ceva vetem","ceva sante anim")
All$Company<-str_replace_all(All$Company,"chanelle","chanelle pharm")
All$Company<-str_replace_all(All$Company,"chiesi","chiesi pharma")
All$Company<-str_replace_all(All$Company,"cipla europe nv","cipla")
All$Company<-str_replace_all(All$Company,"den norske eterfab","den norske eter")
All$Company<-str_replace_all(All$Company,"desitin","desitin pharma a/s")
All$Company<-str_replace_all(All$Company,"eurocept international b.v.","eurocept")
All$Company<-str_replace_all(All$Company,"exeltis sverige","exeltis healthc")


tab2<-subset(tab, Freq<51)
#unknown


tab<-data.frame(table(All$Substance))
All$Substance<-str_replace_all(All$Substance,"\\."," ")
All$Substance<-str_replace_all(All$Substance,"\\,"," ")
All$Substance<-str_replace_all(All$Substance,"  "," ")
All$Substance<-str_replace_all(All$Substance,"who atc not applicable",NA_character_)
All$Substance<-str_replace_all(All$Substance,"anti-d (rh) ig","anti-d (rh) immunoglobulin")
All$Substance<-str_replace_all(All$Substance,"c1-inhibitor plasma deriv","c1-inhibitor plasma derived")
All$Substance<-str_replace_all(All$Substance,"c1 inhibitor plasma derived","c1-inhibitor plasma derived")
All$Substance<-str_replace_all(All$Substance,"amoxicillin&b-lactam inh","amoxicillin and beta-lactamase inhibitor")
All$Substance<-str_replace_all(All$Substance,"piperacillin&b-lact inh","piperacillin and beta-lactamase inhibitor")

splitdat<-split(All, f=All$Nordic.ArticleNo)
splitdat_multiple<-splitdat[sapply(splitdat, nrow)>2]

subset(All, All$Nordic.ArticleNo=="002019")[,1:4]
subset(All, All$Nordic.ArticleNo=="016294")[,1:4]
subset(All, All$Nordic.ArticleNo=="135362")[,1:4]
subset(All, All$Nordic.ArticleNo=="136121")[,1:4]

save(All, file="Data/Sales data.RData")



