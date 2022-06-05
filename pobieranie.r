# https://ec.europa.eu/eurostat/web/population-demography/demography-population-stock-balance/database?node_code=demomwk
# https://appsso.eurostat.ec.europa.eu/nui/submitViewTableAction.do
# https://appsso.eurostat.ec.europa.eu/nui/setupDownloads.do
library("readxl")
library("data.table")
library("reshape")

dataDir    <- file.path(getwd(),"data")
# rozpakowywanie danych  GUS
unzip(file.path(dataDir,"zgony_wg_tygodni.zip"),exdir=file.path(dataDir),setTimes=T)

# zamiana polskich znakow w nazwach plikow 
res <- try({
  hd <- getwd()
  setwd(file.path(dataDir,"zgony_wg_tygodni"))
  try({
    lapply(dir(),function(f){
      file.rename(
        from=f, 
        to = gsub(" ","_",gsub("\x88","l",f))
      )
    })
  })
  setwd(hd)
  
})
# rozpakowywanie danych  EUROSTAT
unzip(file.path(dataDir,"demo_r_mwk_ts.zip"),exdir=file.path(dataDir),setTimes=T)


#####przerabianie plikow GUS
czytajDaneLiczboweZZakladki <- function(f,sheet,plec){
  
  d <- as.data.frame(read_excel(f,sheet=sheet))
  colnames(d)[1:3] <- c("Grupa_wiekowa","Region_id","Region")
  d <- d[-c(1:(grep("^Og",d$Grupa_wiekowa)[1]-1)),]
  
  tygodnie <- 1:(ncol(d)-3)
  tygodnie[nchar(tygodnie)<2] <- paste0("0",tygodnie[nchar(tygodnie)<2])
  colnames(d)[4:ncol(d)] <- tygodnie
  
  d <- reshape::melt(d,id.vars=c("Grupa_wiekowa","Region_id","Region"))
  colnames(d) <- c("Grupa_wiekowa","Region_id","Region","Tydzien","Liczba")
  d$Grupa_wiekowa[grep("Og",d$Grupa_wiekowa)] <- "0 - Inf"
  d$Grupa_wiekowa[grep("wi",d$Grupa_wiekowa)] <- "90 - Inf"
  #d$Liczba[is.na(d$Liczba)] <- 0 
  d <- cbind("Plec"=plec,d)
  
  return(d)
  
}
#------------------------------------------------------------------------------

# iterujemy po plikach; wczytujemy; obrabiamy  
hd <- getwd()
setwd(file.path(dataDir,"zgony_wg_tygodni"))

try({
  mainRet <- do.call("rbind",lapply(dir(),function(f){
    print(f)
    
    ogolem <- czytajDaneLiczboweZZakladki(f,1,"Ogolem")
    mezczyzni <- czytajDaneLiczboweZZakladki(f,2,"Mezczyzni")
    kobiety <- czytajDaneLiczboweZZakladki(f,3,"Kobiety")
    
    dane <- rbind(ogolem,mezczyzni,kobiety)
    
    # tygodnie 
    tygodnie <- as.data.frame(read_excel(f,sheet=grep("tyg",tolower(excel_sheets(f)))))
    tygodnie <- do.call("rbind",lapply(split(tygodnie,tygodnie[,2]),function(x){
      return(data.frame(Tydzien=unique(x[,2]),Od=min(x[,1]),Do=max(x[,1])))
    }))
    tygodnie$Tydzien <- as.character(tygodnie$Tydzien)
    tygodnie$Tydzien <- gsub("T|W","",unlist(lapply(strsplit(tygodnie$Tydzien,"-"),function(x){x[2]})))
    rownames(tygodnie) <- NULL
    
    dane <- merge(x=dane,y=tygodnie,by="Tydzien",all=T)
    dane <- dane[,-which(colnames(dane)=="Tydzien")]
    
    dane <- dane[c("Od","Do","Plec","Grupa_wiekowa","Region_id","Region","Liczba")]
    dane$Liczba <- as.integer(dane$Liczba)
    
    dane$Grupa_wiekowa[dane$Grupa_wiekowa=="0 - 4"] <- "00 - 04"
    dane$Grupa_wiekowa[dane$Grupa_wiekowa=="5 - 9"] <- "05 - 09"
    
    return(dane)
  }))
  
  write.table(mainRet,file="../GUS_dane_przetworzone_pelne.csv",sep=";",dec=",",row.names=F)
})
setwd(hd)


#######################################################################koncowe przerobienie danych
rm(list=ls())

euro <- read.table(file="data/demo_r_mwk_ts_1_Data.csv",sep=",",dec=",",header=T,stringsAsFactors=F)
euro$Value <- as.integer(gsub(",|:","",euro$Value))
euro$GEO <- gsub("Germany (until 1990 former territory of the FRG)", "Germany", euro$GEO, fixed=T)
euro$GEO <- gsub("Czechia", "Czech Republic", euro$GEO, fixed=T)
euro$rok <- substr(euro$TIME, 1, 4)
euro <- na.omit(euro)
euro <- euro[!(euro$rok== 2022), ]


gus <- read.table(file="data/GUS_dane_przetworzone_pelne.csv",sep=";",dec=",",header=T,stringsAsFactors=F)
gus$rok <- substr(gus$Do, 1, 4)
gus$Region <- sub("Makroregion Województwo Mazowieckie", "Mazowieckie", gus$Region)
gus <- na.omit(gus)
gus <- gus[!(gus$rok== 2022), ]
