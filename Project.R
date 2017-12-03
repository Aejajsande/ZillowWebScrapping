#calling all the libraries needed
library(stringr)
library(RSelenium)
library(XML)
library(rvest)
library(xlsx)

# initializing all the vectors needed
zpids=c()
regURL=c()
YearBuilt=c()
LotSize=c()
FloorSize=c()
CurrPrice=c()
PriceSqft=c()
MLSNumber=c()
address=c()
ParcelNumber=c()
stories=c()
ZillowHomeID=c()
LastRemodelYear=c()
StructureType=c()
RoomNumber=c()
ZipCode=c()
HouseType=c()
BedsNumber=c()
BathsNumber=c()
skls=c()
pHist=c()
tHist=c()
neighbourHood=c()
#URL for the base zillow page
baseurl = "https://www.zillow.com/homes/recently_sold/Houston-TX/39051_rid/750001-_price/2839-_mp/globalrelevanceex_sort/30.151658,-94.934235,29.482045,-95.872193_rect/9_zm/"
pages=c()

for (i in 1:20)
{
  tempURL=readLines(paste0(baseurl,i,"_p"))
  temphtmlpage <- htmlParse(tempURL, asText = TRUE)
  #zpids <- c(zpids,xpathSApply(htmlpage, "//*[@id='search-results']/ul[@class='photo-cards']/li/article", function(u) xmlAttrs(u)["data-zpid"]))
  regURL <- c(hrefs,xpathSApply(temphtmlpage, "//*[@id='search-results']/ul[@class='photo-cards']/li/article/div[@class='zsg-photo-card-content zsg-aspect-ratio-content']/a", function(u) xmlAttrs(u)["href"]))
  
  pages=c(pages,temphtmlpage)
}

# creating the selenium server
getSeleniumDriver<- rsDriver() # search for and download Selenium Server java binary.  Only need to run once.
remoteDr <- getSeleniumDriver$client

##Price/sqft
getPriceSqft=function(facts)
{
  isPPSPresent<-grep("[Pp]rice/sqft",facts)
  if(!length(isPPSPresent)==0)
  {PriceSqft=c(PriceSqft,sub("(.*): ","",facts[isPPSPresent]))
  }else
  {PriceSqft=c(PriceSqft,"NA")}
  return(PriceSqft)
}
##MLS
getMLSNumber=function(facts)
{isMLSPresent<-grep("MLS #",facts)
if(!length(isMLSPresent)==0)
{MLSNumber=c(MLSNumber,sub("(.*): ","",facts[isMLSPresent]))
}else
{MLSNumber=c(MLSNumber,"NA")}
return(MLSNumber)}
##Neighbour
getNeighborhood=function(neighbour)
{isNeighPresent<-grep("Neighborhood:",neighbour)
if(!length(isNeighPresent)==0)
{neighbourHood=c(neighbourHood,sub("(.*): ","",neighbour[isNeighPresent]))
}else
{neighbourHood=c(neighbourHood,"NA")}
return(neighbourHood)}
#Address
getAddress=function(meta)
{
  
  if(!length(meta)==0){
    ans<-html_attr(meta,"content")
    address=c(address,ans)
  }else
  {address=c(address,"NA")
  return(address)}
}
#FloorSize
getFloorSize=function(others)
{
  isFloorPresent<-grep("Floor size",others)
  if(!length(isFloorPresent)==0)
  {FloorSize=c(FloorSize,sub("(.*): ","",others[isFloorPresent]))
  }else
  {FloorSize=c(FloorSize,"NA")}
  return(FloorSize)}
#ParcelNumber
getParcelNumber=function(others)
{
  isParcelPresent<-grep("Parcel #",others)
  if(!length(isParcelPresent)==0)
  {ParcelNumber=c(ParcelNumber,sub("(.*): ","",others[isParcelPresent]))
  }else
  {ParcelNumber=c(ParcelNumber,"NA")}
  return(ParcelNumber)}
#Stories
getStories=function(others)
{
  isStoriesPresent<-grep("^Stories",others)
  if(!length(isStoriesPresent)==0)
  {stories=c(stories,sub("(.*): ","",others[isStoriesPresent]))
  }else
  {stories=c(stories,"NA")}
  return(stories)}
#yearBuilt
getYearBuilt=function(facts)
{
  isYearBuiltPresent<-grep("Built in",facts)
  if(!length(isYearBuiltPresent)==0)
  {YearBuilt=c(YearBuilt,sub("(.*) ","",facts[isYearBuiltPresent]))
  }else
  {YearBuilt=c(YearBuilt,"NA")}
  return(YearBuilt)
}
##LotSize
getLotSize=function(facts)
{
  isLotSizePresent<-grep("Lot",facts)
  if(!length(isLotSizePresent)==0)
  {LotSize=c(LotSize,sub("(.*): ","",facts[isLotSizePresent]))
  }else
  {LotSize=c(LotSize,"NA")}
  return(LotSize)
}

#ZillowHomeId
getZillowHomeID=function(others)
{
  isZillowIDPresent<-grep("Zillow Home ID",others)
  if(!length(isZillowIDPresent)==0)
  {ZillowHomeID=c(ZillowHomeID,sub("(.*): ","",others[isZillowIDPresent]))
  }else
  {ZillowHomeID=c(ZillowHomeID,"NA")}
  return(ZillowHomeID)}
#LastRemodelYear
getLastRemodelYear=function(others)
{
  isLRYPresent<-grep("Last remodel",others)
  if(!length(isLRYPresent)==0)
  {LastRemodelYear=c(LastRemodelYear,sub("(.*): ","",others[isLRYPresent]))
  }else
  {LastRemodelYear=c(LastRemodelYear,"NA")}
  return(LastRemodelYear)}
#StructureType
getStructureType=function(others)
{
  isStructurePresent<-grep("Structure type",others)
  if(!length(isStructurePresent)==0)
  {StructureType=c(StructureType,sub("(.*): ","",others[isStructurePresent]))
  }else
  {StructureType=c(StructureType,"NA")}
  return(StructureType)}
#RoomNumber
getRoomNumber=function(others)
{
  isRoomNumberPresent<-grep("Room count",others)
  if(!length(isRoomNumberPresent)==0)
  {RoomNumber=c(RoomNumber,sub("(.*): ","",others[isRoomNumberPresent]))
  }else
  {RoomNumber=c(RoomNumber,"NA")}
  return(RoomNumber)}

getHistoryTables<-function(tableType,tableNode,priceNode,remoteDr)
{
  
  if(length(tableNode)==0){
    return("NA")
  }else{
    remoteDr$setImplicitWaitTimeout(20000)
    tab <-remoteDr$findElements(using = 'class','zsg-table')
    if(length(tab)==0){
      return("NA")
    }
    if(tableType=="price"){
      a<-tab[[1]]$getElementAttribute("outerHTML")
    }else{
      if(length(tab)<2){
        a<-tab[[1]]$getElementAttribute("outerHTML")
      }else{
        a<-tab[[2]]$getElementAttribute("outerHTML")  
      }
      
    }
    
    temptable <- readHTMLTable(a[[1]], header=TRUE, as.data.frame=TRUE)[[1]]
    acc<-temptable
    midTable<-temptable[-length(temptable)]
    
    ans<-""
    for(i in 1:nrow(midTable)){
      for(j in 1:ncol(midTable)){
        
        if(j==1){
          ans<-paste0(ans,toString(midTable[i,j]))
        }else{
          ans<-paste(ans,toString(midTable[i,j]),sep = "_")
          
        }
      }
      ans<-paste0(ans,";")
    }
    return(ans)
  }
}
getCurrentPrice<-function(currPriceNode)
{
  if(length(currPriceNode)==0){
    return("NA")
  }else{
    if(html_text(currPriceNode)=="     "){
      return("NA")
    }else{
      return(html_text(currPriceNode))    
    }
    
  }
}
#HouseType
getHouseType=function(facts)
{
  isHouseTypePresent=facts[grep("([Ss]ingle [Ff]amily)|([Mm]ulti [Ff]amily)|([Cc]ondo) ",facts)]
  if(!length(isHouseTypePresent)==0)
  {HouseType=c(HouseType,isHouseTypePresent)
  }else
  {HouseType=c(HouseType,"NA")}
  return(HouseType)
}
#BedsNumber
getBedsNumber=function(heads)
{
  isBedNumberPresent=heads[grep("(.*) beds",heads)]
  if(!length(isBedNumberPresent)==0)
  {BedsNumber=c(BedsNumber,sub(" (.*)","",isBedNumberPresent))
  }else
  {BedsNumber=c(BedsNumber,"NA")}
  return(BedsNumber)
}
#BathNumber
getBathsNumber=function(heads)
{
  isBathNumberPresent=heads[grep("(.*) baths",heads)]
  if(!length(isBathNumberPresent)==0)
  {BathsNumber=c(BathsNumber,sub(" (.*)","",isBathNumberPresent))
  }else
  {BathsNumber=c(BathsNumber,"NA")}
  return(BathsNumber)
}
#Schoolskimaaka
getSchoolNames=function(schools)
{
  if(!length(schools)==0)
  { skools=""
  lk=str_replace_all(schools,"\n","")
  pk=str_replace_all(lk," \\s+","_")
  dk=str_replace_all(pk,"(^_)|(_$)","")
  for(a in 1:length(schools))
  {
    skools=paste(skools,dk[a],sep=";") 
  }
  
  skls=c(skls,skools)
  }else
  {skls=c(skls,"NA")}
  return(str_replace_all(skls,"^;",""))
}
#ZipCode
getZipCode=function(meta)
{
  if(!length(meta)==0){
    zipAdd<-html_attr(meta,"content")
    zc=grep("(.*) [0-9]{5}$",zipAdd)
    if(zc==1){
      ZipCode=c(ZipCode,sub("(.*) ","",zipAdd))
    }else{
      ZipCode=c(ZipCode,"NA")
    }
  }else{
    ZipCode=c(ZipCode,"NA")
  }
  return(ZipCode)
}


#get all data

url="https://www.zillow.com"
individualUrl=paste0(url,regURL)
for(j in 1:length(individualUrl))
{ 
  
  indPage = read_html(individualUrl[j])
  NodeForFacts=html_nodes(indPage,xpath="//*[@id='hdp-content']/div[@role='main']/div/section[@class='zsg-content-section ']/div[@class='hdp-facts zsg-content-component z-moreless']/div[@class='fact-group-container zsg-content-component top-facts']/ul[@class='zsg-list_square zsg-lg-1-3 zsg-md-1-2 zsg-sm-1-1']/li")
  if(!length(NodeForFacts)==0){
    NodeForPrice=html_node(indPage,xpath='//div[@id="hdp-price-history"]')
    NodeForTax=html_node(indPage,xpath='//div[@id="hdp-tax-history"]')  
    NodesForneighboringSchool=html_nodes(indPage,xpath="//*[@id='nearbySchools']/div[1]/div[@class='zsg-content-item']/ul[@class='nearby-schools-list']/li[@class='nearby-school assigned-school  clearfix']/div[@class='nearby-schools-info']")
    neighbourNodes=html_nodes(indPage,xpath="//*[@id='hdp-content']/div[@role='main']/div/section[@id='hdp-neighborhood']/h2")
    NodeForCurrentPrice=html_node(indPage,xpath='//*[@id="home-value-wrapper"]/div[@class="estimates"]/div[@class="main-row status-icon-row recently-sold-row home-summary-row"]/span[2]')
    AddressMetaNode=html_node(indPage,xpath = '//head/meta[@property="og:zillow_fb:address"]')
    NodeForOthers=html_nodes(indPage,xpath="//*[@id='hdp-content']/div[@role='main']/div/section[@class='zsg-content-section ']/div[@class='hdp-facts zsg-content-component z-moreless']/div[@class='fact-group-container zsg-content-component z-moreless-content hide']/ul[@class='zsg-list_square zsg-lg-1-3 zsg-md-1-2 zsg-sm-1-1']/li")
    NodesForHeadDetails=html_nodes(indPage,xpath="//*[@id='hdp-content']/div[@role='main']/div/div[@class='zsg-lg-2-3 zsg-sm-1-1 hdp-header-description']/header[@class='zsg-content-header addr']/h3/span")
    
    facts = html_text(NodeForFacts)
    others = html_text(NodeForOthers)
    heads = html_text(NodesForHeadDetails)
    schools = html_text(NodesForneighboringSchool)
    neighbour = html_text(neighbourNodes)
    
  }else{
    NodeForPrice=html_node(indPage,xpath='//div[@id="hdp-price-history"]')
    NodeForTax=html_node(indPage,xpath='//div[@id="hdp-tax-history"]')
    NodeForFacts=html_nodes(indPage,xpath="//*[@id='hdp-content']/div[@role='main']/section[@class='zsg-content-section ']/div[@class='hdp-facts zsg-content-component z-moreless']/div[@class='fact-group-container zsg-content-component top-facts']/ul[@class='zsg-list_square zsg-lg-1-3 zsg-md-1-2 zsg-sm-1-1']/li")
    AddressMetaNode=html_node(indPage,xpath = '//head/meta[@property="og:zillow_fb:address"]')
    NodeForOthers=html_nodes(indPage,xpath="//*[@id='hdp-content']/div[@role='main']/section[@class='zsg-content-section ']/div[@class='hdp-facts zsg-content-component z-moreless']/div[@class='fact-group-container zsg-content-component z-moreless-content hide']/ul[@class='zsg-list_square zsg-lg-1-3 zsg-md-1-2 zsg-sm-1-1']/li")
    NodesForHeadDetails=html_nodes(indPage,xpath="//*[@id='hdp-content']/div[@role='main']/div[@class='zsg-lg-2-3 zsg-sm-1-1 hdp-header-description']/header[@class='zsg-content-header addr']/h3/span")
    NodesForneighboringSchool=html_nodes(indPage,xpath="//*[@id='nearbySchools']/div[1]/div[@class='zsg-content-item']/ul[@class='nearby-schools-list']/li[@class='nearby-school assigned-school  clearfix']/div[@class='nearby-schools-info']")
    neighbourNodes=html_nodes(indPage,xpath="//*[@id='hdp-content']/div[@role='main']/section[@id='hdp-neighborhood']/h2")
    NodeForCurrentPrice=html_node(indPage,xpath='//*[@id="home-value-wrapper"]/div[@class="estimates"]/div[@class="main-row  home-summary-row"]/span[1]')
    
    
    facts = html_text(NodeForFacts)
    others = html_text(NodeForOthers)
    heads = html_text(NodesForHeadDetails)
    schools = html_text(NodesForneighboringSchool)
    neighbour = html_text(neighbourNodes)
  }
  
  
  
  CurrPrice=c(CurrPrice,getCurrentPrice(NodeForCurrentPrice))
  YearBuilt=getYearBuilt(facts)
  LotSize=getLotSize(facts)
  PriceSqft=getPriceSqft(facts)
  FloorSize=getFloorSize(others)
  MLSNumber=getMLSNumber(facts)
  address=getAddress(meta)
  ParcelNumber=getParcelNumber(others)
  stories=getStories(others)
  ZillowHomeID=getZillowHomeID(others)
  LastRemodelYear=getLastRemodelYear(others)
  neighbourHood=getNeighborhood(neighbour)
  StructureType=getStructureType(others)
  RoomNumber=getRoomNumber(others)
  ZipCode=getZipCode(meta)
  HouseType=getHouseType(facts)
  BedsNumber=getBedsNumber(heads)
  BathsNumber=getBathsNumber(heads)
  skls=getSchoolNames(schools)
  
  
  
  
  remoteDr$navigate(individualUrl[j])  
  pHist = c(pHist,getHistoryTables("price",NodeForPrice,NodeForPrice,remoteDr))
  tHist = c(tHist,getHistoryTables("tax",NodeForTax,NodeForPrice,remoteDr))
  
  write_html(indPage,file =sprintf("C:\\HTMLPages\\%s.html",ZillowHomeID[length(ZillowHomeID)]))
  
  
}
dataOneRow=data.frame("Zillow_Home_ID"=ZillowHomeID,"Current_Price"=CurrPrice,"Price_Each_SQFT"=PriceSqft,"Price_History"=pHist,"Tax_History"=tHist,"URL"=individualUrl,"Address"=address,"NEIGHBOURHOOD"=neighbourHood,"Zip_Code"=ZipCode,"School"=skls,"Lot_Size"=LotSize,"Floor_Size"=FloorSize,"MLS_Number"=MLSNumber,"Parcel_Number"=ParcelNumber,"stories"=stories,"Built_Year"=YearBuilt,"Last_Year_Remodel"=LastRemodelYear,"House_Type"=HouseType,"Structure_Type"=StructureType,"Baths_Number"=BathsNumber,"Beds_Number"=BedsNumber,"Room_Num"=RoomNumber)

