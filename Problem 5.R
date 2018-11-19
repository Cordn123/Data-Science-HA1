#Problem 5
rm(list=ls()) 
install.packages("xml2")
library(xml2) 
library(data.table) 
library(stringr)

Sys.setlocale(locale = "ru_RU") #this little piece of **** made us suffer so much we couldn't finish the problem :(

#5.1
#Define the working directory: 
unzip(zipfile = "hw1p5.zip")
working_dir_1 <- "/Users/PatratskyAlexander/Desktop/ICEF 3rd Year/Data Science/HA/HA1/Data-Science-HA1/hw1p5/notifications"
working_dir_2 <- "/Users/PatratskyAlexander/Desktop/ICEF 3rd Year/Data Science/HA/HA1/Data-Science-HA1/hw1p5/notifications/daily" 

data_extraction <- function(archive, directive){
  
  setwd(directive)
  zips_to_loop <- list.files(directive)
  zips_to_loop <- zips_to_loop[str_detect(zips_to_loop,".zip")]
  
  #Define archive: 
  x <- zips_to_loop[archive]
  #Obtain contents of the zip file: 
  list_of_xmls<-unzip(x,list=TRUE)[,1]
  #Take one file within a zip file:
  y<-list_of_xmls[1]
  #Establish connection: 
  conn<-unz(x,y)
  #Read the connection: 
  data_raw<-read_xml(conn, encoding = "UTF-8")
  
  #Obtain children of the data_raw: 
  data_children<-xml_children(data_raw)
  #Process start processing one child: 
  one_child<-data_children[1]
  #Convert to list: 
  data_one_child<-unlist(as_list(one_child))
  #Define function which does extraction by name: 
  function_extract_names<-function(x,y){
    y[names(y)==x] }
  #Obtain unique names: 
  x_names<-unique(names(data_one_child))
  
  #
  if (is.null(x_names) == TRUE){
    #data_one_child <- data.table()
    print("ITS A TRAP")
  } else {
    
    #Do the extraction by name:
    #Just apply the above function: 
    data_one_child<-lapply(x_names,function_extract_names,y=data_one_child)
    #Then collapse multiple fields:
    data_one_child<-sapply(data_one_child,paste,collapse="&&&&")
    #Finally convert to data.table (notice the transpose - we do it to have a row of the data.table not the column) 
    data_one_child<-data.table(t(data_one_child))
    #Set names 
    setnames(data_one_child, x_names) 
    
    #Accounting for children with no name
    if (is.null(data_one_child$placingWay.code == TRUE)){
      data_one_child$child_name = "N0_NAME"
    } else { #giving children names
      data_one_child$child_name = paste0("notification_", data_one_child$placingWay.code)
    }
    
    #keeping only required columns
    keep <- c("notificationNumber", "versionNumber", "createDate", "placingWay.code", "placingWay.name", 
              "order.placer.regNum", "lots.lot.products.product.code", 
              "lots.lot.customerRequirements.customerRequirement.maxPrice", "child_name")
    col_left <- c(which(colnames(data_one_child) %in% keep)) #accounting for the case we have other columns or not enough of them
    
    data_one_child <- data_one_child[, ..col_left]
    
    return(data_one_child)
  }
}
data_extraction(10000, working_dir_2) #check whether it works


#5.2
zips_to_loop <- list.files(working_dir_1)
zips_to_loop <- zips_to_loop[str_detect(zips_to_loop,".zip")]

list_of_ch1 <- list()
for (i in 1:length(zips_to_loop)) {
  list_of_ch1[i] <- data_extraction(i, working_dir_1) 
}
list_of_ch1

#This loop takes a quite a while to run
zips_to_loop <- list.files(working_dir_2)
zips_to_loop <- zips_to_loop[str_detect(zips_to_loop,".zip")]

list_of_ch2 <- list()
for (i in 1:length(zips_to_loop)) {
  list_of_ch2[i] <- data_extraction(i, working_dir_2) 
}
list_of_ch2

#Now need to merge 2 list ion one

rbind(list_of_ch1, list_of_ch2)
