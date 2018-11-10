rm(list=ls())
library(xml2)
library(data.table)
library(stringr)

#Define the working directory:
working_dir<-"/Users/PatratskyAlexander/Desktop/ICEF\ 3rd\ Year/Data\ Science/hw1p2.zip"

#Read the file directly from the zip:
y<-list_of_xmls[1]
conn<-unz(x,y)
data_raw<-read_xml(conn,encoding = "UTF-8")