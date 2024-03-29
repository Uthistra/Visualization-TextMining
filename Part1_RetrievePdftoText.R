###################### Part 1 - Retriving Texts from pdf and save into a vector ###########

#####################################Library Installation
#install.packages("pdftools") 
#install.packages("png")
#####################################Library Loading
library(httr)
library(stringi)
library(pdftools)
library(tidyverse)

#####################################To Select the given GRIexcel.Rds file
filename <- file.choose()
rds_data <- readRDS(filename)
#head(rds_data)

##################################### Define the Sector and One drive path in the Local Pc

SECTOR <- "Telecommunications"
DRIVE_PDF_PATH <- "C:/Users/Uthistra/OneDrive/New/OneDrive/Telecommunications"

##################################### retrieve 'Telecommunications' data and omit NA values 

sectorlydata <- rds_data[rds_data$Sector == SECTOR & !is.na(rds_data$`Report Pdf Address`),]
sectorlydata

################# create directory to save the pdf retrieved in the Local Machine

dir.create("files")
setwd("./files")

################# Retrieve all the files in the DRIVE_PDF_PATH
file_list <- list.files(DRIVE_PDF_PATH)
file_list

################# Read all the data saved to the 'sectorlydata' dataframe
for (row in 1:nrow(sectorlydata)) {
 
  #Read and save each files company , year and filename into variables
  company <- sectorlydata[row, "Name"]
  year <- sectorlydata[row, "Publication Year"]
  filename <- paste(stri_replace_all_fixed(company, " ", ""), "_", year, ".pdf", sep = "")


############# Check iteratively the filename exist in the created 'file_list' dataframe 
  if (filename %in% file_list) {
    print(paste("Processing...", filename))
    
############# If file exist copy the relvant file synced from one drive to the Local Drive
    file.copy(paste(DRIVE_PDF_PATH, filename, sep = "/"), ".")
    
############ Read the Pdf text in each pdf and save it into the "TextForAnalyze" vector
    tryCatch({
      text <- pdf_text(filename)
      data <- toString(text)
      sectorlydata[row, "TextForAnalyze"] <- data

    

    }, error = function(e) {
      
############# Avoid and identify corrupted file using Try Catch statement
      print(paste("File", filename, "is corrupted"))
    })

  }
}

########################## Finally save the 'sectorlydata' dataframe to finalDataset.RDS file for the plotting

saveRDS(sectorlydata, file = "finalDataset.RDS") 
########################## Reset the current working Directory

# setwd("E:/DalarnaUni/Data Visualization/Home Assignment") 
# getwd() # get the Current Working Directory path
# write.csv(sectorlydata, "TelecommunicationSubset.csv") - To Write to Csv




