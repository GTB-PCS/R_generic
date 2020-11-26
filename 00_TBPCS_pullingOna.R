######################################################
# SETTINGS
#
# You need to install a version of Java that has the Java Cryptography Extensions - see instructions here https://github.com/chrissyhroberts/ODK_TRAINING/blob/master/User_Guide.md
# URL of aggregate server [must include top level directory]
# This script requires one package dependency from package "getPass". This will be installed automatically if you don't have it.
#                 i.e. "https://test.odk.lshtm.ac.uk/test"
# USER NAME
#                 i.e. "chrissy"
# DECRYPTION PEM LOCATION : Please enter the full path to the decryption key
#                 i.e. "D:/keys/PRIVATE.KEY.pem  (PC)
#                 i.e. "/Volumes/POOKY/PRIVATE.KEY.pem  (UNIX/LINUX/MAC)
# OUTPUT DIRECTORY
#
#				to send to a specific folder use i.e. "output"
#				to send to a timestamped folder use "export.dir.name<-format(Sys.time(), "%y%m%d.%H%M ")"


################################################################################################################
# USER MODIFIABLE SETTINGS
################################################################################################################

setwd <- function(dir) {
  if (missing(dir) || is.null(dir) || dir == "") {
    dir <- "C:/Users/Takuya"
  }
  base::setwd(dir)
}

setwd()

setwd("C:/Users/Takuya/Dropbox/")
setwd("./10.GTB/Working folder Taku-Nobu/R_generic_tzn_based/pull")

library("getPass")
library("anytime")
library("tidyverse")
library("plotKML")
library("sp")
library("rgdal")
library("lubridate")

# set up ODK briefcase in your working directory *you need to run this only once
# download.file(url = "https://github.com/opendatakit/briefcase/releases/download/v1.10.1/ODK-Briefcase-v1.10.1.jar",
#              destfile = paste( "./odkbriefcase.jar",sep = ""), method="curl", mode = "wb",extra="-L")


#server settings
pemlocation<-    NULL
urllocation<-     "https://ona.io/gtbtme"
user <- "tyamanaka"

#directories
export.dir.name <- "./output"
report.dir.name <- "./report"
storage.dir.name <- "./XMLS"


#update software
update.briefcase<-FALSE # keep this as False since the script for this does not work currently

# add list of forms to pull in the line below (comma separated in double quotes)

######################################################

#purge : delete all CSV data prior to run (will remove previous CSV files)
#autodelete : delete data after run (will delete CSV files after report made)
#deletexmlpre : delete previously pulled XML files prior to new pull
#deletexmlpost : delete previously pulled XML files after new pull

purge<-FALSE
autodelete<-FALSE
deletexmlpre<-FALSE
deletexmlpost<-FALSE

####deleteXMLS prior
if(deletexmlpre==T){
  unlink(x = "XMLS",recursive = T)
}

################################################################################################################
####ODK Briefcase Call Function
################################################################################################################

#function to pull data from ODK Aggregate Server
################################################################################################################

odk.briefcase.pull<-
  function(
    aggregate.url=NULL,            #arg  -url   <url>
    odk.directory=NULL,           #arg         <path/to/dir>
    odk.username="admin",         #arg  -u     <username>
    form.id=NULL,                 #arg  -id    <form_id>
    export.dir=".",               #arg  -ed    <path/to/dir>
    storage.dir=".",              #arg  -sd    <path/to/dir>
    export.start.date=NULL,       #arg  -start <yyyy/MM/dd>
    export.end.date=NULL,         #arg -end   <yyyy/MM/dd>
    pem_file=NULL,                #arg -pf    <path/to/file.pem>
    exclude.media.export=FALSE,   #flag -em
    overwrite.csv.export=TRUE,   #flag -oc
    update.odk.briefcase=FALSE
  ){
    
    
    
    ################################################################################################################
    # Purge CSVs and delete XMLS if needed
    ############################################################################################################    
    
    if(deletexmlpre==T){
      unlink(x = storage.dir,recursive = T)
    }
    if(purge==TRUE)
    {
      unlink(x = export.dir.name,recursive = T)
    }
    
    
    
    
    ################################################################################################################
    # INSTALL REQUIRED PACKAGES    
    ############################################################################################################    
    message("installing packages if needed")
    #check if getPass is installed and install it if not
    message("checking for new packages")
    list.of.packages <- c("getPass","anytime","tidyverse","plotKML","sp","rgdal","lubridate")
    
    new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
    if(length(new.packages))
    {
      install.packages(new.packages)
      message (paste("Installing ",new.packages))
    }
    
    ################################################################################################################
    #check if update.odk.briefcase flag was active (default) and update odk briefcase
    ################################################################################################################
    # this section does not work... currently
    message("Checking to see if Briefcase should be updated.")
    if(update.odk.briefcase==T)
      
    {
      message("Updating ODK Briefcase")
      system("curl -L  https://github.com/opendatakit/briefcase/releases/download/v1.10.1/ODK-Briefcase-v1.10.1.jar > ./odkbriefcase.jar")
    }
    
    
    ################################################################################################################    
    #check that only online or offline is being used
    ############################################################################################################
    if (!is.null(aggregate.url) & !is.null(odk.directory))
    {
      message("Please only specify one of aggregate.url and odk.directory")
    }
    
    
    
    
    ################################################################################################################    
    #add flags for command line call
    ###############################################################################################################
    if(!is.null(aggregate.url)){aggregate.url<-paste("-url ", aggregate.url, sep="")}            #arg  -url   <url>
    if(!is.null(odk.directory)){odk.directory<-paste("-od ", odk.directory, sep="")}
    odk.username<-paste("-u ", odk.username, sep="")
    export.dir<-paste("-ed ", export.dir, sep="")
    storage.dir<-paste("-sd ", storage.dir, sep="")
    if(!is.null(export.start.date)){export.start.date<-paste("-start ", export.start.date, sep="")}
    if(!is.null(export.end.date)){export.end.date<-paste("-end ", export.end.date, sep="")}
    if(!is.null(pem_file)){pem_file<-paste("-pf ", pem_file, sep="")}
    if(exclude.media.export==T){exclude.media.export<-"-em"}else{exclude.media.export<-""}
    if(overwrite.csv.export==T){overwrite.csv.export<-"-oc"}else{overwrite.csv.export<-""}
    
    ################################################################################################################
    #Get Password for current user
    ################################################################################################################    
    
    password<- getPass(msg = "Enter Password for server")
    password<- paste("-p ",password,sep="")
    
    
    ################################################################################################################
    #Generate command for system call to odk briefcase for each form in form.id
    ################################################################################################################    
    
    for(p in form.id)
    {
      
      export.filename<-paste(p,".csv",sep="")
      if(!is.null(export.filename)){export.filename<-paste("-f ", export.filename, sep="")}
      form.id2<-paste("-id ", p, sep="")
      
      command<-paste("java -jar odkbriefcase.jar ",
                     aggregate.url,
                     odk.directory,
                     odk.username,
                     form.id2,
                     export.dir,
                     export.start.date,
                     export.end.date,
                     export.filename,
                     storage.dir,
                     pem_file,
                     exclude.media.export,
                     overwrite.csv.export,
                     password,
                     sep=" ")
      
      system(command)
    }
    if(autodelete==TRUE){
      unlink(x = export.dir.name,recursive = T)
    }
    
    ####deleteXMLS
    if(deletexmlpost==TRUE){
      unlink(x = "XMLS",recursive = T)
    }
  }

################################################################################################################    
################################################################################################################    
# END OF FUNCTION
################################################################################################################    
################################################################################################################


################################################################################################################
#create directories if needed
################################################################################################################
if(!file.exists(export.dir.name)){dir.create(export.dir.name)}
if(!file.exists(report.dir.name)){dir.create(report.dir.name)}
if(!file.exists(storage.dir.name)){dir.create(storage.dir.name)}

################################################################################################################
#### Run Call to ODK Briefcase
################################################################################################################
# 
formstopull<-c("tanzania7"
               )

odk.briefcase.pull(aggregate.url = urllocation, 
                   odk.username = user,
                   form.id = formstopull,
                   export.dir = export.dir.name,
                   storage.dir = storage.dir.name,
                   exclude.media.export = F,
                   overwrite.csv.export = T,
                   update.odk.briefcase = update.briefcase, 
                   pem_file = pemlocation)
################################################################################################################


################################################################################################################
####GET THE LIST OF ALL FILES###
allcsvs<-list.files(path = export.dir.name,pattern = ".csv",full.names = T)
################################################################################################################