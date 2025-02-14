#This is code to clean up the FracFocus chemical list and run it through the ChemExpo chemical 
#preprocessing script to generate a list to send to Tony for curation 
#Kristin Isaacs 7/2024

library(dplyr)
library(stringr)

#functions-------------------------------------------------------------------------------------*
#Custom version of chemical_equal function from ccdR that maintains the 400 status responses to get suggestions

prepare_word <- function(word){
  # Handle question marks
  split_words <- stringr::str_split(string = word,
                                    pattern = '\\?',
                                    n = 2)[[1]]
  if (length(split_words) == 1){
    temp_word <- urltools::url_encode(split_words[[1]])
  } else {
    if (nchar(split_words[[2]]) == 0){
      temp_word <- urltools::url_encode(split_words[[1]])
    } else {
      temp_word <- paste0(urltools::url_encode(split_words[[1]]),
                          '?',
                          urltools::url_encode(split_words[[2]]),
                          '=')
    }
  }
  
  # Handle other non-alpha-numeric characters
  temp_word <- gsub("%26", "&", temp_word)
  temp_word <- gsub("%23", "#", temp_word)
  return(temp_word)
}

chemical_equal_all = function(word = NULL, API_key = NULL, Server = chemical_api_server) {
  if (is.null(word) || !is.character(word)) {
    stop("Please input a character value for word!")
  }
  else if (is.null(API_key)) {
    if (has_ccte_key()) {
      API_key <- ccte_key()
    }
    else {
      stop("Please input an API_key!")
    }
  }
  word <- prepare_word(word)
  response <- httr::GET(url = paste0(Server, "/search/equal/", 
                                     word), httr::add_headers(.headers = c(`Content-Type` = "application/json", 
                                                                           `x-api-key` = API_key)))
    return(jsonlite::fromJSON(httr::content(response, as = "text")))

  return()
}

get_all_chemdata = function(word_list = NULL, API_key = NULL, rate_limit = 0L) 
{
  if (is.null(API_key) || !is.character(API_key)) {
    stop("Please input a character string containing a valid API key!")
  }
  if (!is.numeric(rate_limit) | (rate_limit < 0)) {
    warning("Setting rate limit to 0 seconds between requests!")
    rate_limit <- 0L
  }
  if (!is.null(word_list)) {
    if (!is.character(word_list) & !all(sapply(word_list, 
                                               is.character))) {
      stop("Please input a character list for word_list!")
    }
    word_list <- unique(word_list)
    results <- lapply(word_list, function(t) {
      Sys.sleep(rate_limit)
      attempt <- tryCatch({
        chemical_equal_all(word = t, API_key = API_key)
      }, error = function(cond) {
        message(t)
        message(cond$message)
        return(NA)
      })
      return(attempt)
    })
    names(results) <- word_list
    return(results)
  }
  else {
    stop("Please input a list of chemical names!")
  }
}


#Read in unique chemical names

chems<-read.csv("L:/Lab/NERL_Isaacs/kki-24-FracFocus/uniqueingredients.csv")

#standardize names
chems$StandardIngredientName<-trimws(tolower(chems$IngredientName))
chems$CASRN<-trimws(tolower(chems$CASNumber))

#Some custom cleaning before sending to Katherine's script

#Standardize spacing and punctuation
chems$StandardIngredientName<-gsub(")  ",")", chems$StandardIngredientName) #do this first 
chems$StandardIngredientName<-gsub(") ",")", chems$StandardIngredientName) #do this first 
chems$StandardIngredientName<-gsub(" ) ",")", chems$StandardIngredientName) #do this first 
chems$StandardIngredientName<-gsub(")",") ", chems$StandardIngredientName) 

chems$StandardIngredientName<-gsub(" \\(","(", chems$StandardIngredientName) #do this first 
chems$StandardIngredientName<-gsub("\\( ","(", chems$StandardIngredientName) #do this first 
chems$StandardIngredientName<-gsub(" \\( ","(", chems$StandardIngredientName) #do this first 
chems$StandardIngredientName<-gsub("\\("," (", chems$StandardIngredientName) 

chems$StandardIngredientName<-gsub(" , ",",", chems$StandardIngredientName) #do this first 
chems$StandardIngredientName<-gsub(" ,",",", chems$StandardIngredientName) #do this first 
chems$StandardIngredientName<-gsub(",  ",",", chems$StandardIngredientName) #do this first 
chems$StandardIngredientName<-gsub(", ",",", chems$StandardIngredientName) #do this first 
chems$StandardIngredientName<-gsub(",",", ", chems$StandardIngredientName) 

chems$StandardIngredientName<-gsub("//.  ",".", chems$StandardIngredientName) #do this first 
chems$StandardIngredientName<-gsub("//. ",".", chems$StandardIngredientName) #do this first 
chems$StandardIngredientName<-gsub("//.",". ", chems$StandardIngredientName) 

chems$StandardIngredientName<-gsub("- ","-", chems$StandardIngredientName) 
chems$StandardIngredientName<-gsub(" - ","-", chems$StandardIngredientName) 
chems$StandardIngredientName<-gsub(" -","-", chems$StandardIngredientName) 
#chems$StandardIngredientName<-gsub("-"," ", chems$StandardIngredientName)

chems$StandardIngredientName<-gsub("&#39", "'", chems$StandardIngredientName) 

chems$StandardIngredientName<-gsub("\"","", chems$StandardIngredientName)
chems$StandardIngredientName<-gsub("//*","", chems$StandardIngredientName)


#At this point, run chems through katherine's chemical cleaning script.

#make into format expected by python cleaning script
newchemstocurate1<-chems[,c("X","StandardIngredientName","CASRN" )]
colnames(newchemstocurate1)<-c("id","raw_chem_name","raw_cas")
#save as the expected filename
write.csv(newchemstocurate1,"uncurated_chemicals_fracfocus_07092024.csv")

#run the cleaning script
library(reticulate)
#this runs the most recent "uncurated_chemicals" file
py_run_file("clean_chems_FF.py")

#read in cleaning script result MANUALLY CLEANED TWO INSTANCES OF "[[" or "]]" (messes up API call and couldn't quickly ID a regex soln)
newchemstocurate<-read.csv("cleaned_chemicals_for_curation-Jul-10-2024.csv")

library(stringr)
newchemstocurate$chemical_name <- str_replace_all(newchemstocurate$chemical_name,"\\[\\[", "") #fixes two names that include double brackets (messes up API calls)
newchemstocurate$chemical_name <-  str_replace_all(newchemstocurate$chemical_name, "\\]\\]", "") #fixes two names that include double brackets (messes up API calls)

#Now generate unique lists of initial cleaned names and valid CASRN to send to chemistry APIs for curation to DTXSID

#Generate unique list of names; do some more cleaning

uniquenames<-data.frame(unique(newchemstocurate$chemical_name[which(newchemstocurate$chemical_name!="")]))
colnames(uniquenames)<-"chemical_name"
uniquenames$nameid<-1:length(1:length(uniquenames$chemical_name))

#remove anything with "proprietary" or "confidential", etc in name or CASRN
uniquenames<-uniquenames[!grepl("proprietary",uniquenames$chemical_name),]
uniquenames<-uniquenames[!grepl("confid",uniquenames$chemical_name),]
uniquenames<-uniquenames[!grepl("trade ",uniquenames$chemical_name),]
uniquenames<-uniquenames[!grepl("tradesecret",uniquenames$chemical_name),]
uniquenames<-uniquenames[!grepl("supplied by o",uniquenames$chemical_name),]
uniquenames<-uniquenames[!grepl("haz",uniquenames$chemical_name),]
uniquenames<-uniquenames[!grepl("unknown",uniquenames$chemical_name),]
uniquenames<-uniquenames[!grepl("unavailable",uniquenames$chemical_name),]
uniquenames<-uniquenames[!grepl("undisclosed",uniquenames$chemical_name),]
uniquenames<-uniquenames[!grepl("%",uniquenames$chemical_name),] #mixtures or other strings that can't be curated to single chemical

uniquenames<-uniquenames[order(uniquenames$chemical_name),]

#Call CCTE Chemistry APIs to curate chems, including identifying chemical suggestions
library(ccdR) #this is an R package wrapper for the CCTE chemistry, exposure, and hazard APIs

# This calls  Kristin's CCTE API key from the environment;  CESER please request their own if this code is implemented beyond this project
key<-Sys.getenv("APIKEY")

do_name_call<-0
if (do_name_call==1){

#Call the API to curate names
#I broke these out into groups of 200 w/2 sec pause per discussion with Asif
curateddata<-list()
start<-1
batchsize<-200
for (i in 1:length(uniquenames$chemical_name)){
 #cat("/n",i)
 if (i %% batchsize == 0) {
  end<-start+batchsize-1
  chembatch<-uniquenames$chemical_name[start:end]
  nextdata <- get_all_chemdata(API_key=key, word_list = chembatch)
  cat("\n",start)
  start<-i+1  
  curateddata <- c(curateddata, nextdata)
  Sys.sleep(2) #pause for 2 sec
 }
#trailing batch
  if (i  == length(uniquenames$chemical_name) & i %% batchsize != 0) {
    end<-i
    chembatch<-uniquenames$chemical_name[start:end]
    nextdata <- get_all_chemdata(API_key=key, word_list = chembatch)
    curateddata <- c(curateddata, nextdata)
  }
}

#save the curated results
saveRDS(curateddata,"../output/APIchemicalnameresults.rds")
}

curateddata<-readRDS("../output/APIchemicalnameresults.rds")

#parse the API results for names 

checkit = function(list){
  return("dtxsid" %in% names(list))   #check if a chemical was curated
}

k<-unlist(lapply(curateddata, checkit))
k1<-curateddata[k]

for (i in seq_along(k1)){ #could be updated with lapply
  k1[[i]]["cleanname"]<-names(k1)[i]  #this is because the API does update the name formatting a bit; this retains orig name on data frame
  
}

curated_names<-bind_rows(k1)
#if there are two suggested DTSXIDs, take the one with highest curation rank 
#order by cleanname then rank, remove any duplicates (this ensures best curation only retained)
curated_names<-curated_names[order(curated_names$cleanname,curated_names$rank),]
curated_names<-curated_names[!duplicated(curated_names$cleanname),]

#remove some final records that aren't chemicals (this could be done in earlier name cleaning steps but removed here since API calls complete)
blocks<-c("Carrier","Ceramic","Clay","Corn meal","Corn starch","formal")

curated_names<-curated_names[which(!curated_names$searchValue %in% blocks),]
 

k2<-curateddata[!k]

#remove polu\\ydimethyl diallyl ammonium chloride -  the slash impacted API response
k2<-k2[!names(k2)=="polu\\ydimethyl diallyl ammonium chloride"]
names<-names(k2)

#there is prob more efficient way to do the below
suggested_names<-data.frame(matrix(nrow=length(k2),ncol=2))
names(suggested_names)<-c("chemical","suggestions")
suggested_names$chemical<-names
for (i in 1:length(k2)) {
  suggested_names$suggestions[i]<-paste(k2[[i]]$suggestions, collapse = "|")
}

#For now, ignore multiple suggestions or suggestions that are ICHI keys (in the future we could further curate these suggestions)
suggested_names<-suggested_names[which(suggested_names$suggestions!="NA"),]
suggested_names<-suggested_names[!grepl("\\|",suggested_names$suggestions),] #remove chemicals with more than 1 recommendation
suggested_names<-suggested_names[!grepl("\\|",suggested_names$suggestions),] #remove chemicals with more than 1 recommendation
suggested_names<-suggested_names[!str_detect(suggested_names$suggestions,"[[:upper:]]"),] #remove INCHI key recs - these seem to be strange to me


#Get the curations associated with the recommendations
#write for hand curation of suggestions
#write.csv(suggested_names,"../output/suggested_names.csv")

#Acceptance of suggested names were hand curated (could always be updated)

suggested_names_curated<-read.csv("../output/suggested_names_curated.csv")

suggested_names_curated<-suggested_names_curated[which(suggested_names_curated$acceptsuggestion==1),]

unique_suggested_names<-unique(suggested_names$suggestions)
k<-grepl("/",unique_suggested_names)
#remove the 3 with problematic /s. These don't return anything from DSSTox anyway
unique_suggested_names<-unique_suggested_names[!k]

do_suggestions_call<-0

if (do_suggestions_call==1){
  
curatedsuggestions<-list()
start<-1
batchsize<-200
for (i in 1:length(unique_suggested_names)){
  #cat("/n",i)
  if (i %% batchsize == 0) {
    end<-start+batchsize-1
    chembatch<-unique_suggested_names[start:end]
    nextdata <- get_all_chemdata(API_key=key, word_list = chembatch)
    cat("\n",start)
    start<-i+1  
    curatedsuggestions <- c(curatedsuggestions, nextdata)
    Sys.sleep(2) #pause for 2 sec
  }
  #trailing batch
  if (i  == length(unique_suggested_names) & i %% batchsize != 0) {
    end<-i
    chembatch<-unique_suggested_names[start:end]
    nextdata <- get_all_chemdata(API_key=key, word_list = chembatch)
    curatedsuggestions <- c(curatedsuggestions, nextdata)
  }
}
saveRDS(curatedsuggestions,"../output/APIchemicalsuggestionresults.rds")
}

curatedsuggestions<-readRDS("../output/APIchemicalsuggestionresults.rds")

#pull out successfully curated suggestions

k<-unlist(lapply(curatedsuggestions, checkit))

k1<-curatedsuggestions[k]

for (i in seq_along(k1)){ #could be updated with lapply
  k1[[i]]["suggname"]<-names(k1)[i]  #this is because the API does update the name formatting a bit; this retains orig name on data frame

}


final_curated_suggestions<-bind_rows(k1)

#if there are two suggested DTSXIDs, take the one with highest curation rank 
#order by suggname then rank, remove any duplicates (this ensures best curation only retained)
final_curated_suggestions<-final_curated_suggestions[order(final_curated_suggestions$suggname,final_curated_suggestions$rank),]
final_curated_suggestions<-final_curated_suggestions[!duplicated(final_curated_suggestions$suggname),]

#merge back in with "pre-suggestion" names
colnames(suggested_names_curated)[colnames(suggested_names_curated)=="suggestions"]<-"suggname" #rename for merging
suggested_names_curated<-left_join(suggested_names_curated,final_curated_suggestions)

#Curate CASRN

#Generate unique list of CASRN; do some more cleaning

uniqueCAS<-data.frame(unique(newchemstocurate$casrn[which(newchemstocurate$casrn!="")]))
colnames(uniqueCAS)<-"CASRN"
uniqueCAS$casid<-1:length(1:length(uniqueCAS$CASRN))

#remove anything with "proprietary" or "confidential", etc in name or CASRN
uniqueCAS<-uniqueCAS[!grepl("proprietary",uniqueCAS$CASRN),]

uniqueCAS<-uniqueCAS[order(uniqueCAS$CASRN),]

#Call the API to curate CASRN

do_CASRN_call<-0

if (do_CASRN_call==1){
#I broke these out into groups of 200 w/2 sec pause per discussion with Asif
curatedCAS<-list()
start<-1
batchsize<-200
for (i in 1:length(uniqueCAS$CASRN)){
  #cat("/n",i)
  if (i %% batchsize == 0) {
    end<-start+batchsize-1
    chembatch<-uniqueCAS$CASRN[start:end]
    nextdata <- get_all_chemdata(API_key=key, word_list = chembatch)
    cat("\n",start)
    start<-i+1  
    curatedCAS <- c(curatedCAS, nextdata)
    Sys.sleep(2) #pause for 2 sec
  }
  #trailing batch
  if (i  == length(uniqueCAS$CASRN) & i %% batchsize != 0) {
    end<-i
    chembatch<-uniqueCAS$CASRN[start:end]
    nextdata <- get_all_chemdata(API_key=key, word_list = chembatch)
    curatedCAS <- c(curatedCAS, nextdata)
  }
}

saveRDS(curatedCAS,"../output/APICASresults.rds")
}

curatedCAS<-readRDS("../output/APICASresults.rds")

#pull out successfully curated suggestions

k<-unlist(lapply(curatedCAS, checkit))

k1<-curatedCAS[k]

for (i in seq_along(k1)){ #could be updated with lapply
  k1[[i]]["cleancas"]<-names(k1)[i]  #this is because the API might update the CAS formatting a bit; this retains orig CAS on data frame
}

final_curated_CAS<-bind_rows(k1)
#if there are two suggested DTSXIDs, take the one with highest curation rank 
#order by cleanname then rank, remove any duplicates (this ensures best curation only retained)
final_curated_CAS<-final_curated_CAS[order(final_curated_CAS$cleancas,final_curated_CAS$rank),]
final_curated_CAS<-final_curated_CAS[!duplicated(final_curated_CAS$cleancas),]


#Combine the curated names and curated suggestions, retaining the link between the suggestion and the reported name.
#reported_name is cleaned reported name from frac focus
#suggested_name is name suggested by chemical API for clean reported name.If a suggested name exists, the DTXSID is associated with it.
curated_names$suggested_name<-"" #these are names that were curated successfully by original cleaned name

#now rename the DTXSID and preferred name columns so we can ulimately merge with the cleaned CAS data
#dropped the preferred casrn for now; we can always add metadata for the final DTXSID back in at the end
colnames(curated_names)[colnames(curated_names)=="cleanname"]<-"clean_name" #just clean up labeling for consistency
colnames(curated_names)[colnames(curated_names)=="dtxsid"]<-"dtxsid_by_name"
colnames(curated_names)[colnames(curated_names)=="preferredName"]<-"preferredName_by_name"
curated_names<-curated_names[,c("clean_name","suggested_name","dtxsid_by_name","preferredName_by_name")]

suggested_names_curated$suggested_name<-suggested_names_curated$suggname #these are curated chemicals that had to use suggestions
suggested_names_curated$clean_name<-suggested_names_curated$chemical
colnames(suggested_names_curated)[colnames(suggested_names_curated)=="dtxsid"]<-"dtxsid_by_name"
colnames(suggested_names_curated)[colnames(suggested_names_curated)=="preferredName"]<-"preferredName_by_name"
suggested_names_curated<-suggested_names_curated[,c("clean_name","suggested_name","dtxsid_by_name","preferredName_by_name")]

all_curated_chemicals_by_name<-bind_rows(curated_names,suggested_names_curated)

##Rename the rows for the "by CAS" curations
colnames(final_curated_CAS)[colnames(final_curated_CAS)=="cleancas"]<-"clean_casrn" #just clean up labeling for consistency
colnames(final_curated_CAS)[colnames(final_curated_CAS)=="dtxsid"]<-"dtxsid_by_casrn"
colnames(final_curated_CAS)[colnames(final_curated_CAS)=="preferredName"]<-"preferredName_by_casrn"
final_curated_CAS<-final_curated_CAS[,c("clean_casrn","dtxsid_by_casrn","preferredName_by_casrn")]

#now merge in the curations by name and CAS with the original output of the python cleaning
#change some names to match final curated data
colnames(newchemstocurate)[colnames(newchemstocurate)=="chemical_name"]<-"clean_name"
colnames(newchemstocurate)[colnames(newchemstocurate)=="casrn"]<-"clean_casrn"

#merge the curations by name
cleaned_curated_data<-left_join(newchemstocurate,all_curated_chemicals_by_name)

#merge the curations by CAS
cleaned_curated_data<-left_join(cleaned_curated_data,final_curated_CAS)

#add columns for DTXSID clash
cleaned_curated_data$conflict<-0
k<-is.na(cleaned_curated_data$dtxsid_by_casrn)
j<-is.na(cleaned_curated_data$dtxsid_by_name)
cleaned_curated_data$conflict[which(!j & !k & cleaned_curated_data$dtxsid_by_name!=cleaned_curated_data$dtxsid_by_casrn)]<-1

#save final cleaned and curated chemistry data
saveRDS(cleaned_curated_data,"../output/cleaned_curated_chemical_data.RDS")

