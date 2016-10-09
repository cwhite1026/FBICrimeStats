# Load in and process the FBI files- no reason to clutter up the server.R code

process_single_year_murder_circ_weap <- function(file_name, year){
    #Read in the data and cut off empty columns
    data <- read.xls(file_name)
    data <- data[,1:19]
 
    #Define the names of the columns and rename them 
    colnames <- c("Circumstances", "Total murder victims", "Total firearms", 
                  "Handguns", "Rifles", "Shotguns", "Other guns or type not stated", 
                  "Knives or cutting instruments", 
                  "Blunt objects (clubs, hammers, etc.)", 
                  "Personal weapons (hands, fists, feet, etc.)", "Poison",
                  "Pushed or thrown out window", "Explosives", "Fire", "Narcotics",
                  "Drowning", "Strangulation", "Asphyxiation", "Other")
    colnames <- gsub(" ", "00", colnames)
    names(data) <- colnames
    #Cut to just the data part of the table
    data<- data[4:30,]
    
    #Format the first column nicely
    rownames <- as.character(unlist(data[,colnames[1]], use.names=FALSE))
    rownames <- gsub("^\\s+|\\s+$", "", rownames)
    #We have to distinguish between felony and non-felony "Other" since the tags are the same
    rownames[13] <- "Other or unspecified: Felony"
    rownames[26] <- "Other or unspecified: Non-felony"
    rownames <- gsub("[:|1]$", "", rownames)
    #Replace the current first column with the better format
    data[,colnames[1]] <- rownames
    
    #Now cast the data columns as characters, then remove all commas, then cast as numeric
    for(i in 2:19) {
        tempcol <- as.character(data[,colnames[i]])
        tempcol <- gsub(",", "", tempcol)
        data[,colnames[i]] <- tempcol
    }
    
    #Store a copy of the human-readable version
    readable_data <- data
    names(readable_data) <- gsub("00", " ", names(readable_data))
    
    #Melt so we have tidy data
    data <- gather(data, "Weapon", "Number", Total00murder00victims:Other)
    data[,"Weapon"] <- gsub("00", " ", data[,"Weapon"])
    data <- cbind(data, data.frame(Year = rep(year, length(data[,"Weapon"]))))
    
    return(list(data, readable_data))
}


    
make_murder_circumstance_weapon <- function(){
    #Read in the data and cut off empty columns
    file_name_2015 <- "CIUS2015/Expanded_Homicide_Data_Table_11_Murder_Circumstances_by_Weapon_2015.xls"
    file_name_2014 <- "CIUS2014/Expanded_Homicide_Data_Table_11_Murder_Circumstances_by_Weapon_2014.xls"
    file_name_2013 <- "CIUS2013/Expanded_Homicide_Data_Table_11_Murder_Circumstances_by_Weapon_2013.xls"
    file_name_2012 <- "CIUS2012/Expanded_Homicide_Data_Table_11_Murder_Circumstances_by_Weapon_2012.xls"
    file_name_2011 <- "CIUS2011/Expanded_Homicide_Data_Table_11_Murder_Circumstances_by_Weapon_2011.xls"
    file_name_2010 <- "CIUS2010/10shrtbl11.xls"
    data2015 <- process_single_year_murder_circ_weap(file_name_2015, 2015)
    data2014 <- process_single_year_murder_circ_weap(file_name_2014, 2014)
    data2013 <- process_single_year_murder_circ_weap(file_name_2013, 2013)
    data2012 <- process_single_year_murder_circ_weap(file_name_2012, 2012)
    data2011 <- process_single_year_murder_circ_weap(file_name_2011, 2011)
    data2010 <- process_single_year_murder_circ_weap(file_name_2010, 2010)
    
    #Assemble the human-readable tables into a list
    readable_data <- list(data2010[[2]], data2011[[2]], 
                          data2012[[2]], data2013[[2]], 
                          data2014[[2]], data2015[[2]])
    
    #Put the machine-readable tables together
    aggregate_data <- rbind(data2010[[1]], data2011[[1]], 
                            data2012[[1]], data2013[[1]], 
                            data2014[[1]], data2015[[1]])
    
    #Make character guys factors and the number column numeric
    for (name in names(aggregate_data)){
        if (name != "Number"){
            aggregate_data[,name] <- as.factor(aggregate_data[,name])
        } else {
            aggregate_data[,name] <- as.numeric(aggregate_data[,name])
        }
    }

    return(list(aggregate_data, readable_data))
    
}

# data2015 <-read.xls("CIUS2015/Expanded_Homicide_Data_Table_11_Murder_Circumstances_by_Weapon_2015.xls")
# data2015 <- data2015[,1:19]
# 
# data2014 <- read.xls("CIUS2014/Expanded_Homicide_Data_Table_11_Murder_Circumstances_by_Weapon_2014.xls")
# data2014 <- data2014[,1:19]
# 
# #Define the names of the columns and rename them 
# colnames <- c("Circumstances", "Total murder victims", "Total firearms", 
#               "Handguns", "Rifles", "Shotguns", "Other guns or type not stated", 
#               "Knives or cutting instruments", 
#               "Blunt objects (clubs, hammers, etc.)", 
#               "Personal weapons (hands, fists, feet, etc.)", "Poison",
#               "Pushed or thrown out window", "Explosives", "Fire", "Narcotics",
#               "Drowning", "Strangulation", "Asphyxiation", "Other")
# colnames <- gsub(" ", "00", colnames)
# names(data2015) <- colnames
# #Cut to just the data part of the table
# data2015<- data2015[4:30,]
# 
# #Format the first column nicely
# rownames <- as.character(unlist(data2015[,colnames[1]], use.names=FALSE))
# rownames <- gsub("^\\s+|\\s+$", "", rownames)
# #We have to distinguish between felony and non-felony "Other" since the tags are the same
# rownames[13] <- "Other or unspecified: Felony"
# rownames[26] <- "Other or unspecified: Non-felony"
# rownames <- gsub(":$", "", rownames)
# #Replace the current first column with the better format
# data2015[,colnames[1]] <- rownames
# 
# #Now cast the data columns as characters, then remove all commas, then cast as numeric
# for(i in 2:19) {
#     tempcol <- as.character(data2015[,colnames[i]])
#     tempcol <- gsub(",", "", tempcol)
#     data2015[,colnames[i]] <- tempcol
# }
# 
# #Store a copy of the human-readable version
# readable_data2015 <- data2015
# names(readable_data2015) <- gsub("00", " ", names(readable_data2015))
# 
# #Melt so we have tidy data
# data2015 <- gather(data2015, "Weapon", "Number", Total00murder00victims:Other)
# data2015[,"Weapon"] <- gsub("00", " ", data2015[,"Weapon"])
# data2015 <- cbind(data2015, data.frame(Year = rep(2015, length(data2015[,"Weapon"]))))


#===================================================================================
#These are things that I want to include eventually, but for now I don't need them. 


# 
# make_agg_assault_by_weapon <- function() {
#   
#   #Read in the Excel files
#   agg_assault_by_weapon2014 <-read.xls("CIUS2014/Aggravated_Assault_Table_Aggravated_Assault_Types_of_Weapons_Used_Percent_Distribution_by_Region_2014.xls")
#   agg_assault_by_weapon2013 <-read.xls("CIUS2013/Aggravated_Assault_Table_Aggravated_Assault_Types_of_Weapons_Used_Percent_Distribution_by_Region_2013.xls")
#   agg_assault_by_weapon2012 <- read.xls("CIUS2012/Tables/Aggravated_Assault_Table_Aggravated_Assault_Types_of_Weapons_Used_Percent_Distributioin_by_Region_2012.xls")
#   agg_assault_by_weapon2011 <- read.xls("CIUS2011/CIUS2011datatables/aggravated-assault-table2011.xls")
#   agg_assault_by_weapon2010 <- read.xls("CIUS2010/Excel/10aggvtbl.xls")
# 
#   #Rename the columns- they are the same for all years
#   colnames <- unlist(agg_assault_by_weapon2014[3,], use.names = FALSE)
#   colnames <- as.character(colnames)
#   colnames <- gsub("[,\ ]?\n\ ?", ".", colnames)
#   colnames <- gsub(",?\ |,", ".", colnames)
#   names(agg_assault_by_weapon2014) <- colnames
#   names(agg_assault_by_weapon2013) <- colnames
#   names(agg_assault_by_weapon2012) <- colnames
#   names(agg_assault_by_weapon2011) <- colnames
#   names(agg_assault_by_weapon2010) <- colnames
# 
#   #Subset to just the rows with the data- cuts off some superfluous
#   #header stuff at the top and a comment at the bottom
#   agg_assault_by_weapon2014 <- data.table(agg_assault_by_weapon2014[4:8,])
#   agg_assault_by_weapon2013 <- data.table(agg_assault_by_weapon2013[4:8,])
#   agg_assault_by_weapon2012 <- data.table(agg_assault_by_weapon2012[4:8,])
#   agg_assault_by_weapon2011 <- data.table(agg_assault_by_weapon2011[4:8,])
#   agg_assault_by_weapon2010 <- data.table(agg_assault_by_weapon2010[4:8,])
#   
#   #Combine the different years into a single table
#   agg_assault_by_weapon2014[,Year:=2014]
#   agg_assault_by_weapon2013[,Year:=2013]
#   agg_assault_by_weapon2012[,Year:=2012]
#   agg_assault_by_weapon2011[,Year:=2011]
#   agg_assault_by_weapon2010[,Year:=2010]
#   agg_assault_by_weapon <- rbind(agg_assault_by_weapon2014, 
#                                  agg_assault_by_weapon2013,
#                                  agg_assault_by_weapon2012,
#                                  agg_assault_by_weapon2011,
#                                  agg_assault_by_weapon2010)
#   
#   #Clean up the by-year ones
#   rm(list = c("agg_assault_by_weapon2010", "agg_assault_by_weapon2011",
#               "agg_assault_by_weapon2012", "agg_assault_by_weapon2013",
#               "agg_assault_by_weapon2014"))
#   
#   #Make the data tidy- gather and then get rid of totals, which are all 100
#   agg_assault_by_weapon <- gather(data = agg_assault_by_weapon, 
#                                   key="Weapon", value= "Percentage", 
#                                   -Region, -Year)
#   not_totals <- agg_assault_by_weapon$Weapon!="Total.all.weapons1"
#   agg_assault_by_weapon <- agg_assault_by_weapon[not_totals,]
#   
#   #Make sure all the variables are the right format
#   agg_assault_by_weapon <- agg_assault_by_weapon %>%
#     mutate(Percentage=as.numeric(Percentage)) %>%
#     mutate(Year=as.factor(Year)) %>%
#     data.table()
#   agg_assault_by_weapon[, Region := factor(Region, levels=c("Total","Northeast",
#                                                             "Midwest","South", "West"))]
# 
#   return(agg_assault_by_weapon)
# }
# 
# #==================================================================
# 
# make_homicide_by_weapon <- function(){
#   #Read the file in and cut off empty columns at the right
#   homicide_by_weapon2014 <- read.xls("CIUS2014/Expanded_Homicide_Data_Table_7_Murder_Types_of_Weapons_Used_Percent_Distribution_by_Region_2014.xls")
#   homicide_by_weapon2014 <- homicide_by_weapon2014[,1:6]
#   homicide_by_weapon2013 <- read.xls("CIUS2013/Expanded_Homicide_Data_Table_7_Murder_Types_of_Weapons_Used_Percent_Distribution_by_Region_2013.xls")
#   homicide_by_weapon2013 <- homicide_by_weapon2013[,1:6]
#   homicide_by_weapon2012 <- read.xls("CIUS2012/Tables/Expanded_Homicide_Data_Table_7_Murder_Types_of_Weapons_Used_Percent_Distribution_by_Region_2012.xls")
#   homicide_by_weapon2012 <- homicide_by_weapon2012[,1:6]
#   homicide_by_weapon2011 <- read.xls("CIUS2011/CIUS2011datatables/SHR Tables 2011/Expanded_Homicide_Data_Table_7_Murder_Types_of_Weapons_Used_Percent_Distribution_by_Region_2011.xls")
#   homicide_by_weapon2011 <- homicide_by_weapon2011[,1:6]
#   homicide_by_weapon2010 <- read.xls("CIUS2010/Excel/10shrtbl07.xls")
#   homicide_by_weapon2010 <- homicide_by_weapon2010[,1:6]
#   
#   #Rename the columns- all the columns match up across years
#   colnames <- unlist(homicide_by_weapon2014[3,], use.names = FALSE)
#   colnames <- as.character(colnames)
#   colnames <- gsub("[,\ ]?\n\ ?", ".", colnames)
#   colnames <- gsub(",?\ |,", ".", colnames)
#   names(homicide_by_weapon2014) <- colnames
#   names(homicide_by_weapon2013) <- colnames
#   names(homicide_by_weapon2012) <- colnames
#   names(homicide_by_weapon2011) <- colnames
#   names(homicide_by_weapon2010) <- colnames
#   
#   #Cut off header and comment at the end, convert to data.table
#   homicide_by_weapon2014 <- data.table(homicide_by_weapon2014[4:8,])
#   homicide_by_weapon2013 <- data.table(homicide_by_weapon2013[4:8,])
#   homicide_by_weapon2012 <- data.table(homicide_by_weapon2012[4:8,])
#   homicide_by_weapon2011 <- data.table(homicide_by_weapon2011[4:8,])
#   homicide_by_weapon2010 <- data.table(homicide_by_weapon2010[4:8,])
#   
#   #Combine the data into one large table
#   homicide_by_weapon2014[, Year:=2014]
#   homicide_by_weapon2013[, Year:=2013]
#   homicide_by_weapon2012[, Year:=2012]
#   homicide_by_weapon2011[, Year:=2011]
#   homicide_by_weapon2010[, Year:=2010]
#   homicide_by_weapon <- rbind(homicide_by_weapon2014,
#                               homicide_by_weapon2013,
#                               homicide_by_weapon2012,
#                               homicide_by_weapon2011,
#                               homicide_by_weapon2010)
#   
#   #Clean up the unneeded variables
#   rm(list = c("homicide_by_weapon2014", "homicide_by_weapon2013",
#               "homicide_by_weapon2012", "homicide_by_weapon2011",
#               "homicide_by_weapon2010"))
#   
#   #Make the data tidy- gather and then get rid of totals, which are all 100
#   homicide_by_weapon <- gather(data = homicide_by_weapon, 
#                                key="Weapon", value= "Percentage", 
#                                -Region, -Year)
#   not_totals <- homicide_by_weapon$Weapon!="Total.all.weapons2"
#   homicide_by_weapon <- homicide_by_weapon[not_totals,]
#   
#   #Make sure all the variables are the right format
#   homicide_by_weapon <- homicide_by_weapon %>%
#     mutate(Percentage=as.numeric(Percentage)) %>%
#     mutate(Year=as.factor(Year)) %>%
#     data.table()
#   homicide_by_weapon[, Region := factor(Region, levels=c("Total","Northeast",
#                                                          "Midwest","South", "West"))]
#   return(homicide_by_weapon)
# }
# 
# #==================================================================
# 
# make_robbery_by_weapon <- function(){
#   #Read the data
#   robbery_by_weapon2014 <- read.xls("CIUS2014/Robbery_Table_3_Robbery_Types_of_Weapons_Used_Percent_Distribution_by_Region_2014.xls")
#   robbery_by_weapon2013 <- read.xls("CIUS2013/Robbery_Table_3_Robbery_Types_of_Weapons_Used_Percent_Distribution_by_Region_2013.xls")
#   robbery_by_weapon2012 <- read.xls("CIUS2012/Tables/Robbery_Table_3_Robbery_Types_of_Weapons_Used_Percent_Distribution_by_Region_2012.xls")
#   robbery_by_weapon2011 <- read.xls("CIUS2011/CIUS2011datatables/robbery-table-3.xls")
#   robbery_by_weapon2010 <- read.xls("CIUS2010/Excel/10robtbl3.xls")
#   
#   #Rename the columns.  Note that there are two main classes, armed and strong-arm,
#   #and the column labels are split over two rows so they need to be merged.
#   #All years have identical labels
#   colnames1 <- unlist(robbery_by_weapon2014[3,], use.names = FALSE)
#   colnames1 <- as.character(colnames1)
#   colnames1[colnames1==""] <- "Armed"
#   colnames2 <- unlist(robbery_by_weapon2014[4,], use.names = FALSE)
#   colnames <- paste(colnames1, colnames2)
#   colnames <- gsub("[,\ ]?\n\ ?", ".", colnames)
#   colnames <- gsub(",?\ |,", ".", colnames)
#   names(robbery_by_weapon2014) <- colnames
#   names(robbery_by_weapon2013) <- colnames
#   names(robbery_by_weapon2012) <- colnames
#   names(robbery_by_weapon2011) <- colnames
#   names(robbery_by_weapon2010) <- colnames
#   
#   #Cut off the header and footer
#   robbery_by_weapon2014 <- data.table(robbery_by_weapon2014[5:9,])
#   robbery_by_weapon2013 <- data.table(robbery_by_weapon2013[5:9,])
#   robbery_by_weapon2012 <- data.table(robbery_by_weapon2012[5:9,])
#   robbery_by_weapon2011 <- data.table(robbery_by_weapon2011[5:9,])
#   robbery_by_weapon2010 <- data.table(robbery_by_weapon2010[5:9,])
#   
#   #Combine the years into one data table
#   robbery_by_weapon2014[,Year:=2014]
#   robbery_by_weapon2013[,Year:=2013]
#   robbery_by_weapon2012[,Year:=2012]
#   robbery_by_weapon2011[,Year:=2011]
#   robbery_by_weapon2010[,Year:=2010]
#   robbery_by_weapon <- rbind(robbery_by_weapon2014, robbery_by_weapon2013,
#                              robbery_by_weapon2012, robbery_by_weapon2011,
#                              robbery_by_weapon2010)
#   
#   #Clean up unneeded data frames
#   rm(list = c("robbery_by_weapon2014", "robbery_by_weapon2013",
#               "robbery_by_weapon2012", "robbery_by_weapon2011",
#               "robbery_by_weapon2010"))
#   
#   #Make the data tidy- gather and then get rid of totals, which are all 100
#   robbery_by_weapon <- gather(data = robbery_by_weapon, 
#                               key="Weapon", value= "Percentage", 
#                               -Region., -Year)
#   not_totals <- robbery_by_weapon$Weapon!="Total.all.weapons1."
#   robbery_by_weapon <- robbery_by_weapon[not_totals,]
#   
#   #Make sure all the variables are the right format
#   robbery_by_weapon <- robbery_by_weapon %>%
#     mutate(Percentage=as.numeric(Percentage)) %>%
#     mutate(Year=as.factor(Year)) %>%
#     rename(Region = Region.) %>%
#     data.table()
#   robbery_by_weapon[, Region := factor(Region, levels=c("Total","Northeast",
#                                                         "Midwest","South", "West"))]
#   return(robbery_by_weapon)
# }
# 
# #==================================================================
# 
# make_justifiable_by_weapon <- function(){
#   #Read the tables and crop extra columns
#   justifiable_by_weapon_law_enforcement <- read.xls("CIUS2014/Expanded_Homicide_Data_Table_14_Justifiable_Homicide_by_Weapon_Law_Enforcement_2010-2014.xls")
#   justifiable_by_weapon_law_enforcement <- justifiable_by_weapon_law_enforcement[,1:10]
#   justifiable_by_weapon_citizens <- read.xls("CIUS2014/Expanded_Homicide_Data_Table_15_Justifiable_Homicide_by_Weapon_Private_Citizen_2010-2014.xls")
#   justifiable_by_weapon_citizens <- justifiable_by_weapon_citizens[,1:10  ]
#   
#   #Rename the columns- both have the same column names
#   colnames <- unlist(justifiable_by_weapon_law_enforcement[3,], use.names = FALSE)
#   colnames <- as.character(colnames)
#   colnames <- gsub("[,\ ]?\n\ ?", ".", colnames)
#   colnames <- gsub(",?\ |,", ".", colnames)
#   names(justifiable_by_weapon_citizens) <- colnames
#   names(justifiable_by_weapon_law_enforcement) <- colnames
#   
#   #Cut off the header and footer for each
#   justifiable_by_weapon_citizens <- data.table(justifiable_by_weapon_citizens[4:8,])
#   justifiable_by_weapon_law_enforcement <- data.table(justifiable_by_weapon_law_enforcement[4:8,])
#   
#   #Merge the two 
#   justifiable_by_weapon_law_enforcement[, Perpetrator:= "Law Enforcement"]
#   justifiable_by_weapon_citizens[, Perpetrator:="Private Citizen"]
#   justifiable_by_weapon <- rbind(justifiable_by_weapon_law_enforcement, 
#                                  justifiable_by_weapon_citizens)
#   
#   #Clean up
#   rm(list = c("justifiable_by_weapon_law_enforcement",
#               "justifiable_by_weapon_citizens"))
#   return(justifiable_by_weapon)
# }
# 
# #==================================================================
# 
# 
# make_murder_by_weapon <- function(){
#   #Read the table and cut off an empty column
#   murders_by_weapon2014 <- read.xls("CIUS2014/Expanded_Homicide_Data_Table_11_Murder_Circumstances_by_Weapon_2014.xls")
#   murders_by_weapon2014 <- murders_by_weapon2014[,1:19]
#   murders_by_weapon2013 <- read.xls("CIUS2013/Expanded_Homicide_Data_Table_11_Murder_Circumstances_by_Weapon_2013.xls")
#   murders_by_weapon2013 <- murders_by_weapon2013[,1:19]
#   murders_by_weapon2012 <- read.xls("CIUS2012/Tables/Expanded_Homicide_Data_Table_11_Murder_Circumstances_by_Weapon_2012.xls")
#   murders_by_weapon2012 <- murders_by_weapon2012[,1:19]
#   murders_by_weapon2011 <- read.xls("CIUS2011/CIUS2011datatables/SHR Tables 2011/Expanded_Homicide_Data_Table_11_Murder_Circumstances_by_Weapon_2011.xls")
#   murders_by_weapon2011 <- murders_by_weapon2011[,1:19]
#   murders_by_weapon2010 <- read.xls("CIUS2010/Excel/10shrtbl11.xls")
#   murders_by_weapon2010 <- murders_by_weapon2010[,1:19]
#   
#   #Rename the columns- all of the years have the same names
#   colnames <- unlist(murders_by_weapon2014[3,], use.names = FALSE)
#   colnames <- as.character(colnames)
#   colnames <- gsub("[,\ ]?\n\ ?", ".", colnames)
#   colnames <- gsub(",?\ |,", ".", colnames)
#   names(murders_by_weapon2014) <- colnames
#   names(murders_by_weapon2013) <- colnames
#   names(murders_by_weapon2012) <- colnames
#   names(murders_by_weapon2011) <- colnames
#   names(murders_by_weapon2010) <- colnames
#   
#   #Only take the body of the table
#   murders_by_weapon2014 <-  data.table(murders_by_weapon2014[4:30,])
#   murders_by_weapon2013 <-  data.table(murders_by_weapon2013[4:30,])
#   murders_by_weapon2012 <-  data.table(murders_by_weapon2012[4:30,])
#   murders_by_weapon2011 <-  data.table(murders_by_weapon2011[4:30,])
#   murders_by_weapon2010 <-  data.table(murders_by_weapon2010[4:30,])
#   
#   #Merge into one table
#   murders_by_weapon2014[, Year := 2014]
#   murders_by_weapon2013[, Year := 2013]
#   murders_by_weapon2012[, Year := 2012]
#   murders_by_weapon2011[, Year := 2011]
#   murders_by_weapon2010[, Year := 2010]
#   murder_circumstance_by_weapon <- rbind(murders_by_weapon2014, 
#                                          murders_by_weapon2013,
#                                          murders_by_weapon2012, 
#                                          murders_by_weapon2011,
#                                          murders_by_weapon2010)
#   
#   #Clean up
#   rm(list = c("murders_by_weapon2014", "murders_by_weapon2013",
#               "murders_by_weapon2012", "murders_by_weapon2011",
#               "murders_by_weapon2010"))
#   return(murder_circumstance_by_weapon)
# }