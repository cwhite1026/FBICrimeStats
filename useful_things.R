#Subset and reformat data so the plotter knows what to do with it
prep_data <- function(dataset, facet_column, facet_values, 
                      x_column, x_values, fill_column, fill_values){
    #Standardize the format of the data set so it can be plotted with the same function
    
    #Mask down to just the values that we want to use for facet and x axis
    mask <- dataset[,facet_column] %in% facet_values
    mask <- mask & (dataset[,x_column] %in% x_values)
    small_dataset <- dataset[mask,]
    
    #Make sure the levels match the unique values and don't have other things
    small_dataset[,facet_column] <- as.factor(as.character(small_dataset[,facet_column]))
    small_dataset[,x_column] <- as.factor(as.character(small_dataset[,x_column]))
    #We're going to be changing the levels for the fill, so keep it as a character
    small_dataset[,fill_column] <- as.character(small_dataset[,fill_column])
    
    #Remove all the rows that have "Total" in the fill value column
    fill_is_total <- grepl("[T|t]otal", small_dataset[,fill_column])
    small_dataset <- small_dataset[!fill_is_total,]
    
    #Grab the fill values that will be included in the "Other" column
    other_fill_values <- levels(dataset[,fill_column])
    other_fill_value_mask <- !(other_fill_values %in% fill_values)
    other_fill_values <- other_fill_values[other_fill_value_mask]
    
    #If we have anything that needs to be put into the "Other" category, do that
    if (length(other_fill_values) > 0){
        #turn into data.table, make new column for end class
        small_dataset <- data.table(small_dataset)
        # expr <- parse(text = paste0(cn, ":=mean(a)"))
        expr <- paste0(fill_column, " := ifelse(", fill_column,"%in% fill_values, ",fill_column,", 'Other')")
        expr <- parse(text = expr)
        small_dataset[, eval(expr)]#ifelse(fill_column %in% fill_values, fill_column, "Other")]
        
        #Sum up the "Other" category
        expr <- paste0("Number ~ ", fill_column,  " + ", facet_column, " + ", x_column)
        expr <- parse(text = expr)
        small_dataset <- aggregate(eval(expr), data = small_dataset, FUN=sum)
    }
    
    #Rename the columns and make them factors so we don't have to pass 
    #a ton of stuff to the plotting routine
    colnames <- names(small_dataset)
    end_names <- c("Fill", "Facet", "X")
    cols_to_rename <- c(fill_column, facet_column, x_column)
    for (i in seq_along(end_names)){
        colnames[colnames == cols_to_rename[i]] <- end_names[i]
        small_dataset[,cols_to_rename[i]] <- as.factor(small_dataset[,cols_to_rename[i]])
    }
    names(small_dataset) <- colnames
    small_dataset$Fill<- relevel(small_dataset$Fill, "Other")
    # print(small_dataset)
    return(small_dataset)
}
