#A place to store various plotting procedures

#Needs to be adjusted to facet wrap and to take the new-format data
stacked_bar <- function(prepped_data, xaxisname, fillname) {
    data <- prepped_data
    #Change the names so that they'll render nicely
    data$Fill <- unlist(lapply(strwrap(data$Fill, width=25, simplify=FALSE), 
                               paste, collapse="\n"))
    data$Facet <- unlist(lapply(strwrap(data$Facet, width=18, simplify=FALSE), 
                                paste, collapse="\n"))
    # data$X <- unlist(lapply(strwrap(data$X, width=20, simplify=FALSE), 
                            # paste, collapse="\n"))
    data$X <- gsub("\\s*\\([^\\)]+\\)","", data$X)
    
    #Make the main plot
    p <- ggplot(data, aes(x = X, y = Number, fill = Fill)) + 
        geom_bar(width = .9, stat="identity", position="stack") + 
        
        #Nice colors
        scale_fill_brewer(palette="Set1", name=fillname) + 
        
        #Labels
        xlab(xaxisname) + 
        ylab("Number of murders") + 
        labs(title = "Murders by Circumstance \n and Weapon") + 
        
        #Prettifying
        theme(plot.title = element_text(hjust = 0.5, size=18))+
        theme(legend.position="right", legend.direction="vertical")+
        theme(axis.text.x=element_text(angle=50, hjust=.9))+
        
        #Faceting
        facet_grid(.~Facet, scales="free")
    
    return(p)
}