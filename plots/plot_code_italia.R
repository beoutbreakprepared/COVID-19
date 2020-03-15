#italian github
italian_list<-read.csv("C:/Users/pigottdm/Documents/GitHub/COVID-19-ITA/dati-regioni/dpc-covid19-ita-regioni.csv")

unique_locations<-"Lombardia"

for(i in 1:length(unique_locations)){
  #subset to the specific location
  subset_location<-subset(italian_list, italian_list$denominazione_regione == unique_locations)
}

#assume that each field is dependent
#deaths and recovered are invariant

#test of closed system
subset_location$totale_casi[20]
subset_location$totale_ospedalizzati[20]+subset_location$isolamento_domiciliare[20]+
  subset_location$deceduti[20]+subset_location$dimessi_guariti[20]

subset_location$totale_ospedalizzati
subset_location$ricoverati_con_sintomi+subset_location$terapia_intensiva

#so on any given day, totale_attualmente_positivi represents all of the cases in the system

unique_locations<-unique(italian_list$denominazione_regione)
par(mar=c(5,4,4,2))
for(i in 1:length(unique_locations)){
  #subset to the specific location
  subset_location<-subset(italian_list, italian_list$denominazione_regione == unique_locations[i])
  #note skipping the first day as the list doesn't start with the first cases
  ratio_home<-NA
  ratio_hospital_nonICU<-NA
  ratio_hospital_ICU<-NA
  for(j in 2:nrow(subset_location)){
    ratio_home[j]<-subset_location$isolamento_domiciliare[j]/subset_location$totale_attualmente_positivi[j]
    ratio_hospital_nonICU[j]<-subset_location$ricoverati_con_sintomi[j]/subset_location$totale_attualmente_positivi[j]
    ratio_hospital_ICU[j]<-subset_location$terapia_intensiva[j]/subset_location$totale_attualmente_positivi[j]
  }

  
  pdf(file = paste0("C:/Users/pigottdm/Documents/GitHub/COVID-19-ITA/plots/hosp_ratios_",
                    unique_locations[i],
                    "_",
                    substr(subset_location$data[max(nrow(subset_location))],1,10),
                    ".pdf"))
  
  plot(seq(1,nrow(subset_location),1), ratio_home, type = 'l', ylim = c(0:1), axes = FALSE,
       main = paste0(unique_locations[i]," (on ",
                     substr(subset_location$data[max(nrow(subset_location))],1,10),
                     " ",
                     subset_location$totale_ospedalizzati[max(nrow(subset_location))],
                     " hospitalized)"), xlab="", ylab="", col="darkgreen", xaxs="i",yaxs = "i")
  axis(side = 1,
       at = seq(0,nrow(subset_location),1),
       labels = c(0,substr(subset_location$data,1,10)),
       las = 2,
       cex.axis = 0.75)
  axis(side = 2,
       at = seq(0,1,0.1),
       labels = seq(0,1,0.1),
       las = 2,
       cex.axis = 0.75)
  lines(seq(1,nrow(subset_location),1), ratio_hospital_ICU, col = "red")
  lines(seq(1,nrow(subset_location),1), ratio_hospital_nonICU, col = "gold")
  legend(x = max(nrow(subset_location))-6, y = 1,
         legend = c("At Home", "Non-ICU hospitalized", "ICU"),
         fill = c("darkgreen", "yellow", "red"))
  dev.off()
}
##########################################################################################
# plot ratio where x axis is time since first death (cumulatives)
#########################################################################################
unique_locations<-unique(italian_list$denominazione_regione)
#unique_locations<-"Lombardia"
par(mar=c(5,4,4,2))

for(i in 1:length(unique_locations)){
  #subset to the specific location
  subset_location<-subset(italian_list, italian_list$denominazione_regione == unique_locations[i])
  #subset to only look at days post-death
  subset_location<-subset(subset_location, subset_location$deceduti>0)
  
  if(nrow(subset_location)<1){
    print(paste0("No deaths in ", unique_locations[i]))
  }else{
  #note skipping the first day as the list doesn't start with the first cases
  ratio_death_nonICU<-NA
  ratio_death_ICU<-NA
  for(j in 2:nrow(subset_location)){
    ratio_death_nonICU[j]<-subset_location$ricoverati_con_sintomi[j]/subset_location$deceduti[j]
    ratio_death_ICU[j]<-subset_location$terapia_intensiva[j]/subset_location$deceduti[j]
  }
  
  
  pdf(file = paste0("C:/Users/pigottdm/Documents/GitHub/COVID-19-ITA/plots/death_hosp_ratios_",
                    unique_locations[i],
                    "_",
                    substr(subset_location$data[max(nrow(subset_location))],1,10),
                    ".pdf"))
  
  plot(seq(1,nrow(subset_location),1), ratio_death_nonICU, type = 'l', ylim= c(0,max(na.omit(ratio_death_nonICU+1))), axes = FALSE,
       main = paste0(unique_locations[i]," (on ",
                     substr(subset_location$data[max(nrow(subset_location))],1,10),
                     " ",
                     subset_location$deceduti[max(nrow(subset_location))],
                     " deaths)"), xlab="", ylab="", col="gold", xaxs="i",yaxs = "i")
  axis(side = 1,
       at = seq(0,nrow(subset_location),1),
       labels = c(0,substr(subset_location$data,1,10)),
       las = 2,
       cex.axis = 0.75)
  axis(side = 2,
       at = seq(0,max(na.omit(ratio_death_nonICU+1)),1),
       labels = seq(0,max(na.omit(ratio_death_nonICU+1)),1),
       las = 2,
       cex.axis = 0.75)
  lines(seq(1,nrow(subset_location),1), ratio_death_ICU, col = "red")

  legend(x = max(nrow(subset_location))-(max(nrow(subset_location))/6), y = max(na.omit(ratio_death_nonICU+1)),
         legend = c("Non-ICU:Death", "ICU:Death"),
         fill = c("yellow", "red"))
  dev.off()
}
}
######################################################################################
# Same idea, but days since first death all on one plot (cumulatives)
#######################################################################################
unique_locations<-unique(italian_list$denominazione_regione)
#unique_locations<-"Lombardia"
par(mar=c(5,4,4,2))

plot(seq(1,length(unique(italian_list$data)),1), rep(0,length(unique(italian_list$data))), type = 'l', ylim= c(0,12), axes = FALSE,
     main = paste0("Daily ICU to death ratios [since day of first death or 24th Feb]"), xlab="Days", ylab="Ratio", col="black", xaxs="i",yaxs = "i")
axis(side = 1,
     at = seq(0,length(unique(italian_list$data)),1),
     labels = c("Prior to series", seq(1,length(unique(italian_list$data)),1)),
     las = 2,
     cex.axis = 0.75)
axis(side = 2,
     at = seq(0,12,1),
     labels = seq(0,12,1),
     las = 1,
     cex.axis = 0.75)


for(i in 1:length(unique_locations)){
  #subset to the specific location
  subset_location<-subset(italian_list, italian_list$denominazione_regione == unique_locations[i])
  #subset to only look at days post-death
  subset_location<-subset(subset_location, subset_location$deceduti>0)
  
  if(nrow(subset_location)<1){
    print(paste0("No deaths in ", unique_locations[i]))
  }else{
    #note skipping the first day as the list doesn't start with the first cases
    # ratio_death_nonICU<-NA
    ratio_death_ICU<-NA
    for(j in 1:nrow(subset_location)){
      # ratio_death_nonICU[j]<-subset_location$ricoverati_con_sintomi[j]/subset_location$deceduti[j]
      ratio_death_ICU[j]<-subset_location$terapia_intensiva[j]/subset_location$deceduti[j]
    }
  
    lines(seq(1,length(ratio_death_ICU),1), ratio_death_ICU, col = "red")
    
    }
}
###############################################################################################
unique_locations<-unique(italian_list$denominazione_regione)
#unique_locations<-"Lombardia"
par(mar=c(5,4,4,2))

plot(seq(1,length(unique(italian_list$data)),1), rep(0,length(unique(italian_list$data))), type = 'l', ylim= c(0,30), axes = FALSE,
     main = paste0("Daily non-ICU to death ratios [since day of first death or 24th Feb]"), xlab="Days", ylab="Ratio", col="black", xaxs="i",yaxs = "i")
axis(side = 1,
     at = seq(0,length(unique(italian_list$data)),1),
     labels = c("Prior to series", seq(1,length(unique(italian_list$data)),1)),
     las = 2,
     cex.axis = 0.75)
axis(side = 2,
     at = seq(0,30,1),
     labels = seq(0,30,1),
     las = 1,
     cex.axis = 0.75)


for(i in 1:length(unique_locations)){
  #subset to the specific location
  subset_location<-subset(italian_list, italian_list$denominazione_regione == unique_locations[i])
  #subset to only look at days post-death
  subset_location<-subset(subset_location, subset_location$deceduti>0)
  
  if(nrow(subset_location)<1){
    print(paste0("No deaths in ", unique_locations[i]))
  }else{
    #note skipping the first day as the list doesn't start with the first cases
    # ratio_death_nonICU<-NA
    ratio_death_nonICU<-NA
    for(j in 1:nrow(subset_location)){
      # ratio_death_nonICU[j]<-subset_location$ricoverati_con_sintomi[j]/subset_location$deceduti[j]
      ratio_death_nonICU[j]<-subset_location$ricoverati_con_sintomi[j]/subset_location$deceduti[j]
    }
    
    lines(seq(1,length(ratio_death_nonICU),1), ratio_death_nonICU, col = "gold")
    
  }
}
###############################################################################################################
# All Italy version (ratios are cumulatives)
###############################################################################################################
unique_dates<-unique(italian_list$data)
#unique_locations<-"Lombardia"
par(mar=c(5,4,4,2))


ratio_death_ICU<-NA
ratio_death_nonICU<-NA

for(i in 1:length(unique_dates)){
  #subset to the specific location
  subset_date<-subset(italian_list, italian_list$data == unique_dates[i])
  
  ratio_death_ICU[i]<-sum(subset_date$terapia_intensiva)/sum(subset_date$deceduti)
  ratio_death_nonICU[i]<-sum(subset_date$ricoverati_con_sintomi)/sum(subset_date$deceduti)
  
  }
plot(seq(1,length(unique(italian_list$data)),1), rep(0,length(unique(italian_list$data))), type = 'l', ylim= c(0,20), axes = FALSE,
     main = paste0("Daily hospitalization to death ratios [since day of first death or 24th Feb]"), xlab="", ylab="Ratio", col="black", xaxs="i",yaxs = "i")
axis(side = 1,
     at = seq(0,length(unique(italian_list$data)),1),
     labels = c(0,substr(subset_location$data,1,10)),
     las = 2,
     cex.axis = 0.75)
axis(side = 2,
     at = seq(0,20,1),
     labels = seq(0,20,1),
     las = 1,
     cex.axis = 0.75)

lines(seq(1,length(unique_dates),1), ratio_death_ICU, col = "red")
lines(seq(1,length(unique_dates),1), ratio_death_nonICU, col = "gold")

legend(x = length(unique(italian_list$data))-6, y = 20,
       legend = c("Non-ICU:Death", "ICU:Death"),
       fill = c("yellow", "red"))

##########################################################################################
# plot ratio where x axis is time since first death (daily delta deaths)
# unresolved issues with NAs in i = 16
#########################################################################################
unique_locations<-unique(italian_list$denominazione_regione)
#unique_locations<-"Lombardia"
par(mar=c(5,4,4,2))

for(i in 1:length(unique_locations)){
  #subset to the specific location
  subset_location<-subset(italian_list, italian_list$denominazione_regione == unique_locations[i])
  #subset to only look at days post-death
  subset_location<-subset(subset_location, subset_location$deceduti>0)
  
  if(nrow(subset_location)<1){
    print(paste0("No deaths in ", unique_locations[i]))
  }else{
    #note skipping the first day as the list doesn't start with the first cases
    ratio_death_nonICU<-NA
    ratio_death_ICU<-NA
    for(j in 2:nrow(subset_location)){
      
      if((subset_location$deceduti[j]-subset_location$deceduti[j-1])==0){
        ratio_death_nonICU[j]<-NA
        ratio_death_ICU[j]<-NA
      }else{
      ratio_death_nonICU[j]<-subset_location$ricoverati_con_sintomi[j]/(subset_location$deceduti[j]-subset_location$deceduti[j-1])
      ratio_death_ICU[j]<-subset_location$terapia_intensiva[j]/(subset_location$deceduti[j]-subset_location$deceduti[j-1])
    }
    }
    
    # pdf(file = paste0("C:/Users/pigottdm/Documents/GitHub/COVID-19-ITA/plots/incident_death_hosp_ratios_",
    #                   unique_locations[i],
    #                   "_",
    #                   substr(subset_location$data[max(nrow(subset_location))],1,10),
    #                   ".pdf"))
    
    plot(seq(1,nrow(subset_location),1), ratio_death_nonICU, type = 'l', ylim= c(0,max(na.omit(ratio_death_nonICU+1))), axes = FALSE,
         main = paste0(unique_locations[i]," (on ",
                       substr(subset_location$data[max(nrow(subset_location))],1,10),
                       " ",
                       subset_location$deceduti[max(nrow(subset_location))],
                       " deaths)"), xlab="", ylab="", col="gold", xaxs="i",yaxs = "i")
    axis(side = 1,
         at = seq(0,nrow(subset_location),1),
         labels = c(0,substr(subset_location$data,1,10)),
         las = 2,
         cex.axis = 0.75)
    axis(side = 2,
         at = seq(0,max(na.omit(ratio_death_nonICU+1)),1),
         labels = seq(0,max(na.omit(ratio_death_nonICU+1)),1),
         las = 2,
         cex.axis = 0.75)
    lines(seq(1,nrow(subset_location),1), ratio_death_ICU, col = "red")
    
    legend(x = max(nrow(subset_location))-(max(nrow(subset_location))/6), y = max(na.omit(ratio_death_nonICU+1)),
           legend = c("Non-ICU:Death", "ICU:Death"),
           fill = c("yellow", "red"))
    #dev.off()
  }
}
###############################################################################################################
# All Italy version (ratios are populations over daily deaths)
###############################################################################################################
all_italian_list<-read.csv("C:/Users/pigottdm/Documents/GitHub/COVID-19-ITA/dati-andamento-nazionale/dpc-covid19-ita-andamento-nazionale.csv")

unique_dates<-unique(all_italian_list$data)
#unique_locations<-"Lombardia"
par(mar=c(5,4,4,2))

ratio_death_ICU<-NA
ratio_death_nonICU<-NA
ratio_death_hospitalized<-NA

for(i in 2:length(unique_dates)){
  
  ratio_death_ICU[i]<-all_italian_list$terapia_intensiva[i]/(all_italian_list$deceduti[i]-all_italian_list$deceduti[i-1])
  ratio_death_nonICU[i]<-all_italian_list$ricoverati_con_sintomi[i]/(all_italian_list$deceduti[i]-all_italian_list$deceduti[i-1])
  ratio_death_hospitalized[i]<-all_italian_list$totale_ospedalizzati[i]/(all_italian_list$deceduti[i]-all_italian_list$deceduti[i-1])
}
plot(seq(1,length(unique(all_italian_list$data)),1), rep(0,length(unique(all_italian_list$data))), type = 'l', ylim= c(0,160), axes = FALSE,
     main = paste0("Daily in-hospital population to incident death ratios"), xlab="", ylab="Ratio", col="black", xaxs="i",yaxs = "i")
axis(side = 1,
     at = seq(0,length(unique(all_italian_list$data)),1),
     labels = c(0,substr(unique(all_italian_list$data),1,10)),
     las = 2,
     cex.axis = 0.75)
axis(side = 2,
     at = seq(0,160,10),
     labels = seq(0,160,10),
     las = 1,
     cex.axis = 0.75)

lines(seq(1,length(unique_dates),1), ratio_death_ICU, col = "red")
lines(seq(1,length(unique_dates),1), ratio_death_nonICU, col = "gold")
lines(seq(1,length(unique_dates),1), ratio_death_hospitalized, col = "blue")

legend(x = length(unique(all_italian_list$data))-6, y = 160,
       legend = c("Non-ICU:Death", "ICU:Death", "All Hospitalized:Death"),
       fill = c("gold", "red", "blue"))