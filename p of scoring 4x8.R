library(gplots)
library(RColorBrewer)
library(plyr)
library(data.table)
library(arules)
library(tidyverse)

aus_rio1_full =readRDS(file= "aus_rio1_full.rds")
aus_rio2_full =readRDS(file= "aus_rio2_full.rds")
aus_rio3_full =readRDS(file= "aus_rio3_full.rds")
aus_rio4_full =readRDS(file= "aus_rio4_full.rds")
aus_rio5_full =readRDS(file= "aus_rio5_full.rds")
blr_rio1_full =readRDS(file= "blr_rio1_full.rds")
blr_rio2_full =readRDS(file= "blr_rio2_full.rds")
blr_rio3_full =readRDS(file= "blr_rio3_full.rds")
blr_rio4_full =readRDS(file= "blr_rio4_full.rds")
blr_rio5_full =readRDS(file= "blr_rio5_full.rds")
bra_rio1_full =readRDS(file= "bra_rio1_full.rds")
bra_rio2_full =readRDS(file= "bra_rio2_full.rds")
bra_rio3_full =readRDS(file= "bra_rio3_full.rds")
bra_rio4_full =readRDS(file= "bra_rio4_full.rds")
bra_rio5_full =readRDS(file= "bra_rio5_full.rds")
fra_rio1_full =readRDS(file= "fra_rio1_full.rds")
fra_rio2_full =readRDS(file= "fra_rio2_full.rds")
fra_rio3_full =readRDS(file= "fra_rio3_full.rds")
fra_rio4_full =readRDS(file= "fra_rio4_full.rds")
fra_rio5_full =readRDS(file= "fra_rio5_full.rds")
fra_rio6_full =readRDS(file= "fra_rio6_full.rds")
fra_rio7_full =readRDS(file= "fra_rio7_full.rds")
fra_rio8_full =readRDS(file= "fra_rio8_full.rds")
jpn_rio1_full =readRDS(file= "jpn_rio1_full.rds")
jpn_rio2_full =readRDS(file= "jpn_rio2_full.rds")
jpn_rio3_full =readRDS(file= "jpn_rio3_full.rds")
jpn_rio4_full =readRDS(file= "jpn_rio4_full.rds")
jpn_rio5_full =readRDS(file= "jpn_rio5_full.rds")
jpn_rio6_full =readRDS(file= "jpn_rio6_full.rds")
tur_rio1_full =readRDS(file= "tur_rio1_full.rds")
tur_rio2_full =readRDS(file= "tur_rio2_full.rds")
tur_rio3_full =readRDS(file= "tur_rio3_full.rds")
tur_rio4_full =readRDS(file= "tur_rio4_full.rds")
tur_rio5_full =readRDS(file= "tur_rio5_full.rds")
tur_rio6_full =readRDS(file= "tur_rio6_full.rds")
usa_rio1_full =readRDS(file= "usa_rio1_full.rds")
usa_rio2_full =readRDS(file= "usa_rio2_full.rds")
usa_rio3_full =readRDS(file= "usa_rio3_full.rds")
usa_rio4_full =readRDS(file= "usa_rio4_full.rds")
usa_rio5_full =readRDS(file= "usa_rio5_full.rds")
usa_rio6_full =readRDS(file= "usa_rio6_full.rds")
usa_rio7_full =readRDS(file= "usa_rio7_full.rds")
usa_rio8_full =readRDS(file= "usa_rio8_full.rds")

#Entropy of the last 3 seconds before a shot from Rio games
aus_entmatrix_p1_3 = readRDS(file = "aus")
blr_entmatrix_p1_3 = readRDS(file = "blr")
bra_entmatrix_p1_3 = readRDS(file = "bra")
fra_entmatrix_p1_3 = readRDS(file = "fra")
jpn_entmatrix_p1_3 = readRDS(file = "jpn")
tur_entmatrix_p1_3 = readRDS(file = "tur")
usa_entmatrix_p1_3 = readRDS(file = "usa")
#Effectiveness of the last 3 seconds before a shot from Rio games
aus_effmatrix_p1_3 = readRDS(file = "aus_effmatrix_p1_3")
blr_effmatrix_p1_3 = readRDS(file = "blr_effmatrix_p1_3")
bra_effmatrix_p1_3 = readRDS(file = "bra_effmatrix_p1_3")
fra_effmatrix_p1_3 = readRDS(file = "fra_effmatrix_p1_3")
jpn_effmatrix_p1_3 = readRDS(file = "jpn_effmatrix_p1_3")
tur_effmatrix_p1_3 = readRDS(file = "tur_effmatrix_p1_3")
usa_effmatrix_p1_3 = readRDS(file = "usa_effmatrix_p1_3")


#Objects needed for p calculation
delete_rnames = c("Stoppage", "Foul","")
ofb = c(1:7,12,13,18,19,24,25,30,31,36,37,42,43,48,49,54,55:60)

occ_fun_x_four <- 
function(x) {
  x_hat <- floor((x+6.5)/12.5) +1
  return(x_hat)
}

occ_fun_eight <- 
function(y) {
  y_hat <- floor((y-0.25)/11.75) +1
  return(y_hat)
}

index_fun_4x8 <- 
function(x, y) {
  z <- (6*(y-1)) + x
  return(z)
}

p0_fun = function(game){
data= game
traj_vec <- unique(data$RecordID)
short_df = data.frame()
p_3 = data.frame()
p_table = data.frame()
last_label = data.frame()

for (rID in traj_vec){
  #if(data[ data$RecordID==rID, 2] == "1st Quarter"){
  if (length(unique(seq(min(data[ data$RecordID==rID, 5]), max(data[ data$RecordID==rID, 5]),1))) <= 4){
    short_tmp <- data[ data$RecordID==rID, ]
    short_df <- rbind(short_df, short_tmp)
    write.table(short_df, "delete")
    #seperates out the trajectories that are shorter than 4 seconds in length (cant be used in spline function)
  } else {
    my_df <- data[ data$RecordID==rID, ]
    poly_x <- smooth.spline(my_df$Time, my_df$x, df= length(unique(my_df$Time)))
    poly_y <- smooth.spline(my_df$Time, my_df$y, df= length(unique(my_df$Time)))
    pred_time <- seq(min(my_df$Time), max(my_df$Time), 1)
    #Have created the poly function for the traj and created a time ref vector, now two df are created with the new values for x and y
    pred_tmp_x <- data.frame(predict(poly_x, pred_time))
    pred_tmp_y <- data.frame(predict(poly_y, pred_time))
    
    #Generate quantized values for each unique time point (x^ and y^)
    x_hat <- data.frame(sapply(pred_tmp_x$y, occ_fun_x_four))
    y_hat <- data.frame(sapply(pred_tmp_y$y, occ_fun_eight))
    #tmp_vec is a vector containing court locations (z) for the ith trajectory
    tmp_vec <- mapply(index_fun_4x8, x_hat, y_hat)
    #takes the 3rd last z value from tmp_vec and binds them into a vector
    p_tmp <- ifelse((length(tmp_vec) > 4), tmp_vec[length(tmp_vec) ], 0)
    p_3 <- rbind(p_tmp, p_3)
    #adds the 3rd last z vector to a data frame of outcome of each possession
    last_tmp <- tail(my_df[8:11], 1)
    last_label <- rbind(last_tmp, last_label)
    p_table <- cbind(last_label, p_3)
    colnames(p_table) = c("Label1", "Label2", "Label3", "Label4", "z")
  }}#}
return(p_table)
}
#p_fun(game)

aus_group_rio_p = rbind(aus_p1, aus_p2, aus_p3)

pos.p_0_summary_fun = function(combo) {
  
  setDT(combo)
  #Takes "shot" from col 1 and turns it into whatever the results was (Score, miss)
  combo[Label1 == "Shot", Label1 := Label2]
  #If "shot" has ended up in col 2 (becblre there was an on-ball or something), it will replace it with the end result of the play (Score, miss)
  combo[Label2 == "Shot", Label1 := Label3]
  #If "on-ball" occurs at the same time as the play ending outcome, this will replace it with the outcome
  combo[Label1 == "On-ball", Label1 := Label2]
  #If there is a mistake (Shot|TO) where it should be TO, this will fix it
  combo[Label2 == "TO", Label1 := Label2]
  #Same as above
  combo[Label3 == "TO", Label1 := Label3]
  
  #delete out of bounds cells
  combo = combo[ ! combo$z %in% ofb,]
  
  #Reorders the table to show z's as col names and labels as row names with counts of each 
  ptable = data.frame(dcast(combo, Label1 ~ z))
  
  #Rename rows
  rownames(ptable) = ptable[,1]
  ptable = ptable[,-1]
  
  #Remove unwanted rows
  ptable = ptable[ !rownames(ptable) %in% delete_rnames, ]
  
  #Transpose the data frame 
  ptable = data.frame(t(ptable))
  
  #Add the efficiency column and total possessions
  ptable = (mutate(ptable, Possessions = Miss + Score + Shooting.Foul + TO, Eff = ((Score + Shooting.Foul) / (Miss + Score + Shooting.Foul + TO)*100)))
  
  #Drop the "X" from the row names - 
  rownames(ptable) = gsub("X","",rownames(ptable),fixed = TRUE)
  
  #Create a df with 60 cells > merge tmp with p_counts then replace rownames with the 1:60 count in the 1st col. Delete the first 2 cols (not needed) and reorder 
  #This gives a df with all 84 cells with those that have possession values in order and those that dont as all zeros. 
  tmp = data.frame(colID=numeric(60))
  ptable = merge(tmp, ptable, by=0, all=TRUE)
  rownames(ptable) = ptable[,1]
  ptable = ptable[,-1:-2]
  ptable = ptable[order(as.numeric(rownames(ptable))),]
  
  #Make any cells with less than 11 possessions 'NAs' so they are not included in heat map. 
  ptable$Eff[which(ptable$Possessions <5)] <- NA
  
  return(ptable)
}

#Turn the result into a court matrix
aus_effmatrix_p1_3 = matrix(aus_p1_3_summary[,6], nrow=10, ncol=6, byrow=TRUE)

#Set rules for heatmap colours
col_breaks = seq(0,70,2.5)
conf_rules_cols = colorRampPalette(brewer.pal(10, "RdYlGn"))(28)

#Make heatmap
heatmap.2(aus_effmatrix_p1_3, dendrogram = "none", Rowv=F, Colv=F, breaks = col_breaks, col=conf_rules_cols, margins=c(5,35), trace=("none"), density.info = "histogram", keysize = 1, key.title = NA, key.xlab = NA, key.ylab = NA)
