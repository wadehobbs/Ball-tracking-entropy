#This code takes the raw data input and outputs an array of occupancy used to caluculate entropy
#Ensure pacakges are enabled
#For new games, get shot possessions only
data = training_set
shot_vec <- data[ which(apply(data, 1, function(r) any(r %in% c("Shot")))),4]
shot_vec <- as.vector(shot_vec$RecordID)
my_df <- data.frame()
tmp <- data.frame()
for(rID in shot_vec){ 
  tmp <- data[ data$RecordID==rID, ]
  my_df <- rbind(my_df, tmp)
}
training_set_shots = my_df

#functions
xy_extract_fun_5x10 <- function(z){
       row <- floor(z/7) +1
       col <- z - (7*(row-1))
       xy <- list(row, col)
       return(xy)
}

index_fun_5x10 <- function(x, y) {
       z <- (7*(y-1)) + x
       return(z)
}

occ_fun_x_five <- function(x) {
       x_hat <- floor((x+4)/10) +1
       return(x_hat)
}

occ_fun_y_ten <- function(y) {
       y_hat <- floor((y-2)/9.4) +1
       return(y_hat)
}

#NOW A FUNCTION CALLED:
ent_last_4sec_fun_5x10()

inbounds_ref5x10 <- c(9:13,16:20,23:27,30:34, 37:41, 44:48, 51:55, 58:62, 65:69, 72:76)
inbounds_row5x10 <- c(2:11)
inbounds_col5x5 <- c(2:6)

ent_last_4sec_fun <- function(x) {
  data = all_data_miss
  short_df <- data.frame()
  short_tmp <- data.frame()
  occ_array <- array(0, dim=c(12,7,84))
  traj_vec <- unique(data$RecordID)
  #Start of for loop - takes the raw data in and goes through each trajectory, applying following code to each individual trajectory
  for (rID in traj_vec){
    #if(data[ data$RecordID==rID, 2] == "4th Quarter"){
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
      # rel_x <- rbind(rel_x, pred_tmp_x)
      # rel_y <- rbind(rel_y, pred_tmp_y)
      # rel_time <- rbind(rel_time, rel_x, rel_y)
      
      #Generate quantized values for each unique time point (x^ and y^)   
      x_hat <- data.frame(sapply(pred_tmp_x$y, occ_fun_x_five))
      y_hat <- data.frame(sapply(pred_tmp_y$y, occ_fun_y_ten))
      #tmp_vec is a vector containing court locations (z) for the ith trajectory
      tmp_vec <- mapply(index_fun_5x10, x_hat, y_hat)
      #Cuts whole possession down to the last 4 seconds or whatever number i specifiy and reverses cell order so the occ_array is filled for the cell that the shot was taken in. 
      tmp_vec <- rev(tail(tmp_vec, 5))
      #Checks if any trajectoies go out of bounds (some will start ofb but need to fix if they go ofb during possession)
      #ifelse(ofb4x8 %in% tmp_vec, print(rID), next)
      
      xy <- matrix(unlist(xy_extract_fun_5x10(tmp_vec)), ncol=2)
      #xy is a matrix of x and y grid locations for each of the values in the sliding window   
      x <- xy[,1]
      y <- xy[,2]
      
      my_matrix <- matrix(0, nrow=12, ncol=7)
      
      
      my_matrix[x[2],y[2]] = my_matrix[x[2],y[2]]+1
      my_matrix[x[3],y[3]] = my_matrix[x[3],y[3]]+1
      my_matrix[x[4],y[4]] = my_matrix[x[4],y[4]]+1
      #adds a 1 to the matrix of the court in the cells that the ball has passed through    
      occ_array[,,tmp_vec[1]] = occ_array[,,tmp_vec[1]] + my_matrix
    }}
  return(occ_array)
}




# usario1_lastfour_occ = ent_last_4sec_fun(usag11)
# usario2_lastfour_occ = ent_last_4sec_fun(usag4)
# usario3_lastfour_occ = ent_last_4sec_fun(usag9)
# usario4_lastfour_occ = ent_last_4sec_fun(usag3)
# usario5_lastfour_occ = ent_last_4sec_fun(usag12)
# usario6_lastfour_occ = ent_last_4sec_fun(usag8)
# usario7_lastfour_occ = ent_last_4sec_fun(usag7)
# usario8_lastfour_occ = ent_last_4sec_fun(usag5)
# 
# turrio_last4_groupgames = turrio1_lastfour_occ + turrio2_lastfour_occ + turrio3_lastfour_occ + turrio4_lastfour_occ + turrio5_lastfour_occ 

training_set_occ4x8 = occ_array[inbounds_row,inbounds_col,inbounds_4x8_ref]
miss_set_occ5x10_5sec = occ_array[inbounds_row5x10,inbounds_col5x10,inbounds_ref5x10]
training_set_occ6x12 = occ_array[inbounds_row6x12,inbounds_col6x12,inbounds_ref6x12]
training_set_occ7x14 = occ_array[inbounds_row7x14,inbounds_col7x14,inbounds_ref7x14]

#calculation and viz of entropy (full court)
occ_array <- miss_set_occ5x10_5sec
shan_ent <- apply(occ_array, c(3), entropy)
shan_matrix <- matrix(shan_ent, nrow=5, ncol=5, byrow=TRUE)
shan_matrix[is.na(shan_matrix)] <- 0
turrio_last4ent_groupmatrix = shan_matrix
turrio_last4ent_groupmatrix = turrio_last4ent_groupmatrix[1:3,]
turrio_last4ent_groupmatrix[3,1] = NA
turrio_last4ent_groupmatrix[3,4] = NA

heatmap.2(turrio_last4ent_matrix, dendrogram = "none", Rowv=F, Colv=F, col= my_palette_red_green, margins=c(5,35), trace=("none"), density.info = "histogram", keysize = 1, key.title = NA, key.xlab = NA, key.ylab = NA)


usa_total_ent_vec = as.vector(t(usa_total_ent))
ausrio_last4_allgames = ausrio_last4_allgames[inbounds_4x8_ref]


#6x12
index_fun_6x12 <- function(x, y) {
  z <- (8*(y-1)) + x
  return(z)
}

occ_fun_x_six <- function(x) {
  x_hat <- floor((x+2.3333333333333333333333)/8.3333333333333333333333) +1
  return(x_hat)
}

occ_fun_y_twelve <- function(y) {
  y_hat <- floor((y-4.16666666666666666666666)/7.83333333333333333333333) +1
  return(y_hat)
}

xy_extract_fun_6x12 <- function(z){
  row <- floor(z/8) +1
  col <- z - (8*(row-1))
  xy <- list(row, col)
  return(xy)
}

data = training_set_shots
short_df <- data.frame()
short_tmp <- data.frame()
occ_array <- array(0, dim=c(14,8,112))
traj_vec <- unique(data$RecordID)
#Start of for loop - takes the raw data in and goes through each trajectory, applying following code to each individual trajectory
for (rID in traj_vec){
  #if(data[ data$RecordID==rID, 2] == "4th Quarter"){
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
    # rel_x <- rbind(rel_x, pred_tmp_x)
    # rel_y <- rbind(rel_y, pred_tmp_y)
    # rel_time <- rbind(rel_time, rel_x, rel_y)
    
    #Generate quantized values for each unique time point (x^ and y^)   
    x_hat <- data.frame(sapply(pred_tmp_x$y, occ_fun_x_six))
    y_hat <- data.frame(sapply(pred_tmp_y$y, occ_fun_y_twelve))
    #tmp_vec is a vector containing court locations (z) for the ith trajectory
    tmp_vec <- mapply(index_fun_6x12, x_hat, y_hat)
    #Cuts whole possession down to the last 4 seconds or whatever number i specifiy and reverses cell order so the occ_array is filled for the cell that the shot was taken in. 
    tmp_vec <- rev(tail(tmp_vec, 4))
    #Checks if any trajectoies go out of bounds (some will start ofb but need to fix if they go ofb during possession)
    #ifelse(ofb4x8 %in% tmp_vec, print(rID), next)
    
    xy <- matrix(unlist(xy_extract_fun_6x12(tmp_vec)), ncol=2)
    #xy is a matrix of x and y grid locations for each of the values in the sliding window   
    x <- xy[,1]
    y <- xy[,2]
    
    my_matrix <- matrix(0, nrow=14, ncol=8)
    
    
    my_matrix[x[2],y[2]] = my_matrix[x[2],y[2]]+1
    my_matrix[x[3],y[3]] = my_matrix[x[3],y[3]]+1
    my_matrix[x[4],y[4]] = my_matrix[x[4],y[4]]+1
    #adds a 1 to the matrix of the court in the cells that the ball has passed through    
    occ_array[,,tmp_vec[1]] = occ_array[,,tmp_vec[1]] + my_matrix
  }}

#7x14
index_fun_7x14 <- function(x, y) {
  z <- (9*(y-1)) + x
  return(z)
}

occ_fun_x_seven <- function(x) {
  x_hat <- floor((x+1.142857)/7.142857) +1
  return(x_hat)
}

occ_fun_y_fourteen <- function(y) {
  y_hat <- floor((y-5.2857142857)/6.7142857142857) +1
  return(y_hat)
}

xy_extract_fun_7x14 <- function(z){
  row <- floor(z/9) +1
  col <- z - (9*(row-1))
  xy <- list(row, col)
  return(xy)
}

data = training_set_shots
short_df <- data.frame()
short_tmp <- data.frame()
occ_array <- array(0, dim=c(16,9,144))
traj_vec <- unique(data$RecordID)
#Start of for loop - takes the raw data in and goes through each trajectory, applying following code to each individual trajectory
for (rID in traj_vec){
  #if(data[ data$RecordID==rID, 2] == "4th Quarter"){
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
    # rel_x <- rbind(rel_x, pred_tmp_x)
    # rel_y <- rbind(rel_y, pred_tmp_y)
    # rel_time <- rbind(rel_time, rel_x, rel_y)
    
    #Generate quantized values for each unique time point (x^ and y^)   
    x_hat <- data.frame(sapply(pred_tmp_x$y, occ_fun_x_seven))
    y_hat <- data.frame(sapply(pred_tmp_y$y, occ_fun_y_fourteen))
    #tmp_vec is a vector containing court locations (z) for the ith trajectory
    tmp_vec <- mapply(index_fun_7x14, x_hat, y_hat)
    #Cuts whole possession down to the last 4 seconds or whatever number i specifiy and reverses cell order so the occ_array is filled for the cell that the shot was taken in. 
    tmp_vec <- rev(tail(tmp_vec, 4))
    #Checks if any trajectoies go out of bounds (some will start ofb but need to fix if they go ofb during possession)
    #ifelse(ofb4x8 %in% tmp_vec, print(rID), next)
    
    xy <- matrix(unlist(xy_extract_fun_7x14(tmp_vec)), ncol=2)
    #xy is a matrix of x and y grid locations for each of the values in the sliding window   
    x <- xy[,1]
    y <- xy[,2]
    
    my_matrix <- matrix(0, nrow=16, ncol=9)
    
    
    my_matrix[x[2],y[2]] = my_matrix[x[2],y[2]]+1
    my_matrix[x[3],y[3]] = my_matrix[x[3],y[3]]+1
    my_matrix[x[4],y[4]] = my_matrix[x[4],y[4]]+1
    #adds a 1 to the matrix of the court in the cells that the ball has passed through    
    occ_array[,,tmp_vec[1]] = occ_array[,,tmp_vec[1]] + my_matrix
  }}


#Polar cells

xy_extract_fun_pol <- function(curstates) {
  if(curstates == 1){
    x <- 1
    y <- 1
    xy <- list(x,y)
  } else if (curstates == 2) {
    x <- 2
    y <- 1
    xy <- list(x,y)
  } else if (curstates == 3) {
    x <- 3
    y <- 1
    xy <- list(x,y)
  } else if (curstates == 4) {
    x <- 4
    y <- 1
    xy <- list(x,y)
  } else if (curstates == 5) {
    x <- 5
    y <- 2
    xy <- list(x,y)
  } else if (curstates == 6) {
    x <- 1
    y <- 2
    xy <- list(x,y)
  } else if (curstates == 7) {
    x <- 2
    y <- 2
    xy <- list(x,y)
  } else if (curstates == 8) {
    x <- 3
    y <- 2
    xy <- list(x,y)
  } else if (curstates == 9) {
    x <- 4
    y <- 1
    xy <- list(x,y)
  } else if (curstates == 10) {
    x <- 5
    y <- 2
    xy <- list(x,y)
  } else if (curstates == 11) {
    x <- 1
    y <- 3
    xy <- list(x,y)
  } else if (curstates == 12) {
    x <- 2
    y <- 3
    xy <- list(x,y)
  } else if (curstates == 13) {
    x <- 3
    y <- 3
    xy <- list(x,y)
  } else if (curstates == 14) {
    x <- 4
    y <- 3
    xy <- list(x,y)
  } else if (curstates == 15) {
    x <- 5
    y <- 3
    xy <- list(x,y)
  } else if (curstates == 16) {
    x <- 2
    y <- 4
    xy <- list(x,y)
  } else if (curstates == 17) {
    x <- 4
    y <- 4
    xy <- list(x,y)
  } else if (curstates == 18) {
    x <- 3
    y <- 5
    xy <- list(x,y)
  } else if (curstates == 19) {
    x <- 5
    y <- 5
    xy <- list(x,y)
  }
  return(xy)
  }

pol_apply_fun <- function(idata) {  
}
  
  
  
data = all_data_miss
short_df <- data.frame()
short_tmp <- data.frame()
occ_array <- array(0, dim=c(5,5,25))
traj_vec <- unique(data$RecordID)
#Start of for loop - takes the raw data in and goes through each trajectory, applying following code to each individual trajectory
for (rID in traj_vec){
  #if(data[ data$RecordID==rID, 2] == "4th Quarter"){
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
    pred_tmp_x <- data.frame(predict(poly_x, pred_time))[2]
    pred_tmp_y <- data.frame(predict(poly_y, pred_time))[2]
    pred_tmp <- data.frame(pred_tmp_x,pred_tmp_y)
    #rename cols, create the curstates, dis and rad cols
    colnames(pred_tmp) <- c("x", "y")
    pred_tmp$curstates = 0
    #use the cart2pol function to get dis and rad values for each entry in the predicted possession
    pred_tmp$dis <- map2(pred_tmp$x, pred_tmp$y, cart2pol) %>% list.map(.[1])
    pred_tmp$dis = unlist(pred_tmp$dis)
    
    pred_tmp$radml <- map2(pred_tmp$x, pred_tmp$y, cart2pol_midlineL) %>% list.map(.[2])
    pred_tmp$radml = unlist(pred_tmp$radml)
    
    pred_tmp$radmr <- map2(pred_tmp$x, pred_tmp$y, cart2pol_midlineR) %>% list.map(.[2])
    pred_tmp$radmr = unlist(pred_tmp$radmr)
    
    pred_tmp$radhl <- map2(pred_tmp$x, pred_tmp$y, cart2pol_highL) %>% list.map(.[2])
    pred_tmp$radhl = unlist(pred_tmp$radhl)
    
    pred_tmp$radhr <- map2(pred_tmp$x, pred_tmp$y, cart2pol_highR) %>% list.map(.[2])
    pred_tmp$radhr = unlist(pred_tmp$radhr)
    
    for (i in 1:nrow(pred_tmp)) {
      if (pred_tmp$y[i] >= 12 & pred_tmp$y[i] < 19 & pred_tmp$x[i] < 12 & pred_tmp$x[i] > 6) {
        pred_tmp$curstates[i] = 1
      } else if (pred_tmp$y[i] >= 12 & pred_tmp$y[i] < 19 & pred_tmp$x[i] <= 25 & pred_tmp$x[i] >= 12) {
        pred_tmp$curstates[i] = 2
      } else if (pred_tmp$y[i] < 19 & pred_tmp$y[i] >= 12 & pred_tmp$x[i] > 25 & pred_tmp$x[i] < 37) {
        pred_tmp$curstates[i] = 3
      } else if (pred_tmp$y[i] >= 12 & pred_tmp$y[i] < 19 & pred_tmp$x[i] >= 37 & pred_tmp$x[i] <=50) {
        pred_tmp$curstates[i] = 4
      } else if (pred_tmp$y[i] >= 12 & pred_tmp$y[i] < 19 & pred_tmp$x[i] < 56 & pred_tmp$x[i] > 50) {
        pred_tmp$curstates[i] = 5
      } else if (pred_tmp$y[i] >= 19 & pred_tmp$y[i] < 32 & pred_tmp$dis[i]  > 20 & pred_tmp$radml[i] < cart2pol_midlineL(6,31)[2]) {   #dis > 20 as the 3point line ranges from 19.2 to 20.55 in this section so an average was taken
        pred_tmp$curstates[i] = 6  
      } else if (pred_tmp$y[i] >= 19 & pred_tmp$y[i] < 29 & pred_tmp$x[i] <=25 & pred_tmp$x[i] >= 12 & pred_tmp$dis[i] <= 20 & pred_tmp$radml[i] < cart2pol_midlineL(6,31)[2]) {
        pred_tmp$curstates[i] = 7
      } else if (pred_tmp$y[i] <= 31 & pred_tmp$y[i] >= 19 & pred_tmp$x[i] > 25 & pred_tmp$x[i] < 37) {
        pred_tmp$curstates[i] = 8 
      } else if (pred_tmp$y[i] >= 19 & pred_tmp$y[i] < 29 & pred_tmp$x[i] >=37 & pred_tmp$x[i] <= 50 & pred_tmp$dis[i] <= 20 & pred_tmp$radmr[i] > cart2pol_midlineR(56,31)[2]) {
        pred_tmp$curstates[i] = 9
      } else if (pred_tmp$y[i] >= 19 & pred_tmp$y[i] < 32 & pred_tmp$dis[i]  > 20 & pred_tmp$radmr[i] > cart2pol_midlineR(56,31)[2]) { 
        pred_tmp$curstates[i] = 10 
      } else if (pred_tmp$y[i] > 28 & pred_tmp$dis[i]  > 20.5 & pred_tmp$radml[i] > cart2pol_midlineL(6,31)[2] & pred_tmp$radhl[i] < cart2pol_highL(6,47)[2]) {
        pred_tmp$curstates[i] = 11
      } else if (pred_tmp$y[i] > 24 & pred_tmp$x[i] <= 25 & pred_tmp$dis[i] <= 20.5 & pred_tmp$radml[i] > cart2pol_midlineL(6,31)[2] & pred_tmp$radhl[i] < cart2pol_highL(6,47)[2]) {
        pred_tmp$curstates[i] = 12
        # } else if (pred_tmp$y[i] >31 & pred_tmp$dis[i] < 21 & pred_tmp$radhl[i] > cart2pol_highL(6,47)[2] & pred_tmp$radhr[i] < cart2pol_highR(56,47)[2]) {
        #   pred_tmp$curstates[i] = 13
      } else if (pred_tmp$y[i] > 24 & pred_tmp$x[i] >= 37 & pred_tmp$dis[i] <= 20.5 & pred_tmp$radmr[i] < cart2pol_midlineR(56,31)[2] & pred_tmp$radhr[i] > cart2pol_highR(56,47)[2]) {
        pred_tmp$curstates[i] = 14
      } else if (pred_tmp$y[i] > 28 & pred_tmp$dis[i]  > 20.5 & pred_tmp$radmr[i] < cart2pol_midlineR(56,31)[2] & pred_tmp$radhr[i] > cart2pol_highR(56,47)[2]) {
        pred_tmp$curstates[i] = 15
      } else if (pred_tmp$y[i] < 59 & pred_tmp$x[i] <= 31 & pred_tmp$dis[i]  > 21 & pred_tmp$radhl[i] > cart2pol_highL(6,47)[2]) {
        pred_tmp$curstates[i] = 16
      } else if (pred_tmp$y[i] < 59 & pred_tmp$y[i] > 32 & pred_tmp$x[i] > 31 & pred_tmp$dis[i]  > 21 & pred_tmp$radhr[i] < cart2pol_highR(56,47)[2]) {
        pred_tmp$curstates[i] = 17
      } else if (pred_tmp$y[i] >= 59 & pred_tmp$y[i] <= 106 & pred_tmp$x[i] <=56 & pred_tmp$x[i] >= 6) {
        pred_tmp$curstates[i] = 18  
      } else if (pred_tmp$y[i] > 106 | pred_tmp$y[i] < 12 | pred_tmp$x[i] < 6 | pred_tmp$x[i] > 56) {
        pred_tmp$curstates[i] = 19
      } else {pred_tmp$curstates[i] = 13}
      #cat('i=',i,'   ')   
      
    }

    #Cuts whole possession down to the last 4 seconds or whatever number i specifiy and reverses cell order so the occ_array is filled for the cell that the shot was taken in. 
    tmp_vec <- rev(tail(pred_tmp$curstates, 4))

    #Generate quantized values for each unique time point (x^ and y^)   
    #CREATE BREAK IF TMP_VEC AFTER FIRST VALUE == 15
    xy <- map(tmp_vec, xy_extract_fun_pol)
    xy <- matrix(unlist(xy), ncol=2, byrow = TRUE)
    #xy is a matrix of x and y grid locations for each of the values in the sliding window   
    y <- xy[,1]
    x <- xy[,2]
    
    my_matrix <- matrix(0, nrow=5, ncol=5)
    
    
    my_matrix[x[2],y[2]] = my_matrix[x[2],y[2]]+1
    my_matrix[x[3],y[3]] = my_matrix[x[3],y[3]]+1
    my_matrix[x[4],y[4]] = my_matrix[x[4],y[4]]+1
    #adds a 1 to the matrix of the court in the cells that the ball has passed through    
    occ_array[,,tmp_vec[1]] = occ_array[,,tmp_vec[1]] + my_matrix
  }}




