# ---------------------------------------------- Logging Errors -------------------------------------------------- #

## For debug purpose
xprint_text <- function (free_text
){
  
  if(p_debug > 0){
    print(free_text)
  }
  
}

# ---------------------------------------------- Parameters -------------------------------------------------- #
chunk_no <- 0
test_week_start <- 11601
test2_week_end <- 11613
instance_dist_threshold <- 29
total_ntile <- 30
p_seed <- 0
p_debug <-0

output_variable_names <- data.frame(Variable = c(
  "(Intercept)",
  "log_unique_retail_price" ,
  "weekend_days" ,               
  "jan_days" ,           
  "feb_days" ,          
  "mar_days" ,
  "apr_days" ,         
  "may_days" ,         
  "jun_days" ,           
  "jul_days" ,            
  "aug_days" ,          
  "sep_days" ,               
  "oct_days" ,          
  "nov_days" ,          
  "dec_days" ,          
  "mothers_day_days" ,
  "thanksgiving" ,   
  "valentines_day" ,               
  "ash_wednesday" ,             
  "superbowl" ,        
  "passover" ,               
  "st_patricks_day" ,             
  "good_friday" ,     
  "independence_day" ,
  "easter" ,              
  "new_years_day_and_kwanzaa" , 
  "mardi_gras" ,      
  "halloween" ,               
  "pay_day__1st___15th_" ,              
  "payday_lag__4th___18th_" ,
  "ceaser_chavez_day" ,     
  "hanukkah" ,         
  "cinco_de_mayo" ,             
  "cyber_monday" ,
  "nfl_flag" ,               
  "nascar_flag"         ,
  "store_college_fbcollege_fb"         , 
  "store_college_fbcollege_fb_lead" ,
  "avg_price_gap",
  "halo_1p",
  "halo_2p",
  "halo_3p",
  "halo_4p", 
  "halo_5p",
  "snow_occ_ind",
  "significant_snow_ind",
  "three_kings_day"
)
)


output_column_names <- c( "(Intercept)"
                          ,"log_unique_retail_price"
                          ,"weekend_days"
                          ,"jan_days" 
                          ,"feb_days"
                          ,"mar_days"
                          ,"apr_days"
                          ,"may_days"
                          ,"jun_days"
                          ,"jul_days"
                          ,"aug_days"
                          ,"sep_days"
                          ,"oct_days"                        
                          ,"nov_days"
                          ,"dec_days"                        
                          ,"mothers_day_days"
                          ,"thanksgiving"                    
                          ,"valentines_day" 
                          ,"ash_wednesday"                   
                          ,"superbowl"
                          ,"passover"                        
                          ,"st_patricks_day"
                          ,"good_friday"                     
                          ,"independence_day"
                          ,"easter"                          
                          ,"new_years_day_and_kwanzaa"
                          ,"mardi_gras"                      
                          ,"halloween"
                          ,"pay_day__1st___15th_"            
                          ,"payday_lag__4th___18th_"
                          ,"ceaser_chavez_day"               
                          ,"hanukkah"
                          ,"cinco_de_mayo"                   
                          ,"cyber_monday"
                          ,"nfl_flag"                        
                          ,"nascar_flag"
                          ,"store_college_fbcollege_fb"      
                          ,"store_college_fbcollege_fb_lead"
                          ,"avg_price_gap"                   
                          ,"halo_1p"
                          ,"halo_2p"                         
                          ,"halo_3p" 
                          ,"halo_4p"                         
                          ,"halo_5p"
                          ,"snow_occ_ind"                    
                          ,"significant_snow_ind"
                          ,"three_kings_day"                 
                          ,"MAPE" 
                          ,"r_square"                        
                          ,"best_lambda" 
                          ,"total_data_points"               
                          ,"training_data_points"
                          ,"test_data_points"                
                          ,"UPC" 
                          ,"l0_l1_l2"
                          ,"message_txt"
                          ,"selected_model"
)


output_df <- data.frame(matrix(ncol = length(output_column_names), 
                               nrow = 0
))

colnames(output_df) <- output_column_names

output_df$message_txt <- as.character(output_df$message_txt)
output <- output_df

# ---------------------------------------------- Transformations -------------------------------------------------- #

rm_null_column <- function (df_col , inp_no){
  
  # Handling null for a column
  df_col <- as.character(df_col)
  df_col <- ifelse(df_col == 'NULL' , inp_no ,df_col )
  df_col <- as.numeric(df_col)
  df_col
  
}

#####################################################

remove_null <- function (df){
  
  #Handling nulll
  df$avg_price_gap = rm_null_column(df$avg_price_gap,9999999)
  
  df$halo_1p = rm_null_column(df$halo_1p, exp(99))
  df$halo_1p = log(df$halo_1p)
  
  df$halo_2p = rm_null_column(df$halo_2p, exp(99))
  df$halo_2p = log(df$halo_2p)  
  
  df$halo_3p = rm_null_column(df$halo_3p, exp(99))
  df$halo_3p = log(df$halo_3p)    
  
  df$halo_4p = rm_null_column(df$halo_4p, exp(99))
  df$halo_4p = log(df$halo_4p)      
  
  df$halo_5p = rm_null_column(df$halo_5p, exp(99))
  df$halo_5p = log(df$halo_5p)      
  df
}

#####################################################

assign_serial_to_upc <- function (df){
  
  # Assign serial number to UPC numbers
  upc_list <- unique(df$upc_nbr)
  upc_df=data.table(row = 1:length(upc_list),upc_nbr = upc_list)
  
  df <- merge(df,upc_df)
  df=as.data.table(df)
  df=df[,cnt :=.N,by=c("upc_nbr")]
  
  df
}

#####################################################

reorder_columns <- function (op){
  
  op$Intercept <- op$`(Intercept)`
  ret_op <- dplyr::select(op,
                          UPC, 
                          l0_l1_l2,
                          selected_model,
                          MAPE,
                          best_lambda
                          ,Intercept                  
                          ,log_unique_retail_price         
                          ,weekend_days                   
                          ,jan_days                        
                          ,feb_days                        
                          ,mar_days                       
                          ,apr_days                        
                          ,may_days                        
                          ,jun_days                       
                          ,jul_days                        
                          ,aug_days                        
                          ,sep_days                       
                          ,oct_days                        
                          ,nov_days                        
                          ,dec_days                       
                          ,mothers_day_days                
                          ,thanksgiving                    
                          ,valentines_day                 
                          ,ash_wednesday                   
                          ,superbowl                       
                          ,passover                       
                          ,st_patricks_day                 
                          ,good_friday                     
                          ,independence_day               
                          ,easter                          
                          ,new_years_day_and_kwanzaa       
                          ,mardi_gras                     
                          ,halloween                       
                          ,pay_day__1st___15th_            
                          ,payday_lag__4th___18th_        
                          ,ceaser_chavez_day               
                          ,hanukkah                        
                          ,cinco_de_mayo                  
                          ,cyber_monday                    
                          ,nfl_flag                        
                          ,nascar_flag                    
                          ,store_college_fbcollege_fb      
                          ,store_college_fbcollege_fb_lead 
                          ,avg_price_gap                  
                          ,halo_1p                         
                          ,halo_2p                         
                          ,halo_3p                        
                          ,halo_4p                         
                          ,halo_5p                         
                          ,snow_occ_ind                   
                          ,significant_snow_ind            
                          ,three_kings_day                 
                          ,r_square                        
                          ,total_data_points              
                          ,training_data_points            
                          ,test_data_points                
                          ,message_txt                                      
                          ,date_of_run
  )
  
  return(ret_op)
}

# ---------------------------------------------- Partitions -------------------------------------------------- #

# This function will create train test partition
create_partititon <- function (df){
  
  # Assign Partition type  
  df=df[avg_imp_vol>0,train_or_test := ifelse(cal_week < test_week_start,"TRAIN"
                                              ,ifelse(cal_week <= test2_week_end,"TEST2","TEST" ))]
  df
}

#####################################################

# This function will assign quantiles 
crt_qtl_upc <- function (upc_df){
  
  # Assign rank
  upc_df=setorder(upc_df,upc_nbr,train_or_test,instances)
  upc_df=upc_df[,instance_dist := ntile(instances,total_ntile),by=c("upc_nbr","train_or_test")]
  
}

#####################################################

## At UPC level
reg_data_prep <- function (upc_df
){
  #### Get Train Data 
  upc_df=as.data.table(upc_df)
  train=upc_df[train_or_test == "TRAIN" & instance_dist > instance_dist_threshold,]
  
  #### Get Test Data 
  test2=upc_df[train_or_test == "TEST2" & instance_dist > instance_dist_threshold,]
  
  chunk_no <- 100
  rbind(train,test2)    
}

#####################################################

# Null value treatment and Data Filteration
wrangle_at_one_node <- function (hive_single_node_data){
  
  hive_single_node_data <- remove_null(hive_single_node_data)
  hive_single_node_data <- assign_serial_to_upc(hive_single_node_data)
  hive_single_node_data <- create_partititon(hive_single_node_data)
  hive_single_node_data <- crt_qtl_upc(hive_single_node_data)
  hive_single_node_data<-reg_data_prep(hive_single_node_data)  
  return(hive_single_node_data)
}

# ---------------------------------------------- Regularization functions -------------------------------------------------- #

## At UPC level create matrix
l1_l2_create_matrix <- function (upc_df2
){
  
  #### sort
  upc_df2 <- arrange(upc_df2,
                     log_unique_retail_price ,
                     halo_1p,
                     halo_2p,
                     halo_3p,
                     halo_4p, 
                     halo_5p,
                     weekend_days,   
                     jan_days,               
                     feb_days,               
                     mar_days,             
                     apr_days,              
                     may_days,             
                     jun_days,
                     jul_days,               
                     aug_days,              
                     sep_days,               
                     oct_days,              
                     nov_days,              
                     dec_days,               
                     mothers_day_days,          
                     thanksgiving,       
                     valentines_day,    
                     ash_wednesday,               
                     superbowl,            
                     passover,               
                     st_patricks_day, 
                     good_friday,        
                     independence_day,            
                     easter,               
                     new_years_day_and_kwanzaa,     
                     mardi_gras,          
                     halloween,           
                     pay_day__1st___15th_,  
                     payday_lag__4th___18th_,               
                     ceaser_chavez_day,          
                     hanukkah,             
                     cinco_de_mayo, 
                     cyber_monday,   
                     three_kings_day, 
                     significant_snow_ind,               
                     snow_occ_ind,     
                     nfl_flag,  
                     nascar_flag,         
                     store_college_fbcollege_fb,          
                     store_college_fbcollege_fb_lead, 
                     avg_price_gap 
  )
  
  #### Get Test Data 
  x_matrix <- as.matrix(data.frame(upc_df2$log_unique_retail_price ,
                                   upc_df2$halo_1p,
                                   upc_df2$halo_2p,
                                   upc_df2$halo_3p,
                                   upc_df2$halo_4p, 
                                   upc_df2$halo_5p,
                                   upc_df2$weekend_days,   
                                   upc_df2$jan_days,               
                                   upc_df2$feb_days,               
                                   upc_df2$mar_days,             
                                   upc_df2$apr_days,              
                                   upc_df2$may_days,             
                                   upc_df2$jun_days,
                                   upc_df2$jul_days,               
                                   upc_df2$aug_days,              
                                   upc_df2$sep_days,               
                                   upc_df2$oct_days,              
                                   upc_df2$nov_days,              
                                   upc_df2$dec_days,               
                                   upc_df2$mothers_day_days,          
                                   upc_df2$thanksgiving,       
                                   upc_df2$valentines_day,    
                                   upc_df2$ash_wednesday,               
                                   upc_df2$superbowl,            
                                   upc_df2$passover,               
                                   upc_df2$st_patricks_day, 
                                   upc_df2$good_friday,        
                                   upc_df2$independence_day,            
                                   upc_df2$easter,               
                                   upc_df2$new_years_day_and_kwanzaa,     
                                   upc_df2$mardi_gras,          
                                   upc_df2$halloween,           
                                   upc_df2$pay_day__1st___15th_,  
                                   upc_df2$payday_lag__4th___18th_,               
                                   upc_df2$ceaser_chavez_day,          
                                   upc_df2$hanukkah,             
                                   upc_df2$cinco_de_mayo, 
                                   upc_df2$cyber_monday,   
                                   upc_df2$three_kings_day, 
                                   upc_df2$significant_snow_ind,               
                                   upc_df2$snow_occ_ind,     
                                   upc_df2$nfl_flag,  
                                   upc_df2$nascar_flag,         
                                   upc_df2$store_college_fbcollege_fb,          
                                   upc_df2$store_college_fbcollege_fb_lead, 
                                   upc_df2$avg_price_gap 
  ))
  
  chunk_no <- 200
  
  return(x_matrix)
}
#####################################################
get_r_square <- function (y,
                          avg_y,
                          predicted_y
){
  
  SST = sum((y - avg_y)^2)
  SSR = sum((y - predicted_y)^2)
  R_Squared = 1- (SSR/SST)  
  
  chunk_no <- 300
  
  return(R_Squared)
}

#####################################################

get_hold_out_mape <- function (x_hold_out,
                               y,
                               model
){
  
  predicted_y = exp(predict(object = model, x_hold_out))
  
  abs_perc_error = abs(y - predicted_y ) / y
  
  MAPE = 100*mean(abs_perc_error)
  
  chunk_no <- 400
  
  if (is.na(MAPE)){
    MAPE <- -999
  }
  return(MAPE)
}

# ---------------------------------------------- Regularization -------------------------------------------------- #

## Creating a data frame that stores all error messages of the model runs for L1/L2 Regularization
get_error_data_frame <- function (upc,
                                  regression_data,
                                  upc_data,
                                  type
){
  
  error_row <- output_df
  
  error_row[1,"UPC"] <- upc
  
  # Filtering for train and test data for Modeling
  regression_data=as.data.table(regression_data)
  train=regression_data[train_or_test == "TRAIN",]
  test=regression_data[train_or_test == "TEST2",]
  train=as.data.frame(train)
  test=as.data.frame(train)
  regression_data=as.data.frame(regression_data)
  
  error_row$total_data_points <- nrow(upc_data)
  error_row$training_data_points <- nrow(train)  
  error_row$test_data_points <- nrow(test)  
  
  error_row$l0_l1_l2 <- type
  error_row$message_txt <- "wrapper_reg is throwing error ..."
  error_row$selected_model <- -2
  error_row$best_lambda <- -999
  error_row$MAPE <- -999
  
  chunk_no <- 500
  
  return(error_row)
  
}

# Error handling for the L1/L2 Regularization
wrapper_l1_l2_reg <- function (upc_df,
                               type
){
  
  chunk_no <- 600
  l1_l2_type <- type
  type <- ifelse(type == 2 , 0, 1 )
  data1 <- reg_data_prep(upc_df)
  
  data1$log_avg_imp_vol <- log(data1$avg_imp_vol)
  data1$log_unique_retail_price <- log(data1$unique_retail_price)
  
  # Filtering for train and test data for Modeling
  data1=as.data.table(data1)
  train=data1[train_or_test == "TRAIN",]
  test=data1[train_or_test == "TEST2",]
  train=as.data.frame(train)
  test=as.data.frame(train)
  data1=as.data.frame(data1)
  
  train<- arrange(train,
                  log_unique_retail_price ,
                  halo_1p,
                  halo_2p,
                  halo_3p,
                  halo_4p,
                  halo_5p,
                  weekend_days,
                  jan_days,
                  feb_days,
                  mar_days,
                  apr_days,
                  may_days,
                  jun_days,
                  jul_days,
                  aug_days,
                  sep_days,
                  oct_days,
                  nov_days,
                  dec_days,
                  mothers_day_days,
                  thanksgiving,
                  valentines_day,
                  ash_wednesday,
                  superbowl,
                  passover,
                  st_patricks_day,
                  good_friday,
                  independence_day,
                  easter,
                  new_years_day_and_kwanzaa,
                  mardi_gras,
                  halloween,
                  pay_day__1st___15th_,
                  payday_lag__4th___18th_,
                  ceaser_chavez_day,
                  hanukkah,
                  cinco_de_mayo,
                  cyber_monday,
                  three_kings_day,
                  significant_snow_ind,
                  snow_occ_ind,
                  nfl_flag,
                  nascar_flag,
                  store_college_fbcollege_fb,
                  store_college_fbcollege_fb_lead,
                  avg_price_gap
  )
  
  test<- arrange(test,
                 log_unique_retail_price ,
                 halo_1p,
                 halo_2p,
                 halo_3p,
                 halo_4p,
                 halo_5p,
                 weekend_days,
                 jan_days,
                 feb_days,
                 mar_days,
                 apr_days,
                 may_days,
                 jun_days,
                 jul_days,
                 aug_days,
                 sep_days,
                 oct_days,
                 nov_days,
                 dec_days,
                 mothers_day_days,
                 thanksgiving,
                 valentines_day,
                 ash_wednesday,
                 superbowl,
                 passover,
                 st_patricks_day,
                 good_friday,
                 independence_day,
                 easter,
                 new_years_day_and_kwanzaa,
                 mardi_gras,
                 halloween,
                 pay_day__1st___15th_,
                 payday_lag__4th___18th_,
                 ceaser_chavez_day,
                 hanukkah,
                 cinco_de_mayo,
                 cyber_monday,
                 three_kings_day,
                 significant_snow_ind,
                 snow_occ_ind,
                 nfl_flag,
                 nascar_flag,
                 store_college_fbcollege_fb,
                 store_college_fbcollege_fb_lead,
                 avg_price_gap
  )
  
  train_matrix <- l1_l2_create_matrix(train)
  test_matrix <- l1_l2_create_matrix(test)
  
  chunk_no <- 700
  set.seed(p_seed)
  cv.glmmod <- try(cv.glmnet(train_matrix, 
                             y =  (train$log_avg_imp_vol) ,
                             alpha=type
  ),
  silent = TRUE
  )
  
  if (class(cv.glmmod) != "cv.glmnet")
  {
    return(get_error_data_frame(upc_df$upc_nbr[1],
                                data1,
                                upc_df,
                                l1_l2_type
    )
    )
  }
  
  chunk_no <- 800
  best_lambda <- cv.glmmod$lambda.min
  
  set.seed(p_seed)
  model <- glmnet( train_matrix ,
                   y =  train$log_avg_imp_vol, 
                   family ="gaussian", 
                   alpha = type, 
                   lambda = best_lambda
  )
  
  R_Squared <- get_r_square((train$log_avg_imp_vol),
                            mean((train$log_avg_imp_vol)),
                            predict(object = model, train_matrix)
  )
  
  coefficient = data.frame(Variable = dimnames(coef(model))[[1]], Beta = matrix(coef(model)))
  
  coefficient$Variable <- gsub("upc_df2.", "", coefficient$Variable)
  
  
  chunk_no <- 900
  output <- data.frame(t(coefficient$Beta))
  colnames(output) <- coefficient$Variable
  
  
  output$MAPE <- get_hold_out_mape(test_matrix ,
                                   test$avg_vol,
                                   model
  )
  
  
  output$r_square <- R_Squared
  output$best_lambda <- best_lambda
  
  chunk_no <- 1000  
  output$total_data_points <- nrow(upc_df)
  output$training_data_points <- nrow(train)  
  output$test_data_points <- nrow(test)  
  output$UPC <- data1$upc_nbr[1]
  
  output$l0_l1_l2 <- l1_l2_type
  output$message_txt <- "Success"
  output$selected_model <- -1
  
  return(output)
  
}

#####################################################

# L1/L2 Regularization main function
l1_l2_regression <- function (upc_df,
                              type
){
  
  chunk_no <- 1100
  
  l1_l2 <- tryCatch(
    {
      wrapper_l1_l2_reg(upc_df,
                        type
      )
    },
    error= function(e) 
    {
      
      error_return <- output_df
      error_return[1,"UPC"] <- upc_df$upc_nbr[1]
      error_return$l0_l1_l2 <- type
      error_return$message_txt <- message(e)
      return(error_return)
    }
  )
}

# ---------------------------------------------- OLS Regression -------------------------------------------------- #
# Error handling for the OLS Regression 
wrapper_ols_reg <- function (upc_df
){
  
  best_lambda <- 0
  data1 <- reg_data_prep(upc_df)
  data1$log_avg_imp_vol <- log(data1$avg_imp_vol)
  data1$log_unique_retail_price <- log(data1$unique_retail_price)
  
  model = try(lm(log_avg_vol ~ log_unique_retail_price +   
                   weekend_days +               
                   jan_days +           
                   feb_days +          
                   mar_days +
                   apr_days +         
                   may_days +         
                   jun_days +           
                   jul_days +            
                   aug_days +          
                   sep_days +               
                   oct_days +          
                   nov_days +          
                   dec_days +          
                   mothers_day_days +
                   thanksgiving +   
                   valentines_day +               
                   ash_wednesday +             
                   superbowl +        
                   passover +               
                   st_patricks_day +             
                   good_friday +     
                   independence_day +
                   easter +              
                   new_years_day_and_kwanzaa + 
                   mardi_gras +      
                   halloween +               
                   pay_day__1st___15th_ +              
                   payday_lag__4th___18th_ +
                   ceaser_chavez_day +     
                   hanukkah +         
                   cinco_de_mayo +             
                   cyber_monday +
                   nfl_flag +               
                   nascar_flag         +
                   store_college_fbcollege_fb         + 
                   store_college_fbcollege_fb_lead +
                   avg_price_gap
                 , data = train)
              ,
              silent = TRUE
  )
  
  if (class(model) != "lm")
  {
    return(get_error_data_frame(upc_df$upc_nbr[1],
                                data1,
                                upc_df,
                                0
    )
    )
  }
  
  
  step <- try(stepAIC(model, 
                      direction="both",
                      trace=FALSE
  ),
  silent = TRUE
  )
  
  # Check if stepWise is going into error
  if(class(step) == "try-error"){
    
    return(get_error_data_frame(upc_df$upc_nbr[1],
                                data1,
                                upc_df,
                                0
    )
    )
    
  }
  
  MAPE <- get_hold_out_mape(test ,
                            test$avg_vol,
                            step
  )
  
  coefficient <- summary(step)$coef
  r_squared <- summary(step)$r.squared
  adjr_squared <- summary(step)$adj.r.squared
  
  result = data.frame(cbind(upc = train[1,1]  , coefficient ,  
                            best_lambda       , MAPE , 
                            r_squared         , adjr_squared
  )
  )
  
  result$Variable <- rownames(result)
  
  result <- suppressMessages(left_join(output_variable_names, result))
  
  result$Estimate[is.na(result$Estimate)] <- 0
  
  output <- data.frame(t(result$Estimate))
  colnames(output) <- result$Variable
  
  output$MAPE <- MAPE
  output$r_square <- r_squared
  output$best_lambda <- 0
  
  output$total_data_points <- nrow(upc_df)
  output$training_data_points <- nrow(train)  
  output$test_data_points <- nrow(test)    
  output$UPC <- data1$upc_nbr[1]
  
  output$l0_l1_l2 <- 0
  output$message_txt <- "Success"
  output$selected_model <- -1
  
  return(output)
}

#####################################################

# OLS Regression main function
ols_regression <- function (upc_df
){
  
  l0 <- tryCatch(
    {
      wrapper_ols_reg(upc_df
      )
    },
    error= function(e) 
    {
      error_return <- output_df
      error_return[1,"UPC"]  <- upc_df$upc_nbr[1]
      error_return$l0_l1_l2 <- 0
      error_return$message_txt <- message(e)
      return(error_return)
    }
  )
}

# ------------------------------------------- Running all the Regression Models -------------------------------------------------- #

get_upcs_models <- function (upc_data){
  
  paste(upc_data$upc_nbr[1] )
  
  paste("      ")
  xprint_text("Model start point")
  l1 <- l1_l2_regression(upc_data,1)
  l1[is.na(l1)] <- 0
  
  xprint_text("l1 complete")
  
  l2 <- l1_l2_regression(upc_data,2)
  l2[is.na(l2)] <- 0
  xprint_text("l2 complete")
  
  l0 <- ols_regression(upc_data)
  l0[is.na(l0)] <- 0
  xprint_text("l0 complete")
  
  # Selecting the best model from L1,L2 & OLS based on lambda and MAPE
  if(l1$best_lambda > 1 && l2$best_lambda > 1){
    
    l0$selected_model <- 1
    l1$selected_model <- 0
    l2$selected_model <- 0
    
  }
  else if(l1$MAPE < l2$MAPE){
    
    l0$selected_model <- 0
    l1$selected_model <- 1
    l2$selected_model <- 0
    
  }
  else if(l1$MAPE > l2$MAPE){
    
    l0$selected_model <- 0
    l1$selected_model <- 0
    l2$selected_model <- 1
    
  }
  
  # Appending the results of all the 3 regression models
  upc_op <- dplyr::union(dplyr::union(l0,l1),l2)
  return(upc_op)
  
}

