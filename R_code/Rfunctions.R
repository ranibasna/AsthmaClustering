# Helper functions ----
#
data_Prepare <- function(raw_data, impute_data){
  my_ready_data <- raw_data %>% select(- ID)
  # convert first empty spaces to NA
  my_ready_data <- my_ready_data %>% mutate(across(starts_with(c("dus","farm","smok")), ~ na_if(., "")))
  # converting charechter data to factors for missforest to work
  my_ready_data <- my_ready_data %>% mutate(across(where(is.character), factor))
  if(impute_data){
    my_ready_data_imp <- missForest(my_ready_data, verbose = TRUE)
    my_ready_data <- my_ready_data_imp$ximp
  }
  return(my_ready_data)
}
# function to one hot encode vars with more than 3 unique values and scale the numerical vars
# based on the follwoing likns
# https://bpostance.github.io/posts/clustering-mixed-data/
# https://medium.com/analytics-vidhya/the-ultimate-guide-for-clustering-mixed-data-1eefa0b4743b
# we will one hot encod only the vars wit hmore than 2 uniqe values. Also we will scale the all encoded categorical vars

OH_dummy_scale <- function(prepared_data){
  # select only binary vars
  # OH_dummy_scale_data <-
  my_data_bin <- prepared_data %>% select(where(is.factor)) %>% select(function(col) length(unique(col))<3)
  # make them dummy vars with tidymodels
  my_data_bin_dummy <- my_data_bin %>% recipe(~ .) %>% step_dummy(all_nominal()) %>% prep() %>% bake(my_data_bin)
  
  # select categorical vars with more than two unique values
  my_data_cat <- prepared_data %>% select(where(is.factor)) %>% select(function(col) length(unique(col))>2)
  
  
  # Function to check if a column is ordinal
  is_ordinal <- function(col) {
    is.ordered(col)
  }
  
  # Separate the data into ordinal, nominal, and numerical data frames
  my_data_ordinal <- my_data_cat %>% 
    select(where(is_ordinal))
  
  my_data_nominal <- my_data_cat %>% 
    select(where(~is.numeric(.) && !is_ordinal(.)))
  
  # Convert ordinal variables to ordered integers
  if (nrow(my_data_ordinal) > 0) {
    my_data_ordinal <- my_data_ordinal %>%
      mutate(across(everything(), ~as.integer(as.factor(.))))
  }
  
  # One-hot encode nominal variables with more than two levels
  if (nrow(my_data_nominal) > 0) {
    my_data_nominal <- my_data_nominal %>%
      select(where(~length(unique(.)) > 2))
    
    if (ncol(my_data_nominal) > 0) {
      my_data_nominal <- one_hot(as.data.table(my_data_nominal))
    }
  }
  
  # my_data_cat_OH <- one_hot(as.data.table(my_data_cat))
  # select numerical vars
  my_data_con <- prepared_data %>% select(where(is.double))
  # dim(my_data)[2] == dim(my_data_con)[2] + dim(my_data_OH)[2]
  OH_dummy_scale_data <- cbind(my_data_nominal, my_data_ordinal, my_data_con, my_data_bin_dummy)
  # scaling and correcting names for smoking vars
  # OH_dummy_scale_data <- OH_dummy_scale_data %>% rename_with(~str_replace(., '-smokers', "_smokers")) %>% scale() %>% as_tibble()
  return(OH_dummy_scale_data)
}
#
get_ClusterResults <- function(OriginalData, Labels){
  OriginalData$clusters <- Labels %>% as.factor()
  return(OriginalData)
}
#
Get_validate_split <- function(Processed_data, clust_fun){
  # The first splitting of the data 
  n_train <- round(0.8 * nrow(Processed_data)) #80% of length of main data set as integer
  train_indices <- sample(1:nrow(Processed_data), n_train) #creating a vector with random indices
  val_train <- Processed_data[train_indices, ] #generating train data set (with ideces = train_indices)
  val_test <- Processed_data[-train_indices, ] #generating test data set
  splited_list <- list(val_train, val_test)
  return(splited_list)
}

get_val_res <- function(Processed_data, clus_func, Seed, clus_n){
  splited_data <- Get_validate_split(Processed_data = Processed_data)
  if(clus_func=="KM"){
    KM_obj <- clusterboot(splited_data[[1]], B = 60, bootmethod = "boot", clustermethod = kmeansCBI, krange =clus_n)
    km_label <- KM_obj$result$result$cluster
    train_labeled <- get_ClusterResults(OriginalData = splited_data[[1]], Labels = km_label)
    splited_train <- Get_validate_split(Processed_data = train_labeled)
    SplitRF_model <- randomForest(clusters ~ ., data = splited_train[[1]] %>% as.data.frame(), importance = TRUE)
    val_test_KM_obj <- clusterboot(splited_data[[2]], B = 60, bootmethod = "boot", clustermethod = kmeansCBI, krange = clus_n)
    grp_val_test <- val_test_KM_obj$result$result$cluster
  }
  if(clus_func =="kamila"){
    if(dim(Processed_data %>% select(where(is.factor)))[1] == 0){
      stop(paste0(" Check the data type, for applying Kamilia method one needs to have a mixed data type"))
    }    
    my_data_con <- splited_data[[1]] %>% select(where(is.double))
    my_data_cat <- splited_data[[1]] %>% select(where(is.factor))
    # kamRes_obj <- kamila(my_data_con, my_data_cat, numClust=3:5, numInit=10,calcNumClust = "ps",numPredStrCvRun = 10, predStrThresh = 0.5)
    kamRes_obj <- kamila(my_data_con, my_data_cat, numClust=clus_n, numInit=10)
    kamila_label <- kamRes_obj$finalMemb
    train_labeled <- get_ClusterResults(OriginalData = splited_data[[1]], Labels = kamila_label)
    splited_train <- Get_validate_split(Processed_data = train_labeled)
    SplitRF_model <- randomForest(clusters ~ ., data = splited_train[[1]] %>% as.data.frame(), importance = TRUE)
    my_data_con_val <- splited_data[[2]] %>% select(where(is.double))
    my_data_cat_val <- splited_data[[2]] %>% select(where(is.factor))
    # kamRes_obj_val <- kamila(my_data_con_val, my_data_cat_val, numClust=3:5, numInit=10,calcNumClust = "ps",numPredStrCvRun = 10, predStrThresh = 0.5)
    kamRes_obj_val <- kamila(my_data_con_val, my_data_cat_val, numClust=clus_n, numInit=10)
    grp_val_test <- kamRes_obj_val$finalMemb
  }
  if(clus_func=="Kproto"){
    Kproto_obj <- kproto(splited_data[[1]], k =clus_n)
    Kproto_label <- Kproto_obj$cluster
    train_labeled <- get_ClusterResults(OriginalData = splited_data[[1]], Labels = Kproto_label)
    splited_train <- Get_validate_split(Processed_data = train_labeled)
    SplitRF_model <- randomForest(clusters ~ ., data = splited_train[[1]] %>% as.data.frame(), importance = TRUE)
    val_test_Kproto_obj <- kproto(splited_data[[2]], k = clus_n)
    grp_val_test <- val_test_Kproto_obj$cluster
  }
  
  return(adjustedRandIndex(predict(SplitRF_model, splited_data[[2]], type = "class"), grp_val_test))
}

# Plotting functions ----
parallel_box_plot <- function(labeled_original_data){
  num_result_DT <- labeled_original_data %>% select_if(is.numeric)
  num_result_DT$clusters <- labeled_original_data$clusters
  
  num_result_dec_melt <- reshape2::melt(num_result_DT, id.var = "clusters")
  num_result_dec_melt <- num_result_dec_melt %>% as.data.frame()
  
  return(ggplot(data = num_result_dec_melt, aes(x=variable, y=value)) + geom_boxplot(aes(fill=clusters))  )
}
#
radar_plot <- function(labeled_original_data, clusters){
  #result_smok <- labeled_original_data  %>% select(smoking2) %>% mutate(across(everything(), factor)) %>% as.data.table() %>% one_hot() %>% cbind(clusters) %>% group_by(clusters) %>% summarise(across(everything(), ~ mean(. == 1))) 
  # select variables with only No and Yes as values
  binary_vars <- names(labeled_original_data)[sapply(labeled_original_data, function(x) length(unique(x)) == 2)]
  # filter the data to get only the binary variables
  binary_data <- labeled_original_data %>% select(all_of(binary_vars)) # %>% select(-asthma)
  # select first 10 variables
  binary_data <- binary_data[, 1:10]
  result_binary <- binary_data %>% mutate(across(everything(), factor)) %>% as.data.table() %>% one_hot() %>%  cbind(clusters) %>% group_by(clusters) %>% summarise(total = n(), max_value = 1, min_value = 0, across(everything(), ~ mean(. == 1)))  %>% select(- c(total, max_value, min_value)) 
  # bind the tow as categorical 
  #result_cat <- cbind(result_binary, result_smok)
  numCols <- colnames(labeled_original_data %>% select_if(is.double))
  result_num <- labeled_original_data %>% select_if(is.numeric)  %>% mutate_all(rescale) %>% cbind(clusters) %>%
    group_by(clusters) %>%  dplyr::summarise(across((all_of(numCols)), mean))
  
  RiskFactorRadar_DT <- result_binary * 100
  # To use the fmsb package, I have to add 2 lines to the dataframe: the max and min of each topic to show on the plot!
  RiskFactorRadar_DT <- rbind(rep(100,10), rep(0,10), RiskFactorRadar_DT)
  colors_border_3=colormap(colormap=colormaps$temperature, nshades=6, alpha=1)
  colors_in_3=colormap(colormap=colormaps$temperature, nshades=6, alpha=0.3)
  # plotting
  plot_1 <- {radarchart( RiskFactorRadar_DT, axistype=1, pcol=colors_border_3 , pfcol=colors_in_3 , plwd=4, plty=1 , cglcol="grey", cglty=1, axislabcol="grey",  cglwd=1.1,vlcex=0.8 )
    # Legend
    legend(x=1.7, y=1, legend = unique(clusters), bty = "n", pch=20 , col=colors_border_3 , text.col = "black", cex=0.9, pt.cex=1.6)}
    # legend(x=1.7, y=1, legend = c("Smoking-associted", "Older healthy adults", "Sputum cluster","Wheezing adults","Healthy young adults","Symptomatic females"), bty = "n", pch=20 , col=colors_border_3 , text.col = "black", cex=0.9, pt.cex=1.6)}
  #
  ResSympRadar_DT <- result_num * 100
  ResSympRadar_DT <- rbind(rep(100,10), rep(0,10), ResSympRadar_DT)
  plot_2  <- {radarchart( ResSympRadar_DT, axistype=1, pcol=colors_border_3 , pfcol=colors_in_3 , plwd=4, plty=1 , cglcol="grey", cglty=1, axislabcol="grey",  cglwd=1.1, vlcex=0.8 )
    # Legend
    legend(x=1.7, y=1, legend = unique(clusters), bty = "n", pch=20 , col=colors_border_3 , text.col = "black", cex=0.9, pt.cex=1.6)}
    # legend(x=1.7, y=1, legend = c("Smoking-associted", "Older healthy adults", "Sputum cluster","Wheezing adults","Healthy young adults","Symptomatic females"), bty = "n", pch=20 , col=colors_border_3 , text.col = "black", cex=0.9, pt.cex=1.6)}
  plot_lis <- list(plot_1, plot_2)
  return(plot_lis)
  #
}
#
Par_cor_plot <- function(labeled_original_data, selected_vars, IsScaled){
  if(!is.vector(selected_vars)){
    stop(paste0("The class of the selected variables is not of vectore type"))
  }
  
  if(length(selected_vars) > 4){
    stop(paste0("Please reduce the length of the selected variables vectore"))
  }
  clusters <- labeled_original_data$clusters
  
  if(IsScaled){
    num_result <- labeled_original_data %>% select_if(is.numeric) %>%  select(all_of(selected_vars)) %>% mutate_all(rescale) %>% cbind(clusters)
  } else{
    num_result <- labeled_original_data %>% select_if(is.numeric)  %>%  select(all_of(selected_vars)) %>% cbind(clusters)
  }
  
  # Plot
  par_cor_plot <- ggparcoord(num_result,
                             columns = 1:length(selected_vars), groupColumn = length(selected_vars)+1,scale = 'std', order = "anyClass",
                             showPoints = TRUE,
                             title = "Parallel Coordinate Plot for the Data",
                             alphaLines = 0.3
  ) +
    scale_color_viridis_d() +
    theme_ipsum()+
    theme(plot.title = element_text(size=10))
  
  return(par_cor_plot)
}