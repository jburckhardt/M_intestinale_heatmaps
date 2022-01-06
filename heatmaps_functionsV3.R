######################### Creating R Heat Maps out of RAST-predicted genes of different Strains #############################################

# Purpose: The purpose of this script is to create an array of genes that are present ('1') or not ('0') in different 
#          Muribaculacea intestinale strains. This script contains a function that will load the different RAST files (.tsv)
#          from a directory containing the RAST-predicted genes of different Muribaculacea strains and then join all of those
#          RAST files into a dataframe that will define if a specific strains contains or not a gene.

###################################################### Load the libraries that are necessary to run the functions #########################################
library(textshape)
library(heatmaply)
library(tidyverse)
library(ComplexHeatmap)
library(circlize)

###################################################################### Main functions #####################################################################

# In this section we will create two main functios, one that will load all  RAST .tsv files into a dataframe and create an array genes and strains
# and another function that will create the heatmap


create_gene_array <- function(directory, files, absolute_count = FALSE) {
  # The create array function will call on different functions to build an output
  strain_files <- load_files(directory, files) # see functions below for context
  strains_df <- files_to_array(strain_files = strain_files, feature_count = absolute_count) # see functions below for context
  
  return(strains_df)
}


#total <- create_gene_array(directory = "Genome_annotations/") # passed!
#total <- create_gene_array(files = c("Genome_annotations/7BRU_table.tsv", "Genome_annotations/3SKV_table.tsv")) # passed

###################################################################### Functions that build the main functions ############################################
# The functions below will be run by the main function to achieve the ultimate output

load_files <- function(directory, files) {
  # For the following chunk of code we are leaving two options for the user to use: (1) The user can input a directory where all the files are
  # found and there will be a function that will load them into a string vector, (2) The user can input a string vector of the the relative paths
  # where the files they want to load are found
  
  if(missing(directory)) { # missing() checks if the directory parameter is given by the use or not, if it is not given it assumes the 'files=' argument was given
    strain_files <- files
  } else {
    strain_files <- list.files(path = directory, pattern = "*.tsv", full.names = TRUE, recursive = FALSE)
    # list.files load the relative paths of files in a directory into a string vector!
  }
  return(strain_files)
}

##### Tests #######
#load_files(directory = "Genome_annotations/") # Passed
#create_gene_array(directory = "Genome_annotations/") # passed
#load_files(files = c("Genome_annotations/16COLB_table.tsv", "Genome_annotations/6FAAM_table.tsv")) # passed
#create_gene_array(files = c("Genome_annotations/16COLB_table.tsv", "Genome_annotations/3SKV_table.tsv")) # Passed


files_to_array <- function(strain_files, feature_count) {
  # input: a vector of strings that contains the relative paths of the .tsv files
  # output: a dataframe that contains categorical variables (System, subsystem, role, etc) of RAST genes and then columns
  #         with the name of each strain (Which is extracted from the basename of the file) that will have '1' if they contain
  #         the genes in a row or '0' if they do not contain that gene
  
  df_exists <- FALSE # This will allow us to initiate the dataframe with the first file and add the following files to the existing df
  
  # Dont take into consideration the ABSOLUTE feature count -> some genes have more than 1 version inside a genome
  if(feature_count == FALSE){
  # The following for loop iterates
    for(f in strain_files){
      if(df_exists == FALSE){ # This conditions is met for the first file as there is no existing data frame (df)
        df <- read_tsv(file = f) # load f which is the object from the iteration
        df_formated <- format_df(df, f, absolute = feature_count) # See helper function below (df_formated)
        df_exists <- TRUE
      }
      else{
        df <- read_tsv(file = f)
        df_to_add <- format_df(df, f, absolute = feature_count)
        df_formated <- full_join(df_formated, df_to_add) # adds new dataframe to old dataframe
      }
    }
  }else
    for(f in strain_files){
      if(df_exists == FALSE){ # This conditions is met for the first file as there is no existing data frame (df)
        df <- read_tsv(file = f) # load f which is the object from the iteration
        df_formated <- format_df(df, f, absolute = feature_count) # See helper function below
        df_exists <- TRUE # change to true so the condition is not met in second iteration
      }
      else{
        df <- read_tsv(file = f)
        df_to_add <- format_df(df, f, absolute = feature_count)
        df_formated <- full_join(df_formated, df_to_add) # adds new dataframe to old dataframe
      }
    
  }
  
  df_formated <- as.data.frame(mutate_all(df_formated, ~replace(., is.na(.), 0))) # Change NAs to 0 and set tibble as data frame
  return(df_formated)
}


### tests
# files_to_array("Genome_annotations/G6_table.tsv") # passed!
# files_to_array("Genome_annotations/7GAM_table.tsv") # passed!

format_df <- function(df, file, absolute){
  # purpose: This is a helper function that formats the dataframe from the file loared using read_tsv()
  #          such that other df can be joined to it to create an array
  # Input: The input is a dataframe from the loaded file and the file path as a string
  # output: a dataframe with the 'Features' colum removed and a column with the basename of the file (without extension) with the number 1
  #         per row, which signifies that the specific gene is found for that strain
  
  
  if(absolute == FALSE){
    file_name <- tools::file_path_sans_ext(basename(file)) # This gives the file name without extension or file path
    df <- df %>% select(-Features) #%>% # selects every column but the Features column
    df[(file_name)] <- 1 # add the new column with the extracted 'file_name' as its tittle
    return(df)    
  }else{
    file_name <- tools::file_path_sans_ext(basename(file)) # This gives the file name without extension or file path
    feature_counts_list <- c(count_features(df$Features))
    df <- df %>% select(-Features) #%>% # selects every column but the Features column
    df[(file_name)] <- feature_counts_list
    return(df)
  }

}

count_features <- function(string, sp = ",") {
  # This helper functions counts the number of features in the feature column
  
  return(lengths(strsplit(string, split = sp )))
}

#### Tests
# format_df(G6, file = "Genome_annotations/G6_table.tsv") # passed
#total <- files_to_array(c("Genome_annotations/G6_table.tsv", "Genome_annotations/3SKV_table.tsv")) # passed


############################################ Functions for Heatmaps ##############################################

create_heatmap <- function(df, category = NULL, subcategory = NULL, subsystem = NULL, role = NULL, exclude_label,
                           make_heatmap = "plotly", collapse_by = "Role"){
  # inputs: User gives certain variables to which they want to filter the data, and which labels they want 
  #         to exclude in the heatmap.
  # outputs: a heatmap with the genes (role) returned for the specific filter parameters given and the labels
  
  # filter by Category
  if(!is.null(category)){
    df_test <-  df %>% filter(Category == category)
    if(count(df_test) == 0){
      print(paste0("The Category:'", category, "', was not found in the data frame"))
      print("Moving to other filtering parameters")
    }else{
      df <- df_test
    }
  }
  
  #filter by subcategory
  if(!is.null(subcategory)){
    df_test <-  df %>% filter(Subcategory == subcategory)
    if(count(df_test) == 0){
      print(paste0("The Subcategory:'", subcategory, "', was not found in the data frame"))
      print("Moving to other filtering parameters")
    }else{
      df <- df_test
    }
  }
  
  #filter by subsytem
  if(!is.null(subsystem)){
    df_test <-  df %>% filter(Subsystem == subsystem)
    if(count(df_test) == 0){
      print(paste0("The Subsystem:'", subsystem, "', was not found in the data frame"))
      print("Moving to other filtering parameters")
    }else{
      df <- df_test
    }
  }
  
  #filter by Role
  if(!is.null(role)){
    df_test <-  df %>% filter(Role == role)
    if(count(df_test) == 0){
      print(paste0("The Role:'", role, "', was not found in the data frame"))
      print("Moving to other filtering parameters")
    }else{
      df <- df_test
    }
  }
  
  # If user inputs that they want to exclude a label like "Category", "Subsystem", etc.
  if(missing(exclude_label)){
    return(heatmap_func(df, hm = make_heatmap, group = collapse_by))
  }else{
    return(heatmap_func(df, exclude_label = exclude_label, hm = make_heatmap, group = collapse_by))
  }
}

##############################################################################################################

heatmap_func <- function(df, colors, exclude_label, hm, group){
  # input: This function will create the heatmap based on the data frame made by make_heatmap()
  # output: heatmap

  
  ## if given the option to Collapse by group  
  
  if(group != "Role"){
    dots <- lapply(group, as.symbol) # This will allow us to input more than one category into group_by() in a vector form!
    
    df_matrix <- df %>%
      group_by(.dots=dots) %>%
      summarise_if(is.numeric, sum) %>%
      ungroup()
    
  }else{
    ## Default --> Plot by 'Role'
    if(missing(exclude_label)){
      df_matrix <- df %>% 
        distinct(Role, .keep_all = TRUE)
    }else{
      df_matrix <- df %>% 
        select(!exclude_label) %>%
        distinct(Role, .keep_all = TRUE)  
      
      } 
  }    
  
  # Determine the name of the column that will be used to label rows in the heatmap
  group_tail <- group[length(group)] # will always pick the last element of the collapse_by/group parameter (IMPORTANT FOR LABELLING ROWS)
  row_labels <<- df_matrix[[group_tail]] # tail will always pick the last element of the collapse_by!
  
  # Takes out the "Role" column so that a color column for "Role" is not added
  if("Role" %in% colnames(df_matrix)){
    df_matrix <- df_matrix %>% 
      select(-"Role") 
  }
  
  
  # prepare the df to be plotted as a heatmap
  if(hm == "plotly"){
    return(heatmaply(df_matrix,
                     seriate = "mean",
                     colors = c("gray90", "black"),
                     grid_gap = 0.5,
                     row_dend_left = TRUE,
                     column_text_angle = -90,
                     show_dendrogram = c(F, T),
                     Rowv = FALSE,
                     side_color_colorbar_len = 0.2,
                     labRow = as.vector(row_labels), 
                     plot_method = "plotly"
    ))
  }
  
  if(hm == "ggplot"){
    return(heatmaply(df_matrix,
                     seriate = "mean",
                     colors = c("gray90", "black"),
                     grid_gap = 0.5,
                     column_text_angle = -90,
                     show_dendrogram = c(F, T),
                     Rowv = FALSE,
                     side_color_colorbar_len = 0.2,
                     labRow = as.vector(row_labels), 
                     key.title = "Number of Features",
                     #node_type = "scatter",
                     #grid_size = 3,
                     #point_size_mat = z,  # Here is where we add 
                     plot_method = "ggplot"
    # Fix the formatting of the plot with plotly!
    )%>% layout(legend = list(x = 200, y = 0.2,
                              bordercolor = "black",
                              borderwidth = 2,
                              tracegroupgap = 0.5,
                              font = list(size = 10))))
  }
  else{
    return(df_matrix)
  }
}


join_data_heatmap <- function(heatmap_df, growth_df, condition, plot_variable = "rate_mean") {
  
  df_corrected <- growth_df %>%
    filter(Condition == condition) %>%
    select(Strain, plot_variable)
  
  
  # Gather the heatmap dataframe so it is in tidy format. This will allow us to join based on strais as unique ID
  nums <- which(unlist(lapply(heatmap_df, is.numeric))) # numeric variables
  growth_data_matrix <- heatmap_df %>%
    gather("Strain", "features", nums) %>%
    right_join(df_corrected) %>%
    select(-features) %>%
    spread(Strain, plot_variable) %>%
    select(where(is.numeric))
  
  
  # return heatmap plot
  return(heatmaply(heatmap_df,
                   seriate = "mean",
                   colors = c("gray90", "gray45", "black"),
                   #grid_gap = 0.5,
                   column_text_angle = -90,
                   show_dendrogram = c(F, T),
                   Rowv = FALSE,
                   side_color_colorbar_len = 0.2,
                   labRow = as.vector(row_labels), 
                   key.title = "Number of Features",
                   node_type = "scatter",
                   grid_size = 1,
                   point_size_mat = growth_data_matrix,  # Here is where we add
                   plot_method = "ggplot"
                   # Fix the formatting of the plot with plotly!
  )%>% layout(legend = list(x = 200, y = 0.2,
                            bordercolor = "black",
                            borderwidth = 2,
                            tracegroupgap = 0.5,
                            font = list(size = 10))))
  
}



################################# ComplexHeatmap functions #########################################################


# using previous functions, we implement a new function that will return the data in a format suitable for Heatmap()

complex_heatmap_matrix <- function(directory, files, absolute_count, category = NULL, 
                                   subcategory = NULL, subsystem = NULL, role = NULL, exclude_label, collapse_by = "Role", 
                                   col_names = NULL, transpose = FALSE, secondary_grouping = FALSE) {
  
  
  # load the data based on create_gene_array parameters
  dat <- create_gene_array(directory = directory, files = files, absolute_count = absolute_count)
  
  # filter and group the data using create_heatmap()
  dat2 <- create_heatmap(df = dat, category = category, subcategory = subcategory, subsystem = subsystem, role = role,
                         exclude_label = exclude_label, collapse_by = collapse_by, make_heatmap = FALSE)
  
  ### Now make into a matrix that fits the format for ComplexHeatmap
  dat_matrix <- dat2 %>%
    select(where(is.numeric)) %>% # select numeric data
    as.matrix
  
  # Determine the name of the column that will be used to label rows in the heatmap
  group_tail <- collapse_by[length(collapse_by)] # will always pick the last element of the collapse_by/group parameter (IMPORTANT FOR LABELLING ROWS)
  group_start <- collapse_by[1] # pick the first grouping element
  
  # If you want to store secondary grouping 
  
  if(secondary_grouping == TRUE){
    secondary_grouping <<- dat2 %>% select(group_start) %>% pull
  }
  
  
  
  # add row labels to the matrix
  row_names <- dat2 %>% select(group_tail) %>% pull # pull the column in the dataframe that contains the row names
  rownames(dat_matrix) <- row_names # set the row names of your plotting matrix to this vector
  
  # Change col_names
  if(length(col_names) == ncol(dat_matrix)){
    colnames(dat_matrix) <- col_names
  }
  
  # Transpose data set if given
  if(transpose == TRUE){
    dat_matrix <- t(dat_matrix)
  }
  
  return(dat_matrix)
  
  
}

