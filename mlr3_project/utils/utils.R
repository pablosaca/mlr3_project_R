

get_default_params <- function(file_path, file_string){
  
  #--------------------------------------
  # Parámetros básicos del proceso
  # Limpieza de datos y modelización
  #--------------------------------------
  
  # entrada: 
  #   file_path (string)
  #   file_string (string)
  
  # salida: string
  
  path <- get_path()
  file_path_end <- paste0(path, file_path, "/", file_string)
  
  return(yaml.load_file(file_path_end))
}


apply_preprocessing_feats_target <- function(datos){
  
  #--------------------------------------
  # Limpieza de los datos de entrada
  # Features categóricas, numéricas, etc
  #--------------------------------------
  
  # entrada: 
  #   datos: dataframe
  
  # salida:
  #   datos: dataframe
  
  #---------------------------------------------
  # convertir a factor las variables de entrada
  #---------------------------------------------  
  
  datos$smoker <- as.factor(datos$smoker)
  datos$sex <- as.factor(datos$sex)
  datos$region <- as.factor(datos$region)
  
  #---------------------------------------------
  # aplicar logaritmos a la variable target
  #---------------------------------------------  
  
  datos$log_charges <- log(datos$charges)
  datos$charges <- NULL
  
  #---------------------------------------------
  # aplicar logaritmos a las features numéricas
  #--------------------------------------------- 
  
  
  datos$log_age <- log(datos$age)
  datos$log_bmi <- log(datos$bmi)
  
  drop_var <- c('age', 'bmi')
  
  datos_fin <- datos %>% select(-all_of(drop_var))
  
  return(datos_fin)
  
}



