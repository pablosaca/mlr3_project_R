

apply_preprocessing_data_to_model <- function(default_params, datos){
  
  #-------------------------------------------------
  # Obtención de las tareas que contienen
  # los datos de entrenamiento y validación
  # a usar en el modelo de Machine Learning
  
  # Además, esta función contiene el proceso de:
  # - Plantear un problema de regresión
  # - Conversión de variables categóricas a dummy
  # - Estandarización de las variables numéricas
  #-------------------------------------------------
  
  # entrada: 
  #   default_params (dict)
  #   datos (dataframe)
  
  # salida: 
  #   data_list_end (list)
    

  # generación de la task de regresión
  task_data <- TaskRegr$new(id = default_params$model$model_id, 
                            backend = datos,  
                            target = default_params$data$target)
  
  # task dummy 
  task_data_dummy <- apply_dummy_features(default_params = default_params,
                                          task_data = task_data)
  
  # devuelve una lista con las task para train y validación
  data_list <- apply_train_val_dataset(default_params = default_params,
                                       task_data = task_data,
                                       task_data_dummy = task_data_dummy)
  
  # lista de task con los datos estandarizados (train y validación)
  data_list_end <- apply_scale_data(default_params = default_params,
                                    data_list = data_list)  
  
  return(data_list_end)
  
}


apply_dummy_features <- function(default_params, task_data){
  
  #-------------------------------------------------
  # Procesado de datos: convertir a variables dummy
  #-------------------------------------------------
  
  # entrada: 
  #   default_params (dict)
  #   task_data (TaskRegr)
  
  # salida: 
  #   task_data_dummy (TaskRegr)


  dummy_variables <- po(default_params$preprocessing$dummy)  # variable dummy
  dummy_variables$param_set$values$method = "treatment" # elimina la multicolinealidad - borra primer factor
  
  dummy_variables$train(list(task_data))
  
  task_data_dummy <- dummy_variables$predict(list(task_data))[[1]]
  
  return(task_data_dummy)
    
}


apply_train_val_dataset <- function(default_params, task_data, task_data_dummy){
  
  #-------------------------------------------------
  # Definición de los datos de train y validación
  #-------------------------------------------------
  
  # entrada: 
  #   default_params (dict)
  #   task_data (TaskReg)
  #   task_data_dummy (TaskReg)
  
  # salida: 
  #   data_list (list of TaskRegr)
  
  particion <- rsmp("holdout", ratio=default_params$model$prop)
  particion$instantiate(task=task_data)
  
  # obtención de los índices del dataset (train y test)
  id_train <- particion$train_set(i = 1)
  id_test <- particion$test_set(i = 1)
  
  # task de los datos de entrenamiento y test
  task_train <- task_data_dummy$clone()$filter(id_train)
  task_val  <- task_data_dummy$clone()$filter(id_test)
  
  data_list <- list(train_data = task_train, val_data = task_val)
  
  return(data_list)

}


apply_scale_data <- function(default_params, data_list){
  
  #-------------------------------------------------
  # Procesado de datos: estandarización de datos
  #-------------------------------------------------
  
  # entrada: 
  #   default_params (dict)
  #   data_list (list)
  
  # salida: 
  #   output_data (list of TaskRegr)
  
  # obtención de la tarea de datos (train y val)
  task_train <- data_list$train_data
  task_val <- data_list$val_data
  
  centre <- default_params$preprocessing$standardization$params$centre
  scale <- default_params$preprocessing$standardization$params$scale
  
  # estandarización de las variables
  scale_feats <- po(default_params$preprocessing$standardization$type,
                    param_vals = list(
                      center = centre,
                      scale = scale)
                    )

  # se aplica la función preprocesado a los datos de entrenamiento
  scale_feats$train(list(task_train))  

  # se ajustan los datos de entrenamiento y validación 
  task_train_prep <- scale_feats$predict(list(task_train))[[1]]$clone()
  task_val_prep  <- scale_feats$predict(list(task_val))[[1]]$clone()
  
  output_data <- list(train_data = task_train_prep,
                      val_data = task_val_prep)
  
  return(output_data)
  
}