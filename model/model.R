

apply_model <- function(default_params, datos, cv = NULL){
  
  #-------------------------------------------------
  # Crear el modelo de machine learning usando mlr
  # La función permite realizar un modelo siguiendo
  # la técnica de Cross-Validation o usar toda la 
  # muestra de entrenamiento para su desarollo
  #-------------------------------------------------
  
  # entrada: 
  #   default_params (dict)
  #   datos (list)
  #   cv (string or null)
  
  # salida: 
  #   modelo_fit (ResampleResult si cv no es nulo; LearnerRegr en otro caso)
  
  # máquina de vector soport
  svm <- mlr_learners$get(key = default_params$model$model_type)
  
  # actualización de los parámetros
  svm$param_set$values <- list(
    kernel=default_params$model$params_model$kernel,
    cost=default_params$model$params_model$cost,
    type=default_params$model$params_model$regression_type)
  
  
  if (!is.null(cv)){
    
    # método de entrenamiento - uso de Cross-Validation
    resampling_cv <- rsmp("cv", folds = default_params$model$folds)
    
    metrica <- msr(default_params$model$metric)
    
    # entrenamiento del modelo
    modelo_fit <- resample(
      task         = datos,
      learner      = svm,
      resampling   = resampling_cv,
      store_models = TRUE
    )
    
  }
  
  else{
    
    modelo_fit <- svm$train(task = datos) # entrenamiento simple (muestra total)
    
  }
  
  return(modelo_fit)
  
}


apply_predict <- function(default_params, modelo, datos, cv = NULL){
  
  
  #----------------------------------------------------
  # Realizar predicciones de los datos de entrada
  # Según haya sido entrenado el modelo,
  # se plantea el uso del argumento cv.
  # Dejar valor por defecto (cv == NULL) para predecir
  # de forma individual los resultados
  #----------------------------------------------------
  
  # entrada: 
  #   default_params (dict)
  #   modelo (LearnerRegr)
  #   cv (string or null)
  
  # salida: 
  #   modelo_fit (dataframe)

  
  if (!is.null(cv)){
    
    pred_values <- modelo$aggregate(measures = msr(default_params$model$metric_eval))
    
  } else{
    pred_values <- modelo$predict(task = datos)
  }
  
  return(pred_values)
    
}


apply_plot_imp_feats <- function(default_params,
                                modelo_learner,
                                train_task,
                                path){
  
  #----------------------------------------------------
  # Presenta de forma gráfica las variables más 
  # importantes del modelo
  #----------------------------------------------------
  
  # entrada: 
  #   default_params (dict)
  #   modelo_learner (LearnerRegr)
  #   train_task (TaskRegr)
  
  # salida: 
  #   modelo_fit (dataframe)
  
  
  datos_train <- task_train_preprocesado$data()
  
  modelo = explain_mlr3(modelo_learner,
                        data = datos_train, 
                        y = datos_train$log_charges)
  
  modelo_graph <- model_parts(modelo)

  n_feats <- ncol(datos_train) - 1 # resta uno para no contar el target
  user_n_feats <- default_params$model$explicability$show_n_features
  
  if (is.null(user_n_feats)){
    
    max_vars <- n_feats
  
  }
  else{
    
    condition <- user_n_feats > n_feats
    max_vars <- ifelse(condition,
                       n_feats,
                       user_n_feats
                       )

    
  }
    
    grafico <- plot(
      modelo_graph,
      max_vars = max_vars,
      show_boxplots = default_params$model$explicability$show_boxplot
      )
    
    grafico
    
    ggsave(paste0(path, "mlr3_project_R/imp_feats.jpeg"), plot = grafico)

}


apply_plot_shap_values <- function(default_params,
                                   modelo_learner,
                                   train_task,
                                   instance_id,
                                   path){
  
  #----------------------------------------------------
  # Presenta de forma gráfica los shaps values 
  #----------------------------------------------------
  
  # entrada: 
  #   default_params (dict)
  #   modelo_learner (LearnerRegr)
  #   train_task (TaskRegr)
  #   instance_id (integer)
  
  
  datos_train <- task_train_preprocesado$data()
  
  modelo = explain_mlr3(modelo_learner, data = datos_train, 
                        y = datos_train$log_charges)
  
  modelo_graph <- model_parts(modelo)
  
  n_feats <- ncol(datos_train) - 1 # resta uno para no contar el target
  user_n_feats <- default_params$model$explicability$show_n_features
  
  if (is.null(user_n_feats)){
    
    max_vars <- n_feats
    
  }
  else{
    
    condition <- user_n_feats > n_feats
    max_vars <- ifelse(condition,
                       n_feats,
                       user_n_feats
    ) 
    
    
  }
  
  grafico <- plot(predict_parts(explainer = modelo,
                     new_observation = datos_train[instance_id],
                     type = "shap"
                     )
       )
  
  grafico
  
  ggsave(paste0(path, "mlr3_project_R/shap_values.jpeg"), plot = grafico)
}

