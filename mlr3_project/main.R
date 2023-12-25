
library(stringr)
library(yaml) # ficheros yaml
library(tidyverse)
library(data.table)
library(mlr3)
library(mlr3learners)
library(mlr3pipelines)
library(mlr3measures)
library(DALEX) 
library(DALEXtra)


get_path <- function(){
  
  #--------------------------------------
  # Ruta básica de los archivos
  #--------------------------------------
  
  # salida:
  #  default_path (string)
  
  default_path <- getwd()
  elem_default = "/"
  elem_path <- unlist(str_split(default_path, elem_default))
  
  default_path <- paste0(elem_path[[1]],
                         elem_default,
                         elem_path[[2]],
                         elem_default,
                         elem_path[[3]],
                         elem_default,
                         elem_path[[4]],
                         elem_default,
                         "Escritorio",
                         elem_default
  ) 
  
  return(default_path)
}


get_source <- function(module_path){
  
  #--------------------------------------
  # Ruta específica de los archivos
  #--------------------------------------
  
  # entrada:
  #  module_path (string)
  # salida:
  #  default_path (string)
  
  default_path <- get_path()
  
  code_path <- "mlr3_project"
  elem_default = "/"
  format_file = ".R"
  
  path <- paste0(default_path,
                 code_path,
                 elem_default,
                 module_path,
                 format_file
                 )
  
  return(path)
}

#-----------------------------------------------
# LLAMADA A LAS FUNCIONES QUE PERMITEN LA:
# - Obtención de parámetros para el proyecto
# - Limpieza de datos y preprocesado
# - Modelización y presentación de resultados
#-----------------------------------------------

source(get_source(module_path = "preprocessing/preprocessing"))
source(get_source(module_path = "utils/utils"))
source(get_source(module_path = "model/model"))


#-----------------------------------------------
# EJECUCIÓN DEL PROYECTO
#-----------------------------------------------


# 1. CARGA DE DATOS

# 1.1 OBTENCIÓN DE LOS PARÁMETROS DEL PROYECTO
default_params <- get_default_params("mlr3_project/config", "params_file.yml")


# 1.2 CARGA DE LOS DATOS DE
data_path <- paste0(default_params$path$data,"/insurance",".csv")
datos <- fread(data_path)

# 2. LIMPIEZA DE DATOS. ACTUALIZACIÓN DE LAS FEATURES
datos <- apply_preprocessing_feats_target(datos)

# 3. PREPROCESADO PREVIO A LA MODELIZACIÓN

# - Creación de la muestra train/validación
# - Conversión de variables categóricas a dummies
# - Estandarización de las variables numéricas
task_train_val_data_list <- apply_preprocessing_data_to_model(default_params, datos)

## obtención de la muestra de train y validación
task_train_preprocesado <- task_train_val_data_list$train_data
task_val_preprocesado <- task_train_val_data_list$val_data


# 4. MODELIZACIÓN

# Creación de modelos siguiendo la técnica de cross-validation
modelo_cv <- apply_model(default_params = default_params,
                         datos = task_train_preprocesado,
                         cv="cv")

# Creación del modelo con toda la muestra
modelo <- apply_model(default_params = default_params,
                      datos = task_train_preprocesado)

# r2 global - cv 
apply_predict(default_params = default_params,
              modelo = modelo_cv,
              datos = task_val_preprocesado,
              cv="cv")

pred <- apply_predict(default_params = default_params,
                      modelo = modelo,
                      datos = task_val_preprocesado)


# guardar los resultados de las predicciones
path <- get_path()
saveRDS(pred, file=paste0(path, "mlr3_project/prueba.rds"))


# 5. EXPLICABILIDAD DE MODELOS

apply_plot_imp_feats(default_params = default_params,
                     modelo_learner = modelo,
                     train_task = task_train_preprocesado,
                     path = path)


apply_plot_shap_values(default_params = default_params,
                       modelo_learner = modelo,
                       train_task = task_train_preprocesado,
                       instance_id = 245,
                       path = path)
