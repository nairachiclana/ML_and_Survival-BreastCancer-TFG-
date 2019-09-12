library(ggvis)
library(dplyr)
if (FALSE) {
  library(RSQLite)
  library(dbplyr)
}
#library(tidyr)
library(h2o)
h2o.init()

function(input, output, patient) {
  
  
  #----------------------------------------------------------------
  #----------CTS5--------------------------------------------------
  #----------------------------------------------------------------

  prediction_cts5 <- reactive({
    
    #cts5 parameters
    cts5_t_fijo=input$cts5_t_fijo
    cts5_estado_menop=input$cts5_estado_menop
    cts5_subtipo_bc_Nuria.ki67=input$cts5_subtipo_bc_Nuria.ki67
    cts5_riesgo=input$cts5_riesgo
    cts5_tumor_size_group_paper_score=input$cts5_tumor_size_group_paper_score
    cts5_ki67_status_paper_score=input$cts5_ki67_status_paper_score
    cts5_hormone_receptor=input$cts5_hormone_receptor
    cts5_edad_Kmeans_k4=input$cts5_edad_Kmeans_k4
    cts5_nodal_status_hier_k3=input$cts5_nodal_status_hier_k3
    cts5_gg_extraidos_hier_k4=input$cts5_gg_extraidos_hier_k4
    
    if(cts5_ki67_status_paper_score=="Low (<10%)") cts5_ki67_status_paper_score="Low"
    else if(cts5_ki67_status_paper_score=="Borderline (>=10% y <=20%)") cts5_ki67_status_paper_score="Borderline"
    else if(cts5_ki67_status_paper_score=="High (>20%)") cts5_ki67_status_paper_score="High"
    
    #datasets
    load(file="/Users/nairachiclana/Google Drive/TFG/FEATURES/datasets/df_tcga_cts5.RData")
    #model
    load(file="/Users/nairachiclana/Google Drive/TFG/FEATURES/h2o/modelos_b_cts5.RData")
    modelo_cts5=h2o.loadModel(cts5_b_model_path_DL)
   
    
    #create patient
    new_patient_cts5=c(cts5_estado_menop,cts5_subtipo_bc_Nuria.ki67, cts5_riesgo,"dummy", cts5_tumor_size_group_paper_score,cts5_ki67_status_paper_score,cts5_hormone_receptor,cts5_edad_Kmeans_k4,cts5_nodal_status_hier_k3, cts5_gg_extraidos_hier_k4, cts5_t_fijo)
    new_patient_cts5=as.data.frame(t(new_patient_cts5))
    colnames(new_patient_cts5)=colnames(df.cts5)[ !colnames(df.cts5)==c("evento")]
    new_patient_cts5$seguimiento_years<-as.numeric(as.character(new_patient_cts5$seguimiento_years))
  
    
    treatments<-c("IA", "TAMOXIFENO","TAMOXIFENO-IA")
    
    #t fijo 
    probs_t_fijo=vector()
    for (treatment in treatments) {
      new_patient_cts5$hormonoteraphy=treatment
      pred_0<-as.data.frame(h2o.predict(object=modelo_cts5, newdata=as.h2o(new_patient_cts5)))$p0
      probs_t_fijo<-c(probs_t_fijo, pred_0)
    }
    names(probs_t_fijo)=treatments
    output$t_fijo_cts5=renderText(probs_t_fijo)
    
    #Survival curve years
    years=min(df.cts5$seguimiento_years):max(df.cts5$seguimiento_years)
    ia_years=vector()
    tamoxifeno_years=vector()
    tamoxifeno_ia_years=vector()
    for (year in years) {
      new_patient_cts5$seguimiento_years=year
      probs_years=vector()
      for (treatment in treatments) {
        #asignar tratamiento
        new_patient_cts5$hormonoteraphy=treatment
        #predecit prob of no recurrence for each treatment
        pred_0<-as.data.frame(h2o.predict(object=modelo_cts5, newdata=as.h2o(new_patient_cts5)))$p0
        probs_years<-c(probs_years, pred_0)
      }
      ia_years<-c(ia_years, probs_years[1])
      tamoxifeno_years<-c(tamoxifeno_years, probs_years[2])
      tamoxifeno_ia_years<-c(tamoxifeno_ia_years, probs_years[3])
    }
    
  
    print(years)
    print(ia_years)
    print(tamoxifeno_years)
    print(tamoxifeno_ia_years)
    #plot
      #df_cts5_plot=as.data.frame(cbind(years,ia_years, tamoxifeno_years,tamoxifeno_ia_years))
    
    
    #prueba
    data_matrix=matrix(c(ia_years,tamoxifeno_years,tamoxifeno_ia_years), ncol=3, nrow=length(years), byrow=FALSE)
    colnames(data_matrix)=c("IA", "Tamoxifeno", "Tamoxifeno+IA")
    mdata=data.frame(years, data_matrix)
    data<-tbl_df(mdata)
    
    data <- data %>% tidyr::gather(hormonoterapia, survival_probability, -years)
    data %>% ggvis(~years, ~survival_probability, stroke = ~hormonoterapia) %>% layer_lines()
    
     #ggvis(df_cts5_plot, ~years, ~ia_years)
           # %>% layer_paths(~years, ~tamoxifeno_years)
  # %>% layer_points( fill:="blue", size := 30)  %>% layer_paths(stroke:="blue") 
    
  
  })
  
  prediction_cts5  %>% bind_shiny("plot_cts5")



  
  #----------------------------------------------------------------
  #----------TCGA--------------------------------------------------
  #----------------------------------------------------------------
  
  prediction_tcga <- reactive({
    
    #tcga parameters
    tcga_t_fijo=input$tcga_t_fijo
    tcga_estado_menop=input$tcga_estado_menop
    tcga_tumor_stage=input$tcga_tumor_stage
    tcga_hormone_receptor=input$tcga_hormone_receptor
    tcga_edad_Kmeans_k3=input$tcga_edad_Kmeans_k3
    tcga_nodal_status_k3_neg=input$tcga_nodal_status_k3_neg
    tcga_subtipo_bc=input$tcga_subtipo_bc
    
    load(file="/Users/nairachiclana/Google Drive/TFG/FEATURES/datasets/df_tcga_cts5.RData")
    load(file="/Users/nairachiclana/Google Drive/TFG/FEATURES/h2o/modelos_tcga.RData")
    modelo_tcga=h2o.loadModel(tcga_model_path_XGBoost)
    
    
    #create patient
    new_patient_tcga=c(tcga_tumor_stage, tcga_estado_menop, tcga_hormone_receptor,  tcga_subtipo_bc, tcga_edad_Kmeans_k3, tcga_nodal_status_k3_neg, tcga_t_fijo) 
    new_patient_tcga=as.data.frame(t(new_patient_tcga))
    colnames(new_patient_tcga)=colnames(df.tcga)[ !colnames(df.tcga)==c("evento")]
    new_patient_tcga$seguimiento_years<-as.numeric(as.character(new_patient_tcga$seguimiento_years))
    
    #prediction
    predictions=vector()
    pred_0<-as.data.frame(h2o.predict(object=modelo_tcga, newdata=as.h2o(new_patient_tcga)))$p0
    output$t_fijo_tcga=renderText(pred_0)
    
    #Survival curve
    years=min(df.tcga$seguimiento_years):max(df.tcga$seguimiento_years)
    for (year in years) {
      new_patient_tcga$seguimiento_years=year
      pred_t<-as.data.frame(h2o.predict(object=modelo_tcga, newdata=as.h2o(new_patient_tcga)))$p0
      predictions=c(predictions, pred_t)
    }
    
    #plot
    print(years)
    length(years)
    print(predictions)
    length(predictions)
    df_tcga_plot=as.data.frame(cbind(years, predictions))
    survival_predictions=predictions
    ggvis(df_tcga_plot, ~years, ~survival_predictions)   %>% layer_paths(stroke:="red") 
     
    #plot(years, predictions, type="l", lwd=3, pch=10, col="red", xlab="Years", ylab="Survival probability")
    
  })
  
  prediction_tcga  %>% bind_shiny("plot_tcga")
  
  
}
