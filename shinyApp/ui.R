library(ggvis)

# For dropdown menu
actionLink <- function(inputId, ...) {
  tags$a(href='javascript:void',
         id=inputId,
         class='action-button',
         ...)
}

fluidPage(
  titlePanel("Curva supervivencia paciente"),
  fluidRow(
    column(3,
      helpText("Introducir los datos clínicos de la paciente en el dataset deseado, esperar para visualizar la curva de supervivencia completa así como el valor de probabilidad de supervivencia exacto para el tiempo fijo."),
      wellPanel(
        h4("Datos clínicos Hospital"),
        sliderInput("cts5_t_fijo", "Año fijo de supervivencia",0, 25, 10, step = 1), #inicio,fin,defecto, step 
        selectInput("cts5_estado_menop", "Estado menopausico", c("Premenopausica", "Postmenopausica")),
        selectInput("cts5_subtipo_bc_Nuria.ki67", "Subtipo de Cancer", c("Luminal_A", "Luminal_B")),
        selectInput("cts5_riesgo", "Riesgo", c("BAJO","INTERMEDIO", "ALTO")),
        selectInput("cts5_tumor_size_group_paper_score", "Grupo tumoral", c("(1.5,10]", "(10,20]", "(20,30]", "(30,100]")),
        selectInput("cts5_ki67_status_paper_score", "Estado ki67", c("Low (<10%)", "Borderline (>=10% y <=20%)", "High (>20%)")),
        selectInput("cts5_hormone_receptor", "Receptores hormonales", c("Estrogen-receptor-positive", "Estrogen-and-Progesterone-receptor-positive")),
        selectInput("cts5_edad_Kmeans_k4", "Grupo Edad", c("(0,44]", "(44,56]", "(56,69]", "(69,100]")),
        selectInput("cts5_nodal_status_hier_k3", "Estado nodal", c("(-1,0]", "(0,10]", "(10,25]")),
        selectInput("cts5_gg_extraidos_hier_k4", "Ganglios extraidos", c("(-1,1]", "(1,11]", "(11,17]", "(17,32]"))
        #actionButton("do_cts5", "Execute CTS5 predictions")
      ),
      
      wellPanel(
        h4("Datos clínicos TCGA"),
        sliderInput("tcga_t_fijo", "Año fijo de supervivencia",0, 12, 10, step = 1), 
        selectInput("tcga_estado_menop", "Estado menopausico", c("Premenopausica", "Postmenopausica")),
        selectInput("tcga_subtipo_bc", "Subtipo de cancer", c("LuminalA", "LuminalB", "HER2-enriched", "TN")),
        selectInput("tcga_tumor_stage", "Etapa tumoral", c("I", "II", "III", "IV")),
        selectInput("tcga_hormone_receptor", "Receptores hormonales", c("Estrogen-receptor-positive", "Estrogen-and-Progesterone-receptor-positive", "Triple Negative", "Other")),
        selectInput("tcga_edad_Kmeans_k3", "Grupo Edad", c("(0,54]", "(54,70]", "(70,100]")),
        selectInput("tcga_nodal_status_k3_neg", "Estado nodal", c("(-1,0]", "(0,8]", "(8,19]", "(19,44]"))
        #actionButton("do_tcga", "Execute TCGA predictions")
      
        #textOutput("prueba")
        
      )
    ),
    column(9,wellPanel("Hospital Survival Curve (Recaida)",ggvisOutput("plot_cts5"), verbatimTextOutput("t_fijo_cts5")), wellPanel("TCGA Survival Curve (Muerte)", ggvisOutput("plot_tcga"), verbatimTextOutput("t_fijo_")))
    
  )
)
