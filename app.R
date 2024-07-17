library(shiny)
library(shinydashboard)
library(shinythemes)
library(dashboardthemes)
library(DT)
library(ggplot2)
library(dplyr)
library(reshape2)
library(corrplot)
library(caret)
library(randomForest)
library(plotly)
library(rsconnect)
#deployApp(appDir = getwd(), appName = "fp_data_mining_stroke")


#setwd("D:/kuliah/SMT 6/Data Mining/FP UAS")

# dataset
df_raw = read.csv("healthcare-dataset-stroke-data.csv")
df_bersih = read.csv("databersih.csv")
colnames(df_bersih) = c("Gender", "Age", "Hypertension", "Heart Disease", "Married",
                        "Work Type", "Residence Type", "Glucose", 
                        "BMI","Smoking Status", "Stroke")
df_bersih = df_bersih %>%
  mutate(`Work Type` = recode(`Work Type`,
                              "Private" = "Private job",
                              "Self-employed" = "Self-employed",
                              "children" = "Children",
                              "Govt_job" = "Government job",
                              "Never_worked" = "Never worked"),
         `Smoking Status` = recode(`Smoking Status`,
                                   "formerly smoked" = "Formerly smoked",
                                   "never smoked" = "Never smoked",
                                   "smokes" = "Smokes"))
X_encoded = read.csv("X-encoded.csv")
y_encoded = read.csv("y-encoded.csv")

df_encoded = cbind(X_encoded, y_encoded)

# random forest untuk prediksi
train_index <- createDataPartition(df_encoded$Stroke, p = 0.8, list = FALSE)
train_data <- df_encoded[train_index, ]
test_data <- df_encoded[-train_index, ]
train_data$Stroke <- as.factor(train_data$Stroke)
test_data$Stroke <- as.factor(test_data$Stroke)
train_data$Work.Type <- as.factor(train_data$Work.Type)
test_data$Work.Type <- as.factor(test_data$Work.Type)
train_data$Smoking.Status <- as.factor(train_data$Smoking.Status)
test_data$Smoking.Status <- as.factor(test_data$Smoking.Status)

# Model formula
formula <- Stroke ~ Age + Work.Type + Average.Glucose.Level + BMI + Smoking.Status
tuneGrid <- expand.grid(mtry = c(2, 3, 4, 5))
set.seed(123)
rf_model <- train(formula, data = train_data, method = "rf", tuneGrid = tuneGrid, trControl = trainControl(method = "cv"))


# Header
header <- dashboardHeader(
  title = "Final Project Data Mining",
  titleWidth = 300,
  dropdownMenu(
    headerText = "Kontak Penyusun Melalui Media Sosial Berikut! ^^",
    type = 'message',
    icon = icon("linkedin"),
    messageItem(
      from = "Khalida Aurora Amanda Putri",
      message = "LinkedIn",
      icon = icon("linkedin"),
      href = "https://www.linkedin.com/in/khalidaaurora/"
    ),
    messageItem(
      from = "Latifatul Khumairoh",
      message = "LinkedIn",
      icon = icon("linkedin"),
      href = "https://www.linkedin.com/in/latifatulkhumairoh/"
    ),
    messageItem(
      from = "Defara Herena Fitri Fadila",
      message = "LinkedIn",
      icon = icon("linkedin"),
      href = "https://www.linkedin.com/in/defara-herena-fitri-fadila-133311245/"
    )
  )
)

# Sidebar
sidebar <- dashboardSidebar(
  width = 200,
  sidebarMenu(
    menuItem("Beranda", tabName = "Beranda", icon = icon("home")),
    menuItem("Deskripsi Data", tabName = "Deskripsi", icon = icon("bars")),
    menuItem("Dataset", tabName = "Database", icon = icon("database")),
    menuItem("Permasalahan", tabName = "Masalah", icon = icon("question")),
    menuItem("Visualisasi", tabName = "Viz", icon = icon("chart-simple")),
    menuItem("Prediksi", tabName = "Prediksi", icon = icon("plus")),
    menuItem("Penyusun", tabName = "Penyusun", icon = icon("person"))
  )
)

# Body
body <- dashboardBody(
  shinyDashboardThemes("flat_red"),
  tabItems(
    tabItem(tabName = "Beranda",
            titlePanel(
              h1(strong("Prediksi Penyakit Stroke"),
                 style="text-align:center;")),
            div(p("Dashboard ini disusun untuk memenuhi Final Project Mata Kuliah Data Mining dan Visualisasi",
                 style="text-align:center;")),
            br(),
            tags$iframe(style="border: none; margin: 0 auto; display: block;", width="720", height="405",
                        src="https://www.youtube.com/embed/RHm1_XYE3n0?si=Sw3PkHHn5HtFUssc",
                        frameborder="0", allowfullscreen=TRUE)),
    tabItem(tabName = "Deskripsi",
            div(h1(strong("Stroke Prediction")), style = "text-align: center;"),
            box(title = strong("Tentang Dataset"), width = 12, solidHeader = TRUE,
                p(style = "text-align: justify;","Menurut World Health Organization (WHO), stroke merupakan penyebab kematian kedua terbesar secara global, 
                  bertanggung jawab atas sekitar 11% dari total kematian. Dataset ini digunakan untuk memprediksi apakah seorang 
                  pasien berpotensi mengalami stroke berdasarkan parameter jenis kelamin, usia, berbagai status penyakit, 
                  status pernikahan, jenis pekerjaan, jenis tempat tinggal, rata-rata level gula dalam darah, IMT, dan status 
                  merokok. Setiap baris dalam data memberikan informasi relevan tentang pasien tersebut."
                )),
            box(title = strong("Informasi"), width = 12, solidHeader = TRUE,
                p(style = "text-align: justify;", strong("id : "), "Kode pembeda tiap data."),
                p(style = "text-align: justify;", strong("gender : "), "Jenis kelamin dengan kelas male, female, dan other."),
                p(style = "text-align: justify;", strong("hypertension : "), "Penderita hipertensi, dibagi menjadi 0 dan 1. 
                  Orang yang tidak menderita hipertensi dikode dengan 0, sedangkan penderita hipertensi dikode dengan 1."),
                p(style = "text-align: justify;", strong("heart_disease : "), "Penderita sakit jantung, dibagi menjadi 0 dan 1. 
                  Orang yang tidak menderita sakit jantung dikode dengan 0, sedangkan penderita sakit jantung dikode dengan 1."),
                p(style = "text-align: justify;", strong("ever_married : "), "Status pernikahan, dibagi menjadi No dan Yes."),
                p(style = "text-align: justify;", strong("work_type : "), "Jenis pekerjaan, dibagi menjadi Children (Anak), 
                  Govt_jov (Pekerja pemerintahan), Never_worked (Tidak pernah bekerja), Private (Pekerja swasta), dan Self-employed (Pekerja sendiri)."),
                p(style = "text-align: justify;", strong("residence_type : "), "Jenis tempat tinggal, dibagi menjadi Rural (Desa) dan Urban (Kota)."),
                p(style = "text-align: justify;", strong("avg_glucose_level : "), "Rata-rata level gula dalam darah."),
                p(style = "text-align: justify;", strong("bmi : "), "Indeks Massa Tubuh."),
                p(style = "text-align: justify;", strong("smoking_status : "), "Status merokok, dibagi menjadi Formerly smoked (Pernah merokok), 
                  Never smoked (Tidak pernah merokok), Smokes (Merokok), dan Unknown (Tidak diketahui)."),
                p(style = "text-align: justify;", strong("stroke : "), "Penderita stroke, dibagi menjadi 0 dan 1. 
                  Orang yang tidak menderita stroke dikode dengan 0, sedangkan penderita stroke dikode dengan 1.")
            )
    ),
    tabItem(tabName = "Database",
            tabsetPanel(
              tabPanel("Data Mentah",
                     div(h3(strong("Stroke Prediction Dataset")), style = "text-align: center;"),
                     p(style="text-align: center;",tags$a("Kaggle Dataset by FEDESORIANO", href = "https://www.kaggle.com/datasets/fedesoriano/stroke-prediction-dataset/data")),
                     DTOutput("table1")
            ),
              tabPanel("Data Bersih",
                     div(h3(strong("Stroke Prediction Dataset")), style = "text-align: center;"),
                     DTOutput("table2")
            )
    )
  ),
  tabItem(tabName = "Masalah",
          div(h1(strong("Permasalahan Dataset")), style = "text-align: center;"),
          box(title = strong("Permasalahan tiap Variabel"), width = 12, solidHeader = TRUE,
              p(style = "text-align: justify;", strong("id : "), "Dilakukan dropping kolom karena tidak relevan."),
              p(style = "text-align: justify;", strong("gender : "), "Kategori Other dihapus karena hanya ada 1 data."),
              p(style = "text-align: justify;", strong("hypertension : "), "Data siap digunakan."),
              p(style = "text-align: justify;", strong("heart_disease : "), "Data siap digunakan."),
              p(style = "text-align: justify;", strong("ever_married : "), "Data siap digunakan."),
              p(style = "text-align: justify;", strong("work_type : "), "Data siap digunakan."),
              p(style = "text-align: justify;", strong("residence_type : "), "Data siap digunakan."),
              p(style = "text-align: justify;", strong("avg_glucose_level : "), "Terdapat data outlier sehingga data 
                  outlier dihapus dengan menghapus nilai di luar IQR."),
              p(style = "text-align: justify;", strong("bmi : "), "Terdapat missing value dan outlier sehingga missing value 
                  diatasi dengan KNN dan outlier atau data di luar IQR dihapus."),
              p(style = "text-align: justify;", strong("smoking_status : "), "Kategori Unknown merupakan missing value sehingga diatasi dengan metode KNN."),
              p(style = "text-align: justify;", strong("stroke : "), "Target dari data (variabel yang ingin diprediksi) dengan banyak data per kategori tidak seimbang.
                  Data kemudian diatasi dengan upsampling."),
              p(style = "text-align: justify;", strong("seluruh variabel : "), "Dilakukan penggantian penulisan nama variabel supaya lebih tertata.")
          )
  ),
  tabItem(tabName = "Viz",
          div(h1(strong("Statistika Deskriptif")), style = "text-align: center;"),
          verbatimTextOutput('summary'),
          div(h1(strong("Visualisasi")), style = "text-align: center;"),
          fluidRow(
            column(6, selectInput("num_variable", "Pilih Variabel Numerik", choices = c("Age", "BMI", "Glucose"))),
            column(6, selectInput("cat_variable", "Pilih Variabel Kategorik", choices = c("Gender","Hypertension", "Heart Disease","Married", "Work Type", "Residence Type", "Smoking Status"))),
            column(6, plotlyOutput("numPlot")),
            column(6, plotlyOutput("pieChart"))),
          fluidRow(
            box(plotlyOutput("barChart"),
                div("Banyak data dengan kategori Stroke dan Tidak Stroke berbeda jauh, sehingga dapat dikatakan data imbalance.", style="text-align: center;")),
            box(plotlyOutput("heatmapPlot"))
          )
  ),
  tabItem(tabName = "Penyusun",
          div(h1("PENYUSUN"),style="text-align: center;"),
          box(width = 4,
              status = NULL,
              div(imageOutput("osa"),style="text-align: center;",
                  style = "margin-bottom:-180px;"),
              div(strong("Khalida Aurora Amanda Putri"),style="text-align: center;"),
              div(strong("5003211022"),style="text-align: center;")
          ),
          box(width = 4,
              status = NULL,
              div(imageOutput("ifa"),style="text-align: center;",
                  style = "margin-bottom:-180px;"),
              div(strong("Latifatul Khumairoh"),style="text-align: center;"),
              div(strong("5003211032"),style="text-align: center;")
          ),
          box(width = 4,
              status = NULL,
              div(imageOutput("fara"),style="text-align: center;",
                  style = "margin-bottom:-180px;"),
              div(strong("Defara Herena Fitri Fadila"),style="text-align: center;"),
              div(strong("5003211060"),style="text-align: center;")
          ),
          box(width = 12,
              div(strong("Departemen Statistika"),style="text-align: center;"),
              div(strong("Fakultas Sains dan Analitika Data"),style="text-align: center;"),
              div(strong("Institut Teknologi Sepuluh Nopember Surabaya"),style="text-align: center;"),
              div(strong("2024"),style="text-align: center;")
          )
  ),
  tabItem(tabName = "Prediksi",
          h2("Prediksi Penyakit Stroke"),
          box(width = 12,
              div(strong("Mohon isi formulir sesuai dengan keterangan berikut"),style="text-align: center;"),
              div(strong("Jenis Pekerjaan"),style="text-align: left;"),
              div("0 : Pekerja pemerintahan",style="text-align: left;"),
              div("1 : Tidak pernah bekerja",style="text-align: left;"),
              div("2 : Pekerja swasta",style="text-align: left;"),
              div("3 : Pekerja sendiri",style="text-align: left;"),
              div("4 : Anak (belum bekerja)",style="text-align: left;"),
              div(strong("Status Merokok"),style="text-align: left;"),
              div("0 : Dulu perokok",style="text-align: left;"),
              div("2 : Tidak pernah merokok",style="text-align: left;"),
              div("3 : Perokok",style="text-align: left;"),
              div(""),
              div(strong("Mohon lakukan perhitungan Body Mass Index (BMI)"),style="text-align: center;"),
              div(imageOutput("rumus_bmi"),style="text-align: center;",
                  style = "margin-bottom:-300px;"),
              div(""),
              div("Note : "),
              div("* Rata-rata gula darah dan BMI dengan angka di belakang koma 
                  dapat dituliskan menggunakan koma, contoh 20,5"),
              div("* Masukkan angka baru jika ingin melakukan prediksi ulang"),
          ),
          fluidRow(
            column(4, numericInput("input_age", "Usia:", value = NULL, min = 0, max = 120)),
            column(4, selectInput("input_work_type", "Jenis Pekerjaan:",
                                  choices = c("0", "1", "2", "3", "4"), selected = NULL)),
            column(4, numericInput("input_avg_glucose", "Rata-rata Gula Darah:", value = NULL, min = 0, max = 1000))
          ),
          fluidRow(
            column(4, numericInput("input_bmi", "BMI:", value = NULL, min = 10, max = 60)),
            column(4, selectInput("input_smoking_status", "Status Merokok:",
                                  choices = c("0", "2", "3"), selected = NULL))
          ),
          actionButton("button_pred", "Prediksi"),
          verbatimTextOutput("hasil_pred"),
          box(width = 12,
              div(strong("Perhatian!!!"),style="text-align: left; color: red;"),
              p("Hasil hanya berupa prediksi dari data yang sudah ada sebelumnya. Tetap waspada terhadap tanda-tanda Stroke.
                Kenali SEGERA KE RS!",style="text-align: justify;"),
              div("SE : SEnyum tidak simetris",style="text-align: left;"),
              div("GE : GErak tubuh melemah",style="text-align: left;"),
              div("RA : bicaRA menjadi belepotan, seperti terbata-bata atau pelo",style="text-align: left;"),
              div("KE : KEbas separuh tubuh",style="text-align: left;"),
              div("R  : Rabun atau pandangan mengabur tiba-tiba",style="text-align: left;"),
              div("S  : Sakit kepala hebat",style="text-align: left;"))
  )
  )
)

# UI
ui <- dashboardPage(header = header,
                    sidebar = sidebar,
                    body = body,
                    )

# Server
server <- function(input, output, session) {
  # data raw
  output$table1 <- renderDT({
    df_raw <- read.csv("healthcare-dataset-stroke-data.csv")
    DT::datatable(df_raw, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # data bersih
  output$table2 <- renderDT({
    df_bersih  
    DT::datatable(df_bersih, options = list(pageLength = 10, scrollX = TRUE))  
  })
  
  # box plot
  boxplot_data <- reactive({
    req(input$num_variable)
    df_bersih %>% select(all_of(input$num_variable))
  })
  
  output$numPlot <- renderPlotly({
    data <- stack(boxplot_data())
    
    plot_ly(data, 
            x = ~ind, y = ~values, 
            type = 'box', 
            marker = list(color = "black", size = 3),
            boxmean = 'sd') %>%
      layout(title = paste(input$num_variable, "Distribution"),
             xaxis = list(title = "Variables"),
             yaxis = list(title = "Values"))
  })
  
  # pie chart
  output$pieChart <- renderPlotly({
    selected_cat <- input$cat_variable
    
    cat_data <- df_bersih %>%
      group_by(!!sym(selected_cat)) %>%
      summarize(count = n()) %>%
      mutate(percentage = count / sum(count) * 100)
    
    plot_ly(cat_data, 
            labels = ~get(selected_cat), 
            values = ~percentage, 
            type = 'pie',
            textinfo = 'label+percent',
            insidetextorientation = 'radial',
            marker = list(colors = c("rgb(142, 186, 217)",
                                     "rgb(235, 137, 105)",
                                     "rgb(163, 205, 166)",
                                     "rgb(226, 174, 213)",
                                     "rgb(200, 184, 140)")),
            hoverinfo = 'label+percent+count') %>%
      layout(title = paste(selected_cat, "Distribution"),
             showlegend = TRUE)
  })
  
  #bar plot imbalance
  output$barChart <- renderPlotly({
    bar_data <- df_bersih %>%
      count(Stroke)
    plot_ly(bar_data, x = ~as.factor(Stroke), y = ~n, type = 'bar', 
            marker = list(color = "rgb(235, 137, 105)")) %>%
      layout(title = "Stroke Distribution",
             xaxis = list(title = "Stroke"),
             yaxis = list(title = "Count"))
  })
  
  # heatmap
  correlation_matrix <- cor(select(df_bersih, Age, BMI, Glucose))
  correlation_data <- reshape2::melt(correlation_matrix)
  
  output$heatmapPlot <- renderPlotly({
    plot_ly(z = ~correlation_matrix, x = colnames(correlation_matrix), y = colnames(correlation_matrix), type = "heatmap",
            colorscale = "RdYlBu",
            colorbar = list(title = "Correlation",
                            tickvals = c(-1, -0.5, 0, 0.5, 1),
                            ticktext = c("-1", "-0.5", "0", "0.5", "1"))) %>%
      layout(title = "Correlation Heatmap",
             xaxis = list(title = "Variables"),
             yaxis = list(title = "Variables"))
  })
  
  # summary
  output$summary <- renderPrint({
    summary(df_bersih)
  })
  
  # penyusun
  output$osa <- renderImage({
    list(src="WWW/osa.jpg",height = 200, width = 150)
  },deleteFile = F)
  output$ifa <- renderImage({
    list(src="WWW/ifa.jpg",height = 200, width = 150)
  },deleteFile = F)
  output$fara <- renderImage({
    list(src="WWW/fara.jpg",height = 200, width = 150)
  },deleteFile = F)
  
  # prediksi
  hasil_prediksi <- reactive({
    req(input$input_age, input$input_work_type, input$input_avg_glucose, input$input_bmi, input$input_smoking_status)
    
    new_data <- data.frame(
      Age = input$input_age,
      Work.Type = as.factor(input$input_work_type),
      Average.Glucose.Level = input$input_avg_glucose,
      BMI = input$input_bmi,
      Smoking.Status = as.factor(input$input_smoking_status),
      Stroke = NA  
    )
    
    prediksi <- predict(rf_model, newdata = new_data)
    ifelse(prediksi == 1, "Stroke", "No Stroke")
  })
  
  output$hasil_pred <- renderPrint({
    hasil_prediksi()
  })
  
  observeEvent(input$button_pred, {
    hasil_prediksi()
  })
  
  output$rumus_bmi <- renderImage({
    list(src="WWW/rumus_bmi.png",height = 80, width = 250)
  },deleteFile = F)

}

shinyApp(ui, server)

