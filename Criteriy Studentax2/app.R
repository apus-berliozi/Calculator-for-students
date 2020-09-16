#Приложение/сайт, рассчитывающее критерий Стьюдента в загружаемом пользователем документе
library(shiny)
# Серверная составляющая
server <- function(input, output) {
  pervoeznach <-
    reactive({
      #Присваиваем загружаемый пользователем файл как переменную
      file <- input$file1
      ext <- tools::file_ext(file$datapath)
      req(file)
      validate(need(ext == "csv", "Please upload a csv file"))
      rabfile <-
        read.csv(file$datapath, header = TRUE, sep = input$sep)
      rabfile[input$prizn1]
    })
  vtoroeznach <- reactive({
    file <- input$file1
    ext <- tools::file_ext(file$datapath)
    req(file)
    validate(need(ext == "csv", "Please upload a csv file"))
    rabfile <-
      read.csv(file$datapath, header = TRUE, sep = input$sep)
    rabfile[input$prizn2]
  })
  statistica <-
    reactive({ if(input$Check) {
      test <- t.test(pervoeznach() ~ vtoroeznach())
      as.numeric(test$statistic)}
      else{
        test <- t.test(pervoeznach(), vtoroeznach())
        as.numeric(test$statistic)}
      
    })
  pznach <- 
    reactive({ if(input$Check){
      test <- t.test(pervoeznach() ~ vtoroeznach())
      as.numeric(test$p.value) } else {      test <- t.test(pervoeznach(), vtoroeznach())
      as.numeric(test$p.value)
        
      }
    })
  parameter <- 
    reactive({ if(input$Check){
      test <- t.test(pervoeznach() ~ vtoroeznach())
      as.numeric(test$parameter) } else{
        test <- t.test(pervoeznach(),  vtoroeznach())
        as.numeric(test$parameter)
      }
    })
  pervoesredn <- 
    reactive({ if(input$Check){
      test <- t.test(pervoeznach() ~ vtoroeznach())
      as.numeric(test$estimate[1]) } else{
        test <- t.test(pervoeznach(), vtoroeznach())
        as.numeric(test$estimate[1])
      }
    })
  vtoroesredn <- 
    reactive({ if(input$Check){
      test <- t.test(pervoeznach() ~ vtoroeznach())
      as.numeric(test$estimate[2]) } else {
        test <- t.test(pervoeznach(), vtoroeznach())
        as.numeric(test$estimate[2])
      }
    })
  
  output$ttest <- renderText({ vekt <- c("Критерий Стьюдента равен:",
                                       as.character(statistica()))
  vekt
  }) 
  output$parameter <- renderText({ vekt <- c("Количество степеней свободы:",
                                             as.character(parameter()))
  vekt
  })
  output$pvalue <- renderText({ vekt <- c("Величина p-значения:",
                                          as.character(pznach()))
  vekt
  })
  output$pervoesredn <- renderText({ vekt <- c("Среднее значение первой колонки/исследуемого признака:",
                                               as.character(pervoesredn()))
  vekt
  })
  output$vtoroesredn <- renderText({ vekt <- c("Среднее значение второй колонки/исследуемого признака:", input$p,
                                               as.character(vtoroesredn()))
  vekt
  })
  output$prizn1 <- renderText(input$prizn1) #NB: обмозговать так, чтобы пользователю не нужно было вводить заголовок 
  output$prizn2 <- renderText(input$prizn2)
  
  #Записываем результат
  
  
}
ui <- shinyUI(pageWithSidebar(
  headerPanel("Сравнение двух независимых выборок по t-критерию Стьюдента"),
  sidebarPanel(
    h2("Данное приложение работает с файлами пользователя"),
    helpText(
      "В загружаемом Вами файле должно быть две колонки с заголовком,
             которые содержат необходимую для обработки информацию"
    ),
    fileInput(
      "file1",
      "Выберите CSV файл",
      accept = ".csv",
      buttonLabel = "Загрузить",
      placeholder = "Файл не выбран"
    ),
    radioButtons(
      'sep',
      'Разделитель',
      c(
        'Запятая' = ',',
        'Точка с запятой' = ';',
        'Табуляция' = '\t'
      ),
      'Запятая'
    ),
    radioButtons(
      'quote',
      'Кавычки',
      c(
        'Отсутствуют' = '',
        'Двойные' = '"',
        'Одинарные' = "'"
      ),
      'Двойные'
    ),
    numericInput("prizn1", "Введите номер столбца содержащего первый исследуемый признак", value = 0),
    numericInput("prizn2", "Введите номер столбца, содержащего второй исследуемый признак или группирующий фактор", value = 0),
    checkboxInput(
      "Check",
      "Во втором столбце находится группирующий признак",
      value = FALSE
    )
  ),
  mainPanel(p(
    textOutput("ttest"),
    br(),
    p(textOutput("pvalue")),
    br(),
    p(
      textOutput("parameter")
    ),
    br(),
    p( textOutput("pervoesredn")
    ),
    br(),
    p(
      textOutput("vtoroesredn")
    ),
    br()
  ),
  #Справочная информация
  h3("Справочная информация"),
  p(
    " t-Критерий Стьюдента был разработан",
    a("Уильямом Сили Госсетом",
      href = "https://en.wikipedia.org/wiki/William_Sealy_Gosset"),
    "в 1908 году, результаты исследований были
опубликованны в статье",
    em("'The probable error
of a mean'"),
    "('Вероятностная ошибка среднего'),
под псевдонимом Стьюдент "
  ),
  img(src = "cartinka1.png", height = 70, width = 200),
  p("Уильям Силли Госсет"),
  br(),
  p(
    " t-критерий Стьюдента относят к",
    em("мерам различий для несвязанных выборрок"),
    "т.к. на его основе мы можем судить о том, насколько
сильно отличаются две
выборки друг от друга по среднему признаку. Это, в свою очередь, позволяет нам сказать,
действовал ли в одной из выборок новый систематический фактор по сравнению
с другой (к примеру, добавление токсиканта вредно повлияет на подопытных животных
и может привести к снижению плодовитости - как индивидуально так и в среднем (Э.В. Ивантер, А.В. Коросов,",
    "'Введение в количественную биологию', стр.88)"
  ),
  br(),
  p("Однако стоит помнить, что при наличии в выборках", em("выбросов"), "(образцов с сильно отличающимся от других признаком),
t-критерий будет давать неадекватные результаты. Чтобы предотвратить это, желательно либо исключить мутантные образцы из анализа, либо воспользоваться непараметрическим U-критерием Манна-Уитни")
  )
  
))

shinyApp(ui = ui, server = server)
