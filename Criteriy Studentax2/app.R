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
      rabfile[1]
    })
  vtoroeznach <- reactive({
    file <- input$file1
    ext <- tools::file_ext(file$datapath)
    req(file)
    validate(need(ext == "csv", "Please upload a csv file"))
    rabfile <-
      read.csv(file$datapath, header = TRUE, sep = input$sep)
    rabfile[2]
  })
  statistica <-
    reactive({
      test <- t.test(pervoeznach(), vtoroeznach())
      as.numeric(test$statistic)
    })
  pznach <- 
    reactive({
      test <- t.test(pervoeznach(), vtoroeznach())
      as.numeric(test$p.value)
    })
  parameter <- 
    reactive({
      test <- t.test(pervoeznach(), vtoroeznach())
      as.numeric(test$parameter)
    })
  pervoesredn <- 
    reactive({
      test <- t.test(pervoeznach(), vtoroeznach())
      as.numeric(test$estimate[1])
    })
  vtoroesredn <- 
    reactive({
      test <- t.test(pervoeznach(), vtoroeznach())
      as.numeric(test$estimate[2])
    })
  
  output$ttest <- renderText({
    statistica()
  }) 
  output$parameter <- renderText({
    parameter()
  })
  output$pvalue <- renderText({
    pznach()
  })
  output$pervoesredn <- renderText({
    pervoesredn()
  })
  output$vtoroesredn <- renderText({
    vtoroesredn()
  })
  output$prizn1 <- renderText(input$prizn1)
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
    textInput("prizn1", "Введите заголовок первого столбца"),
    textInput("prizn2", "Введите заголовок второго столбца")
  ),
  mainPanel(p(
    tags$b("Критерий Стьюдента равен:"),
    textOutput("ttest"),
    p(tags$b("P-значение:"), textOutput("pvalue")),
    p(
      tags$b("Количество степеней свободы:"),
      textOutput("parameter")
    ),
    p(
      tags$b("Среднее значение:"),
      textOutput("prizn1"),
      textOutput("pervoesredn")
    ),
    p(
      tags$b("Среднее значение:"),
      textOutput("prizn2"),
      textOutput("vtoroesredn")
    ),
  ),
  p(tags$b("Критерий Стьюдента равен:"), textOutput("Chtopoluch")), #Тут выводится результат
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
  