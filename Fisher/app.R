library(shiny)
server <- function(input, output) {
  pervoeznach <-
    reactive({
      #Присваиваем необходимую для анализа колонку в загружаемом
      # пользователем файле как переменную
      file <- input$file1
      ext <- tools::file_ext(file$datapath)
      req(file)
      validate(need(ext == "csv", "Please upload a csv file"))
      rabfile <-
        read.csv(file$datapath, header = TRUE, sep = input$sep)
      as.matrix(rabfile[input$isssledprizn])
    })
  vtoroeznach <- 
    reactive({
    file <- input$file1
    ext <- tools::file_ext(file$datapath)
    req(file)
    validate(need(ext == "csv", "Please upload a csv file"))
    rabfile <-
      read.csv(file$datapath, header = TRUE, sep = input$sep)
    as.matrix(rabfile[input$grupfactor])
  })
  #Присваиваем необходимую для анализа колонку в загружаемом
  # пользователем файле как переменную
  statistica <- reactive ({
    if (input$Check == TRUE) {
      test <- var.test(pervoeznach() ~ vtoroeznach())
      as.numeric(test$statistic)
    } else{
      test <- var.test(pervoeznach(), vtoroeznach())
      as.numeric(test$statistic)
    }
  })
  pznach <- reactive({
    if (input$Check == TRUE) {
      test <- var.test(pervoeznach() ~ vtoroeznach())
      as.numeric(test$p.value)
    } else{
      test <- var.test(pervoeznach(), vtoroeznach())
      as.numeric(test$p.value)
    }
  })
  doverit.int1 <- reactive({
    if (input$Check == TRUE) {
      test <- var.test(pervoeznach() ~ vtoroeznach())
      as.numeric(test$conf.int[1])
    } else{
      test <- var.test(pervoeznach(), vtoroeznach())
      as.numeric(test$conf.int[1])
    }
  })
  doverit.int2 <- reactive({
    if (input$Check == TRUE) {
      test <- var.test(pervoeznach() ~ vtoroeznach())
      as.numeric(test$conf.int[2])
    } else{
      test <- var.test(pervoeznach(), vtoroeznach())
      as.numeric(test$conf.int[2])
    }
  })
  
  output$testFishera <- renderText({
    statistica()
  })
  output$pvalue <- renderText({
    pznach()
  })
  output$int1 <- renderText({
    doverit.int1() 
  })
  output$int2 <- renderText({
    doverit.int2()
  })
}




ui <- shinyUI(pageWithSidebar(
  headerPanel("Определение U критерия Манна-Уитни"),
  sidebarPanel(
    h2("Приложение работает с файлами пользователя в формате .csv."),
    helpText(
      "Перед загрузкой файла рекомендуется задать все необходимые параметры, иначе могут возникнуть ошибки.
             В загружаемом Вами файле должны быть заголовки "
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
    numericInput(
      "isssledprizn",
      "Введите номер столбца, который содержит исследуемый признак",
      value = 0
    ),
    numericInput(
      "grupfactor",
      "Введите номер столбца, который содержит группирующий фактор или",
      value = 0
    ),
    checkboxInput(
      "Check",
      "Во втором столбце находится группирующий признак",
      value = FALSE
    )
    
  ),
  mainPanel(
    p(
      tags$b("Критерий Фишера равен:"),
      textOutput("testFishera"),
      p(tags$b("P-значение:"), textOutput("pvalue")),
      p(tags$b("Доверительный интервал:"), textOutput("int1"), textOutput("int2"))
    ),
    p(h2("Справочная инфоормация")),
    p(
      "U - критерий Манна-Уитни был предложен в 1945 году Фрэнком Вилкоксоном. Он был улучшен в 1947 году
    Х.Б. Манном и Д.Р. Уитни, в следствии чего чаще его называют их именами."
    ),
    p(
      "Критерий прост, чувствителен и довольно мощен. Служит для доказательство отличия двух выборок,
    подходит для анализа непараметрических данных.",
      em(
        "(Э.В. Ивантер, А.В. Коросов Введение в количественную биологию, стр. 100)"
      )
    ),
    p(
      "Расчёты начинаются с выстраивания показателя признака двух выборок в порядке уменьшения/увеличения, и присваивания
    этим значений рангов. В случае наличия одинаковых признаков, им будет присвоен средний арифметический ранг"
    ),
    p(h3("Ограничения")),
    p(
      tags$b(
        "В каждой выборке должно быть не менее 3-х и не более 60-ти наблюдений."
      )
    )
  )
  
))
shinyApp(ui=ui, server = server)
