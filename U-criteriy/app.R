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
  vtoroeznach <- reactive({
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
      test <- wilcox.test(pervoeznach() ~ vtoroeznach())
      as.numeric(test$statistic)
    } else{
      test <- wilcox.test(pervoeznach(), vtoroeznach())
      as.numeric(test$statistic)
    }
  })
  pznach <- reactive({
    if (input$Check == TRUE) {
      test <- wilcox.test(pervoeznach() ~ vtoroeznach())
      as.numeric(test$p.value)
    } else{
      test <- wilcox.test(pervoeznach(), vtoroeznach())
      as.numeric(test$p.value)
    }
  })
  
  output$Wilcoxtest <- renderText({
    statistica()
  })
  output$pvalue <- renderText({
    pznach()
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
      "Во втором столбике находится группирующий признак",
      value = FALSE
    )
    
  ),
  mainPanel(
    p(
      tags$b("U-критерий равен:"),
      textOutput("Wilcoxtest"),
      p(tags$b("P-значение:"), textOutput("pvalue"))
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
shinyApp(ui = ui, server = server)
library(help = "stats")
