#Дисперссионый тест Фишера
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
  
  output$testFishera <-
    renderText({
      vekt <- c("Критерий Фишера равен:", as.character(statistica())) vekt
    })
  output$pvalue <-
    renderText({
      vekt <- c("P-значение равно:", as.character(pznach())) vekt
      vekt
    })
  output$int1 <-
    renderText({
      vekt <-
        c("Первый доверительный интервал равен:",
          as.character(doverit.int1()))
      vekt
    })
  output$int2 <-
    renderText({
      vekt <-
        c("Второй доверительный интервал равен:",
          as.character(doverit.int2()))
      vekt
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
      "Введите номер столбца, который содержит группирующий фактор или второй исследуемый признак",
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
      textOutput("testFishera"),
      textOutput("pvalue"),
      textOutput("int1"),
      textOutput("int2")
    ),
    p(h2("Справочная информация")),
    p(
      "F - критерий Фишера был изобретён", a("Рональдом Фишером", href = "https://en.wikipedia.org/wiki/Ronald_Fisher"), ", британским учёным статистом и генетиком. По его словам, на изобретение этого (и не только) теста его
      сподвиг забавный случай: с 1910-го по 1914-ый год Фишер работал на агробиологической станции, коллектив которой состоял преимущественно из мужчин, но однажды
      на станцию была принята Мюриэль Бристоль, ради которой было решено устраить в общей комнате традиционные чаепития. Во время одного из них начался
      привычный спор о том, как правильно разбавлять чай молоком: наливая в горячий чай холодное молоко, или в холодное молоко горячий чай? Ряд сотрудников лаборатории высказали мнение,
      что особой разницы никто не почувствует, с чем не согласилась Бристоль, сказав, что будет способна отличить 'правильный метод' (заливание в горячий чай холодного молока) от 'неправильного' (в холодный чай горячее молоко). Интересный факт,
      что правильный метод определяла британская знать, т.к. при заливании в холодное молоко горячего чая фарфоровые чашки трескались и бились."
    ),
    p("В соседней комнате приготовили разными методами несколько кружек чая, после чего Мюриэль безошибочно было определенно, какой чай как приготовлен. Фишер же задумался о том, как много кружек
      необходимо приготовить, чтобы сказать наверняка о тонкости вкуса человека, и как именно лучше анализировать полученные данные ", a ("источник", href = "https://en.wikipedia.org/wiki/Fisher%27s_exact_test")),
    p(
      "Суть критерия достаточно проста: мы сравниваем дисперсии двух разных выборок путём деления бОльшей дисперсии не меньшую",
      em(
        "(Э.В. Ивантер, А.В. Коросов Введение в количественную биологию, стр. 95)"
      )
    )
  )
  
))
shinyApp(ui = ui, server = server)
