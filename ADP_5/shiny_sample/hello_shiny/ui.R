library(shiny)

shinyUI(fluidPage(
  titlePanel("타이틀바 입니다."),
  sidebarLayout(
    sidebarPanel(p(span("사이드바", style="color:blue"),
                   "입니다."),
                 p("made by Joshua")),
    mainPanel(
      h1("메인 입니다."),
      h2("안녕하세요. 처음 시작하는 shiny app 입니다.", align="center"),
      p("HTML 태그도 몇가지는 실행이 됩니다. 전부는 아니지만 일부 문장을 작성하는데 문제가 없습니다."),
      strong("그럼 어느정도까지 실행되는지 한번 살펴볼까요?"),
      em("이탤릭체도 실행이 됩니다."),
      p("이렇게 문장 중간에",
        span("색상", style="color:red"),
        "도 변경해 줄 수 있습니다.")
    )
  )
))
