library(googleVis)
library(lubridate)
subway_specific_gwanghwa = subset(subway, stat_name=="광화문")
## 데이터타입을 date로 변환하는 함수 : ymd, 변환안해주면 차트 그릴 때 에러 뜸

subway_specific_gwanghwa$income_date = ymd(subway_specific_gwanghwa$income_date) 
Subway_graph <- gvisCalendar(subway_specific_gwanghwa, 
                             datevar="income_date", 
                             numvar="on_tot",
                             options=list(
                               title="Daily traffic in Gwanghwa",
                               height=320,
                               calendar="{yearLabel: { fontName: 'Times-Roman',
                               fontSize: 32, color: '#1A8763', bold: true},
                               cellSize: 10,
                               cellColor: { stroke: 'red', strokeOpacity: 0.2 },
                               focusedCellColor: {stroke:'red'}}")
                             )
plot(Subway_graph)

subway_specific_dap = subset(subway, stat_name=="답십리")
subway_specific_dap$income_date = ymd(subway_specific_dap$income_date)
Subway_graph <- gvisCalendar(subway_specific_dap, 
                             datevar="income_date", 
                             numvar="on_tot",
                             options=list(
                               title="Daily traffic in Dapsipri",
                               height=320,
                               calendar="{yearLabel: { fontName: 'Times-Roman',
                               fontSize: 32, color: '#1A8763', bold: true},
                               cellSize: 10,
                               cellColor: { stroke: 'red', strokeOpacity: 0.2 },
                               focusedCellColor: {stroke:'red'}}")
                             )
plot(Subway_graph)