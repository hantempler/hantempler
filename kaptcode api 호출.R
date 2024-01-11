cnt<-0
url_list<-list()

for(i in 1:nrow(code_all)){
  cnt <- cnt+1
  url_list[cnt] <- paste0("http://apis.data.go.kr/1613000/AptBasisInfoService1/getAphusBassInfo?serviceKey=cyquWr9Wfk59TupGu4hiOUNQzg4SxLBUUrDFZDXf5xNR%2B%2FJGexr%2FGqrqZ%2FY8ZzIvWr1ZBWrHHRZoHnKAbDUxAQ%3D%3D&kaptCode="
                          ,code_all[i,kaptcode])
} 


library(XML)        # install.packages("XML")      
library(data.table) # install.packages("data.table")
library(stringr)    # install.packages("stringr")

raw_data <- list()        # xml 임시 저장소
root_Node <- list()       # 거래내역 추출 임시 저장소
total <- list()           # 거래내역 정리 임시 저장소

#---# [2단계: URL 요청 - XML 응답]

for(i in 1:length(url_list)){   # 요청목록(url_list) 반복
  tryCatch(
    {
      raw_data[[i]] <- xmlTreeParse(url_list[i], useInternalNodes = TRUE,encoding = "utf-8") # 결과 저장
      root_Node[[i]] <- xmlRoot(raw_data[[i]])	# xmlRoot로 추출
      items <- root_Node[[i]][[2]][['item']]  # 전체 거래내역(items) 추출
      size <- xmlSize(items)                   # 전체 거래 건수 확인    
      item <- data.table()  # 전체 거래내역(items) 저장 임시 리스트 생성
      item_temp_dt <- data.table()  # 세부 거래내역(item) 저장 임시 테이블 생성
      Sys.sleep(.1)  # 0.1초 멈춤
        for(m in 1:length(url_list)){  # 전체 거래건수(size)만큼 반복
        #---# 세부 거래내역 분리   
          item_temp <- xmlSApply(items,xmlValue)
          item_temp_dt <- data.table(bjdCode = item_temp[[1]],     # 거래 년 
                                     conyear = item_temp[[2]],    # 거래 월
                                     new_old = item_temp[[3]],      # 거래 일
                                     term = item_temp[[4]],    # 거래금액
                                     year = item_temp[[5]],    # 지역코드
                                     dong = item_temp[[6]]  # 법정동
                                     )   # 층수 
          item[[m]] <- item_temp_dt}    # 분리된 거래내역 순서대로 저장
        code_bind <- rbindlist(item)     # 통합 저장
      
      #---# [5단계: 응답 내역 저장]
      
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}    # 바깥쪽 반복문 종료


### 개발자의 변

- item_temp <- xmlSApply(items,xmlValue) 33번 행에서 문제가 생겼다. 
  실거래가 데이터에서 불러오던 방식대로, 코드를 구성하였으나,
  문제는 items에 포함된 item의 개수였다.
  
  이번 API는 단 한개의 item으로만 구성되는 형태로,
  실거래는 많은 item을 포함한다.
  
  기존의 방식대로 item_temp <- xmlSApply(items[[m]],xmlValue) 식을 구성해버리면
  31개의 구성항목을 계속해서 루프한다.
  
  그래서 식을 변경했다.
  
  xml 구조를 잘알지 못하기에 범한 삽질이다.
  
  그래도 알았으니 다행이다.
