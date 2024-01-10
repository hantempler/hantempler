a <- seq(from = 1, to =20000, by=1000)
b <- seq(from = 1000, to =20000, by=1000)



a_1<-as.integer()
b_1<-data.frame(b)
c<-cbind(a,b)
c<-data.frame(c)

c$a<-as.integer(c$a)
c$b<-as.integer(c$b)


cnt<-0
url_list<-list()

for(i in 1:nrow(c)){
  cnt <- cnt+1
  url_list[cnt] <- paste0("https://apis.data.go.kr/1613000/AptListService2/getTotalAptList?serviceKey=cyquWr9Wfk59TupGu4hiOUNQzg4SxLBUUrDFZDXf5xNR%2B%2FJGexr%2FGqrqZ%2FY8ZzIvWr1ZBWrHHRZoHnKAbDUxAQ%3D%3D&pageNo=1&numOfRows=",
                          c[i,2])
  } 


library(XML)        # install.packages("XML")      
library(data.table) # install.packages("data.table")
library(stringr)    # install.packages("stringr")

raw_data <- list()        # xml 임시 저장소
root_Node <- list()       # 거래내역 추출 임시 저장소
total <- list()           # 거래내역 정리 임시 저장소

for(i in 1:length(url_list)){
  tryCatch(
    {
      raw_data[[i]] <- xmlTreeParse(url_list[i], useInternalNodes = TRUE,encoding = "utf-8") # 결과 저장
      root_Node[[i]] <- xmlRoot(raw_data[[i]])	# xmlRoot로 추출
      items <- root_Node[[i]][[2]][['items']]  # 전체 거래내역(items) 추출
      size <- xmlSize(items)                   # 전체 거래 건수 확인    
      
      #---# [4단계: 거래 내역 추출]
      
      item <- list()  # 전체 거래내역(items) 저장 임시 리스트 생성
      item_temp_dt <- data.table()  # 세부 거래내역(item) 저장 임시 테이블 생성
      Sys.sleep(.1)  # 0.1초 멈춤
      for(m in 1:size){  # 전체 거래건수(size)만큼 반복
        #---# 세부 거래내역 분리   
        item_temp <- xmlSApply(items[[m]],xmlValue)
        item_temp_dt <- data.table(year = item_temp[4],     # 거래 년 
                                   month = item_temp[8],    # 거래 월
                                   day = item_temp[9],      # 거래 일
                                   price = item_temp[1],    # 거래금액
                                   code = item_temp[13],    # 지역코드
                                   dong_nm = item_temp[6],  # 법정동
                                   jibun = item_temp[12]   # 지번
                                   )   # 층수 
        item[[m]] <- item_temp_dt}
      
      apt_bind <- rbindlist(item)     # 통합 저장
      
      #---# [5단계: 응답 내역 저장]
      
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}   # 바깥쪽 반복문 종료



