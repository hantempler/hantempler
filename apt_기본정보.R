c<-c(1:200)

cnt<-0
url_list<-list()

for(i in 1:length(c)){
  cnt <- cnt+1
  url_list[cnt] <- paste0("http://apis.data.go.kr/1613000/AptListService2/getTotalAptList?serviceKey=cyquWr9Wfk59TupGu4hiOUNQzg4SxLBUUrDFZDXf5xNR%2B%2FJGexr%2FGqrqZ%2FY8ZzIvWr1ZBWrHHRZoHnKAbDUxAQ%3D%3D&pageNo=",
                          c[i],
                          "&numOfRows=100")
} 


library(XML)        # install.packages("XML")      
library(data.table) # install.packages("data.table")
library(stringr)    # install.packages("stringr")

raw_data <- list()        # xml 임시 저장소
root_Node <- list()       # 거래내역 추출 임시 저장소
total <- list()           # 거래내역 정리 임시 저장소
aptcode<-data.table()

for(i in 197:200){
  tryCatch(
    {
      raw_data[[i]] <- xmlTreeParse(url_list[i], useInternalNodes = TRUE,encoding = "utf-8") # 결과 저장
      root_Node[[i]] <- xmlRoot(raw_data[[i]])	# xmlRoot로 추출
      items <- root_Node[[i]][[2]][['items']]  # 전체 거래내역(items) 추출
      size <- xmlSize(items)                   # 전체 거래 건수 확인    
      
      #---# [4단계: 거래 내역 추출]
      
      item <- list()  # 전체 거래내역(items) 저장 임시 리스트 생성
      item_temp_dt <- data.table()  # 세부 거래내역(item) 저장 임시 테이블 생성
      Sys.sleep(.5)  # 0.1초 멈춤
      for(m in 1:size){  # 전체 거래건수(size)만큼 반복
        #---# 세부 거래내역 분리   
        item_temp <- xmlSApply(items[[m]],xmlValue)
        
        if (length(item_temp) == 7){
          item_temp_dt <- data.table(as1 = item_temp[1],     # 거래 년 
                                     as2 = item_temp[2],    # 거래 월
                                     as3 = item_temp[3],      # 거래 일
                                     as4 = item_temp[4],    # 거래금액
                                     bjdcode = item_temp[5],    # 지역코드
                                     kaptcode = item_temp[6],  # 법정동
                                     kaptname = item_temp[7]
          )
        }
        else if(length(item_temp) == 6){
          item_temp_dt <- data.table(as1 = item_temp[1],     # 거래 년 
                                     as2 = item_temp[2],    # 거래 월
                                     as3 = item_temp[3],      # 거래 일
                                     as4 = "",    # 거래금액
                                     bjdcode = item_temp[4],    # 지역코드
                                     kaptcode = item_temp[5],  # 법정동
                                     kaptname = item_temp[6]
          )
        }
        
        item[[m]] <- item_temp_dt}
      
      apt_bind <- rbindlist(item)     # 통합 저장
      assign(paste0("apt_bind_", i), apt_bind)
      save(apt_bind, file = paste0("apt_bind_", i, ".rdata"))
      msg <- paste0("[", i,"/",length(url_list), "] 수집한 데이터를 테이블에 결합한 후 저장합니다.") # 알림 메시지
      cat(msg, "\n\n")
           #---# [5단계: 응답 내역 저장]
           
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}   # 바깥쪽 반복문 종료


folder_path <- "C:/Users/john/Desktop/새 폴더/02"
file_list <- list.files(path = folder_path, pattern = "\\.rdata$", full.names = TRUE )

code<-data.table()
apt_code<-data.table()

code_all<-data.table()
code_all$as1<-data.table()
code_all$as2<-data.table()
code_all$as3<-data.table()
code_all$as4<-data.table()
code_all$bjdcode<-data.table()
code_all$kaptcode<-data.table()
code_all$kaptname<-data.table()

for (i in 1:length(file_list)) {
  load(file_list[i])
  code_all<- rbind(code_all,apt_bind)
}
