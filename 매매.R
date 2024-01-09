#------------------------------------
# 03-1 크롤링 준비: 무엇을 준비할까?
#------------------------------------  

#---# [1단계: 작업폴더 설정하기]

# install.packages("rstudioapi")   # rstudioapi 설치                         
setwd(dirname(rstudioapi::getSourceEditorContext()$path)) # 작업폴더 설정
getwd()   # 확인

#---# [2단계: 수집 대상지역 설정]

loc <- read.csv("./01_code/sigun_code/sigun_code.csv", fileEncoding="UTF-8")  #  지역코드
loc$code <- as.character(loc$code) # 행정구역명 문자 변환     
head(loc, 2) # 확인


#---# [3단계: 수집 기간 설정]

datelist <- seq(from = as.Date('2024-01-01'), # 시작
                to   = as.Date('2024-01-31'), # 종료
                by    = '1 month')            # 단위
datelist <- format(datelist, format = '%Y%m') # 형식변환(YYYY-MM-DD => YYYYMM) 
datelist[1:3]          # 확인


#---# [4단계: 인증키 입력하기]

service_key <- "cyquWr9Wfk59TupGu4hiOUNQzg4SxLBUUrDFZDXf5xNR%2B%2FJGexr%2FGqrqZ%2FY8ZzIvWr1ZBWrHHRZoHnKAbDUxAQ%3D%3D"  # 인증키 입력


#--------------------------------------------------
# 3-2 요청목록 생성: 자료를 어떻게 요청할까?
#--------------------------------------------------

#---# [1단계: 요청목록 만들기]

url_list <- list() # 빈 리스트 만들기
cnt <-0	           # 반복문의 제어 변수 초깃값 설정

#---# [2단계: 요청목록 채우기]

for(i in 1:nrow(loc)){           # 외부반복: 25개 자치구
  for(j in 1:length(datelist)){  # 내부반복: 12개월
    cnt <- cnt + 1               # 반복누적 카운팅
    #---# 요청 목록 채우기 (25 X 12= 300)
    url_list[cnt] <- paste0("http://openapi.molit.go.kr:8081/OpenAPI_ToolInstallPackage/service/rest/RTMSOBJSvc/getRTMSDataSvcAptTrade?",
                            "LAWD_CD=", loc[i,1],         # 지역코드
                            "&DEAL_YMD=", datelist[j],    # 수집월
                            "&numOfRows=", 100,           # 한번에 가져올 최대 자료 수
                            "&serviceKey=", service_key)  # 인증키
  } 
  Sys.sleep(0.1)   # 0.1초간 멈춤
  msg <- paste0("[", i,"/",nrow(loc), "]  ", loc[i,4], " 의 크롤링 목록이 생성됨 => 총 [", cnt,"] 건") # 알림 메시지
  cat(msg, "\n\n") 
}

#---# [3단계: 요청 목록 동작 확인]

length(url_list)                # 요청목록 갯수 확인
browseURL(paste0(url_list[1]))  # 정상작동 확인(웹브라우저 실행)


#----------------
# 3-3 크롤링 실행
#----------------  

#---# [1단계: 임시 저장 리스트 생성]

library(XML)        # install.packages("XML")      
library(data.table) # install.packages("data.table")
library(stringr)    # install.packages("stringr")

raw_data <- list()        # xml 임시 저장소
root_Node <- list()       # 거래내역 추출 임시 저장소
total <- list()           # 거래내역 정리 임시 저장소

dirpath<-paste0(getwd(),"/02_raw_data")
unlink(dirpath, recursive = TRUE)

dir.create("02_raw_data") # 새로운 폴더 만들기

#---# [2단계: URL 요청 - XML 응답]

for(i in 1:length(url_list)){
  tryCatch(
    {
      raw_data[[i]] <- xmlTreeParse(url_list[i], useInternalNodes = TRUE,encoding = "utf-8") # 결과 저장
      root_Node[[i]] <- xmlRoot(raw_data[[i]])	# xmlRoot로 추출
      
      #---# [3단계: 전체 거래 건수 확인]
      
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
                                   jibun = item_temp[12],   # 지번
                                   con_year = item_temp[3], # 건축연도 
                                   apt_nm = item_temp[7],   # 아파트 이름   
                                   area = item_temp[10],     # 전용면적
                                   floor = item_temp[14])   # 층수 
        item[[m]] <- item_temp_dt}
      
      apt_bind <- rbindlist(item)     # 통합 저장
      
      #---# [5단계: 응답 내역 저장]
      
      region_nm <- subset(loc, code== str_sub(url_list[i],115, 119))$addr_1 # 지역명 추출
      month <- str_sub(url_list[i],130, 135)   # 연월(YYYYMM) 추출
      path <- as.character(paste0("./02_raw_data/", region_nm, "_", month,".csv")) # 저장위치 설정
      write.csv(apt_bind, path)     # csv 저장
      msg <- paste0("[", i,"/",length(url_list), "] 수집한 데이터를 [", path,"]에 저장 합니다.") # 알림 메시지
      cat(msg, "\n\n")
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}   # 바깥쪽 반복문 종료


#----------
# 3-4 통합
#---------- 

#---# [1단계: csv 통합]

setwd(dirname(rstudioapi::getSourceEditorContext()$path))  # 작업폴더 설정
files <- dir("./02_raw_data")    # 폴더 내 모든 파일 이름 읽기
library(plyr)               # install.packages("plyr")
apt_price <- ldply(as.list(paste0("./02_raw_data/", files)), read.csv) # 모든 파일 하나로 결합
tail(apt_price, 2)  # 확인

#---# [2단계: 저장]

dirpath<-paste0(getwd(),"/03_integrated/p")
unlink(dirpath, recursive = TRUE)
dir.create("./03_integrated/p")   # 새로운 폴더 생성

save(apt_price, file = "./03_integrated/p/03_apt_price.rdata") # 저장
write.csv(apt_price, "./03_integrated/p/03_apt_price.csv")   


#--------------------------
# 4-1 불필요한 정보 지우기
#--------------------------

#---# [1단계: 아파트 실거래 자료 불러오기]

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
options(warn=-1)

load("./03_integrated/p/03_apt_price.rdata")  # 실거래 자료 불러오기
head(apt_price, 2)                          # 확인

#---# [2단계: 결측값 확인]

table(is.na(apt_price))         # 현재 결측값 확인
#apt_price <- na.omit(apt_price) # 결측값 제거
table(is.na(apt_price))         # 결측값 제거 확인

#head(apt_price$price, 2)      # 현재 결측값 확인  

library(stringr)              # 문자열 처리 패키지 실행
apt_price <- as.data.frame(apply(apt_price, 2, str_trim)) # 공백 제거
head(apt_price$price, 2)                                  # 확인

#--------------------------
# 4-2 항목별 데이터 다듬기
#--------------------------

#---# [1단계: 매매 연월일, 연월 데이터 만들기]

library(lubridate)  # install.packages("lubridate") / install.packages("cli") 
library(dplyr)      # install.packages("dplyr")
apt_price <- apt_price %>% mutate(ymd=make_date(year, month, day))  # 연월일
apt_price$ym <- floor_date(apt_price$ymd, "month")                  # 연월
head(apt_price, 2)                                                  # 확인

#---# [2단계: 매매가 확인]

head(apt_price$price, 3)

apt_price$price <- apt_price$price %>% sub(",","",.) %>% as.numeric() # 매매가 변환(문자 → 숫자)
head(apt_price$price, 3)  # 확인

#---# [3단계: 주소 조합]

#head(apt_price$apt_nm, 30)  # 아파트 이름 현황

apt_price$apt_nm <- gsub("\\(.*","", apt_price$apt_nm) # 괄호이후 삭제
#head(apt_price$apt_nm, 30)                             # 아파트 이름 확인

#loc <- read.csv("./01_code/sigun_code/sigun_code.csv", fileEncoding="UTF-8")  # 지역코드 불러오기
apt_price <- merge(apt_price, loc, by = 'code')         # 지역명 결합하기
apt_price$juso_jibun <- paste0(apt_price$addr_2, " ", apt_price$dong," ",
                               apt_price$jibun," ",apt_price$apt_nm) # 주소조합
head(apt_price, 2)                                      # 확인

#---# [4단계: 건축연도 변환]

#head(apt_price$con_year, 3)

apt_price$con_year <- apt_price$con_year %>% as.numeric()   # 건축연도 숫자변환
#head(apt_price$con_year, 3)   # 건축연도 확인

#---# [5단계: 평당 매매가 만들기]

#head(apt_price$area, 3)   # 확인

apt_price$area <- apt_price$area %>% as.numeric() %>% round(0)  # 전용면적 숫자변환
#head(apt_price$area, 3)          # 확인

apt_price$py <- round(((apt_price$price/apt_price$area) * 3.3), 0) # 평당가격 계산
#head(apt_price$py, 3)           # 확인


#---# [6단계: 층수 변환]

#min(apt_price$floor)   # 확인

apt_price$floor <- apt_price$floor %>% as.numeric() %>% abs() # 층수 숫자변환
#min(apt_price$floor)

apt_price$cnt <- 1   # 모든 데이터에 숫자 1 할당
#head(apt_price, 2)   # 확인


#---------------
# 4-3 저장하기
#---------------

#---# [1단계: 필요칼럼 추출]

apt_price <- apt_price %>% select(ymd, ym, year, code, addr_1, apt_nm, 
                                  juso_jibun, price, con_year,  area, floor, py, cnt) # 칼럼 추출
#head(apt_price, 2)  # 확인

#---# [2단계: 저장]

#setwd(dirname(rstudioapi::getSourceEditorContext()$path))

dirpath<-paste0(getwd(),"/04_preprocess/p")
unlink(dirpath, recursive = TRUE)

dir.create("./04_preprocess/p")   # 새로운 폴더 생성
save(apt_price, file = "./04_preprocess/p/04_price.rdata") # 저장
write.csv(apt_price, "./04_preprocess/p/04_price.csv") 
write.csv(apt_price, "./04_preprocess/p/04_price.csv", row.names = F, fileEncoding='CP949')

apt_price$n_code<-paste0(apt_price$code,"_",apt_price$apt_nm,"_",apt_price$area)


apt_price_n<-left_join(apt_price, max, by = "n_code" )
