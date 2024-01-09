
# install.packages("rstudioapi")   # rstudioapi 설치   
#

#---# [1단계: 작업폴더 설정하기]

                      
setwd(dirname(rstudioapi::getSourceEditorContext()$path)) # 작업폴더 설정
getwd()   # 확인
library(dplyr)
library(XML)        # install.packages("XML")      
library(data.table) # install.packages("data.table")
library(stringr)    # install.packages("stringr")


#---# [2단계: 수집 대상지역 설정]

loc <- read.csv("./01_code/sigun_code/sigun_code.csv", fileEncoding="UTF-8")  #  지역코드
loc$code <- as.character(loc$code) # 행정구역명 문자 변

head(loc, 2) # 확인

#---# [3단계: 수집 기간 설정]

target_date_s<-'2024-01-01'
target_date_e<-'2024-01-31'

datelist <- seq(from = as.Date(target_date_s), # 시작
                to   = as.Date(target_date_e), # 종료
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


for(i in 1:nrow(loc)){
  for(j in 1:length(datelist)){  # 내부반복: 12개월
    cnt <- cnt + 1               # 반복누적 카운팅
    #---# 요청 목록 채우기 (25 X 12= 300)
    url_list[cnt] <- paste0("http://openapi.molit.go.kr:8081/OpenAPI_ToolInstallPackage/service/rest/RTMSOBJSvc/getRTMSDataSvcAptRent?",
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

raw_data <- list()        # xml 임시 저장소
root_Node <- list()       # 거래내역 추출 임시 저장소
total <- list()           # 거래내역 정리 임시 저장소
dirpath<-paste0(getwd(),"/02_rent_raw_data")
unlink(dirpath, recursive = TRUE)
dir.create("02_rent_raw_data") # 새로운 폴더 만들기

#---# [2단계: URL 요청 - XML 응답]

for(i in 1:length(url_list)){   # 요청목록(url_list) 반복
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
        item_temp_dt <- data.table(renewal = item_temp[1],     # 거래 년 
                                   conyear = item_temp[2],    # 거래 월
                                   new_old = item_temp[3],      # 거래 일
                                   term = item_temp[4],    # 거래금액
                                   year = item_temp[5],    # 지역코드
                                   dong = item_temp[6],  # 법정동
                                   deposit = item_temp[7],   # 지번
                                   apt_nm = item_temp[8], # 건축연도 
                                   month = item_temp[9],   # 아파트 이름   
                                   rent_fee = item_temp[10],     # 전용면적
                                   day = item_temp[11],     # 전용면적
                                   area = item_temp[12],     # 전용면적
                                   f_deposit = item_temp[13],     # 전용면적
                                   f_rent_fee = item_temp[14],     # 전용면적
                                   jibun = item_temp[15],     # 전용면적
                                   code = item_temp[16],     # 전용면적
                                   floor = item_temp[17])   # 층수 
        item[[m]] <- item_temp_dt}    # 분리된 거래내역 순서대로 저장
      apt_bind <- rbindlist(item)     # 통합 저장
      
      #---# [5단계: 응답 내역 저장]
      
      region_nm <- subset(loc, code== str_sub(url_list[i],114, 118))$addr_1 # 지역명 추출
      month <- str_sub(url_list[i],129, 134)   # 연월(YYYYMM) 추출
      path <- as.character(paste0("./02_rent_raw_data/", region_nm, "_", month,".csv")) # 저장위치 설정
      write.csv(apt_bind, path)     # csv 저장
      msg <- paste0("[", i,"/",length(url_list), "] 수집한 데이터를 [", path,"]에 저장 합니다.") # 알림 메시지
      cat(msg, "\n\n")
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}    # 바깥쪽 반복문 종료


#----------
# 3-4 통합
#---------- 

#---# [1단계: csv 통합]

setwd(dirname(rstudioapi::getSourceEditorContext()$path))  # 작업폴더 설정
files <- dir("./02_rent_raw_data")    # 폴더 내 모든 파일 이름 읽기
library(plyr)               # install.packages("plyr")
apt_rent <- ldply(as.list(paste0("./02_rent_raw_data/", files)), read.csv) # 모든 파일 하나로 결합
tail(apt_rent, 2)  # 확인

#---# [2단계: 저장]
dirpath<-paste0(getwd(),"/03_integrated")
unlink(dirpath, recursive = TRUE)
dir.create("./03_integrated")   # 새로운 폴더 생성
save(apt_rent, file = "./03_integrated/03_apt_rent.rdata") # 저장
write.csv(apt_rent, "./03_integrated/03_apt_rent.csv")   

#--------------------------
# 4-1 불필요한 정보 지우기
#--------------------------

#---# [1단계: 자료 불러오기]

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
options(warn=-1)

load("./03_integrated/03_apt_rent.rdata")  # 실거래 자료 불러오기
head(apt_rent, 2)                          # 확인

#---# [2단계: 결측값 확인]

#table(is.na(apt_rent))         # 현재 결측값 확인
#apt_rent <- na.omit(apt_rent) # 결측값 제거
#table(is.na(apt_rent))         # 결측값 제거 확인

#head(apt_rent$price, 2)      # 현재 결측값 확인  

library(stringr)              # 문자열 처리 패키지 실행
apt_rent <- as.data.frame(apply(apt_rent, 2, str_trim)) # 공백 제거
head(apt_rent$deposit, 2)                                  # 확인

#--------------------------
# 4-2 항목별 데이터 다듬기
#--------------------------

#---# [1단계: 매매 연월일, 연월 데이터 만들기]

library(lubridate)  # install.packages("lubridate") / install.packages("cli") 
library(dplyr)      # install.packages("dplyr")
apt_rent <- apt_rent %>% mutate(ymd=make_date(year, month, day))  # 연월일
apt_rent$ym <- floor_date(apt_rent$ymd, "month")                  # 연월
head(apt_rent, 2)                                                  # 확인

#---# [2단계: 매매가 확인]

head(apt_rent$deposit, 3)

apt_rent$deposit <- apt_rent$deposit %>% sub(",","",.) %>% as.numeric() # 매매가 변환(문자 → 숫자)
apt_rent$f_deposit <- apt_rent$f_deposit %>% sub(",","",.) %>% as.numeric() # 매매가 변환(문자 → 숫자)
apt_rent$rent_fee <- apt_rent$rent_fee %>% sub(",","",.) %>% as.numeric() # 매매가 변환(문자 → 숫자)
apt_rent$f_rent_fee <- apt_rent$f_rent_fee %>% sub(",","",.) %>% as.numeric() # 매매가 변환(문자 → 숫자)

#---# [3단계: 주소 조합]

head(apt_rent$apt_nm, 30)  # 아파트 이름 현황

apt_rent$apt_nm <- gsub("\\(.*","", apt_rent$apt_nm) # 괄호이후 삭제
head(apt_rent$apt_nm, 30)                             # 아파트 이름 확인

loc <- read.csv("./01_code/sigun_code/sigun_code.csv", fileEncoding="UTF-8")  # 지역코드 불러오기

apt_rent <- merge(apt_rent, loc, by = 'code')         # 지역명 결합하기
apt_rent$juso_jibun <- paste0(apt_rent$addr_2, " ", apt_rent$dong," ",
                              apt_rent$jibun," ",apt_rent$apt_nm) # 주소조합
head(apt_rent, 2)                                      # 확인

#---# [4단계: 건축연도 변환]

head(apt_rent$conyear, 3)

apt_rent$conyear <- apt_rent$conyear %>% as.numeric()   # 건축연도 숫자변환
head(apt_rent$conyear, 3)   # 건축연도 확인

#---# [5단계: 평당 매매가 만들기]

head(apt_rent$area, 3)   # 확인

apt_rent$area <- apt_rent$area %>% as.numeric() %>% round(0)  # 전용면적 숫자변환
head(apt_rent$area, 3)          # 확인

apt_rent$py <- ifelse(apt_rent$rent_fee == 0,round(((apt_rent$deposit/apt_rent$area) * 3.3), 0),"") %>% as.numeric() # 평당가격 계산
head(apt_rent$py, 3)           # 확인


#---# [6단계: 층수 변환]

min(apt_rent$floor)   # 확인
#apt_rent <- rename(apt_rent, "floor" = "층")


apt_rent$floor <- apt_rent$floor %>% as.numeric() %>% abs() # 층수 숫자변환
min(apt_rent$floor)

apt_rent$cnt <- 1   # 모든 데이터에 숫자 1 할당
head(apt_rent, 2)   # 확인



#---------------
# 4-3 저장하기
#---------------

#---# [1단계: 필요칼럼 추출]

apt_rent_all <- apt_rent %>% select(ymd, ym, year, code, addr_1, apt_nm, 
                                    juso_jibun, deposit, rent_fee, conyear,  area, floor, py,cnt) # 칼럼 추출


#head(apt_rent1, 2)  # 확인

#---# [2단계: 저장]
dirpath<-paste0(getwd(),"/04_preprocess")
unlink(dirpath, recursive = TRUE)
dir.create("./04_preprocess")   # 새로운 폴더 생성

save(apt_rent_all, file = "./04_preprocess/04_apt_rent_all.rdata") # 저장
write.csv(apt_rent_all, "./04_preprocess/04_apt_rent_all.csv") 

load("./04_preprocess/04_apt_rent_all.rdata")
load("./rent_2023.rdata")  # 실거래 자료 불러오기
rent_2023<-rbind(rent_2023,apt_rent_all )

rent_2023$py <- ifelse(rent_2023$rent_fee == 0,round(((rent_2023$deposit/rent_2023$area) * 3.3), 0),"") %>% as.numeric() # 평당가격 계산

save(rent_2023, file = "./rent_2023.rdata")



