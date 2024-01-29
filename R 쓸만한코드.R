##참고할 코드 모음

#최고값 산출

max_price <- apt_price %>%
  group_by(n_code) %>%
  arrange(price) %>%
  slice(1)

max_price <- apt_price %>%
  group_by(n_code) %>%
  slice(which.max(price))


#최저값 산출
min_price <- apt_price %>%
  group_by(n_code) %>%
  slice(which.min(price))


#거래건 합산

거래량_연간 <- apt_price %>%
  group_by(n_code) %>%
  summarise(거래량 = sum(cnt))


#값 바꾸기 : 특정단어, 다른 단어로 바꾸기

code$법정동명_1<-gsub(" ","",code$법정동명_1)
apt_price_1$juso_jibun<-gsub("강원도","강원특별자치도",apt_price_1$juso_jibun)
apt_price_1$addr_1<-gsub("강원도","강원특별자치도",apt_price_1$addr_1)

# 특정폴더에 있는 파일명 불러오기

folder_path <- "C:/Users/USER/Documents/R/작업데이터/1. 아파트매매/01_code/sigun_grid"

file_list <- list.files(path = folder_path, pattern = "\\.Rdata$", full.names = TRUE )
file_list <- list.files(path = "C:/Users/USER/Documents", pattern = "\\.jpg$", full.names = TRUE)

# 열값 중 특정단어가 포함된 행만 골라내기

jibun2<-jibun2 %>% filter(!grepl("BL|null",번지))

#열이름 바꾸기



#저장하기

save(apt_price, file = "./04_preprocess/p/04_price.rdata") # 저장
write.csv(apt_price, "./04_preprocess/p/04_price.csv") 
write.csv(apt_price, "./04_preprocess/p/04_price.csv", row.names = F, fileEncoding='CP949')

# 괄호이후 삭제
apt_rent$apt_nm <- gsub("\\(.*","", apt_rent$apt_nm) 


# 여러파일을 불러와서 읽고, 다시 저장하기

folder_path <- "C:/Users/USER/Documents/R/작업데이터/1. 아파트매매/01_code/sigun_grid"
file_list <- list.files(path = folder_path, pattern = "\\.Rdata$", full.names = TRUE )

file_name<-as.data.frame(file_list)
file_name$file_list<-gsub(".Rdata","",file_name$file_list)
file_name$file_list<-gsub(".rdata","",file_name$file_list)
file_name$file_list<-gsub("do_","",file_name$file_list)
file_name$file_list<-gsub("C:/Users/USER/Documents/R/작업데이터/1. 아파트매매/01_code/sigun_grid/","",file_name$file_list)

file_name$sig<-str_sub(file_name$file_list,1,4)
file_name$지역<-str_sub(file_name$file_list,6,)
file_name<-file_name %>% select(-file_list)

grid_all<-data.frame()
grid_all$geometry<-data.frame()
grid_all$sig<-data.frame()
grid_all$지역<-data.frame()

for (i in 1:length(file_list)) {
  load(file_list[i])
  grid<- grid %>% st_sf 
  grid$sig<-c(file_name[i,1])
  grid$지역<-c(file_name[i,2])
  grid<-grid %>% st_cast()
  path <- as.character(paste0(file_name[i,1], "_",file_name[i,2],".rdata"))
  save(grid, file = path)
}


# 현재연도를 추가하기

current_year <- as.numeric(format(Sys.Date(), "%Y"))
apt_price$c_year <- current_year



## 월별 거래건수 취합


# 필요한 패키지 로드
library(dplyr)
library(tidyr)
library(lubridate)

# 품목별 월 판매 건수 취합
monthly_sales <- sales_data %>%
  group_by(Item, Month) %>%
  summarise(TotalSales = sum(Sales))

# spread 함수 사용 (Month 열이 이미 존재)
monthly_sales_wide <- spread(monthly_sales, key = Month, value = TotalSales, fill = 0)

# 결과 출력
print(monthly_sales_wide)


## 특정시점에서 현재일까지의 거래건수 계산


# 필요한 패키지 로드
library(dplyr)
library(tidyr)
library(lubridate)


# 현재 날짜 계산
current_date <- Sys.Date()

# 품목별 월 판매 건수 취합
monthly_1year <- apt_price %>%
  filter(ymd >= (current_date %m-% months(12))) %>% 
  group_by(n_code) %>%
  summarise(year = sum(cnt))

# spread 함수 사용 (Month 열이 이미 존재)
monthly_sales_wide <- spread(monthly_sales, key = Month, value = TotalSales, fill = 0)

# 결과 출력
print(monthly_sales_wide)


# 중복된 행 지우기, 남겨놓고

conyear<-distinct(conyear,juso,.keep_all=TRUE) 

# 열이름 한꺼번에 바꾸기
colnames(df) <- new_col_names
