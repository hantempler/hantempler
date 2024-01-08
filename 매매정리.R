
##---- 작업폴더 세팅
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

##---- 기본라이브러리 불러오기
library(data.table)
library(magrittr)
library(stringr)
library(dplyr)
library(lubridate)

##파일불러오기

apt_price<-fread("P_2023_12.csv", skip=15 )

##데이터유형 정리

#거래금액
apt_price<-rename(apt_price,"거래금액" = "거래금액(만원)")
apt_price$거래금액<-gsub(",","",apt_price$거래금액)
apt_price$거래금액<-as.numeric(apt_price$거래금액)
table(is.na(apt_price$거래금액))

#거래년월
apt_price$계약년월<-as.character(apt_price$계약년월)
table(is.na(apt_price$계약년월))

#계약일
apt_price$계약일<-as.character(apt_price$계약일)
table(is.na(apt_price$계약일))

#전용면적
apt_price<-rename(apt_price,"전용면적" = "전용면적(㎡)")
table(is.na(apt_price$전용면적))

#단지명
apt_price$단지명 <-gsub("\\(.*","", apt_price$단지명)
table(is.na(apt_price$단지명))

#거래일 정리하기
apt_price$year<-str_sub(apt_price$계약년월,1,4)
apt_price$month<-str_sub(apt_price$계약년월,5,6)
apt_price<- rename(apt_price,"day"="계약일")

apt_price$ymd<-paste0(apt_price$year,"-",apt_price$month,"-",apt_price$day)

apt_price$ymd<-as.Date(apt_price$ymd)
apt_price$ym <- floor_date(apt_price$ymd, "month")   


##지역코드 정리

code_존재<-fread("존재.csv")
code_폐지<-fread("폐지.csv")
code<-rbind(code_존재,code_폐지)
rm(code_존재,code_폐지)

code$법정동명_1<-code$법정동명

chu_1<-c("고양시","고양군","성남시","수원시","안산시","안양시",
         "용인시","용인군","창원시","창원군","전주시","천안시","청주시",
         "포항시","부천시")


chu_2<-c("고양","고양","성남","수원","안산","안양",
         "용인","용인","창원","창원","전주","천안","청주",
         "포항","부천")


for(i in 1:length(chu_1)){
  name_1<-chu_1[i]
  name_2<-chu_2[i]
  code$법정동명_1<-sub(name_1,name_2,code$법정동명_1)
}

code$법정동명_1<-gsub(" ","",code$법정동명_1)

apt_price$시군구<-gsub("충청북도 청주상당구 북문로2가동","충청북도 청주상당구 북문로2가",apt_price$시군구)
apt_price$시군구<-gsub("충청북도 청주상당구 북문로3가동","충청북도 청주상당구 북문로3가",apt_price$시군구)
apt_price$시군구<-gsub("충청북도 청주상당구 남문로1가동","충청북도 청주상당구 남문로1가",apt_price$시군구)

juso<- apt_price%>% select(시군구)
juso<-unique(juso)
juso$시군구_1<-gsub(" ","",juso$시군구)

juso_u<-left_join(juso,code,by = c("시군구_1" = "법정동명_1"))
juso_nocode<-juso_u %>% filter(is.na(법정동코드))
juso_u_1<-distinct(juso_u,시군구, .keep_all = TRUE)
juso_u_1$법정동코드<-juso_u_1$법정동코드 %>% as.character()

#apt_price 데이터에 코드값 붙이기
apt_price<-left_join(apt_price,juso_u_1,by = "시군구")
table(is.na(apt_price$법정동코드))

#apt_price : juso_jibun 만들기
apt_price$juso_jibun<-paste0(apt_price$시군구," ",apt_price$번지," ",apt_price$단지명)
table(is.na(apt_price$juso_jibun))


#apt_price : 필요한 파일만 남기기
apt_price <- apt_price %>% select(ymd, ym, year, 법정동코드, 시군구, 단지명, 
                                  juso_jibun, 거래금액, 건축년도, 전용면적, 층)


#apt_price : 평단가 만들기
apt_price$area<-apt_price$전용면적 %>% as.numeric() %>% round(0)
apt_price$py <-round(((apt_price$거래금액/apt_price$전용면적) * 3.3),0)
apt_price<-apt_price %>% select(-전용면적)

#apt_price : 층수조정하기
min(apt_price$층)

apt_price<-rename(apt_price, "floor" = "층")
apt_price$floor<-apt_price$floor %>% as.numeric() %>% abs()
table(is.na(apt_price$floor))


#모든 거래에 1 지정

apt_price$cnt<-1


##파일이름 한꺼번에 바꾸기
colnames(apt_price) <- c("ymd","ym","year","code","addr_1","apt_nm","juso_jibun","price","conyear","area","floor","py","cnt")


#Code 숫자 조정하기
apt_price$code<-substr(apt_price$code,1,6)

# 작업공간 정리
objects_in_memory <- ls()
# 삭제하고 싶지 않은 객체의 이름
object_to_keep <- "apt_price"

# objects_in_memory에서 object_to_keep를 제외한 나머지 객체 삭제
objects_to_remove <- setdiff(objects_in_memory, object_to_keep)

# 나머지 객체 삭제
rm(list = objects_to_remove)

filename<-"202312.Rdata"
save(apt_price,file = filename)
