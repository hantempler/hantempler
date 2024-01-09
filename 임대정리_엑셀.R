setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# readxl 패키지 로드
library(readxl)
library(data.table)
library(magrittr)
library(stringr)
library(dplyr)
library(lubridate)

##파일불러오기

filename<-"202312.xlsx"
savename<-paste0(str_sub(filename,1,6),".","rdata")
apt_rent <- read_excel(filename, skip = 15)


##데이터유형 정리

#거래금액
apt_rent<-rename(apt_rent,"보증금" = "보증금(만원)")
apt_rent$보증금<-gsub(",","",apt_rent$보증금)
apt_rent$보증금<-as.numeric(apt_rent$보증금)
table(is.na(apt_rent$보증금))

apt_rent<-rename(apt_rent,"월세" = "월세(만원)")
apt_rent$월세<-gsub(",","",apt_rent$월세) %>% as.numeric()
table(is.na(apt_rent$월세))

#거래년월
apt_rent$계약년월<-as.character(apt_rent$계약년월)
table(is.na(apt_rent$계약년월))

#계약일
apt_rent$계약일<-as.character(apt_rent$계약일)
table(is.na(apt_rent$계약일))

#전용면적
apt_rent<-rename(apt_rent,"전용면적" = "전용면적(㎡)")
table(is.na(apt_rent$전용면적))

#단지명
apt_rent$단지명 <-gsub("\\(.*","", apt_rent$단지명)
table(is.na(apt_rent$단지명))

#거래일 정리하기
apt_rent$year<-str_sub(apt_rent$계약년월,1,4)
apt_rent$month<-str_sub(apt_rent$계약년월,5,6)
apt_rent<- rename(apt_rent,"day"="계약일")

apt_rent$ymd<-paste0(apt_rent$year,"-",apt_rent$month,"-",apt_rent$day)

apt_rent$ymd<-as.Date(apt_rent$ymd)
apt_rent$ym <- floor_date(apt_rent$ymd, "month")   

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

apt_rent$시군구<-gsub("충청북도 청주상당구 북문로2가동","충청북도 청주상당구 북문로2가",apt_rent$시군구)
apt_rent$시군구<-gsub("충청북도 청주상당구 북문로3가동","충청북도 청주상당구 북문로3가",apt_rent$시군구)
apt_rent$시군구<-gsub("충청북도 청주상당구 남문로1가동","충청북도 청주상당구 남문로1가",apt_rent$시군구)

juso<- apt_rent%>% select(시군구)
juso<-unique(juso)
juso$시군구_1<-gsub(" ","",juso$시군구)

juso_u<-left_join(juso,code,by = c("시군구_1" = "법정동명_1"))
juso_nocode<-juso_u %>% filter(is.na(법정동코드))
juso_u_1<-distinct(juso_u,시군구, .keep_all = TRUE)
juso_u_1$법정동코드<-juso_u_1$법정동코드 %>% as.character()

#apt_rent 데이터에 코드값 붙이기
apt_rent<-left_join(apt_rent,juso_u_1,by = "시군구")
table(is.na(apt_rent$법정동코드))

#apt_rent : juso_jibun 만들기
apt_rent$juso_jibun<-paste0(apt_rent$시군구," ",apt_rent$번지," ",apt_rent$단지명)
table(is.na(apt_rent$juso_jibun))


#apt_rent : 필요한 파일만 남기기
apt_rent <- apt_rent %>% select(ymd, ym, year, 법정동코드, 시군구, 단지명, 
                                juso_jibun, 보증금, 월세, 건축년도, 전용면적, 층)


#apt_rent : 평단가 만들기
apt_rent$area<-apt_rent$전용면적 %>% as.numeric() %>% round(0)
apt_rent$py <-ifelse(apt_rent$월세<=0,round(((apt_rent$보증금/apt_rent$area) * 3.3),0),"")
apt_rent<-apt_rent %>% select(-전용면적)

#apt_rent : 층수조정하기
min(apt_rent$층)
apt_rent<-rename(apt_rent, "floor" = "층")
apt_rent$floor<-apt_rent$floor %>% as.numeric() %>% abs()
table(is.na(apt_rent$floor))

#모든 거래에 1 지정

apt_rent$cnt<-1


#파일이름 한꺼번에 바꾸기
colnames(apt_rent) <- c("ymd","ym","year","code","addr_1","apt_nm","juso_jibun","deposit","rent_fee","conyear","area","floor","py","cnt")

#Code 숫자 조정하기
apt_rent$code<-substr(apt_rent$code,1,6)

apt_rent$conyear<-apt_rent$conyear %>% str_trim() %>% as.numeric()
table(is.na(apt_rent$conyear))

# 작업공간 정리

objects_in_memory <- ls()
# 삭제하고 싶지 않은 객체의 이름
object_to_keep <- c("apt_rent","임대_2023","savename")

# objects_in_memory에서 object_to_keep를 제외한 나머지 객체 삭제
objects_to_remove <- setdiff(objects_in_memory, object_to_keep)

# 나머지 객체 삭제
rm(list = objects_to_remove)




save(apt_rent, file = savename)

임대_2023<-rbind(임대_2023, apt_rent)
