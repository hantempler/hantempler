
library(fuzzyjoin)

# 예제 데이터 생성
df1 <- data.frame(ID = c(1, 2, 3),
                  Name = c("John Doe", "Alice Smith", "Bob Johnson"),
                  Age = c(25, 30, 35))

df2 <- data.frame(ID = c(2, 3, 4),
                  Name = c("Alice Smyth", "Bob Jonson", "Charlie Brown"),
                  Age = c(28, 35, 40),
                  Salary = c(50000, 60000, 70000))

# "Name" 열을 기준으로 왼쪽 조인
joined_df <- stringdist_left_join(nm_2, nm_1,
                                  by = c("apt_nm" = "nm"),
                                  method = "jaccard",
                                  max_dist = 0.2)

print(joined_df)
