# 소비트렌드 코리아 2020 - KDX한국데이터 거래소


## 개요
### 1. 설명
* 전략적 온라인 광고 노출을 위한 OS유형별 소비패턴 분석.

### 2. 데이터셋 구성
* Mcorporation.zip


### 3. 참가
* 인원 : 3명
* 작업툴 : R
* 기간 : 20.10.10 ~ 20.10.24

## 프로그래밍

### 1. 라이브러리 불러오기

```
library(tidyverse) # 데이터 가공 및 시각화
library(readxl) # 엑셀파일 불러오기 패키지
library(ggplot2)
library(lubridate)
library(dplyr)
library(zoo)
library(modelr)
options(na.action = na.warn)
library(pastecs)
library(WRS)
library(utils)
```
* tidyverse, dplyr와 ggplot2는 데이터를 가공하고 시각화하는데 사용하였습니다.
* readxl은 엑셀파일을 불러오기 위하여 사용하였습니다.
* lubridate 패키지는 날짜를 가져오기 위하여 사용하였습니다.
* zoo, modelr, pastecs, WRS, untils 패키지는 기술통계량과 t검정을 하기 위하여 사용하였습니다

### 2. 데이터 불러오기
#### 2-1 다중 엑셀파일 불러오기
```
files <- list.files(path = "data/Mcorporation/상품 카테고리 데이터_KDX 시각화 경진대회 Only/", pattern = "*.xlsx", full.names = TRUE) # 다중 엑셀파일 불러오기

glimpse(files)
```

#### 2-2 # KDX_CONTEST_파일정의서.xlsx : 파일 제외

```
products <- sapply(files[2:65], read_excel, simplify=FALSE) %>% 
  bind_rows(.id = "id") %>% 
  select(-id)

glimpse(products)
```

### 3. 기술적 통계량으로 분석하기
- 분석 배경을 보기 쉽게 하기위해 실시
#### 3-1 고객성별 요약

```
products %>%
  filter(고객성별 != "없음") %>%
  select(고객성별, 구매금액, 구매수) %>%
  group_by(고객성별) %>%
  summarize(구매금액평균 = mean(구매금액), 구매수평균 = mean(구매수)) %>%
  mutate(금액비율 = 구매금액평균 * 100 / sum(구매금액평균), 수량비율 = 구매수평균 * 100 / sum(구매수평균))
```
#### 3-2 OS유형별 요약

```
products %>%
  filter(OS유형 != "없음") %>%
  select(OS유형, 구매금액, 구매수) %>%
  group_by(OS유형) %>%
  summarize(구매금액평균 = mean(구매금액), 구매수평균 = mean(구매수)) %>%
  mutate(금액비율 = 구매금액평균 * 100 / sum(구매금액평균), 수량비율 = 구매수평균 * 100 / sum(구매수평균))
```
#### 3-3 고객나이별 요약

```
products %>%
  filter(고객나이 > 0 & 고객나이 < 80) %>%
  select(고객나이, 구매금액, 구매수) %>%
  mutate(금액비율 = 구매금액 * 100 / sum(구매금액), 수량비율 = 구매수 * 100 / sum(구매수)) %>%
  group_by(고객나이) %>%
  summarize(구매금액평균 = mean(구매금액), 구매수평균 = mean(구매수), 금액비 = sum(금액비율), 수량비 = sum(수량비율))
```

#### 3-4 카테고리별 요약
```
products %>%
  select(카테고리명, 구매금액, 구매수) %>%
  group_by(카테고리명) %>%
  summarize(구매금액평균 = mean(구매금액)) %>%
  mutate(금액비율 = 구매금액평균 * 100 / sum(구매금액평균)) %>%
  arrange(desc(구매금액평균)) %>%
  head(10)
```
#### 3-5 구매금액 총합
```
products %>%  
  select(카테고리명, 구매금액, 구매수) %>%
  group_by(카테고리명) %>%
  summarize(구매금액평균 = mean(구매금액)) %>%
  summarize(평균합 = sum(구매금액평균))
```

#### 3-6 구매수 평균 총합
```
products %>%  
  select(카테고리명, 구매금액, 구매수) %>%
  group_by(카테고리명) %>%
  summarize(구매수평균 = mean(구매수)) %>%
  summarize(평균합 = sum(구매수평균))
```

### 4 각 OS별 및 카테고리별 그래프로 출력 

#### 4-1 OS별 매달 구매금액 변화 그래프
```
products2 <- products %>% 
  mutate(년월 = as.yearmon(as.character(구매날짜), "%Y%m")) %>%
  rename(date = "년월") %>%
  filter(OS유형 != "없음") %>%
  group_by(date, OS유형) %>%
  summarise(mean = mean(구매금액))

products_line <- products2 %>% 
  data_grid(date, OS유형) %>%
  gather_predictions(lm(mean ~ date * OS유형, data = products2))

products2 %>%
  ggplot(aes(date, mean, colour = OS유형)) +
  geom_line(lwd = 2) +
  geom_line(data = products_line, aes(y = pred),lwd = 1) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold")) +
  theme_bw() +
  theme(legend.title = element_text(size = 15, face = "bold")) +
  theme(legend.text = element_text(size = 12))
```

#### 4-2 안드로이드의 카테고리별 구매금액
```
products %>%
  filter(OS유형 == "안드로이드") %>%
  group_by(카테고리명, OS유형) %>%
  summarise(mean = mean(구매금액)) %>%
  arrange(desc(mean)) %>%
  head(10) %>%
  ggplot(aes(x = reorder(카테고리명, -mean), y = mean)) +
  geom_bar(stat="identity", position = "dodge", width=.5, fill = "#619CFF") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6,)) + 
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold")) +
  theme_bw()
```

#### 4-3 IOS의 카테고리별 구매금액
```
products %>%
  filter(OS유형 == "IOS") %>%
  group_by(카테고리명, OS유형) %>%
  summarise(mean = mean(구매금액)) %>%
  arrange(desc(mean)) %>%
  head(10) %>%
  ggplot(aes(x = reorder(카테고리명, -mean), y = mean)) +
  geom_bar(stat="identity", position = "dodge", width=.5, fill = "#fc9272") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6,)) + 
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold")) +
  theme_bw()
```

#### 4-4 나이대별 남녀 소비 비율 (안드로이드)
```
products %>%
  filter(OS유형 == "안드로이드") %>%
  filter(고객나이 > 0 & 고객나이 < 80) %>%
  group_by(고객나이, 고객성별) %>%
  summarise(mean = mean(구매금액)) %>%
  ggplot(aes(x = 고객나이, y = mean, fill = 고객성별)) +
  geom_bar(stat="identity", position = "dodge", width=5) + 
  scale_x_continuous(breaks = seq(0, 80, by = 10)) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold")) +
  theme_bw()

```

#### 4-5 나이대별 남녀 소비 비율(IOS)
```
products %>%
  filter(OS유형 == "IOS") %>%
  filter(고객나이 > 0 & 고객나이 < 80) %>%
  group_by(고객나이, 고객성별) %>%
  summarise(mean = mean(구매금액)) %>%
  ggplot(aes(x = 고객나이, y = mean, fill = 고객성별)) +
  geom_bar(stat="identity", position = "dodge", width=5) + 
  scale_x_continuous(breaks = seq(0, 80, by = 10)) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold")) +
  theme_bw()
```

#### 4-6 안드로이드와 IOS 나이대별 구매금액
```
products %>%
  filter(OS유형 == "안드로이드" | OS유형 == "IOS") %>%
  filter(고객나이 > 0 & 고객나이 < 80) %>%
  group_by(고객나이, OS유형) %>%
  summarise(mean = mean(구매금액)) %>%
  ggplot(aes(x = 고객나이, y = mean, fill = OS유형)) +
  geom_bar(stat="identity", position = "dodge", width=5) + 
  scale_fill_manual(values = c('#F8766D','#619CFF')) +
  scale_x_continuous(breaks = seq(0, 80, by = 10)) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold")) +
  theme_bw()
```

#### 4-7 OS별 매달 구매금액 변화 그래프
```
products %>% 
  mutate(년월 = as.yearmon(as.character(구매날짜), "%Y%m")) %>%
  rename(date = "년월") %>%
  filter(OS유형 != "없음") %>%
  group_by(date, OS유형) %>%
  summarise(mean = mean(구매금액)) %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = mean, colour = OS유형),lwd = 2) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold")) +
  theme(legend.title = element_text(size = 15, face = "bold")) +
  theme(legend.text = element_text(size = 12)) +
  theme_bw()

```

#### 4-8 OS별 매달 구매수 변화 그래프
```
products %>% 
  mutate(년월 = as.yearmon(as.character(구매날짜), "%Y%m")) %>%
  rename(date = "년월") %>%
  filter(OS유형 != "없음") %>%
  group_by(date, OS유형) %>%
  summarise(mean = mean(구매수)) %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = mean, colour = OS유형),lwd = 2) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold")) +
  theme(legend.title = element_text(size = 15, face = "bold")) +
  theme(legend.text = element_text(size = 12)) + 
  theme_bw()
```


### 5 독립 t검정

#### 5-1 데이터 필터링
```
products1 <- products %>%
  mutate(년월 = as.yearmon(as.character(구매날짜), "%Y%m")) %>%
  rename(date = "년월") %>%
  filter(OS유형 == "IOS" | OS유형 == "안드로이드") %>%
  group_by(date, OS유형) %>%
  summarise(mean = mean(구매금액))

glimpse(products1)
```

#### 5-2 평균과 표준오차
```
by(products1$mean, products1$OS유형, stat.desc, basic = FALSE, norm = TRUE)

#t.test()
ind.t.test<-t.test(mean ~ OS유형, data = products1)

ind.t.test
```
* p 값(p  < 2.2e-16)이 0.05보다 작으므로 귀무가설은 기각되었다. 
 
* 평균적으로 안드로이드를 사용한 소비의 구매금액(M = 27,478,580, SE = 334116 )이 IOS를 사용한 
소비의 구매금액 (M = 14,926,760, SE = 186361)보다 더 크다. 
 
* 그 차이는 유의하다(t (26.644) = -32.809, p  < 0.05).

