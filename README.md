# 連載「私は東京で働きたい」のデータ取得と加工


<div align="right">
朝日新聞デジタル企画報道部　小宮山亮磨  <br>
@ryomakom  <br>
2024/11/11  </div>

## この文書は

「私は東京で働きたい データが示す日本のいま」と題した3本の記事を朝日新聞デジタルで[発表しました](https://www.asahi.com/rensai/list.html?id=2329&iref=com_matome)。著者はデジタル企画報道部の同僚・[中山美里記者](https://digital.asahi.com/reporter-bio/a4c3f4ab73adacae29a5e10562014d23c27bdd55e2dfb7b7b785d8c51a1a7c71?iref=article_reporter_profile)と私。地方から東京へと移り住む若い女性が増えていることやその背景事情を、住民基本台帳のデータなどから描こうという企画でした。

元となるデータをどう取得し、どう加工したのか。私が担当した範囲について、以下、説明していきます。


まずはパッケージの呼び出しなど。不動のエースtidyverse様と、estatapiといって、政府統計をAPIで読み込むためのもの（これは素晴らしいのでお薦め。対応してないデータも多々ありますが）。

```{r}

library(tidyverse)
library(estatapi)

# estatapiを利用するためのIDを入力（以下のYOUR_IDというのはダミー。実際には利用者が個別に取得する必要あり）
appId <- "YOUR_ID"

```

## 20～24歳人口に占める女性の割合

[連載1本目](https://digital.asahi.com/articles/ASSC7253ZSC7ULLI005M.html)。20～24歳の人口に占める女性の割合が、長くトップだった鹿児島県で急落し、一方で東京都がぐんぐん上昇して2021年に追い抜きましたよ、というグラフを描く。この連載の中核となるデータ。

住民基本台帳に基づくもので、estatからあらかじめ取得して[GitHubのページ](https://github.com/ryomakom/population_BRS)にまとめてあったものを利用した。1995年から2024年までの人口が市区町村ごと、都道府県ごと、年代ごと、性別ごとに記録されている。

```{r}

pop <- read_csv("data/population.csv")

pop %>%
  filter(is.na(city),
         pref!="合計",
         age %in% c("a20_24")) %>% # 都道府県別にみた20～24歳のデータにしぼる
  group_by(year,pref,sex) %>% 
  summarize(population=sum(population)) %>% 
  pivot_wider(names_from=sex,values_from = population) %>%
  mutate(fmrate=100*female/all,
         color = ifelse(pref %in% c("東京都","鹿児島県"), pref, "Other")) %>%
  ggplot(aes(year,fmrate,group=pref,color=color)) +
  geom_line()+
  scale_color_manual(values = c("東京都" = "blue","鹿児島県" = "red", "Other" = "gray")) +
  labs(title = "20~24歳人口に占める女性比率",
       subtitle = "赤が鹿児島県、青が東京都。その他はグレーで表示",
       x = "年", y = "女性比率(％)") +
  theme_minimal()+
  theme(legend.position = "none") 

```

![](https://github.com/ryomakom/xxx.jpg)



## 住民票の移動による人口増減

[連載2本目](https://digital.asahi.com/articles/ASSC72DDWSC7ULLI006M.html)。住民基本台帳人口移動報告に基づいて、引っ越し（住民票の移動）による各都道府県の人口増減データを見る。

```{r}

# このデータベースから取得
# https://www.e-stat.go.jp/dbview?sid=0003420482

estat_getMetaInfo(appId = appId, statsDataId = "0003420482")

moving <- estat_getStatsData(
  appId = appId,
  statsDataId = "0003420482") %>% 
  rename(category=表章項目,age=年齢,sex=性別,nationality=国籍,area=地域,year=`時間軸（年次）`) %>% 
  select(year,area,category,age,sex,nationality,unit,value,annotation) %>% 
  filter(nationality=="移動者") %>% # 日本人・外国人含む全体の移動者のデータだけ抽出
  select(-nationality) %>% 
  bind_rows(moving10_13) %>% 
  mutate(year=as.double(str_sub(year,1,4)),
         category=ifelse(str_detect(category,"転入者数"),"転入",
                  ifelse(str_detect(category,"転出者数"),"転出",
                  ifelse(str_detect(category,"転入超過数"),"転入超過",NA))),
         age=ifelse(str_detect(age,"０～４歳"),"00～04歳",
             ifelse(str_detect(age,"0～4歳"),"00～04歳",
             ifelse(str_detect(age,"５～９歳"),"05～09歳",
             ifelse(str_detect(age,"5～9歳"),"05～09歳",age))))) %>% 
  mutate(age=ifelse(age=="総数","all",
             ifelse(age=="90歳以上","a90_",
             str_c("a",str_sub(age,1,5))))) %>% 
  mutate(age=str_replace_all(age,"～","_")) %>% 
  mutate(sex=ifelse(sex=="総数","all",
             ifelse(sex=="男","male",
             ifelse(sex=="女","female",sex)))) %>%
  rename(pref=area) %>% 
  filter(pref!="全国",
         !str_detect(pref,"市"),
         !str_detect(pref,"特別区"),
         !str_detect(pref,"圏"))

# 20～24歳の女性が2023年に転入超過だったのは首都圏の1都3県と大阪府、愛知県だけ。これら5都府県では差し引き4万9千人増え、うち3万2千人が東京都で増えた
moving %>%
  filter(year==2023,
         sex=="female",
         age=="a20_24",
         category=="転入超過",
         value>0) %>% 
  select(pref,value) %>% 
  kableExtra::kable()
```
# ℹ 396 more rows
# ℹ Use `print(n = ...)` to see more rows
> test %>% filter(year==2023,sex=="female",age=="a20_24",category=="転入超過",value>0)
# A tibble: 6 × 8
   year pref     category age    sex    unit  value annotation
  <dbl> <chr>    <chr>    <chr>  <chr>  <chr> <dbl> <chr>     
1  2023 埼玉県   転入超過 a20_24 female 人     2712 NA        
2  2023 千葉県   転入超過 a20_24 female 人     1479 NA        
3  2023 東京都   転入超過 a20_24 female 人    32250 NA        
4  2023 神奈川県 転入超過 a20_24 female 人     6920 NA        
5  2023 愛知県   転入超過 a20_24 female 人       57 NA        
6  2023 大阪府   転入超過 a20_24 female 人     5192 NA        
> test %>% filter(year==2023,sex=="female",age=="a20_24",category=="転入超過",value>0) %>% summarize(sum(value))
# A tibble: 1 × 1
  `sum(value)`
         <dbl>
1        48610
> 32250/48610
[1] 0.6634437
> test %>% filter(year>=2020,pref=="鹿児島県",sex!="all",age=="a20_24",category=="転入超過")
# A tibble: 8 × 8
   year pref     category age    sex    unit  value annotation
  <dbl> <chr>    <chr>    <chr>  <chr>  <chr> <dbl> <chr>     
1  2023 鹿児島県 転入超過 a20_24 male   人     -579 NA        
2  2022 鹿児島県 転入超過 a20_24 male   人     -420 NA        
3  2021 鹿児島県 転入超過 a20_24 male   人     -377 NA        
4  2020 鹿児島県 転入超過 a20_24 male   人     -619 NA        
5  2023 鹿児島県 転入超過 a20_24 female 人     -957 NA        
6  2022 鹿児島県 転入超過 a20_24 female 人    -1077 NA        
7  2021 鹿児島県 転入超過 a20_24 female 人    -1083 NA        
8  2020 鹿児島県 転入超過 a20_24 female 人    -1074 NA        
> test %>% filter(year>=2020,pref=="鹿児島県",sex!="all",age=="a20_24",category=="転入超過") %>% group_by(sex) %>% summarize(sum(value))
# A tibble: 2 × 2
  sex    `sum(value)`
  <chr>         <dbl>
1 female        -4191
2 male          -1995
> test %>% filter(year>=2020,pref=="鹿児島県",sex!="all",age=="a20_24",category=="転入超過") %>% group_by(sex) %>% summarize(sum(value)) %>% gt()
> moving <- estat_getStatsData(
+     appId = appId,
+     statsDataId = "0003420482") %>% 
+     rename(category=表章項目,age=年齢,sex=性別,nationality=国籍,area=地域,year=`時間軸（年次）`) %>% 
+     select(year,area,category,age,sex,nationality,unit,value,annotation) %>% 
+     filter(nationality=="移動者") %>% # 日本人・外国人含む全体の移動者のデータだけ抽出
+     select(-nationality) %>% 
+     bind_rows(moving10_13) %>% 
+     mutate(year=as.double(str_sub(year,1,4)),
+            category=ifelse(str_detect(category,"転入者数"),"転入",
+                            ifelse(str_detect(category,"転出者数"),"転出",
+                                   ifelse(str_detect(category,"転入超過数"),"転入超過",NA))),
+            age=ifelse(str_detect(age,"０～４歳"),"00～04歳",
+                       ifelse(str_detect(age,"0～4歳"),"00～04歳",
+                              ifelse(str_detect(age,"５～９歳"),"05～09歳",
+                                     ifelse(str_detect(age,"5～9歳"),"05～09歳",age))))) %>% 
+     mutate(age=ifelse(age=="総数","all",
+                       ifelse(age=="90歳以上","a90_",
+                              str_c("a",str_sub(age,1,5))))) %>% 
+     mutate(age=str_replace_all(age,"～","_")) %>% 
+     mutate(sex=ifelse(sex=="総数","all",
+                       ifelse(sex=="男","male",
+                              ifelse(sex=="女","female",sex)))) %>%
+     rename(pref=area) %>% 
+     filter(pref!="全国",
+            !str_detect(pref,"市"),
+            !str_detect(pref,"特別区"),
+            !str_detect(pref,"圏"))
Fetching record 1-100000... (total: 295920 records)

Fetching record 100001-200000... (total: 295920 records)                                                                                                      

Fetching record 200001-295920... (total: 295920 records)                                                                                                      

                                                                                                                                                              
> moving %>%
+     filter(year==2023,
+            sex=="female",
+            age=="a20_24",
+            category=="転入超過",
+            value>0)
# A tibble: 6 × 8
   year pref     category age    sex    unit  value annotation
  <dbl> <chr>    <chr>    <chr>  <chr>  <chr> <dbl> <chr>     
1  2023 埼玉県   転入超過 a20_24 female 人     2712 NA        
2  2023 千葉県   転入超過 a20_24 female 人     1479 NA        
3  2023 東京都   転入超過 a20_24 female 人    32250 NA        
4  2023 神奈川県 転入超過 a20_24 female 人     6920 NA        
5  2023 愛知県   転入超過 a20_24 female 人       57 NA        
6  2023 大阪府   転入超過 a20_24 female 人     5192 NA        
> # 鹿児島県ではこの年代の女性が2020年以降の転出超過で4200人減少。一方で男性は2000人減にとどまった
> moving %>%
+     filter(year>=2020,
+            pref=="鹿児島県",
+            sex!="all",
+            age=="a20_24",
+            category=="転入超過") %>%
+     group_by(sex) %>%
+     summarize(sum(value))
# A tibble: 2 × 2
  sex    `sum(value)`
  <chr>         <dbl>
1 female        -4191
2 male          -1995
> 
> # https://www.e-stat.go.jp/dbview?sid=0003084609
> wage2010_2019 <- estat_getStatsData(
+     appId = appId,
+     statsDataId = "0003084609",
+     cdCat01 = c("01","02","03"),
+     cdCat02 = c("03"),
+     cdCat04 = c("01"))
Fetching record 1-45120... (total: 45120 records)

>                                                                                                                                                             
> # 2020～2022年のデータ
> # https://www.e-stat.go.jp/dbview?sid=0003426933
> wage2020_2022 <- estat_getStatsData(
+     appId = appId,
+     statsDataId = "0003426933",
+     cdCat01 = c("01","02","03"),
+     cdCat02 = c("03"),
+     cdCat04 = c("01"))
Fetching record 1-13536... (total: 13536 records)

>                                                                                                                                                             
> 
> wage <- bind_rows(wage2010_2019 %>%
+                       rename(sex=性別_基本,
+                              age=年齢階級_基本,
+                              company_size=企業規模_基本,
+                              company_type=産業分類,
+                              pref=地域,
+                              year=`時間軸（2009～2019）`,
+                              type=表章項目) %>%
+                       select(sex,age,year,pref,company_size,company_type,type,unit,value,annotation),
+                   wage2020_2022 %>%
+                       rename(sex=性別_基本,
+                              age=年齢階級_基本,
+                              company_size=企業規模_基本,
+                              company_type=産業分類,
+                              pref=地域,
+                              year=`時間軸（2020～2023）`,
+                              type=表章項目) %>%
+                       select(sex,age,year,pref,company_size,company_type,type,unit,value,annotation))
> wage
# A tibble: 58,656 × 10
   sex    age      year   pref   company_size           company_type type  unit  value annotation
   <chr>  <chr>    <chr>  <chr>  <chr>                  <chr>        <chr> <chr> <dbl> <chr>     
 1 男女計 20～24歳 2019年 北海道 企業規模計（10人以上） Ｔ１ 産業計  年齢  歳     23   NA        
 2 男女計 20～24歳 2018年 北海道 企業規模計（10人以上） Ｔ１ 産業計  年齢  歳     22.9 NA        
 3 男女計 20～24歳 2017年 北海道 企業規模計（10人以上） Ｔ１ 産業計  年齢  歳     22.9 NA        
 4 男女計 20～24歳 2016年 北海道 企業規模計（10人以上） Ｔ１ 産業計  年齢  歳     22.8 NA        
 5 男女計 20～24歳 2015年 北海道 企業規模計（10人以上） Ｔ１ 産業計  年齢  歳     22.9 NA        
 6 男女計 20～24歳 2014年 北海道 企業規模計（10人以上） Ｔ１ 産業計  年齢  歳     22.8 NA        
 7 男女計 20～24歳 2013年 北海道 企業規模計（10人以上） Ｔ１ 産業計  年齢  歳     22.9 NA        
 8 男女計 20～24歳 2012年 北海道 企業規模計（10人以上） Ｔ１ 産業計  年齢  歳     22.9 NA        
 9 男女計 20～24歳 2011年 北海道 企業規模計（10人以上） Ｔ１ 産業計  年齢  歳     23.1 NA        
10 男女計 20～24歳 2010年 北海道 企業規模計（10人以上） Ｔ１ 産業計  年齢  歳     22.9 NA        
# ℹ 58,646 more rows
# ℹ Use `print(n = ...)` to see more rows
> wage %>% distinct(company_size)
# A tibble: 4 × 1
  company_size          
  <chr>                 
1 企業規模計（10人以上）
2 1,000人以上           
3 100～999人            
4 10～99人              
> wage %>% distinct(company_type)
# A tibble: 1 × 1
  company_type
  <chr>       
1 Ｔ１ 産業計 
> wage
# A tibble: 58,656 × 10
   sex    age      year   pref   company_size           company_type type  unit  value annotation
   <chr>  <chr>    <chr>  <chr>  <chr>                  <chr>        <chr> <chr> <dbl> <chr>     
 1 男女計 20～24歳 2019年 北海道 企業規模計（10人以上） Ｔ１ 産業計  年齢  歳     23   NA        
 2 男女計 20～24歳 2018年 北海道 企業規模計（10人以上） Ｔ１ 産業計  年齢  歳     22.9 NA        
 3 男女計 20～24歳 2017年 北海道 企業規模計（10人以上） Ｔ１ 産業計  年齢  歳     22.9 NA        
 4 男女計 20～24歳 2016年 北海道 企業規模計（10人以上） Ｔ１ 産業計  年齢  歳     22.8 NA        
 5 男女計 20～24歳 2015年 北海道 企業規模計（10人以上） Ｔ１ 産業計  年齢  歳     22.9 NA        
 6 男女計 20～24歳 2014年 北海道 企業規模計（10人以上） Ｔ１ 産業計  年齢  歳     22.8 NA        
 7 男女計 20～24歳 2013年 北海道 企業規模計（10人以上） Ｔ１ 産業計  年齢  歳     22.9 NA        
 8 男女計 20～24歳 2012年 北海道 企業規模計（10人以上） Ｔ１ 産業計  年齢  歳     22.9 NA        
 9 男女計 20～24歳 2011年 北海道 企業規模計（10人以上） Ｔ１ 産業計  年齢  歳     23.1 NA        
10 男女計 20～24歳 2010年 北海道 企業規模計（10人以上） Ｔ１ 産業計  年齢  歳     22.9 NA        
# ℹ 58,646 more rows
# ℹ Use `print(n = ...)` to see more rows
> wage %>% distinct(company_type)
# A tibble: 1 × 1
  company_type
  <chr>       
1 Ｔ１ 産業計 
> wage %>% mutate(year=as.double(str_sub(year,1,4))) %>%
+     mutate(area=ifelse(pref %in% c("東京都","埼玉県","神奈川県","千葉県"),"1都3県",
+                        ifelse(pref %in% c("福岡県","大分県","宮崎県","佐賀県","長崎県","熊本県","鹿児島県"),"九州",
+                               "else"))) %>%
+     filter(sex=="女",
+            company_size=="企業規模計（10人以上）",
+            company_type=="Ｔ１ 産業計",
+            type=="きまって支給する現金給与額") %>%
+     ggplot(aes(year,value,color=area,group=pref)) +
+     geom_line() +
+     scale_color_manual(values = c("1都3県" = "blue", "九州" = "red", "else" = "gray")) +
+     labs(title="20~24歳女性の都道府県別平均給与")
> wage %>% mutate(year=as.double(str_sub(year,1,4))) %>%
+     mutate(area=ifelse(pref %in% c("東京都","埼玉県","神奈川県","千葉県"),"2",
+                        ifelse(pref %in% c("福岡県","大分県","宮崎県","佐賀県","長崎県","熊本県","鹿児島県"),"3",
+                               "1"))) %>%
+     filter(sex=="男女計",
+            company_size=="企業規模計（10人以上）",
+            company_type=="Ｔ１ 産業計",
+            type %in% c("きまって支給する現金給与額","年間賞与その他特別給与額")) %>%
+     select(year,pref,type,unit,value,area) %>% 
+     pivot_wider(names_from = type,values_from = value) %>% 
+     mutate(yearly_income=(きまって支給する現金給与額*12+年間賞与その他特別給与額)/10) %>% 
+     select(year,pref,area,yearly_income) 
# A tibble: 611 × 4
    year pref   area  yearly_income
   <dbl> <chr>  <chr>         <dbl>
 1  2019 北海道 1              303.
 2  2018 北海道 1              302.
 3  2017 北海道 1              285.
 4  2016 北海道 1              281.
 5  2015 北海道 1              282.
 6  2014 北海道 1              266.
 7  2013 北海道 1              260.
 8  2012 北海道 1              255.
 9  2011 北海道 1              263.
10  2010 北海道 1              268.
# ℹ 601 more rows
# ℹ Use `print(n = ...)` to see more rows
> wage %>% mutate(year=as.double(str_sub(year,1,4))) %>%
+     mutate(area=ifelse(pref %in% c("東京都","埼玉県","神奈川県","千葉県"),"1都3県",
+                        ifelse(pref %in% c("福岡県","大分県","宮崎県","佐賀県","長崎県","熊本県","鹿児島県"),"九州",
+                               "その他"))) %>%
+     filter(sex=="男女計",
+            company_size=="企業規模計（10人以上）",
+            company_type=="Ｔ１ 産業計",
+            type %in% c("きまって支給する現金給与額","年間賞与その他特別給与額")) %>%
+     select(year,pref,type,unit,value,area) %>% 
+     pivot_wider(names_from = type,values_from = value) %>% 
+     mutate(yearly_income=(きまって支給する現金給与額*12+年間賞与その他特別給与額)/10) %>% 
+     select(year,pref,area,yearly_income) 
# A tibble: 611 × 4
    year pref   area   yearly_income
   <dbl> <chr>  <chr>          <dbl>
 1  2019 北海道 その他          303.
 2  2018 北海道 その他          302.
 3  2017 北海道 その他          285.
 4  2016 北海道 その他          281.
 5  2015 北海道 その他          282.
 6  2014 北海道 その他          266.
 7  2013 北海道 その他          260.
 8  2012 北海道 その他          255.
 9  2011 北海道 その他          263.
10  2010 北海道 その他          268.
# ℹ 601 more rows
# ℹ Use `print(n = ...)` to see more rows
> wage %>% mutate(year=as.double(str_sub(year,1,4))) %>%
+     mutate(area=ifelse(pref %in% c("東京都","埼玉県","神奈川県","千葉県"),"1都3県",
+                        ifelse(pref %in% c("福岡県","大分県","宮崎県","佐賀県","長崎県","熊本県","鹿児島県"),"九州",
+                               "その他"))) %>%
+     filter(sex=="女",
+            company_size=="企業規模計（10人以上）",
+            company_type=="Ｔ１ 産業計",
+            type %in% c("きまって支給する現金給与額","年間賞与その他特別給与額")) %>%
+     select(year,pref,type,unit,value,area) %>% 
+     pivot_wider(names_from = type,values_from = value) %>% 
+     mutate(yearly_income=(きまって支給する現金給与額*12+年間賞与その他特別給与額)/10) %>% # 月収とボーナスを年収に換算
+     select(year,pref,area,yearly_income) %>% 
+     ggplot(aes(year,yearly_income,color=area)) +
+     geom_line() +
+     scale_color_manual(values = c("1都3県" = "blue", "九州" = "red", "else" = "gray")) +
+     labs(title="20~24歳女性の都道府県別平均給与") 
> wage %>% mutate(year=as.double(str_sub(year,1,4))) %>%
+     mutate(area=ifelse(pref %in% c("東京都","埼玉県","神奈川県","千葉県"),"1都3県",
+                        ifelse(pref %in% c("福岡県","大分県","宮崎県","佐賀県","長崎県","熊本県","鹿児島県"),"九州",
+                               "その他"))) %>%
+     filter(sex=="女",
+            company_size=="企業規模計（10人以上）",
+            company_type=="Ｔ１ 産業計",
+            type %in% c("きまって支給する現金給与額","年間賞与その他特別給与額")) %>%
+     select(year,pref,type,unit,value,area) %>% 
+     pivot_wider(names_from = type,values_from = value) %>% 
+     mutate(yearly_income=(きまって支給する現金給与額*12+年間賞与その他特別給与額)/10) %>% # 月収とボーナスを年収に換算
+     select(year,pref,area,yearly_income) %>% 
+     ggplot(aes(year,yearly_income,group=pref,color=area)) +
+     geom_line() +
+     scale_color_manual(values = c("1都3県" = "blue", "九州" = "red", "else" = "gray")) +
+     labs(title="20~24歳女性の都道府県別平均給与")  
> wage %>% mutate(year=as.double(str_sub(year,1,4))) %>%
+     mutate(area=ifelse(pref %in% c("東京都","埼玉県","神奈川県","千葉県"),"1都3県",
+                        ifelse(pref %in% c("福岡県","大分県","宮崎県","佐賀県","長崎県","熊本県","鹿児島県"),"九州",
+                               "その他"))) %>%
+     filter(sex=="女",
+            company_size=="企業規模計（10人以上）",
+            company_type=="Ｔ１ 産業計",
+            type %in% c("きまって支給する現金給与額","年間賞与その他特別給与額")) %>%
+     select(year,pref,type,unit,value,area) %>% 
+     pivot_wider(names_from = type,values_from = value) %>% 
+     mutate(yearly_income=(きまって支給する現金給与額*12+年間賞与その他特別給与額)/10) %>% # 月収とボーナスを年収に換算
+     select(year,pref,area,yearly_income) %>% 
+     ggplot(aes(year,yearly_income,group=pref,color=area)) +
+     geom_line() +
+     scale_color_manual(values = c("1都3県" = "blue", "九州" = "red", "その他" = "gray")) +
+     labs(title="20~24歳女性の都道府県別平均給与")  
> wage %>% mutate(year=as.double(str_sub(year,1,4))) %>%
+     mutate(area=ifelse(pref %in% c("東京都","埼玉県","神奈川県","千葉県"),"1都3県",
+                        ifelse(pref %in% c("福岡県","大分県","宮崎県","佐賀県","長崎県","熊本県","鹿児島県"),"九州",
+                               "その他"))) %>%
+     filter(sex=="女",
+            company_size=="企業規模計（10人以上）",
+            company_type=="Ｔ１ 産業計",
+            type %in% c("きまって支給する現金給与額","年間賞与その他特別給与額")) %>%
+     select(year,pref,type,unit,value,area) %>% 
+     pivot_wider(names_from = type,values_from = value) %>% 
+     mutate(yearly_income=(きまって支給する現金給与額*12+年間賞与その他特別給与額)/10) %>% # 月収とボーナスを年収に換算
+     select(year,pref,area,yearly_income) %>% 
+     ggplot(aes(as.int(year),yearly_income,group=pref,color=area)) +
+     geom_line() +
+     scale_color_manual(values = c("1都3県" = "blue", "九州" = "red", "その他" = "gray")) +
+     labs(title="20~24歳女性の都道府県別平均給与",
+          x="年",
+          y="年収（万円）")  
Error in `geom_line()`:
! Problem while computing aesthetics.
ℹ Error occurred in the 1st layer.
Caused by error in `as.int()`:
! could not find function "as.int"
Run `rlang::last_trace()` to see where the error occurred.
> wage %>% mutate(year=as.double(str_sub(year,1,4))) %>%
+     mutate(area=ifelse(pref %in% c("東京都","埼玉県","神奈川県","千葉県"),"1都3県",
+                        ifelse(pref %in% c("福岡県","大分県","宮崎県","佐賀県","長崎県","熊本県","鹿児島県"),"九州",
+                               "その他"))) %>%
+     filter(sex=="女",
+            company_size=="企業規模計（10人以上）",
+            company_type=="Ｔ１ 産業計",
+            type %in% c("きまって支給する現金給与額","年間賞与その他特別給与額")) %>%
+     select(year,pref,type,unit,value,area) %>% 
+     pivot_wider(names_from = type,values_from = value) %>% 
+     mutate(yearly_income=(きまって支給する現金給与額*12+年間賞与その他特別給与額)/10) %>% # 月収とボーナスを年収に換算
+     select(year,pref,area,yearly_income) %>% 
+     ggplot(aes(year,yearly_income,group=pref,color=area)) +
+     geom_line() +
+     scale_color_manual(values = c("1都3県" = "blue", "九州" = "red", "その他" = "gray")) +
+     labs(title="20~24歳女性の都道府県別平均給与",
+          x="年",
+          y="年収（万円）") +
+     scale_x_continuous(breaks = seq(1, 5, by = 1)) + # 横軸を整数目盛り
+ 
+ 
> wage %>% mutate(year=as.double(str_sub(year,1,4))) %>%
+     mutate(area=ifelse(pref %in% c("東京都","埼玉県","神奈川県","千葉県"),"1都3県",
+                        ifelse(pref %in% c("福岡県","大分県","宮崎県","佐賀県","長崎県","熊本県","鹿児島県"),"九州",
+                               "その他"))) %>%
+     filter(sex=="女",
+            company_size=="企業規模計（10人以上）",
+            company_type=="Ｔ１ 産業計",
+            type %in% c("きまって支給する現金給与額","年間賞与その他特別給与額")) %>%
+     select(year,pref,type,unit,value,area) %>% 
+     pivot_wider(names_from = type,values_from = value) %>% 
+     mutate(yearly_income=(きまって支給する現金給与額*12+年間賞与その他特別給与額)/10) %>% # 月収とボーナスを年収に換算
+     select(year,pref,area,yearly_income) %>% 
+     ggplot(aes(year,yearly_income,group=pref,color=area)) +
+     geom_line() +
+     scale_color_manual(values = c("1都3県" = "blue", "九州" = "red", "その他" = "gray")) +
+     labs(title="20~24歳女性の都道府県別平均給与",
+          x="年",
+          y="年収（万円）") +
+     scale_x_continuous(breaks = seq(1, 5, by = 1)) 
> wage %>% mutate(year=as.double(str_sub(year,1,4))) %>%
+     mutate(area=ifelse(pref %in% c("東京都","埼玉県","神奈川県","千葉県"),"1都3県",
+                        ifelse(pref %in% c("福岡県","大分県","宮崎県","佐賀県","長崎県","熊本県","鹿児島県"),"九州",
+                               "その他"))) %>%
+     filter(sex=="女",
+            company_size=="企業規模計（10人以上）",
+            company_type=="Ｔ１ 産業計",
+            type %in% c("きまって支給する現金給与額","年間賞与その他特別給与額")) %>%
+     select(year,pref,type,unit,value,area) %>% 
+     pivot_wider(names_from = type,values_from = value) %>% 
+     mutate(yearly_income=(きまって支給する現金給与額*12+年間賞与その他特別給与額)/10) %>% # 月収とボーナスを年収に換算
+     select(year,pref,area,yearly_income) %>% 
+     ggplot(aes(year,yearly_income,group=pref,color=area)) +
+     geom_line() +
+     scale_color_manual(values = c("1都3県" = "blue", "九州" = "red", "その他" = "gray")) +
+     labs(title="20~24歳女性の都道府県別平均給与",
+          x="年",
+          y="年収（万円）") 
> wage %>% mutate(year=as.double(str_sub(year,1,4))) %>%
+     mutate(area=ifelse(pref %in% c("東京都","埼玉県","神奈川県","千葉県"),"1都3県",
+                        ifelse(pref %in% c("福岡県","大分県","宮崎県","佐賀県","長崎県","熊本県","鹿児島県"),"九州",
+                               "その他"))) %>%
+     filter(sex=="女",
+            company_size=="企業規模計（10人以上）",
+            company_type=="Ｔ１ 産業計",
+            type %in% c("きまって支給する現金給与額","年間賞与その他特別給与額")) %>%
+     select(year,pref,type,unit,value,area) %>% 
+     pivot_wider(names_from = type,values_from = value) %>% 
+     mutate(yearly_income=(きまって支給する現金給与額*12+年間賞与その他特別給与額)/10) %>% # 月収とボーナスを年収に換算
+     select(year,pref,area,yearly_income) %>% 
+     ggplot(aes(year,yearly_income,group=pref,color=area)) +
+     geom_line() +
+     scale_color_manual(values = c("1都3県" = "blue", "九州" = "red", "その他" = "gray")) +
+     labs(title="20~24歳女性の都道府県別平均給与",
+          x="年",
+          y="年収（万円）") +
+     scale_x_continuous(breaks = 1)) 
Error: unexpected ')' in:
"         y="年収（万円）") +
    scale_x_continuous(breaks = 1))"
> wage %>% mutate(year=as.double(str_sub(year,1,4))) %>%
+     mutate(area=ifelse(pref %in% c("東京都","埼玉県","神奈川県","千葉県"),"1都3県",
+                        ifelse(pref %in% c("福岡県","大分県","宮崎県","佐賀県","長崎県","熊本県","鹿児島県"),"九州",
+                               "その他"))) %>%
+     filter(sex=="女",
+            company_size=="企業規模計（10人以上）",
+            company_type=="Ｔ１ 産業計",
+            type %in% c("きまって支給する現金給与額","年間賞与その他特別給与額")) %>%
+     select(year,pref,type,unit,value,area) %>% 
+     pivot_wider(names_from = type,values_from = value) %>% 
+     mutate(yearly_income=(きまって支給する現金給与額*12+年間賞与その他特別給与額)/10) %>% # 月収とボーナスを年収に換算
+     select(year,pref,area,yearly_income) %>% 
+     ggplot(aes(year,yearly_income,group=pref,color=area)) +
+     geom_line() +
+     scale_color_manual(values = c("1都3県" = "blue", "九州" = "red", "その他" = "gray")) +
+     labs(title="20~24歳女性の都道府県別平均給与",
+          x="年",
+          y="年収（万円）") +
+     scale_x_continuous(breaks = seq(2010, 2022, by = 1)) 
> wage %>% mutate(year=as.double(str_sub(year,1,4))) %>%
+     mutate(area=ifelse(pref %in% c("東京都","埼玉県","神奈川県","千葉県"),"1都3県",
+                        ifelse(pref %in% c("福岡県","大分県","宮崎県","佐賀県","長崎県","熊本県","鹿児島県"),"九州",
+                               "その他"))) %>%
+     filter(sex=="女",
+            company_size=="企業規模計（10人以上）",
+            company_type=="Ｔ１ 産業計",
+            type %in% c("きまって支給する現金給与額","年間賞与その他特別給与額")) %>%
+     select(year,pref,type,unit,value,area) %>% 
+     pivot_wider(names_from = type,values_from = value) %>% 
+     mutate(yearly_income=(きまって支給する現金給与額*12+年間賞与その他特別給与額)/10) %>% # 月収とボーナスを年収に換算
+     select(year,pref,area,yearly_income) %>% 
+     ggplot(aes(year,yearly_income,group=pref,color=area)) +
+     geom_line() +
+     scale_color_manual(values = c("1都3県" = "blue", "九州" = "red", "その他" = "gray")) +
+     labs(title="20~24歳女性の都道府県別平均給与",
+          x="年",
+          y="年収（万円）") +
+     scale_x_continuous(breaks = seq(2010, 2022, by = 5)) 
> wage %>% mutate(year=as.double(str_sub(year,1,4))) %>%
+     mutate(area=ifelse(pref %in% c("東京都","埼玉県","神奈川県","千葉県"),"1都3県",
+                        ifelse(pref %in% c("福岡県","大分県","宮崎県","佐賀県","長崎県","熊本県","鹿児島県"),"九州",
+                               "その他"))) %>%
+     filter(sex=="女",
+            company_size=="企業規模計（10人以上）",
+            company_type=="Ｔ１ 産業計",
+            type %in% c("きまって支給する現金給与額","年間賞与その他特別給与額")) %>%
+     select(year,pref,type,unit,value,area) %>% 
+     pivot_wider(names_from = type,values_from = value) %>% 
+     mutate(yearly_income=(きまって支給する現金給与額*12+年間賞与その他特別給与額)/10) %>% # 月収とボーナスを年収に換算
+     select(year,pref,area,yearly_income) %>% 
+     ggplot(aes(year,yearly_income,group=pref,color=area)) +
+     geom_line() +
+     scale_color_manual(values = c("1都3県" = "blue", "九州" = "red", "その他" = "gray")) +
+     labs(title="20~24歳女性の都道府県別平均給与",
+          x="年",
+          y="年収（万円）") +
+     scale_x_continuous(breaks = seq(2010, 2022, by = 3)) 
> wage %>% mutate(year=as.double(str_sub(year,1,4))) %>%
+     mutate(area=ifelse(pref %in% c("東京都","埼玉県","神奈川県","千葉県"),"1都3県",
+                        ifelse(pref %in% c("福岡県","大分県","宮崎県","佐賀県","長崎県","熊本県","鹿児島県"),"九州",
+                               "その他"))) %>%
+     filter(sex=="女",
+            company_size=="企業規模計（10人以上）",
+            company_type=="Ｔ１ 産業計",
+            type %in% c("きまって支給する現金給与額","年間賞与その他特別給与額")) %>%
+     select(year,pref,type,unit,value,area) %>% 
+     pivot_wider(names_from = type,values_from = value) %>% 
+     mutate(yearly_income=(きまって支給する現金給与額*12+年間賞与その他特別給与額)/10) %>% # 月収とボーナスを年収に換算
+     select(year,pref,area,yearly_income) %>% 
+     ggplot(aes(as.int(year),yearly_income,group=pref,color=area)) +
+     geom_line() +
+     scale_color_manual(values = c("1都3県" = "blue", "九州" = "red", "その他" = "gray")) +
+     labs(title="20~24歳女性の都道府県別平均給与",
+          x="年",
+          y="年収（万円）") +
+     scale_x_continuous(breaks = seq(2010, 2022, by = 3)) 
Error in `geom_line()`:
! Problem while computing aesthetics.
ℹ Error occurred in the 1st layer.
Caused by error in `as.int()`:
! could not find function "as.int"
Run `rlang::last_trace()` to see where the error occurred.
> wage %>% mutate(year=as.double(str_sub(year,1,4))) %>%
+     mutate(area=ifelse(pref %in% c("東京都","埼玉県","神奈川県","千葉県"),"1都3県",
+                        ifelse(pref %in% c("福岡県","大分県","宮崎県","佐賀県","長崎県","熊本県","鹿児島県"),"九州",
+                               "その他"))) %>%
+     filter(sex=="女",
+            company_size=="企業規模計（10人以上）",
+            company_type=="Ｔ１ 産業計",
+            type %in% c("きまって支給する現金給与額","年間賞与その他特別給与額")) %>%
+     select(year,pref,type,unit,value,area) %>% 
+     pivot_wider(names_from = type,values_from = value) %>% 
+     mutate(yearly_income=(きまって支給する現金給与額*12+年間賞与その他特別給与額)/10) %>% # 月収とボーナスを年収に換算
+     select(year,pref,area,yearly_income) %>% 
+     ggplot(aes(year,yearly_income,group=pref,color=area)) +
+     geom_line() +
+     scale_color_manual(values = c("1都3県" = "blue", "九州" = "red", "その他" = "gray")) +
+     labs(title="20~24歳女性の都道府県別平均給与",
+          x="年",
+          y="年収（万円）") +
+     scale_x_continuous(breaks = seq(2010, 2022, by = 3)) 
> wage %>% mutate(year=as.double(str_sub(year,1,4))) %>%
+     mutate(area=ifelse(pref %in% c("東京都","埼玉県","神奈川県","千葉県"),"1都3県",
+                        ifelse(pref %in% c("福岡県","大分県","宮崎県","佐賀県","長崎県","熊本県","鹿児島県"),"九州",
+                               "その他"))) %>%
+     filter(sex=="女",
+            company_size=="企業規模計（10人以上）",
+            company_type=="Ｔ１ 産業計",
+            type %in% c("きまって支給する現金給与額","年間賞与その他特別給与額")) %>%
+     select(year,pref,type,unit,value,area) %>% 
+     pivot_wider(names_from = type,values_from = value) %>% 
+     mutate(yearly_income=(きまって支給する現金給与額*12+年間賞与その他特別給与額)/10) %>% # 月収とボーナスを年収に換算
+     select(year,pref,area,yearly_income) %>% 
+     ggplot(aes(year,yearly_income,group=pref,color=area)) +
+     geom_line() +
+     scale_color_manual(values = c("1都3県" = "blue", "九州" = "red", "その他" = "gray")) +
+     labs(title="20~24歳女性の都道府県別平均年収の推移",
+          x="年",
+          y="年収（万円）") +
+     scale_x_continuous(breaks = seq(2010, 2022, by = 3)) 
> moving %>%
+     filter(year==2023,
+            sex=="female",
+            age=="a20_24",
+            category=="転入超過",
+            value>0) %>% 
+     gt()
> moving %>%
+     filter(year==2023,
+            sex=="female",
+            age=="a20_24",
+            category=="転入超過",
+            value>0) %>% 
+     kable()
Error in kable(.) : could not find function "kable"
> library(kable)
Error in library(kable) :  ‘kable’ という名前のパッケージはありません 
> moving %>%
+     filter(year==2023,
+            sex=="female",
+            age=="a20_24",
+            category=="転入超過",
+            value>0) %>% 
+     kableExtra::kable()
<table>
 <thead>
  <tr>
   <th style="text-align:right;"> year </th>
   <th style="text-align:left;"> pref </th>
   <th style="text-align:left;"> category </th>
   <th style="text-align:left;"> age </th>
   <th style="text-align:left;"> sex </th>
   <th style="text-align:left;"> unit </th>
   <th style="text-align:right;"> value </th>
   <th style="text-align:left;"> annotation </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> 2023 </td>
   <td style="text-align:left;"> 埼玉県 </td>
   <td style="text-align:left;"> 転入超過 </td>
   <td style="text-align:left;"> a20_24 </td>
   <td style="text-align:left;"> female </td>
   <td style="text-align:left;"> 人 </td>
   <td style="text-align:right;"> 2712 </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2023 </td>
   <td style="text-align:left;"> 千葉県 </td>
   <td style="text-align:left;"> 転入超過 </td>
   <td style="text-align:left;"> a20_24 </td>
   <td style="text-align:left;"> female </td>
   <td style="text-align:left;"> 人 </td>
   <td style="text-align:right;"> 1479 </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2023 </td>
   <td style="text-align:left;"> 東京都 </td>
   <td style="text-align:left;"> 転入超過 </td>
   <td style="text-align:left;"> a20_24 </td>
   <td style="text-align:left;"> female </td>
   <td style="text-align:left;"> 人 </td>
   <td style="text-align:right;"> 32250 </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2023 </td>
   <td style="text-align:left;"> 神奈川県 </td>
   <td style="text-align:left;"> 転入超過 </td>
   <td style="text-align:left;"> a20_24 </td>
   <td style="text-align:left;"> female </td>
   <td style="text-align:left;"> 人 </td>
   <td style="text-align:right;"> 6920 </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2023 </td>
   <td style="text-align:left;"> 愛知県 </td>
   <td style="text-align:left;"> 転入超過 </td>
   <td style="text-align:left;"> a20_24 </td>
   <td style="text-align:left;"> female </td>
   <td style="text-align:left;"> 人 </td>
   <td style="text-align:right;"> 57 </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2023 </td>
   <td style="text-align:left;"> 大阪府 </td>
   <td style="text-align:left;"> 転入超過 </td>
   <td style="text-align:left;"> a20_24 </td>
   <td style="text-align:left;"> female </td>
   <td style="text-align:left;"> 人 </td>
   <td style="text-align:right;"> 5192 </td>
   <td style="text-align:left;"> NA </td>
  </tr>
</tbody>
</table>
> moving %>%
+     filter(year==2023,
+            sex=="female",
+            age=="a20_24",
+            category=="転入超過",
+            value>0) %>% 
+     kableExtra::kable()
<table>
 <thead>
  <tr>
   <th style="text-align:right;"> year </th>
   <th style="text-align:left;"> pref </th>
   <th style="text-align:left;"> category </th>
   <th style="text-align:left;"> age </th>
   <th style="text-align:left;"> sex </th>
   <th style="text-align:left;"> unit </th>
   <th style="text-align:right;"> value </th>
   <th style="text-align:left;"> annotation </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> 2023 </td>
   <td style="text-align:left;"> 埼玉県 </td>
   <td style="text-align:left;"> 転入超過 </td>
   <td style="text-align:left;"> a20_24 </td>
   <td style="text-align:left;"> female </td>
   <td style="text-align:left;"> 人 </td>
   <td style="text-align:right;"> 2712 </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2023 </td>
   <td style="text-align:left;"> 千葉県 </td>
   <td style="text-align:left;"> 転入超過 </td>
   <td style="text-align:left;"> a20_24 </td>
   <td style="text-align:left;"> female </td>
   <td style="text-align:left;"> 人 </td>
   <td style="text-align:right;"> 1479 </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2023 </td>
   <td style="text-align:left;"> 東京都 </td>
   <td style="text-align:left;"> 転入超過 </td>
   <td style="text-align:left;"> a20_24 </td>
   <td style="text-align:left;"> female </td>
   <td style="text-align:left;"> 人 </td>
   <td style="text-align:right;"> 32250 </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2023 </td>
   <td style="text-align:left;"> 神奈川県 </td>
   <td style="text-align:left;"> 転入超過 </td>
   <td style="text-align:left;"> a20_24 </td>
   <td style="text-align:left;"> female </td>
   <td style="text-align:left;"> 人 </td>
   <td style="text-align:right;"> 6920 </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2023 </td>
   <td style="text-align:left;"> 愛知県 </td>
   <td style="text-align:left;"> 転入超過 </td>
   <td style="text-align:left;"> a20_24 </td>
   <td style="text-align:left;"> female </td>
   <td style="text-align:left;"> 人 </td>
   <td style="text-align:right;"> 57 </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2023 </td>
   <td style="text-align:left;"> 大阪府 </td>
   <td style="text-align:left;"> 転入超過 </td>
   <td style="text-align:left;"> a20_24 </td>
   <td style="text-align:left;"> female </td>
   <td style="text-align:left;"> 人 </td>
   <td style="text-align:right;"> 5192 </td>
   <td style="text-align:left;"> NA </td>
  </tr>
</tbody>
</table>
> moving %>%
+     filter(year>=2020,
+            pref=="鹿児島県",
+            sex!="all",
+            age=="a20_24",
+            category=="転入超過") %>%
+     group_by(sex) %>%
+     summarize(sum(value)) %>% 
+     kableExtra::kable()
<table>
 <thead>
  <tr>
   <th style="text-align:left;"> sex </th>
   <th style="text-align:right;"> sum(value) </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> female </td>
   <td style="text-align:right;"> -4191 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> male </td>
   <td style="text-align:right;"> -1995 </td>
  </tr>
</tbody>
</table>
> pop %>%
+     filter(is.na(city), pref!="合計", age=="a20_24") %>% # 都道府県別にみた20～24歳のデータにしぼる
+     pivot_wider(names_from=sex,values_from = population) %>%
+     mutate(fmrate20_24=100*female/all,
+            color = ifelse(pref %in% c("東京都","鹿児島県"), pref, "Other")) %>%
+     ggplot(aes(year,fmrate20_24,group=pref,color=color)) +
+     geom_line()+
+     scale_color_manual(values = c("東京都" = "blue","鹿児島県" = "red", "Other" = "gray")) +
+     labs(title = "20~24歳人口に占める女性比率",
+          subtitle = "赤が鹿児島県、青が東京都。その他はグレーで表示",
+          x = "年", y = "女性比率(％)") +
+     theme_minimal()+
+     theme(legend.position = "none") 
> pop %>%
+     filter(is.na(city), pref!="合計", age %in% c("a20_24","a25_29")) %>%               # 都道府県別にみた20～24歳のデータにしぼる
+     pivot_wider(names_from=sex,values_from = population) %>%
+     mutate(fmrate20_24=100*female/all,
+            color = ifelse(pref %in% c("東京都","鹿児島県"), pref, "Other")) %>%
+     ggplot(aes(year,fmrate20_24,group=pref,color=color)) +
+     geom_line()+
+     scale_color_manual(values = c("東京都" = "blue","鹿児島県" = "red", "Other" = "gray")) +
+     labs(title = "20~24歳人口に占める女性比率",
+          subtitle = "赤が鹿児島県、青が東京都。その他はグレーで表示",
+          x = "年", y = "女性比率(％)") +
+     theme_minimal()+
+     theme(legend.position = "none") 
> pop %>%
+     filter(is.na(city), pref!="合計", age %in% c("a20_24","a25_29"))
# A tibble: 8,460 × 7
    year code   pref   city  sex    age    population
   <dbl> <chr>  <chr>  <chr> <chr>  <chr>       <dbl>
 1  1995 010006 北海道 NA    all    a20_24     415693
 2  1995 010006 北海道 NA    all    a25_29     362692
 3  1995 010006 北海道 NA    male   a20_24     207697
 4  1995 010006 北海道 NA    male   a25_29     175221
 5  1995 010006 北海道 NA    female a20_24     207996
 6  1995 010006 北海道 NA    female a25_29     187471
 7  1995 020001 青森県 NA    all    a20_24      90070
 8  1995 020001 青森県 NA    all    a25_29      85581
 9  1995 020001 青森県 NA    male   a20_24      45426
10  1995 020001 青森県 NA    male   a25_29      42228
# ℹ 8,450 more rows
# ℹ Use `print(n = ...)` to see more rows
> pop %>%
+     filter(is.na(city), pref!="合計", age %in% c("a20_24","a25_29")) %>% group_by(year,pref,sex) %>% summarize(female=sum(female),all=sum(all)) %>%     pivot_wider(names_from=sex,values_from = population) %>%
+     mutate(fmrate20_24=100*female/all,
+            color = ifelse(pref %in% c("東京都","鹿児島県"), pref, "Other")) %>%
+     ggplot(aes(year,fmrate20_24,group=pref,color=color)) +
+     geom_line()+
+     scale_color_manual(values = c("東京都" = "blue","鹿児島県" = "red", "Other" = "gray")) +
+     labs(title = "20~24歳人口に占める女性比率",
+          subtitle = "赤が鹿児島県、青が東京都。その他はグレーで表示",
+          x = "年", y = "女性比率(％)") +
+     theme_minimal()+
+     theme(legend.position = "none") 
Error in `summarize()`:
ℹ In argument: `female = sum(female)`.
ℹ In group 1: `year = 1995`, `pref = "三重県"`, `sex = "all"`.
Caused by error:
! object 'female' not found
Run `rlang::last_trace()` to see where the error occurred.
> pop %>%
+     filter(is.na(city), pref!="合計", age %in% c("a20_24","a25_29")) %>% group_by(year,pref,sex) %>% summarize(female=sum(female),all=sum(all)) 
Error in `summarize()`:
ℹ In argument: `female = sum(female)`.
ℹ In group 1: `year = 1995`, `pref = "三重県"`, `sex = "all"`.
Caused by error:
! object 'female' not found
Run `rlang::last_trace()` to see where the error occurred.
> pop %>%
+     filter(is.na(city), pref!="合計", age %in% c("a20_24","a25_29")) %>% group_by(year,pref,sex)
# A tibble: 8,460 × 7
# Groups:   year, pref, sex [4,230]
    year code   pref   city  sex    age    population
   <dbl> <chr>  <chr>  <chr> <chr>  <chr>       <dbl>
 1  1995 010006 北海道 NA    all    a20_24     415693
 2  1995 010006 北海道 NA    all    a25_29     362692
 3  1995 010006 北海道 NA    male   a20_24     207697
 4  1995 010006 北海道 NA    male   a25_29     175221
 5  1995 010006 北海道 NA    female a20_24     207996
 6  1995 010006 北海道 NA    female a25_29     187471
 7  1995 020001 青森県 NA    all    a20_24      90070
 8  1995 020001 青森県 NA    all    a25_29      85581
 9  1995 020001 青森県 NA    male   a20_24      45426
10  1995 020001 青森県 NA    male   a25_29      42228
# ℹ 8,450 more rows
# ℹ Use `print(n = ...)` to see more rows
> pop %>%
+     filter(is.na(city), pref!="合計", age %in% c("a20_24","a25_29")) %>% group_by(year,pref,sex) %>% summarize(population=sum(population)) %>%     pivot_wider(names_from=sex,values_from = population) %>%
+     mutate(fmrate20_24=100*female/all,
+            color = ifelse(pref %in% c("東京都","鹿児島県"), pref, "Other")) %>%
+     ggplot(aes(year,fmrate20_24,group=pref,color=color)) +
+     geom_line()+
+     scale_color_manual(values = c("東京都" = "blue","鹿児島県" = "red", "Other" = "gray")) +
+     labs(title = "20~24歳人口に占める女性比率",
+          subtitle = "赤が鹿児島県、青が東京都。その他はグレーで表示",
+          x = "年", y = "女性比率(％)") +
+     theme_minimal()+
+     theme(legend.position = "none") 
`summarise()` has grouped output by 'year', 'pref'. You can override using the `.groups` argument.
> pop %>%
+     filter(is.na(city), pref!="合計", age %in% c("a20_24","a25_29","a30_34","a35_39")) %>% group_by(year,pref,sex) %>% summarize(population=sum(population)) %>%     pivot_wider(names_from=sex,values_from = population) %>%
+     mutate(fmrate20_24=100*female/all,
+            color = ifelse(pref %in% c("東京都","鹿児島県"), pref, "Other")) %>%
+     ggplot(aes(year,fmrate20_24,group=pref,color=color)) +
+     geom_line()+
+     scale_color_manual(values = c("東京都" = "blue","鹿児島県" = "red", "Other" = "gray")) +
+     labs(title = "20~24歳人口に占める女性比率",
+          subtitle = "赤が鹿児島県、青が東京都。その他はグレーで表示",
+          x = "年", y = "女性比率(％)") +
+     theme_minimal()+
+     theme(legend.position = "none") 
`summarise()` has grouped output by 'year', 'pref'. You can override using the `.groups` argument.
> pop %>%
+     filter(is.na(city),
+            pref!="合計",
+            age %in% c("a20_24")) %>% # 都道府県別にみた20～24歳のデータにしぼる
+     group_by(year,pref,sex) %>% 
+     summarize(population=sum(population)) %>% 
+     pivot_wider(names_from=sex,values_from = population) %>%
+     mutate(fmrate=100*female/all,
+            color = ifelse(pref %in% c("東京都","鹿児島県"), pref, "Other")) %>%
+     ggplot(aes(year,fmrate,group=pref,color=color)) +
+     geom_line()+
+     scale_color_manual(values = c("東京都" = "blue","鹿児島県" = "red", "Other" = "gray")) +
+     labs(title = "20~24歳人口に占める女性比率",
+          subtitle = "赤が鹿児島県、青が東京都。その他はグレーで表示",
+          x = "年", y = "女性比率(％)") +
+     theme_minimal()+
+     theme(legend.position = "none") 
`summarise()` has grouped output by 'year', 'pref'. You can override using the `.groups` argument.
> 
> moving %>%
+     filter(year==2023,
+            sex=="female",
+            age=="a20_24",
+            category=="転入超過",
+            value>0) %>% 
+     kableExtra::kable()
<table>
 <thead>
  <tr>
   <th style="text-align:right;"> year </th>
   <th style="text-align:left;"> pref </th>
   <th style="text-align:left;"> category </th>
   <th style="text-align:left;"> age </th>
   <th style="text-align:left;"> sex </th>
   <th style="text-align:left;"> unit </th>
   <th style="text-align:right;"> value </th>
   <th style="text-align:left;"> annotation </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> 2023 </td>
   <td style="text-align:left;"> 埼玉県 </td>
   <td style="text-align:left;"> 転入超過 </td>
   <td style="text-align:left;"> a20_24 </td>
   <td style="text-align:left;"> female </td>
   <td style="text-align:left;"> 人 </td>
   <td style="text-align:right;"> 2712 </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2023 </td>
   <td style="text-align:left;"> 千葉県 </td>
   <td style="text-align:left;"> 転入超過 </td>
   <td style="text-align:left;"> a20_24 </td>
   <td style="text-align:left;"> female </td>
   <td style="text-align:left;"> 人 </td>
   <td style="text-align:right;"> 1479 </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2023 </td>
   <td style="text-align:left;"> 東京都 </td>
   <td style="text-align:left;"> 転入超過 </td>
   <td style="text-align:left;"> a20_24 </td>
   <td style="text-align:left;"> female </td>
   <td style="text-align:left;"> 人 </td>
   <td style="text-align:right;"> 32250 </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2023 </td>
   <td style="text-align:left;"> 神奈川県 </td>
   <td style="text-align:left;"> 転入超過 </td>
   <td style="text-align:left;"> a20_24 </td>
   <td style="text-align:left;"> female </td>
   <td style="text-align:left;"> 人 </td>
   <td style="text-align:right;"> 6920 </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2023 </td>
   <td style="text-align:left;"> 愛知県 </td>
   <td style="text-align:left;"> 転入超過 </td>
   <td style="text-align:left;"> a20_24 </td>
   <td style="text-align:left;"> female </td>
   <td style="text-align:left;"> 人 </td>
   <td style="text-align:right;"> 57 </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2023 </td>
   <td style="text-align:left;"> 大阪府 </td>
   <td style="text-align:left;"> 転入超過 </td>
   <td style="text-align:left;"> a20_24 </td>
   <td style="text-align:left;"> female </td>
   <td style="text-align:left;"> 人 </td>
   <td style="text-align:right;"> 5192 </td>
   <td style="text-align:left;"> NA </td>
  </tr>
</tbody>
</table>
> moving %>%
+     filter(year==2023,
+            sex=="female",
+            age=="a20_24",
+            category=="転入超過",
+            value>0) %>% gt
> moving %>%
+     filter(year==2023,
+            sex=="female",
+            age=="a20_24",
+            category=="転入超過",
+            value>0) %>% 
+     select(pref,value) %>% 
+     kableExtra::kable()
<table>
 <thead>
  <tr>
   <th style="text-align:left;"> pref </th>
   <th style="text-align:right;"> value </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> 埼玉県 </td>
   <td style="text-align:right;"> 2712 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 千葉県 </td>
   <td style="text-align:right;"> 1479 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 東京都 </td>
   <td style="text-align:right;"> 32250 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 神奈川県 </td>
   <td style="text-align:right;"> 6920 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 愛知県 </td>
   <td style="text-align:right;"> 57 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 大阪府 </td>
   <td style="text-align:right;"> 5192 </td>
  </tr>
</tbody>
</table>

```{r}

# 鹿児島県ではこの年代の女性が2020年以降の転出超過で4200人減少。一方で男性は2000人減にとどまった
moving %>%
  filter(year>=2020,
         pref=="鹿児島県",
         sex!="all",
         age=="a20_24",
         category=="転入超過") %>%
  group_by(sex) %>%
  summarize(sum(value)) %>% 
  kableExtra::kable()

```

<table>
 <thead>
  <tr>
   <th style="text-align:left;"> sex </th>
   <th style="text-align:right;"> sum(value) </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> female </td>
   <td style="text-align:right;"> -4191 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> male </td>
   <td style="text-align:right;"> -1995 </td>
  </tr>
</tbody>
</table>


## 20～24歳女性の年収
[連載3本目](https://digital.asahi.com/articles/ASSC7268NSC7ULLI011M.html)。賃金構造基本統計調査から、この年代の女性の年収の推移を都道府県別に見る。

```{r}
# 2010～2019年のデータ。年代は20～24歳にしぼり、企業規模と業種についてはすべてを含んだ値を取得する
# https://www.e-stat.go.jp/dbview?sid=0003084609
wage2010_2019 <- estat_getStatsData(
    appId = appId,
    statsDataId = "0003084609",
    cdCat01 = c("01","02","03"),
    cdCat02 = c("03"),
    cdCat04 = c("01"))

# 2020～2022年のデータ
# https://www.e-stat.go.jp/dbview?sid=0003426933
wage2020_2022 <- estat_getStatsData(
    appId = appId,
    statsDataId = "0003426933",
    cdCat01 = c("01","02","03"),
    cdCat02 = c("03"),
    cdCat04 = c("01"))

# 二つのデータを合体
wage <- bind_rows(wage2010_2019 %>%
                    rename(sex=性別_基本,
                           age=年齢階級_基本,
                           company_size=企業規模_基本,
                           company_type=産業分類,
                           pref=地域,
                           year=`時間軸（2009～2019）`,
                           type=表章項目) %>%
                    select(sex,age,year,pref,company_size,company_type,type,unit,value,annotation),
                  wage2020_2022 %>%
                    rename(sex=性別_基本,
                           age=年齢階級_基本,
                           company_size=企業規模_基本,
                           company_type=産業分類,
                           pref=地域,
                           year=`時間軸（2020～2023）`,
                           type=表章項目) %>%
                    select(sex,age,year,pref,company_size,company_type,type,unit,value,annotation))

# グラフを描く
wage %>% mutate(year=as.double(str_sub(year,1,4))) %>%
  mutate(area=ifelse(pref %in% c("東京都","埼玉県","神奈川県","千葉県"),"1都3県",
                     ifelse(pref %in% c("福岡県","大分県","宮崎県","佐賀県","長崎県","熊本県","鹿児島県"),"九州",
                            "その他"))) %>%
  filter(sex=="女",
         company_size=="企業規模計（10人以上）",
         company_type=="Ｔ１ 産業計",
         type %in% c("きまって支給する現金給与額","年間賞与その他特別給与額")) %>%
  select(year,pref,type,unit,value,area) %>% 
  pivot_wider(names_from = type,values_from = value) %>% 
  mutate(yearly_income=(きまって支給する現金給与額*12+年間賞与その他特別給与額)/10) %>% # 月収とボーナスを年収に換算
  select(year,pref,area,yearly_income) %>% 
  ggplot(aes(year,yearly_income,group=pref,color=area)) +
  geom_line() +
  scale_color_manual(values = c("1都3県" = "blue", "九州" = "red", "その他" = "gray")) +
  labs(title="20~24歳女性の都道府県別平均年収の推移",
       x="年",
       y="年収（万円）") +
  scale_x_continuous(breaks = seq(2010, 2022, by = 3)) 

```

![](https://github.com/ryomakom/xxx.jpg)
以上。
