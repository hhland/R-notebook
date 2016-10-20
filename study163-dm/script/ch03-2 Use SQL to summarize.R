library(sqldf)

setwd('D:\\R_edu\\data')
sale <- read.csv("data/sale.csv")
#*********************************************************************************************/
#按照年份汇总*/
year_sale<-sqldf("select year,sum(sale) as sum_sale,
                 sum(profit) as sum_profit from sale group by year")
  #错误的方式*/
year_sale<-sqldf("select year,market,sum(sale) as sum_sale,
                 sum(profit) as sum_profit from sale group by year")
  #例. 获得sale表的总行数。
sqldf("select  count(distinct market)  from  sale")

#定位低于全国销售平均水平地域*/
sqldf("select avg(sale) as all_avg_sale from sale ")
low1_market<-sqldf("select market,avg(sale) as mavg_sale
              from sale 
              group by market
              having mavg_sale<32308
              order by mavg_sale")
  #使用嵌套语句（子查询）完成
low2_market<-sqldf("select market,avg(sale) as mavg_sale
                  from sale 
                  group by market
                  having mavg_sale<(select avg(sale) as all_avg_sale from sale)
                  order by mavg_sale")


low2_market<-sqldf(" select * from (select market,avg(sale) as mavg_sale
              from sale 
              group by market )
              where mavg_sale<32308
              order by mavg_sale")



