

##############################
options(stringsAsFactors=F,digits=10)
rm(list=ls())
setwd("/home/yibo/桌面/互联网数据/4_杭州房事")
source("./script/hzhouse.R")
time1 <- Sys.time()

cat("\n====================\n")
cat("小区列表 启动\n")
res_xqlist <- xqlist_fetch(source="lianjia")
xqlist <- do.call(rbind,lapply(res_xqlist,function(z){
  do.call(rbind,lapply(z,xqlist_extract,source="lianjia"))}))
xqlist <- unique(xqlist)
xqlist <- xqlist[xqlist$name!="未知小区",]
cat("小区列表 成功\n")

cat("\n====================\n")
cat("小区详情 启动\n")
res_xqinfo <- xqinfo_fetch(source="lianjia",xqlist)
xqinfo <- do.call(rbind,lapply(res_xqinfo$res,xqinfo_extract,
                               source="lianjia"))
xqinfo$trend_detail <- sapply(res_xqinfo$res_trend,xqtrends_extract,
                              source="lianjia")
xqinfo <- unique(xqinfo)
xqinfo <- xqinfo[xqinfo$name!="未知小区",]
cat("小区详情 成功\n")

cat("\n====================\n")
cat("成交信息 启动\n")
token <- login_token(source="lianjia",user="",pass="")
# 登录成功则更新token，不成功则使用已有token
if(token!=""){
  save(token,file="./data/hzhouse_3_lianjia_token.RData")
  cat("\ntoken update\n")
} else{
  load("./data/hzhouse_3_lianjia_token.RData")
  cat("\nlogin failed\n")
}
# 测试token
r_test <- fetch_url("http://hz.lianjia.com/chengjiao",
                cookie=c("lianjia_token"=token))
deal_content <- read_html(httr::content(r_test,"text",encoding="UTF-8"))
deal_cnt <- html_nodes(deal_content,
                       xpath='.//div[@class="content"]//
                       div[@class="leftContent"]//
                       div[@class="resultDes clear"]//
                       div[@class="total fl"]//span')
cat("r_test:",as.integer(gsub("","",html_text(deal_cnt))),"\n",sep="")
rm(r_test,deal_content,deal_cnt)
res_dealinfo <- dealinfo_fetch(source="lianjia",xqinfo,token)
dealcnt <- do.call(rbind,lapply(res_dealinfo,function(z){
  do.call(rbind,lapply(z,dealcnt_extract,source="lianjia"))}))
dealcnt <- unique(dealcnt)
dealinfo <- do.call(rbind,lapply(res_dealinfo,function(z){
  do.call(rbind,lapply(z,dealinfo_extract,source="lianjia"))}))
dealinfo <- unique(dealinfo)
cat("成交信息 成功\n")

save(res_xqlist,xqlist,
     res_xqinfo,xqinfo,
     res_dealinfo,dealcnt,dealinfo,
     file=paste0("./data/hzhouse_3_lianjia_",Sys.Date(),".RData"))
time2 <- Sys.time()

cat("\n====================\n")
cat("time use : ",round(difftime(time2,time1,units="mins"),1)," mins\n",sep="")
cat("xqlist has : ",nrow(xqlist)," records\n",sep="")
cat("xqinfo has : ",nrow(xqinfo)," records\n",sep="")
cat("dealinfo has : ",nrow(dealinfo)," records\n",sep="")
cat("deal_date range : ",paste(range(dealinfo$deal_date),collapse=" ~ "),sep="")

