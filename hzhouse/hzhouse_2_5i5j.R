

##############################
options(stringsAsFactors=F,digits=10)
rm(list=ls())
setwd("/home/yibo/桌面/互联网数据/4_杭州房事")
source("./script/hzhouse.R")
time1 <- Sys.time()

cat("\n====================\n")
cat("小区列表 启动\n")
res_xqlist <- xqlist_fetch(source="5i5j")
xqlist <- do.call(rbind,lapply(res_xqlist,function(z){
  do.call(rbind,lapply(z,xqlist_extract,source="5i5j"))}))
xqlist <- unique(xqlist)
xqlist <- xqlist[xqlist$name!="未知小区",]
cat("小区列表 成功\n")

cat("\n====================\n")
cat("小区详情 启动\n")
res_xqinfo <- xqinfo_fetch(source="5i5j",xqlist)
xqinfo <- do.call(rbind,lapply(res_xqinfo,xqinfo_extract,source="5i5j"))
xqinfo <- unique(xqinfo)
xqinfo <- xqinfo[xqinfo$name!="未知小区",]
cat("小区详情 成功\n")

cat("\n====================\n")
cat("成交信息 启动\n")
table(xqinfo$deals_pgn,useNA="ifany")
xqinfo$deals_pgn[xqinfo$deals_pgn<=1] <- 1
table(xqinfo$deals_pgn,useNA="ifany")
res_dealinfo <- dealinfo_fetch(source="5i5j",xqinfo)
dealinfo <- do.call(rbind,lapply(res_dealinfo,function(z){
  do.call(rbind,lapply(z,dealinfo_extract,source="5i5j"))}))
dealinfo <- unique(dealinfo)
cat("成交信息 成功\n")

save(res_xqlist,xqlist,
     res_xqinfo,xqinfo,
     res_dealinfo,dealinfo,
     file=paste0("./data/hzhouse_2_5i5j_",Sys.Date(),".RData"))
time2 <- Sys.time()

cat("\n====================\n")
cat("time use : ",round(difftime(time2,time1,units="mins"),1)," mins\n",sep="")
cat("xqlist has : ",nrow(xqlist)," records\n",sep="")
cat("xqinfo has : ",nrow(xqinfo)," records\n",sep="")
cat("dealinfo has : ",nrow(dealinfo)," records\n",sep="")
cat("deal_date range : ",paste(range(dealinfo$deal_date),collapse=" ~ "),sep="")

