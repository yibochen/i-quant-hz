

##############################
options(stringsAsFactors=F,digits=10)
rm(list=ls())
setwd("/home/yibo/桌面/互联网数据/4_杭州房事")
source("./script/hzhouse.R")
time1 <- Sys.time()

cat("\n====================\n")
cat("小区列表 启动\n")
res_xqlist <- xqlist_fetch(source="tmsf")
xqlist <- do.call(rbind,lapply(res_xqlist,xqlist_extract,source="tmsf"))
xqlist <- unique(xqlist)
xqlist <- xqlist[xqlist$name!="未知小区",]
cat("小区列表 成功\n")

cat("\n====================\n")
cat("小区详情 启动\n")
res_xqinfo <- xqinfo_fetch(source="tmsf",xqlist)
xqinfo <- do.call(rbind,lapply(res_xqinfo$res,xqinfo_extract,
                               source="tmsf"))
xqinfo$trend_detail <- sapply(res_xqinfo$res_trend,xqtrends_extract,
                              source="tmsf")
xqinfo <- unique(xqinfo)
xqinfo <- xqinfo[xqinfo$name!="未知小区",]
cat("小区详情 成功\n")

save(res_xqlist,xqlist,
     res_xqinfo,xqinfo,
     file=paste0("./data/hzhouse_1_tmsf_",Sys.Date(),".RData"))
time2 <- Sys.time()

cat("\n====================\n")
cat("time use : ",round(difftime(time2,time1,units="mins"),1)," mins\n",sep="")
cat("xqlist has : ",nrow(xqlist)," records\n",sep="")
cat("xqinfo has : ",nrow(xqinfo)," records\n",sep="")

