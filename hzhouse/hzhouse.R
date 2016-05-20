

require(xml2)
require(httr)
require(rvest)
require(jsonlite)

# 抓取页面
fetch_url <- function(url,cookie="",header="",trycnt_threshold=100){
  try_cnt <- 0
  r <- try(httr::GET(url,set_cookies(cookie),add_headers(header)),silent=T)
  while(class(r)!="response"){
    try_cnt <- try_cnt+1
    if(try_cnt>=trycnt_threshold){stop("can't connect to web")}
    cat("try ",try_cnt,"\n",sep="")
    r <- try(httr::GET(url,set_cookies(cookie),add_headers(header)),silent=T)
  }
  while(r$status_code!=200){
    try_cnt <- try_cnt+1
    if(try_cnt>=trycnt_threshold){stop("can't connect to web")}
    cat("try ",try_cnt,"\n",sep="")
    r <- try(httr::GET(url,set_cookies(cookie),add_headers(header)),silent=T)
  }
  return(r)
}
fetch_url_post <- function(url,cookie="",header="",body="",
                           trycnt_threshold=100){
  try_cnt <- 0
  r <- try(httr::POST(url,set_cookies(cookie),add_headers(header),body=body),
           silent=T)
  while(class(r)!="response"){
    try_cnt <- try_cnt+1
    if(try_cnt>=trycnt_threshold){stop("can't connect to web")}
    cat("try ",try_cnt,"\n",sep="")
    r <- try(httr::POST(url,set_cookies(cookie),add_headers(header),body=body),
             silent=T)
  }
  while(r$status_code!=200){
    try_cnt <- try_cnt+1
    if(try_cnt>=trycnt_threshold){stop("can't connect to web")}
    cat("try ",try_cnt,"\n",sep="")
    r <- try(httr::POST(url,set_cookies(cookie),add_headers(header),body=body),
             silent=T)
  }
  return(r)
}
fetch_text <- function(url,trycnt_threshold=100){
  try_cnt <- 0
  r <- try(readLines(url),silent=T)
  while(class(r)!="character"){
    try_cnt <- try_cnt+1
    if(try_cnt>=trycnt_threshold){stop("can't connect to web")}
    cat("try ",try_cnt,"\n",sep="")
    r <- try(readLines(url),silent=T)
  }
  return(r)
}

# 抓取小区列表页面
xqlist_fetch <- function(source=c("tmsf","5i5j","lianjia")){
  if(source=="tmsf"){
    url_community <- "http://www.tmsf.com/esf/esfnSearch_communityList.htm"
    tmp <- readLines(url_community)
    pgn <- as.integer(gsub("^.+doPage\\('(\\d+)'\\).+$","\\1",
                           grep("末页",tmp,value=T)))
    cat("tmsf has ",pgn," pages\n",sep="")
    res <- vector(mode="list",length=pgn)
    for(i in 1:length(res)){
      body <- list(
        "aid"="",
        "pr"="",
        "age"="",
        "ctype"="",
        "imgcount"="",
        "page"=i,
        "keywords"=""
      )
      res[[i]] <- fetch_url_post(url_community,body=body)
      cat("page ",i," of ",pgn,",suceed","\n",sep="")
    }
    return(res)
  }
  if(source=="lianjia"){
    url_base <- "http://hz.lianjia.com/xiaoqu/"
    # 分区域抓取
    area_list <- c("jiande",
                   "xihu",
                   "xiacheng",
                   "jianggan",
                   "gongshu",
                   "shangcheng",
                   "binjiang",
                   "yuhang",
                   "xiaoshan",
                   "linan",
                   "hainingshi")
    url_base2 <- paste0(url_base,area_list)
    res <- vector(mode="list",length=length(url_base2))
    for(i in 1:length(res)){
      # 先读取一次页面，确定每个区域的页数
      r <- fetch_url(url_base2[i])
      area_page <- read_html(httr::content(r,"text",encoding="UTF-8"))
      area_cnt <- html_nodes(area_page,
                             xpath='.//body//div[@class="wrapper"]//
                             div[@class="con-box"]//
                             div[@class="page-box house-lst-page-box"]')
      area_pgn <- as.integer(gsub("^.*totalPage\":(\\d+),\"curPage.*$","\\1",
                                  html_attr(area_cnt,"page-data")))
      cat("area ",area_list[i]," has ",area_pgn," pages\n",sep="")
      res[[i]] <- vector(mode="list",length=area_pgn)
      for(j in 1:length(res[[i]])){
        res[[i]][[j]] <- fetch_url(paste(url_base2[i],"/pg",j,sep=""))
        cat("area ",area_list[i],
            ",page ",j," of ",area_pgn,
            ",suceed","\n",sep="")
      }
    }
    return(res)
  }
  if(source=="5i5j"){
    url_base <- "http://hz.5i5j.com/community/"
    # 分区域抓取
    area_list <- c("gongshu",
                   "xiacheng",
                   "shangcheng",
                   "binjiang",
                   "yuhang",
                   "xiaoshan",
                   "xihu",
                   "jianggan",
                   "fuyang")
    url_base2 <- paste0(url_base,area_list)
    res <- vector(mode="list",length=length(url_base2))
    for(i in 1:length(res)){
      # 先读取一次页面，确定每个区域的页数
      r <- fetch_url(url_base2[i])
      area_page <- read_html(httr::content(r,"text",encoding="UTF-8"))
      area_cnt <- html_nodes(area_page,
                             xpath='.//body//section//div[@class="main"]//
                             div[@class="list-comm-l"]//
                             h3[@class="list-comm-sort"]//font')
      area_cnt <- as.integer(html_text(area_cnt))
      # 每页12个小区
      area_pgn <- ceiling(area_cnt/12)
      cat("area ",area_list[i]," has ",area_pgn," pages\n",sep="")
      res[[i]] <- vector(mode="list",length=area_pgn)
      for(j in 1:length(res[[i]])){
        res[[i]][[j]] <- fetch_url(paste(url_base2[i],"/n",j,sep=""))
        cat("area ",area_list[i],
            ",page ",j," of ",area_pgn,
            ",suceed","\n",sep="")
      }
    }
    return(res)
  }
}

# 解析小区列表信息
xqlist_extract <- function(source=c("tmsf","5i5j","lianjia"),response){
  if(source=="tmsf"){
    content <- read_html(httr::content(response,"text",encoding="UTF-8"))
    xq_list <- html_nodes(content,
                          xpath='.//body//div[@class="unit"]//
                          div[@class="cslist"]//
                          div[@class="cslist_l fl"]//
                          div[@class="cslistfy"]')
    # 小区id/名称
    id1 <- html_attr(xq_list,"id")
    xq1 <- html_nodes(xq_list,xpath='./table/tr/td[@width=300]/a[1]')
    id2 <- gsub("^/esf/xq_index_(\\d+)\\.htm$","\\1",
                sapply(xq1,html_attr,"href"))
    name <- html_text(xq1)
    # 区域
    xq2 <- html_nodes(xq_list,xpath='./table/tr/td[@width=300]/font[1]')
    area <- html_text(xq2)
    # 位置
    xq3 <- html_nodes(xq_list,xpath='./table/tr/td[@width=300]/font[2]')
    location <- html_attr(xq3,"title")
    # 二手房源
    xq4 <- html_nodes(xq_list,xpath='./table/tr/td[@width=300]/a[3]')
    fy_2s <- html_text(xq4)
    # 出租房源
    xq5 <- html_nodes(xq_list,xpath='./table/tr/td[@width=300]/a[4]')
    fy_cz <- html_text(xq5)
    # 价格
    xq6 <- html_nodes(xq_list,xpath='./table/tr/td[@colspan="2"]/a/span[1]')
    price <- html_text(xq6)
    xq7 <- html_nodes(xq_list,xpath='./table/tr/td[@colspan="2"]/a/span[last()]')
    trend_1m <- gsub("\t|\n|\r","",html_text(xq7))
    # 返回结果
    return(data.frame(id=id1,id2,name,area,location,
                      fy_2s,fy_cz,price,trend_1m))
  }
  if(source=="lianjia"){
    content <- read_html(httr::content(response,"text",encoding="UTF-8"))
    # 每页30个小区
    xq_list <- html_nodes(content,
                          xpath='.//body//div[@class="wrapper"]//
                          div[@class="con-box"]//
                          div[@class="list-wrap"]//
                          ul//li//div[@class="info-panel"]')
    # 小区id和名称
    id <- gsub("^/xiaoqu/(\\d+)/$","\\1",
               sapply(html_nodes(xq_list,xpath='./h2/a'),html_attr,"href"))
    name <- sapply(html_nodes(xq_list,xpath='./h2/a'),html_attr,"title")
    # 90天成交
    sold_90d <- html_text(html_nodes(xq_list,
                                     xpath='./div[@class="col-1"]//
                                     div[@class="where"]//a[1]'))
    # 房源出租
    fy_cz <- html_text(html_nodes(xq_list,
                                  xpath='./div[@class="col-1"]//
                                  div[@class="where"]//a[2]'))
    # 房源出售
    fy_2s <- html_text(html_nodes(xq_list,
                                  xpath='./div[@class="col-2"]//
                                  div[@class="square"]//
                                  div//a//span'))
    # 区域
    area1 <- html_text(html_nodes(xq_list,
                                  xpath='./div[@class="col-1"]//
                                  div[@class="other"]//div[@class="con"]/a[1]'))
    area2 <- html_text(html_nodes(xq_list,
                                  xpath='./div[@class="col-1"]//
                                  div[@class="other"]//div[@class="con"]/a[2]'))
    area <- paste(area1,area2)
    # 年代
    year <- html_text(html_nodes(xq_list,
                                 xpath='./div[@class="col-1"]//
                                 div[@class="other"]//div[@class="con"]'))
    year <- gsub("^(.*)/(.*)$","\\2",year)
    # 学校
    school <- html_text(html_nodes(xq_list,
                                   xpath='./div[@class="col-1"]//
                                   div[@class="chanquan"]//
                                   div[@class="left agency"]//
                                   div[@class="view-label left"]'))
    # 价格
    price <- html_text(html_nodes(xq_list,
                                  xpath='./div[@class="col-3"]//
                                  div[@class="price"]//span[@class="num"]'))
    # 返回结果
    return(data.frame(id,name,area,year,school,fy_cz,fy_2s,sold_90d,price))
  }
  if(source=="5i5j"){
    content <- read_html(httr::content(response,"text",encoding="UTF-8"))
    # 每页12个小区
    xq_list <- html_nodes(content,
                          xpath='.//body//section//div[@class="main"]//
                          div[@class="list-comm-l"]//
                          div[@class="list-body"]//ul//li')
    # 经纬度
    lon1 <- as.numeric(sapply(xq_list,html_attr,"x"))
    lat1 <- as.numeric(sapply(xq_list,html_attr,"y"))
    # 小区id和名称
    id1 <- gsub("^/community/(\\d+)$","\\1",
                sapply(html_nodes(xq_list,
                                  xpath='./div[@class="list-info-comm"]/h2/a'),
                       html_attr,"href"))
    name <- sapply(html_nodes(xq_list,
                              xpath='./div[@class="list-info-comm"]/h2/a'),
                   html_attr,"title")
    # 区域
    area <- html_text(html_nodes(xq_list,
                                 xpath='./div[@class="list-info-comm"]/
                                 dl/dd/p[1]'))
    # 房源
    fy <- html_text(html_nodes(xq_list,
                               xpath='./div[@class="list-info-comm"]/
                               dl/dd/p[2]'))
    # 年代
    year <- html_text(html_nodes(xq_list,
                                 xpath='./div[@class="list-info-comm"]/
                                 dl/dd/p[3]'))
    # 成交
    sold_30d <- html_text(html_nodes(xq_list,
                                     xpath='./div[@class="list-info-comm"]/
                                     dl/dd/p[4]'))
    # 价格
    price <- html_text(html_nodes(xq_list,
                                  xpath='./div[@class="list-info-comm"]/
                                  dl/dt/h3'))
    # 趋势
    trend1 <- html_attr(html_nodes(xq_list,
                                   xpath='./div[@class="list-info-comm"]/
                                   dl/dt/p/em/span'),"class")
    trend2 <- html_text(html_nodes(xq_list,
                                   xpath='./div[@class="list-info-comm"]/
                                   dl/dt/p/em'))
    trend <- paste0(trend1,trend2)
    # 地图信息(id和经纬度)
    bdmap <- html_text(html_nodes(content,xpath='.//body/script[last()-1]'))
    bdmap <- strsplit(gsub("^.+= \\[([^]]+)\\];.+$","\\1",bdmap),"\\},\\{")[[1]]
    id2 <- gsub("^.*\"id\":(\\d+),.+$","\\1",bdmap)
    lon2 <- as.numeric(gsub("^.*:\"([0-9.]+)\\|([0-9.]+)\".*$","\\1",bdmap))
    lat2 <- as.numeric(gsub("^.*:\"([0-9.]+)\\|([0-9.]+)\".*$","\\2",bdmap))
    # 返回结果
    return(data.frame(id=id1,id2,name,area,lon1,lat1,lon2,lat2,
                      year,fy,sold_30d,price,trend))
  }
}

# 抓取小区详情页面(链家和透明售房需要额外抓取趋势页面)
xqinfo_fetch <- function(source=c("tmsf","5i5j","lianjia"),xqlist){
  if(source=="tmsf"){
    res <- res_trend <- vector(mode="list",length=nrow(xqlist))
    for(i in 1:length(res)){
      url <- paste0("http://www.tmsf.com/esf/xq_index_",xqlist$id[i],".htm")
      tmp <- "http://www.tmsf.com/esfn/webXqAction_tendencyForMonthly.jspx?kid="
      url2 <- paste0(tmp,xqlist$id[i])
      res[[i]] <- fetch_url(url)
      res_trend[[i]] <- fetch_url(url2)
      cat("community ",i," of ",length(res)," suceed","\n",sep="")
    }
    return(list(res=res,res_trend=res_trend))
  }
  if(source=="lianjia"){
    res <- res_trend <- vector(mode="list",length=nrow(xqlist))
    for(i in 1:length(res)){
      url <- paste0("http://hz.lianjia.com/xiaoqu/",xqlist$id[i])
      url2 <- paste0("http://hz.lianjia.com/fangjia/priceTrend/c",xqlist$id[i])
      res[[i]] <- fetch_url(url)
      res_trend[[i]] <- fetch_text(url2)
      cat("community ",i," of ",length(res)," suceed","\n",sep="")
    }
    return(list(res=res,res_trend=res_trend))
  }
  if(source=="5i5j"){
    res <- vector(mode="list",length=nrow(xqlist))
    for(i in 1:length(res)){
      url <- paste0("http://hz.5i5j.com/community/",xqlist$id[i])
      res[[i]] <- fetch_url(url)
      cat("community ",i," of ",length(res)," suceed","\n",sep="")
    }
    return(res)
  }
}

# 解析小区趋势信息
xqtrends_extract <- function(source=c("tmsf","5i5j","lianjia"),response){
  if(source=="tmsf"){
    content <- httr::content(response,"text",encoding="UTF-8")
    json <- fromJSON(content)
    # 月份:签约均价
    return(paste(unlist(json$ticks),
                 unlist(json$line),
                 sep=":",collapse=";"))
  }
  if(source=="lianjia"){
    json <- fromJSON(response)
    return(paste(json$currentLevel$month,
                 json$currentLevel$dealPrice$total,
                 sep=":",collapse=";"))
  }
}

# 解析小区详情信息
xqinfo_extract <- function(source=c("tmsf","5i5j","lianjia"),response){
  if(source=="tmsf"){
    content <- read_html(httr::content(response,"text",encoding="UTF-8"))
    # 小区id
    id <- html_attr(html_nodes(content,
                               xpath='.//body//div[@class="linel"]//a[3]'),
                    "href")
    id <- gsub("^/esf/xq_index_(\\d+)\\.htm$","\\1",id)
    # 名称
    name <- html_text(html_nodes(content,
                                 xpath='.//body//div[@class="unite"]//
                                 div[@class="hh1"]//table//tr//td//h1'))
    # 区域
    area <- html_text(html_nodes(content,
                                 xpath='.//body//div[@class="unite"]//
                                 div[@class="hh1"]//table//tr//td//span'))
    area <- gsub("\\[|\\]","",area)
    # 基本信息
    xq_info1 <- html_nodes(content,
                           xpath='.//body//div[@class="unite"]//
                           div[@class="detail_l"]//
                           div[@class="detleft"]//
                           table//
                           tr//td//
                           div[@class="housedata"]//
                           ul[@class="basedata"]//
                           li[position()<=7]')
    # 近月均价
    price <- html_text(html_nodes(xq_info1[1],xpath='./span[2]'))
    # 同比上月
    trend_1m <- gsub("\r|\n|\t|","",
                     html_text(html_nodes(xq_info1[1],xpath='./span[1]')))
    if(grepl("近月无签约",html_text(xq_info1[1]))){
      price <- trend_1m <- "近月无签约"
    }
    # 二手房源
    fy_2s <- html_text(html_nodes(xq_info1[2],xpath='./span[1]'))
    # 出租房源
    fy_cz <- html_text(html_nodes(xq_info1[2],xpath='./span[2]'))
    # 位置
    location <- gsub("\r|\t|\n|地址：|　街景地图","",html_text(xq_info1[3]))
    # 竣工时间
    year <- html_text(xq_info1[4])
    # 开发商
    company <- html_text(xq_info1[5])
    # 物业公司
    serve <- html_text(xq_info1[6])
    # 物业费
    serveprice <- html_text(xq_info1[7])
    # 更多信息
    xq_info2 <- html_nodes(content,
                           xpath='.//body//div[@class="unite"]//
                           div[@class="detail_l"]//
                           div[@class="detleft"]//
                           div[@class="detwo"]//
                           div[@class="etabwe"]')
    # 项目介绍
    intro <- gsub("\r|\t|\n","",html_text(xq_info2[2]))
    # 周边配套
    peitao <- gsub("\n+","\n",gsub("\r|\t| ","",html_text(xq_info2[3])))
    # 交通状况
    traffic <- html_text(html_nodes(xq_info2[1],
                                    xpath='./div[@class="ldetabwe"]/ul/li'))
    # 物业类型
    type <- html_text(html_nodes(xq_info2[1],xpath='./ul/li[1]'))
    # 建造年代
    year2 <- html_text(html_nodes(xq_info2[1],xpath='./ul/li[2]'))
    # 所属城区
    area2 <- html_text(html_nodes(xq_info2[1],xpath='./ul/li[3]'))
    # 建筑面积
    size <- html_text(html_nodes(xq_info2[1],xpath='./ul/li[4]'))
    # 绿化比例
    green <- html_text(html_nodes(xq_info2[1],xpath='./ul/li[5]'))
    # 楼层状况
    floor <- html_text(html_nodes(xq_info2[1],xpath='./ul/li[6]'))
    # 停车位
    parking <- html_text(html_nodes(xq_info2[1],xpath='./ul/li[7]'))
    # 经纬度
    lonlat <- html_text(html_nodes(content,
                                   xpath='.//body//div[@class="unite"]//
                                   div[@class="detail_l"]//
                                   div[@class="detleft"]//
                                   div[@class="detwo"]//div//script'))
    lon <- as.numeric(gsub("^.+lng = ([0-9.]+);.*$","\\1",lonlat))
    lat <- as.numeric(gsub("^.+lat = ([0-9.]+);.*$","\\1",lonlat))
    return(data.frame(id,name,
                      area,area2,
                      location,
                      lon,lat,
                      price,trend_1m,
                      fy_2s,fy_cz,
                      year,year2,
                      type,
                      company,serve,serveprice,
                      size,green,floor,parking,
                      intro,peitao,traffic))
  }
  if(source=="lianjia"){
    content <- read_html(httr::content(response,"text",encoding="UTF-8"))
    # 标题信息
    t0 <- html_nodes(content,
                     xpath='.//body//div[@class="wrapper"]//
                     div[@class="nav-container detail-container"]//
                     section[@class="detail-block clear"]//
                     div[@class="res-top clear"]//
                     div[@class="title fl"]')
    id <- gsub("^http://hz.lianjia.com/xiaoqu/(\\d+)/$","\\1",
               html_attr(html_nodes(t0,xpath='.//a[1]'),"href"))
    name <- html_text(html_nodes(t0,xpath='.//a[1]'))
    area <- html_text(html_nodes(t0,xpath='./span[1]'))
    location <- html_attr(html_nodes(t0,xpath='./span[2]'),"title")
    lonlat <- html_attr(html_nodes(t0,xpath='.//a[2]'),"xiaoqu")
    lon <- as.numeric(gsub("^\\[([0-9.]+), ([0-9.]+)\\]$","\\2",lonlat))
    lat <- as.numeric(gsub("^\\[([0-9.]+), ([0-9.]+)\\]$","\\1",lonlat))
    # 价格
    price <- html_text(html_nodes(content,
                                  xpath='.//body//div[@class="wrapper"]//
                                  div[@class="nav-container detail-container"]//
                                  section[@class="detail-block clear"]//
                                  div[@class="top-detail"]//
                                  div[@class="res-info fr"]//
                                  div[@class="col-1"]//
                                  div[@class="price-pre"]//
                                  span[@class="num"]'))
    # 基本信息
    t1 <- html_text(html_nodes(content,
                               xpath='.//body//div[@class="wrapper"]//
                               div[@class="nav-container detail-container"]//
                               section[@class="detail-block clear"]//
                               div[@class="top-detail"]//
                               div[@class="res-info fr"]//
                               div[@class="col-2 clearfix"]//ol//li'))
    # 价格趋势
    price2 <- html_text(html_nodes(content,
                                   xpath='.//body//div[@class="wrapper"]//
                                   div[@id="dealPrice"]//
                                   div[@class="bottom"]//
                                   div[@class="second fl"]//
                                   div[@class="next fl"]//p[1]'))
    trend <- html_text(html_nodes(content,
                                  xpath='.//body//div[@class="wrapper"]//
                                  div[@id="dealPrice"]//
                                  div[@class="bottom"]//
                                  div[@class="second fl"]//
                                  div[@class="next fl"]//p[2]'))
    if(length(price)<=0){price <- NA}
    if(length(price2)<=0){price2 <- NA}
    if(length(trend)<=0){trend <- NA}
    # 返回结果
    return(data.frame(id,
                      name,
                      area,
                      location,
                      lon,
                      lat,
                      price,
                      price2,
                      trend,
                      year=gsub("^建筑年代：(.*)$","\\1",t1[1]),
                      serveprice=gsub("^物业费用：(.*)$","\\1",t1[2]),
                      serve=gsub("^物业公司：(.*)$","\\1",t1[3]),
                      company=gsub("^开发商：(.*)$","\\1",t1[4]),
                      buildcnt=gsub("^楼栋总数：(.*)容积率：(.*)$","\\1",t1[5]),
                      rjl=gsub("^楼栋总数：(.*)容积率：(.*)$","\\2",t1[5]),
                      housecnt=gsub("^房屋总数：(.*)绿化率：(.*)$","\\1",t1[6]),
                      green=gsub("^房屋总数：(.*)绿化率：(.*)$","\\2",t1[6]),
                      lianjiashop=gsub("^附近门店：(.*)$","\\1",t1[7])))
  }
  if(source=="5i5j"){
    content <- read_html(httr::content(response,"text",encoding="UTF-8"))
    # 主要字段
    t0 <- html_text(html_nodes(content,
                               xpath='.//body//
                               section[@class="w-full house-basic2"]//
                               div[@class="house-main"]//
                               div[@class="house-basic-main"]//
                               ul[@class="house-info comm-info"]//
                               li[position()<10]'))
    t0 <- gsub("\r|\n|\t| +","",t0)
    # 价格趋势(挂牌价和成交价)
    t1 <- html_text(html_nodes(content,
                               xpath='.//body//
                               section[@class="w-full"]//
                               div[@class="main"]//
                               section[@class="detail-intro menu1"]//
                               div[@class="comm-price"]//
                               div[@class="comm-data1"]//
                               script[2]'))
    trenddate <- gsub("^.+data : \\[([0-9,]+),\\].+$","\\1",t1)
    trendsellprice <- gsub("^.+挂牌均价.+\\[([0-9,]+),\\].+成交均价.+$",
                           "\\1",t1)
    trenddealprice <- gsub("^.+挂牌均价.+成交均价.+\\[([0-9,]+),\\].+$",
                           "\\1",t1)
    # 更多字段
    t2 <- html_text(html_nodes(content,
                               xpath='.//body//
                               section[@class="w-full"]//
                               div[@class="main"]//
                               section[@class="detail-intro menu2"]//
                               div[@class="comm-baseInfo-l"]//
                               ul//li'))
    # 描述
    t3 <- html_text(html_nodes(content,
                               xpath='.//body//
                               section[@class="w-full"]//
                               div[@class="main"]//
                               section[@class="detail-intro menu2"]//
                               div[@class="comm-baseInfo-all"]'))
    desc <- gsub("\r|\n|\t|\n +","",t3)
    # 地图信息
    t4 <- html_text(html_nodes(content,
                               xpath='.//body/script[last()]'))
    lon1 <- as.numeric(gsub("^.+var pointx = \"([0-9.]+)\";.+$","\\1",t4))
    lon2 <- as.numeric(gsub("^.+var mapX = \"([0-9.]+)\";.+$","\\1",t4))
    lat1 <- as.numeric(gsub("^.+var pointy = \"([0-9.]+)\";.+$","\\1",t4))
    lat2 <- as.numeric(gsub("^.+var mapY = \"([0-9.]+)\";.+$","\\1",t4))
    id <- gsub("^.+setEcommerceView',\"(\\d+)\",.+$","\\1",t4)
    # 成交记录的页数
    deals_pgn <- html_attr(html_nodes(content,xpath='.//
                                      section[@class="detail-intro fl menu6"]//
                                      ul[@class="deal-page"]//
                                      a[last()]'),"onclick")
    deals_pgn <- as.integer(gsub("^getDeals\\(.*\\d+.*,\"(\\d+)\"\\)$",
                                 "\\1",deals_pgn))
    # 没有页码，先写成-1，后面再按照1页来抓取
    if(length(deals_pgn)<=0){deals_pgn <- -1}
    # 返回结果
    return(data.frame(id,
                      price=t0[1],
                      fy=t0[2],
                      area=t0[3],
                      location=t0[4],
                      year=t0[5],
                      school=t0[6],
                      company=t0[7],
                      serveprice=t0[8],
                      sold=t0[9],
                      trenddate,
                      trendsellprice,
                      trenddealprice,
                      deals_pgn,
                      name=t2[1],
                      serveprice2=t2[2],
                      size=t2[3],
                      type=t2[4],
                      sizeland=t2[5],
                      rjl=t2[6],
                      location2=t2[7],
                      year2=t2[8],
                      company2=t2[9],
                      housecnt=t2[10],
                      green=t2[11],
                      parking=t2[12],
                      serve=t2[13],
                      desc,
                      lon1,
                      lon2,
                      lat1,
                      lat2))
  }
}

# 登录获取token
login_token <- function(source=c("tmsf","5i5j","lianjia"),user,pass){
  if(source=="lianjia"){
    token <- ""
    # 预登录，获取LT
    r0 <- fetch_url("https://passport.lianjia.com/cas/prelogin/loginTicket?")
    (lt <- gsub("^\\{\"data\":\"(.+)\"\\}$","\\1",httr::content(r0,"text")))
    body <- c(
      "username"=user,
      "password"=pass,
      "verifycode"="",
      "service"="http://hz.lianjia.com/",
      "isajax"="true",
      "code"="",
      "remember"="1",
      "lt"=lt
    )
    # 登录，获取ST
    login_url <- paste0("https://passport.lianjia.com/cas/login?",
                        paste(names(body),body,sep="=",collapse="&"))
    r1 <- fetch_url(login_url)
    (st <- gsub("^.+ticket\":\"(.+)\"\\}\n$","\\1",httr::content(r1,"text")))
    # 登录，获取token
    login_url <- paste0("http://login.lianjia.com/login/getUserInfo/",
                        "?service=http://hz.lianjia.com/&st=",
                        st,"&callback=casback",
                        round(as.numeric(Sys.time())*1000),"0")
    r2 <- fetch_url(login_url)
    # httr::content(r2,"text")
    set_cookie <- r2$headers[["set-cookie"]][[1]]
    if(length(set_cookie)==1){
      if(grepl("lianjia_token",set_cookie)){
        token <- gsub("^.*lianjia_token=([^;]+);.*$","\\1",set_cookie)
      }
    }
    return(token)
  }
}

# 抓取成交记录页面
dealinfo_fetch <- function(source=c("tmsf","5i5j","lianjia"),xqinfo,token=""){
  if(source=="lianjia"){
    # 链家需要登录，才能看到完整的成交记录，否则会缺少最近一个月的数据
    res <- vector(mode="list",length=nrow(xqinfo))
    for(i in 1:length(res)){
      # 抓取第一页，得到页数
      r <- fetch_url(paste0("http://hz.lianjia.com/chengjiao/c",xqinfo$id[i]),
                     cookie=c("lianjia_token"=token))
      content <- read_html(httr::content(r,"text",encoding="UTF-8"))
      deals_pgs <- html_nodes(content,
                              xpath='.//body//div[@class="content"]//
                              div[@class="leftContent"]//
                              div[@class="contentBottom clear"]//
                              div[@class="page-box fr"]//
                              div[@class="page-box house-lst-page-box"]')
      deals_pgn <- as.integer(gsub("^.*totalPage\":(\\d+),\"curPage.*$","\\1",
                                   html_attr(deals_pgs,"page-data")))
      deal_cnt <- html_nodes(content,
                             xpath='.//div[@class="content"]//
                             div[@class="leftContent"]//
                             div[@class="resultDes clear"]//
                             div[@class="total fl"]//span')
      deal_cnt <- as.integer(gsub("","",html_text(deal_cnt)))
      # 没有页码，标记为0页，表示没有成交，返回空值
      if(length(deals_pgn)<=0 | deal_cnt<=0){
        deals_pgn <- 0
        r <- NA
      }
      # 100页，或者套数超多，可能是该小区的id失效导致没有信息用整体取代
      if(deals_pgn>=100 | deal_cnt>=3000){
        deals_pgn <- 0
        r <- NA
      }
      cat("community ",i," has ",deals_pgn," pages\n",sep="")
      res[[i]] <- vector(mode="list",length=max(deals_pgn,1))
      # 第一页不再重抓
      res[[i]][[1]] <- r
      if(deals_pgn>1){
        for(j in 2:deals_pgn){
          dealurl <- paste0("http://hz.lianjia.com/chengjiao/pg",
                            j,"c",xqinfo$id[i])
          res[[i]][[j]] <- fetch_url(dealurl,
                                     cookie=c("lianjia_token"=token))
          cat("community ",i,
              ",page ",j," of ",deals_pgn,
              ",suceed","\n",sep="")
        }
      }
      cat("community ",i," of ",length(res)," suceed","\n",sep="")
    }
    return(res)
  }
  if(source=="5i5j"){
    res <- vector(mode="list",length=nrow(xqinfo))
    for(i in 1:length(res)){
      id <- xqinfo$id[i]
      name <- xqinfo$name[i]
      deals_pgn <- xqinfo$deals_pgn[i]
      deal_headers <- c(
        # "Host"="hz.5i5j.com",
        # "Connection"="keep-alive",
        # "Accept"="text/html, */*; q=0.01",
        # "User-Agent"="Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/47.0.2526.111 Safari/537.36",
        # "Referer"="http://hz.5i5j.com/community/41181",
        # "Accept-Encoding"="gzip, deflate, sdch",
        # "Accept-Language"="en-US,en;q=0.8,zh;q=0.6",
        "X-Requested-With"="XMLHttpRequest"
      )
      res[[i]] <- vector(mode="list",length=deals_pgn)
      for(j in 1:deals_pgn){
        deal_url <- "http://hz.5i5j.com/exchange/getdeals?communityId="
        deal_url <- paste(deal_url,id,"&page=",j,sep="")
        res[[i]][[j]] <- fetch_url(deal_url,header=deal_headers)
        cat("community ",i,
            ",page ",j," of ",deals_pgn,
            ",suceed","\n",sep="")
      }
      cat("community ",i," of ",length(res)," suceed","\n",sep="")
    }
    return(res)
  }
}

# 解析小区成交数
dealcnt_extract <- function(source=c("tmsf","5i5j","lianjia"),response){
  if(source!="lianjia"){
    stop("only lianjia need this!")
  }
  if(class(response)=="response"){
    # 小区id
    id <- gsub("^http://hz.lianjia.com/chengjiao/.*c(\\d+)$","\\1",response$url)
    deal_content <- read_html(httr::content(response,"text",encoding="UTF-8"))
    deal_cnt <- html_nodes(deal_content,
                           xpath='.//div[@class="content"]//
                           div[@class="leftContent"]//
                           div[@class="resultDes clear"]//
                           div[@class="total fl"]//span')
    deal_cnt <- as.integer(gsub("","",html_text(deal_cnt)))
    return(data.frame(community_id=id,deal_cnt))
  } else{
    return(NULL)
  }
}

# 解析成交明细
dealinfo_extract <- function(source=c("tmsf","5i5j","lianjia"),response){
  if(source=="lianjia"){
    if(class(response)=="response"){
      # 小区id
      id <- gsub("^http://hz.lianjia.com/chengjiao/.*c(\\d+)$","\\1",
                 response$url)
      deal_content <- read_html(httr::content(response,"text",encoding="UTF-8"))
      deal_list <- html_nodes(deal_content,
                              xpath='.//div[@class="content"]//
                              div[@class="leftContent"]//
                              ul[@class="listContent"]//li')
      t1 <- html_nodes(deal_list,
                       xpath='./div[@class="info"]/div[@class="title"]/a')
      house_id <- html_attr(t1,"href")
      house_id <- gsub("^http:.+chengjiao/(.+).html$","\\1",house_id)
      house_name <- html_text(t1)
      t2 <- html_nodes(deal_list,
                       xpath='./div[@class="info"]/
                       div[@class="address"]/
                       div[@class="houseInfo"]')
      house_face <- html_text(t2)
      t3 <- html_nodes(deal_list,
                       xpath='./div[@class="info"]/
                       div[@class="address"]/
                       div[@class="dealDate"]')
      deal_date <- html_text(t3)
      t4 <- html_nodes(deal_list,
                       xpath='./div[@class="info"]/
                       div[@class="address"]/
                       div[@class="totalPrice"]')
      deal_price_total <- html_text(t4)
      t5 <- html_nodes(deal_list,
                       xpath='./div[@class="info"]/
                       div[@class="flood"]/
                       div[@class="source"]')
      deal_source <- html_text(t5)
      t6 <- html_nodes(deal_list,
                       xpath='./div[@class="info"]/
                       div[@class="flood"]/
                       div[@class="positionInfo"]')
      house_position <- html_text(t6)
      t7 <- html_nodes(deal_list,
                       xpath='./div[@class="info"]/
                       div[@class="flood"]/
                       div[@class="unitPrice"]')
      deal_price <- html_text(t7)
      t8 <- html_nodes(deal_list,
                       xpath='./div[@class="info"]/
                       div[@class="dealHouseInfo"]')
      deal_house_info <- html_text(t8)
      # 返回结果
      if(length(house_id)>0){
        return(data.frame(community_id=id,house_id,house_name,
                          house_face,house_position,deal_house_info,
                          deal_date,deal_source,deal_price_total,deal_price))
      } else{
        return(NULL)
      }
    } else{
      return(NULL)
    }
  }
  if(source=="5i5j"){
    if(class(response)=="response"){
      # id
      id <- gsub("^http.+communityId=(\\d+)&page=.*$","\\1",response$url)
      deal_content <- read_html(httr::content(response,"text",encoding="UTF-8"))
      deal_table <- html_nodes(deal_content,
                               xpath='.//li[@class="watch-record-text"]//
                               ul[@class="watch-record-text2"]')
      # 基本信息
      house_info1 <- html_text(html_nodes(deal_table,xpath='.//li[1]//b'))
      house_info2 <- html_text(html_nodes(deal_table,xpath='.//li[1]//span'))
      # 面积
      house_size <- html_text(html_nodes(deal_table,xpath='.//li[2]'))
      # 成交日期
      deal_date <- html_text(html_nodes(deal_table,xpath='.//li[3]'))
      # 总价
      house_price_total <- html_text(html_nodes(deal_table,xpath='.//li[4]'))
      # 单价
      house_price <- html_text(html_nodes(deal_table,xpath='.//li[5]'))
      charToRaw(house_info2[1])
      house_info <- gsub("\xc2\xa0+",";",paste(house_info1,house_info2,sep=";"))
      # 返回结果
      if(length(house_info)>0){
        return(data.frame(id,house_info,house_size,deal_date,
                          house_price_total,house_price))
      } else{
        return(NULL)
      }
    } else{
      return(NULL)
    }
    return(res)
  }
}

