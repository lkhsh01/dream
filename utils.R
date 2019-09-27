library(jsonlite)
library(tidyverse)
library(httr)
  
get_pattern <- function(str)
{
  if (str == "ㄱ") return("^[가-낑]")
  else if (str == "ㄴ") return("^[나-닢]")
  else if (str == "ㄷ") return("^[다-띵]")
  else if (str == "ㄹ") return("^[라-링]")
  else if (str == "ㅁ") return("^[마-밑]")
  else if (str == "ㅂ") return("^[바-삥]")
  else if (str == "ㅅ") return("^[사-씽]")
  else if (str == "ㅇ") return("^[아-잎]")
  else if (str == "ㅈ") return("^[자-찧]")
  else if (str == "ㅊ") return("^[차-칭]")
  else if (str == "ㅋ") return("^[카-킹]")
  else if (str == "ㅌ") return("^[타-팅]")
  else if (str == "ㅍ") return("^[파-핑]")
  else if (str == "ㅎ") return("^[하-힝]")
  else return("")
}

## get name stat

get_name_stat <- function(mum) {
  tb <- tibble()
  for (j in 1:2) {
    for (i in 1:9999) {
      api_url <-
        paste0(
          "https://koreanname.me/api/contains/",
          j,
          "/",
          enc2utf8("재") %>% URLencode,
          "/",
          i
        )
      
      res <- GET(api_url) %>%
        content("text") %>%
        fromJSON()
      
      tb <- bind_rows(tb, res[["contains"]])
      
      if (res[["hasNext"]] %>% isFALSE())
        break
    }
  }
  
  tb_res <- tb %>%
    filter(str_detect(name, "^재")) %>%
    mutate(name = paste0("이", name)) %>%
    group_by(name) %>%
    summarise(count = sum(count)) %>%
    arrange(desc(count)) %>%
    mutate(rank = row_number()) %>%
    mutate(fname = str_remove(name, "이재")) %>% 
    filter(str_detect(fname, get_pattern(mum)))
  
  tb_res <- tb_res %>% 
    select(순위 = rank, 이름 = name, 건수 = count )
  
  return(tb_res)
}

## 카테고리정보추가
get_name_cat <- function(mum) {
  get_name_stat(mum) %>%
    select(이름, 순위, 건수) %>%
    arrange(이름) %>% return()
}

# ## run
# get_name_stat("ALL")
# get_name_cat("")

