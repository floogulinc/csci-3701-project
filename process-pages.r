library(tidyverse)  
library(rvest)
library(rlist)
library(furrr)

future::plan(multiprocess)

chunk2 <- function(x,n) split(x, cut(seq_along(x), n, labels = FALSE)) 

index <- 1:253772
index.nomissing <- list.filter(index, class(html_node(read_html(paste("site-dump-1/id.php_id=", ., sep="")), "#paddingWrapper")) != "xml_missing")

# dont wan't to rerun this on accident
#saveRDS(index.nomissing, "indexnomissing.Rda") 


parse.id = function(addr) {
  return(sub(".*id.php\\?id=", '', addr) %>% strtoi)
}

process.indices <- function(ind) {
  paste("site-dump-1/id.php_id=", ind, sep = "") %>% map(read_html) %>% map(html_node, "#paddingWrapper") %>% {
    tibble(
      id = ind,
      name = map(., html_node, "h2") %>% map_chr(html_text, trim = TRUE),
      schools = map(., ~ {
        tibble (
          thesis = html_nodes(.x, "#thesisTitle") %>% html_text(trim = TRUE),
          university = html_nodes(.x, xpath = "//span[contains(@style,'#006633')]") %>% html_text(),
          advisors = html_nodes(.x, xpath = '//p[contains(@style, "text-align: center; line-height: 2.75ex")]|//p[text()[contains(.,"Advisor")]]') %>% map(html_nodes, "a") %>% map(html_attr, "href") %>% map(map, parse.id),
          countries = html_nodes(.x, xpath = "//div[contains(@style, 'line-height: 30px; text-align: center; margin-bottom: 1ex')]") %>% map(html_nodes, "img") %>% map(map, html_attr, "title")
        )
      }),
      advisors = map(., html_nodes, xpath = '//p[contains(@style, "text-align: center; line-height: 2.75ex")]|//p[text()[contains(.,"Advisor")]]') %>% map(html_nodes, "a") %>% map(html_attr, "href") %>% map(map, parse.id),
      students.raw = map(., html_node, "table") %>% map_if(~ class(.) != "xml_missing" ,html_table),
      students.id = map(., html_node, "table") %>% map(html_nodes, "a") %>% map(html_attr, "href") %>% map(map, parse.id)
    )
  }
}


process.trycatch <- function(ind) {
  tryCatch({
    process.indices(ind)
  }, warning = function(w) {
    print(ind)
    print(w)
  }, error = function(e) {
    print(ind)
    print(e)
  }, finally = {
  })
}


index.nomissing <- readRDS("indexnomissing.Rda")

site.data <- future_map_dfr(chunk2(index.nomissing, 50), process.indices, .progress = TRUE)

saveRDS(site.data, "sitedata.Rda")
