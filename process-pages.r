library(tidyverse)  
library(rvest)
library(rlist)

parse.id = function(addr) {
  return(sub(".*id.php\\?id=", '', addr) %>% strtoi)
}


index <- 19964:19966
index.nomissing <- list.filter(index, class(html_node(read_html(paste("site-dump-1/id.php_id=", ., sep="")), "#paddingWrapper")) != "xml_missing")

test <- paste("site-dump-1/id.php_id=", index.nomissing, sep = "") %>% map(read_html) %>% map(html_node, "#paddingWrapper") %>% {
  tibble(
    id = index.nomissing,
    name = map(., html_node, "h2") %>% map_chr(html_text, trim = TRUE),
    schools = map(., ~ {
      tibble (
        thesis = html_nodes(.x, "#thesisTitle") %>% html_text(trim = TRUE),
        university = html_nodes(.x, xpath = "//span[contains(@style,'#006633')]") %>% html_text(),
        advisors = html_nodes(.x, xpath = '//p[text()[contains(.,"Advisor")]]') %>% map(html_nodes, "a") %>% map(html_attr, "href") %>% map(map, parse.id),
        country = html_nodes(.x, "img") %>% html_attr("title")
      )
    }),
    #country = map(., html_node, "img") %>% map_chr(html_attr, name = "title"),
    advisors = map(., html_nodes, xpath = '//p[text()[contains(.,"Advisor")]]') %>% map(html_nodes, "a") %>% map(html_attr, "href") %>% map(map, parse.id),
    students.raw = map(., html_node, "table") %>% map_if(~ class(.) != "xml_missing" ,html_table),
    students.id = map(., html_node, "table") %>% map(html_nodes, "a") %>% map(html_attr, "href") %>% map(map, parse.id)
  )
}


