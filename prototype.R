require(topicmodels)
require(lubridate)
require(xml2)
require(magrittr)
require(jiebaR)
require(dplyr)
require(tm)
require(ngramrr) ### using my github version, not the CRAN version
require(wordcloud)

extractchildren <- function(item) {
    item %>% xml_find_one("title") %>% xml_text -> title
    item %>% xml_find_one("link") %>% xml_text -> link
    item %>% xml_find_one("description") %>% xml_text -> body
    item %>% xml_find_one("pubDate") %>% xml_text -> date
    return(c(title, link, body, date))
}

clean_text <- function(item_text) {
    item_text <- stringr::str_replace_all(item_text, "<br />", " ") #remove br
    item_text <- stringr::str_replace_all(item_text, "</?p>", " ") #remove p

    item_text <- stringr::str_replace_all(item_text, "（http://[0-9a-z\\._/]+）", "")
    return(item_text)
}


english_content <- function(text) {
    return(textcat::textcat(text) == "english")

}


extract_xml <- function(xml_url, extractEng = TRUE) {
    read_xml(xml_url) -> tc
    tc %>% xml_find_all(xpath = "/rss/channel/item") -> allitems
    rebuttals <- plyr::ldply(allitems, extractchildren)
    colnames(rebuttals) <- c("title", "url", "content", "date")
    if (extractEng) {
        rebuttals %>% mutate(content = clean_text(content)) %>% mutate(english = english_content(content)) %>% filter(!english) -> clean_rebuttals
    } else {
        rebuttals %>% mutate(content = clean_text(content)) %>% mutate(english = FALSE) %>% filter(!english) -> clean_rebuttals
    }
    cutter <- worker()
    segmentx <- function(content) {
        paste(segment(content, cutter), collapse = " ")
    }
    segmentedtext <- as.character(sapply(clean_rebuttals$content, segmentx))
    dtm2(segmentedtext, char = FALSE, ngmin = 1, ngmax = 1, wordLengths = c(1, Inf)) -> rebuttals_dtm
    termf <- apply(rebuttals_dtm, 2, sum)
    term <- colnames(rebuttals_dtm)
    return(list(dtm = rebuttals_dtm, clean_rebuttals = clean_rebuttals))
}


parsed2014 <- extract_xml("http://www.police.gov.hk/pprb/letters2editors2014_tc.xml")


parsed2013 <- extract_xml("http://www.police.gov.hk/pprb/letters2editors2013_tc.xml")


parsednew <- extract_xml("http://www.police.gov.hk/pprb/letters2editors_tc.xml", FALSE)

combined_dtm <- c(parsednew[[1]], parsed2014[[1]], parsed2013[[1]])

combined_dtm_tfidf <- weightTfIdf(combined_dtm)

apply(combined_dtm_tfidf, 2, sum) %>% sort(decreasing = TRUE) %>% head(150) -> top150tfidf

top150tfidf <- top150tfidf[nchar(names(top150tfidf)) > 1]

top150tfidf <- top150tfidf[names(top150tfidf) != "br" & names(top150tfidf) != "amp" ]

top150tfidf <- top150tfidf[!stringr::str_detect(names(top150tfidf), "^[一二三四五六七八九十]+[名月日項]$")]

png("wordcloud_rebuttal.png", width = 1024, height = 768)
wordcloud(names(top150tfidf), top150tfidf, min.freq = 0, random.order = FALSE, rot.per = 0, max.words = 90, colors= brewer.pal(8, "PuBu")[1:8])
dev.off()


dtm_lda <- combined_dtm
dtm_lda <- dtm_lda[,nchar(colnames(dtm_lda)) > 1 & !stringr::str_detect(colnames(dtm_lda), "^[0-9a-z\\-\\.]+$") & !stringr::str_detect(colnames(dtm_lda), "^[一二三四五六七八九十百千多萬]+[名月日項歲輛個年號元人時分宗號分米事支點]?$")]

dtm_lda <- removeSparseTerms(dtm_lda, (nrow(dtm_lda) - 2) / nrow(dtm_lda))



cv <- function(n, dtm_lda, holdout = 8) {
    idx <- c(rep(FALSE, holdout), rep(TRUE, nrow(dtm_lda) - holdout))[sample(1:nrow(dtm_lda))]
    top10 <- LDA(dtm_lda[idx,], n)
    p <- perplexity(top10, dtm_lda[!idx,])
    return(p)
}

cv_data <- plyr::ldply(rep(2:8, 10), cv, dtm_lda = dtm_lda, .progress = "text")

png("cross_validation.png", width = 1024, height = 768)
plot(2:7, tapply(cv_data[,1], rep(2:8, 10), mean)[1:6], type = "l", xlab = "topic number", ylab = "Perplexity", lwd = 2)
dev.off()

top3 <- LDA(dtm_lda, 3)
perplexity(top3, dtm_lda)
terms(top3, 40)[,3]

max.col(posterior(top3, dtm_lda)$topic)




### TODO remove stopwords
### TF-IDF

grep("^[一二三四五六七八九十]+[名月]$", names(top150tfidf), value = TRUE)

wordcloud(term, termf, max.words = 200, random.order = FALSE, min.freq = 3, rot.per = 0)



as.Date(c(parsed2014[[2]][,4], parsednew[[2]][,4], parsed2013[[2]][,4]))

as.Date(c(parsed2014[[2]][,4], parsednew[[2]][,4], parsed2013[[2]][,4])) %>% format("%Y") -> year

as.Date(c(parsed2014[[2]][,4], parsednew[[2]][,4], parsed2013[[2]][,4])) %>% format("%m") -> mon

data_frame(year, mon) %>% group_by(year, mon) %>% summarise(n = n()) %>% ungroup %>%mutate(year = as.numeric(year), mon = as.numeric(mon)) -> freq_rebuttals

data_frame(year = c(rep(2013, 12), rep(2014, 12), rep(2015, 12)), mon = rep(seq(1,12), 3)) -> final_freq

final_freq %>% left_join(freq_rebuttals) %>% mutate(n = ifelse(is.na(n), 0, n)) -> final_freq

png("timeseries_rebuttal.png", width = 1024, height = 768)
plot(final_freq$n, type = 'l', xlab = 'mon', ylab = "No of rebuttals", xaxt = "n", lwd = 2)
axis(1, at = 1:36, labels = rep(c("j", "f", "m", "a", "m", "j", "j", "a", "s", "o", "n", "d"), 3))
dev.off()

data_frame(year = year[max.col(posterior(top3, dtm_lda)$topic) == 3], mon =mon[max.col(posterior(top3, dtm_lda)$topic) == 3]) -> top1rebu

top1rebu %>% group_by(year, mon) %>% summarise(n = n()) %>% ungroup %>%mutate(year = as.numeric(year), mon = as.numeric(mon)) -> freq_rebuttals

data_frame(year = c(rep(2013, 12), rep(2014, 12), rep(2015, 12)), mon = rep(seq(1,12), 3)) -> final_freq

final_freq %>% left_join(freq_rebuttals) %>% mutate(n = ifelse(is.na(n), 0, n)) -> final_freq

png("timeseries_topic3.png", width = 1024, height = 768)
plot(final_freq$n, type = 'l', xlab = 'mon', ylab = "No of rebuttals", xaxt = "n", lwd = 2)
axis(1, at = 1:36, labels = rep(c("j", "f", "m", "a", "m", "j", "j", "a", "s", "o", "n", "d"), 3))
dev.off()
