library(RGoogleAnalytics)
load("tokens.RData")
token.ids <- c(1, 1, 1, 2)
lapply(tokens, ValidateToken)

queryList <- function(i, a, b, id){
	Init(start.date=a, end.date=b,
		dimensions="ga:date", metrics="ga:pageviews, ga:avgTimeOnPage, ga:pageviewsPerSession, ga:entranceRate",
		max.results=1000, sort="ga:date", table.id=id[[i]])
}

ql <- lapply(1:length(ids), queryList, a="2014-05-07", b="2015-05-06", id=ids)
query <- lapply(ql, QueryBuilder)

d <- lapply(4,
	function(i, ...){
		tk <- tokens[token.ids[i]]
		cbind(Acct=names(tk), Site=names(ids)[i], GetReportData(query[[i]], tk[[1]])) },
	query, ids, tokens, token.ids)

library(data.table)
d <- rbindlist(d)
setkey(d, Site)
d[, totalTime := pageviews*avgTimeOnPage]
d[, list(sum(pageviews), sum(totalTime)), by=Acct]
d[, list(sum(pageviews), sum(totalTime)), by=Site]
d["Shiny", list(sum(pageviews), sum(totalTime)), by=pageTitle]
