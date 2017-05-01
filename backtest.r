library(RODBC)
library(xts)

# these are all incredibly SQL-injectible
# good thing we don't care about security

obelix.read.params <- function(config_file) {
    source(config_file, local=TRUE)
    list(db=db, driver=driver, uid=uid, pwd=pwd)
}

obelix.connect <- function(config_file) {
    connect_params <- obelix.read.params(config_file)
    template <- 'driver={%s};server=obelix.mit.edu;uid=%s;pwd=%s;database=%s'
    connect_string <- sprintf(template, connect_params['driver'], connect_params['uid'], connect_params['pwd'], connect_params['db'])
    odbcDriverConnect(connect_string)
}

# params: db connection, id number
# return: string ticker symbol
obelix.get_ticker <- function(dbconn, id) {
    querystr <- sprintf('SELECT TOP 1 ticker FROM dim_equity WHERE id=%d', id)
    sqlQuery(dbconn, querystr)[[1]]
}

# params: db connection, id number, date
# return: beta
obelix.get_beta <- function(dbconn, id, date) {
    # it's unclear how exactly beta is measured by obelix - documentation doesn't really exist
    # seems to be tracked against SPX over a quarter?
}

obelix.stocks_in_decile <- function(dbconn, decile, date) {
    year <- as.numeric(format(date, "%Y"))
    sqlQuery(dbconn, sprintf('SELECT id FROM decilemember WHERE y = %d AND decile = %d;', year, decile))
}

obelix.logret_over_period <- function(dbconn, ids, start, end) {
    starttime <- format(date, '%Y%m%d')
    endtime <- format(date, '%Y%m%d')
    querystr <- sprintf('SELECT id, sum(r) FROM fact_equity WHERE id in (%s) AND d BETWEEN \'%s\' AND \'%s\' GROUP BY id;', paste(ids, collapse=','), start, end)
    sqlQuery(dbconn, querystr)
}

obelix.logret_daily <- function(dbconn, id, start, end) {
    starttime <- format(date, '%Y%m%d')
    endtime <- format(date, '%Y%m%d')
    querystr <- sprintf('SELECT d, r FROM fact_equity WHERE id = %s AND d BETWEEN \'%s\' AND \'%s\';', id, start, end)
    data <- sqlQuery(dbconn, querystr)
    as.xts(data$'r', order.by=as.Date(data$d))
}

# compute log returns at constant weights
# I think this function is actually entrirely useless :(
portfolio.logret_daily <- function(dbconn, id_weights, start, end) {
    returns <- lapply(rownames(id_weights),
                      function(row) {
                          id_weights[[row, 'weights']] * (exp(obelix.logret_daily(dbconn, id_weights[[row, 'ids']], start, end)) - 1)
                      })
    returns$all <- T
    fullxts <- do.call(merge.xts, returns)
    fullxts[is.na(fullxts)] <- 0
    as.xts(log(1 + rowSums(fullxts, na.rm=T)), order.by=time(fullxts))
}

# compute log returns, where weights start as specified and track value
# weights must add to 1
portfolio.growth_of_dollar <- function(dbconn, id_weights, start, end) {
    # get price at each day * initial weight
    prices_normalized <- lapply(rownames(id_weights),
                      function(row) {
                          id_weights[[row, 'weights']] * exp(cumsum(obelix.logret_daily(dbconn, id_weights[[row, 'ids']], start, end)))
                      })
    prices_normalized$all <- T
    fullprices <- do.call(merge.xts, prices_normalized)
    # initial price is p, ending price is p(1 + simple)
    # total portfolio value is sum over all of weight * p(1+simple)
    # price on any given day is now just the sum of rows
    growth_of_dollar <- as.xts(rowSums(fullprices, na.rm=T), order.by=time(fullprices))
    return(growth_of_dollar)
}

portfolio.logret_value_weight <- function(dbconn, id_weights, start, end) {
    growth <- portfolio.growth_of_dollar(dbconn, id_weights, start, end)
    logret <- log(diff(growth, arithmetic=F))
    logret[is.na(logret)] <- 0
    return(logret)
}

portfolio.bond_returns <- function() {
}

# compute holdings ratio such that the net beta is 1
portfolio.holdings_ratio <- function(beta1, beta2) {
    (beta2 - 1) / (beta1 + beta2)
}
