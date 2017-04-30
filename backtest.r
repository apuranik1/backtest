library(RODBC)

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

# compute holdings ratio such that the net beta is 1
holdings_ratio <- function(beta1, beta2) {
    (beta2 - 1) / (beta1 + beta2)
}
