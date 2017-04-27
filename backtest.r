library(RODBC)

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

obelix.get_ticker <- function(dbconn, id) {
    querystr <- sprintf('SELECT TOP 1 ticker FROM dim_equity WHERE id=%d', id)
    sqlQuery(dbconn, querystr)[[1]]
}
