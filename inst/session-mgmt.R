# code adapted from https://github.com/PaulC91/shinyauthr

cookie_expiry <- 7 # Days until session expires

get_sessions_from_db <- function(conn=con, expiry=cookie_expiry) {
	dbReadTable(conn, "sessions") %>%
		as_tibble() %>%
		filter(login_time > now() - days(expiry))
}

# This function must accept two parameters: user and sessionid. It will be
# called whenever the user
# successfully logs in with a password.

add_session_to_db <- function(pubkey, sessionid, conn=con) {
	tibble(pubkey=pubkey, sessionid=sessionid, login_time=now()) %>%
		dbWriteTable(conn, "sessions", ., append=TRUE, row.names=FALSE)
}
