#* check for api key
#* @filter checkAuth
function(req, res) {
	if (is.null(req$HTTP_API_KEY)){
		res$status <- 401
		return(list(status=res$status, error="Authentication required"))
	}
	else if (!is.null(req$HTTP_API_KEY)) {
		key_check <- tbl(pool, "api_keys") %>%
			filter(api_key %in% !!req$HTTP_API_KEY) %>%
			collect
		if (nrow(key_check) == 1) {
			if (key_check$expiry_date >= now(tzone="UTC")) {
				req$pubkey <- key_check$pubkey
				plumber::forward()
			} else {
				res$status <- 401
				return(list(status=res$status, error="API key expired"))
			}
		} else {
			res$status <- 401
			return(list(status=res$status, error="API key not found"))
		}
	}
	else {
		plumber::forward()
	}
}

#* Sats4probes
#* @tag Sats4stats
#* @post /probes
function(req, res) {
	# check last submission time
	last_submission <- tbl(pool, "mc") %>%
		filter(submitter==!!req$pubkey, date_submitted==max(date_submitted)) %>%
		pull(date_submitted) %>%
		unique
	if (length(last_submission) > 0 && last_submission >= now(tzone="UTC") - hours(23) + minutes(45)) {
		res$status <- 422
		return(list(status=res$status, error="Please wait at least 24 hours between submissions"))
	}

	# check data is in post body
	if (is.null(req$postBody) || req$postBody == ""){
		res$status <- 422
		return(list(status=res$status, error="Did not receive any data; check for command typos, or check the path to lncli is correct, or test if there is non-empty output from the command lncli querymc"))
	}

	# validate data in body
	tryCatch({
			dat <- jsonlite::flatten(jsonlite::parse_json(req$postBody, simplifyVector=TRUE)$pairs)
		},
		error = function(e) {
			res$status <- 422
			return(list(status=res$status, error="Failed to parse data"))
		}
	)
	expected_cols <- c("node_from", "node_to", "history.fail_time", "history.fail_amt_sat", "history.fail_amt_msat", "history.success_time", "history.success_amt_sat", "history.success_amt_msat")
	if (identical(names(dat), expected_cols) && nrow(dat) > 0) {
		req$data <- dat
	} else {
		res$status <- 422
		return(list(status=res$status, error="Data does not match expected format"))
	}

	# transform validated data
	tryCatch({
			req$data_transform <- req$data %>%
				mutate_at(vars(history.fail_time:history.success_amt_msat), as.numeric) %>%
				mutate(
					date_submitted=now(tzone="UTC"),
					submitter=req$pubkey,
					time=ifelse(history.success_time>0, history.success_time, history.fail_time),
					amt=ifelse(history.success_time>0,history.success_amt_sat, history.fail_amt_sat),
					htlc=ifelse(history.success_time>0, "success", "fail")) %>%
				dplyr::rename('from'='node_from', 'to'='node_to') %>%
				dplyr::select(submitter, date_submitted, time, from, to, amt, htlc)
		},
		error = function(e) {
			res$status <- 422
			return(list(status=res$status, error="Unexepcted values observed"))
		}
	)

	# fetch a price for the transformed data
	tryCatch({
			req$data_value <- price_function(req$data_transform, pool, pubkey=req$pubkey)
			if (req$data_value == 0) {
				res$status <- 422
				return(list(status=res$status, error="Data value too low for payment"))
			}
		},
		error = function(e) {
			res$status <- 422
			return(list(status=res$status, error="Failed to compute a price"))
		}
	)

	# attempt a keysend for the price
	tryCatch({
			pay_req_headers <- add_headers(c("Grpc-Metadata-macaroon"=Sys.getenv("PAY_MACAROON")))
			pre_image <<- charToRaw(paste(sample(c(0:9, letters, LETTERS), 32, replace = TRUE), collapse = ""))
			req$payment_hash <- base64enc::base64encode(sha256(pre_image))
			payment_content <- jsonlite::toJSON(
				list(
					dest=base64enc::base64encode(hex2raw(req$pubkey)),
					amt=req$data_value,
					payment_hash=req$payment_hash,
					dest_custom_records=list("5482373484"=base64enc::base64encode(pre_image))
				), auto_unbox=TRUE)
			resp <- POST(url=Sys.getenv("PAY_URL"), body=payment_content, config=pay_req_headers)
			if (status_code(resp) == 200) {
				req$payment_date <- as_datetime(resp$date, tz="UTC")
			} else {
				res$status <- 422
				return(list(status=res$status, error="Failed to send payment"))
			}
		},
		error = function(e) {
			res$status <- 422
			return(list(status=res$status, error="Failed to send payment"))
		}
	)

	# commit keysend info to db on successful payment
	tryCatch({
			dbWriteTable(pool, "mc", req$data_transform, append=TRUE, overwrite=FALSE)
			accounting_info <- req$data_transform %>%
				dplyr::select(submitter, date_submitted) %>%
				unique %>%
				mutate(
					hash=paste0(base64enc::base64decode(req$payment_hash), collapse=""),
					amount=req$data_value,
					data_type="Router Mission Control")
			dbWriteTable(pool, "payments", accounting_info, append=TRUE)
		},
		error = function(e) {
			res$status <- 422
			return(list(status=res$status, error="Failed to commit to db"))
		}
	)

	# build the response
	return(
		list(
			receipt=list(
				settlement_time=req$payment_date,
				hash=paste0(base64enc::base64decode(req$payment_hash), collapse=""),
				amount=req$data_value
			)
		)
	)
}

#* Payout info
#* @tag Sats4stats
#* @get /payouts
function(req, res) {
	payouts <- tbl(pool, "payments") %>%
		filter(submitter==!!req$pubkey) %>%
		dplyr::select(date_submitted, data_type, amount, hash) %>%
		collect %>%
		nest_by(date_submitted, .key='details')
	return(list(payouts=payouts))
}

#* Current bid values
#* @preempt checkAuth
#* @get /bidinfo
function(req, res) {
	return(
    list(
      routermc=list(
        premium_accounts=as.numeric(Sys.getenv("ROUTERMC_PREM_BID")),
        free_accounts=as.numeric(Sys.getenv("ROUTERMC_BID"))
      )
    )
  )
}
