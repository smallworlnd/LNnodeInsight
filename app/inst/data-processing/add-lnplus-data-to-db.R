library(jsonlite)
library(httr)
library(tidyverse)
library(tidyr)
library(DBI)
library(pool)
library(RPostgreSQL)

tryCatch({
	pool <- dbPool(
			drv=PostgreSQL(),
			host=Sys.getenv("DB_HOST"),
			port=Sys.getenv("DB_PORT"),
			dbname=Sys.getenv("DB_NAME"),
			user=Sys.getenv("DB_USER"),
			password=Sys.getenv("DB_PW"))
	},
	error = function(e) { 
		print(e)
		print("could not open db")
		quit(save="no")
	}
)

tryCatch({
	pending_swaps_lists <- fromJSON(Sys.getenv("LNPLUS_PENDING_SWAPS_URL"), flatten=TRUE)
	pending_swaps_df <- Map(cbind,
			pending_swaps_lists$participants,
			swap_id=pending_swaps_lists$id,
			swap_amt=pending_swaps_lists$capacity_sats,
			participant_max_count=pending_swaps_lists$participant_max_count,
			url=pending_swaps_lists$web_url,
			min_channels=pending_swaps_lists$participant_min_channels_count,
			min_cap=pending_swaps_lists$participant_min_capacity_sats) %>%
		bind_rows %>%
		dplyr::select(
			pubkey,
			alias,
			participant_identifier,
			opening_to_participant_identifier,
			receiving_from_participant_identifier,
			lnplus_rank_number,
			lnplus_rank_name,
			positive_ratings_count,
			negative_ratings_count,
			min_channels,
			min_cap,
			swap_amt,
			swap_id,
			participant_max_count,
			url) %>%
		replace_na(list(min_channels=0, min_cap=0))
	},
	error = function(e) {
		print("could not fetch ln+ data")
		poolClose(pool)
		quit(save="no")
	}
)

tryCatch({
	dbWriteTable(pool, 'lnplus_pending', pending_swaps_df, row.names=FALSE, overwrite=TRUE, append=FALSE)
	},
	error = function(e) { 
		print(e)
		print("Could not upload lnplus pending swap data")
	},
	warning = function(w) {
		print(w)
	}
)

poolClose(pool)
