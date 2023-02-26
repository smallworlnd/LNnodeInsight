hex2raw <- function(x) {
	chars <- strsplit(x, "")[[1]]
	as.raw(strtoi(paste0(chars[c(TRUE, FALSE)], chars[c(FALSE, TRUE)]), base=16L))
}
