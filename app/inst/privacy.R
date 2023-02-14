intro_title <- 'Introduction'
intro <- 'This Privacy Policy outlines the information that LNnodeInsight ("we", "us") collects and how we use it to provide data analytics services to users ("you") of the Bitcoin Lightning Network. This policy applies to all users of our website and services. By using our website and services, you are acknowledging and accepting the terms of this Privacy Policy.'

consent_title <- 'Consent'
consent <- 'By using our website, you hereby consent to our Privacy Policy and agree to its terms.'

collection_title <- 'Collection of Information'

user_info_title <- 'Information Provided by the User'
user_info <- 'The information that we collect from you, and reasons for collecting this information, will be made expressly clear to you. We do not track you via third party applications or collect metadata from you such as IP address, browser information or session usage information. We use cookies for session reauthorization, see clause below.'
auto_info_title <- 'Automatic Collection from Publicly Visible Sources'
auto_info <- 'We collect information publicly gossiped by nodes on the Bitcoin Lightning Network, such as node information (pubkey, alias, addresses, etc.) and channel parameters (fee policies, capacity, etc.). We also collect non-gossiped information gathered by our own nodes on the Lightning Network. This information is used to provide accurate and up-to-date data analytics.'
third_info_title <- 'Collection of Information from Third Parties'
third_info <- 'We may collect information from third parties, such as Lightning Network explorers and related services, to improve our services.'

info_sharing_title <- 'Sharing of Information'
info_sharing <- 'We only share data derived from the Lightning Network gossip on a publicly visible endpoint. We do not share other data exclusively with advertising partners or third parties because we do not have advertising partners, third-party cookies or third-party applications.'

cookies_title <- 'Cookies'
cookies_desc <- 'We use cookies to provide persistent sessions. Cookies are small files that are stored on your device and contain information about your session on LNnodeInsight. You may disable cookies in your browser, which would only require you to sign in to LNnodeInsight at each use.'

conclusion_title <- 'Conclusion'
conclusion <- 'We are committed to protecting your privacy and use the information we collect in accordance with this Privacy Policy. If you have any questions or concerns about our Privacy Policy, please contact us.'

privacy <- column(12,
	fluidRow(
		h3(intro_title),
		p(intro),
		h3(collection_title),
		h4(user_info_title),
		user_info,
		h4(auto_info_title),
		auto_info,
		h4(third_info_title),
		third_info,
		h3(info_sharing_title),
		info_sharing,
		h3(cookies_title),
		cookies_desc,
		h3(conclusion_title),
		conclusion
	)
)
