intro_title <- "Introduction"
intro <- "Welcome to LNnodeInsight, a data analytics platform in the Bitcoin Lightning Network industry. By accessing and using our services, you agree to be bound by the following terms and conditions. If you do not agree to these terms, please do not use our services."

conduct_title <- 'Conduct on the Website'
conduct <- 'You agree to use our services for lawful purposes only and to not engage in any activity that does not deliberately disrupt the services offered at LNnodeInsight. You are responsible for your conduct while using our services.'

ip_title <- 'Intellectual Property'
ip <- 'The content of our services, including but not limited to text, graphics, logos, and images, is the property of LNnodeInsight or its licensors and is protected by copyright and other intellectual property laws. You may not modify, copy, distribute, or use our content for commercial purposes without written permission from LNnodeInsight.'

disclaimer_title <- 'Disclaimer of Warranties'
disclaimer <- 'Our services are provided "as is" and LNnodeInsight makes no representations or warranties of any kind, express or implied, as to the operation of the services or the information, content, or materials included on the services. LNnodeInsight does not warrant that the services will be uninterrupted or error-free.'

termination_title <- 'Termination'
termination <- 'LNnodeInsight reserves the right to terminate your access to our services at any time, without notice, for any reason, including but not limited to violation of these terms of service.'

conclusion_title <- ""
conclusion <- 'By using our services, you acknowledge that you have read and understood these terms of service and agree to be bound by them. If you have any questions, please contact us.'

tos <- column(12,
	fluidRow(
		h3(intro_title),
		p(intro),
		h3(conduct_title),
		conduct,
		h3(ip_title),
		ip,
		h3(disclaimer_title),
		disclaimer,
		h3(termination_title),
		termination,
		h3(conclusion_title),
		conclusion
	)
)
