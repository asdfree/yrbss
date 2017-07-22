machine_specific_replacements <- 
	list( 
		
		# replace the folder path on macnix
		c( 'path.expand( \"~\" ) , \"YRBSS\"' , paste0( '"' , getwd() , '"' ) ) ,
		
		# change other things in the script to be run
		c( "hello" , "howdy" )
		
	)

source( lodown::syntaxtractor( "yrbss" , replacements = machine_specific_replacements , setup_test = "setup" ) , echo = TRUE )

