# maladolescence
# epidemiology
# sex, drugs, rock and roll
library(SAScii)

sas_url <-
	"https://www.cdc.gov/yrbs/files/2023/2023XXH_SAS_Input_Program.sas"

sas_text <- tolower( readLines( sas_url ) )

# find the (out of numerical order)
# `site` location variable's position
# within the SAS input program
site_location <- which( sas_text == '@1 site $3.' )

# find the start field's position
# within the SAS input program
input_location <- which( sas_text == "input" )

# create a vector from 1 to the length of the text file
sas_length <- seq( length( sas_text ) )

# remove the site_location
sas_length <- sas_length[ -site_location ]

# re-insert the site variable's location
# immediately after the starting position
sas_reorder <- 
	c( 
		sas_length[ seq( input_location ) ] , 
		site_location , 
		sas_length[ seq( input_location + 1 , length( sas_length ) ) ] 
	)

# re-order the sas text file
sas_text <- sas_text[ sas_reorder ]

sas_tf <- tempfile()

writeLines( sas_text , sas_tf )
dat_tf <- tempfile()

dat_url <-
	"https://www.cdc.gov/yrbs/files/2023/XXH2023_YRBS_Data.dat"
	
download.file( dat_url , dat_tf , mode = 'wb' )

yrbss_df <- read.SAScii( dat_tf , sas_tf )

names( yrbss_df ) <- tolower( names( yrbss_df ) )

yrbss_df[ , 'one' ] <- 1
# yrbss_fn <- file.path( path.expand( "~" ) , "YRBSS" , "this_file.rds" )
# saveRDS( yrbss_df , file = yrbss_fn , compress = FALSE )
# yrbss_df <- readRDS( yrbss_fn )
library(survey)

yrbss_design <- 
	svydesign( 
		~ psu , 
		strata = ~ stratum , 
		data = yrbss_df , 
		weights = ~ weight , 
		nest = TRUE 
	)
yrbss_design <- 
	update( 
		yrbss_design , 
		did_not_always_wear_seat_belt = as.numeric( qn8 == 1 ) ,
		ever_used_marijuana = as.numeric( qn46 == 1 ) ,
		tried_to_quit_tobacco_past_year = as.numeric( qn40 == 1 ) ,
		used_tobacco_past_year = as.numeric( q40 > 1 )
	)
sum( weights( yrbss_design , "sampling" ) != 0 )

svyby( ~ one , ~ ever_used_marijuana , yrbss_design , unwtd.count )
svytotal( ~ one , yrbss_design )

svyby( ~ one , ~ ever_used_marijuana , yrbss_design , svytotal )
svymean( ~ bmipct , yrbss_design , na.rm = TRUE )

svyby( ~ bmipct , ~ ever_used_marijuana , yrbss_design , svymean , na.rm = TRUE )
svymean( ~ q2 , yrbss_design , na.rm = TRUE )

svyby( ~ q2 , ~ ever_used_marijuana , yrbss_design , svymean , na.rm = TRUE )
svytotal( ~ bmipct , yrbss_design , na.rm = TRUE )

svyby( ~ bmipct , ~ ever_used_marijuana , yrbss_design , svytotal , na.rm = TRUE )
svytotal( ~ q2 , yrbss_design , na.rm = TRUE )

svyby( ~ q2 , ~ ever_used_marijuana , yrbss_design , svytotal , na.rm = TRUE )
svyquantile( ~ bmipct , yrbss_design , 0.5 , na.rm = TRUE )

svyby( 
	~ bmipct , 
	~ ever_used_marijuana , 
	yrbss_design , 
	svyquantile , 
	0.5 ,
	ci = TRUE , na.rm = TRUE
)
svyratio( 
	numerator = ~ tried_to_quit_tobacco_past_year , 
	denominator = ~ used_tobacco_past_year , 
	yrbss_design ,
	na.rm = TRUE
)
sub_yrbss_design <- subset( yrbss_design , qn40 > 1 )
svymean( ~ bmipct , sub_yrbss_design , na.rm = TRUE )
this_result <- svymean( ~ bmipct , yrbss_design , na.rm = TRUE )

coef( this_result )
SE( this_result )
confint( this_result )
cv( this_result )

grouped_result <-
	svyby( 
		~ bmipct , 
		~ ever_used_marijuana , 
		yrbss_design , 
		svymean ,
		na.rm = TRUE 
	)
	
coef( grouped_result )
SE( grouped_result )
confint( grouped_result )
cv( grouped_result )
degf( yrbss_design )
svyvar( ~ bmipct , yrbss_design , na.rm = TRUE )
# SRS without replacement
svymean( ~ bmipct , yrbss_design , na.rm = TRUE , deff = TRUE )

# SRS with replacement
svymean( ~ bmipct , yrbss_design , na.rm = TRUE , deff = "replace" )
svyciprop( ~ did_not_always_wear_seat_belt , yrbss_design ,
	method = "likelihood" , na.rm = TRUE )
svyttest( bmipct ~ did_not_always_wear_seat_belt , yrbss_design )
svychisq( 
	~ did_not_always_wear_seat_belt + q2 , 
	yrbss_design 
)
glm_result <- 
	svyglm( 
		bmipct ~ did_not_always_wear_seat_belt + q2 , 
		yrbss_design 
	)

summary( glm_result )

unwtd_count_result <-
	unwtd.count( ~ did_not_always_wear_seat_belt , yrbss_design )

stopifnot( coef( unwtd_count_result ) == 15071 )

wtd_n_result <-
	svytotal( 
		~ one , 
		subset(
			yrbss_design , 
			!is.na( did_not_always_wear_seat_belt ) 
		)
	)

stopifnot( round( coef( wtd_n_result ) , 0 ) == 16917 )

share_result <-
	svymean(
		~ did_not_always_wear_seat_belt ,
		yrbss_design ,
		na.rm = TRUE 
	)

stopifnot( round( coef( share_result ) , 4 ) == .3958 )

stopifnot( round( SE( share_result ) , 4 ) == .0172 )

ci_result <-
	svyciprop(
		~ did_not_always_wear_seat_belt ,
		yrbss_design , 
		na.rm = TRUE
	)

stopifnot( round( confint( ci_result )[1] , 4 ) == 0.3621 )

stopifnot( round( confint( ci_result )[2] , 4 ) == 0.4304 )
library(srvyr)
yrbss_srvyr_design <- as_survey( yrbss_design )
yrbss_srvyr_design %>%
	summarize( mean = survey_mean( bmipct , na.rm = TRUE ) )

yrbss_srvyr_design %>%
	group_by( ever_used_marijuana ) %>%
	summarize( mean = survey_mean( bmipct , na.rm = TRUE ) )
