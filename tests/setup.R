
#
#
#
library(SAScii)

sas_url <-
	"https://www.cdc.gov/healthyyouth/data/yrbs/files/2019/2019XXH-SAS-Input-Program.sas"

sas_text <- tolower( readLines( sas_url ) )

# find the site location
site.location <- which( sas_text == '@1 site $3.' )

# find the start field location
input.location <- which( sas_text == "input" )

# create a vector from 1 to the length of the text file
sas_length <- seq( length( sas_text ) )

# remove the site location
sas_length <- sas_length[ -site.location ]

# re-insert the site location right after input
sas_reorder <- 
	c( 
		sas_length[ seq( input.location ) ] , 
		site.location , 
		sas_length[ seq( input.location + 1 , length( sas_length ) ) ] 
	)

# re-order the sas text file
sas_text <- sas_text[ sas_reorder ]

sas_tf <- tempfile()

writeLines( sas_text , sas_tf )
dat_tf <- tempfile()

dat_url <-
	"https://www.cdc.gov/healthyyouth/data/yrbs/files/2019/XXH2019_YRBS_Data.dat"
	
download.file( dat_url , dat_tf , mode = 'wb' )

yrbss_df <- read.SAScii( dat_tf , sas_tf )

names( yrbss_df ) <- tolower( names( yrbss_df ) )

yrbss_df[ , 'one' ] <- 1

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
		q2 = q2 ,
		never_rarely_wore_seat_belt = as.numeric( qn8 == 1 ) ,
		ever_used_marijuana = as.numeric( qn45 == 1 ) ,
		tried_to_quit_tobacco_past_year = as.numeric( q39 == 2 ) ,
		used_tobacco_past_year = as.numeric( q39 > 1 )
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
	ci = TRUE ,
	keep.var = TRUE ,
	na.rm = TRUE
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
svyciprop( ~ never_rarely_wore_seat_belt , yrbss_design ,
	method = "likelihood" , na.rm = TRUE )
svyttest( bmipct ~ never_rarely_wore_seat_belt , yrbss_design )
svychisq( 
	~ never_rarely_wore_seat_belt + q2 , 
	yrbss_design 
)
glm_result <- 
	svyglm( 
		bmipct ~ never_rarely_wore_seat_belt + q2 , 
		yrbss_design 
	)

summary( glm_result )
library(srvyr)
yrbss_srvyr_design <- as_survey( yrbss_design )
yrbss_srvyr_design %>%
	summarize( mean = survey_mean( bmipct , na.rm = TRUE ) )

yrbss_srvyr_design %>%
	group_by( ever_used_marijuana ) %>%
	summarize( mean = survey_mean( bmipct , na.rm = TRUE ) )

unwtd.count( ~ never_rarely_wore_seat_belt , yrbss_design )

svytotal( ~ one , subset( yrbss_design , !is.na( never_rarely_wore_seat_belt ) ) )
 
svymean( ~ never_rarely_wore_seat_belt , yrbss_design , na.rm = TRUE )

svyciprop( ~ never_rarely_wore_seat_belt , yrbss_design , na.rm = TRUE , method = "beta" )

