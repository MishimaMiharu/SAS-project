/*data import with preliminary data processing*/
PROC IMPORT
	DATAFILE = "/home/u60916893/SAS_project/final1.csv"
	OUT = temp
	DBMS = CSV
	replace;
RUN;

/*data processing*/
data temp1;
	set temp;
	last_vacant_duration = last_scraped - last_review;
	cummulation_duration = first_review - host_since;
	age = last_scraped - host_since;
	drop last_scraped last_review first_review host_since;
	if host_response_rate ^= 'nan' then host_response_rate_num = input(host_response_rate,percent.);
	else host_response_rate_num = .;
	if host_acceptance_rate ^= 'nan' then host_acceptance_rate_num = input(host_acceptance_rate,percent.);
	else host_acceptance_rate_num = .;
	drop host_response_rate host_acceptance_rate;	
	length district $10.;
	if neighbourhood = 0 then district = 'Jiading';
	else if neighbourhood = 1 then district = 'Fengxian';
	else if neighbourhood = 2 then district = 'Baoshan';
	else if neighbourhood = 3 then district = 'Chongming';
	else if neighbourhood = 4 then district = 'Xuhui';
	else if neighbourhood = 5 then district = 'Putuo';
	else if neighbourhood = 6 then district = 'Yangpu';
	else if neighbourhood = 7 then district = 'Songjiang';
	else if neighbourhood = 8 then district = 'Pudong';
	else if neighbourhood = 9 then district = 'Hongkou';
	else if neighbourhood = 10 then district = 'Jinshan';
	else if neighbourhood = 11 then district = 'Changning';
	else if neighbourhood = 12 then district = 'Minhang';
	else if neighbourhood = 13 then district = 'Qingpu';
	else if neighbourhood = 14 then district = 'Jing_an';
	else district = 'Huangpu';
	drop neighbourhood;	
	drop latitude longitude;	
	drop property_type;	
	drop bathrooms_text;
	rename num = bathrooms_num;
	rename share_private_NA = bathrooms_shared;
	rename half_full = bathrooms_half;	
	rename amenities = amenities_num;
run;

/*data processing*/
data temp3;
	set temp1;
	if host_response_time = ' ' then host_response_time = 'unsure';
	if review_scores_accuracy = . then review_scores_accuracy = 4;
	if review_scores_cleanliness = . then review_scores_cleanliness = 4;
	if review_scores_checkin  = . then review_scores_checkin = 4;
	if review_scores_communication = . then review_scores_communication = 4;
	if review_scores_location = . then review_scores_location = 4;
	if review_scores_value = . then review_scores_value = 4;
	if host_response_rate_num ^= . then host_response_rate_num = 0.945;
	if host_acceptance_rate_num ^= . then host_acceptance_rate_num = 0.878;
	where bathrooms_shared ^= .;
	drop host_total_listings_count;
	if price > 5000 then delete; 
	if bedrooms = . then delete;
	if beds = . then delete;
	if review_avg = . then review_avg = 0.349;
run;

/*boxcox*/
data temp4;
	set temp3;
	boxcox_price = (price**(-0.3)-1)/(-0.3);
run;

proc sgplot data = temp4;
	histogram boxcox_price;
run;

/*variable selection*/
*rough modeling;
proc glm data = temp4 plots = diagnostics(unpack);
	class host_response_time host_is_superhost host_identity_verified 
	room_type has_availability instant_bookable district bathrooms_shared bathrooms_half deslang hostlang;
	model boxcox_price = host_response_time--amenities_num minimum_nights--district;
run;
*variable select by lasso;
proc glmselect data = temp4 plots (stepaxis = normb) = all;
	class host_response_time host_is_superhost host_identity_verified 
	room_type has_availability instant_bookable district 
	bathrooms_shared bathrooms_half deslang hostlang;
	model boxcox_price = host_response_time--amenities_num minimum_nights--district
	/selection=lasso(stop = aic choose = aic);
run;

/*defining the corresponding dummy variable*/ 
data temp5;
	set temp4;
	if bathrooms_shared = 0 then bathrooms_reg = 0;
	else bathrooms_reg = 1;
	if room_type = 'Entire home/apt' then room_type_reg = 0;
	else if room_type = 'Shared room' then room_type_reg = 1;
	else room_type_reg = 2;
	if district = 'Jiading' then district_reg = 0;
	else if district = 'Chongming' then district_reg =1;
	else if district = 'Songjiang' then district_reg = 2;
	else district_reg = 3;
run;

/*variable transformation*/

%macro scatter(var = ,);
	proc sgplot data = temp5;
		scatter x = &var y = boxcox_price;
	run;
%mend;

%scatter(var=accommodates);
%scatter(var=price_std);
%scatter(var=amenities_num);
%scatter(var = review_scores_location);
%scatter(var = hostscore);

data temp6;
	set temp5;
	log_accommodates = log(accommodates);
	log_price_std = log(price_std+0.001);
	e_review_scores_location = exp(review_scores_location);
	log_hostscore = log(hostscore + 2);
run;

%macro scatter(var = ,);
	proc sgplot data = temp6;
		scatter x = &var y = boxcox_price;
	run;
%mend;

%scatter(var=log_accommodates);
%scatter(var=log_price_std);
%scatter(var=amenities_num);
%scatter(var = e_review_scores_location);
%scatter(var = log_hostscore);

/*interaction detection between cat_variable and con_variable*/
%macro interaction_plot(varx=,cat=);
	proc sgplot data = temp6;
		STYLEATTRS DATACONTRASTCOLORS=(LightSkyBlue LightCoral)WALLCOLOR=WhiteSmoke;
		REG X=&varx Y=boxcox_price / GROUP=&cat MARKERATTRS=(Symbol=CircleFilled) CLM CLMTRANSPARENCY=0.8;
		TITLE 'Interaction Plot';
	RUN;
%mend;	

%interaction_plot(varx = log_accommodates,cat = room_type_reg);/******/
%interaction_plot(varx = log_accommodates, cat = bathrooms_reg);/******/
%interaction_plot(varx = log_accommodates, cat = district_reg);
%interaction_plot(varx = log_price_std, cat = room_type_reg);
%interaction_plot(varx = log_price_std, cat = bathrooms_reg);
%interaction_plot(varx = log_price_std, cat = district_reg);
%interaction_plot(varx = e_review_scores_location, cat = district_reg);/******/
%interaction_plot(varx = e_review_scores_location, cat = room_type_reg);
%interaction_plot(varx = e_review_scores_location, cat = bathrooms_reg);
%interaction_plot(varx = amenities_num, cat = district_reg);
%interaction_plot(varx = amenities_num, cat = room_type_reg);
%interaction_plot(varx = amenities_num, cat = bathrooms_reg);
%interaction_plot(varx = log_hostscore, cat = district_reg);
%interaction_plot(varx = log_hostscore, cat = room_type_reg);
%interaction_plot(varx = log_hostscore, cat = bathrooms_reg);


/*fit the model with all interaction except the one mentioned above and do WLS*/
/*and extract the outlier*/
proc glm data = temp6 plots=diagnostics(unpack);
	class bathrooms_reg room_type_reg district_reg;
	model boxcox_price = log_accommodates room_type_reg bathrooms_reg log_price_std 
	amenities_num e_review_scores_location log_hostscore district_reg
	log_accommodates*room_type_reg log_accommodates*bathrooms_reg e_review_scores_location*district_reg 
	bathrooms_reg*district_reg log_accommodates*log_price_std log_accommodates*log_hostscore 
	log_price_std*log_hostscore log_accommodates*e_review_scores_location log_price_std*amenities_num;
	where price_std ^= 0;
	output out = reg1_1(keep = _all_ r1_1 fv1_1) Residual = r1_1 Predicted = fv1_1;
run;

data reg2_1;
	set reg1_1;
	absr2_1 = abs(r1_1);
run;

proc glm data = reg2_1;
	model absr2_1 = fv1_1 fv1_1*fv1_1;
	output out = reg3_1 (keep = _all_ fv3_1) predicted = fv3_1;
run;

data reg4_1;
	set reg3_1;
	w4_1=1/((fv3_1)**2);
run;

proc glm data = reg4_1 plots=diagnostics(unpack);
	class bathrooms_reg room_type_reg district_reg;
	model boxcox_price = log_accommodates room_type_reg bathrooms_reg log_price_std 
	amenities_num e_review_scores_location log_hostscore district_reg
	log_accommodates*room_type_reg log_accommodates*bathrooms_reg e_review_scores_location*district_reg 
	bathrooms_reg*district_reg log_accommodates*log_price_std log_accommodates*log_hostscore 
	log_price_std*log_hostscore log_accommodates*e_review_scores_location log_price_std*amenities_num;
	weight w4_1;
	output out = reg5_1(keep = _all_ rstu5_1 h5_1 cd5_1) Rstudent = rstu5_1 h = h5_1 cookd = cd5_1;
run;

data reg6_1;
	set reg5_1;
	where abs(rstu5_1) < 3 ;
run;

proc glm data = reg6_1 plots=diagnostics(unpack);
	class bathrooms_reg room_type_reg district_reg;
	model boxcox_price = log_accommodates room_type_reg bathrooms_reg log_price_std 
	amenities_num e_review_scores_location log_hostscore district_reg
	log_accommodates*room_type_reg log_accommodates*bathrooms_reg e_review_scores_location*district_reg 
	bathrooms_reg*district_reg log_accommodates*log_price_std log_accommodates*log_hostscore 
	log_price_std*log_hostscore log_accommodates*e_review_scores_location log_price_std*amenities_num;
	output out = reg7_1(keep = _all_ r7_1 fv7_1)Residual = r7_1 Predicted = fv7_1;
run;


data reg8_1;
	set reg7_1;
	absr8_1 = abs(r7_1);
run;

proc glm data = reg8_1;
	model absr8_1 = fv7_1 fv7_1*fv7_1;
	output out = reg9_1 (keep = _all_ fv9_1) predicted = fv9_1;
run;

data reg10_1;
	set reg9_1;
	w10_1=1/((fv9_1)**2);
run;

proc glm data = reg10_1 /*plots=diagnostics(unpack)*/;
	class bathrooms_reg room_type_reg district_reg;
	model boxcox_price = log_accommodates room_type_reg bathrooms_reg log_price_std 
	amenities_num e_review_scores_location log_hostscore district_reg
	log_accommodates*room_type_reg log_accommodates*bathrooms_reg e_review_scores_location*district_reg 
	bathrooms_reg*district_reg log_accommodates*log_price_std log_accommodates*log_hostscore 
	log_price_std*log_hostscore log_accommodates*e_review_scores_location log_price_std*amenities_num;
	weight w10_1;
	/*output out = reg11_1(keep = _all_ resid11_1) residual = resid11_1;*/
run;

proc univariate data = reg11_1 normal;
	var resid11_1;
run;



/* fit the data when price_std = 0 and play model diag*/
proc glm data = temp6 plots=diagnostics(unpack);
	class bathrooms_reg room_type_reg district_reg;
	model boxcox_price = log_accommodates room_type_reg bathrooms_reg 
	amenities_num e_review_scores_location log_hostscore district_reg
	log_accommodates*room_type_reg log_accommodates*bathrooms_reg e_review_scores_location*district_reg 
	log_accommodates*log_hostscore log_accommodates*amenities_num amenities_num*log_accommodates
	log_accommodates*amenities_num*log_hostscore;
	where price_std = 0;
	output out = reg1_2(keep = _all_ r1_2 fv1_2) Residual = r1_2 Predicted = fv1_2;
run;

data reg2_2;
	set reg1_2;
	absr2_2 = abs(r1_2);
run;

proc glm data = reg2_2;
	model absr2_2 = fv1_2 fv1_2*fv1_2;
	output out = reg3_2 (keep = _all_ fv3_2) predicted = fv3_2;
run;

data reg4_2;
	set reg3_2;
	w4_2=1/((fv3_2)**2);
run;

proc glm data = reg4_2 plots=diagnostics(unpack);
	class bathrooms_reg room_type_reg district_reg;
	model boxcox_price = log_accommodates room_type_reg bathrooms_reg 
	amenities_num e_review_scores_location log_hostscore district_reg
	log_accommodates*room_type_reg log_accommodates*bathrooms_reg e_review_scores_location*district_reg 
	log_accommodates*log_hostscore log_accommodates*amenities_num amenities_num*log_accommodates
	log_accommodates*amenities_num*log_hostscore;
	weight w4_2;
	output out = reg5_2(keep = _all_ rstu5_2 h5_2 cd5_2) Rstudent = rstu5_2 h = h5_2 cookd = cd5_2;
run;

data reg6_2;
	set reg5_2;
	where abs(rstu5_2) < 3 ;
run;

proc glm data = reg6_2 plots=diagnostics(unpack);
	class bathrooms_reg room_type_reg district_reg;
	model boxcox_price = log_accommodates room_type_reg bathrooms_reg 
	amenities_num e_review_scores_location log_hostscore district_reg
	log_accommodates*room_type_reg log_accommodates*bathrooms_reg e_review_scores_location*district_reg 
	log_accommodates*log_hostscore log_accommodates*amenities_num amenities_num*log_accommodates
	log_accommodates*amenities_num*log_hostscore;
	output out = reg7_2(keep = _all_ r7_2 fv7_2)Residual = r7_2 Predicted = fv7_2;
run;


data reg8_2;
	set reg7_2;
	absr8_2 = abs(r7_2);
run;

proc glm data = reg8_2;
	model absr8_2 = fv7_2 fv7_2*fv7_2;
	output out = reg9_2 (keep = _all_ fv9_2) predicted = fv9_2;
run;

data reg10_2;
	set reg9_2;
	w10_2=1/((fv9_2)**2);
run;

proc glm data = reg10_2 plots=diagnostics(unpack);
	class bathrooms_reg room_type_reg district_reg;
	model boxcox_price = log_accommodates room_type_reg bathrooms_reg 
	amenities_num e_review_scores_location log_hostscore district_reg
	log_accommodates*room_type_reg log_accommodates*bathrooms_reg e_review_scores_location*district_reg 
	log_accommodates*log_hostscore log_accommodates*amenities_num amenities_num*log_accommodates
	log_accommodates*amenities_num*log_hostscore;
	weight w10_2;
	output out = reg11_2(keep = _all_ resid11_2) residual = resid11_2;
run;

proc univariate data = reg11_2 normal;
	var resid11_2;
run;












