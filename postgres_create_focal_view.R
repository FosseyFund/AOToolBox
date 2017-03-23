
################################################
########################create/replace views
################################################
behaviorsViewCreate <- function(con){
lsTables <- dbGetQuery(con, "select *  from pg_tables where schemaname!='pg_catalog' AND schemaname!='information_schema';")

mainTablesList <- lsTables[lsTables$schemaname=="main_tables",]$tablename
if(prod(c("list_sessions", "list_focals", "list_behaviors")%in%mainTablesList)==0) stop("list_sessions, list_focals and/or list_behaviors are missing in this database!")
lsBehaviorsCols <- dbGetQuery(con, "SELECT *
FROM information_schema.columns
WHERE table_schema = 'main_tables'
  AND table_name   = 'list_behaviors'")$column_name
lsBehaviorsCols <- lsBehaviorsCols[!lsBehaviorsCols%in%c("device_id","focal_start_time","behavior_time","actor","subject","comment","latitude","longitude","gps_horizontal_precision","altitude","created_by","created_on","last_modif_by","last_modif_on")]
  
accessoryTablesList <- lsTables[lsTables$schemaname=="accessory_tables",]$tablename
normalizedTables <- accessoryTablesList[regexpr("list_behaviors_", accessoryTablesList)==1]
if(length(normalizedTables)==0) {
	return(paste("DROP VIEW IF EXISTS main_tables.all_focal_data_view;
CREATE VIEW main_tables.all_focal_data_view AS (

 SELECT list_sessions.device_id,
    list_sessions.session_start_time,
    list_sessions.session_end_time,
    list_sessions.group_id,
    list_sessions.pin_code_name,
    list_focals.focal_start_time,
    list_focals.focal_end_time,
    list_focals.focal_individual_id,
    list_behaviors.behavior_time,
    list_behaviors.actor,
    list_behaviors.subject,
    ",paste(paste('list_behaviors', lsBehaviorsCols, sep="."), collapse=', '),",
    list_behaviors.comment,
    list_behaviors.latitude,
    list_behaviors.longitude,
    list_behaviors.gps_horizontal_precision,
    list_behaviors.altitude,
    list_sessions.gps_on,
    list_sessions.compass_on,
    list_sessions.map_mode_on,
    list_sessions.physical_contact_threshold,
    list_sessions.layout_info_json_version,
    list_sessions.behaviors_json_version,
    list_focals.set_duration,
    list_focals.set_scan_interval
   FROM ((list_sessions
     LEFT JOIN list_focals ON (((list_focals.device_id = list_sessions.device_id) AND (list_focals.session_start_time = list_sessions.session_start_time))))
     LEFT JOIN list_behaviors ON (((list_focals.device_id = list_behaviors.device_id) AND (list_focals.focal_start_time = list_behaviors.focal_start_time))))
  ORDER BY ROW(list_sessions.session_start_time, list_focals.focal_start_time, list_behaviors.behavior_time)
);"))
} else {

aggVars <- gsub("list_behaviors_", "", normalizedTables) 
 
list_behav_agg <- function(agg="part_eaten", index=1)
{
	
	paste0("(SELECT foo.device_id,
            foo.focal_start_time,
            foo.behavior_time,
            foo.actor,
            foo.subject,",
            paste(paste('foo', lsBehaviorsCols, sep="."), collapse=', '),",",
 			paste0('string_agg(foo.',agg,", ';') AS ", agg,","),
            "foo.comment,
            foo.latitude,
            foo.longitude,
            foo.gps_horizontal_precision,
            foo.altitude
           FROM ( SELECT list_behaviors.device_id,
                    list_behaviors.focal_start_time,
                    list_behaviors.behavior_time,
                    list_behaviors.actor,
                    list_behaviors.subject,",
                    paste(paste('list_behaviors', lsBehaviorsCols, sep="."), collapse=', '),",",
					paste0("list_behaviors_", agg,".",agg),",
                    list_behaviors.comment,
                    list_behaviors.latitude,
                    list_behaviors.longitude,
                    list_behaviors.gps_horizontal_precision,
                    list_behaviors.altitude
                    
                   FROM (list_behaviors
                     LEFT JOIN accessory_tables.list_behaviors_",agg," ON (((list_behaviors.device_id = list_behaviors_",agg,".device_id) AND (list_behaviors.behavior_time = list_behaviors_",agg,".behavior_time) AND (list_behaviors.actor = list_behaviors_",agg,".actor) AND (list_behaviors.subject = list_behaviors_",agg,".subject))))) AS foo
          GROUP BY foo.device_id, foo.focal_start_time, foo.behavior_time, foo.actor, foo.subject, ",
paste(paste('foo', lsBehaviorsCols, sep="."), collapse=', ')
          ,", foo.comment, foo.latitude, foo.longitude, foo.gps_horizontal_precision, foo.altitude) AS list_behaviors",index)
}

#list_behav_agg(agg="part_eaten", index=1)


leftOuterJoin <- function(nVars=3){
ans <- character(0)
for(i in 2:nVars){
	ans <- c(ans,
  paste0("LEFT OUTER JOIN ",list_behav_agg(agg= aggVars[i], index=i)," ON (((list_behaviors1.device_id = list_behaviors",i,".device_id) AND (list_behaviors1.behavior_time = list_behaviors",i,".behavior_time) AND (list_behaviors1.actor = list_behaviors",i,".actor) AND (list_behaviors1.subject = list_behaviors",i,".subject)))"))
  }
  return(ans)
}
#paste(leftOuterJoin(nVars=3), collapse="\n ")



return(paste("DROP VIEW IF EXISTS main_tables.all_focal_data_view;
CREATE VIEW main_tables.all_focal_data_view AS (

 SELECT list_sessions.device_id,
    list_sessions.session_start_time,
    list_sessions.session_end_time,
    list_sessions.group_id,
    list_sessions.pin_code_name,
    list_focals.focal_start_time,
    list_focals.focal_end_time,
    list_focals.focal_individual_id,
    list_behaviors.behavior_time,
    list_behaviors.actor,
    list_behaviors.subject,
    ",paste(paste('list_behaviors', lsBehaviorsCols, sep="."), collapse=', '),",
    ",paste(paste('list_behaviors' ,aggVars, sep="."), collapse=', '),",
    list_behaviors.comment,
    list_behaviors.latitude,
    list_behaviors.longitude,
    list_behaviors.gps_horizontal_precision,
    list_behaviors.altitude,
    list_sessions.gps_on,
    list_sessions.compass_on,
    list_sessions.map_mode_on,
    list_sessions.physical_contact_threshold,
    list_sessions.layout_info_json_version,
    list_sessions.behaviors_json_version,
    list_focals.set_duration,
    list_focals.set_scan_interval
   FROM ((list_sessions
     LEFT JOIN list_focals ON (((list_focals.device_id = list_sessions.device_id) AND (list_focals.session_start_time = list_sessions.session_start_time))))
     LEFT JOIN (SELECT list_behaviors1.device_id,
            list_behaviors1.focal_start_time,
            list_behaviors1.behavior_time,
            list_behaviors1.actor,
            list_behaviors1.subject,",
            paste(paste('list_behaviors1', lsBehaviorsCols, sep="."), collapse=', '),",",
            paste(paste(paste0('list_behaviors',1:length(aggVars)) ,aggVars, sep="."), collapse=', '),",
            list_behaviors1.comment,
            list_behaviors1.latitude,
            list_behaviors1.longitude,
            list_behaviors1.gps_horizontal_precision,
            list_behaviors1.altitude
FROM ",
  list_behav_agg(agg=aggVars[1], index=1),
paste(leftOuterJoin(nVars=length(aggVars)), collapse="\n "),
") AS list_behaviors ON (((list_focals.device_id = list_behaviors.device_id) AND (list_focals.focal_start_time = list_behaviors.focal_start_time))))
  ORDER BY ROW(list_sessions.session_start_time, list_focals.focal_start_time, list_behaviors.behavior_time)
);"))
}
}

######################################
#########################trigger function
######################################

behaviorsViewTriggerCreate <- function(con){
	lsTables <- dbGetQuery(con, "select *  from pg_tables where schemaname!='pg_catalog' AND schemaname!='information_schema';")

	mainTablesList <- lsTables[lsTables$schemaname=="main_tables",]$tablename
if(prod(c("list_sessions", "list_focals", "list_behaviors")%in%mainTablesList)==0) stop("list_sessions, list_focals and/or list_behaviors are missing in this database!")
lsBehaviorsCols <- dbGetQuery(con, "SELECT *
FROM information_schema.columns
WHERE table_schema = 'main_tables'
  AND table_name   = 'list_behaviors'")$column_name
lsBehaviorsCols <- lsBehaviorsCols[!lsBehaviorsCols%in%c("device_id","focal_start_time","behavior_time","actor","subject","comment","latitude","longitude","gps_horizontal_precision","altitude","created_by","created_on","last_modif_by","last_modif_on")]
  
accessoryTablesList <- lsTables[lsTables$schemaname=="accessory_tables",]$tablename
normalizedTables <- accessoryTablesList[regexpr("list_behaviors_", accessoryTablesList)==1]
if(length(normalizedTables)!=0) {
	
insertNewNormalizedTableItem <- function(agg='food_item'){
	paste0("IF NEW.",agg," IS NOT NULL THEN
FOR i in SELECT unnest(string_to_array(NEW.",agg,", ';'))
LOOP
INSERT INTO accessory_tables.list_behaviors_",agg," (device_id, 
		behavior_time, 
		actor,
		subject,
		",agg,") 
	VALUES(NEW.device_id, 
		NEW.behavior_time, 
		NEW.actor,
		NEW.subject,
		i
		)
	ON CONFLICT DO NOTHING;
END LOOP;
END IF;"
)
}
aggVars <- gsub("list_behaviors_", "", normalizedTables) 

temp <- paste(unlist(lapply(aggVars, insertNewNormalizedTableItem)), collapse="\n")
} else temp <- "" 


	return(paste0("
	CREATE OR REPLACE FUNCTION main_tables.all_focal_data_view_insert()
RETURNS TRIGGER
LANGUAGE plpgsql
AS $function$
DECLARE i text;
BEGIN
	IF (NEW.device_id IS NOT NULL AND 
	NEW.session_start_time IS NOT NULL) THEN
        INSERT INTO  main_tables.list_sessions (device_id,
		session_start_time,
		session_end_time,
		group_id,
		pin_code_name,
		layout_info_json_version,
		behaviors_json_version,
		gps_on,
		compass_on, 
		map_mode_on,
		physical_contact_threshold)
	VALUES(NEW.device_id,
		NEW.session_start_time,
		NEW.session_end_time,
		NEW.group_id,
		NEW.pin_code_name,
		NEW.layout_info_json_version,
		NEW.behaviors_json_version,
		NEW.gps_on,
		NEW.compass_on,
		NEW.map_mode_on,
		NEW.physical_contact_threshold) 
	ON CONFLICT DO NOTHING;
	IF (NEW.focal_start_time IS NOT NULL) THEN
	

INSERT INTO  main_tables.list_focals (device_id, 
		session_start_time, 
		focal_start_time, 
		focal_end_time, 
		set_duration, 
		set_scan_interval,
		focal_individual_id) 
	VALUES(NEW.device_id,
		NEW.session_start_time, 
		NEW.focal_start_time,
		NEW.focal_end_time, 
		NEW.set_duration, 
		NEW.set_scan_interval, 
		NEW.focal_individual_id)
	ON CONFLICT DO NOTHING;

	IF (NEW.behavior_time IS NOT NULL AND
		NEW.actor IS NOT NULL AND
		NEW.subject IS NOT NULL) THEN
	INSERT INTO  main_tables.list_behaviors (device_id, 
		focal_start_time, 
		behavior_time, 
		actor,
		subject,
		", paste(lsBehaviorsCols, collapse=', '),",		
		comment,
		latitude,
		longitude,
		gps_horizontal_precision,
		altitude) 
	VALUES(NEW.device_id, 
		NEW.focal_start_time, 
		NEW.behavior_time, 
		NEW.actor,
		NEW.subject,
		", paste(paste('NEW', lsBehaviorsCols, sep="."), collapse=', '), ",
		NEW.comment,
		NEW.latitude,
		NEW.longitude,
		NEW.gps_horizontal_precision,
		NEW.altitude)
	ON CONFLICT DO NOTHING;",
	temp,"
END IF;
END IF;
END IF;

    RETURN NEW;
 
    END;
$function$;

DROP TRIGGER IF EXISTS all_focal_data_view_insert_trig ON main_tables.all_focal_data_view;
CREATE TRIGGER all_focal_data_view_insert_trig
    INSTEAD OF INSERT ON
      main_tables.all_focal_data_view FOR EACH ROW EXECUTE PROCEDURE main_tables.all_focal_data_view_insert();
"))
}








































