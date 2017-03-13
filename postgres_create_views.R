
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
	return(paste("DROP VIEW IF EXISTS main_tables.all_focal_data_view2;
CREATE VIEW main_tables.all_focal_data_view2 AS (

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

return(paste("DROP VIEW IF EXISTS main_tables.all_focal_data_view2;
CREATE VIEW main_tables.all_focal_data_view2 AS (

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
