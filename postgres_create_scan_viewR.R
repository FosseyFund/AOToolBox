################################################
########################create/replace views
################################################
scansViewCreate <- function(con){
lsTables <- dbGetQuery(con, "select *  from pg_tables where schemaname!='pg_catalog' AND schemaname!='information_schema';")

mainTablesList <- lsTables[lsTables$schemaname=="main_tables",]$tablename
if(prod(c("list_sessions", "list_focals", "list_scans")%in%mainTablesList)==0) stop("list_sessions, list_focals and/or list_scans are missing in this database!")
lsScanDataCols <- dbGetQuery(con, "SELECT *
FROM information_schema.columns
WHERE table_schema = 'main_tables'
  AND table_name   = 'scan_data'")$column_name
lsScanDataCols <- lsScanDataCols[! lsScanDataCols%in%c("device_id","scan_time","scanned_individual_id","x_position","y_position","created_by","created_on","last_modif_by","last_modif_on")]
  
accessoryTablesList <- lsTables[lsTables$schemaname=="accessory_tables",]$tablename
normalizedTables <- accessoryTablesList[regexpr("scan_data_", accessoryTablesList)==1]
if(length(normalizedTables)==0) {
	return(paste("DROP VIEW IF EXISTS main_tables.all_scan_data_view;
CREATE VIEW main_tables.all_scan_data_view AS (

 SELECT list_sessions.device_id,
    list_sessions.session_start_time,
    list_sessions.session_end_time,
    list_sessions.group_id,
    list_sessions.pin_code_name,
    list_focals.focal_start_time,
    list_focals.focal_end_time,
    list_focals.focal_individual_id,
    list_scans.scan_time,
    scan_data.scanned_individual_id,
    ",paste(paste('scan_data', lsScanDataCols, sep="."), collapse=', '),",
    scan_data.x_position,
    scan_data.y_position,
    list_scans.latitude,
    list_scans.longitude,
    list_scans.gps_horizontal_precision,
    list_scans.altitude,
    list_sessions.gps_on,
    list_sessions.compass_on,
    list_sessions.map_mode_on,
    list_sessions.physical_contact_threshold,
    list_sessions.layout_info_json_version,
    list_sessions.behaviors_json_version,
    list_focals.set_duration,
    list_focals.set_scan_interval
   FROM list_sessions
     LEFT JOIN list_focals ON ((list_focals.device_id = list_sessions.device_id) AND (list_focals.session_start_time = list_sessions.session_start_time))
     LEFT JOIN list_scans ON ((list_focals.device_id = list_scans.device_id) AND (list_focals.focal_start_time = list_scans.focal_start_time))
     LEFT JOIN scan_data ON ((list_scans.device_id = scan_data.device_id) AND (list_scans.scan_time = scan_data.scan_time))

  ORDER BY ROW(list_sessions.session_start_time, list_focals.focal_start_time, list_scans.scan_time)
);"))
} else {

aggVars <- gsub("scan_data_", "", normalizedTables)
 
list_scans_agg <- function(agg="part_eaten", index=1)
{
	
	paste0("(SELECT foo.device_id,
            foo.scan_time,
            foo.scanned_individual_id,",
            paste(paste('foo', lsScanDataCols, sep="."), collapse=', '),",",
 			paste0('string_agg(foo.',agg,", ';') AS ", agg,","),
            "foo.x_position,
            foo.y_position
           FROM (SELECT scan_data.device_id,
                    scan_data.scan_time,
                    scan_data.scanned_individual_id,",
                    paste(paste('scan_data', lsScanDataCols, sep="."), collapse=', '),",",
                    paste0("scan_data_", agg,".",agg),",
                    scan_data.x_position,
                    scan_data.y_position
                   FROM (scan_data
                     LEFT JOIN accessory_tables.scan_data_",agg," ON (((scan_data.device_id = scan_data_",agg,".device_id) AND (scan_data.scan_time = scan_data_",agg,".scan_time) AND (scan_data.scanned_individual_id = scan_data_",agg,".scanned_individual_id))))) AS foo
          GROUP BY foo.device_id, foo.scan_time, foo.scanned_individual_id, ",
paste(paste('foo', lsScanDataCols, sep="."), collapse=', ')
          ,", foo.x_position, foo.y_position) AS scan_data",index)
}

#list_scans_agg(agg="part_eaten", index=1)


leftOuterJoin <- function(nVars=3){
ans <- character(0)
for(i in 2:nVars){
	ans <- c(ans,
  paste0("LEFT OUTER JOIN ",list_scans_agg(agg= aggVars[i], index=i)," ON (((scan_data1.device_id = scan_data",i,".device_id) AND (scan_data1.scan_time = scan_data",i,".scan_time) AND (scan_data1.scanned_individual_id = scan_data",i,".scanned_individual_id)))"))
  }
  return(ans)
}
#paste(leftOuterJoin(nVars=2), collapse="\n ")



return(paste("DROP VIEW IF EXISTS main_tables.all_scan_data_view;
CREATE VIEW main_tables.all_scan_data_view AS (

 SELECT list_sessions.device_id,
    list_sessions.session_start_time,
    list_sessions.session_end_time,
    list_sessions.group_id,
    list_sessions.pin_code_name,
    list_focals.focal_start_time,
    list_focals.focal_end_time,
    list_focals.focal_individual_id,
    list_scans.scan_time,
    scan_data.scanned_individual_id,
    ",paste(paste('scan_data', lsScanDataCols, sep="."), collapse=', '),",
    ",paste(paste('scan_data' ,aggVars, sep="."), collapse=', '),",
    scan_data.x_position,
    scan_data.y_position,
    list_scans.latitude,
    list_scans.longitude,
    list_scans.gps_horizontal_precision,
    list_scans.altitude,
    list_sessions.gps_on,
    list_sessions.compass_on,
    list_sessions.map_mode_on,
    list_sessions.physical_contact_threshold,
    list_sessions.layout_info_json_version,
    list_sessions.behaviors_json_version,
    list_focals.set_duration,
    list_focals.set_scan_interval
   FROM list_sessions
     LEFT JOIN list_focals ON ((list_focals.device_id = list_sessions.device_id) AND (list_focals.session_start_time = list_sessions.session_start_time))
     LEFT JOIN list_scans ON ((list_focals.device_id = list_scans.device_id) AND (list_focals.focal_start_time = list_scans.focal_start_time))
     LEFT JOIN (SELECT scan_data1.device_id,
            scan_data1.scan_time,
            scan_data1.scanned_individual_id,",
            paste(paste('scan_data1', lsScanDataCols, sep="."), collapse=', '),",",
            paste(paste(paste0('scan_data',1:length(aggVars)) ,aggVars, sep="."), collapse=', '),",
            scan_data1.x_position,
            scan_data1.y_position
FROM ",
  list_scans_agg(agg=aggVars[1], index=1),
paste(leftOuterJoin(nVars=length(aggVars)), collapse="\n "),
") AS scan_data ON (((list_scans.device_id = scan_data.device_id) AND (list_scans.scan_time = scan_data.scan_time)))
  ORDER BY ROW(list_sessions.session_start_time, list_focals.focal_start_time, list_scans.scan_time)
);"))
}
}

