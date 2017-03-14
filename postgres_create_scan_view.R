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


######################################
#########################trigger function
######################################

scansViewTriggerCreate <- function(){
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


if(length(normalizedTables)!=0) {
      
insertNewNormalizedTableItem <- function(agg='food_item'){
      paste0("IF NEW.",agg," IS NOT NULL THEN
FOR i in SELECT unnest(string_to_array(NEW.",agg,", ';'))
LOOP
INSERT INTO accessory_tables.scan_data_",agg," (device_id, 
            scan_time, 
            scanned_individual_id,
            ",agg,") 
      VALUES(NEW.device_id, 
            NEW.scan_time, 
            NEW.scanned_individual_id,
            i
            )
      ON CONFLICT DO NOTHING;
END LOOP;
END IF;"
)
}
aggVars <- gsub("scan_data_", "", normalizedTables)

temp <- paste(unlist(lapply(aggVars, insertNewNormalizedTableItem)), collapse="\n")
} else temp <- "" 

      return(paste0("
      CREATE OR REPLACE FUNCTION main_tables.all_scan_data_view_insert()
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

      IF (NEW.scan_time IS NOT NULL) THEN
      INSERT INTO  main_tables.list_scans (device_id, 
            focal_start_time, 
            scan_time, 
            latitude,
            longitude,
            gps_horizontal_precision,
            altitude) 
      VALUES(NEW.device_id, 
            NEW.focal_start_time, 
            NEW.scan_time, 
            NEW.latitude,
            NEW.longitude,
            NEW.gps_horizontal_precision,
            NEW.altitude)
      ON CONFLICT DO NOTHING;
      
      IF (NEW.scanned_individual_id IS NOT NULL) THEN
      INSERT INTO  main_tables.scan_data (
          device_id, 
          scan_time, 
          scanned_individual_id,
          ", paste(lsScanDataCols, collapse=', '),",      
          x_position,
          y_position) 
      VALUES(NEW.device_id, 
          NEW.scan_time, 
          NEW.scanned_individual_id,
          ", paste(paste('NEW', lsScanDataCols, sep="."), collapse=', '), ", 
          NEW.x_position,
          NEW.y_position)
     ON CONFLICT DO NOTHING;",
     temp,"
END IF;
END IF;
END IF;
END IF;

    RETURN NEW;
 
    END;
$function$;

DROP TRIGGER IF EXISTS all_scan_data_view_insert_trig ON main_tables.all_scan_data_view;
CREATE TRIGGER all_scan_data_view_insert_trig
    INSTEAD OF INSERT ON
      main_tables.all_scan_data_view FOR EACH ROW EXECUTE PROCEDURE main_tables.all_scan_data_view_insert();
"))
}


scansViewTriggerCreate()






































