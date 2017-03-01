CREATE OR REPLACE FUNCTION accessory_tables.create_intermediate_table(tablename text, var1 text, var2 text, var3 text)
RETURNS BOOLEAN AS
$BODY$
DECLARE accessTable text;
BEGIN
accessTable:='accessory_tables';
EXECUTE format(
'IF NOT EXISTS (SELECT 1 FROM pg_tables WHERE  schemaname = %L AND tablename = %L) THEN 
create table %L.%L_%L (
device_ID text,
%L  timestamp,
%L text,
%L text,
created_by text DEFAULT CURRENT_USER,
created_on timestamp DEFAULT CURRENT_TIMESTAMP,
last_modif_by text DEFAULT CURRENT_USER,
last_modif_on timestamp DEFAULT CURRENT_TIMESTAMP,
PRIMARY KEY (device_ID, %L, %L , %L),
FOREIGN KEY (device_ID, %L, %L) REFERENCES main_tables.%L(device_ID, %L, %L) ON UPDATE CASCADE ON DELETE CASCADE
);
IF EXISTS (SELECT 1 FROM pg_tables WHERE  schemaname = %L AND tablename = %L) THEN
ALTER TABLE accessory_tables.%L_%L ADD FOREIGN KEY (%L) REFERENCES accessory_tables.%L(value) ON UPDATE CASCADE ON DELETE CASCADE;END IF;
END IF;
DROP TRIGGER IF EXISTS row_modif_stamp ON accessory_tables.%L_%L;
CREATE TRIGGER row_modif_stamp BEFORE INSERT OR UPDATE ON accessory_tables.%L_%L FOR EACH ROW EXECUTE PROCEDURE main_tables.row_modif_stamp();
' ,accessTable, tablename, accessTable, tablename, var3, var1, var2, var3, var1, var2, var3, var1, var2, tablename, var1, var2, accessTable,var3,
tablename, var3, var3, var3,tablename, var3,tablename, var3);

RETURN TRUE;
END;
$BODY$
LANGUAGE 'plpgsql'


SELECT accessory_tables.create_intermediate_table('continuous_focal_variables', 'focal_start_time', 'continuousVars', 'animal_id');



CREATE OR REPLACE FUNCTION accessory_tables.create_intermediate_table(tablename text, var1 text, var2 text, var3 text)
RETURNS BOOLEAN AS
$BODY$
DECLARE accessTable text;
BEGIN
accessTable:='accessory_tables';
EXECUTE format(
'IF NOT EXISTS (SELECT 1 FROM pg_tables WHERE  schemaname = %L AND tablename = %L) THEN 
create table %L.%L_%L (
device_ID text,
%L  timestamp,
%L text,
%L text,
created_by text DEFAULT CURRENT_USER,
created_on timestamp DEFAULT CURRENT_TIMESTAMP,
last_modif_by text DEFAULT CURRENT_USER,
last_modif_on timestamp DEFAULT CURRENT_TIMESTAMP,
PRIMARY KEY (device_ID, %L, %L , %L),
FOREIGN KEY (device_ID, %L, %L) REFERENCES main_tables.%L(device_ID, %L, %L) ON UPDATE CASCADE ON DELETE CASCADE
);
' ,accessTable, tablename, accessTable, tablename, var3, var1, var2, var3, var1, var2, var3, var1, var2, tablename, var1, var2);

RETURN TRUE;
END;
$BODY$
LANGUAGE 'plpgsql'


SELECT accessory_tables.create_intermediate_table('continuous_focal_variables', 'focal_start_time', 'continuousVars', 'animal_id');