*> sort verb exists can be used to sort a file
*> sorting uses a temp work file to hold  records
*> while sort is running

SORT work-file ON ASCENDING KEY rec-key1
USING input-file GIVING output-file.

*> also there is merge to merge? files

MERGE WORK-FILE ON ASCENDING KEY REC-KEY1
USING INPUT-1, INPUT-2 GIVING OUTPUT-FILE.

*> merge opens the file, trasnfers records to work file
*> sorts the sort-file
*>transfers sorted records from work file to out file
*>closes input file and the output file and deletes the work file.
