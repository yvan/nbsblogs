*> field (column, can have primary/secondary keys, descriptions attached)
*> record (basically one row of the file)
*> physical records=disk block, logical records=info used by program
*> file (collection of related records for example student grades)
*> types of file org:
*> sequential: records can read in seduqnce only, new records cannot be inserted
*> in between, added records cannot be changed, records can be completely
*> over written if the new data is the exact same length as the old data, good
*> for printing outputs

INPUT-OUTPUT SECTION.
FILE CONTROL.
  SELECT FILE-NAME ASSIGN TO DD-NAME-JCL
  ORGANIZATION IS SEQUENTIAL

*> indexed: consists of an index file and a data FILE
*> records can be read sequentially, recrods can be
*> fetched by index , sorted index is maintated that
*> realtes a key value to the records position in file

  INPUT-OUTPUT SECTION.
  FILE-CONTROL.
    SELECT FILE-NAME ASSIGN TO DD-NAME-JCL
    ORGANIZATION IS INDEXED
    RECORD KEY IS PRIMARY-KEY
    ALTERNAT RECORD KEY IS REC-KEY

*> relative: records can be sequential order
*> records can be inserted using a relative key
*> relative files provide the fastest access to
*> records, relative key represents the location
*> of a record close tot he beignning of the file

INPUT-OUTPUT SECTION.
FILE-CONTROL.
  SELECT FILE-NAME ASSIGN TO DD-NAME-JCL
  ORGANIZATION IS RELATIVE
  RELATIVE IS REC-KEY

*> files have access modes, sequential, random, dynamic
*> sequential uses the order of insertion for sequential files
*> the index for index files, and the relative reord keys for reltive files
*> sdynamic allows you to switch back n forth among the two.

*> file verbs:
*> open: always have to open a file before other verbs
*> read: reads the file record by record
*> write: writes records to a file
*> rewrite: updates records
*> delete: only good on indexed and relative indexed files, deletes a record
*> start: index/relative files, place pointer at specific record
*> close: close a file
