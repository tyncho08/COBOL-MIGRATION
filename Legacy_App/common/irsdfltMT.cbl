       >>source free
*>***********************************************
*>                                              *
*>                   irsdflt                    *
*>               Table RDB Handler              *
*>                                              *
*> This is a modified variant of a DAL for irs  *
*>  as it uses reduced functions.               *
*>                                              *
*>***********************************************
*>
 identification division.
 Program-Id.            irsdfltMT.
*>**
*> Author.              Vincent B Coen, FBCS, FIDM, FIDPM, CPL
*>                      for Applewood Computers.
*>**
*> Security.            Copyright (C) 2016 & later, Vincent Bryan Coen.
*>                      Distributed under the GNU General Public License
*>                      v2.0. Only. See the file COPYING for details.
*>**
*> Remarks.             irsdflt  File RDB Handler using amended JC preSQL.
*>                      **********************************************
*>
*>                      This version uses an amended JC pre-Sql processor.
*>                      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
*>
*>                      This and associated modules relate to ACAS versions v3.02 and later.
*>
*>                      This modules does all file handling using RDB and is called
*>                      from the ACAS
*>                      irsdflt (as well as final, system-4 and system) file handler acas000
*>                      which in turn is called
*>                      from each ACAS application module that requires file/RDB data handling.
*>
*>                      If RDBMS (Relational DataBase Management Systems) is in use it will be
*>                      called by the specific module to handle similar processing as a Cobol
*>                      flat file and will pass the equivelent
*>                      RDB (Relational DataBase) row as a Cobol file record (01 level) moving
*>                      row by row to the correct Cobol flat file fields as required.
*>
*>                      RDB DAL (Data Access Layer) modules are individually modified to handle:
*>                      MS SQL server, Mysql, DB2, Postgres and Oracle as available and tested.
*>                      These are contained in seperate directories for each RDB, eg
*>                       'MSSQL' (MS SQL Server), 'Mysql', 'DB2', 'Postgres'. 'Oracle'.
*>                       You need to compile from the correct directory for the specific
*>                       RDB you will use and have installed along with all of the development
*>                       libraries and include files etc using the correct pre-compiler tool
*>                       to process the 'EXEC SQL' code converting to Cobol calls etc.
*>                       see the RDB specific ACAS notes.
*>
*>                      For specific SQL servers supported, the pre-compiler system is included
*>                       where ever possible but for some, copyright reasons may prevent
*>                       inclusion. In some cases for one specific RDB more than one precompiler
*>                       is used as an experiment to help find the ideal one to use.
*>
*>                      In addition:
*>                        If the system has been set up to (see the System File set up via the
*>                        main menu module for each sub system), it will also process BOTH flat
*>                        file  AND the correct rdb tables,
*>
*>                        it will write/delete/update etc to both but read from 1=Flat and be
*>                        overwritten by the rdb access if data is present.
*>                       This will help in transferring the Cobol flat files to rdb tables.
*>
*>                      If you wish to convert a running ACAS system over from Flat files
*>                      to RDBMS see below. However it is recommended to use the Duplicate
*>                      processing of files/table as outlined above where no or very low
*>                      data records exist:
*>
*>                      Depending on the RDB you wish to use there is
*>                      also, included LMs (Load Modules) to convert each ISAM
*>                      (Indexed Sequential) file to the rdb database tables if you wish to
*>                      convert the system in one hit, without using the Duplicate file/RDB
*>                      processing procedures. These will also need to be compiled from the
*>                      specific LM directory that contains the rdb DAL modules.
*>                      These will be very RDB specific.
*>
*>                      For a existing running ACAS system using the Load modules AND
*>                      not using the dup processing process as indicated above is the
*>                      recommended way to go.
*>**
*>  File Handling:
*>     Uses the Global Error / Access logging file within the acas0nn module.
*>**
*> Called by Modules:
*>                      acasirsub3 - irsdflt  Cobol Handler.
*>
*>**
*> Error Messages Used.
*>                      SM004 SQL Err no in 'mysql-procedures'
*>                      SM901 Note error and hit return.
*>**
*> Version.             1.00 17/06/2016.
*>
*>**
*> Changes.
*> 04/08/16 vbc - .01 Removed file action logging to sep. module ditto acas011.
*> 18/07/16 vbc - .02 Insert call for SQL error msg where missing and move
*>                    spaces / zero to SQL-Msg| Err and other little fixes/cleanups.
*>                    Remap all changes to the .scb source file.
*> 20/07/16 vbc - .03 Close logger if testing-1 set on table close.
*>                    branch to 999-end instead of 998-free when EOF on read next.
*>                    Seems to auto release if end of cursor data reached
*>                    and causes an abort ??
*> 24/07/16 vbc -     Taken from the stockMT sources with same versioning.
*> 30/07/16 vbc - .04 Change Open and Close to also show table name in operation.
*> 31/07/16 vbc - .05 Moved mv spaces/zero in Write to before insert.
*>                    Remove fhlogger file close in Process-Close.
*>                    Forgot error 989 so commented.
*> 27/08/16 vbc -     Taken from systemMT with same versioning.
*> 18/09/16 vbc -     Temp added code to find out why a SQL error happens turned out
*>                    that using as a SL or PL delimiter a \ screws it up when processing
*>                    as it would need another '\'. There are others '
*>                    This could also be a problem for name and address fields.<<<<<
*>                    May be its to do with the windoz path type delim.
*>                    Solution do NOT use it.
*> 23/09/16 vbc - .06 Removed the lock/unlock as these are not needed with Mysql as
*>                    it will do itself when using select + update/delete according to
*>                    "Definitive guide to MySQL 5 page 254" see text :
*> Tips MySQL always executes individual commands in such a way that they cannot be
*> influenced by other commands. Therefore, no locking is necessary for a single command
*> UPDATE … WHERE id=1234 or DELETE …WHERE id=5678.
*> You need to execute locking only if you are executing several interdependent commands
*> during which another client should not alter the underlying data. Usually, that is the
*> case when you first execute a SELECT command and then process the resulting data with an
*> UPDATE or DELETE command.
*> LOCK and UNLOCK should not be used for InnoDB tables. With older versions of MySQL
*> (< 4.0.22), the locking mechanisms of MySQL and InnoDB can get in each other’s way. LOCK
*> also ends the current transaction, which is often an undesired outcome. Instead, use
*> transactions that block only individual records (instead of entire tables).
*> If you really wish to block an entire InnoDB table, there is available a new command,
*> since MySQL 5.0.3, namely, LOCK TABLE[S] TRANSACTIONAL.
*>
*>  REMEMBER to remove for all other DAL units.
*>
*> 25/09/16 vbc - .07 Changed for one Cobol File/Record to many table rows - irsdfltMT and finalMT.
*> 30/12/16 vbc - .08 Now using Sql-State as well.
*>                    THIS is NOT having the sqlstate or errno between writes/rewrites
*>                     might be needed.
*> 01/03/18 vbc - .11 Renamed error messages to SM901, SM004 as needed.
*> 16/04/24 vbc       Copyright notice update superseding all previous notices.
*>**
*>  Module USAGE
*>**************
*>
*>    On Entry:         Set up Linkage areas -
*>    ********              Default-Record
*>                                   = Contents of data record to be written/read
*>                          File-Access = File-Function as needed.
*>                                        Access-Type   as needed.
*>                          File-Defs (File-Definitions) = Paths set up.
*>
*>    On Exit:          Linkage contains:
*>    *******               Record = containing a read data record or table row
*>                          Fs-Reply = 0, 99 or other value where
*>                                     0  = Operation completed successfully
*>                                     10 = End of (Cobol) File returned to calling module only.
*>                                     21 = Invalid key on START OR key not found
*>                                     22 - Attempt to duplicate a key value.
*>                                     23 = Key not found.     from read indexed
*>                                     99 = Indicates an error see WE-Error, SQL-ERR/MSG for more info
*>                          WE-Error   0    = Operation completed successfully
*>                                     1+   = Failure to open IRS file
*>                                     2+   = Indexed IRS record not found
*>                                     3+   = IRS EOF reached
*>
*>  Above values 1, 2 & 3 override that in FS-Reply for irs
*>
*>                                     999  = Not used here - Yet.
*>                                     998* = File-Key-No Out Of Range not 1, 2 or 3.
*>                                     997* = Access-Type wrong (< 5 or > 8)
*>                                     996* = File Delete key out of range (not = 1 or 2)
*>                                     995* = During Delete SQLSTATE not '00000' investigate using MSG-Err/Msg
*>                                     994* = During Rewrite,                     ^^ see above ^^
*>                                     992* = Invalid Function requested in File-Function
*>                                     990* = Unknown and unexpected error, again ^^ see above ^^
*>                                     989* = Unexpected error on Read-Indexed, investigate as above.
*>                                     911* = Rdb Error during initializing,
*>                                            possibly can not connect to database
*>                                             Check connect data and
*>                                             see SQL-Err & SQL-MSG
*>                                            Produced by Mysql-1100-Db-Error in copy
*>                                            module mysql-procedure.
*>                                     910* = Table locked > 5 seconds
*>                                     901  = File Def Record size not =< than ws record size
*>                                            Module needs ws definition changing to correct size
*>                                            FATAL, Stop using system, fix source code
*>                                            and recompile before using system again.
*>                                     Other = any other rdbms errors see specific
*>                                             (Rdbms) manual
*>                          SQL-Err  = Error code from RDBMS is set if above 2 are non zero
*>                          SQL-Msg  = Non space providing more info if SQL-Err non '00000'
*>                                     * = FS-Reply = 99.
*>                                     + = IRS FH & DAL Only.
*>
 copy "ACAS-SQLstate-error-list.cob".
*>
*>       During testing a log file will be created containing datetime stamp, task and return codes
*>       for both FS-Reply and WE-Error and table used - in this case the Stock File.
*>       WARNING - This file could get large so needs manually deleting after examination.
*>
*>       NOTE that the value in SQL-Err is the standard ANSI RDBMS error code to help keep
*>         DAL handler changes to a minimum when reusing code for other RDBs - hopefully
*>
*>        SHOULD THIS BE IN THE DAL ONLY ?  Only for RDB accessing.
*>
*>********************************************************************************************
*>
*> Copyright Notice.
*> ****************
*>
*> This notice supersedes all prior copyright notices & was updated 2024-04-16.
*>
*> These files and programs are part of the Applewood Computers Accounting
*> System and is Copyright (c) Vincent B Coen. 1976-2025 and later.
*>
*> This program is now free software; you can redistribute it and/or modify it
*> under the terms listed here and of the GNU General Public License as
*> published by the Free Software Foundation; version 3 and later as revised
*> for PERSONAL USAGE ONLY and that includes for use within a business but
*> EXCLUDES repackaging or for Resale, Rental or Hire in ANY way.
*>
*> Persons interested in repackaging, redevelopment for the purpose of resale or
*> distribution in a rental or hire mode must get in touch with the copyright
*> holder with your commercial plans and proposals.
*>
*> ACAS is distributed in the hope that it will be useful, but WITHOUT
*> ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
*> FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
*> for more details. If it breaks, you own both pieces but I will endeavour
*> to fix it, providing you tell me about the problem.
*>
*> You should have received a copy of the GNU General Public License along
*> with ACAS; see the file COPYING.  If not, write to the Free Software
*> Foundation, 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA.
*>
*>**********************************************************************************
*>
 environment division.
 copy "envdiv.cob".
*>
 input-output section.
 Data Division.
 Working-Storage Section.
 77  prog-name          pic x(19)    value "irsdfltMT (3.02.11)".
*>
*> JC WS requirements here
*>
 77  ws-Where           pic x(512).
*>
*>  Used within presql generated code
*>
 01  WS-Reply           pic x           value space.
 01  WS-MYSQL-I         PIC S9(6) COMP.
 01  WS-MYSQL-EDIT      PIC -Z(18)9.9(9).
 01  WS-Key             pic 99.
*>
*> The communication area for the MySQL database changed for free/mysql
*>
*>  jc preSQL MySQL stuff ends
*>
*> Metadata on primary and alternate keys...  from Prima DAL for IRSDFLT-REC
*>
 01  Table-Of-Keynames.
     03  filler         pic x(30) value 'DEF-REC-KEY              '.
     03  filler         pic x(8)  value '00010001'. *> offset/length  as one byte (tinyint)
     03  filler         pic x(3)  value 'STR'. *> data type
 01  filler redefines table-of-keynames.
     03  keyOfReference occurs 1   indexed by KOR-x1.
         05  KeyName    pic x(30).
         05  KOR-Offset pic 9(4).
         05  KOR-Length pic 9(4).
         05  KOR-Type   pic XXX.                    *> Not used currently
*>
*> The START condition cannot be compounded, and it must use a
*> Key of Reference within the record. (These are COBOL rules...)
*> The interface defines which key and the relation condition.
*>
*>
 01  DAL-Data.
         05  MOST-Relation   pic xxx.                  *> valid are >=, <=, <, >, =
         05  Most-Cursor-Set pic 9    value zero.
             88  Cursor-Not-Active    value zero.
             88  Cursor-Active        value 1.
*>
*>  Variables common to all DALs
*>  ****************************
*>
 01  subscripts comp-5.
     12 J                pic s9(4).
     12 K                pic s9(4).
     12 L                pic s9(4).
*>
 01  Old-File-Function   pic 9 value zero.
*>
 01  work-fields.
     03  ws-saved-fs-reply pic 99.
     03  ws-env-lines    pic 999              value zero.
     03  ws-lines        binary-char unsigned value zero.
     03  ws-22-lines     binary-char unsigned value zero.
     03  ws-23-lines     binary-char unsigned value zero.
     03  ws-98-lines     binary-char unsigned value zero.
     03  ws-99-lines     binary-char unsigned value zero.
     03  ws-Rec-Cnt      binary-long unsigned value zero.
*>
 01  Temp-work-fields.
     03  Temp-SQL-Err    pic x(5).     *> 29/07/16
     03  A               pic 99      comp     value zero.
*>
 01  Error-Messages.
     03  SM901          pic x(31) value "SM901 Note error and hit return".
*>
*> /MYSQL VAR\
*>       ACASDB
*>       TABLE=IRSDFLT-REC,HV
 COPY "mysql-variables.cpy".
*>
*>    Definitions for the IRSDFLT-REC Table
*>
       01  TP-IRSDFLT-REC                        USAGE POINTER.
       01  TD-IRSDFLT-REC.
           05  HV-DEF-REC-KEY                    PIC  9(03) COMP.
           05  HV-DEF-ACS                        PIC  9(05) COMP.
           05  HV-DEF-CODES                      PIC X(2).
           05  HV-DEF-VAT                        PIC X(1).
*> /MYSQL-END\

 Linkage Section.
*>**************
*>
*>**************************************************************
 copy "wsfnctn.cob".            *> File-Access
*>
*>**************************************************************
*>
 copy "Test-Data-Flags.cob".  *> set sw-testing to zero to stop logging.
*>
 copy "irswsdflt.cob".
*>
 screen section.
*>=============
*>
 01  Display-Message-1       foreground-color 2.
     03          value "WS-Where="                line 23 col  1.
     03  from WS-Where (1:J)           pic x(69)          col 10.
*>
 01  Display-Message-2       foreground-color 2.
     03      value "SM004 SQL Err No.="           line  4 col  1.    *> size 18 char
     03  using Ws-Mysql-Error-Number   pic x(4)           col 19.    *>      4       == 22
     03      value " Para="                               col 23.    *> size 6 char  == 28
     03  using WS-No-Paragraph         pic 9(3)           col 29.    *>      4       == 32
     03      value " SQL Cmd="                            col 32.    *>      9       == 41
     03  using Ws-Mysql-Command        pic x(199)         col 41.
     03      value "SQL Err Msg="                 line  7 col  1.    *>      12
     03  using Ws-Mysql-Error-Message  pic x(67)          col 13.
*>
*>
 PROCEDURE DIVISION   using File-Access
                            ACAS-DAL-Common-data
                            Default-Record.   *>  Ws record
*>**********************************************
 ba-ACAS-DAL-Process  section.
     accept   ws-env-lines from lines.
     if       ws-env-lines < 24
              move  24 to ws-env-lines ws-lines
     else
              move  ws-env-lines to ws-lines
     end-if
*> Force Esc, PgUp, PgDown, PrtSC to be detected
     set      ENVIRONMENT "COB_SCREEN_EXCEPTIONS" to "Y".
     set      ENVIRONMENT "COB_SCREEN_ESC" to "Y".
*>
 ba010-Initialise.
     move     zero   to We-Error                        *> as in irsub3
     move     spaces to WS-MYSQL-Error-Message
                        WS-MYSQL-Error-Number
                        WS-Log-Where
                        WS-File-Key
                        SQL-Msg
                        SQL-Err
                        SQL-State.
*>
*>   As this is for only one set of 32 rows we will skip this ...
*>
*>  Work out what is being requested and convert to action!!
*>
*>    This version uses the JC pre-Sql processor.
*>    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
*>
     evaluate File-Function
        when  1
              go to ba020-Process-Open
        when  2
              go to ba030-Process-Close
        when  3
              go to ba040-Process-Read-Next
        when  5
              go to ba070-Process-Write
        when  7
              go to ba090-Process-Rewrite
        when  other                     *> others are unused
              go to ba100-Bad-Function
     end-evaluate.
*>
 ba020-Process-Open.
*>
*> To get here the calling program MUST have read the Cobol system record first.
*>

 *>
 *>  Manual process MYSQL INIT
 *>    then perform MYSQL-1000-OPEN  THRU MYSQL-1090-EXIT
 *>
     string   DB-Schema      delimited by space
              X"00"          delimited by size
                               into WS-MYSQL-BASE-NAME
     end-string.
     string   DB-Host        delimited by space
              X"00"          delimited by size
                               into WS-MYSQL-HOST-NAME
     end-string.
     string   DB-UName       delimited by space
              X"00"          delimited by size
                               into WS-MYSQL-IMPLEMENTATION
     end-string.
     string   DB-UPass       delimited by space
              X"00"          delimited by size
                               into WS-MYSQL-PASSWORD
     end-string.
     string   DB-Port        delimited by space
              X"00"          delimited by size
                               into WS-MYSQL-PORT-NUMBER
     end-string.
     string   DB-Socket      delimited by space
              X"00"          delimited by size
                               into WS-MYSQL-SOCKET
     end-string.
     move     1 to ws-No-Paragraph.
     PERFORM  MYSQL-1000-OPEN  THRU MYSQL-1090-EXIT.
     if       fs-reply not = zero
              go   to ba999-end.
*>
*> /MYSQL INIT\
*>       BASE=ACASDB
*>       IMPLEMENTATION=dev-prog-001
*>       PASSWORD=mysqlpass
*> /MYSQL-END\
*>
     move    "OPEN IRS DEFAULT" to WS-File-Key
     move    zero   to Most-Cursor-Set
     go      to ba999-end.
*>
 ba030-Process-Close.
     if      Cursor-Active         *> this will not occur for the system rows/records
             perform ba998-Free.
*>
     move     2 to ws-No-Paragraph.
     move    "CLOSE IRS DEFAULT" to WS-File-Key.
*>  /MYSQL CLOSE\
*>
*>    Close the Database
*>
           PERFORM MYSQL-1980-CLOSE THRU MYSQL-1999-EXIT
*>  /MYSQL-END\
*>
     go      to ba999-end.
*>
 ba040-Process-Read-Next.    *> Getting the 32 rows for loading into one cobol rec
*>
*>   Here a SELECT first then fetch if no cursor active using lowest
*>    possible key of "000"
*>           [ DEF-REC-KEY ]
*>
     if       Cursor-Not-Active
              set      KOR-x1 to 1                *> 1 = Primary, 2 = Abrev, 3 = Desc (NOT Delete function as can be dups)
              move     KOR-offset (KOR-x1) to K
              move     KOR-length (KOR-x1) to L
*>
              move     spaces to WS-Where
              move     1   to J
              string   "`"                   delimited by size
                       KeyName (KOR-x1)      delimited by space
                       "`"                   delimited by size
                       " > "                 delimited by size
                       '"000"'                 delimited by size  *> changed from '000'
                       " ORDER BY "          delimited by size
                       "`"                   delimited by size
                       KeyName (KOR-x1)      delimited by space
                       "`"                   delimited by size
                       " ASC"               delimited by size
                                      into ws-Where
                                      with pointer J
              end-string
*>
              move     ws-Where (1:J)   to WS-Log-Where       *>  For test logging
              move     3 to ws-No-Paragraph
 *> DEF-REC-KEY > "000" ORDER BY DEF-REC-KEY ASC
*>               /MYSQL SELECT\
*>
*>    Select rows
*>
*>                    TABLE=IRSDFLT-REC
           INITIALIZE WS-MYSQL-COMMAND
           STRING "SELECT * FROM "
             "`IRSDFLT-REC`"
             " WHERE "
             ws-Where (1:J)
            ";"  X"00" INTO WS-MYSQL-COMMAND
           PERFORM MYSQL-1210-COMMAND THRU MYSQL-1219-EXIT
           PERFORM MYSQL-1220-STORE-RESULT THRU MYSQL-1239-EXIT
           MOVE WS-MYSQL-RESULT TO TP-IRSDFLT-REC
*>               /MYSQL-END\
           move    "> 0" to WS-File-Key
              if    Testing-2
                    display Display-Message-1 with erase eos
              end-if
*>
*>  It could be an empty table so test for it
*>
              if       WS-MYSQL-Count-Rows = zero
                       call  "MySQL_errno" using WS-MYSQL-Error-Number
                       call  "MySQL_sqlstate" using WS-MYSQL-SQLstate
                       move  WS-MYSQL-SqlState   to SQL-State
                       if    WS-MYSQL-Error-Number  not = "0  "    *> set non '0' if no rows ?
                             move WS-MYSQL-Error-Number to SQL-Err
                             call "MySQL_error" using WS-MYSQL-Error-Message
                             move WS-MYSQL-Error-Message to SQL-Msg
                       end-if       *> do not really need to do this meaning the above CALL
                       move  10 to fs-reply
                       move  10 to WE-Error
                       move  "No Data" to WS-File-Key
                       go to ba998-Free        *> can clear the dup code after testing
              end-if
              move     1 to Most-Cursor-Set    *> should test if select worked first??????
     end-if.
*>
*>  If here cursor is set, so get the 32 rows in one hit
*>
     move     spaces to WS-Log-Where.
     move     4 to ws-No-Paragraph.
     initialize Default-Record with filler.
     perform  varying A from 1 by 1
                    until A > 32
  *>                     or return-code not = zero
*>       /MYSQL FETCH\
*>
*>    Fetch next record
*>
*>              TABLE=IRSDFLT-REC
           MOVE TP-IRSDFLT-REC TO WS-MYSQL-RESULT
           CALL "MySQL_fetch_record" USING WS-MYSQL-RESULT
                    HV-DEF-REC-KEY
                    HV-DEF-ACS
                    HV-DEF-CODES
                    HV-DEF-VAT

*>       /MYSQL-END\
      end-call
*>
*> No it should not happen but ..
*>
              if       return-code = -1     *> no more data so free cursor & return
                  or   A > 32 or = zero
                       move 10 to fs-Reply WE-Error
                       move    "EOF" to WS-File-Key
                       move     zero to Most-Cursor-Set
                       if       Testing-1
                                perform Ca-Process-Logs
                       end-if
                       exit perform
              end-if
  *>  return-code = zero   and
              if       HV-DEF-REC-KEY = zero or > 32
                       move    "EOF3" to WS-File-Key
                       move    HV-DEF-REC-KEY to WE-Error
                       move     zero to Most-Cursor-Set
                       if       Testing-1
                                perform Ca-Process-Logs
                       end-if
                       exit perform
              end-if
*>
              if       WS-MYSQL-Count-Rows = zero
                       call  "MySQL_errno" using WS-MYSQL-Error-Number
                       call  "MySQL_sqlstate" using WS-MYSQL-SQLstate
                       move  WS-MYSQL-SqlState   to SQL-State
                       if    WS-MYSQL-Error-Number  not = "0  "
                             call "MySQL_error" using WS-MYSQL-Error-Message
                             move WS-MYSQL-Error-Number  to SQL-Err   *> Not really needed but help for testing
                             move WS-MYSQL-Error-Message to SQL-Msg   *>  ditto
                             move 10 to fs-reply                     *> EOF equivilent !!
                             move 10 to WE-Error
                             move    "EOF2" to WS-File-Key
                       end-if
                       move     zero to Most-Cursor-Set
                       if       Testing-1
                                perform Ca-Process-Logs
                       end-if
                       exit perform
              end-if
              move     HV-DEF-ACS   to Def-Acs (HV-DEF-REC-KEY)    *> KEY = table position
              move     HV-DEF-VAT   to Def-Vat (HV-DEF-REC-KEY)
              move     HV-DEF-CODES to Def-Codes (HV-DEF-REC-KEY)
              move     HV-DEF-REC-KEY to WS-Key
              move     WS-Key         to WS-File-Key
              if       Testing-1    *> do for each row
                       perform Ca-Process-Logs
              end-if
     end-perform                                                      *> Have all of the data
*>
 *>    move     HV-DEF-REC-KEY to WS-File-Key.
 *>    move     zero to fs-reply WE-Error.
     go       to ba999-exit.                             *> have written logs if testing
*>
 ba070-Process-Write.
     move     zero to WS-Mysql-Time-Step
                      WS-SQL-Retry.
*>
     move     zero to FS-Reply WE-Error
     move     spaces to SQL-Msg
     move     zero to SQL-Err
     move     10 to ws-No-Paragraph.
     initialise TD-IRSDFLT-REC.
     perform  varying A from 1 by 1 until A > 32
 *>             if       Def-Acs (A)   = zero    *> check for empty occurs
 *>                 and  Def-Codes (A) = spaces
 *>                 and  Def-Vat (A)   = space
 *>                      exit perform cycle
 *>             end-if
              if       Def-Acs (A)  numeric
                       move     Def-Acs (A) to HV-DEF-ACS
              else
                       move     zeros       to HV-DEF-ACS
              end-if
              move     A to WS-Key
              move     WS-Key to WS-File-Key              *> will contain the record with problems
              move     WS-Key        to HV-DEF-REC-KEY
              move     Def-Vat   (A) to HV-DEF-VAT
              move     Def-Codes (A) to HV-DEF-CODES
              perform  bb200-Insert
              if       WS-MYSQL-COUNT-ROWS not = 1
                       call  "MySQL_errno" using WS-MYSQL-Error-Number
                       call  "MySQL_sqlstate" using WS-MYSQL-SQLstate
                       move  WS-MYSQL-SqlState   to SQL-State
                       if    WS-MYSQL-Error-Number  not = "0  "   *> 14/12/16
                             call "MySQL_error" using Ws-Mysql-Error-Message
                             move WS-MYSQL-Error-Number  to SQL-Err
                             move WS-MYSQL-Error-Message to SQL-Msg
                             if    SQL-Err (1:4) = "1062"
                                              or = "1022"   *> Dup key (rec already present)
                                    or Sql-State = "23000"  *> Dup key (rec already present)
                                   move 22 to fs-reply
                             else
                                   move 99 to fs-reply                  *> this may need changing for val in WE-Error!!
                             end-if
                       end-if
              end-if
              if       Testing-1
                       perform Ca-Process-Logs
              end-if
              if       fs-reply not = zero
                       move fs-reply to ws-saved-fs-reply
                       move zero to fs-reply                   *> for next loop iteration
              end-if
     end-perform
     if       ws-saved-fs-reply not = zero                     *> restore last error
              move ws-saved-fs-reply to fs-reply
     end-if
     go       to ba999-Exit.
*>
 ba090-Process-Rewrite.
*>
     move     zero to WS-Mysql-Time-Step
                      WS-SQL-Retry.
     move     17 to ws-No-Paragraph.
     perform  varying A from 1 by 1 until A > 32
 *>             if       Def-Acs (A)   = zero    *> check for empty occurs
 *>                 and  Def-Codes (A) = spaces
 *>                 and  Def-Vat (A)   = space
 *>                      exit perform cycle
 *>             end-if
              if       Def-Acs   (A) numeric
                       move     Def-Acs   (A) to HV-DEF-ACS
              else
                       move     zeros         to HV-DEF-ACS
              end-if
              move     A       to WS-Key
              move     WS-Key  to WS-File-Key              *> will contain the record with problems
                                  HV-DEF-REC-KEY
              move     Def-Vat   (A) to HV-DEF-VAT
              move     Def-Codes (A) to HV-DEF-CODES
*>
              set      KOR-x1 to 1            *> 1 = Primary, 2 = Abrev, 3 = Desc (NOT Delete function as can be dups)
              move     KOR-offset (KOR-x1) to K
              move     KOR-length (KOR-x1) to L
*>
              move     spaces to WS-Where
              move     1   to J
              string   "`"                   delimited by size
                       KeyName (KOR-x1)      delimited by space
                       "`"                   delimited by size
                       '="'                  delimited by size
*>                       Default-Record (K:L)       delimited by size
*>                       HV-DEF-REC-KEY        delimited by size                   *>  use the real key for row
                       WS-KEY                delimited by size  *> use the real key for row
                       '"'                   delimited by size
                               into WS-Where
                                 with pointer J
              end-string
              move     WS-Where (1:J)   to WS-Log-Where    *>  For test logging
              perform  bb300-Update
*>
              if       Testing-2
                       display Display-Message-1 with erase eos
              end-if
*>
              if       WS-MYSQL-COUNT-ROWS not = 1
                       call "MySQL_errno" using WS-MYSQL-Error-Number
                       call  "MySQL_sqlstate" using WS-MYSQL-SQLstate
                       move  WS-MYSQL-SqlState   to SQL-State
                       if    WS-MYSQL-Error-Number  not = "0   "
                             call "MySQL_error" using Ws-Mysql-Error-Message
                             move WS-MYSQL-Error-Number  to SQL-Err
                             move WS-MYSQL-Error-Message to SQL-Msg
                             move 99 to fs-reply                  *> this may need changing for val in WE-Error!!
                             move 994 to WE-Error
                       end-if
              end-if
              if       Testing-1
                       perform Ca-Process-Logs
              end-if
     end-perform
*>
     move     zero   to FS-Reply WE-Error.
     move     zero   to SQL-Err.
     move     spaces to SQL-Msg.
     go       to ba999-exit.
*>
 ba100-Bad-Function.
*>
*> Houston; We have a problem
*>
     move     990 to WE-Error.
     move     99 to Fs-Reply.
     go       to ba999-end.
*>
*> /MYSQL PRO\
 COPY "mysql-procedures.cpy".
*> /MYSQL-END\
*>
 ba998-Free.
     move     20 to ws-No-Paragraph.
*>      /MYSQL FREE\
*>
*>    Free result array
*>
*>             TABLE=IRSDFLT-REC
           MOVE TP-IRSDFLT-REC TO WS-MYSQL-RESULT
           CALL "MySQL_free_result" USING WS-MYSQL-RESULT end-call
*>      /MYSQL-END\
     move     zero to Most-Cursor-Set.
*>
 ba999-end.
*>
     if       Testing-1
              perform Ca-Process-Logs
     end-if.
*>
 ba999-exit.
     exit program.
*>
 bb200-Insert Section.
*>*******************
*>
*>  /MYSQL INSERT\
*>
*>    Insert a row
*>
*>         TABLE=IRSDFLT-REC
           INITIALIZE WS-MYSQL-COMMAND
           MOVE 1 TO WS-MYSQL-I
           STRING 'INSERT INTO '
                    '`IRSDFLT-REC` SET '
              INTO WS-MYSQL-COMMAND
              WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`DEF-REC-KEY`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-DEF-REC-KEY
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(18:03))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`DEF-ACS`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-DEF-ACS
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(16:05))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`DEF-CODES`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           STRING FUNCTION TRIM (HV-DEF-CODES,TRAILING)
                  '"'
                   INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`DEF-VAT`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           STRING FUNCTION TRIM (HV-DEF-VAT,TRAILING)
                  '"'
                   INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           STRING ";" INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING X"00" INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
       PERFORM MYSQL-1210-COMMAND THRU MYSQL-1219-EXIT
*>  /MYSQL-END\
       .     *> period here
*>
 bb200-Exit.
     exit section.
*>
 bb300-Update Section.
*>*******************
*>
*>  /MYSQL UPDATE\
*>
*>    Update a row
*>
*>      TABLE=IRSDFLT-REC
           INITIALIZE WS-MYSQL-COMMAND
           MOVE 1 TO WS-MYSQL-I
           STRING 'UPDATE '
                    '`IRSDFLT-REC` SET '
              INTO WS-MYSQL-COMMAND
              WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`DEF-REC-KEY`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-DEF-REC-KEY
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(18:03))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`DEF-ACS`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-DEF-ACS
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(16:05))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`DEF-CODES`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           STRING FUNCTION TRIM (HV-DEF-CODES,TRAILING)
                  '"'
                   INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`DEF-VAT`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           STRING FUNCTION TRIM (HV-DEF-VAT,TRAILING)
                  '"'
                   INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           STRING " WHERE "
              INTO WS-MYSQL-COMMAND
              WITH POINTER WS-MYSQL-I end-string
           STRING FUNCTION TRIM (WS-Where (1:J))
              INTO WS-MYSQL-COMMAND
              WITH POINTER WS-MYSQL-I end-string
           STRING ";" X"00" INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           PERFORM MYSQL-1210-COMMAND THRU MYSQL-1219-EXIT
*>  /MYSQL-END\
       .     *> period here
*>
 bb300-Exit.
     exit section.
*>
 Ca-Process-Logs.
*>**************
*>
     call     "fhlogger" using File-Access
                               ACAS-DAL-Common-data.
*>
 ca-Exit.     exit.
*>
 end program irsdfltMT.
