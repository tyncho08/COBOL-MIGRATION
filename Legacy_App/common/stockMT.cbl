       >>source free
*>***********************************************
*>                                              *
*>                    Stock                     *
*>            File/Table RDB Handler            *
*>                                              *
*>***********************************************
*>
 identification division.
 Program-Id.            stockMT.
*>**
*> Author.              Vincent B Coen, FBCS, FIDM, FIDPM, CPL
*>                      for Applewood Computers.
*>**
*> Security.            Copyright (C) 2016 and later, Vincent Bryan Coen.
*>                      Distributed under the GNU General Public License
*>                      v2.0. Only. See the file COPYING for details.
*>**
*> Remarks.             Stock File RDB Handler using amended JC preSQL.
*>                      **********************************************
*>
*>                      This version uses an amended JC pre-Sql processor.
*>                      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
*>
*>                      This and associated modules relate to ACAS versions v3.02 and later.
*>
*>                      This modules does all file handling using RDB and is called
*>                      from the ACAS
*>                      stock file handler acas011
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
*>                      acas011 - Stock Master Cobol Handler.
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
*> 30/07/16 vbc - .04 Change Open and Close to also show table name in operation.
*> 31/07/16 vbc - .05 Moved mv spaces/zero in Write to before insert.
*>                    Remove fhlogger file close in Process-Close.
*>                    Forgot error 989 so commented.
*> 01/03/18 vbc - .11 Renamed error messages to SM901, SM004 as needed.
*> 06/08/23 vbc - .12 For start using not > (fn = 9 )
*> 30/03/24 vbc - .13 Added Stock-Arrival-Date within filler next the table - done.
*> 16/04/24 vbc       Copyright notice update superseding all previous notices.
*> 27/01/25 vbc - .14 Add in support for 4th key - Location (with dups) to support
*> 29/01/25 vbc       changes in st030 - also applies to acas011.
*>**
*>  Module USAGE
*>**************
*>
*>    On Entry:         Set up Linkage areas -
*>    ********              WS-Stock-Record
*>                                          = Contents of data record to be written/read
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
*> copy "envdiv.cob".
*>
 input-output section.
 Data Division.
 Working-Storage Section.
 77  prog-name           pic x(17)    value "stockMT (3.02.14)".
*>
*> JC WS requirements here
*>
 77  ws-Where            pic x(512).
*>
*>  Used within presql generated code
*>
 01  WS-Reply            pic x           value space.
 01  WS-MYSQL-I          PIC S9(4) COMP.
 01  WS-MYSQL-EDIT       PIC -Z(18)9.9(9).
 *>
 *> TESING data
 *>
 01  ws-temp-ed          pic 9(10).
*>
*> The communication area for the MySQL database changed for free/mysql
*>
*>  jc preSQL MySQL stuff ends
*>
*> Metadata on primary and alternate keys...  from Prima DAL for STOCK-REC
*>
 01  Table-Of-Keynames.
*>> this is from the 2012 version so including it
     03  filler          pic x(30) value "STOCK-KEY                     ".
     03  filler          pic x(8)  value "00010013". *> offset/length
     03  filler          pic xxx   value "STR".      *> key is string
*>
*> Experiemental  vbc 02/04/12  ADDed in missing keys
     03  filler          pic x(30) value "STOCK-ABREV-KEY               ".
     03  filler          pic x(8)  value "00140007". *> offset/length
     03  filler          pic xxx   value "STR".      *> key is string
*>
*> with Duplicates so do NOT delete on this key
     03  filler          pic x(30) value "STOCK-DESC                    ".
     03  filler          pic x(8)  value "00420032". *> offset/length
     03  filler          pic xxx   value "STR".      *> key is string
*>
*> with duplicates so do not delete on this key  New 27/01/25
     03  filler          pic x(30) value "STOCK-LOCATION                ".
     03  filler          pic x(8)  value "00870010". *> offset/length
     03  filler          pic xxx   value "STR".      *> key is string


 01  filler redefines table-of-keynames.
     03  KeyOfReference occurs 4    indexed by KOR-x1.
         05 KeyName      pic x(30).
         05 KOR-Offset   pic 9(4).
         05 KOR-Length   pic 9(4).
         05 KOR-Type     pic XXX.                    *> Not used currently
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
 01  subscripts usage comp-5.
     12 J                pic s9(4).
     12 K                pic s9(4).
     12 L                pic s9(4).
*>
 01  Old-File-Function   pic 9 value zero.
*>
 01  work-fields.
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
*>
 01  Error-Messages.
     03  SM901           pic x(31) value "SM901 Note error and hit return".
*>
*> /MYSQL VAR\
*>       ACASDB
*>       TABLE=STOCK-REC,HV
 COPY "mysql-variables.cpy".
*>
*>    Definitions for the STOCK-REC Table
*>
       01  TP-STOCK-REC                          USAGE POINTER.
       01  TD-STOCK-REC.
           05  HV-STOCK-KEY                      PIC X(13).
           05  HV-STOCK-ABREV-KEY                PIC X(7).
           05  HV-STOCK-SUPPLIER-P1              PIC X(7).
           05  HV-STOCK-SUPPLIER-P2              PIC X(7).
           05  HV-STOCK-SUPPLIER-P3              PIC X(7).
           05  HV-STOCK-DESC                     PIC X(32).
           05  HV-STOCK-CONSTRUCT-ITEM           PIC X(13).
           05  HV-STOCK-LOCATION                 PIC X(10).
           05  HV-STOCK-PA-CODE                  PIC X(3).
           05  HV-STOCK-SA-CODE                  PIC X(3).
           05  HV-STOCK-SERVICES-FLAG            PIC X(1).
           05  HV-STOCK-LAST-ACTUAL-COST         PIC  9(07)V9(02) COMP.
           05  HV-STOCK-CONSTRUCT-BUNDLE         PIC  9(10) COMP.
           05  HV-STOCK-UNDER-CONSTRUCTION       PIC  9(10) COMP.
           05  HV-STOCK-WORK-IN-PROGRESS         PIC  9(10) COMP.
           05  HV-STOCK-REORDER-PNT              PIC  9(10) COMP.
           05  HV-STOCK-STD-REORDER              PIC  9(10) COMP.
           05  HV-STOCK-BACK-ORDERED             PIC  9(10) COMP.
           05  HV-STOCK-ON-ORDER                 PIC  9(10) COMP.
           05  HV-STOCK-HELD                     PIC  9(10) COMP.
           05  HV-STOCK-PRE-SALES                PIC  9(10) COMP.
           05  HV-STOCK-RETAIL                   PIC  9(07)V9(02) COMP.
           05  HV-STOCK-COST                     PIC  9(07)V9(04) COMP.
           05  HV-STOCK-VALUE                    PIC  9(09)V9(02) COMP.
           05  HV-STOCK-ARRIVAL-DATE             PIC  9(10) COMP.
           05  HV-STOCK-ORDER-DUE                PIC  9(10) COMP.
           05  HV-STOCK-ORDER-DAT                PIC  9(10) COMP.
           05  HV-STOCK-ADDS                     PIC  9(10) COMP.
           05  HV-STOCK-DEDUCTS                  PIC  9(10) COMP.
           05  HV-STOCK-WIP-ADDS                 PIC  9(10) COMP.
           05  HV-STOCK-WIP-DEDS                 PIC  9(10) COMP.
           05  HV-STOCK-TD-ADDS-01               PIC  9(10) COMP.
           05  HV-STOCK-TD-ADDS-02               PIC  9(10) COMP.
           05  HV-STOCK-TD-ADDS-03               PIC  9(10) COMP.
           05  HV-STOCK-TD-ADDS-04               PIC  9(10) COMP.
           05  HV-STOCK-TD-ADDS-05               PIC  9(10) COMP.
           05  HV-STOCK-TD-ADDS-06               PIC  9(10) COMP.
           05  HV-STOCK-TD-ADDS-07               PIC  9(10) COMP.
           05  HV-STOCK-TD-ADDS-08               PIC  9(10) COMP.
           05  HV-STOCK-TD-ADDS-09               PIC  9(10) COMP.
           05  HV-STOCK-TD-ADDS-10               PIC  9(10) COMP.
           05  HV-STOCK-TD-ADDS-11               PIC  9(10) COMP.
           05  HV-STOCK-TD-ADDS-12               PIC  9(10) COMP.
           05  HV-STOCK-TD-DEDS-01               PIC  9(10) COMP.
           05  HV-STOCK-TD-DEDS-02               PIC  9(10) COMP.
           05  HV-STOCK-TD-DEDS-03               PIC  9(10) COMP.
           05  HV-STOCK-TD-DEDS-04               PIC  9(10) COMP.
           05  HV-STOCK-TD-DEDS-05               PIC  9(10) COMP.
           05  HV-STOCK-TD-DEDS-06               PIC  9(10) COMP.
           05  HV-STOCK-TD-DEDS-07               PIC  9(10) COMP.
           05  HV-STOCK-TD-DEDS-08               PIC  9(10) COMP.
           05  HV-STOCK-TD-DEDS-09               PIC  9(10) COMP.
           05  HV-STOCK-TD-DEDS-10               PIC  9(10) COMP.
           05  HV-STOCK-TD-DEDS-11               PIC  9(10) COMP.
           05  HV-STOCK-TD-DEDS-12               PIC  9(10) COMP.
           05  HV-STOCK-TD-WIP-ADDS-01           PIC  9(10) COMP.
           05  HV-STOCK-TD-WIP-ADDS-02           PIC  9(10) COMP.
           05  HV-STOCK-TD-WIP-ADDS-03           PIC  9(10) COMP.
           05  HV-STOCK-TD-WIP-ADDS-04           PIC  9(10) COMP.
           05  HV-STOCK-TD-WIP-ADDS-05           PIC  9(10) COMP.
           05  HV-STOCK-TD-WIP-ADDS-06           PIC  9(10) COMP.
           05  HV-STOCK-TD-WIP-ADDS-07           PIC  9(10) COMP.
           05  HV-STOCK-TD-WIP-ADDS-08           PIC  9(10) COMP.
           05  HV-STOCK-TD-WIP-ADDS-09           PIC  9(10) COMP.
           05  HV-STOCK-TD-WIP-ADDS-10           PIC  9(10) COMP.
           05  HV-STOCK-TD-WIP-ADDS-11           PIC  9(10) COMP.
           05  HV-STOCK-TD-WIP-ADDS-12           PIC  9(10) COMP.
           05  HV-STOCK-TD-WIP-DEDS-01           PIC  9(10) COMP.
           05  HV-STOCK-TD-WIP-DEDS-02           PIC  9(10) COMP.
           05  HV-STOCK-TD-WIP-DEDS-03           PIC  9(10) COMP.
           05  HV-STOCK-TD-WIP-DEDS-04           PIC  9(10) COMP.
           05  HV-STOCK-TD-WIP-DEDS-05           PIC  9(10) COMP.
           05  HV-STOCK-TD-WIP-DEDS-06           PIC  9(10) COMP.
           05  HV-STOCK-TD-WIP-DEDS-07           PIC  9(10) COMP.
           05  HV-STOCK-TD-WIP-DEDS-08           PIC  9(10) COMP.
           05  HV-STOCK-TD-WIP-DEDS-09           PIC  9(10) COMP.
           05  HV-STOCK-TD-WIP-DEDS-10           PIC  9(10) COMP.
           05  HV-STOCK-TD-WIP-DEDS-11           PIC  9(10) COMP.
           05  HV-STOCK-TD-WIP-DEDS-12           PIC  9(10) COMP.
*> /MYSQL-END\

 Linkage Section.
*>**************
*>
*>**********************************************************************
 copy "wsfnctn.cob".                         *> File-Access
*>
*>**********************************************************************
*>
 copy "Test-Data-Flags.cob".  *> set sw-testing to zero to stop logging.
*>
*>  Record definition for Cobol file here:
*> Generated by MOSTGEN from the COPY Book... The record buffer
*>  still maps to wsstock.cob 07/02/24
*>  added stock-arrival-date (within filler) from filler  30/03/24
*>
 01  Stock-Rec.
     03  Stock-Key                pic x(13).
     03  Stock-Abrev-Key          pic x(7).
     03  Stock-Suppliers-Group.
         05  Stock-Supplier-P1    pic x(7).                          *> Primary   Supplier
         05  Stock-Supplier-P2    pic x(7).                          *> Secondary Supplier
         05  Stock-Supplier-P3    pic x(7).                          *> Back Up   Supplier
     03  Stock-Desc               pic x(32).
     03  Stock-Construct-Item     pic x(13).
     03  Stock-Location           pic x(10).
     03  Stock-PA-Code            PIC XXX.
     03  Stock-SA-Code            PIC XXX.
     03  Stock-Services-Flag      pic x.                  *>    flag for services not product (Y/N)
     03  Stock-Last-Actual-Cost   pic 9(7)v99     comp-3.
     03  Stock-Arrival-Date       pic 9(8)        comp.    *> New 30/03/24 within filler.
     03  filler                   pic x(4).
     03  Stock-Construct-Bundle   pic s9(6)       comp.
     03  Stock-Under-Construction pic s9(6)       comp.
     03  Stock-Work-in-Progress   pic s9(6)       comp.
     03  Stock-ReOrder-Pnt        pic s9(6)       comp.
     03  Stock-Std-ReOrder        pic s9(6)       comp.
     03  Stock-Back-Ordered       pic s9(6)       comp.
     03  Stock-On-Order           pic s9(6)       comp.
     03  Stock-Held               pic s9(6)       comp.
     03  Stock-Pre-Sales          pic s9(6)       comp.
     03  Stock-Retail             pic 9(7)v99     comp-3.
     03  Stock-Cost               pic 9(7)v9999   comp-3. *>    Based on last Order Only
     03  Stock-Value              pic 9(9)v99     comp-3.
     03  Stock-Order-Due          pic 9(8)        comp.
     03  Stock-Order-Dat          pic 9(8)        comp.
     03  Stock-Mthly-Running-Tots. *> name was changed because it is too long when prefixed
         05  Stock-Adds           pic 9(8)        comp.
         05  Stock-Deducts        pic 9(8)        comp.
         05  Stock-Wip-Adds       pic 9(8)        comp.
         05  Stock-Wip-Deds       pic 9(8)        comp.
     03  Stock-History.
         05  Stock-TD-Adds-01     pic 9(8) usage comp.
         05  Stock-TD-Adds-02     pic 9(8) usage comp.
         05  Stock-TD-Adds-03     pic 9(8) usage comp.
         05  Stock-TD-Adds-04     pic 9(8) usage comp.
         05  Stock-TD-Adds-05     pic 9(8) usage comp.
         05  Stock-TD-Adds-06     pic 9(8) usage comp.
         05  Stock-TD-Adds-07     pic 9(8) usage comp.
         05  Stock-TD-Adds-08     pic 9(8) usage comp.
         05  Stock-TD-Adds-09     pic 9(8) usage comp.
         05  Stock-TD-Adds-10     pic 9(8) usage comp.
         05  Stock-TD-Adds-11     pic 9(8) usage comp.
         05  Stock-TD-Adds-12     pic 9(8) usage comp.
         05  Stock-TD-Deds-01     pic 9(8) usage comp.
         05  Stock-TD-Deds-02     pic 9(8) usage comp.
         05  Stock-TD-Deds-03     pic 9(8) usage comp.
         05  Stock-TD-Deds-04     pic 9(8) usage comp.
         05  Stock-TD-Deds-05     pic 9(8) usage comp.
         05  Stock-TD-Deds-06     pic 9(8) usage comp.
         05  Stock-TD-Deds-07     pic 9(8) usage comp.
         05  Stock-TD-Deds-08     pic 9(8) usage comp.
         05  Stock-TD-Deds-09     pic 9(8) usage comp.
         05  Stock-TD-Deds-10     pic 9(8) usage comp.
         05  Stock-TD-Deds-11     pic 9(8) usage comp.
         05  Stock-TD-Deds-12     pic 9(8) usage comp.
         05  Stock-TD-Wip-Adds-01 pic 9(8) usage comp.
         05  Stock-TD-Wip-Adds-02 pic 9(8) usage comp.
         05  Stock-TD-Wip-Adds-03 pic 9(8) usage comp.
         05  Stock-TD-Wip-Adds-04 pic 9(8) usage comp.
         05  Stock-TD-Wip-Adds-05 pic 9(8) usage comp.
         05  Stock-TD-Wip-Adds-06 pic 9(8) usage comp.
         05  Stock-TD-Wip-Adds-07 pic 9(8) usage comp.
         05  Stock-TD-Wip-Adds-08 pic 9(8) usage comp.
         05  Stock-TD-Wip-Adds-09 pic 9(8) usage comp.
         05  Stock-TD-Wip-Adds-10 pic 9(8) usage comp.
         05  Stock-TD-Wip-Adds-11 pic 9(8) usage comp.
         05  Stock-TD-Wip-Adds-12 pic 9(8) usage comp.
         05  Stock-TD-Wip-Deds-01 pic 9(8) usage comp.
         05  Stock-TD-Wip-Deds-02 pic 9(8) usage comp.
         05  Stock-TD-Wip-Deds-03 pic 9(8) usage comp.
         05  Stock-TD-Wip-Deds-04 pic 9(8) usage comp.
         05  Stock-TD-Wip-Deds-05 pic 9(8) usage comp.
         05  Stock-TD-Wip-Deds-06 pic 9(8) usage comp.
         05  Stock-TD-Wip-Deds-07 pic 9(8) usage comp.
         05  Stock-TD-Wip-Deds-08 pic 9(8) usage comp.
         05  Stock-TD-Wip-Deds-09 pic 9(8) usage comp.
         05  Stock-TD-Wip-Deds-10 pic 9(8) usage comp.
         05  Stock-TD-Wip-Deds-11 pic 9(8) usage comp.
         05  Stock-TD-Wip-Deds-12 pic 9(8) usage comp.
     03  filler                   pic x(15).               *> 400  expansion
*>
 screen section.
*>=============
*>
 01  Display-Message-1       foreground-color 2.
     03          value "WS-Where="                line 23 col  1.
     03  from WS-Where (1:J)           pic x(69)          col 10.
*>
 01  Display-Message-2       foreground-color 2.
     03      value "SM004 SQL Err No.="            line 4 col  1.    *> size 18 char
     03  using Ws-Mysql-Error-Number   pic x(4)           col 19.    *>      4       == 22
     03      value " Para="                               col 23.    *> size 6 char  == 28
     03  using WS-No-Paragraph         pic 9(3)           col 29.    *>      4       == 32
     03      value " SQL Cmd="                            col 32.    *>      9       == 41
     03  using Ws-Mysql-Command        pic x(199)         col 41.
     03      value "SQL Err Msg="                  line 7 col  1.    *>      12
     03  using Ws-Mysql-Error-Message  pic x(67)          col 13.
*>
*>
 PROCEDURE DIVISION   using File-Access
                            ACAS-DAL-Common-data
                            Stock-Rec.   *>  Ws record
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
*>
*>     move     zero   to We-Error
*>                        Fs-Reply.
*>
     move     spaces to WS-MYSQL-Error-Message
                        WS-MYSQL-Error-Number
                        WS-Log-Where
                        WS-File-Key
                        SQL-Msg
                        SQL-Err
                        SQL-State.
*>
*>   Now Test for valid key for start, read-indexed and delete
*>
     evaluate File-Function
              when  4   *> fn-read-indexed
              when  9   *> fn-start
                if     File-Key-No < 1 or > 4     *> chg 29/01/25
                       move 998 to WE-Error       *> file seeks key type out of range        998
                       move 99 to fs-reply
                       go   to ba999-end
                end-if
              when     8  *> fn-delete
                if     File-Key-No < 1 or > 2  *> NOT for Location key as dups allowed
                       move 996 to WE-Error       *> file seeks key type out of range        996
                       move 99 to fs-reply
                       go   to ba999-end
                end-if
     end-evaluate.
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
        when  4
              go to ba050-Process-Read-Indexed
        when  5
              go to ba070-Process-Write
        when  7
              go to ba090-Process-Rewrite
        when  8
              go to ba080-Process-Delete
        when  9
              go to ba060-Process-Start
        when  other                          *> 6 is spare / unused
              go to ba100-Bad-Function
     end-evaluate.
*>
 ba020-Process-Open.
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
*> *> /MYSQL INIT\
*>       BASE=ACASDB
*>       IMPLEMENTATION=dev-prog-001
*>       PASSWORD=mysqlpass
*> *> /MYSQL-END\
*>
     move     "OPEN STOCK" to WS-File-Key
     move     zero   to Most-Cursor-Set
     go       to ba999-end.
*>
 ba030-Process-Close.
     if       Cursor-Active
              perform ba998-Free.
*>
     move     2 to ws-No-Paragraph.
     move    "CLOSE STOCK" to WS-File-Key.
*>  /MYSQL CLOSE\
*>
*>    Close the Database
*>
           PERFORM MYSQL-1980-CLOSE THRU MYSQL-1999-EXIT
*>  /MYSQL-END\
*>
     go      to ba999-end.
*>
 ba040-Process-Read-Next.
*>
*>   Here a SELECT first then fetch if no cursor active using lowest
*>    possible key of "0000000000000"
*>           [ STOCK-KEY ]
*>
     if       Cursor-Not-Active
              set      KOR-x1 to 1                *> 1 = Primary, 2 = Abrev, 3 = Desc (NOT Delete function as can be dups)
                                                  *> 4 = Location                     (NOT Delete function as can be dups) 27/01/25
              move     KOR-offset (KOR-x1) to K
              move     KOR-length (KOR-x1) to L
*>
              move     spaces to WS-Where
              move     1   to J
              string   "`"                   delimited by size
                       KeyName (KOR-x1)      delimited by space
                       "`"                   delimited by size
                       " > "                 delimited by size
                       '"0000000000000"'       delimited by size
                       ' ORDER BY '          delimited by size
                       "`"                   delimited by size
                       keyname (KOR-x1)      delimited by space
                       "`"                   delimited by size
                         ' ASC'              delimited by size
                                      into ws-Where
                                      with pointer J
              end-string
*>
              move     ws-Where (1:J)   to WS-Log-Where       *>  For test logging
              move     3 to ws-No-Paragraph
*>               /MYSQL SELECT\
*>
*>    Select rows
*>
*>                    TABLE=STOCK-REC
           INITIALIZE WS-MYSQL-COMMAND
           STRING "SELECT * FROM "
             "`STOCK-REC`"
             " WHERE "
             ws-Where (1:J)      *> STOCK-KEY > "0000000000000" ORDER BY STOCK-REC ASC
            ";"  X"00" INTO WS-MYSQL-COMMAND
           PERFORM MYSQL-1210-COMMAND THRU MYSQL-1219-EXIT
           PERFORM MYSQL-1220-STORE-RESULT THRU MYSQL-1239-EXIT
           MOVE WS-MYSQL-RESULT TO TP-STOCK-REC
*>               /MYSQL-END\
              move     "0000000000000" to WS-File-Key
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
                       if    WS-MYSQL-Error-Number  not = "0  "
                             move WS-MYSQL-Error-Number to SQL-Err
                             call "MySQL_error" using WS-MYSQL-Error-Message
                             move WS-MYSQL-Error-Message to SQL-Msg
                       end-if
                       move  10 to fs-reply
                       move  10 to WE-Error
                       move  "No Data" to WS-File-Key
                       go to ba999-End       *> can clear the dup code after testing
              end-if
              set Cursor-Active to true
              move     WS-MYSQL-Count-Rows to WS-Temp-Ed
              string   "> 0 got cnt=" delimited by size
                       WS-Temp-ED delimited by size
                       " recs"
                        into WS-File-Key
              end-string
              perform ba999-End                         *> log it
     end-if.
*>
*>  If here cursor is set, so get the next row
*>
     move     spaces to WS-Log-Where.
     move     4 to ws-No-Paragraph.
*>      /MYSQL FETCH\
*>
*>    Fetch next record
*>
*>             TABLE=STOCK-REC
           MOVE TP-STOCK-REC TO WS-MYSQL-RESULT
           CALL "MySQL_fetch_record" USING WS-MYSQL-RESULT
                    HV-STOCK-KEY
                    HV-STOCK-ABREV-KEY
                    HV-STOCK-SUPPLIER-P1
                    HV-STOCK-SUPPLIER-P2
                    HV-STOCK-SUPPLIER-P3
                    HV-STOCK-DESC
                    HV-STOCK-CONSTRUCT-ITEM
                    HV-STOCK-LOCATION
                    HV-STOCK-PA-CODE
                    HV-STOCK-SA-CODE
                    HV-STOCK-SERVICES-FLAG
                    HV-STOCK-LAST-ACTUAL-COST
                    HV-STOCK-CONSTRUCT-BUNDLE
                    HV-STOCK-UNDER-CONSTRUCTION
                    HV-STOCK-WORK-IN-PROGRESS
                    HV-STOCK-REORDER-PNT
                    HV-STOCK-STD-REORDER
                    HV-STOCK-BACK-ORDERED
                    HV-STOCK-ON-ORDER
                    HV-STOCK-HELD
                    HV-STOCK-PRE-SALES
                    HV-STOCK-RETAIL
                    HV-STOCK-COST
                    HV-STOCK-VALUE
                    HV-STOCK-ARRIVAL-DATE
                    HV-STOCK-ORDER-DUE
                    HV-STOCK-ORDER-DAT
                    HV-STOCK-ADDS
                    HV-STOCK-DEDUCTS
                    HV-STOCK-WIP-ADDS
                    HV-STOCK-WIP-DEDS
                    HV-STOCK-TD-ADDS-01
                    HV-STOCK-TD-ADDS-02
                    HV-STOCK-TD-ADDS-03
                    HV-STOCK-TD-ADDS-04
                    HV-STOCK-TD-ADDS-05
                    HV-STOCK-TD-ADDS-06
                    HV-STOCK-TD-ADDS-07
                    HV-STOCK-TD-ADDS-08
                    HV-STOCK-TD-ADDS-09
                    HV-STOCK-TD-ADDS-10
                    HV-STOCK-TD-ADDS-11
                    HV-STOCK-TD-ADDS-12
                    HV-STOCK-TD-DEDS-01
                    HV-STOCK-TD-DEDS-02
                    HV-STOCK-TD-DEDS-03
                    HV-STOCK-TD-DEDS-04
                    HV-STOCK-TD-DEDS-05
                    HV-STOCK-TD-DEDS-06
                    HV-STOCK-TD-DEDS-07
                    HV-STOCK-TD-DEDS-08
                    HV-STOCK-TD-DEDS-09
                    HV-STOCK-TD-DEDS-10
                    HV-STOCK-TD-DEDS-11
                    HV-STOCK-TD-DEDS-12
                    HV-STOCK-TD-WIP-ADDS-01
                    HV-STOCK-TD-WIP-ADDS-02
                    HV-STOCK-TD-WIP-ADDS-03
                    HV-STOCK-TD-WIP-ADDS-04
                    HV-STOCK-TD-WIP-ADDS-05
                    HV-STOCK-TD-WIP-ADDS-06
                    HV-STOCK-TD-WIP-ADDS-07
                    HV-STOCK-TD-WIP-ADDS-08
                    HV-STOCK-TD-WIP-ADDS-09
                    HV-STOCK-TD-WIP-ADDS-10
                    HV-STOCK-TD-WIP-ADDS-11
                    HV-STOCK-TD-WIP-ADDS-12
                    HV-STOCK-TD-WIP-DEDS-01
                    HV-STOCK-TD-WIP-DEDS-02
                    HV-STOCK-TD-WIP-DEDS-03
                    HV-STOCK-TD-WIP-DEDS-04
                    HV-STOCK-TD-WIP-DEDS-05
                    HV-STOCK-TD-WIP-DEDS-06
                    HV-STOCK-TD-WIP-DEDS-07
                    HV-STOCK-TD-WIP-DEDS-08
                    HV-STOCK-TD-WIP-DEDS-09
                    HV-STOCK-TD-WIP-DEDS-10
                    HV-STOCK-TD-WIP-DEDS-11
                    HV-STOCK-TD-WIP-DEDS-12

*>      /MYSQL-END\
     end-call
*>
     if       return-code = -1     *> no more data so free cursor & return
              move     10 to fs-Reply WE-Error
              move     "EOF" to WS-File-Key
              move     zero  to Most-Cursor-Set
              go to ba999-End
     end-if
*>
     if       WS-MYSQL-Count-Rows = zero
              call  "MySQL_errno" using WS-MYSQL-Error-Number
              call  "MySQL_sqlstate" using WS-MYSQL-SQLstate
              move  WS-MYSQL-SqlState   to SQL-State
              if    WS-MYSQL-Error-Number  not = "0  "
                    call "MySQL_error" using WS-MYSQL-Error-Message
                    move 10 to fs-reply                  *> EOF equivilent !!
                    move 10 to WE-Error
                    move WS-MYSQL-Error-Number  to SQL-Err   *> Not really needed but help for testing
                    move WS-MYSQL-Error-Message to SQL-Msg   *>  ditto
                    initialize STOCK-REC with filler
                    move    "EOF2" to WS-File-Key
              end-if
              set Cursor-Not-Active to true
              go to ba999-End
     end-if.
*>
     if       fs-reply = 10
              set Cursor-Not-Active to true
              move    "EOF3" to WS-File-Key
              go to ba999-End
     end-if.
*>
     perform  bb100-UnloadHVs.       *> transfer/move HV vars to Record layout
     move     HV-Stock-Key to WS-File-Key.
     move     zero to fs-reply WE-Error.
     go       to ba999-end.
*>
 ba050-Process-Read-Indexed.
*>
*>  Now do on correct key within WHERE
*>  Sets up key and compare data
*>
     set      KOR-x1 to File-Key-No      *> 1 = Primary, 2 = Abrev, 3 = Desc (NOT Delete function as can be dups)
     move     KOR-offset (KOR-x1) to K   *> 4 = Location (NOT Delete as can be dups)  *> 29/01/25
     move     KOR-length (KOR-x1) to L
*>
     move     spaces to WS-Where
     move     1   to J
     string   "`"                   delimited by size
              KeyName (KOR-x1)      delimited by space
              "`"                   delimited by size
              '="'                  delimited by size
              Stock-Rec (K:L)       delimited by size
              '"'                   delimited by size
                      into WS-Where
                        with pointer J
     end-string
     move     WS-Where (1:J)   to WS-Log-Where.    *>  For test logging
     if    Testing-2
           display Display-Message-1 with erase eos
     end-if
     move     5 to ws-No-Paragraph
*>      /MYSQL SELECT\
*>
*>    Select rows
*>
*>             TABLE=STOCK-REC
           INITIALIZE WS-MYSQL-COMMAND
           STRING "SELECT * FROM "
             "`STOCK-REC`"
             " WHERE "
             WS-Where (1:J)
            ";"  X"00" INTO WS-MYSQL-COMMAND
           PERFORM MYSQL-1210-COMMAND THRU MYSQL-1219-EXIT
           PERFORM MYSQL-1220-STORE-RESULT THRU MYSQL-1239-EXIT
           MOVE WS-MYSQL-RESULT TO TP-STOCK-REC
*>      /MYSQL-END\
*>
     if     WS-MYSQL-Count-Rows = zero
            move 23  to fs-Reply             *> could also be 21 or 14
            move zero to WE-Error
            go to ba998-Free
     end-if
     move     6 to ws-No-Paragraph
*>      /MYSQL FETCH\
*>
*>    Fetch next record
*>
*>             TABLE=STOCK-REC
           MOVE TP-STOCK-REC TO WS-MYSQL-RESULT
           CALL "MySQL_fetch_record" USING WS-MYSQL-RESULT
                    HV-STOCK-KEY
                    HV-STOCK-ABREV-KEY
                    HV-STOCK-SUPPLIER-P1
                    HV-STOCK-SUPPLIER-P2
                    HV-STOCK-SUPPLIER-P3
                    HV-STOCK-DESC
                    HV-STOCK-CONSTRUCT-ITEM
                    HV-STOCK-LOCATION
                    HV-STOCK-PA-CODE
                    HV-STOCK-SA-CODE
                    HV-STOCK-SERVICES-FLAG
                    HV-STOCK-LAST-ACTUAL-COST
                    HV-STOCK-CONSTRUCT-BUNDLE
                    HV-STOCK-UNDER-CONSTRUCTION
                    HV-STOCK-WORK-IN-PROGRESS
                    HV-STOCK-REORDER-PNT
                    HV-STOCK-STD-REORDER
                    HV-STOCK-BACK-ORDERED
                    HV-STOCK-ON-ORDER
                    HV-STOCK-HELD
                    HV-STOCK-PRE-SALES
                    HV-STOCK-RETAIL
                    HV-STOCK-COST
                    HV-STOCK-VALUE
                    HV-STOCK-ARRIVAL-DATE
                    HV-STOCK-ORDER-DUE
                    HV-STOCK-ORDER-DAT
                    HV-STOCK-ADDS
                    HV-STOCK-DEDUCTS
                    HV-STOCK-WIP-ADDS
                    HV-STOCK-WIP-DEDS
                    HV-STOCK-TD-ADDS-01
                    HV-STOCK-TD-ADDS-02
                    HV-STOCK-TD-ADDS-03
                    HV-STOCK-TD-ADDS-04
                    HV-STOCK-TD-ADDS-05
                    HV-STOCK-TD-ADDS-06
                    HV-STOCK-TD-ADDS-07
                    HV-STOCK-TD-ADDS-08
                    HV-STOCK-TD-ADDS-09
                    HV-STOCK-TD-ADDS-10
                    HV-STOCK-TD-ADDS-11
                    HV-STOCK-TD-ADDS-12
                    HV-STOCK-TD-DEDS-01
                    HV-STOCK-TD-DEDS-02
                    HV-STOCK-TD-DEDS-03
                    HV-STOCK-TD-DEDS-04
                    HV-STOCK-TD-DEDS-05
                    HV-STOCK-TD-DEDS-06
                    HV-STOCK-TD-DEDS-07
                    HV-STOCK-TD-DEDS-08
                    HV-STOCK-TD-DEDS-09
                    HV-STOCK-TD-DEDS-10
                    HV-STOCK-TD-DEDS-11
                    HV-STOCK-TD-DEDS-12
                    HV-STOCK-TD-WIP-ADDS-01
                    HV-STOCK-TD-WIP-ADDS-02
                    HV-STOCK-TD-WIP-ADDS-03
                    HV-STOCK-TD-WIP-ADDS-04
                    HV-STOCK-TD-WIP-ADDS-05
                    HV-STOCK-TD-WIP-ADDS-06
                    HV-STOCK-TD-WIP-ADDS-07
                    HV-STOCK-TD-WIP-ADDS-08
                    HV-STOCK-TD-WIP-ADDS-09
                    HV-STOCK-TD-WIP-ADDS-10
                    HV-STOCK-TD-WIP-ADDS-11
                    HV-STOCK-TD-WIP-ADDS-12
                    HV-STOCK-TD-WIP-DEDS-01
                    HV-STOCK-TD-WIP-DEDS-02
                    HV-STOCK-TD-WIP-DEDS-03
                    HV-STOCK-TD-WIP-DEDS-04
                    HV-STOCK-TD-WIP-DEDS-05
                    HV-STOCK-TD-WIP-DEDS-06
                    HV-STOCK-TD-WIP-DEDS-07
                    HV-STOCK-TD-WIP-DEDS-08
                    HV-STOCK-TD-WIP-DEDS-09
                    HV-STOCK-TD-WIP-DEDS-10
                    HV-STOCK-TD-WIP-DEDS-11
                    HV-STOCK-TD-WIP-DEDS-12

*>      /MYSQL-END\
     end-call
*>
     if       WS-MYSQL-Count-Rows not > zero
              call     "MySQL_errno" using WS-MYSQL-Error-Number
              call  "MySQL_sqlstate" using WS-MYSQL-SQLstate
              move  WS-MYSQL-SqlState   to SQL-State
              if    WS-MYSQL-Error-Number  not = "0  "
                    move 23 to fs-reply
                    move 990 to WE-Error
                    call "MySQL_error" using WS-MYSQL-Error-Message
                    move WS-MYSQL-Error-Number to SQL-Err
                    move WS-MYSQL-Error-Message to SQL-Msg
                    move spaces to WS-File-Key
                    go to ba998-Free
              else
                    move 23   to fs-reply
                    move 989  to WE-Error
                    move zero to SQL-Err
                    move spaces to SQL-Msg
                    move spaces to WS-File-Key
                    go to ba998-Free
              end-if
     end-if                        *> row count zero should show up as a MYSQL error ?
     perform  bb100-UnloadHVs       *> transfer/move HV vars to ws-Record layout
     move     HV-Stock-Key to WS-File-Key
     move     zero to FS-Reply WE-Error.
     go       to ba998-Free.
*>
 ba060-Process-Start.
*>
*>  Check for Param error 1st on start
*>
     if       access-type < 5 or > 8                   *> not using not < or not > WAS 8 6/8/23
              move 99 to FS-Reply
              move 997 to WE-Error                     *> Invalid calling parameter settings     997
              go to ba999-end
     end-if
*>
*>  First clear any active cursors
*>
     if       Cursor-Active
              perform ba998-Free.
*>
*>  Now do Start on correct key before read-next  within WHERE
*>  Set up MOST-relation for condition test and key
*>
     set      KOR-x1 to File-Key-No.    *> 1 = Primary, 2 = Abrev, [ 3 = Desc & 4 = Location ] (NOT Delete function as can be dups)
     move     KOR-offset (KOR-x1) to K
     move     KOR-length (KOR-x1) to L
*>
     move     spaces to MOST-Relation.
     move     1   to J.
     evaluate Access-Type
              when  5                           *> fn-equal-to
                    move "=  " to MOST-Relation
              when  6                           *> fn-less-than
                    move "<  " to MOST-Relation
              when  7                           *> fn-greater-than
                    move ">  " to MOST-Relation
              when  8                           *> fn-not-less-than
                    move ">= " to MOST-Relation
              when  9                           *> fn-not-greater-than
                    move "<= " to MOST-Relation
     end-evaluate
*>
     move     spaces to WS-Where
     string   "`"                   delimited by size
              KeyName (KOR-x1)      delimited by space
              "`"                   delimited by size
              MOST-relation         delimited by space
              '"'                   delimited by size
              Stock-Rec (K:L)       delimited by size
              '"'                   delimited by size
              ' ORDER BY '          delimited by size
              "`"                   delimited by size
              keyname (KOR-x1)      delimited by space
              "`"                   delimited by size
                ' ASC  '            delimited by size
                             into WS-Where
                             with pointer J
     end-string
     move     WS-Where (1:J)  to WS-Log-Where.    *>  For test logging
     move     Stock-Rec (K:L) to WS-File-Key
     if    Testing-2
           display Display-Message-1 with erase eos
     end-if
*>
     move     8 to ws-No-Paragraph
*>      /MYSQL SELECT\
*>
*>    Select rows
*>
*>             TABLE=STOCK-REC
           INITIALIZE WS-MYSQL-COMMAND
           STRING "SELECT * FROM "
             "`STOCK-REC`"
             " WHERE "
             WS-Where (1:J)
            ";"  X"00" INTO WS-MYSQL-COMMAND
           PERFORM MYSQL-1210-COMMAND THRU MYSQL-1219-EXIT
           PERFORM MYSQL-1220-STORE-RESULT THRU MYSQL-1239-EXIT
           MOVE WS-MYSQL-RESULT TO TP-STOCK-REC
*>      /MYSQL-END\
*>
     if       WS-MYSQL-Count-Rows not zero
              set Cursor-Active to true
     end-if
*>
     if       WS-MYSQL-Count-Rows = zero
              call  "MySQL_errno" using WS-MYSQL-Error-Number
              call  "MySQL_sqlstate" using WS-MYSQL-SQLstate
              move  WS-MYSQL-SqlState   to SQL-State
              if    WS-MYSQL-Error-Number  not = "0  "
                    call "MySQL_error" using WS-MYSQL-Error-Message
                    move WS-MYSQL-Error-Number  to SQL-Err
                    move WS-MYSQL-Error-Message to SQL-Msg
                    move 21 to fs-reply                  *> this may need changing for val in WE-Error!!
                    move zero to WE-Error
              end-if
     else
              move  zero to FS-Reply WE-Error
     end-if
     go       to ba999-end.
*>
*>  Now a read next will process a Fetch from cursor
*>
 ba070-Process-Write.
     perform  bb000-HV-Load.                       *>  move stock-rec fields to HV fields
     move     zero to FS-Reply WE-Error
     move     spaces to SQL-Msg
     move     zero to SQL-Err
     move     10 to ws-No-Paragraph.
     perform  bb200-Insert.
     if       WS-MYSQL-COUNT-ROWS not = 1
              call "MySQL_errno" using WS-MYSQL-Error-Number
              call  "MySQL_sqlstate" using WS-MYSQL-SQLstate
              move  WS-MYSQL-SqlState   to SQL-State
              if    WS-MYSQL-Error-Number  not = "0  "
                    call "MySQL_error" using Ws-Mysql-Error-Message
                    move WS-MYSQL-Error-Number  to SQL-Err
                    move WS-MYSQL-Error-Message to SQL-Msg
                    if    SQL-Err (1:4) = "1062"
                                     or = "1022"
                          or Sql-State = "23000"  *> Dup key (rec already present)
                          move 22 to fs-reply
                    else
                          move 99 to fs-reply                  *> this may need changing for val in WE-Error!!
                    end-if
              end-if
     end-if
     go       to ba999-End.
*>
 ba080-Process-Delete.
*>
*> Can only use Primary key and non dup 2nd
*>
     set      KOR-x1 to File-Key-No      *> 1 = Primary, 2 = Abrev, [ 3 = Desc and 4 = Location ]
                                         *> (NOT Delete function as can be dups)
     move     KOR-offset (KOR-x1) to K
     move     KOR-length (KOR-x1) to L
*>
     move     spaces to WS-Where
     move     1   to J
     string   "`"                   delimited by size
              KeyName (KOR-x1)      delimited by space
              "`"                   delimited by size
              '="'                  delimited by size
              Stock-Rec (K:L)       delimited by size
              '"'                   delimited by size
                      into WS-Where
                        with pointer J
     end-string
     move     Stock-Rec (K:L)  to WS-File-Key.
     move     WS-Where (1:J)   to WS-Log-Where.   *>  For test logging
     if    Testing-2
           display Display-Message-1 with erase eos
     end-if
     move     13 to ws-No-Paragraph.
*>      /MYSQL DELETE\
*>
*>    Delete a row
*>
*>             TABLE=STOCK-REC
           INITIALIZE WS-MYSQL-COMMAND
           STRING "DELETE FROM "
             "`STOCK-REC`"
             " WHERE "
             WS-Where (1:J)
             X"00" INTO WS-MYSQL-COMMAND
           PERFORM MYSQL-1210-COMMAND THRU MYSQL-1219-EXIT
*>      /MYSQL-END\
     if       WS-MYSQL-COUNT-ROWS not = 1
              call "MySQL_errno" using WS-MYSQL-Error-Number
              call  "MySQL_sqlstate" using WS-MYSQL-SQLstate
              move  WS-MYSQL-SqlState   to SQL-State
              if    WS-MYSQL-Error-Number  not = "0  "
                    call "MySQL_error" using Ws-Mysql-Error-Message
                    move WS-MYSQL-Error-Number  to SQL-Err
                    move WS-MYSQL-Error-Message to SQL-Msg
                    move 99 to fs-reply
                    move 995 to WE-Error
              end-if
              go to ba999-End
     else
              move spaces to SQL-Msg SQL-State
              move zero   to SQL-Err
     end-if.
     move     zero to FS-Reply WE-Error.
     go       to ba999-End.
*>
 ba090-Process-Rewrite.
     perform  bb000-HV-Load.       *> Load up the HV fields from table record in WS
     move     17 to ws-No-Paragraph.
     set      KOR-x1 to 1            *> 1 = Primary, 2 = Abrev, [3 = Desc & 4 = Location] (NOT Delete function as can be dups)
     move     KOR-offset (KOR-x1) to K
     move     KOR-length (KOR-x1) to L
*>
     move     spaces to WS-Where
     move     1   to J
     string   "`"                   delimited by size
              KeyName (KOR-x1)      delimited by space
              "`"                   delimited by size
              '="'                  delimited by size
              Stock-Rec (K:L)       delimited by size
              '"'                   delimited by size
                      into WS-Where
                        with pointer J
     end-string
     move     WS-Where (1:J)   to WS-Log-Where.    *>  For test logging
     perform  bb300-Update.
*>
     if       Testing-2
              display Display-Message-1 with erase eos
     end-if
*>
     if       WS-MYSQL-COUNT-ROWS not = 1
              call     "MySQL_errno" using WS-MYSQL-Error-Number
              call     "MySQL_sqlstate" using WS-MYSQL-SQLstate
              move     WS-MYSQL-SqlState   to SQL-State
              if    WS-MYSQL-Error-Number  not = "0  "
                    call "MySQL_error" using Ws-Mysql-Error-Message
                    move WS-MYSQL-Error-Number  to SQL-Err
                    move WS-MYSQL-Error-Message to SQL-Msg
                    move 99 to fs-reply                  *> this may need changing for val in WE-Error!!
                    move 994 to WE-Error
              end-if
              go to ba999-End
     end-if
     move     zero   to FS-Reply WE-Error.
     move     zero   to SQL-Err.
     move     spaces to SQL-Msg SQL-State.
     go       to ba999-end.
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
*>             TABLE=STOCK-REC
           MOVE TP-STOCK-REC TO WS-MYSQL-RESULT
           CALL "MySQL_free_result" USING WS-MYSQL-RESULT end-call
*>      /MYSQL-END\
     set Cursor-Not-Active to true.
*>
 ba999-end.
*>  Any Clean ups before quiting    move data record ?????  do so at the start as well ??????
*>
     if       Testing-1
              perform Ca-Process-Logs
     end-if.
*>
 ba999-exit.
     exit program.
*>
 bb000-HV-Load      Section.
*>*************************
*>
*>  Load the Host variables with data from the passed record
*>
*> This Method loads the Host Variables for the Base table with
*> the data passed in the data-buffer.
*>
     initialize TD-STOCK-REC.
     move     stock-key     to hv-stock-key
     move     STOCK-ABREV-KEY          to HV-STOCK-ABREV-KEY                *> secondary #2 key
     move     STOCK-ADDS               to HV-STOCK-ADDS
     move     STOCK-ARRIVAL-DATE       to HV-STOCK-ARRIVAL-DATE
     move     STOCK-BACK-ORDERED       to HV-STOCK-BACK-ORDERED
     move     STOCK-CONSTRUCT-BUNDLE   to HV-STOCK-CONSTRUCT-BUNDLE
     move     STOCK-CONSTRUCT-ITEM     to HV-STOCK-CONSTRUCT-ITEM
     move     STOCK-COST               to HV-STOCK-COST
     move     STOCK-DEDUCTS            to HV-STOCK-DEDUCTS
     move     STOCK-DESC               to HV-STOCK-DESC                    *> secondary #3 key
     move     STOCK-HELD               to HV-STOCK-HELD
     move     STOCK-LAST-ACTUAL-COST   to HV-STOCK-LAST-ACTUAL-COST
     move     STOCK-LOCATION           to HV-STOCK-LOCATION                *> secondary #4 key
     move     STOCK-ON-ORDER           to HV-STOCK-ON-ORDER
     move     STOCK-ORDER-DAT          to HV-STOCK-ORDER-DAT
     move     STOCK-ORDER-DUE          to HV-STOCK-ORDER-DUE
     move     STOCK-PA-CODE            to HV-STOCK-PA-CODE
     move     STOCK-PRE-SALES          to HV-STOCK-PRE-SALES
     move     STOCK-REORDER-PNT        to HV-STOCK-REORDER-PNT
     move     STOCK-RETAIL             to HV-STOCK-RETAIL
     move     STOCK-SA-CODE            to HV-STOCK-SA-CODE
     move     STOCK-SERVICES-FLAG      to HV-STOCK-SERVICES-FLAG
     move     STOCK-STD-REORDER        to HV-STOCK-STD-REORDER
     move     STOCK-SUPPLIER-P1        to HV-STOCK-SUPPLIER-P1
     move     STOCK-SUPPLIER-P2        to HV-STOCK-SUPPLIER-P2
     move     STOCK-SUPPLIER-P3        to HV-STOCK-SUPPLIER-P3
     move     STOCK-TD-ADDS-01         to HV-STOCK-TD-ADDS-01
     move     STOCK-TD-ADDS-02         to HV-STOCK-TD-ADDS-02
     move     STOCK-TD-ADDS-03         to HV-STOCK-TD-ADDS-03
     move     STOCK-TD-ADDS-04         to HV-STOCK-TD-ADDS-04
     move     STOCK-TD-ADDS-05         to HV-STOCK-TD-ADDS-05
     move     STOCK-TD-ADDS-06         to HV-STOCK-TD-ADDS-06
     move     STOCK-TD-ADDS-07         to HV-STOCK-TD-ADDS-07
     move     STOCK-TD-ADDS-08         to HV-STOCK-TD-ADDS-08
     move     STOCK-TD-ADDS-09         to HV-STOCK-TD-ADDS-09
     move     STOCK-TD-ADDS-10         to HV-STOCK-TD-ADDS-10
     move     STOCK-TD-ADDS-11         to HV-STOCK-TD-ADDS-11
     move     STOCK-TD-ADDS-12         to HV-STOCK-TD-ADDS-12
     move     STOCK-TD-DEDS-01         to HV-STOCK-TD-DEDS-01
     move     STOCK-TD-DEDS-02         to HV-STOCK-TD-DEDS-02
     move     STOCK-TD-DEDS-03         to HV-STOCK-TD-DEDS-03
     move     STOCK-TD-DEDS-04         to HV-STOCK-TD-DEDS-04
     move     STOCK-TD-DEDS-05         to HV-STOCK-TD-DEDS-05
     move     STOCK-TD-DEDS-06         to HV-STOCK-TD-DEDS-06
     move     STOCK-TD-DEDS-07         to HV-STOCK-TD-DEDS-07
     move     STOCK-TD-DEDS-08         to HV-STOCK-TD-DEDS-08
     move     STOCK-TD-DEDS-09         to HV-STOCK-TD-DEDS-09
     move     STOCK-TD-DEDS-10         to HV-STOCK-TD-DEDS-10
     move     STOCK-TD-DEDS-11         to HV-STOCK-TD-DEDS-11
     move     STOCK-TD-DEDS-12         to HV-STOCK-TD-DEDS-12
     move     STOCK-TD-WIP-ADDS-01     to HV-STOCK-TD-WIP-ADDS-01
     move     STOCK-TD-WIP-ADDS-02     to HV-STOCK-TD-WIP-ADDS-02
     move     STOCK-TD-WIP-ADDS-03     to HV-STOCK-TD-WIP-ADDS-03
     move     STOCK-TD-WIP-ADDS-04     to HV-STOCK-TD-WIP-ADDS-04
     move     STOCK-TD-WIP-ADDS-05     to HV-STOCK-TD-WIP-ADDS-05
     move     STOCK-TD-WIP-ADDS-06     to HV-STOCK-TD-WIP-ADDS-06
     move     STOCK-TD-WIP-ADDS-07     to HV-STOCK-TD-WIP-ADDS-07
     move     STOCK-TD-WIP-ADDS-08     to HV-STOCK-TD-WIP-ADDS-08
     move     STOCK-TD-WIP-ADDS-09     to HV-STOCK-TD-WIP-ADDS-09
     move     STOCK-TD-WIP-ADDS-10     to HV-STOCK-TD-WIP-ADDS-10
     move     STOCK-TD-WIP-ADDS-11     to HV-STOCK-TD-WIP-ADDS-11
     move     STOCK-TD-WIP-ADDS-12     to HV-STOCK-TD-WIP-ADDS-12
     move     STOCK-TD-WIP-DEDS-01     to HV-STOCK-TD-WIP-DEDS-01
     move     STOCK-TD-WIP-DEDS-02     to HV-STOCK-TD-WIP-DEDS-02
     move     STOCK-TD-WIP-DEDS-03     to HV-STOCK-TD-WIP-DEDS-03
     move     STOCK-TD-WIP-DEDS-04     to HV-STOCK-TD-WIP-DEDS-04
     move     STOCK-TD-WIP-DEDS-05     to HV-STOCK-TD-WIP-DEDS-05
     move     STOCK-TD-WIP-DEDS-06     to HV-STOCK-TD-WIP-DEDS-06
     move     STOCK-TD-WIP-DEDS-07     to HV-STOCK-TD-WIP-DEDS-07
     move     STOCK-TD-WIP-DEDS-08     to HV-STOCK-TD-WIP-DEDS-08
     move     STOCK-TD-WIP-DEDS-09     to HV-STOCK-TD-WIP-DEDS-09
     move     STOCK-TD-WIP-DEDS-10     to HV-STOCK-TD-WIP-DEDS-10
     move     STOCK-TD-WIP-DEDS-11     to HV-STOCK-TD-WIP-DEDS-11
     move     STOCK-TD-WIP-DEDS-12     to HV-STOCK-TD-WIP-DEDS-12
     move     STOCK-UNDER-CONSTRUCTION to HV-STOCK-UNDER-CONSTRUCTION
     move     STOCK-VALUE              to HV-STOCK-VALUE
     move     STOCK-WIP-ADDS           to HV-STOCK-WIP-ADDS
     move     STOCK-WIP-DEDS           to HV-STOCK-WIP-DEDS
     move     STOCK-WORK-IN-PROGRESS   to HV-STOCK-WORK-IN-PROGRESS.
*>
*> Loading HVs implies a non-Fetch action. RGs are handled separately for
*> all such actions so they must not be loaded here.
*>
 bb000-Exit.
     exit section.
*>
 bb100-UnloadHVs    Section.
*>*************************
*>
*>  Load the data buffer in the interface with data from the host variables.
*> (init moved lower)
*>
*> NULL fields must not be returned in the buffer. SQL filters each column to
*>  ensure it has a proper value.  This saves using indicator variables.
*>
     initialize STOCK-REC.
*>
     move     HV-STOCK-ABREV-KEY          to  STOCK-ABREV-KEY
     move     HV-STOCK-ADDS               to  STOCK-ADDS
     move     HV-STOCK-ARRIVAL-DATE       to  STOCK-ARRIVAL-DATE
     move     HV-STOCK-BACK-ORDERED       to  STOCK-BACK-ORDERED
     move     HV-STOCK-CONSTRUCT-BUNDLE   to  STOCK-CONSTRUCT-BUNDLE
     move     HV-STOCK-CONSTRUCT-ITEM     to  STOCK-CONSTRUCT-ITEM
     move     HV-STOCK-COST               to  STOCK-COST
     move     HV-STOCK-DEDUCTS            to  STOCK-DEDUCTS
     move     HV-STOCK-DESC               to  STOCK-DESC
     move     HV-STOCK-HELD               to  STOCK-HELD
     move     HV-STOCK-KEY                to  STOCK-KEY
     move     HV-STOCK-LAST-ACTUAL-COST   to  STOCK-LAST-ACTUAL-COST
     move     HV-STOCK-LOCATION           to  STOCK-LOCATION
     move     HV-STOCK-ON-ORDER           to  STOCK-ON-ORDER
     move     HV-STOCK-ORDER-DUE          to  STOCK-ORDER-DUE
     move     HV-STOCK-ORDER-DAT          to  STOCK-ORDER-DAT
     move     HV-STOCK-PA-CODE            to  STOCK-PA-CODE
     move     HV-STOCK-PRE-SALES          to  STOCK-PRE-SALES
     move     HV-STOCK-REORDER-PNT        to  STOCK-REORDER-PNT
     move     HV-STOCK-RETAIL             to  STOCK-RETAIL
     move     HV-STOCK-SA-CODE            to  STOCK-SA-CODE
     move     HV-STOCK-SERVICES-FLAG      to  STOCK-SERVICES-FLAG
     move     HV-STOCK-STD-REORDER        to  STOCK-STD-REORDER
     move     HV-STOCK-SUPPLIER-P1        to  STOCK-SUPPLIER-P1
     move     HV-STOCK-SUPPLIER-P2        to  STOCK-SUPPLIER-P2
     move     HV-STOCK-SUPPLIER-P3        to  STOCK-SUPPLIER-P3
     move     HV-STOCK-TD-ADDS-01         to  STOCK-TD-ADDS-01
     move     HV-STOCK-TD-ADDS-02         to  STOCK-TD-ADDS-02
     move     HV-STOCK-TD-ADDS-03         to  STOCK-TD-ADDS-03
     move     HV-STOCK-TD-ADDS-04         to  STOCK-TD-ADDS-04
     move     HV-STOCK-TD-ADDS-05         to  STOCK-TD-ADDS-05
     move     HV-STOCK-TD-ADDS-06         to  STOCK-TD-ADDS-06
     move     HV-STOCK-TD-ADDS-07         to  STOCK-TD-ADDS-07
     move     HV-STOCK-TD-ADDS-08         to  STOCK-TD-ADDS-08
     move     HV-STOCK-TD-ADDS-09         to  STOCK-TD-ADDS-09
     move     HV-STOCK-TD-ADDS-10         to  STOCK-TD-ADDS-10
     move     HV-STOCK-TD-ADDS-11         to  STOCK-TD-ADDS-11
     move     HV-STOCK-TD-ADDS-12         to  STOCK-TD-ADDS-12
     move     HV-STOCK-TD-DEDS-01         to  STOCK-TD-DEDS-01
     move     HV-STOCK-TD-DEDS-02         to  STOCK-TD-DEDS-02
     move     HV-STOCK-TD-DEDS-03         to  STOCK-TD-DEDS-03
     move     HV-STOCK-TD-DEDS-04         to  STOCK-TD-DEDS-04
     move     HV-STOCK-TD-DEDS-05         to  STOCK-TD-DEDS-05
     move     HV-STOCK-TD-DEDS-06         to  STOCK-TD-DEDS-06
     move     HV-STOCK-TD-DEDS-07         to  STOCK-TD-DEDS-07
     move     HV-STOCK-TD-DEDS-08         to  STOCK-TD-DEDS-08
     move     HV-STOCK-TD-DEDS-09         to  STOCK-TD-DEDS-09
     move     HV-STOCK-TD-DEDS-10         to  STOCK-TD-DEDS-10
     move     HV-STOCK-TD-DEDS-11         to  STOCK-TD-DEDS-11
     move     HV-STOCK-TD-DEDS-12         to  STOCK-TD-DEDS-12
     move     HV-STOCK-TD-WIP-ADDS-01     to  STOCK-TD-WIP-ADDS-01
     move     HV-STOCK-TD-WIP-ADDS-02     to  STOCK-TD-WIP-ADDS-02
     move     HV-STOCK-TD-WIP-ADDS-03     to  STOCK-TD-WIP-ADDS-03
     move     HV-STOCK-TD-WIP-ADDS-04     to  STOCK-TD-WIP-ADDS-04
     move     HV-STOCK-TD-WIP-ADDS-05     to  STOCK-TD-WIP-ADDS-05
     move     HV-STOCK-TD-WIP-ADDS-06     to  STOCK-TD-WIP-ADDS-06
     move     HV-STOCK-TD-WIP-ADDS-07     to  STOCK-TD-WIP-ADDS-07
     move     HV-STOCK-TD-WIP-ADDS-08     to  STOCK-TD-WIP-ADDS-08
     move     HV-STOCK-TD-WIP-ADDS-09     to  STOCK-TD-WIP-ADDS-09
     move     HV-STOCK-TD-WIP-ADDS-10     to  STOCK-TD-WIP-ADDS-10
     move     HV-STOCK-TD-WIP-ADDS-11     to  STOCK-TD-WIP-ADDS-11
     move     HV-STOCK-TD-WIP-ADDS-12     to  STOCK-TD-WIP-ADDS-12
     move     HV-STOCK-TD-WIP-DEDS-01     to  STOCK-TD-WIP-DEDS-01
     move     HV-STOCK-TD-WIP-DEDS-02     to  STOCK-TD-WIP-DEDS-02
     move     HV-STOCK-TD-WIP-DEDS-03     to  STOCK-TD-WIP-DEDS-03
     move     HV-STOCK-TD-WIP-DEDS-04     to  STOCK-TD-WIP-DEDS-04
     move     HV-STOCK-TD-WIP-DEDS-05     to  STOCK-TD-WIP-DEDS-05
     move     HV-STOCK-TD-WIP-DEDS-06     to  STOCK-TD-WIP-DEDS-06
     move     HV-STOCK-TD-WIP-DEDS-07     to  STOCK-TD-WIP-DEDS-07
     move     HV-STOCK-TD-WIP-DEDS-08     to  STOCK-TD-WIP-DEDS-08
     move     HV-STOCK-TD-WIP-DEDS-09     to  STOCK-TD-WIP-DEDS-09
     move     HV-STOCK-TD-WIP-DEDS-10     to  STOCK-TD-WIP-DEDS-10
     move     HV-STOCK-TD-WIP-DEDS-11     to  STOCK-TD-WIP-DEDS-11
     move     HV-STOCK-TD-WIP-DEDS-12     to  STOCK-TD-WIP-DEDS-12
     move     HV-STOCK-UNDER-CONSTRUCTION to  STOCK-UNDER-CONSTRUCTION
     move     HV-STOCK-VALUE              to  STOCK-VALUE
     move     HV-STOCK-WIP-ADDS           to  STOCK-WIP-ADDS
     move     HV-STOCK-WIP-DEDS           to  STOCK-WIP-DEDS
     move     HV-STOCK-WORK-IN-PROGRESS   to  STOCK-WORK-IN-PROGRESS.
*>
 bb100-Exit.
     exit section.
*>
 bb200-Insert Section.
*>*******************
*>
*>  /MYSQL INSERT\
*>
*>    Insert a row
*>
*>         TABLE=STOCK-REC
           INITIALIZE WS-MYSQL-COMMAND
           MOVE 1 TO WS-MYSQL-I
           STRING 'INSERT INTO '
                    '`STOCK-REC` SET '
              INTO WS-MYSQL-COMMAND
              WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-KEY`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           STRING FUNCTION TRIM (HV-STOCK-KEY,TRAILING)
                  '"'
                   INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-ABREV-KEY`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           STRING FUNCTION TRIM (HV-STOCK-ABREV-KEY,TRAILING)
                  '"'
                   INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-SUPPLIER-P1`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           STRING FUNCTION TRIM (HV-STOCK-SUPPLIER-P1,TRAILING)
                  '"'
                   INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-SUPPLIER-P2`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           STRING FUNCTION TRIM (HV-STOCK-SUPPLIER-P2,TRAILING)
                  '"'
                   INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-SUPPLIER-P3`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           STRING FUNCTION TRIM (HV-STOCK-SUPPLIER-P3,TRAILING)
                  '"'
                   INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-DESC`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           STRING FUNCTION TRIM (HV-STOCK-DESC,TRAILING)
                  '"'
                   INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-CONSTRUCT-ITEM`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           STRING FUNCTION TRIM (HV-STOCK-CONSTRUCT-ITEM,TRAILING)
                  '"'
                   INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-LOCATION`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           STRING FUNCTION TRIM (HV-STOCK-LOCATION,TRAILING)
                  '"'
                   INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-PA-CODE`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           STRING FUNCTION TRIM (HV-STOCK-PA-CODE,TRAILING)
                  '"'
                   INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-SA-CODE`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           STRING FUNCTION TRIM (HV-STOCK-SA-CODE,TRAILING)
                  '"'
                   INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-SERVICES-FLAG`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           STRING FUNCTION TRIM (HV-STOCK-SERVICES-FLAG,TRAILING)
                  '"'
                   INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-LAST-ACTUAL-COST`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-STOCK-LAST-ACTUAL-COST
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(14:07))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING "." INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING WS-MYSQL-EDIT(22:02)
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-CONSTRUCT-BUNDLE`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-STOCK-CONSTRUCT-BUNDLE
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(11:10))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-UNDER-CONSTRUCTION`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-STOCK-UNDER-CONSTRUCTION
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(11:10))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-WORK-IN-PROGRESS`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-STOCK-WORK-IN-PROGRESS
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(11:10))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-REORDER-PNT`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-STOCK-REORDER-PNT
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(11:10))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-STD-REORDER`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-STOCK-STD-REORDER
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(11:10))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-BACK-ORDERED`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-STOCK-BACK-ORDERED
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(11:10))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-ON-ORDER`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-STOCK-ON-ORDER
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(11:10))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-HELD`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-STOCK-HELD
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(11:10))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-PRE-SALES`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-STOCK-PRE-SALES
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(11:10))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-RETAIL`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-STOCK-RETAIL
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(14:07))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING "." INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING WS-MYSQL-EDIT(22:02)
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-COST`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-STOCK-COST
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(14:07))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING "." INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING WS-MYSQL-EDIT(22:04)
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-VALUE`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-STOCK-VALUE
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(12:09))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING "." INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING WS-MYSQL-EDIT(22:02)
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-ARRIVAL-DATE`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-STOCK-ARRIVAL-DATE
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(11:10))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-ORDER-DUE`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-STOCK-ORDER-DUE
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(11:10))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-ORDER-DAT`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-STOCK-ORDER-DAT
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(11:10))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-ADDS`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-STOCK-ADDS
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(11:10))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-DEDUCTS`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-STOCK-DEDUCTS
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(11:10))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-WIP-ADDS`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-STOCK-WIP-ADDS
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(11:10))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-WIP-DEDS`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-STOCK-WIP-DEDS
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(11:10))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-TD-ADDS-01`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-STOCK-TD-ADDS-01
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(11:10))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-TD-ADDS-02`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-STOCK-TD-ADDS-02
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(11:10))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-TD-ADDS-03`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-STOCK-TD-ADDS-03
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(11:10))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-TD-ADDS-04`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-STOCK-TD-ADDS-04
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(11:10))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-TD-ADDS-05`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-STOCK-TD-ADDS-05
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(11:10))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-TD-ADDS-06`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-STOCK-TD-ADDS-06
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(11:10))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-TD-ADDS-07`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-STOCK-TD-ADDS-07
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(11:10))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-TD-ADDS-08`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-STOCK-TD-ADDS-08
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(11:10))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-TD-ADDS-09`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-STOCK-TD-ADDS-09
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(11:10))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-TD-ADDS-10`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-STOCK-TD-ADDS-10
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(11:10))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-TD-ADDS-11`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-STOCK-TD-ADDS-11
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(11:10))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-TD-ADDS-12`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-STOCK-TD-ADDS-12
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(11:10))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-TD-DEDS-01`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-STOCK-TD-DEDS-01
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(11:10))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-TD-DEDS-02`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-STOCK-TD-DEDS-02
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(11:10))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-TD-DEDS-03`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-STOCK-TD-DEDS-03
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(11:10))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-TD-DEDS-04`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-STOCK-TD-DEDS-04
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(11:10))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-TD-DEDS-05`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-STOCK-TD-DEDS-05
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(11:10))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-TD-DEDS-06`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-STOCK-TD-DEDS-06
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(11:10))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-TD-DEDS-07`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-STOCK-TD-DEDS-07
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(11:10))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-TD-DEDS-08`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-STOCK-TD-DEDS-08
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(11:10))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-TD-DEDS-09`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-STOCK-TD-DEDS-09
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(11:10))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-TD-DEDS-10`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-STOCK-TD-DEDS-10
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(11:10))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-TD-DEDS-11`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-STOCK-TD-DEDS-11
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(11:10))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-TD-DEDS-12`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-STOCK-TD-DEDS-12
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(11:10))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-TD-WIP-ADDS-01`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-STOCK-TD-WIP-ADDS-01
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(11:10))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-TD-WIP-ADDS-02`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-STOCK-TD-WIP-ADDS-02
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(11:10))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-TD-WIP-ADDS-03`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-STOCK-TD-WIP-ADDS-03
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(11:10))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-TD-WIP-ADDS-04`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-STOCK-TD-WIP-ADDS-04
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(11:10))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-TD-WIP-ADDS-05`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-STOCK-TD-WIP-ADDS-05
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(11:10))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-TD-WIP-ADDS-06`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-STOCK-TD-WIP-ADDS-06
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(11:10))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-TD-WIP-ADDS-07`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-STOCK-TD-WIP-ADDS-07
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(11:10))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-TD-WIP-ADDS-08`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-STOCK-TD-WIP-ADDS-08
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(11:10))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-TD-WIP-ADDS-09`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-STOCK-TD-WIP-ADDS-09
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(11:10))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-TD-WIP-ADDS-10`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-STOCK-TD-WIP-ADDS-10
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(11:10))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-TD-WIP-ADDS-11`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-STOCK-TD-WIP-ADDS-11
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(11:10))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-TD-WIP-ADDS-12`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-STOCK-TD-WIP-ADDS-12
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(11:10))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-TD-WIP-DEDS-01`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-STOCK-TD-WIP-DEDS-01
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(11:10))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-TD-WIP-DEDS-02`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-STOCK-TD-WIP-DEDS-02
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(11:10))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-TD-WIP-DEDS-03`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-STOCK-TD-WIP-DEDS-03
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(11:10))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-TD-WIP-DEDS-04`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-STOCK-TD-WIP-DEDS-04
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(11:10))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-TD-WIP-DEDS-05`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-STOCK-TD-WIP-DEDS-05
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(11:10))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-TD-WIP-DEDS-06`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-STOCK-TD-WIP-DEDS-06
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(11:10))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-TD-WIP-DEDS-07`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-STOCK-TD-WIP-DEDS-07
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(11:10))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-TD-WIP-DEDS-08`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-STOCK-TD-WIP-DEDS-08
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(11:10))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-TD-WIP-DEDS-09`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-STOCK-TD-WIP-DEDS-09
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(11:10))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-TD-WIP-DEDS-10`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-STOCK-TD-WIP-DEDS-10
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(11:10))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-TD-WIP-DEDS-11`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-STOCK-TD-WIP-DEDS-11
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(11:10))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-TD-WIP-DEDS-12`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-STOCK-TD-WIP-DEDS-12
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(11:10))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
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
*>      TABLE=STOCK-REC
           INITIALIZE WS-MYSQL-COMMAND
           MOVE 1 TO WS-MYSQL-I
           STRING 'UPDATE '
                    '`STOCK-REC` SET '
              INTO WS-MYSQL-COMMAND
              WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-KEY`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           STRING FUNCTION TRIM (HV-STOCK-KEY,TRAILING)
                  '"'
                   INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-ABREV-KEY`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           STRING FUNCTION TRIM (HV-STOCK-ABREV-KEY,TRAILING)
                  '"'
                   INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-SUPPLIER-P1`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           STRING FUNCTION TRIM (HV-STOCK-SUPPLIER-P1,TRAILING)
                  '"'
                   INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-SUPPLIER-P2`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           STRING FUNCTION TRIM (HV-STOCK-SUPPLIER-P2,TRAILING)
                  '"'
                   INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-SUPPLIER-P3`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           STRING FUNCTION TRIM (HV-STOCK-SUPPLIER-P3,TRAILING)
                  '"'
                   INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-DESC`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           STRING FUNCTION TRIM (HV-STOCK-DESC,TRAILING)
                  '"'
                   INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-CONSTRUCT-ITEM`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           STRING FUNCTION TRIM (HV-STOCK-CONSTRUCT-ITEM,TRAILING)
                  '"'
                   INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-LOCATION`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           STRING FUNCTION TRIM (HV-STOCK-LOCATION,TRAILING)
                  '"'
                   INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-PA-CODE`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           STRING FUNCTION TRIM (HV-STOCK-PA-CODE,TRAILING)
                  '"'
                   INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-SA-CODE`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           STRING FUNCTION TRIM (HV-STOCK-SA-CODE,TRAILING)
                  '"'
                   INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-SERVICES-FLAG`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           STRING FUNCTION TRIM (HV-STOCK-SERVICES-FLAG,TRAILING)
                  '"'
                   INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-LAST-ACTUAL-COST`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-STOCK-LAST-ACTUAL-COST
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(14:07))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING "." INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING WS-MYSQL-EDIT(22:02)
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-CONSTRUCT-BUNDLE`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-STOCK-CONSTRUCT-BUNDLE
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(11:10))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-UNDER-CONSTRUCTION`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-STOCK-UNDER-CONSTRUCTION
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(11:10))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-WORK-IN-PROGRESS`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-STOCK-WORK-IN-PROGRESS
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(11:10))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-REORDER-PNT`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-STOCK-REORDER-PNT
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(11:10))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-STD-REORDER`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-STOCK-STD-REORDER
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(11:10))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-BACK-ORDERED`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-STOCK-BACK-ORDERED
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(11:10))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-ON-ORDER`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-STOCK-ON-ORDER
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(11:10))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-HELD`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-STOCK-HELD
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(11:10))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-PRE-SALES`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-STOCK-PRE-SALES
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(11:10))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-RETAIL`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-STOCK-RETAIL
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(14:07))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING "." INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING WS-MYSQL-EDIT(22:02)
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-COST`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-STOCK-COST
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(14:07))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING "." INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING WS-MYSQL-EDIT(22:04)
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-VALUE`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-STOCK-VALUE
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(12:09))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING "." INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING WS-MYSQL-EDIT(22:02)
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-ARRIVAL-DATE`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-STOCK-ARRIVAL-DATE
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(11:10))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-ORDER-DUE`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-STOCK-ORDER-DUE
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(11:10))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-ORDER-DAT`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-STOCK-ORDER-DAT
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(11:10))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-ADDS`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-STOCK-ADDS
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(11:10))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-DEDUCTS`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-STOCK-DEDUCTS
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(11:10))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-WIP-ADDS`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-STOCK-WIP-ADDS
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(11:10))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-WIP-DEDS`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-STOCK-WIP-DEDS
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(11:10))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-TD-ADDS-01`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-STOCK-TD-ADDS-01
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(11:10))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-TD-ADDS-02`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-STOCK-TD-ADDS-02
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(11:10))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-TD-ADDS-03`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-STOCK-TD-ADDS-03
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(11:10))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-TD-ADDS-04`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-STOCK-TD-ADDS-04
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(11:10))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-TD-ADDS-05`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-STOCK-TD-ADDS-05
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(11:10))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-TD-ADDS-06`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-STOCK-TD-ADDS-06
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(11:10))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-TD-ADDS-07`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-STOCK-TD-ADDS-07
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(11:10))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-TD-ADDS-08`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-STOCK-TD-ADDS-08
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(11:10))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-TD-ADDS-09`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-STOCK-TD-ADDS-09
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(11:10))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-TD-ADDS-10`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-STOCK-TD-ADDS-10
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(11:10))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-TD-ADDS-11`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-STOCK-TD-ADDS-11
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(11:10))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-TD-ADDS-12`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-STOCK-TD-ADDS-12
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(11:10))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-TD-DEDS-01`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-STOCK-TD-DEDS-01
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(11:10))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-TD-DEDS-02`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-STOCK-TD-DEDS-02
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(11:10))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-TD-DEDS-03`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-STOCK-TD-DEDS-03
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(11:10))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-TD-DEDS-04`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-STOCK-TD-DEDS-04
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(11:10))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-TD-DEDS-05`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-STOCK-TD-DEDS-05
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(11:10))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-TD-DEDS-06`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-STOCK-TD-DEDS-06
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(11:10))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-TD-DEDS-07`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-STOCK-TD-DEDS-07
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(11:10))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-TD-DEDS-08`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-STOCK-TD-DEDS-08
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(11:10))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-TD-DEDS-09`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-STOCK-TD-DEDS-09
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(11:10))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-TD-DEDS-10`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-STOCK-TD-DEDS-10
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(11:10))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-TD-DEDS-11`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-STOCK-TD-DEDS-11
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(11:10))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-TD-DEDS-12`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-STOCK-TD-DEDS-12
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(11:10))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-TD-WIP-ADDS-01`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-STOCK-TD-WIP-ADDS-01
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(11:10))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-TD-WIP-ADDS-02`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-STOCK-TD-WIP-ADDS-02
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(11:10))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-TD-WIP-ADDS-03`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-STOCK-TD-WIP-ADDS-03
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(11:10))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-TD-WIP-ADDS-04`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-STOCK-TD-WIP-ADDS-04
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(11:10))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-TD-WIP-ADDS-05`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-STOCK-TD-WIP-ADDS-05
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(11:10))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-TD-WIP-ADDS-06`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-STOCK-TD-WIP-ADDS-06
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(11:10))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-TD-WIP-ADDS-07`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-STOCK-TD-WIP-ADDS-07
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(11:10))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-TD-WIP-ADDS-08`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-STOCK-TD-WIP-ADDS-08
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(11:10))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-TD-WIP-ADDS-09`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-STOCK-TD-WIP-ADDS-09
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(11:10))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-TD-WIP-ADDS-10`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-STOCK-TD-WIP-ADDS-10
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(11:10))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-TD-WIP-ADDS-11`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-STOCK-TD-WIP-ADDS-11
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(11:10))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-TD-WIP-ADDS-12`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-STOCK-TD-WIP-ADDS-12
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(11:10))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-TD-WIP-DEDS-01`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-STOCK-TD-WIP-DEDS-01
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(11:10))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-TD-WIP-DEDS-02`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-STOCK-TD-WIP-DEDS-02
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(11:10))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-TD-WIP-DEDS-03`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-STOCK-TD-WIP-DEDS-03
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(11:10))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-TD-WIP-DEDS-04`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-STOCK-TD-WIP-DEDS-04
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(11:10))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-TD-WIP-DEDS-05`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-STOCK-TD-WIP-DEDS-05
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(11:10))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-TD-WIP-DEDS-06`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-STOCK-TD-WIP-DEDS-06
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(11:10))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-TD-WIP-DEDS-07`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-STOCK-TD-WIP-DEDS-07
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(11:10))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-TD-WIP-DEDS-08`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-STOCK-TD-WIP-DEDS-08
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(11:10))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-TD-WIP-DEDS-09`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-STOCK-TD-WIP-DEDS-09
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(11:10))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-TD-WIP-DEDS-10`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-STOCK-TD-WIP-DEDS-10
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(11:10))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-TD-WIP-DEDS-11`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-STOCK-TD-WIP-DEDS-11
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(11:10))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`STOCK-TD-WIP-DEDS-12`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-STOCK-TD-WIP-DEDS-12
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(11:10))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
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
end program stockMT.
