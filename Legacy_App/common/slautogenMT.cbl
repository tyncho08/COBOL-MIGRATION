       >>source free
*>***********************************************
*>                                              *
*>               Sales Autogen                  *
*>             Table RDB Handler                *
*>                                              *
*>***********************************************
*>
 identification division.
 Program-Id.            slautogenMT.
*>**
*> Author.              Vincent B Coen, FBCS, FIDM, FIDPM, CPL
*>                      for Applewood Computers.
*>**
*> Security.            Copyright (C) 2023 & later, Vincent Bryan Coen.
*>                      Distributed under the GNU General Public License
*>                      v2.0. Only. See the file COPYING for details.
*>**
*> Remarks.             Sales Autogen File RDB Handler using amended JC preSQL.
*>                      **********************************************
*>           13/06/23   Updated for SAAUTOGEN record layouts differences.
*>
*>                      This version uses an amended JC pre-Sql processor.
*>                      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
*>
*>  This module and that for the FH handles one record at a time for
*>    the header and the invoice lines based on the key last two digits
*>      being 00 or not respectively.
*>
*>===========================================================================
*>
*> Coding completed but needs dry testing and verifying against sales to see
*>  what functions are used and how to confirm correct logic.
*>
*>          Logic to be used:  [ for programmer ]
*>
*>                      THIS module handles two tables - primary and a secondary one
*>                      The primary (SAAUTOGEN-REC) has all columns that directly link
*>                      to the Cobol file Header data and
*>                      SAAUTOGEN-LINES-REC for up to 40 rows for those both using the
*>                      same key for the search. This hold the invoice item lines with one
*>                      row per line. This table only holds this record type.
*>
*>                      Both of these tables consist of the existing Cobol file records.
*>
*>                      During processing for write, rewrite, delete, seek/search operations
*>                      will examine the last two chars of the key (Line or test) and if
*>                      zero its a main table row and if 01 - 40 is a Line item row so
*>                      easy to work out where to write out to.
*>
*>                      FOR start we do for both tables saving into -RG1.
*>                         See if we need this code  ????????
*>
*>                      Reading both next and indexed is a little more complex :
*>
*>                      Read next needs to know what the previous record key (test) was so if
*>                      it is:
*>                      If 00 then read line table for same invoice starting at 01  and at
*>                      invoice change read primary table.
*>
*>                      if not 00 then read line table unless invoice changed then
*>                      change to primary read.
*>
*>                      If read indexed if test = 00 use primary otherwise use
*>                      line table.
*>
*>                      So we need to keep track of previous read key and operation ?
*>                      and the prior key (JIC).
*>
*>===========================================================================
*>
*>                      This and associated modules relate to ACAS versions v3.02 and later.
*>
*>                      This modules does all file handling using RDB and is called
*>                      from the ACAS
*>                      Sales Autogen file handler acas004
*>                      which in turn is called
*>                      from each ACAS application module that requires file/RDB data handling.
*>
*>                      If RDBMS (Relational Database Management Systems) is in use it will be
*>                      called by the specific module to handle similar processing as a Cobol
*>                      flat file and will pass the equivalent
*>                      RDB (Relational Database) row as a Cobol file record (01 level) moving
*>                      row by row to the correct Cobol flat file fields as required.
*>
*>                      RDB DAL (Data Access Layer) modules are individually modified to handle:
*>                      MS SQL server, Mysql, DB2, Postgres and Oracle as available and tested.
*>                      These are contained in separate directories for each RDB, e.g.
*>                       'MSSQL' (MS SQL Server), 'Mysql', 'DB2', 'Postgres'. 'Oracle'.
*>                       You need to compile from the correct directory for the specific
*>                       RDB you will use and have installed along with all of the development
*>                       libraries and include files etc using the correct pre-compiler tool
*>                       to process the Exec SQL code converting to Cobol calls etc.
*>                       see the RDB specific ACAS notes.
*>
*>                      For specific SQL servers supported, the pre-compiler system is included
*>                       where ever possible but for some, copyright reasons may prevent
*>                       inclusion. In some cases for one specific RDB more than one precompiler
*>                       is used as an experiment to help find the ideal one to use.
*>
*>                      If you wish to convert a running ACAS system over from Flat files
*>                      to RDBMS see below.
*>
*>                      Depending on the RDB you wish to use there is
*>                      also, included LMs (Load Modules) to convert each ISAM
*>                      (Indexed Sequential) file to the rdb database tables.
*>                      These will also need to be compiled from the
*>                      specific LM directory that contains the rdb DAL modules.
*>                      These will be very RDB specific.
*>
*>    As the MySQL versioning is the first of the DAL and FH processes being
*>    developed all sources are in the common directory.
*>**
*>  File Handling:
*>     Uses the Global Error / Access logging file within the FH acas0nn module
*>               and in all DALs.
*>**
*> Called by Modules:
*>                      acas004 - Sales Autogen Cobol Handler.
*>
*>**
*> Error Messages Used.
*>                      SM004 SQL Err no in 'mysql-procedures'
*>                      SM901 Note error and hit return.
*>**
*> Version.             1.00 14/05/2023.
*>
*>**
*> Changes.
*> 14/05/23 vbc - .00 Code taken from SLinvoiceMT and changelog cleared down
*> 12/07/23 vbc - .01 In bc050-Process-Read and bc300-Update-Rg1 changed to set KOR-x1 to 2
*>                    from 'to 1'. Ditto in plinvoiceMT, slinvoiceMt, plautogenMT, slautogenMT.
*> 26/07/23 vbc - .02 In bc050-Process-Read and bc300-Update-Rg1 changed to set KOR-x1 to 2
*>                    from 'to 1'. Ditto plinvoiceMT & *autogenMT - in progress - did say
*>                    that these have only been dry tested.......
*>                    RG1 processing missing for (re)writes also need to check that load
*>                    and unload is present - ditto for header records.
*>                    New proc - bc090-Process-Rewrite used in ba090-Process-Rewrite
*>                    fix bug where data NOT moved to/from ws-invoice-record
*>                    and WS-Invoice-Line - I hope.
*>                    Remove setup code in bc300-Update-rg1 as in caller.
*>                    Amendments taken from slinvoiceMT.scb with changes.
*> 08/08/23 vbc - .03 Table keys NOT being moved to for both tables but not from - stops overwrite keys.
*> 16/04/24 vbc       Copyright notice update superseding all previous notices.
*>**
*>  Module USAGE
*>**************
*>
*>    On Entry:         Set up Linkage areas -
*>    ********              WS-Invoice-Record = Contents of data record to be written/read
*>                          File-Access = File-Function as needed.
*>                                        Access-Type   as needed.
*>                          File-Defs (File-Definitions) = Paths set up.
*>
*>    On Exit:          Linkage contains:
*>    *******               Record = containing a read data record or table row for both tables.
*>                          Fs-Reply = 0, 99 or other value where
*>                                     0  = Operation completed successfully
*>                                     10 = End of (Cobol) File returned to calling module only.
*>                                     21 = Invalid key on START OR key not found
*>                                     22 - Attempt to duplicate a key value.
*>                                     23 = Key not found.     from read indexed
*>                                     99 = Indicates an error see WE-Error, SQL-ERR/MSG & SQL-State for more info
*>                          WE-Error   0    = Operation completed successfully
*>
*>                                     999  = Not used here - Yet.
*>                                     998* = File-Key-No Out Of Range not 1, 2 or 3.
*>                                     997* = Access-Type wrong (< 5 or > 8)
*>                                     996* = File Delete key out of range (not = 1 or 2)
*>                                     995* = During Delete SQLSTATE not '00000' investigate using MSG-Err/Msg
*>                                            or row count not > 0
*>                                     994* = During Rewrite,                     ^^ see above ^^
*>                                     992* = Invalid Function requested in File-Function
*>                                     990* = Unknown and unexpected error, again ^^ see above ^^
*>                                     989* = Unexpected error on Read-Indexed, investigate as above.
*>                                     911* = Rdb Error during initializing,
*>                                            possibly can not connect to database
*>                                             Check connect data and
*>                                             see SQL-Err & SQL-MSG
*>                                     910* = Table locked > 5 seconds
*>                                     901  = File Def Record size not =< than ws record size
*>                                            Module needs ws definition changing to correct size
*>                                            FATAL, Stop using system, fix source code
*>                                            and recompile before using system again.
*>                                     8nn  = Processing on RG Table & rows.
*>                                     890  = Unknown and unexpected error, again ^^ see above on RG processing
*>                                     880  = Unexpected range error in Rg1 secondary key.
*>                                            Report to programming team.
*>                                     Other = any other rdbms errors see specific
*>                                             (Rdbms) manual
*>                          SQL-Err   = Error code from RDBMS is set if above 2 are non zero
*>                          SQL-Msg   = Non space providing more info if SQL-Err non '00000'
*>                                      * = FS-Reply = 99.
*>                          SQL-State = In support of SQL-Err for dup keys etc but could be used for other errors.
*>
*>       During testing a log file will be created containing datetime stamp, task and return codes
*>       for FS-Reply, WE-Error & SQL-State with table used - in this case the Payments File.
*>       WARNING - This file could get large so needs manually deleting after examination.
*>
*>       NOTE that the value in SQL-State is the standard ANSI RDBMS error code to help keep
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
 copy "envdiv.cob".
*>
 input-output section.
 Data Division.
 Working-Storage Section.
 77  prog-name           pic x(21)    value "slautogenMT (3.02.03)".
*>
*> JC WS requirements here
*>
 77  ws-Where           pic x(512).
 77  ws-Where-2         pic x(512).    *> RG1
*>
*>  Used within presql generated code
*>
 01  WS-Reply           pic x           value space.
 01  WS-MYSQL-I         PIC S9(4) COMP.
 01  WS-MYSQL-EDIT      PIC -Z(18)9.9(9).
*>
 *> TESING data
 01  WS-Temp-ED-Row         pic 9(7).  *> for row cnt. 10M-1 Inv. records
*>
*>  This will hold the last key read so we can know if the next request
*>    is for a body row or not.
*>      we will need to also hold the value from WS-Sih-Lines in
*>       the invoice table - stored in WS-Actual-Lines-In-Row.
*>
*>     Not used for writing but could be for deletes.
*>
 01  WS-Last-Read-Key.
     03  WS-Last-Read-Invoice pic 9(8)    value zero.
     03  WS-Last-Read-Line    pic 99      value 40.
 01  WS-Actual-Lines-In-Row   pic 99      value zero.
*>
*> The communication area for the MySQL database changed for free/mysql
*>
*>  jc preSQL MySQL stuff ends
*>
*> Metadata on primary keys...   for SAAUTOGEN-REC
*>
 01  Table-Of-KeyNames.
     03  filler         pic x(30) value 'SINVOICE-KEY                  '.   *> In SAAUTOGEN-REC
     03  filler         pic x(8)  value '00010010'.  *> offset/length
     03  filler         pic x(3)  value 'STR'.       *> data type
 *>
     03  filler         pic x(30) value 'IL-LINE-KEY'.                      *> In SAAUTOGEN-LINES-REC
     03  filler         pic x(8)  value '00010010'.  *> offset/length
     03  filler         pic x(3)  value 'STR'.       *> data type
 01  filler redefines Table-Of-KeyNames.
     03  KeyOfReference occurs 2
                             indexed by KOR-x1.
         05  keyname    pic x(30).
         05  KOR-offset pic 9(4).
         05  KOR-length pic 9(4).
         05  KOR-Type   pic XXX.                    *> Not used currently
*>
*>  Start of RG (Repeat Groups)                        NOT USED - YET.
*> Metadata on Repeating Groups...                     but acts as a reminder
*>
 01  RG-Table.
     03  filler  pic x(30) value 'SAAUTOGEN-LINES-REC'. *> table name
     03  filler  pic s9(9) comp-5 value 40.         *> Max row count
     03  filler  pic s9(9) comp-5 value zero.       *> current row
*>                                                     Repeats as needed = None.
 01  filler redefines RG-Table.
     03  RG-Entry          occurs 1
                          indexed by RG-x1.
         05 RG-recname    pic x(30).
         05 RG-maxoccurs  pic s9(9) comp-5.
         05 RG-noloaded   pic s9(9) comp-5.        *> Number loaded.
*>
 01  DAL-Data.
         05  MOST-Relation   pic xxx.                  *> valid are >=, <=, <, >, =
         05  Most-Cursor-Set pic 9    value zero.
             88  Cursor-Not-Active    value zero.
             88  Cursor-Active        value 1.
         05  Most-Cursor-Set-2 pic 9  value zero.      *> RG 1
             88  Cursor-Not-Active-2  value zero.
             88  Cursor-Active-2      value 1.
*>
*>  Variables common to all DALs
*>  ****************************
*>
 01  subscripts usage comp-5.
     12 J                    pic s9(4).
     12 K                    pic s9(4).
     12 L                    pic s9(4).
     12 J2                   pic s9(4).
     12 K2                   pic s9(4).
     12 L2                   pic s9(4).
     12 M                    pic s9(4).
*>
 01  WS-Body-Key             pic x(9).  *> NOT USED
*>
 01  work-fields.
     03  ws-env-lines    pic 999              value zero.
     03  ws-lines        binary-char unsigned value zero.
     03  ws-22-lines     binary-char unsigned value zero.
     03  ws-23-lines     binary-char unsigned value zero.
     03  ws-98-lines     binary-char unsigned value zero.
     03  ws-99-lines     binary-char unsigned value zero.
*>
 01  Error-Messages.
     03  SM901          pic x(31) value "SM901 Note error and hit return".
*>
 01  WS-Invoice-Line.
     03  WS-Sil-Key.                     *> 10 bytes
         05  WS-Sil-Invoice pic 9(8).
         05  WS-Sil-Line    pic 99.
     03  WS-Sil-Product     pic x(13).   *> +1 17/5/13
     03  WS-Sil-Pa          pic xx.
     03  WS-Sil-Qty         binary-short.
     03  WS-Sil-Type        pic x.
     03  WS-Sil-Description pic x(32).   *> +8 17/5/13
     03  WS-Sil-Net         pic s9(7)v99   comp-3.
     03  WS-Sil-Unit        pic s9(7)v99   comp-3.
     03  WS-Sil-Discount    pic 99v99      comp.
     03  WS-Sil-Vat         pic s9(7)v99   comp-3.
     03  WS-Sil-Vat-Code    pic 9.
     03  WS-Sil-Update      pic x.
         88 WS-Sil-Analyised                       value "Z".
     03  filler             pic x.          *> size 80 to here -1 26/7/23 2 match slwsinv
*>
*> Load up rec layouts from the database for MySQL
*>
*> /MYSQL VAR\
*>       ACASDB
*>       TABLE=SAAUTOGEN-REC,HV
*>       TABLE=SAAUTOGEN-LINES-REC,HV1
 COPY "mysql-variables.cpy".
*>
*>    Definitions for the SAAUTOGEN-REC Table
*>
       01  TP-SAAUTOGEN-REC                      USAGE POINTER.
       01  TD-SAAUTOGEN-REC.
           05  HV-SINVOICE-KEY                   PIC X(10).
           05  HV-IH-INVOICE                     PIC  9(10) COMP.
           05  HV-IH-TEST                        PIC  9(03) COMP.
           05  HV-IH-CUSTOMER                    PIC X(7).
           05  HV-IH-DAT                         PIC  9(10) COMP.
           05  HV-IH-FREQ                        PIC X(1).
           05  HV-IH-REPEAT                      PIC S9(02) COMP.
           05  HV-IH-LAST-DATE                   PIC  9(10) COMP.
           05  HV-IH-TYPE                        PIC  9(03) COMP.
           05  HV-IH-REF                         PIC X(10).
           05  HV-IH-DESCRIPTION                 PIC X(32).
           05  HV-IH-P-C                         PIC S9(07)V9(02) COMP.
           05  HV-IH-NET                         PIC S9(07)V9(02) COMP.
           05  HV-IH-EXTRA                       PIC S9(07)V9(02) COMP.
           05  HV-IH-CARRIAGE                    PIC S9(07)V9(02) COMP.
           05  HV-IH-VAT                         PIC S9(07)V9(02) COMP.
           05  HV-IH-DISCOUNT                    PIC S9(07)V9(02) COMP.
           05  HV-IH-E-VAT                       PIC S9(07)V9(02) COMP.
           05  HV-IH-C-VAT                       PIC S9(07)V9(02) COMP.
           05  HV-IH-STATUS                      PIC X(1).
           05  HV-IH-STATUS-P                    PIC X(1).
           05  HV-IH-STATUS-L                    PIC X(1).
           05  HV-IH-STATUS-C                    PIC X(1).
           05  HV-IH-STATUS-A                    PIC X(1).
           05  HV-IH-STATUS-I                    PIC X(1).
           05  HV-IH-DEDUCT-DAYS                 PIC  9(03) COMP.
           05  HV-IH-DEDUCT-AMT                  PIC  9(03)V9(02) COMP.
           05  HV-IH-DEDUCT-VAT                  PIC  9(03)V9(02) COMP.
           05  HV-IH-DAYS                        PIC  9(03) COMP.
           05  HV-IH-CR                          PIC  9(10) COMP.
           05  HV-IH-LINES                       PIC  9(03) COMP.
           05  HV-IH-DAY-BOOK-FLAG               PIC X(1).
           05  HV-IH-UPDATE                      PIC X(1).
*>
*>    Definitions for the SAAUTOGEN-LINES-REC Table
*>
       01  TP-SAAUTOGEN-LINES-REC                USAGE POINTER.
       01  TD-SAAUTOGEN-LINES-REC.
           05  HV1-IL-LINE-KEY                   PIC X(10).
           05  HV1-IL-INVOICE                    PIC  9(10) COMP.
           05  HV1-IL-LINE                       PIC  9(03) COMP.
           05  HV1-IL-PRODUCT                    PIC X(13).
           05  HV1-IL-PA                         PIC X(2).
           05  HV1-IL-QTY                        PIC  9(05) COMP.
           05  HV1-IL-TYPE                       PIC X(1).
           05  HV1-IL-DESCRIPTION                PIC X(32).
           05  HV1-IL-NET                        PIC S9(07)V9(02) COMP.
           05  HV1-IL-UNIT                       PIC S9(07)V9(02) COMP.
           05  HV1-IL-DISCOUNT                   PIC  9(02)V9(02) COMP.
           05  HV1-IL-VAT                        PIC S9(07)V9(02) COMP.
           05  HV1-IL-VAT-CODE                   PIC  9(03) COMP.
           05  HV1-IL-UPDATE                     PIC X(1).
*> /MYSQL-END\
*>
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
*>  Using the first record but not the 2nd as it uses occurs 40 but
*>   to reduce Ram usage get rid of the occurs.
*>
 copy "slwsinv.cob"   replacing SInvoice-Header    by WS-Invoice-Record
                                 leading ==sih-==  by ==WS-Sih-==
                                  ==occurs 40.==   by ==.==
                                  ==sil-==         by ==Un-Used-Sil-==.
*>
 screen section.
*>=============
*>
 01  Display-Message-1       foreground-color 2.
     03          value "WS-Where="               line 23 col  1.
     03  from WS-Where (1:J)           pic x(69)         col 10.
*>
 01  Display-Message-2       foreground-color 2.
     03      value "SM004 SQL Err No.="           line 4 col  1.
     03  using Ws-Mysql-Error-Number   pic x(4)          col 19.
     03      value " Para="                              col 23.
     03  using WS-No-Paragraph         pic 9(3)          col 29.
     03      value " SQL Cmd="                           col 32.
     03  using Ws-Mysql-Command        pic x(199)        col 41.
     03      value "SQL Err Msg="                 line 7 col  1.
     03  using Ws-Mysql-Error-Message  pic x(67)         col 13.
*>
*>
 PROCEDURE DIVISION   using File-Access
                            ACAS-DAL-Common-data
                            WS-Invoice-Record.   *>  Ws record
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
     add      2 to ws-lines giving ws-99-lines.
     add      1 to ws-lines giving ws-98-lines.
*>
 ba010-Initialise.
*>
     move     zero   to SQL-State.
*>
*>                        We-Error
*>                        Fs-Reply.
*>
     move     spaces to WS-MYSQL-Error-Message
                        WS-MYSQL-Error-Number
                        WS-Log-Where
                        WS-File-Key
                        SQL-Msg
                        SQL-Err.
*>
*>  Work out what is being requested and convert to action!!
*>
*>    This version uses the JC pre-Sql processor.
*>    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
*>
     evaluate File-Function
        when  1
              go to ba020-Process-Open          *> Coded / D.tested
        when  2
              go to ba030-Process-Close         *> Coded / D.tested
        when  3
        when  34                                *> Read-Next-Header - Special
              go to ba040-Process-Read-Next     *>         cursor active
        when  4
              go to ba050-Process-Read-Indexed
        when  5
              go to ba070-Process-Write         *> Coded / D.tested
*>
*> option 6 is a special to cleardown all LINE data for 1 invoice
*>
        when  6
              go to ba085-Process-Delete-All    *> Coded / D.Tested --- DELETE-ALL  Special
        when  7
              go to ba090-Process-Rewrite
        when  8
              go to ba080-Process-Delete
        when  9
              go to ba060-Process-Start         *> Coded / D.tested -> read-next ?
        when  other
              go to ba100-Bad-Function
     end-evaluate.
*>
*> Paragraphs used for RDB and log file
*>
*>  ba020-Process-Open         - P = 1
*>  ba030-Process-Close        - P = 2
*>  ba040-Process-Read-Next    - P = 3
*>  ba041-Reread               - P = 4
*>  ba050-Process-Read-Indexed - P = 5  SELECT
*>  ba050-Process-Read-Indexed - P = 6  FETCH
*>  ba060-Process-Start        - P = 8  SELECT  Invoice HEAD
*>  ba060-Process-Start        - P = 57 SELECT  Invoice LINE
*>  ba070-Process-Write        - P = 10 bb200-insert    HEAD
*>  ba080-Process-Delete       - P = 13 DELETE          HEAD
*>  ba085-Process-Delete-All   - P = 15 DELETE          HEAD
*>  ba090-Process-Rewrite      - P = 17 UPDATE          HEAD
*>  ba998-Free                 - P = 20 FREE
*>  bc050-Process-Read-Indexed - P = 51 SELECT          LINES
*>                                   52 FETCH           --
*>  bc070-Process-Write        - P = 53 INSERT          --
*>                                   56 UPDATE          --
*>  bc080-Process-Delete       - P = 54 DELETE          --
*>  bc085-Process-Delete-All   - P = 55 DELETE          --
*>  bc090-Process-Rewrite      - P = 56 UPDATE          --
*>
 ba020-Process-Open.             *> dry tested - no rg01 requirements.
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
     move    "OPEN SL AUTOGEN" to WS-File-Key
     set     Cursor-Not-Active to true
     go      to ba999-end.
*>
 ba030-Process-Close.             *> dry tested - no rg01 requirements.
     if      Cursor-Active
             perform ba998-Free.
*>
     move     2 to ws-No-Paragraph.
     move    "CLOSE SL AUTOGEN" to WS-File-Key.
*>  /MYSQL CLOSE\
*>
*>    Close the Database
*>
           PERFORM MYSQL-1980-CLOSE THRU MYSQL-1999-EXIT
*>  /MYSQL-END\
     go      to ba999-end.
*>
 ba040-Process-Read-Next.             *> dry test.. - Has rg01 requirements.
*>
*>  Here we have for a given key first read and transfer a inv header
*>   then pass on for the same key all body lines all one at
*>     a time. This will mean that the DAL has to keep track of the
*>       original request. so lets look at for any given invoice read in
*>           then read body into WS and pass back.
*>
*>   Here a SELECT first then fetch if no cursor active using lowest
*>    possible key of "0000000000"
*>           [ SINVOICE-KEY ]
*>
     if       Cursor-Not-Active
              set      KOR-x1 to 1                *> 1 = Primary
              move     KOR-offset (KOR-x1) to K
              move     KOR-length (KOR-x1) to L
*>
              move     spaces to WS-Where
              move     1   to J
              string   "`"                   delimited by size
                       KeyName (KOR-x1)      delimited by space
                       "`"                   delimited by size
                       " >= "                delimited by size
                       '"0000000000"'        delimited by size  *> for 1st time active only
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
*>                    TABLE=SAAUTOGEN-REC
           INITIALIZE WS-MYSQL-COMMAND
           STRING "SELECT * FROM "
             "`SAAUTOGEN-REC`"
             " WHERE "
             ws-Where (1:J)
            ";"  X"00" INTO WS-MYSQL-COMMAND
           PERFORM MYSQL-1210-COMMAND THRU MYSQL-1219-EXIT
           PERFORM MYSQL-1220-STORE-RESULT THRU MYSQL-1239-EXIT
           MOVE WS-MYSQL-RESULT TO TP-SAAUTOGEN-REC
*>               /MYSQL-END\
           move    "0000000000" to WS-File-Key
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
                       move 10 to fs-reply
                       move 10 to WE-Error        *> should be 0 'JIC' likewise the others
                       move    "No Data" to WS-File-Key
                       go to ba999-End       *> can clear the dup code after testing
              end-if                         *> We have data
              set      Cursor-Active to true
              move     WS-MYSQL-Count-Rows to WS-Temp-ED-Row
              move    spaces to WS-File-Key
              string   "> 0 got cnt=" delimited by size
                       WS-Temp-ED-Row delimited by size
                       " recs for INVOICE-REC Table"
                        into WS-File-Key
              end-string
              perform ba999-End                         *> log it
     end-if.
*>
 ba041-Reread.
*>
*>  If here cursor is set (even from start), so get the next row
*>
     move     spaces to WS-Log-Where.
     move     4 to ws-No-Paragraph.
     move     zero to return-code.
*>
*>  First check if a prev read has occured
*>
     if       WS-Last-Read-Invoice = zero      *> DALS Not yet called
              go to ba042-Fetch.               *>  so get header rec.
*>
*> Check for status of body-line counts etc & get the next row in sequence within invoice.
*>  but 1st support for Read-Next-Header to bypass line processing.
*>
     if       FN-Read-Next-Header
              go to ba042-Fetch.
*>
     if       WS-Last-Read-Line < WS-Actual-Lines-In-Row
              add 1 WS-Last-Read-Line giving   WS-Sih-Test
              move  WS-Last-Read-Invoice    to WS-Sih-Invoice
              perform bc050-Process-Read-Indexed thru bc059-Exit
              if     fs-Reply not = zero    *> = 23    *> no row found
                     initialise WS-Invoice-Record
                     move spaces to WS-File-Key
                     string WS-Invoice-Key
                            " Not Found"
                                 into WS-File-Key
                     end-string
                     go to ba999-End        *> This should NOT happen
              else     *> got a row
                     move WS-Sih-Test to WS-Last-Read-Line
                     go to ba999-End
              end-if
     end-if.
*>
*> Not, so get next invoice Header rec
*>
 ba042-Fetch.
*>      /MYSQL FETCH\
*>
*>    Fetch next record
*>
*>             TABLE=SAAUTOGEN-REC
           MOVE TP-SAAUTOGEN-REC TO WS-MYSQL-RESULT
           CALL "MySQL_fetch_record" USING WS-MYSQL-RESULT
                    HV-SINVOICE-KEY
                    HV-IH-INVOICE
                    HV-IH-TEST
                    HV-IH-CUSTOMER
                    HV-IH-DAT
                    HV-IH-FREQ
                    HV-IH-REPEAT
                    HV-IH-LAST-DATE
                    HV-IH-TYPE
                    HV-IH-REF
                    HV-IH-DESCRIPTION
                    HV-IH-P-C
                    HV-IH-NET
                    HV-IH-EXTRA
                    HV-IH-CARRIAGE
                    HV-IH-VAT
                    HV-IH-DISCOUNT
                    HV-IH-E-VAT
                    HV-IH-C-VAT
                    HV-IH-STATUS
                    HV-IH-STATUS-P
                    HV-IH-STATUS-L
                    HV-IH-STATUS-C
                    HV-IH-STATUS-A
                    HV-IH-STATUS-I
                    HV-IH-DEDUCT-DAYS
                    HV-IH-DEDUCT-AMT
                    HV-IH-DEDUCT-VAT
                    HV-IH-DAYS
                    HV-IH-CR
                    HV-IH-LINES
                    HV-IH-DAY-BOOK-FLAG
                    HV-IH-UPDATE

*>      /MYSQL-END\
     end-call
*>
     if       return-code = -1     *> no more data so free cursor & return
              move 10 to fs-Reply
                         WE-Error      *> should be 0 likewise the others
              move    "EOF" to WS-File-Key
              set Cursor-Not-Active to true
              go to ba999-End
     end-if
*>
     if       WS-MYSQL-Count-Rows = zero   *> no data but should not happen here
              call  "MySQL_errno" using WS-MYSQL-Error-Number
              call  "MySQL_sqlstate" using WS-MYSQL-SQLstate
              move  WS-MYSQL-SqlState   to SQL-State
              if    WS-MYSQL-Error-Number  not = "0  "
                    call "MySQL_error" using WS-MYSQL-Error-Message
                    move WS-MYSQL-Error-Number  to SQL-Err   *> Not really needed but help for testing
                    move WS-MYSQL-Error-Message to SQL-Msg   *>  ditto
                    initialize WS-Invoice-Record with filler
                   move    "EOF2" to WS-File-Key
              end-if
              move 10 to fs-reply                  *> EOF equivilent !!
              move 10 to WE-Error
              set Cursor-Not-Active to true
              go to ba999-End
     end-if.
*>
     if       fs-reply = 10                 *> should not happen as tested prior
              set Cursor-Not-Active to true
              move    "EOF3" to WS-File-Key
              go to ba999-End
     end-if.
*>
     perform  bb100-UnloadHVs.       *> transfer/move HV vars to Record layout
     move     WS-Invoice-Key to WS-File-Key.
     move     zero to fs-reply WE-Error.
     go       to ba999-End.      *> exit.
*>
 ba050-Process-Read-Indexed.             *> dry test.. - Has rg01 requirements.
*>
*>  Test what the key end is (Line or test) & if not zero
*>   we do RG1 insead of primary table
*>
     if       WS-Sih-test not = zero     *> if true  process line RG
              perform  bc050-Process-Read-Indexed  thru bc059-Exit
              go       to ba999-exit.
*>
*>  Now do on correct key within WHERE
*>  Sets up key and compare data
*>
     set      KOR-x1 to 1.      *> 1 = Primary
     move     KOR-offset (KOR-x1) to K
     move     KOR-length (KOR-x1) to L
*>
     move     spaces to WS-Where
     move     1   to J
     string   "`"                   delimited by size
              KeyName (KOR-x1)      delimited by space
              "`"                   delimited by size
              '="'                  delimited by size
              WS-Invoice-Record (K:L)       delimited by size
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
*>             TABLE=SAAUTOGEN-REC
           INITIALIZE WS-MYSQL-COMMAND
           STRING "SELECT * FROM "
             "`SAAUTOGEN-REC`"
             " WHERE "
             WS-Where (1:J)
            ";"  X"00" INTO WS-MYSQL-COMMAND
           PERFORM MYSQL-1210-COMMAND THRU MYSQL-1219-EXIT
           PERFORM MYSQL-1220-STORE-RESULT THRU MYSQL-1239-EXIT
           MOVE WS-MYSQL-RESULT TO TP-SAAUTOGEN-REC
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
*>             TABLE=SAAUTOGEN-REC
           MOVE TP-SAAUTOGEN-REC TO WS-MYSQL-RESULT
           CALL "MySQL_fetch_record" USING WS-MYSQL-RESULT
                    HV-SINVOICE-KEY
                    HV-IH-INVOICE
                    HV-IH-TEST
                    HV-IH-CUSTOMER
                    HV-IH-DAT
                    HV-IH-FREQ
                    HV-IH-REPEAT
                    HV-IH-LAST-DATE
                    HV-IH-TYPE
                    HV-IH-REF
                    HV-IH-DESCRIPTION
                    HV-IH-P-C
                    HV-IH-NET
                    HV-IH-EXTRA
                    HV-IH-CARRIAGE
                    HV-IH-VAT
                    HV-IH-DISCOUNT
                    HV-IH-E-VAT
                    HV-IH-C-VAT
                    HV-IH-STATUS
                    HV-IH-STATUS-P
                    HV-IH-STATUS-L
                    HV-IH-STATUS-C
                    HV-IH-STATUS-A
                    HV-IH-STATUS-I
                    HV-IH-DEDUCT-DAYS
                    HV-IH-DEDUCT-AMT
                    HV-IH-DEDUCT-VAT
                    HV-IH-DAYS
                    HV-IH-CR
                    HV-IH-LINES
                    HV-IH-DAY-BOOK-FLAG
                    HV-IH-UPDATE

*>      /MYSQL-END\
     end-call
*>
     if       WS-MYSQL-Count-Rows not > zero
              call  "MySQL_errno" using WS-MYSQL-Error-Number
              call  "MySQL_sqlstate" using WS-MYSQL-SQLstate
              move  WS-MYSQL-SqlState   to SQL-State
              if    WS-MYSQL-Error-Number  not = "0  "
                    move 990 to WE-Error
                    call "MySQL_error" using WS-MYSQL-Error-Message
                    move WS-MYSQL-Error-Number to SQL-Err
                    move WS-MYSQL-Error-Message to SQL-Msg
              else
                    move 989  to WE-Error
                    move zero to SQL-Err
                    move spaces to SQL-Msg
              end-if
              move 23   to fs-reply
              move spaces to WS-File-Key
              go to ba998-Free
     end-if                        *> row count zero should show up as a MYSQL error ?
     perform bb100-UnloadHVs       *> transfer/move HV vars to ws-Record layout
     move     HV-SINVOICE-KEY to WS-File-Key.
     perform ba999-End.
*>
     move     zero to FS-Reply WE-Error.
     perform  ba998-Free.                       *> Free cursor
     go       to ba999-Exit.
*>
 ba060-Process-Start.            *>  coded for header & lines [ NEEDED ? check all calling code ].
*>                                   Need to see if bcnn-Read-Next is needed
*>                                   if not than rg1 processing can be removed.
*>
*>  Check for Param error 1st on start
*>
     if       access-type < 5 or > 8                   *> not using not < or not >
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
*>   Header Records First.
*>
*>  Now do Start on correct key before read-next  within WHERE
*>  Set up MOST-relation for condition test and key on Header table.
*>
     set      KOR-x1 to 1.
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
              when  9                           *> fn-not-greater-than [ not currently used in ACAS ]
                    move "<= " to MOST-Relation
     end-evaluate
*>
     move     spaces to WS-Where
     string   "`"                   delimited by size
              KeyName (KOR-x1)      delimited by space
              "`"                   delimited by size
              MOST-relation         delimited by space
              '"'                   delimited by size
              WS-Invoice-Record (K:L)       delimited by size
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
     move     WS-Invoice-Record (K:L) to WS-File-Key
     if    Testing-2
           display Display-Message-1 with erase eos
     end-if
*>
     move     8 to ws-No-Paragraph
*>      /MYSQL SELECT\
*>
*>    Select rows
*>
*>             TABLE=SAAUTOGEN-REC
           INITIALIZE WS-MYSQL-COMMAND
           STRING "SELECT * FROM "
             "`SAAUTOGEN-REC`"
             " WHERE "
             WS-Where (1:J)
            ";"  X"00" INTO WS-MYSQL-COMMAND
           PERFORM MYSQL-1210-COMMAND THRU MYSQL-1219-EXIT
           PERFORM MYSQL-1220-STORE-RESULT THRU MYSQL-1239-EXIT
           MOVE WS-MYSQL-RESULT TO TP-SAAUTOGEN-REC
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
              end-if
              move 21 to fs-reply                  *> this may need changing for val in WE-Error!!
              move zero to WE-Error
     else
              move  zero to FS-Reply WE-Error
              move     WS-MYSQL-Count-Rows to WS-Temp-ED-Row
              string   MOST-relation
                       WS-Invoice-Record (K:L)
                       " got " delimited by size
                       WS-Temp-ED-Row delimited by size
                       " recs"
                        into WS-File-Key
              end-string
     end-if
     perform  ba999-End.
*>
*>  Extra code for RG1 table -- Line Body Items - ASSUMING here so ?
*>
     set      KOR-x1 to 2.
     move     KOR-offset (KOR-x1) to K2.
     move     KOR-length (KOR-x1) to L2.
     move     1   to J2.
     move     spaces to WS-Where-2.
     string   "`"                   delimited by size
              KeyName (KOR-x1)      delimited by space
              "`"                   delimited by size
              MOST-relation         delimited by space
              WS-Invoice-Record (K2:L2)       delimited by size
              ' ORDER BY '          delimited by size
              "`"                   delimited by size
              keyname (KOR-x1)      delimited by space
              "`"                   delimited by size
                ' ASC  '            delimited by size
                             into WS-Where-2
                             with pointer J2
     end-string
*>
*>    Save the pointers (SAAUTOGEN-REC) [ and we will restore at end ]
*>
     move     WS-Mysql-Result     to WS-Mysql-Save-Result.     *> primary Tbl
     move     WS-Mysql-Count-Rows to WS-Mysql-Save-Count-Rows.
     move     zero to WS-Mysql-Count-Rows.
*>
     move     57 to ws-No-Paragraph
*>      /MYSQL SELECT\
*>
*>    Select rows
*>
*>             TABLE=SAAUTOGEN-LINES-REC
           INITIALIZE WS-MYSQL-COMMAND
           STRING "SELECT * FROM "
             "`SAAUTOGEN-LINES-REC`"
             " WHERE "
             WS-Where-2 (1:J2)
            ";"  X"00" INTO WS-MYSQL-COMMAND
           PERFORM MYSQL-1210-COMMAND THRU MYSQL-1219-EXIT
           PERFORM MYSQL-1220-STORE-RESULT THRU MYSQL-1239-EXIT
           MOVE WS-MYSQL-RESULT TO TP-SAAUTOGEN-LINES-REC
*>      /MYSQL-END\
*>
     if       WS-MYSQL-Count-Rows not zero
              set Cursor-Active-2 to true
     else
              set Cursor-Not-Active-2 to true
     end-if
*>
*> Should not be zero for invoice Line table if still unposted and cleared.
*>
     if       WS-MYSQL-Count-Rows = zero
              call  "MySQL_errno" using WS-MYSQL-Error-Number
              call  "MySQL_sqlstate" using WS-MYSQL-SQLstate
              move  WS-MYSQL-SqlState   to SQL-State
              if    WS-MYSQL-Error-Number  not = "0  "
                    call "MySQL_error" using WS-MYSQL-Error-Message
                    move WS-MYSQL-Error-Number  to SQL-Err
                    move WS-MYSQL-Error-Message to SQL-Msg
              end-if
              move 21 to fs-reply                  *> this may need changing for val in WE-Error!!
              move zero to WE-Error
     else
              move  zero to FS-Reply WE-Error
              move     WS-MYSQL-Count-Rows to WS-Temp-ED-Row
              string   MOST-relation
                       WS-Invoice-Record (K2:L2)
                       " got " delimited by size
                       WS-Temp-ED-Row delimited by size
                       " recs RG1 (Lines)"
                        into WS-File-Key
              end-string
     end-if
*>
*>  Save pointer and count for RG1 also Cursor2 active (or not )
*>
*>  ALL this does depend on application code  in slnnn ???? <<<<<<<
*>
     move     WS-Mysql-Result to WS-Mysql-Save-Result-RG1.    *> RG 1 Tb2
     move     WS-Mysql-Count-Rows to WS-Mysql-Save-Count-Rows-RG1.
*>
*> Restore the Primary table pointer & row count.
*>
     move     WS-Mysql-Save-Result to WS-Mysql-Result.     *> primary Tbl
     move     WS-Mysql-Save-Count-Rows to WS-Mysql-Count-Rows.
*>
     go       to ba999-end.
*>
*>  Now a read next will process a Fetch from cursor ( or cursor-2 )
*>
 ba070-Process-Write.             *> dry test... - Has rg01 requirements.
*>
*>  Are we being requested writing a body line (rg01) ? lets check
*>    but rec still in WS-Invoice-Record
*>
     if       WS-Sih-Test not = zero               *> It is a line row
              perform  bc070-Process-Write thru bc070-Exit
              go to ba999-Exit.                    *> logging rec done.
*>
*>  Nope its a Invoice Header
*>
     perform  bb000-HV-Load.                       *>  move WS-Invoice-Record fields to HV fields
     move     WS-Invoice-Key to WS-File-Key.
     move     zero to FS-Reply
                      WE-Error
                      SQL-State.
     move     spaces to SQL-Msg
     move     zero to SQL-Err
     move     10 to ws-No-Paragraph.
     perform  bb200-Insert.
     if       WS-MYSQL-COUNT-ROWS not = 1
              call  "MySQL_errno" using WS-MYSQL-Error-Number
              call  "MySQL_sqlstate" using WS-MYSQL-SQLstate
              move  WS-MYSQL-SqlState   to SQL-State
              move  99 to fs-reply                  *> this may need changing for val in WE-Error!!
              if    WS-MYSQL-Error-Number  not = "0  "
                    call "MySQL_error" using Ws-Mysql-Error-Message
                    move WS-MYSQL-Error-Number  to SQL-Err
                    move WS-MYSQL-Error-Message to SQL-Msg
                    if    Sql-State = "23000"      *> Dup key
                       or SQL-Err (1:4) = "1062"   *> Ditto, ditto
                                     or = "1022"
                          move 22 to fs-reply
                    end-if
              end-if
     end-if
     perform  ba999-End.
*>
     go       to ba999-Exit.
*>
 ba080-Process-Delete.             *> dry tested. - Has rg01 requirements.
*>
*>
*>  Are we being requested deleting a body line ? lets check
*>
*> [ Deleting all for a key should go to Delete-ALL   ???? ]
*>
     if       WS-Sih-Test not = zero
              perform  bc080-Process-Delete thru bc080-Exit
              go to ba999-Exit.
*>
*>  Nope its a Invoice Header
*>
     set      KOR-x1 to 1.
     move     KOR-offset (KOR-x1) to K
     move     KOR-length (KOR-x1) to L
*>
     move     spaces to WS-Where
     move     1   to J
     string   "`"                   delimited by size
              KeyName (KOR-x1)      delimited by space
              "`"                   delimited by size
              '="'                  delimited by size
              WS-Invoice-Record (K:L)       delimited by size
              '"'                   delimited by size
                      into WS-Where
                        with pointer J
     end-string
     move     WS-Invoice-Record (K:L)  to WS-File-Key.
     move     WS-Where (1:J)   to WS-Log-Where.    *>  For test logging
     if    Testing-2
           display Display-Message-1 with erase eos
     end-if
     move     13 to ws-No-Paragraph.
*>      /MYSQL DELETE\
*>
*>    Delete a row
*>
*>             TABLE=SAAUTOGEN-REC
           INITIALIZE WS-MYSQL-COMMAND
           STRING "DELETE FROM "
             "`SAAUTOGEN-REC`"
             " WHERE "
             WS-Where (1:J)
             X"00" INTO WS-MYSQL-COMMAND
           PERFORM MYSQL-1210-COMMAND THRU MYSQL-1219-EXIT
*>      /MYSQL-END\
     if       WS-MYSQL-COUNT-ROWS not = 1
              call  "MySQL_errno" using WS-MYSQL-Error-Number
              call  "MySQL_sqlstate" using WS-MYSQL-SQLstate
              move  WS-MYSQL-SqlState   to SQL-State
              if    WS-MYSQL-Error-Number  not = "0  "
                    call "MySQL_error" using Ws-Mysql-Error-Message
                    move WS-MYSQL-Error-Number  to SQL-Err
                    move WS-MYSQL-Error-Message to SQL-Msg
              end-if
              move 99 to fs-reply
              move 995 to WE-Error
              go to ba999-End
     else
              move spaces to SQL-Msg
              move zero   to SQL-Err
     end-if.
     perform  ba999-End.
*>
     move     zero to FS-Reply WE-Error.
     go       to ba999-Exit.
*>
 ba085-Process-Delete-ALL.    *> Delete all recs for a given invoice key
*>                                 / lines
*>
*> This is the equivalent of :
*>           EXEC SQL
*>              DELETE
*>              FROM SAAUTOGEN-REC WHERE IH-INVOICE = {key value (1:8)}
*>           END-EXEC.
*>
*>  This will delete all lines for given invoice.
*>
*>           EXEC SQL
*>              DELETE
*>              FROM SAAUTOGEN-LINES-REC WHERE IL-INVOICE = {key value (1:8)}
*>           END-EXEC.
*>
*>  That creates the following code from dbpre
*>
*>   MOVE LOW-VALUES TO SQLCA-STATEMENT.
*>   STRING
*>     "DELETE " DELIMITED BY SIZE
*>     "FROM " DELIMITED BY SIZE
*>     "SAINVOICE-REC " DELIMITED BY SIZE
*>     "WHERE `IH-INVOICE`="
*>         key value (1:8)
*>            INTO SQLCA-STATEMENT.
*>   CALL "MySQL_query" USING SQLCA-STATEMENT.
*>   MOVE LOW-VALUES TO SQLCA-STATEMENT.
*>   STRING
*>     "DELETE " DELIMITED BY SIZE
*>     "FROM " DELIMITED BY SIZE
*>     "SAINV-LINES-REC " DELIMITED BY SIZE
*>     "WHERE `IL-INVOICE`="
*>         key value (1:8)
*>            INTO SQLCA-STATEMENT.
*>   CALL "MySQL_query" USING SQLCA-STATEMENT.
*>
*>  Then doing same for other table
*>
*> So if this does not work it will be changed.
*>
     set      KOR-x1 to 1
     move     KOR-offset (KOR-x1) to K
     move     KOR-length (KOR-x1) to L
*>
     move     spaces to WS-Where
     move     1   to J
     string   "`"                   delimited by size
              KeyName (KOR-x1)      delimited by space
              "`"                   delimited by size
              '="'                  delimited by size          *> was '<"'
              WS-Invoice-Record (K:L)    delimited by size
              '"'                   delimited by size
                      into WS-Where
                        with pointer J
     end-string
     move     spaces to WS-File-Key
     move     WS-Invoice-Key to WS-File-Key
     move     WS-Where (1:J)   to WS-Log-Where.   *>  For test logging
     if    Testing-2
           display Display-Message-1 with erase eos
     end-if
     move     15 to ws-No-Paragraph.
*>      /MYSQL DELETE\
*>
*>    Delete a row
*>
*>             TABLE=SAAUTOGEN-REC
           INITIALIZE WS-MYSQL-COMMAND
           STRING "DELETE FROM "
             "`SAAUTOGEN-REC`"
             " WHERE "
             WS-Where (1:J)
             X"00" INTO WS-MYSQL-COMMAND
           PERFORM MYSQL-1210-COMMAND THRU MYSQL-1219-EXIT
*>      /MYSQL-END\
     if       WS-MYSQL-COUNT-ROWS not > zero    *> Changed for delete-ALL
              call  "MySQL_errno" using WS-MYSQL-Error-Number
              call  "MySQL_sqlstate" using WS-MYSQL-SQLstate
              move  WS-MYSQL-SqlState   to SQL-State
              if    WS-MYSQL-Error-Number  not = "0  "
                    call "MySQL_error" using Ws-Mysql-Error-Message
                    move WS-MYSQL-Error-Number  to SQL-Err
                    move WS-MYSQL-Error-Message to SQL-Msg
              end-if
              move 99  to fs-reply
              move 995 to WE-Error
              go to ba999-End
     else      *> of course there could be no data in table
              move spaces to SQL-Msg
              move zero   to SQL-Err
     end-if.
     move     zero to FS-Reply
                      WE-Error.
     perform  ba999-End.
*>
*> Process RG data  where key is inv # for all lines within inv#
*>
     perform  bc085-Process-Delete-ALL thru bc085-Exit.
     go       to ba999-Exit.
*>
 ba090-Process-Rewrite.             *> dry tested - Has rg01 requirements.
*>
*>  Are we being requested rewriting a body line ? lets check but
*>    rec still in WS-Invoice-Record
*>
     if       WS-Sih-Test not = zero
              perform  bc090-Process-ReWrite thru bc090-Exit
              go to ba999-Exit.                    *> logging rec done.
*>
*>  Nope its a Invoice Header
*>
     perform  bb000-HV-Load.       *> Load up the HV fields from table record in WS
     move     WS-Invoice-Key to WS-File-Key.
     move     17 to ws-No-Paragraph.
     set      KOR-x1 to 1            *> 1 = Primary
     move     KOR-offset (KOR-x1) to K
     move     KOR-length (KOR-x1) to L
*>
     move     spaces to WS-Where
     move     1   to J
     string   "`"                   delimited by size
              KeyName (KOR-x1)      delimited by space
              "`"                   delimited by size
              '="'                  delimited by size
              WS-Invoice-Record (K:L)       delimited by size
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
              call  "MySQL_errno" using WS-MYSQL-Error-Number
              call  "MySQL_sqlstate" using WS-MYSQL-SQLstate
              move  WS-MYSQL-SqlState   to SQL-State
              if    WS-MYSQL-Error-Number  not = "0  "
                    call "MySQL_error" using Ws-Mysql-Error-Message
                    move WS-MYSQL-Error-Number  to SQL-Err
                    move WS-MYSQL-Error-Message to SQL-Msg
              end-if
              move 99 to fs-reply                  *> this may need changing for val in WE-Error!!
              move 994 to WE-Error
              go to ba999-End
     end-if
     move     zero   to FS-Reply
                        WE-Error
                        SQL-Err.
     move     spaces to SQL-Msg.
     perform  ba999-End.
*>
     go       to ba999-Exit.
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
*>             TABLE=SAAUTOGEN-REC
           MOVE TP-SAAUTOGEN-REC TO WS-MYSQL-RESULT
           CALL "MySQL_free_result" USING WS-MYSQL-RESULT end-call
*>      /MYSQL-END\
     set      Cursor-Not-Active to true.
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
     initialize TD-SAAUTOGEN-REC.
*>
     move     WS-Invoice-Key      to HV-SINVOICE-KEY.
*>
     move     WS-Sih-Invoice       to HV-IH-INVOICE.
     move     WS-Sih-Test          to HV-IH-TEST.
     move     WS-Sih-Customer      to HV-IH-CUSTOMER
     move     WS-Sih-Date          to HV-IH-DAT
     move     WS-Sih-Freq          to HV-IH-FREQ
     move     WS-Sih-Repeat        to HV-IH-REPEAT
     move     WS-Sih-Last-Date     to HV-IH-LAST-DATE
     move     WS-Sih-Type          to HV-IH-TYPE
     move     WS-Sih-Ref           to HV-IH-REF
     move     WS-Sih-Description   to HV-IH-DESCRIPTION
     move     WS-Sih-P-C           to HV-IH-P-C
     move     WS-Sih-Net           to HV-IH-NET
     move     WS-Sih-Extra         to HV-IH-EXTRA
     move     WS-Sih-Carriage      to HV-IH-CARRIAGE
     move     WS-Sih-Vat           to HV-IH-VAT
     move     WS-Sih-Discount      to HV-IH-DISCOUNT
     move     WS-Sih-E-Vat         to HV-IH-E-VAT
     move     WS-Sih-C-Vat         to HV-IH-C-VAT
     move     WS-Sih-Status        to HV-IH-STATUS
     move     WS-Sih-Status-P      to HV-IH-STATUS-P
     move     WS-Sih-Status-L      to HV-IH-STATUS-L
     move     WS-Sih-Status-C      to HV-IH-STATUS-C
     move     WS-Sih-Status-A      to HV-IH-STATUS-A
     move     WS-Sih-Status-I      to HV-IH-STATUS-I
     move     WS-Sih-Lines         to HV-IH-LINES
     move     WS-Sih-Deduct-Days   to HV-IH-DEDUCT-DAYS
     move     WS-Sih-Deduct-Amt    to HV-IH-DEDUCT-AMT
     move     WS-Sih-Deduct-Vat    to HV-IH-DEDUCT-VAT
     move     WS-Sih-Days          to HV-IH-DAYS
     move     WS-Sih-CR            to HV-IH-CR
     move     WS-Sih-Day-Book-Flag to HV-IH-DAY-BOOK-FLAG
     move     WS-Sih-Update        to HV-IH-UPDATE.
*>
 bb000-Exit.
     exit section.
*>
 bb100-UnloadHVs    Section.
*>*************************
*>
*>  Load the data buffer in the interface with data from the host variables.
*>
     initialize WS-Invoice-Record.
*>
     move     HV-IH-INVOICE       to WS-Sih-Invoice
     move     HV-IH-TEST          to WS-Sih-Test
     move     HV-IH-CUSTOMER      to WS-Sih-Customer
     move     HV-IH-DAT           to WS-Sih-Date
     move     HV-IH-FREQ          to WS-Sih-Freq
     move     HV-IH-REPEAT        to WS-Sih-Repeat
     move     HV-IH-LAST-DATE     to WS-Sih-Last-Date
     move     HV-IH-TYPE          to WS-Sih-Type
     move     HV-IH-REF           to WS-Sih-Ref
     move     HV-IH-DESCRIPTION   to WS-Sih-Description
     move     HV-IH-P-C           to WS-Sih-P-C
     move     HV-IH-NET           to WS-Sih-Net
     move     HV-IH-EXTRA         to WS-Sih-Extra
     move     HV-IH-CARRIAGE      to WS-Sih-Carriage
     move     HV-IH-VAT           to WS-Sih-Vat
     move     HV-IH-DISCOUNT      to WS-Sih-Discount
     move     HV-IH-E-VAT         to WS-Sih-E-Vat
     move     HV-IH-C-VAT         to WS-Sih-C-Vat
     move     HV-IH-STATUS        to WS-Sih-Status
     move     HV-IH-STATUS-P      to WS-Sih-Status-P
     move     HV-IH-STATUS-L      to WS-Sih-Status-L
     move     HV-IH-STATUS-C      to WS-Sih-Status-C
     move     HV-IH-STATUS-A      to WS-Sih-Status-A
     move     HV-IH-STATUS-I      to WS-Sih-Status-I
     move     HV-IH-LINES         to WS-Sih-Lines
     move     HV-IH-DEDUCT-DAYS   to WS-Sih-Deduct-Days
     move     HV-IH-DEDUCT-AMT    to WS-Sih-Deduct-Amt
     move     HV-IH-DEDUCT-VAT    to WS-Sih-Deduct-Vat
     move     HV-IH-DAYS          to WS-Sih-Days
     move     HV-IH-CR            to WS-Sih-CR
     move     HV-IH-DAY-BOOK-FLAG to WS-Sih-Day-Book-Flag
     move     HV-IH-UPDATE        to WS-Sih-Update.
*>
*>  THIS BLOCK SPECIAL FOR THIS DAL AS IT ALSO processes a RG.
*>  ---------------------------------------------------------
*>   Here save the ih-Lines to WS so we can keep track of body-lines.
*>
     move     HV-IH-LINES         to WS-Actual-Lines-In-Row.
*>
*>   Save sih Invoice & test as last key read.
*>
     move     HV-IH-INVOICE       to WS-Last-Read-Invoice.
     move     HV-IH-TEST          to WS-Last-Read-Line. *> should be zero
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
*>         TABLE=SAAUTOGEN-REC
           INITIALIZE WS-MYSQL-COMMAND
           MOVE 1 TO WS-MYSQL-I
           STRING 'INSERT INTO '
                    '`SAAUTOGEN-REC` SET '
              INTO WS-MYSQL-COMMAND
              WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`SINVOICE-KEY`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           STRING FUNCTION TRIM (HV-SINVOICE-KEY,TRAILING)
                  '"'
                   INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`IH-INVOICE`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-IH-INVOICE
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(11:10))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`IH-TEST`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-IH-TEST
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(18:03))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`IH-CUSTOMER`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           STRING FUNCTION TRIM (HV-IH-CUSTOMER,TRAILING)
                  '"'
                   INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`IH-DAT`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-IH-DAT
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(11:10))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`IH-FREQ`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           STRING FUNCTION TRIM (HV-IH-FREQ,TRAILING)
                  '"'
                   INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`IH-REPEAT`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-IH-REPEAT
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(19:02))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`IH-LAST-DATE`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-IH-LAST-DATE
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(11:10))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`IH-TYPE`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-IH-TYPE
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(18:03))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`IH-REF`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           STRING FUNCTION TRIM (HV-IH-REF,TRAILING)
                  '"'
                   INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`IH-DESCRIPTION`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           STRING FUNCTION TRIM (HV-IH-DESCRIPTION,TRAILING)
                  '"'
                   INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`IH-P-C`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-IH-P-C
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
           STRING '`IH-NET`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-IH-NET
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
           STRING '`IH-EXTRA`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-IH-EXTRA
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
           STRING '`IH-CARRIAGE`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-IH-CARRIAGE
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
           STRING '`IH-VAT`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-IH-VAT
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
           STRING '`IH-DISCOUNT`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-IH-DISCOUNT
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
           STRING '`IH-E-VAT`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-IH-E-VAT
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
           STRING '`IH-C-VAT`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-IH-C-VAT
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
           STRING '`IH-STATUS`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           STRING FUNCTION TRIM (HV-IH-STATUS,TRAILING)
                  '"'
                   INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`IH-STATUS-P`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           STRING FUNCTION TRIM (HV-IH-STATUS-P,TRAILING)
                  '"'
                   INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`IH-STATUS-L`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           STRING FUNCTION TRIM (HV-IH-STATUS-L,TRAILING)
                  '"'
                   INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`IH-STATUS-C`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           STRING FUNCTION TRIM (HV-IH-STATUS-C,TRAILING)
                  '"'
                   INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`IH-STATUS-A`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           STRING FUNCTION TRIM (HV-IH-STATUS-A,TRAILING)
                  '"'
                   INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`IH-STATUS-I`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           STRING FUNCTION TRIM (HV-IH-STATUS-I,TRAILING)
                  '"'
                   INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`IH-DEDUCT-DAYS`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-IH-DEDUCT-DAYS
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(18:03))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`IH-DEDUCT-AMT`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-IH-DEDUCT-AMT
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(18:03))
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
           STRING '`IH-DEDUCT-VAT`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-IH-DEDUCT-VAT
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(18:03))
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
           STRING '`IH-DAYS`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-IH-DAYS
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(18:03))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`IH-CR`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-IH-CR
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(11:10))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`IH-LINES`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-IH-LINES
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(18:03))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`IH-DAY-BOOK-FLAG`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           STRING FUNCTION TRIM (HV-IH-DAY-BOOK-FLAG,TRAILING)
                  '"'
                   INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`IH-UPDATE`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           STRING FUNCTION TRIM (HV-IH-UPDATE,TRAILING)
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
*>      TABLE=SAAUTOGEN-REC
           INITIALIZE WS-MYSQL-COMMAND
           MOVE 1 TO WS-MYSQL-I
           STRING 'UPDATE '
                    '`SAAUTOGEN-REC` SET '
              INTO WS-MYSQL-COMMAND
              WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`SINVOICE-KEY`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           STRING FUNCTION TRIM (HV-SINVOICE-KEY,TRAILING)
                  '"'
                   INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`IH-INVOICE`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-IH-INVOICE
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(11:10))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`IH-TEST`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-IH-TEST
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(18:03))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`IH-CUSTOMER`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           STRING FUNCTION TRIM (HV-IH-CUSTOMER,TRAILING)
                  '"'
                   INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`IH-DAT`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-IH-DAT
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(11:10))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`IH-FREQ`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           STRING FUNCTION TRIM (HV-IH-FREQ,TRAILING)
                  '"'
                   INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`IH-REPEAT`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-IH-REPEAT
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(19:02))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`IH-LAST-DATE`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-IH-LAST-DATE
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(11:10))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`IH-TYPE`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-IH-TYPE
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(18:03))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`IH-REF`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           STRING FUNCTION TRIM (HV-IH-REF,TRAILING)
                  '"'
                   INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`IH-DESCRIPTION`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           STRING FUNCTION TRIM (HV-IH-DESCRIPTION,TRAILING)
                  '"'
                   INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`IH-P-C`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-IH-P-C
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
           STRING '`IH-NET`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-IH-NET
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
           STRING '`IH-EXTRA`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-IH-EXTRA
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
           STRING '`IH-CARRIAGE`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-IH-CARRIAGE
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
           STRING '`IH-VAT`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-IH-VAT
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
           STRING '`IH-DISCOUNT`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-IH-DISCOUNT
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
           STRING '`IH-E-VAT`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-IH-E-VAT
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
           STRING '`IH-C-VAT`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-IH-C-VAT
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
           STRING '`IH-STATUS`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           STRING FUNCTION TRIM (HV-IH-STATUS,TRAILING)
                  '"'
                   INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`IH-STATUS-P`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           STRING FUNCTION TRIM (HV-IH-STATUS-P,TRAILING)
                  '"'
                   INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`IH-STATUS-L`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           STRING FUNCTION TRIM (HV-IH-STATUS-L,TRAILING)
                  '"'
                   INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`IH-STATUS-C`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           STRING FUNCTION TRIM (HV-IH-STATUS-C,TRAILING)
                  '"'
                   INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`IH-STATUS-A`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           STRING FUNCTION TRIM (HV-IH-STATUS-A,TRAILING)
                  '"'
                   INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`IH-STATUS-I`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           STRING FUNCTION TRIM (HV-IH-STATUS-I,TRAILING)
                  '"'
                   INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`IH-DEDUCT-DAYS`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-IH-DEDUCT-DAYS
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(18:03))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`IH-DEDUCT-AMT`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-IH-DEDUCT-AMT
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(18:03))
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
           STRING '`IH-DEDUCT-VAT`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-IH-DEDUCT-VAT
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(18:03))
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
           STRING '`IH-DAYS`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-IH-DAYS
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(18:03))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`IH-CR`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-IH-CR
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(11:10))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`IH-LINES`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV-IH-LINES
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(18:03))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`IH-DAY-BOOK-FLAG`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           STRING FUNCTION TRIM (HV-IH-DAY-BOOK-FLAG,TRAILING)
                  '"'
                   INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`IH-UPDATE`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           STRING FUNCTION TRIM (HV-IH-UPDATE,TRAILING)
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
 bc000-RG-Process section.
*>***********************
*>
*> This section contains mirror processes in ba000 section that act
*> in support of paragraph based process to only deal with the
*> RG ( Repeat Group ) segments of data.
*>
*>  Like ba000 processes they handle one action at a time and do not
*>  do multiple commands or row at once.  This may come later but only
*>   on a as needed basis.
*>
*>  So, bc050 processes the RG table for the held key obtained in
*>  ba040 & ba050.  Same applies to Rewrite, Write & Delete.
*>
*>   This form will be used to process the invoice files tables for
*>    both Sales & Purchase Ledgers.
*>
*>  Note that ws-No-Paragraph start at 51 for RG processing.
*>  Each bc para must end with a bc0n0-Exit
*>
*>   Last para used is 58.
*>
 bc050-Process-Read-Indexed.    *> Dry chk complete.
*>
*>  This routine is called by for both Read-Next ?? and Read-Indexed.
*>
*>    But first save the pointers [and we will restore at end].
*>
     move     WS-Mysql-Result     to WS-Mysql-Save-Result.     *> primary Tbl
     move     WS-Mysql-Count-Rows to WS-Mysql-Save-Count-Rows.
     move     zero to WS-Mysql-Count-Rows.
*>
*>  Use the Key for the primary table to get the RGs that could be up to 40
*>   one at a time on request.
*>
*>   WS-Last-Read-Key is the last one read so if changes we are done.
*>
*>  NEED TO TRANSFER RECORD TO/FROM 01 levels
*>
     set      KOR-x1 to 2.                    *> was  1 = Primary 12/07/23
     move     KOR-offset (KOR-x1) to K
     move     KOR-length (KOR-x1) to L
     move     spaces to WS-Where
     move     1   to J
     string   "`"                   delimited by size
              KeyName (KOR-x1)      delimited by space
              "`"                   delimited by size
              '="'                  delimited by size
              WS-Invoice-Record (K:L)    delimited by size
              '"'                   delimited by size
                      into WS-Where
                        with pointer J
     end-string.
     move     WS-Where (1:J)   to WS-Log-Where.    *>  For test logging
     if    Testing-2
           display Display-Message-1 with erase eos
     end-if
     move     51 to ws-No-Paragraph.
*>      /MYSQL SELECT\
*>
*>    Select rows
*>
*>             TABLE=SAAUTOGEN-LINES-REC
           INITIALIZE WS-MYSQL-COMMAND
           STRING "SELECT * FROM "
             "`SAAUTOGEN-LINES-REC`"
             " WHERE "
             WS-Where (1:J)
            ";"  X"00" INTO WS-MYSQL-COMMAND
           PERFORM MYSQL-1210-COMMAND THRU MYSQL-1219-EXIT
           PERFORM MYSQL-1220-STORE-RESULT THRU MYSQL-1239-EXIT
           MOVE WS-MYSQL-RESULT TO TP-SAAUTOGEN-LINES-REC
*>      /MYSQL-END\
*>
*> Test for no data but this should not happen - Bug in code somewhere
*>  when inserting it. So we will do a log report for analysis.
*>
     if       WS-MYSQL-Count-Rows = zero    *> This should not happen as have to be > 0
              call    "MySQL_errno" using WS-MYSQL-Error-Number
              call    "MySQL_sqlstate" using WS-MYSQL-SQLstate
              move    WS-MYSQL-SqlState   to SQL-State
              if      WS-MYSQL-Error-Number  not = "0  "    *> set non '0' if no rows ?
                      move WS-MYSQL-Error-Number to SQL-Err
                      call "MySQL_error" using WS-MYSQL-Error-Message
                      move WS-MYSQL-Error-Message to SQL-Msg
              end-if
              move    23  to fs-reply
              move    890 to WE-Error
              move    spaces to WS-File-Key
              string  "No RG1 Data for "
                      WS-Invoice-Record (K:L)
                        into WS-File-Key
              end-string
              initialise WS-Invoice-Record       *> Clear both in case called does not spot end of
                         WS-Invoice-Line         *> data for Invoice and to help debugging if so
              go      to bc058-Restore-Pointers  *> do ba999-end at end
     end-if
*>
     move     WS-MYSQL-Count-Rows to WS-Temp-ED-Row.
     string   "RG > 0 got cnt=" delimited by size
              WS-Temp-ED-Row delimited by size
                 " recs, KEY="
              WS-Invoice-Record (K:L)
                        into WS-File-Key
     end-string.
     perform ba999-End.                         *> log it & continue
*>
 bc051-Fetch-RG1.
*>
*>  Now get the data from RG table/s.
*>
     move     52 to ws-No-Paragraph
*>      /MYSQL FETCH\
*>
*>    Fetch next record
*>
*>             TABLE=SAAUTOGEN-LINES-REC
           MOVE TP-SAAUTOGEN-LINES-REC TO WS-MYSQL-RESULT
           CALL "MySQL_fetch_record" USING WS-MYSQL-RESULT
                    HV1-IL-LINE-KEY
                    HV1-IL-INVOICE
                    HV1-IL-LINE
                    HV1-IL-PRODUCT
                    HV1-IL-PA
                    HV1-IL-QTY
                    HV1-IL-TYPE
                    HV1-IL-DESCRIPTION
                    HV1-IL-NET
                    HV1-IL-UNIT
                    HV1-IL-DISCOUNT
                    HV1-IL-VAT
                    HV1-IL-VAT-CODE
                    HV1-IL-UPDATE

*>      /MYSQL-END\
     end-call
*>
     if       WS-MYSQL-Count-Rows  > zero
*> transfer/move HV1 vars to ws-Invoice-Line then to ws-Invoice-Record
              perform  bc100-UnloadHVs-rg1
              move     WS-Invoice-Line to WS-Invoice-Record
     else
              initialise WS-Invoice-Line   *> Incase called does not spot no more data for invoice.
                         WS-Invoice-Record
              move 23 to FS-Reply
     end-if
*>
*> No more rows for key
*>
     move     WS-Invoice-Record (K:L) to WS-File-Key.
     move     zero to FS-Reply WE-Error.
*>
 bc058-Restore-Pointers.
*>
*> Restore the Primary table pointer & row count.
*>
     move     WS-Mysql-Save-Result to WS-Mysql-Result.     *> primary Tbl
     move     WS-Mysql-Save-Count-Rows to WS-Mysql-Count-Rows.
     perform  ba999-End.
*>
 bc059-Exit.  Exit.
*>
 bc070-Process-Write.  *> dry coded. tested - 1
*>
     move     WS-Invoice-Record  to  WS-Invoice-Line.
     perform  bc000-HV-Load-rg1.
     move     WS-Invoice-Key to WS-File-Key.   *> Same as WS-Sil-Key
     move     zero to FS-Reply WE-Error.
     move     spaces to SQL-Msg
                        SQL-State.
     move     zero to SQL-Err.
     move     53  to ws-No-Paragraph.
     perform  bc200-Insert-rg1.          *> chgd 26/07/23
*>
     if       WS-MYSQL-COUNT-ROWS not = 1
              call  "MySQL_errno" using WS-MYSQL-Error-Number
              call  "MySQL_sqlstate" using WS-MYSQL-SQLstate
              move  WS-MYSQL-SqlState   to SQL-State
              move  99 to fs-reply                  *> this may need changing for val in WE-Error!!
              if    WS-MYSQL-Error-Number  not = "0  "
                 or Sql-State = "23000"      *> Dup key (rec already present)
                    call "MySQL_error" using Ws-Mysql-Error-Message
                    move WS-MYSQL-Error-Number  to SQL-Err
                    move WS-MYSQL-Error-Message to SQL-Msg
                    if   Sql-State = "23000"      *> Dup key (rec already present)
                      or SQL-Err (1:4) = "1062"
                      or               = "1022"
                         move 22 to fs-reply
                    else
                         move 99 to fs-reply                  *> this may need changing for val in WE-Error!!
                    end-if
              end-if
              move    spaces to WS-File-Key
              string  "Cant Re|WriteRG1 Data on "
                      WS-Invoice-Record (K:L)
                      " RG="
                      WS-Sil-Line
                          into WS-File-Key
              end-string
     end-if
     perform ba999-End.
*>
 bc070-Exit.  Exit.
*>
 bc080-Process-Delete.     *> Dry tested.
*>
*>  Delete one invoice-line (Item). But data is in WS-Invoice-Record
*>
     set      KOR-x1 to 2.      *> inv.line
     move     KOR-offset (KOR-x1) to K
     move     KOR-length (KOR-x1) to L
*>
     move     spaces to WS-Where
     move     1   to J
     string   "`"                   delimited by size
              KeyName (KOR-x1)      delimited by space
              "`"                   delimited by size
              '="'                  delimited by size
              WS-Invoice-Record (K:L)       delimited by size
              '"'                   delimited by size
                      into WS-Where
                        with pointer J
     end-string
     move     WS-Invoice-Record (K:L)  to WS-File-Key.
     move     WS-Where (1:J)   to WS-Log-Where.    *>  For test logging
     if    Testing-2
           display Display-Message-1 with erase eos
     end-if
     move     54 to ws-No-Paragraph.           *> Delete all rows (9<=) for key
*>      /MYSQL DELETE\
*>
*>    Delete a row
*>
*>             TABLE=SAAUTOGEN-LINES-REC
           INITIALIZE WS-MYSQL-COMMAND
           STRING "DELETE FROM "
             "`SAAUTOGEN-LINES-REC`"
             " WHERE "
             WS-Where (1:J)
             X"00" INTO WS-MYSQL-COMMAND
           PERFORM MYSQL-1210-COMMAND THRU MYSQL-1219-EXIT
*>      /MYSQL-END\
*>
*>  We could have from 0 to 1 so this Error report is moot.
*>
     if       WS-MYSQL-COUNT-ROWS not > zero
              call "MySQL_errno" using WS-MYSQL-Error-Number
              call  "MySQL_sqlstate" using WS-MYSQL-SQLstate
              move  WS-MYSQL-SqlState   to SQL-State
              if    WS-MYSQL-Error-Number  not = "0  "
                    call "MySQL_error" using Ws-Mysql-Error-Message
                    move WS-MYSQL-Error-Number  to SQL-Err
                    move WS-MYSQL-Error-Message to SQL-Msg
              end-if
              move spaces to WS-File-Key
              move WS-MYSQL-COUNT-ROWS to WS-Temp-ED-Row
              string "Delete for "
                     WS-Invoice-Record (K:L)
                     " only found (rg01) "
                     WS-Temp-ED-Row
                     " Rows"
                          into WS-File-Key
              end-string
              go to ba999-End
     else
              move spaces to SQL-Msg SQL-State
              move zero   to SQL-Err
     end-if.
     move     zero to FS-Reply WE-Error.
     perform  ba999-End.
*>
 bc080-Exit.  Exit.
*>
 bc085-Process-Delete-ALL.    *> THIS IS NON STANDARD - NEEDED (sl810) - Coded /
*>
*> Processes RG rows - Called by ba085-Process-Delete-All
*>  As coded it will delete all body-lines within a given invoice, hopefully.
*> This is the equivalent of :
*>           EXEC SQL
*>              DELETE
*>              FROM SAAUTOGEN-LINES-REC WHERE 'IL-INVOICE' = WS-Sih-Invoice
*>           END-EXEC.
*>
*>  That creates the following code from dbpre
*>
*>   MOVE LOW-VALUES TO SQLCA-STATEMENT.
*>   STRING
*>   "DELETE " DELIMITED SIZE
*>     "FROM " DELIMITED SIZE
*>     "SAINV-LINES-REC " DELIMITED SIZE
*>     "WHERE 'IL-INVOICE' = " '"' WS-Sih-Invoice '"' *> "
*>   INTO SQLCA-STATEMENT.
*>   CALL "MySQL_query" USING SQLCA-STATEMENT
*>
*> So if this does not work, it will be changed.
*>
     set      KOR-x1 to 2
     move     KOR-offset (KOR-x1) to K
     move     KOR-length (KOR-x1) to L
*>
     move     spaces to WS-Where
     move     1   to J
     string
 *>             "`"                   delimited by size
 *>             KeyName (KOR-x1)      delimited by space    *>   ?????? should be SA
 *>             "`"                   delimited by size
 *>             WS-Invoice-Record (K:L)    delimited by size
*>
              "'IL-INVOICE'"        delimited by size
              '="'                  delimited by size      *> was '<"'
              WS-Sih-Invoice        delimited by size
              '"'                   delimited by size
                      into WS-Where
                        with pointer J
     end-string
     move     spaces to WS-File-Key
     move     WS-Where (1:J)   to WS-Log-Where.   *>  For test logging
     if    Testing-2
           display Display-Message-1 with erase eos
     end-if
     move     55  to ws-No-Paragraph.
*>      /MYSQL DELETE\
*>
*>    Delete a row
*>
*>             TABLE=SAAUTOGEN-LINES-REC
           INITIALIZE WS-MYSQL-COMMAND
           STRING "DELETE FROM "
             "`SAAUTOGEN-LINES-REC`"
             " WHERE "
             WS-Where (1:J)
             X"00" INTO WS-MYSQL-COMMAND
           PERFORM MYSQL-1210-COMMAND THRU MYSQL-1219-EXIT
*>      /MYSQL-END\
     move     WS-MYSQL-COUNT-ROWS to WS-Temp-ED-Row
     string   "Deleting All lines in " delimited by size
               WS-Sih-Invoice          delimited by size
               " with "
                WS-Temp-Ed-Row (6:2)   delimited by size
                " lines."
                      into WS-File-Key
     end-string                                   *> for logging
     if       WS-MYSQL-COUNT-ROWS not > zero    *> Changed for delete-ALL
              call  "MySQL_errno" using WS-MYSQL-Error-Number
              call  "MySQL_sqlstate" using WS-MYSQL-SQLstate
              move  WS-MYSQL-SqlState   to SQL-State
              if    WS-MYSQL-Error-Number  not = "0  "
                    call "MySQL_error" using Ws-Mysql-Error-Message
                    move WS-MYSQL-Error-Number  to SQL-Err
                    move WS-MYSQL-Error-Message to SQL-Msg
                    move 99 to fs-reply
                    move 995 to WE-Error
              end-if
              perform  ba999-End
              go   to bc085-Exit
     else      *> of course there could be no data in table
              move spaces to SQL-Msg
              move zero   to SQL-Err
     end-if.
     move     zero to FS-Reply WE-Error.
     perform  ba999-End.
*>
 bc085-Exit.  Exit.
*>
 bc090-Process-Rewrite section.
*>****************************
*>
     move     WS-Invoice-Record  to WS-Invoice-Line.
     perform  bc000-HV-Load-rg1.       *> Load up the HV rg1 fields from table record in WS
     move     WS-Sil-Key to WS-File-Key.
*>
     move     56    to ws-No-Paragraph.
     set      KOR-x1 to 2            *> 2 = Lines
     move     KOR-offset (KOR-x1) to K
     move     KOR-length (KOR-x1) to L
*>
     move     spaces to WS-Where
     move     1   to J
     string   "`"                   delimited by size
              KeyName (KOR-x1)      delimited by space
              "`"                   delimited by size
              '="'                  delimited by size
              WS-Invoice-Record (K:L)       delimited by size
              '"'                   delimited by size
                      into WS-Where
                        with pointer J
     end-string
     move     WS-Where (1:J)   to WS-Log-Where.    *>  For test logging
*>
     perform  bc300-Update-rg1.
*>
     if       Testing-2
              display Display-Message-1 with erase eos
     end-if
*>
     if       WS-MYSQL-COUNT-ROWS not = 1
              call  "MySQL_errno" using WS-MYSQL-Error-Number
              call  "MySQL_sqlstate" using WS-MYSQL-SQLstate
              move  WS-MYSQL-SqlState   to SQL-State
              if    WS-MYSQL-Error-Number  not = "0  "
                    call "MySQL_error" using Ws-Mysql-Error-Message
                    move WS-MYSQL-Error-Number  to SQL-Err
                    move WS-MYSQL-Error-Message to SQL-Msg
              end-if
              move 99 to fs-reply                  *> this may need changing for val in WE-Error!!
              move 994 to WE-Error
              perform ba999-End
              go to bc090-Exit
     end-if
     move     zero   to FS-Reply
                        WE-Error
                        SQL-Err.
     move     spaces to SQL-Msg.
     perform  ba999-End.
*>
 bc090-Exit.  exit.
*>
 bc000-HV-Load-rg1 Section.     *> Dry chk ?
*>*************************
*>
*>  Load the Host variables with data from the passed record
*>
*> Accommodate Repeating Groups...
*> Now load the RG multi-rows into the 'OCCURS' fields of record.
*>
*> Loading RG table: SAAUTOGEN-LINES-REC
*> <<<<  this should be a move from invoice-rec to line rec >>>>> <<>>
*>
     initialize TD-SAAUTOGEN-LINES-REC.
*>
     move     WS-Sil-Key         to HV1-IL-LINE-KEY.
     move     WS-Sil-Line        to HV1-IL-LINE
     move     WS-Sil-Invoice     to HV1-IL-INVOICE
     move     WS-Sil-Product     to HV1-IL-PRODUCT
     move     WS-Sil-Pa          to HV1-IL-PA
     move     WS-Sil-Qty         to HV1-IL-QTY
     move     WS-Sil-Type        to HV1-IL-TYPE
     move     WS-Sil-Description to HV1-IL-DESCRIPTION
     move     WS-Sil-Net         to HV1-IL-NET
     move     WS-Sil-Unit        to HV1-IL-UNIT
     move     WS-Sil-Discount    to HV1-IL-DISCOUNT
     move     WS-Sil-Vat         to HV1-IL-VAT
     move     WS-Sil-Vat-Code    to HV1-IL-VAT-CODE
     move     WS-Sil-Update      to HV1-IL-UPDATE.
*>
*> End of SAAUTOGEN-LINES-REC unload...
*> End of Repeating Group processing...
*>
 bc000-Exit.
     exit section.
*>
 bc100-UnloadHVs-rg1 Section.    *> Dry chk ?
*>**************************
*>
*>  Load the data buffer in the interface with data from the host variables.
*>
*> Accommodate Repeating Groups...
*> Now unload the RG multi-rows into the fields of record.
*>
*> Unloading RG table: SAAUTOGEN-LINES-REC
*>
     initialize WS-Invoice-Line.
*>
     move     HV1-IL-LINE-KEY    to  WS-Sil-Key.
     move     HV1-IL-LINE        to  WS-Sil-Line
     move     HV1-IL-PRODUCT     to  WS-Sil-Product
     move     HV1-IL-PA          to  WS-Sil-Pa
     move     HV1-IL-QTY         to  WS-Sil-Qty
     move     HV1-IL-TYPE        to  WS-Sil-Type
     move     HV1-IL-DESCRIPTION to  WS-Sil-Description
     move     HV1-IL-NET         to  WS-Sil-Net
     move     HV1-IL-UNIT        to  WS-Sil-Unit
     move     HV1-IL-DISCOUNT    to  WS-Sil-Discount
     move     HV1-IL-VAT         to  WS-Sil-Vat
     move     HV1-IL-VAT-CODE    to  WS-Sil-Vat-Code
     move     HV1-IL-UPDATE      to  WS-Sil-Update.
*>
*> End of SAAUTOGEN-LINES-REC unload... but save the last read line.
*>
     move     HV1-IL-Line to WS-Last-Read-Line.
*>
*> Now move it to primary WS record area.
*>
     move     WS-Invoice-Line to WS-Invoice-Record.
*>
*> End of Repeating Group processing...
*>
 bc100-Exit.
     exit section.
*>
 bc200-Insert-rg1 Section.
*>***********************
*>
*>  /MYSQL INSERT\
*>
*>    Insert a row
*>
*>         TABLE=SAAUTOGEN-LINES-REC
           INITIALIZE WS-MYSQL-COMMAND
           MOVE 1 TO WS-MYSQL-I
           STRING 'INSERT INTO '
                    '`SAAUTOGEN-LINES-REC` SET '
              INTO WS-MYSQL-COMMAND
              WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`IL-LINE-KEY`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           STRING FUNCTION TRIM (HV1-IL-LINE-KEY,TRAILING)
                  '"'
                   INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`IL-INVOICE`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV1-IL-INVOICE
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(11:10))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`IL-LINE`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV1-IL-LINE
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(18:03))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`IL-PRODUCT`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           STRING FUNCTION TRIM (HV1-IL-PRODUCT,TRAILING)
                  '"'
                   INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`IL-PA`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           STRING FUNCTION TRIM (HV1-IL-PA,TRAILING)
                  '"'
                   INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`IL-QTY`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV1-IL-QTY
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(16:05))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`IL-TYPE`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           STRING FUNCTION TRIM (HV1-IL-TYPE,TRAILING)
                  '"'
                   INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`IL-DESCRIPTION`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           STRING FUNCTION TRIM (HV1-IL-DESCRIPTION,TRAILING)
                  '"'
                   INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`IL-NET`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV1-IL-NET
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
           STRING '`IL-UNIT`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV1-IL-UNIT
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
           STRING '`IL-DISCOUNT`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV1-IL-DISCOUNT
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(19:02))
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
           STRING '`IL-VAT`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV1-IL-VAT
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
           STRING '`IL-VAT-CODE`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV1-IL-VAT-CODE
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(18:03))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`IL-UPDATE`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           STRING FUNCTION TRIM (HV1-IL-UPDATE,TRAILING)
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
 bc200-Exit.
     exit section.
*>
 bc300-Update-rg1 Section.
*>***********************
*>
*>  /MYSQL UPDATE\
*>
*>    Update a row
*>
*>      TABLE=SAAUTOGEN-LINES-REC
           INITIALIZE WS-MYSQL-COMMAND
           MOVE 1 TO WS-MYSQL-I
           STRING 'UPDATE '
                    '`SAAUTOGEN-LINES-REC` SET '
              INTO WS-MYSQL-COMMAND
              WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`IL-LINE-KEY`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           STRING FUNCTION TRIM (HV1-IL-LINE-KEY,TRAILING)
                  '"'
                   INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`IL-INVOICE`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV1-IL-INVOICE
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(11:10))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`IL-LINE`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV1-IL-LINE
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(18:03))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`IL-PRODUCT`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           STRING FUNCTION TRIM (HV1-IL-PRODUCT,TRAILING)
                  '"'
                   INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`IL-PA`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           STRING FUNCTION TRIM (HV1-IL-PA,TRAILING)
                  '"'
                   INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`IL-QTY`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV1-IL-QTY
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(16:05))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`IL-TYPE`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           STRING FUNCTION TRIM (HV1-IL-TYPE,TRAILING)
                  '"'
                   INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`IL-DESCRIPTION`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           STRING FUNCTION TRIM (HV1-IL-DESCRIPTION,TRAILING)
                  '"'
                   INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`IL-NET`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV1-IL-NET
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
           STRING '`IL-UNIT`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV1-IL-UNIT
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
           STRING '`IL-DISCOUNT`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV1-IL-DISCOUNT
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(19:02))
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
           STRING '`IL-VAT`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV1-IL-VAT
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
           STRING '`IL-VAT-CODE`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           MOVE HV1-IL-VAT-CODE
             TO WS-MYSQL-EDIT
           STRING FUNCTION TRIM (WS-MYSQL-EDIT(18:03))
             INTO WS-MYSQL-COMMAND
             WITH POINTER WS-MYSQL-I end-string
           STRING '"' INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
           STRING ', 'INTO WS-MYSQL-COMMAND
           WITH POINTER WS-MYSQL-I end-string
*>
           STRING '`IL-UPDATE`="' INTO WS-MYSQL-COMMAND
                   WITH POINTER WS-MYSQL-I end-string
           STRING FUNCTION TRIM (HV1-IL-UPDATE,TRAILING)
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
 bc300-Exit.
     exit section.
*>
 bc998-Free.
     move     58 to ws-No-Paragraph.
*>      /MYSQL FREE\
*>
*>    Free result array
*>
*>             TABLE=SAAUTOGEN-LINES-REC
           MOVE TP-SAAUTOGEN-LINES-REC TO WS-MYSQL-RESULT
           CALL "MySQL_free_result" USING WS-MYSQL-RESULT end-call
*>      /MYSQL-END\
     set      Cursor-Not-Active-2 to true.
*>
 Ca-Process-Logs.
*>**************
*>
     call     "fhlogger" using File-Access
                               ACAS-DAL-Common-data.
*>
 ca-Exit.     exit.
*>
end program slautogenMT.
