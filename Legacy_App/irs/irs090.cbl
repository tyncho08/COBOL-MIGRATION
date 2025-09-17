       >>source free
*>*****************************************************************
*>                                                                *
*>      P O S T F I L E   A N A L Y S I S   R E P O R T           *
*>                                                                *
*>*****************************************************************
*>
 identification division.
 program-id.            irs090.
*>
*> Author.              V.B.COEN, FIDPM FBCS for Applewood Computers.
*>**
*> Security.            Copyright (C) 1976-2025, Vincent Bryan Coen.
*>                      Distributed under the GNU General Public License.
*>                      See the file COPYING for details.
*>
*>**
*> Remarks.             Report of posting file by month.
*>**
*> Version.             See prog-name in ws.
*>**
*> Calls.               acasirsub1  ->
*>                       irsnominalMT
*>**
*> Error messages used.
*>  System Wide
*>                      SM901 }
*>                      SM903 } Produced by DAL.
*>                      SM904 }
*>                      SY008.
*>                      IR901 }
*>                      IR902 } From FH.
*>                      IR911 }
*>                      IR912 }
*>                      IR913 }
*>                      IR914 }
*>                      IR915 }
*>                      IR916 }
*>  Module Specific.
*>                      IR990       Does not exist..Aborting (  )
*>                      IR991 Error on writing work file. Reply (  )
*>**
*> Changes.
*> 28/05/84 vbc - hilite display heads.
*> 03/03/85 vbc - modify where amounts are added to ws-years.
*> 26/09/89 vbc - 2.53
*>                Mods for cobol/2.
*> 23/01/09 vbc - Migration to Open/GNU Cobol as version 3.
*> 20/02/09 vbc - It is not a bug for there to be minus zero in reports.
*> 21/02/09 vbc - Added support for environment LINES and COLUMNS as needed. NOT
*> 28/02/09 vbc - Get rid of the -0 figures with no success must be a OC issue.
*> 07/09/10 vbc - .10 Mod lpr.
*> 21/09/10 vbc -     Added print spool.
*> 12/10/16 vbc - .11 Cosmetics on headings and for last line.
*> 20/12/16 vbc -     All irs progs call cpybk Proc-ZZ100-ACAS-IRS-Calls.
*> 29/01/18 vbc - .12 Removed unused error mesages from WS & chgd to v3.02.
*>                    Added IR990 & 18 in place of very old style messages.
*>                    moved wssystem.cob (irs) to copybook & renamed wssystem.cob,
*>                    fdwsnl & print-spool-commandxxx in irs src with leading 'irs'
*>                    using in irs progs. Using copybooks throughout.
*> 10/02/18 vbc - .13 Renamed IR011-16 to IR911-16 and others.
*> 02/05/18 vbc - .14 Added Time to report (in Listing) & do this for most of the
*>                    reports in IRS as it is hard to work out the latest reports.
*> 16/04/24 vbc       Copyright notice update superseding all previous notices.
*>
*>*************************************************************************
*>
*>  From copyright.cob.
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
*>*************************************************************************
*>
 environment division.
 copy "envdiv.cob".
*>
 input-output section.
 file-control.
*>
 *>    select  nominal-ledger  assign nl-file
 *>                            organization indexed
 *>                            access dynamic
 *>                            record key-1
 *>                            status fs-reply.
*>
     select  work-file       assign "workpost.tmp"
                             organization sequential
                             status fs-reply.
*>
     select  work-file-2     assign "work2.tmp"
                             organization indexed
                             access dynamic
                             record wl4-no
                             status fs-reply.
*>
     select  print-file      assign "prt-1"
                             organization line sequential.
*>
 data division.
*>**************
*>
 file section.
*>-------------
*>
 fd  work-file.
*>
 01  work-record.
     03  work-mm         pic 99.
     03  work-account    pic 9(5)      comp.
     03  work-amount     pic s9(7)v99  comp.
*>
*> fd  nominal-ledger.
*>  copy "irsfdwsnl.cob".
 fd  work-file-2.
*>
 01  work-record-2.
     03  filler.
         05  wl4-no      pic 9(5)  comp.
     03  wl4-group       pic x(126).
*>
 fd  print-file.
*>
 01  print-record        pic x(132).
*>
 working-storage section.
 77  prog-name           pic x(16)  value "irs090 (3.02.14)".
 77  sr-reply            pic 99     value zeros.
 copy "irsprint-spool-command.cob".
*>
 01  filler.
     03  nl-file         pic x(11).
     03  ws-page-no      pic 99        comp            value zero.
     03  ws-line-cnt     pic 99        comp            value zero.
     03  a               pic 99        comp            value zero.
     03  b               pic 99        comp            value zero.
     03  z               pic s9(8)      comp           value zero.
     03  ws-saved-account pic 9(5)     comp            value zero.
     03  ws-mths         pic s9(8)v99   comp     occurs 12.
     03  ws-years        pic s9(8)      comp               value zero.
     03  ws-mlits-a      pic x(60)    value " JAN  FEB MARCHAPRIL " &
          "MAY  JUNEJULY  AUG SEPT  OCT  NOV  DEC ".
     03  ws-mlits-b  redefines ws-mlits-a
                         pic x(5) occurs 12.
*>
     03  hdtime                          value spaces.
         05  hd-hh       pic xx.
         05  hd-mm       pic xx.
         05  hd-ss       pic xx.
         05  hd-uu       pic xx.
     03  HD2-Time.
         05  filler      pic x     value spaces.
         05  hd2-hh      pic xx.
         05  filler      pic x     value ":".
         05  hd2-mm      pic xx.
*>
 01  filler.
     03  line-1.
       05  l1-client     pic x(32)    value spaces.
       05  filler        pic x(23)    value spaces.
       05  filler        pic x(25)    value "Monthly Analysis Report".
       05  filler        pic x(28)    value spaces.
       05  l1-date       pic x(8).
       05  P-Time        pic x(6)     value spaces.
       05  filler        pic xx       value spaces.
       05  filler        pic x(5)     value "Page ".
       05  l1-page       pic zz9.
*>
     03  line-2.
       05  filler        pic x(29)    value "<----------Account--------->".
       05  filler        pic x        value "<".
       05  filler        pic x(46)    value all "-".
       05  filler        pic x(4)     value "Year".
       05  filler        pic x(43)    value all "-".
       05  filler        pic x        value ">".
*>
     03  line-3.
       05  filler        pic x(28)    value "  No Typ      Name".
       05  l3-lmonths                occurs 12.
         07  filler      pic xxx.
         07  l3-litmonth pic x(5).
       05  l3-ltotal     pic x(9)     value "  Total".
*>
     03  line-4.
      04  filler.
       05  l4-no         pic z(4)9b.
      04  l4-group.
       05  l4-type       pic xb.
       05  l4-name       pic x(20).
       05  l4-month      pic -(7)9    occurs 12.
       05  l4-total      pic -(7)9.
*>
    03  line-5           pic x(48)   value
        "  Note that above values are subject to rounding".
*>
 copy "wsfnctn.cob".
 copy "irsfdwsnl.cob"      replacing record-1      by WS-IRSNL-Record.
*>
 01  Dummies-For-Unused-FH-Calls.      *> IRS call blk at zz100-ACAS-IRS-Calls
 *>    03  WS-IRSNL-Record       pic x.
     03  WS-IRS-Default-Record pic x.
     03  Posting-Record        pic x.
     03  Final-Record          pic x.
     03  WS-IRS-Posting-Record pic x.
*>
 copy "Test-Data-Flags.cob".  *> set sw-testing to zero to stop logging.
*>
 01  Error-Messages.
*> System Wide      USED are :
     03  SY008          pic x(32) value "SY008 Note message & Hit return ".
     03  IR911          pic x(47) value "IR911 Error on systemMT processing, Fs-reply = ".
     03  IR912          pic x(51) value "IR912 Error on irsnominalMT processing, Fs-reply = ".
     03  IR913          pic x(48) value "IR913 Error on irsdfltMT processing, Fs-reply = ".
     03  IR914          pic x(51) value "IR914 Error on irspostingMT processing, Fs-reply = ".
     03  IR915          pic x(49) value "IR915 Error on irsfinalMT processing, Fs-reply = ".
     03  IR916          pic x(50) value "IR916 Error on slpostingMT processing, Fs-reply = ".
*> Module specific  USED are :
     03  IR990          pic x(41) value "IR990       Does not exist..Aborting (  )".
     03  IR991          pic x(44) value "IR991 Error on writing work file. Reply (  )".
*>
 linkage   section.
*>*****************
*>
 copy "irswssystem.cob"   *> IRS param record
              replacing system-record  by IRS-System-Params. *> (01 level)
*>
 copy "wssystem.cob"      *> new for v3.02
             replacing  System-Record by WS-System-Record
                        Run-Date   by ACAS-Run-Date     *> these 3 are in binary
                        Start-Date by ACAS-Start-Date   *> IRS expects as x(8)
                        End-Date   by ACAS-End-Date     *>  dd/mm/yy
                        suser      by ACAS-suser
                        Address-1  by ACAS-Address-1
                        Address-2  by ACAS-Address-2
                        Address-3  by ACAS-Address-3
                        Address-4  by ACAS-Address-4
                        Post-Code  by ACAS-Post-Code
                        Print-Spool-Name by ACAS-Print-Spool-Name
                        Pass-Value by ACAS-Pass-Value
                        Pass-Word  by ACAS-Pass-Word
                        OP-System  By ACAS-Op-System
                        Client     by IRS-Client
                        System-Ops by IRS-System-Ops
                        Next-Post  by IRS-Next-Post
                        Vat-Rates2 by IRS-Vat-Rates2
                        Vat1       by IRS-Vat1
                        Vat2       by IRS-Vat2
                        Vat3       by IRS-Vat3
                        Vat-Group  by IRS-Vat-Group
                        Vat-Psent  by IRS-Vat-Psent
                        Save-Sequ  by IRS-Save-Sequ
                        System-Work-Group by IRS-System-Work-Group
                        PL-App-Created by IRS-PL-App-Created
                        PL-Approp-AC   by IRS-PL-Approp-AC
                        1st-Time-Flag  by IRS-First-Time-Flag.
*>
 copy "wsnames.cob".
*>
 procedure division using IRS-System-Params
                          WS-System-Record
                          File-Defs.
*>*****************************************
*>
 la-control section.
*>***************
*>
     perform  lb-init.
     perform  lc-process.
     perform  ld-output-final.
     perform  le-close.
*>
 laa-exit.
     exit     program.
*>
 lb-init section.
*>****************
*>
     accept   hdtime from time.
     if       hdtime not = "00000000"
              move hd-hh to hd2-hh
              move hd-mm to hd2-mm
              move HD2-Time  to P-Time.
*>
     move     zero to a b.
     perform  varying b from 1 by 1 until b > 12 move zeros to ws-mths (b)
     end-perform
*>
     move     run-date to l1-date.
     move     client to l1-client.
*>
 lbb-screen-heads.
*>
     display  prog-name                                            at 0101 with erase eos foreground-color 2.
     display  "Monthly Analysis Report"         at 0130 with foreground-color 1 background-color 7.
     display  run-date                                             at 0173 with foreground-color 2.
     display  "You will need 132 column wide paper"                at 0623 with foreground-color 3.
     display  "Which Month does your Tax Year start (01-12)  [  ]" at 1201 with foreground-color 2.
*>
 lbc-get-year-start.
*>
     accept   sr-reply at 1248 with foreground-color 3.
     if       sr-reply < 01 or > 12
              go to lbc-get-year-start.
*>
     open     input  work-file.      *>  nominal-ledger.
     perform  acasirsub1-Open-Input.
     open     output  print-file work-file-2.
     perform  lzc-set-report-months thru lzc-exit.
     perform  lzd-headings thru lzd-exit.
     move     zero to a b.
*>
 lbz-exit.    exit section.
*>
 lc-process section.
*>******************
*>
 lca-start.
*>
     read     work-file at end
              perform  lze-print-line thru lzf-exit
              go to lcz-exit.
     if       ws-saved-account = zero
              perform lzf-get-nominal thru lzf-exit
              move work-account to ws-saved-account.
*>
     if       ws-saved-account not = work-account
              perform lze-print-line thru lze-exit
              perform lzf-get-nominal thru lzf-exit
              move work-account to ws-saved-account.
     if       work-mm < 01 or > 12
              move 01 to work-mm.
     add      work-amount to ws-mths (work-mm).
     go       to lca-start.
*>
 lcz-exit.    exit section.
*>
 ld-output-final section.
*>***********************
*>
     close    work-file-2.         *> nominal-ledger
     open     input work-file-2.   *> nominal-ledger
     perform  acasirsub1-Close.
     perform  acasirsub1-Open-Input.
*>
 ldb.
*>
     perform  lzg-read-nl-seq thru lzg-exit.
     if       fs-reply = "FE"
              go to ldz-exit.
     move     owning to wl4-no.
     read     work-file-2  invalid key
              go to ldb.
*>
     if       wl4-no not = owning
              go to ldb.
     if       ws-line-cnt > 60
              perform lzd-headings thru lzd-exit.
     move     wl4-no to l4-no.
     move     wl4-group to l4-group.
     write    print-record from line-4 after 1.
     add      1 to ws-line-cnt.
     go       to ldb.
*>
 ldz-exit.    exit section.
*>
 le-close section.
*>****************
*>
     write    print-record from line-5 after 3.
     close    print-file work-file work-file-2.      *>  nominal-ledger
     perform  acasirsub1-Close.
     move     Print-Spool-Name to PSN.
     call     "SYSTEM" using Print-Report.
     open     output  work-file-2 work-file.
     close    work-file-2 work-file.
*>
 lez-exit.
     exit     section.
*>
*>*********************************************
*>                                            *
*>  Common routines used in the above code    *
*>                                            *
*>*********************************************
*>
 lz-common-routines section.
*>**************************
*>
 lzc-set-report-months.
*>
     move     1 to a.
     move     sr-reply to b.
*>
 lzc-set-mth.
*>
     move     ws-mlits-b (b)  to  l3-litmonth (a).
     add      1 to a.
     add      1 to b.
     if       a > 12
              go to lzc-exit.
     if       b > 12
              move 1 to b.
*>
     go       to lzc-set-mth.
*>
 lzc-exit.    exit.
*>
 lzd-headings.
*>
     add      1 to ws-page-no.
     move     ws-page-no to l1-page.
     write    print-record from line-1 before 1.
     write    print-record from line-2 after 1.
     write    print-record from line-3 after 1.
     move     spaces to print-record.
     write    print-record after 1.
     move     5 to ws-line-cnt.
*>
 lzd-exit.    exit.
*>
 lze-print-line.
*>
     move     1 to a.
     move     sr-reply to b.
*>
 lze-set-mth.
*>
     if       (ws-mths (b) = 0 and ws-mths (b) negative)
           or ws-mths (b) = -0
              move +0 to ws-mths (b).    *> yep , but it doesnt work
     move     ws-mths (b)  to  z.
     add      z to ws-years.
     move     ws-mths (b)  to  l4-month (a).
     add      1 to a.
     add      1 to b.
     if       a > 12
              go to lze-work.
     if       b > 12
              move 1 to b.
     go       to lze-set-mth.
*>
 lze-work.
*>
     move     nl-name to l4-name.
     if       sub-nominal = zero
              move owning to wl4-no
              move "M" to l4-type
       else
              move sub-nominal to wl4-no
              move "S" to l4-type.
*>
     move     ws-years to l4-total.
     move     zero to ws-years.
     perform  varying b from 1 by 1 until b > 12
              move zero to ws-mths (b)
     end-perform
     move     l4-group to wl4-group.
     write    work-record-2 invalid key
              move "FF" to fs-reply.
     if       fs-reply not = zero
              display IR991    at 1601
              display fs-reply at 1642
              display SY008    at 1701
              accept fs-reply at 1734
              go to lcz-exit.
*>
 lze-exit.    exit.
*>
 lzf-get-nominal.
*>
     move     work-account to owning.
     move     zero    to sub-nominal.
*>
 lzf-read.
*>
     perform  acasirsub1-Read-Indexed.
     if       fs-reply not = zero
              display IR990 at 1401 with foreground-color 2
              display owning at 1401 with foreground-color 3
              display FS-Reply at 1439 with foreground-color 3
              display SY008 at 1501
              accept fs-reply at 1534
              perform  acasirsub1-Close
              close work-file work-file-2 print-file
              stop run.
*>
*>    Get pointer to Sub nominal to find main a/c
*>
     if       tipe = "S"
              move owning to sub-nominal
              move rec-pointer to owning
              go to lzf-read.
*>
*>    At this point, we have got the record we want
*>
 lzf-exit.    exit.
*>
 lzg-read-nl-seq.
*>
     perform  acasirsub1-Read-Next.
     if       fs-reply = 10
              move "FE" to fs-reply.
*>
     if       fs-reply = "FE"
              go to lzg-exit.
*>
     if       tipe = "S"
              go to lzg-read-nl-seq.
     if       sub-nominal not = zeros
              move sub-nominal to owning.
*>
 lzg-exit.    exit.
*>
 copy "Proc-ZZ100-ACAS-IRS-Calls.cob".
*>
