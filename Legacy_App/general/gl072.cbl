       >>source free
*>******************************************************
*>                                                     *
*>             Batch  Transaction  Update              *
*>                                                     *
*>******************************************************
*>
 identification          division.
*>===============================
*>
*>**
      program-id.         gl072.
*>**
*>    Author.             GL was written by Simon Whine MBCS, on behalf of
*>                        Applewood Computers and its group of Companies.
*>                        All changes/migrations by:
*>                        Vincent B. Coen FBCS, FIDM, FIDPM.
*>                        Converted To Cis January 85,
*>                        For Applewood Computers.
*>                        Written to supplement IRS to support larger numbers for
*>                        accounts to 10 digits nominal and subnominals and money
*>                        amounts to 100M - 1 for customers requiring a
*>                        comparable? but cheaper product than Oracle financials.
*>                        Reduced down some point later in time for accnts 6
*>                        digits and reduced money amounts.
*>**
*>    Security.           Copyright (C) 1976-2025, Vincent Bryan Coen.
*>                        Distributed under the GNU General Public License.
*>                        See the file COPYING for details.
*>**
*>    Remarks.            Batch Update.
*>**
*>    Version.            See Prog-Name In Ws.
*>**
*>    Called Modules.     NONE.
*>**
*>    Error messages used.
*>                        NONE
*>**
*>   Changes:
*> 28/01/09 vbc - Migration to Open Cobol.
*> 29/05/09 vbc - Support for Page-Lines instead of fixed number.
*> 07/09/10 vbc - .03 Mod lpr.
*> 20/12/11 vbc - .04 Support for dates other than UK & clean up msgs
*>                    Error msgs to GLnnn, Support for path+filenames.
*>                    Support for Page-Lines instead of fixed number.
*> 24/10/16 vbc - .   ALL programs now using wsnames.cob in copybooks.
*> 13/01/18 vbc - .05 Updated for v3.02 & FH & DALs.
*> 05/06/23 vbc - .06 chg fdprint to x(120) tidy up report.
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
 environment             division.
*>===============================
*>
 copy "envdiv.cob".
 input-output            section.
*>------------------------------
*>
 file-control.
*>-----------
*>
     select  post-trans  assign  post-trans-name
                         access  sequential
                         status  fs-reply
                         organization  line sequential.
*> copy "seledger.cob".
*> copy "selbatch.cob".
 copy "selprint.cob".
 data                    division.
*>===============================
*>
 file section.
*>-----------
*>
 fd  post-trans.
*>
 01  post-trans-record.
     03  post-batch      pic 9(5).
     03  post-post       pic 9(5).
     03  post-code       pic xx.
     03  post-date       pic x(8).
     03  post-ledger.
         05  post-ac     pic 9(6).
         05  post-pc     pic 99.
     03  post-amount     pic s9(8)v99.
     03  post-legend     pic x(32).
*>
*> copy "fdledger.cob".
*> copy "fdbatch.cob".
 copy "fdprint.cob"  replacing ==x(132)== by ==x(120)==.
 working-storage section.
*>----------------------
*>
 77  prog-name           pic x(15)       value "gl072 (3.02.06)".
 copy "print-spool-command.cob".
 copy "wsfnctn.cob".
 copy "wsledger.cob".
 copy "wsbatch.cob".
*>
*> REMARK OUT ANY IN USE
*>
 01  Dummies-4-Unused-ACAS-FH-Calls.      *> Call blk at zz080-ACAS-Calls
*>     03  Default-Record         pic x.
     03  Final-Record           pic x.
     03  System-Record-4        pic x.
*>     03  WS-Ledger-Record       pic x.
     03  WS-Posting-Record      pic x.
*>     03  WS-Batch-Record        pic x.
     03  WS-IRS-Posting-Record  pic x.
     03  WS-Stock-Audit-Record  pic x.
     03  WS-Stock-Record        pic x.
     03  WS-Sales-Record        pic x.
     03  WS-Value-Record        pic x.
     03  WS-Delivery-Record     pic x.
     03  WS-Analysis-Record     pic x.
     03  WS-Del-Inv-Nos-Record  pic x.
     03  WS-Purch-Record        pic x.
     03  WS-Pay-Record          pic x.
     03  WS-Invoice-Record      pic x.
     03  WS-OTM3-Record         pic x.
     03  WS-PInvoice-Record     pic x.
     03  WS-OTM5-Record         pic x.
*>
 copy "Test-Data-Flags.cob".  *> set sw-testing to zero to stop logging.
*>
 01  filler.
     03  save-batch      pic 9(5)        value zero.
     03  save-ledger     pic 9(8)        value zero.
     03  read-ledger     pic x           value space.
     03  line-cnt        binary-char     value zero.
     03  y               pic 99          value zero.
     03  tot-dr          pic 9(8)v99     value zero.
     03  tot-cr          pic 9(8)v99     value zero.
*>
     03  ws-env-lines    pic 999       value zero.
     03  ws-lines        binary-char unsigned value zero.
     03  ws-23-lines     binary-char unsigned value zero.
     03  ws-22-lines     binary-char unsigned value zero.
     03  ws-21-lines     binary-char unsigned value zero.
     03  ws-20-lines     binary-char unsigned value zero.
     03  Body-lines      binary-char unsigned value zero.
*>
 01  All-My-Constants    pic 9(4).
     copy "screenio.cpy".
*>
 01  ws-Test-Date            pic x(10).
 01  ws-date-formats.
     03  ws-swap             pic xx.
     03  ws-Conv-Date        pic x(10).
     03  ws-date             pic x(10).
     03  ws-UK redefines ws-date.
         05  ws-days         pic xx.
         05  filler          pic x.
         05  ws-month        pic xx.
         05  filler          pic x.
         05  ws-year         pic x(4).
     03  ws-USA redefines ws-date.
         05  ws-usa-month    pic xx.
         05  filler          pic x.
         05  ws-usa-days     pic xx.
         05  filler          pic x.
         05  filler          pic x(4).
     03  ws-Intl redefines ws-date.
         05  ws-intl-year    pic x(4).
         05  filler          pic x.
         05  ws-intl-month   pic xx.
         05  filler          pic x.
         05  ws-intl-days    pic xx.
*>
*> 01  Error-Messages.
*> System Wide
*>
 01  line-1.   *> 120
     03  l1-prog             pic x(50).
     03  filler              pic x(63)   value "Transaction  Posting".
     03  filler              pic x(5)    value "Page ".
     03  l1-page             pic z9.
*>
 01  line-3.
     03  filler              pic x(8)    value "Cycle - ".
     03  l3-cycle            pic z9.
     03  filler              pic x(18)   value "          Batch - ".
     03  l3-batch            pic z(4)9.
     03  filler              pic x(10)   value spaces.
     03  l3-type             pic x(12)   value "General".
     03  l3-desc             pic x(32)   value spaces.
     03  filler              pic x(23)   value spaces.
     03  l3-date             pic x(10).
*>
 01  line-4.
     03  filler              pic x(120)  value
         "Account   Br/P C    --Balance--   Trans  --Date--    ---" &
         "Debit---    ---Credit--       -------------Legend------------".
*>
 01  line-5.
     03  filler              pic x(34)   value "-------".
     03  filler              pic x(5)    value "-----".
*>
 01  line-6.
     03  l6-account          pic 9999.99 blank when zero.
     03  filler              pic x(6)    value spaces.
     03  l6-pc               pic 99      blank when zero.
     03  filler              pic x(5)    value spaces.
     03  l6-balance          pic z(7)9.99cr blank when zero.
     03  l6-tran             pic bbbz9   blank when zero.
     03  filler              pic xxxx    value spaces.
     03  l6-date             pic x(12).
     03  l6-debit            pic z(7)9.99  blank when zero.
     03  filler              pic xxxx     value spaces.
     03  l6-credit           pic z(7)9.99  blank when zero.
     03  filler              pic x(6)     value spaces.
     03  l6-legend           pic x(32).
*>
 01  line-7.
     03  filler              pic x(54)    value spaces.
     03  filler              pic x(11)    value all "=".
     03  filler              pic xxxx     value spaces.
     03  filler              pic x(11)    value all "=".
*>
 linkage section.
*>--------------
*>
 copy "wscall.cob".
 copy "wssystem.cob".
 copy "wsnames.cob".
*>
 01  to-day              pic x(10).
*>
 procedure division using ws-calling-data
                          system-record
                          to-day
                          file-defs.
*>***************************************
*>
 gl072-Main section.
*>*****************
*>
     move     Print-Spool-Name to PSN.
     move     1  to File-Key-No.
*>
     display  "Phase - 4.  Transaction Update" at 0801 with foreground-color 2.
     move     prog-name to l1-prog.
     perform  GL-Batch-Open.               *> open     i-o  batch-file ledger-file.
     perform  GL-Nominal-Open.
     open     input  post-trans.
     open     output print-file.
*>
     move     zero  to  save-batch save-ledger.
*>
 loop.
*>***
*>
     read     post-trans  at end
              perform  end-account
              perform  end-batch
              go to    end-run.
*>
     if       post-batch  not numeric
              go to  loop.
*>
     if       post-batch not = save-batch
       and    save-batch not = zero
       and    we-error   not = 999
              perform  end-account
              perform  end-batch
              move  zero  to  save-ledger
              perform  headings  through  headings-end.
*>
     if       save-batch  equal  zero
              move  post-batch  to  save-batch
              perform  headings  through  headings-end.
*>
     if       we-error  equal  999
              go to  loop.
*>
     if       post-ledger not = save-ledger
       and    save-ledger not = zero
              perform  end-account
              perform  new-account.
*>
     if       save-ledger  equal  zero
              perform  new-account.
*>
     move     post-ledger  to  save-ledger.
*>
     move     post-post  to  l6-tran.
     move     post-date  to  l6-date.
     if       post-amount  >  zero
              move  post-amount  to  l6-debit
              add   post-amount  to  tot-dr
              move  zero         to  l6-credit
     else
              move      post-amount  to    l6-credit
              subtract  post-amount  from  tot-cr
              move      zero         to    l6-debit.
*>
     move     post-legend  to  l6-legend.
     add      post-amount  to  ledger-balance.
*>
     write    print-record  from  line-6 after 1.
     add      1 to line-cnt.
     if       line-cnt > Page-Lines
              perform  end-account
              perform  headings
              move     "R"  to  read-ledger
              perform  new-account.
*>
     go       to loop.
*>
 headings.
*>*******
*>
     perform  get-batch.
*>
     if       we-error  equal  999
              go to  headings-end.
*>
     perform  zz070-convert-date.
     move     ws-date  to  l3-date.
     add      1  to  y.
     move     y  to  l1-page.
     move     scycle  to  l3-cycle.
     move     post-batch  to  l3-batch.
     if       y not = 1
              write print-record from line-1 after page
              write print-record from line-3 after 1
              move spaces to print-record
              write print-record after 1
     else
              write print-record from line-1 before 1
              write print-record from line-3 before 1.
     write    print-record  from  line-4 after 1.
     write    print-record  from  line-5 after 1.
     move     5 to line-cnt.
*>
 headings-end.
*>***********
*>
 end-batch.
*>********
*>
     move     1  to  cleared-status.
     move     run-date  to  posted.
     perform  GL-Batch-Rewrite.               *> rewrite  batch-record.
*>
 end-account.
*>**********
*>
     perform  GL-Nominal-Rewrite.             *> rewrite  ledger-record.
*>
     write    print-record  from  line-7 after 1.
     add      1 to line-cnt.
     divide   WS-Ledger-Nos  by  100  giving  l6-account.
     move     ledger-pc       to  l6-pc.
     move     ledger-balance  to  l6-balance.
     move     tot-dr          to  l6-debit.
     move     tot-cr          to  l6-credit.
     move     to-day          to  l6-date.
     move     ledger-name     to  l6-legend.
     move     zero            to  l6-tran.
*>
     write    print-record  from  line-6 after 1.
     write    print-record  from  line-7 after 1.
*>
     move     spaces  to  print-record.
     write    print-record after 1.
     add      3 to line-cnt.
*>
 new-account.
*>**********
*>
     move     post-ledger  to  WS-Ledger-Key.
*>
     if       read-ledger not = "R"
              perform  GL-Nominal-Read-Next.             *> read  ledger-file  record.
*>
     if       read-ledger not = "R"
              move  zero   to  tot-dr  tot-cr.
*>
     divide   WS-Ledger-Nos  by  100  giving  l6-account.
     move     ledger-pc       to  l6-pc.
     move     ledger-balance  to  l6-balance.
     move     zero            to  l6-debit.
     move     zero            to  l6-credit.
     perform  zz070-convert-date.
     move     ws-date         to  l6-date.
*>
     if       read-ledger not = "R"
              if    ledger-balance  >  zero
                    move  ledger-balance  to  l6-debit
                    add   ledger-balance  to  tot-dr
              else
                    move  ledger-balance  to  l6-credit
                    add   ledger-balance  to  tot-cr.
*>
     move     "Brought Forward"  to  l6-legend.
     move     zero            to  l6-tran.
     move     space  to  read-ledger.
*>
     write    print-record  from  line-6 after 2.
     add      2 to line-cnt.
     move     zero  to  l6-balance.
*>
 end-run.
*>******
*>
     close    post-trans print-file.                   *> close  batch-file ledger-file.
     perform  GL-Batch-Close.
     perform  GL-Nominal-Close.
     call     "SYSTEM" using Print-Report.
*>
 main-exit.
     goback.
*>
 get-batch               section.
*>==============================
*>
     move     1           to  WS-Ledger.
     move     post-batch  to  save-batch  WS-Batch-Nos.
*>
     perform  GL-Batch-Read-Next.                       *> read     batch-file  record.
*>
     move     description of WS-Batch-Record to  l3-desc.
*>
     if       not waiting
              move  999  to  we-error
              move  0    to  save-batch
     else
              move  0    to  we-error.
*>
 main-exit.   exit.
*>********    ****
*>
 zz070-Convert-Date        section.
*>********************************
*>
*>  Converts date in to-day to UK/USA/Intl date format
*>****************************************************
*> Input:   to-day
*> output:  ws-date as uk/US/Inlt date format
*>
     move     to-day to ws-date.
*>
     if       Date-Form = zero
              move 1 to Date-Form.
     if       Date-UK
              go to zz070-Exit.
     if       Date-USA                *> swap month and days
              move ws-days to ws-swap
              move ws-month to ws-days
              move ws-swap to ws-month
              go to zz070-Exit.
*>
*> So its International date format
*>
     move     "ccyy/mm/dd" to ws-date.  *> swap Intl to UK form
     move     to-day (7:4) to ws-Intl-Year.
     move     to-day (4:2) to ws-Intl-Month.
     move     to-day (1:2) to ws-Intl-Days.
*>
 zz070-Exit.
     exit     section.
*>
 copy "Proc-ACAS-FH-Calls.cob".
*>
