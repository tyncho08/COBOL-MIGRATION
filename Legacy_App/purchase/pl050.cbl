       >>source free
*>*************************************************
*>                                                *
*>          Purchase Order Proof  Report          *
*>                                                *
*>   This program does NOT update any files for   *
*>                this report.                    *
*>                                                *
*>*************************************************
*>
 identification          division.
*>===============================
*>
*>**
      program-id.         pl050.
*>**
*>    Author.             V B Coen FBCS, FIDM, FIDPM, Applewood Computers.
*>**
*>    Security.           Copyright (C) 1976-2025 & later, Vincent Bryan Coen.
*>                        Distributed under the GNU General Public License.
*>                        See the file COPYING for details.
*>*
*>    Remarks.            Purchase Orders Proof Report.
*>**
*>    Version.            See Prog-Name In Ws.
*>
*>    Called Modules.     Maps04.
*>                        acas022           (Purchase Ledger/Payables)
*>                         purchMT
*>                        acas026  ->       (Purchase Order Folios[invoices])
*>                         PinvoiceMT.
*>**
*>    Error messages used.
*>                        PL120
*>**
*>   Changes.
*> 21/05/84 Vbc - Report On Vat Codes.
*> 28/06/84 Vbc - Remove Extra, Discount Analysis,Adjust Position
*>                Of Prompt Pay In Report Totals.
*> 22/03/09 vbc - Migration to Open Cobol v3.00.00
*> 29/05/09 vbc - Support for Page-Lines instead of fixed number.
*> 07/09/10 vbc - .03 Mod lpr.
*> 12/12/11 vbc - .04 Error msgs to SLnnn.Support for dates other than UK
*>                    (Neither used here)
*>                    Support for path+filenames.
*>                    Updated version to 3.01.nn
*> 24/10/16 vbc - .   ALL programs now using wsnames.cob in copybooks
*> 07/01/18 vbc - .05 Support for RDB on tables
*>                    instead of just cobol files. Update version to v3.02
*>                    Now uses Proc-ACAS-FH-Calls.cob for all FH & DAL calls
*>                    Added in support for remaining FH & DALs.
*> 22/03/18 vbc - .06 Chg or added msgs for no data with file test
*>                    Chg program title and report head - same for SL050.
*>                    If called by xl150 do not stop for any errors.
*> 16/04/24 vbc       Copyright notice update superseding all previous notices.
*> 18/08/25 vbc - .07 in initialize Total-Group (a) had 4 in it, chg to varying 1 by 1
*>                    until > 3 - compiler problem ?.
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
*>------------
*>
 *> copy "selpl.cob".
 *> copy "selpinv.cob".
 copy "selprint.cob".
*>
 data                    division.
*>===============================
*>
 file section.
*>-----------
*>
 *> copy "fdpl.cob".
 *> copy "fdpinv.cob".
 copy "fdprint.cob".
 working-storage section.
*>----------------------
 77  prog-name           pic x(15) value "PL050 (3.02.07)".
 copy "print-spool-command.cob".
 copy "wsmaps03.cob".
 copy "wsfnctn.cob".
*>
*>  EX FDs
*>
 copy "wspl.cob".
 copy "wsfdpinv.cob". *> replacing Invoice-Record by WS-PInvoice-Record.
*>
*> REMARK OUT, ANY IN USE
*>
 01  Dummies-4-Unused-ACAS-FH-Calls.      *> Call blk at zz080-ACAS-Calls
     03  Default-Record         pic x.
     03  Final-Record           pic x.
     03  System-Record-4        pic x.
     03  WS-Ledger-Record       pic x.
     03  WS-Posting-Record      pic x.
     03  WS-Batch-Record        pic x.
     03  WS-IRS-Posting-Record  pic x.
     03  WS-Stock-Audit-Record  pic x.
     03  WS-Stock-Record        pic x.
     03  WS-Sales-Record        pic x.
     03  WS-Value-Record        pic x.
     03  WS-Delivery-Record     pic x.
     03  WS-Analysis-Record     pic x.
     03  WS-Del-Inv-Nos-Record  pic x.
*>     03  WS-Purch-Record        pic x.
     03  WS-Pay-Record          pic x.
     03  WS-Invoice-Record      pic x.
     03  WS-OTM3-Record         pic x.
*>     03  WS-PInvoice-Record     pic x.
     03  WS-OTM5-Record         pic x.
*>
 01  ws-data.
     03  test-product.
         05  filler      pic x.
             88  il-comment              value "/".
         05  filler      pic x(11).
     03  ws-reply        pic x.
     03  ws-error        pic 9           value zero.
         88  purchase-missing            value 1.
     03  i               pic 99.
     03  j               pic 999.
     03  k               pic 99.
     03  l               pic 99          value zero.
     03  first-time      pic x           value "Y".
     03  print-out       pic x           value space.
     03  a               pic 9           value zero.
     03  Total-Group                     occurs 3    comp-3.
         05  total-ded   pic s9(8)v99.
         05  total-gds   pic s9(8)v99.
         05  total-car   pic s9(8)v99.
         05  total-net   pic s9(8)v99.
         05  total-vat   pic s9(8)v99.
         05  total-grs   pic s9(8)v99.
         05  total-cvat  pic s9(8)v99    occurs 3.
     03  ws-deduct-amt   pic s999v99 comp-3 value zero.
     03  line-cnt        binary-char        value zero.
*>
 *> copy "wspinv.cob".  *> replacing WS table.
 copy "plwspinv.cob".   *> invoice table
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
 copy "Test-Data-Flags.cob".  *> set sw-testing to zero to stop logging.
*>
 01  File-Info                          value zero.
     05 File-Size-Bytes  pic 9(18) comp.
     05 Mod-DD           pic 9(2)  comp.
     05 Mod-MO           pic 9(2)  comp.
     05 Mod-YYYY         pic 9(4)  comp.
     05 Mod-HH           pic 9(2)  comp.
     05 Mod-MM           pic 9(2)  comp.
     05 Mod-SS           pic 9(2)  comp.
     05 filler           pic 9(2)  comp. *> Always 00
*>
 01  Error-Messages.
*> System Wide
*> Module specific
     03  PL120          pic x(57) value "PL120 No Transactions to proof! ...Press return for menu.".
*>
 01  ws-group-lits.
     03  filler          pic x(17)      value "Receipts".
     03  filler          pic x(17)      value "Invoices".
     03  filler          pic x(17)      value "Credit Notes".
 01  filler  redefines ws-group-lits.
     03  ws-lit          pic x(17)   occurs 3.

 01  line-1.
     03  l1-version      pic x(49)       value spaces.
     03  filler          pic x(75)       value "Unapplied Purchase Transaction Report".
     03  filler          pic x(5)        value "Page ".
     03  l3-page         pic zz9.
*>
 01  line-2.
     03  l3-user         pic x(49)       value spaces.
     03  filler          pic x(37)       value spaces.
     03  filler          pic x(36)       value spaces.
     03  l1-date         pic x(10).
*>
 01  line-4.
     03  filler          pic x(66)       value
         " Number  ---Date--- <------------Supplier------------>    --Type--".
     03  filler          pic xxxx        value spaces.
     03  filler          pic x(62)       value
         "     Goods                 <---Net-->  <---Vat-->  <--Gross->".
*>
 01  line-5.
     03  l5-nos          pic z(7)9b.
     03  l5-date         pic x(11).
     03  l5-cust         pic x(9).
     03  l5-name         pic x(25).
     03  l5-marker       pic x(4)        value spaces.
     03  l5-type         pic x(14).
     03  l5-goods        pic z(6)9.99cr.
     03  filler          pic x(12)       value spaces.
     03  l5-net          pic z(6)9.99cr.
     03  l5-vat          pic z(6)9.99cr.
     03  l5-gross        pic z(6)9.99cr.
*>
 01  line-6.
     03  filler          pic xxx         value spaces.
     03  filler          pic x(10)       value "    Total".
     03  l6-lit          pic x(18).
     03  l6-goods        pic z(6)9.99cr.
     03  l6-carr         pic z(6)9.99cr.
     03  l6-net          pic z(6)9.99cr.
     03  l6-vat          pic z(6)9.99cr.
     03  l6-gross        pic z(6)9.99cr.
     03  l6-ded          pic z(6)9.99cr.
*>
 01  line-7.
     03  filler          pic x(36)      value spaces.
     03  filler          pic x(66)      value
     "Goods    Carriage         Net         Vat       Gross  Prompt Pay".
*> "
 01  line-8.
     03  filler          pic x(9)        value "Vat Code".
     03  l8-vat-code     pic 9.
     03  filler          pic xxx         value spaces.
     03  l8-vat-rate     pic z9.99.
     03  filler          pic x(5)        value "%".
     03  l8-amount1      pic z(7)9.99cr.
     03  l8-amount2      pic z(9)9.99cr.
     03  l8-amount3      pic z(9)9.99cr.
*>
 01  line-9.
     03  filler          pic x(70)       value
         "V.A.T. Reconciliation       Receipts       Invoices   Credit Notes".
*>
 01  line-a.
     03  filler          pic x(46)       value spaces.
     03  la-a            pic x(12).
     03  la-code         pic x(6).
     03  la-b            pic x(8).
     03  la-amount       pic z(6)9.99cr.
     03  filler          pic x(2)        value spaces.
     03  la-c            pic x(9).
     03  la-vat          pic 9.
*>
 01  line-b.
     03  filler          pic x(46)       value spaces.
     03  lb-a            pic x(12)       value spaces.
     03  lb-code         pic x(6).
     03  lb-b            pic x(8)        value spaces.
     03  lb-amount       pic z(6)9.99cr.
*>
 01  line-d.
     03  filler          pic x(46)       value spaces.
     03  ld-a            pic x(12).
     03  ld-code         pic x(6)        value spaces.
     03  ld-b            pic x(8)        value spaces.
     03  ld-amount       pic z(6)9.99cr.
     03  filler          pic x(2)        value spaces.
     03  ld-c            pic x(9).
     03  ld-vat          pic 9.
*>
 linkage section.
*>***************
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
*>=======================================
*>
 init01 section.
     move     Print-Spool-Name to PSN.
     move     spaces to print-out.
     perform  varying a from 1 by 1 until a > 3
              initialize Total-Group (a)
     end-perform
*>
     move     prog-name to l1-version.
     perform  zz070-Convert-Date.
     move     ws-date to l1-date.
     move     1  to File-Key-No.
*>
 Menu-Return.
     display  prog-name at 0101 with foreground-color 2 erase eos.
     display  "Purchase Orders Proof Report" at 0128  with foreground-color 2.
     display  ws-date at 0171 with foreground-color 2.
*>
     if       FS-Cobol-Files-Used
              call  "CBL_CHECK_FILE_EXIST" using File-26
                                                 File-Info
              if    return-code not = zero          *> not = zero - No file found
                    display PL120   at 0501 with foreground-color 4
                    if     WS-Caller not = "xl150"
                           accept ws-reply at 0563 with foreground-color 6
                    end-if
                    goback.                         *> As no transactions.
*>
     perform  Purch-Open-Input.   *> open     input  purchase-file  invoice-file.
     perform  PInvoice-Open-Input.
     open     output print-file.
*>
     move     zero  to  j.
     perform  headings.
*>
 loop.
     perform  PInvoice-Read-Next.  *> read     invoice-file  next record  at end
     if       FS-Reply not = zero
              go to  main-end.
*>
     move     WS-PInvoice-Record  to  PInvoice-Header.
*>
     if       ih-test not = zero
              go to  loop.
*>
*> trap transactions which are already on the sales ledger.
*>
     if       applied
              go to  loop.
     move     zero  to l.
     move     ih-supplier  to  WS-Purch-Key  l5-cust.
*>
     move     space to ws-reply.
     perform  Purch-Read-Indexed.  *> read     purchase-file   record invalid key
     if       FS-Reply not = zero
              move "X" to ws-reply.
*>
     if       ws-reply = "X"
              move "Supplier Unknown" to l5-name
              move 1 to ws-error
     else     move purch-name  to  l5-name.
*>
     move     ih-invoice  to  l5-nos.
     move     ih-date  to  u-bin.
     perform  zz060-Convert-Date.
     move     ws-date  to  l5-date.
*>
     move     ih-deduct-amt to ws-deduct-amt.
*>
     if       ih-type = 3
              multiply -1 by ih-net
              multiply -1 by ih-carriage
              multiply -1 by ih-vat
              multiply -1 by ih-c-vat.
*>
     if       ih-type = 2
              multiply -1 by ws-deduct-amt.
*>
     if       ws-deduct-amt = zero
              move  spaces  to  lb-a  lb-code
              move  zero    to  lb-amount
     else
              move  "Prompt Pay  "  to  lb-a
              move  spaces          to  lb-code
              move  ws-deduct-amt   to  lb-amount.
*>
     if       ih-carriage = zero
              move  spaces to ld-a  ld-code  ld-c
              move  zero   to ld-amount  ld-vat
     else
              move  "Carriage    "  to  ld-a
              move  spaces          to  ld-code  ld-c
              move  ih-carriage     to  ld-amount
              if    ih-c-vat = zero
                    move  zero  to  ld-vat
              else
                    move  1  to  ld-vat.
*>
     move     ih-type to a.
*>
     if       ih-type = 1
              move  "Receipt"  to  l5-type
     else
              if    ih-type = 2
                    move  "Invoice"  to  l5-type
              else
                    move  "Cr. Note" to  l5-type.
*>
     add      ih-c-vat to total-cvat (a, 1).
*>
     move     ih-net  to  l5-goods.
     add      ih-vat ih-c-vat giving  l5-vat.
     add      ih-net ih-carriage giving  l5-net.
     add      ih-vat ih-c-vat ih-net ih-carriage giving l5-gross.
*>
     add      ih-net  to  total-gds (a).
     add      ws-deduct-amt to total-ded (a).
     add      ih-carriage  to  total-car (a).
     add      ih-vat  ih-c-vat  to  total-vat (a).
     add      ih-net ih-carriage to  total-net (a).
     add      ih-vat ih-c-vat ih-net ih-carriage to total-grs (a).
*>
     if       line-cnt > Page-Lines
              perform headings.
*>
     write    print-record  from  line-5 after 2  lines.
     add      2 to line-cnt.
*>
     move     "Y" to Print-Out.
     move     "Y" to first-time.
     perform  analysis-print  ih-lines times.
     perform  extra-analysis.
     go       to loop.
*>
 main-end.
*>
     perform  Purch-Close.   *> close    purchase-file invoice-file.
     perform  PInvoice-Close.
*>
     perform  main12 in headings.
     write    print-record from line-7 after 2.
     move     spaces to print-record.
     write    print-record after 1.
     move     zero to a.
     perform  total-report 3 times.
*>
     if       purchase-missing
              move "Warning: Record/s missing in Purchase File" to print-record
              write print-record after 2
              move "Y" to Print-Out.
*>
     write    print-record from line-9 after 5.
     move     spaces to print-record.
     write    print-record after 1.
     perform  compute-vatcodes 3 times.
     close    print-file.
     if       Print-Out = "Y"
              call   "SYSTEM" using Print-Report.
*>
 menu-call.
     exit     program.
*>
 headings section.
*>***************
*>
 main12.
*>
     add      1  to  j.
     move     j  to  l3-page.
*>
     move     usera to  l3-user.
     if       j not = 1
              write print-record from line-1 after page
              write print-record from line-2 after 1
              move spaces to print-record
              write print-record after 1
     else
              write print-record from line-1 before 1
              write print-record from line-2 before 1
     end-if.
*>
 cont-hds.
*>
     write    print-record  from  line-4 after 1.
     move     spaces  to  print-record.
     write    print-record after 1.
     move     5 to line-cnt.
*>
 analysis-print          section.
*>==============================
*>
     perform  PInvoice-Read-Next. *> read     invoice-file  next record.
*>
     move     WS-PInvoice-Record  to  invoice-line (1).
     move     il-product (1)  to  test-product.
     if       il-comment
              go to  main-exit.
*>
     if       ih-type = 3
              multiply -1 by il-vat (1)
              multiply -1 by il-net (1).
*>
     move     il-pa (1)   to  la-code.
     move     il-net (1)  to  la-amount.
     move     il-vat-code (1) to k   la-vat.
     if       k > zero
              add il-vat (1) to total-cvat (a, k).
*>
     if       first-time = "Y"
              move  "Analysis"  to  la-a
              move  "Value"     to  la-b
              move  "Vat Code"  to  la-c
              move  "N"         to  first-time
     else
              move  spaces      to  la-a  la-b  la-c.
*>
     write    print-record  from  line-a after 1.
     add      1 to line-cnt.
     if       line-cnt > Page-Lines
              perform headings.
*>
 main-exit.   exit section.
*>********    ****
*>
 extra-analysis section.
*>=====================
*>
     if       line-cnt > Page-Lines - 2
              perform headings.
*>
     if       lb-a not = spaces
              write  print-record  from  line-b after 1
              add 1 to line-cnt
              move  spaces  to  lb-a.
*>
     if       ld-a not = spaces
              write  print-record  from  line-d after 1
              add 1 to line-cnt
              move  spaces  to  ld-a.
*>
 main-exit.   exit section.
*>********    ****
*>
 total-report section.
*>*******************
*>
     add      1 to a.
     move     zero to i.
     move     total-gds (a) to  l6-goods.
     move     total-car (a) to  l6-carr.
     move     total-net (a) to  l6-net.
     move     total-vat (a) to  l6-vat.
     move     total-grs (a) to  l6-gross.
     move     total-ded (a) to  l6-ded.
     move     ws-lit (a)    to  l6-lit.
     add      1 to line-cnt.
     write    print-record from line-6 after 1.
*>
 compute-vatcodes section.
*>***********************
*>
     add      1 to i.
     move     i to l8-vat-code.
     move     vat-rate (i) to l8-vat-rate.
     move     total-cvat (1, i) to l8-amount1.
     move     total-cvat (2, i) to l8-amount2.
     move     total-cvat (3, i) to l8-amount3.
     write    print-record from line-8 after 1.
     add      1 to line-cnt.
*>
*>
 zz060-Convert-Date        section.
*>********************************
*>
*>  Converts date in binary to UK/USA/Intl date format
*>****************************************************
*> Input:   u-bin
*> output:  ws-date as uk/US/Inlt date format
*>          u-date & ws-Date = spaces if invalid date
*>
     perform  maps04.
     if       u-date = spaces
              move spaces to ws-Date
              go to zz060-Exit.
     move     u-date to ws-date.
*>
     if       Date-Form = zero
              move 1 to Date-Form.
     if       Date-UK
              go to zz060-Exit.
     if       Date-USA                *> swap month and days
              move ws-days to ws-swap
              move ws-month to ws-days
              move ws-swap to ws-month
              go to zz060-Exit.
*>
*> So its International date format
*>
     move     "ccyy/mm/dd" to ws-date.  *> swap Intl to UK form
     move     u-date (7:4) to ws-Intl-Year.
     move     u-date (4:2) to ws-Intl-Month.
     move     u-date (1:2) to ws-Intl-Days.
*>
 zz060-Exit.
     exit     section.
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
 maps04       section.
*>*******************
*>
     call     "maps04"  using  maps03-ws.
*>
 maps04-exit.
     exit     section.
*>
 copy "Proc-ACAS-FH-Calls.cob".
*>
