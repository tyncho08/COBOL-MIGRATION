       >>source free
*>****************************************************
*>                                                   *
*>      S T A T E M E N T   P R O D U C T I O N      *
*>                                                   *
*>    Needs to be changed to suit your statements.   *
*>    see body and end of statements section         *
*>      eg, the last 170 or so lines of Cobol code   *
*>      but also copied below here:                  *
*>****************************************************
*>  Does not update any data files                   *
*>****************************************************
*> Note that sl110, 120 & 190 are similar.
*>*************************************************************************************
*>====================================================================================*
*> You might want to change the (Cups) spool name to reflect the printer containing   *
*>  any preprinted special paper, eg continuous forms or a laser that has the         *
*>  statement Template pre-loaded.                                                    *
*>   See next statement then you can uncomment the call statement below.              *
*> NOTE that at the moment the program spools & printed with                          *
*>   the system name of prt-2. See manual for further notes on this.                  *
*>------------------------------------------------------------------------------------*
*>      Similar issues will apply to invoice production as well, see SL930 & SL950    *
*>          for printing the delivery notes and picking lists.                        *
*>       as well as the late letter printing SL190.                                   *
*>====================================================================================*
*>
*>#####################################################################################
*> Open Source users without IT support or skills in Cobol can contact programmer     #
*>  support at Applewood Computers for assistance in this or any other matter.        #
*>#####################################################################################
*>
*>*************************************************************************************
*>
 identification          division.
*>===============================
*>
      program-id.         sl110.
*>*
*>    Author.             V B Coen FBCS, FIDM, FIDPM, 24/10/83
*>                        For Applewood Computers.
*>**
*>    Security.           Copyright (C) 1976-2025 & later, Vincent Bryan Coen.
*>                        Distributed under the GNU General Public License.
*>                        See the file COPYING for details.
*>**
*>    Remarks.            Statement Production.
*>                        Setup to Use pre-printed continous forms or a laser Template file
*>                        Printer set to spool 2.
*>
*>**
*>    Version.            See Prog-Name In Ws.
*>
*>    Called modules.     maps04.
*>                        acas012  ->
*>                         salesMT
*>                        acas019  ->
*>                         otm3MT.     ONLY for Table processing.
*>**
*>    Error messages used.
*>                        SL003
*>                        SL150
*>                        SL151
*>**
*>    changes.
*> 14/02/83 vbc - 053970-990
*> 23/03/83 Vbc - Correct Flavour Lines.
*> 31/03/83 Vbc - Correct Name, Address, Amount Lines For Letter.
*> 02/05/83 Vbc - Stop Letter Routine Using Closed Records.
*> 24/10/83 Vbc - Cis Cobol Conversion.
*> 01/03/84 Vbc - Support For Sales-Unapplied.
*> 30/03/84 Vbc - Dont Check For Oi-Invoice = 99999999,Not Applic
*>                 Remove Code For Letters, Now A Separate Program.
*> 02/10/84 Vbc - Support For Sixth Address Line.
*> 30/11/84 Vbc - Fix Bug Re Flavour2.
*> 03/03/09 vbc - Migration to Open Cobol v3.00.00.
*> 25/11/11 vbc - .04 Error msgs to SLnnn.Support for dates other than UK
*> 08/12/11 vbc - .05 Support for path+filenames.
*> 09/12/11 vbc -     Updated version to 3.01.nn
*> 11/12/11 vbc - .06 Changed usage of Stk-Date-Form to the global field Date-Form making former redundent.
*> 20/05/13 vbc - .07 Removed double sided printing from print-spool-command.cob.
*> 28/05/13 vbc - .08 Spooling set to prt-2 with PSN2
*>                    removed all outstanding Adobe Reader API code as its a chargable product.
*> 24/10/16 vbc - .09 ALL programs now using wsnames.cob in copybooks
*> 15/01/17 vbc - .   All programs upgraded to v3.02 for RDB processing.
*> 21/01/17 vbc - .10 Replace refs to maps99 by display msgs.
*> 25/01/17 vbc       Dry testing Completed.
*> 09/02/17 vbc - .11 Updated FD/WS for 019 and replaced cobol verbs
*>                    for access to FH/DALs when not using Cobol files.
*>                    Sort requirements removed (sl115) when processing
*>                    RDB tables as the sort step removed in place of
*>                    read Table by Order.
*>                    Note: When using RDB tables there is no sorting,
*>                    Data is from selecting by order.
*> 16/06/20 vbc - .12 Adding titles, heads etc if SL-Comp-Stat true
*> 10/12/22 vbc   .13 Added para after some sections 4 GC 3.2 warning msgs.
*> 07/04/23 vbc - .14 Change FDprint from 132 to 92 for max print line size
*>                    stops blank line printing on HP 7305 Inktank printers.
*>                    Will need to check that there are no other progs printing
*>                    Portrait format. Yes, work fine for HP8600 !.
*>                .15 Amend the lpr spool command copybook to
*>                    print-command-p-dispatch for singled-sided printing.
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
 copy "envdiv.cob".
 input-output            section.
 file-control.
*>-----------
*>
*> copy "selsl.cob".
 copy "slselois.cob".
 copy "selprint.cob" replacing "prt-1" by "prt-2".
 data                    division.
 file section.
*>-----------
*>
*> copy "fdsl.cob".
 copy "slfdois.cob".
 copy "fdprint.cob" replacing ==x(132)== by ==x(92)==.
*>
 working-storage section.
*>----------------------
 77  prog-name           pic x(15) value "SL110 (3.02.15)".
*>
 copy "print-spool-command-p-dispatch.cob" replacing "prt-1" by "prt-2".
 copy "wsmaps03.cob".
 copy "wsfnctn.cob".
*> copy "wsoi.cob".
 copy "slwsoi3.cob".
 copy "wssl.cob".
*>
*> REMARK OUT ANY IN USE
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
*>     03  WS-Sales-Record        pic x.
     03  WS-Value-Record        pic x.
     03  WS-Delivery-Record     pic x.
     03  WS-Analysis-Record     pic x.
     03  WS-Del-Inv-Nos-Record  pic x.
     03  WS-Purch-Record        pic x.
     03  WS-Pay-Record          pic x.
     03  WS-Invoice-Record      pic x.
*>     03  WS-OTM3-Record         pic x.
     03  WS-PInvoice-Record     pic x.
     03  WS-OTM5-Record         pic x.
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
 copy "Test-Data-Flags.cob".  *> set sw-testing to zero to stop logging.
*>
 01  flavour-line1                            value spaces.
     03  filler          pic x.
     03  flavour1        pic x(64).
 01  flavour-line2                            value spaces.
     03  filler          pic x.
     03  flavour2.
         05 fl2-back-tag pic x.
         05 fl2-rest     pic x(63).
*>
 01  ws-data.
     03  menu-reply      pic 9.
     03  ws-reply        pic x.
     03  a               pic 99.
     03  y               pic 99.
     03  z               pic 99.
     03  ws-inv8         pic z(7)9.
     03  ws-inv redefines ws-inv8 pic x(8).
     03  truth           pic 9.
         88  a-true                        value  1.
         88  a-false                       value  0.
     03  address-A       pic x(96).
     03  address-line    pic x(36).
     03  first-time      pic 9           value zero.
     03  ws-spaces       pic x(80)       value spaces.
     03  st-date         pic x(10)       value spaces.
     03  work-1          pic s9(6)v99.
     03  work-2          pic s9(6)v99.
     03  i               pic 99.
     03  j               pic 99.
     03  k               pic 99.
     03  l-p             pic 99 value zero.
     03  customer-in     pic x(7) value space.
     03  filler  redefines  customer-in.
         05  array-l     pic x  occurs  7.
     03  pay-date        binary-long        value zero.
     03  pay-value       pic s9(6)v99       value zero.
     03  pay-paid        pic s9(6)v99.
     03  escape-code     pic x.
     03  last-read       pic x(7)           value spaces.
     03  inv-amount      pic s9(7)v99.
     03  amount-out      pic s9(7)v99.
     03  pay-amount      pic s9(7)v99.
*>
 01  ws-amount-screen-display.
     03  ws-poundsd      pic 9(6).
     03  ws-period       pic x              value ".".
     03  ws-penced       pic v99.
 01  ws-amount-screen-accept redefines ws-amount-screen-display.
     03  ws-pound        pic 9(6).
     03  filler          pic x.
     03  ws-pence        pic v99.
*>
 01  ws-amount-work.
     03  amt-wk-pds      pic 9(6).
     03  amt-wk-pence    pic v99.
 01  ws-amount-ok redefines ws-amount-work.
     03  amt-ok          pic 9(6)v99.
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
 01  Error-Messages.
*> System Wide
     03  SL003          pic x(28) value "SL003 Hit Return To Continue".
*> Module specific
     03  SL150          pic x(39) value "SL150 Re-align statement to top of form".
     03  SL151          pic x(35) value "SL151 Sales Transactions Not Posted".
*>
 01  balances        comp-3.
     03  bal-0           pic s9(7)v99 value zero.
     03  bal-30          pic s9(7)v99 value zero.
     03  bal-60          pic s9(7)v99 value zero.
     03  bal-90          pic s9(7)v99 value zero.
     03  bal-t           pic s9(7)v99 value zero.
*>
 01  line-1.  *> 56
     03  filler          pic x(24)       value spaces.
     03  l1-s-name       pic x(32).
*>
 01  line-2. *> 92
     03  filler          pic x(24)       value spaces.
     03  l2-s-address    pic x(24).
     03  filler          pic x(21)       value spaces.
     03  l2-r-name       pic x(23).
*>
 01  line-3.
     03  filler          pic x(24)       value spaces.
     03  l3-s-address    pic x(24).
     03  filler          pic x(21)       value spaces.
     03  l3-r-address    pic x(23).
*>
 01  line-4.
     03  filler          pic x(24)       value spaces.
     03  l4-s-address    pic x(24).
     03  filler          pic x(21)       value spaces.
     03  l4-r-address    pic x(23).
*>
 01  line-5.
     03  filler          pic x(24)       value spaces.
     03  l5-s-address    pic x(24).
     03  filler          pic x(21)       value spaces.
     03  l5-r-address    pic x(23).
*>
 01  line-7.
     03  filler          pic x(9)        value spaces.
     03  l7-c-name       pic x(32).
*>
     03  L7-Heads                        value spaces.
         05  filler      pic x(17).
         05  L7-Date-Hd1 pic x(4).        *> 'DATE'
         05  filler      pic x(7).
         05  L7-Date-Hd2 pic x(4).        *> 'DATE'
         05  filler      pic x(7).
         05  L7-Cust-Hd1 pic X(8).        *> 'CUST.No.'
*>
 01  line-8.                       *> 1st line addr  91
     03  filler          pic x(9)        value spaces.
     03  l8-c-address    pic x(32).
     03  filler          pic x(15)       value spaces.
     03  l8-s-date       pic x(10).
     03  filler          pic x(3)        value spaces.
     03  l8-r-date       pic x(10).
     03  filler          pic x(5)        value spaces.
     03  l8-r-cust       pic x(7).
*>
 01  line-9.                       *> 2nd line addr
     03  filler          pic x(9)        value spaces.
     03  l9-c-address    pic x(32).
*>
 01  line-10.                       *> 3rd line addr
     03  filler          pic x(9)        value spaces.
     03  l10-c-address   pic x(32).
*>
 01  line-11.                       *> 4th line addr
     03  filler          pic x(9)        value spaces.
     03  l11-c-address   pic x(32).
     03  filler          pic x(18)       value spaces.
     03  l11-s-cust      pic x(7).
*>
 01  line-12.
     03  filler          pic xx          value spaces.
     03  l12-s-invoice   pic z(7)9       blank when zero.
     03  filler          pic xxx         value spaces.
     03  l12-date        pic x(11).       *> 24
     03  l12-desc        pic x(31).       *> 55
     03  l12-s-amount    pic z(6)9.99cr.  *> 67
     03  filler          pic xxx         value spaces.   *> 70
     03  l12-r-invoice   pic z(7)9       blank when zero.  *> 78
     03  l12-r-amount    pic z(7)9.99cr.                   *> 91
*>
 01  line-13.
     03  l13-amount1     pic z(6)9.99cr.
*>    blank when zero.
     03  filler          pic x(2)        value spaces.    *> 14
     03  l13-amount2     pic z(6)9.99cr.                 *> 26
*>    blank when zero.
     03  filler          pic x           value spaces.
     03  l13-amount3     pic z(6)9.99cr.                 *> 13 = 39
*>    blank when zero.
     03  filler          pic x           value spaces.
     03  l13-amount4     pic z(6)9.99cr.                 *> 52
*>    blank when zero.
     03  filler          pic x           value spaces.
     03  l13-amount5     pic z(6)9.99cr.                 *> 65
*>    blank when zero.
     03  filler          pic x(12)       value spaces.
     03  l13-amount6     pic z(6)9.99cr.                 *> 24 - 89
*>    blank when zero.
*>
 01  line-14.
     03  filler          pic x(9)        value spaces.
     03  l14-c-address   pic x(32).
     03  L14-Heads                       value spaces.
         05  filler      pic x(15).
         05  L14-Cust-HD pic x(8).    *> 'CUST.No.'
*>
 01  Line-15.
     03  filler          pic x(9)        value spaces.
     03  L15-Ret-Head    pic x(51)       value "PLEASE RETURN A COPY OF STATEMENT WITH YOUR PAYMENT".
*>
 01  Line-16.                 *> Print before L12
     03  L16-Inv-HD1     pic x(9)          value "INVOICE #".
     03  filler          pic x(6)          value spaces.
     03  L16-Date-HD     pic x(4)          value "DATE".
     03  filler          pic x(5)          value spaces.    *> 24
     03  L16-Exp-HD      pic x(11)         value "EXPLANATION". *> 35
     03  filler          pic x(23)         value spaces.          *> 58
     03  L16-Amd-HD1     pic x(6)          value "AMOUNT".       *> 64
     03  filler          pic x(5)          value spaces.         *> 69
     03  L16-Inv-HD2     pic x(9)          value "INVOICE #".     *> 78
     03  L16-Amd-HD2     pic x(10)         value "    AMOUNT".    *> 88
*>
 01  Line-Total-Heads.
     03  filler          pic x(5)         value spaces.
     03  filler          pic x(7)         value "CURRENT".    *> 12
     03  filler          pic x(7)         value spaces.
     03  filler          pic x(7)         value "30 DAYS".    *> 26
     03  filler          pic x(6)         value spaces.
     03  filler          pic x(7)         value "60 DAYS".    *> 39
     03  filler          pic x(5)         value spaces.
     03  filler          pic x(8)         value "90+ DAYS".   *> 52
     03  filler          pic x(4)         value spaces.
     03  filler          pic x(9)         value "TOTAL DUE".   *> 65
     03  filler          pic x(13)         value spaces.
     03  filler          pic x(9)         value "TOTAL DUE".   *> 88
*>
 linkage section.
*>**************
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
*>
     if       FS-Cobol-Files-Used
              call  "CBL_CHECK_FILE_EXIST" using File-19    *> Open ITM3 file
                                                 File-Info
              if    return-code not = zero          *> not = zero - No file found
                    display SL151   at 2301
                    display SL003   at 2401
                    accept ws-reply at 2430
                    go to menu-exit
              end-if
     end-if
*>
*> Force Esc, PgUp, PgDown, PrtSC to be detected
*>
     set      ENVIRONMENT "COB_SCREEN_EXCEPTIONS" to "Y".
     set      ENVIRONMENT "COB_SCREEN_ESC" to "Y".
     move     Print-Spool-Name2 to PSN2.			*> Spool 2
     move     1  to File-Key-No.
*>
     move     to-day to u-date.
     display  prog-name at 0202 with foreground-color 2 erase eos.
     display  "Statement Production" at 0232 with foreground-color 2.
     perform  zz070-Convert-Date.
     display  ws-date at 0271 with foreground-color 2.
*>
*> Check if heads needed if so set up lits
*>
     if       SL-Comp-Stat
              move     spaces     to Line-7
              move     "DATE"     to L7-Date-Hd1
                                     L7-Date-Hd2
              move     "CUST.No." to L7-Cust-Hd1
                                     L14-Cust-HD.

     perform  Statements.
*>
     call     "SYSTEM" using Print-Report.
*>
 menu-exit.
*>********
*>
     exit     program.
*>
 accept-money.
*>-----------
*>
     move     zero to ws-poundsd amt-ok ws-penced.
     display  ws-amount-screen-display at curs  with foreground-color 3.
     accept   ws-amount-screen-accept at curs   with foreground-color 3 update.
     move     ws-pound to amt-wk-pds.
     move     ws-pence to amt-wk-pence.
*>
*>****************************************************************
*>                  P r o c e d u r e s                          *
*>****************************************************************
*>
 statements              section.
*>==============================
*>
     display  "****************************************" at 0441 with foreground-color 2.
     display  "*Date    [  /  /    ]*                 *" at 0541 with foreground-color 2.
     display  "*A/C Nos    [       ]*               ***" at 0641 with foreground-color 2.
     display  "*Value    [         ]*                 *" at 0741 with foreground-color 2.
     display  "****************************************" at 0841 with foreground-color 2.
*>
     display  "Notes" at 1101 with foreground-color 2.
     display  "*****" at 1201 with foreground-color 2.
     display  " (1)  -   <Date>  :  Appears on statement" AT 1401 with foreground-color 2.
     display  " (2)  -   <A/C>   :  A/C's to match for printing" at 1501 with foreground-color 2.
     display  " (3)  -   <Value> :  Minimum O/S to print" AT 1601 with foreground-color 2.
*>
 date-input.
*>*********
*>
     display  ws-date at 0551 with foreground-color 3.
     accept   ws-date at 0551 with foreground-color 3 update.
     if       ws-date = spaces
              go to main-exit.
*>
     move     ws-date to ws-test-date.
     perform  zz050-Validate-Date.
     if       u-bin = zero
              go to  date-input.
*>
*>     move     u-date  to  st-date.  *> UK default date, ws-test-date is in display/accept format
     move     u-bin to pay-date.      *> this one is in binary days from 01/01/1600
*>
 customer-input.
*>*************
*>
     accept   customer-in at 0654 with foreground-color 2 update.
     move     function upper-case (customer-in) to customer-in.
*>
 value-input.
*>**********
*>
     move     0752 to curs.
     perform  accept-money.
     move     amt-ok to pay-value.
*>
 flavour-in.
*>*********
*>
     display  "Msg Line 1 - [" at 1801 with foreground-color 2.
     display  "]" at 1879 with foreground-color 2.
     display  "Msg Line 2 - [" at 1901 with foreground-color 2.
     display  "]" at 1979 with foreground-color 2.
*>
     accept   flavour1 at 1815 with foreground-color 3 update.
     accept   flavour2 at 1915 with foreground-color 3 update.
     if       fl2-back-tag = "<"
              go to flavour-in.
*>
     if       FS-Cobol-Files-Used
              open     input  open-item-file-s
              if       fs-reply not = zero
                       close open-item-file-s
                       go to Main-End
              end-if
     else
              perform  OTM3-Open-Input
              if       fs-reply not = zero
                       perform OTM3-Close
                       go to Main-End
              end-if
     end-if
     perform  Sales-Open-Input.
     open     output print-file.
*>
     if       customer-in = spaces
              go to read-sales.
*>
     move     customer-in to WS-Sales-Key.
     set      fn-not-less-than to true.
     move     1  to File-Key-No.
     perform  Sales-Start.                   *> start sales-file key not < WS-Sales-Key invalid key
     if       fs-reply = 21
              move 1 to y.
     inspect  customer-in replacing all spaces by "Z".
*>
 read-sales.
*>*********
*>
     perform  Sales-Read-Next.        *> read sales-file next record at end
     if       fs-reply = 10
              go to  main-end.
*>
     if       sales-unapplied not = zero
              subtract sales-unapplied from sales-current.
*>
     if       sales-current  = zero
              go to read-sales.
*>
     if       pay-value not = zero
       and    sales-current  <  pay-value
              go to  read-sales.
*>
     if       customer-in not =   spaces
        and   WS-Sales-Key > customer-in
              go to main-end.
*>
     move     sales-address  to  address-A.
*>
     move     sales-name  to  l7-c-name.
*>
     move     1  to  a.
     unstring address-A  delimited  by  sl-delim
              into  address-line count a pointer  a.
     move     address-line  to  l8-c-address.
*>
     move     spaces  to  address-line.
     unstring address-A  delimited  by  sl-delim
              into  address-line count a pointer  a.
     move     address-line  to  l9-c-address.
*>
     move     spaces  to  address-line.
     unstring address-A delimited  by  sl-delim
              into  address-line count a pointer  a.
     move     address-line  to  l10-c-address.
*>
     move     spaces to address-line.
     unstring address-A delimited  by  sl-delim
              into  address-line  count a pointer  a.
     move     address-line  to  l14-c-address.
*>
     move     spaces to address-line.
     unstring address-A delimited  by  sl-delim
              into  address-line  count a pointer  a.
     move     address-line  to  l11-c-address.
*>
     move     usera  to l1-s-name         l2-r-name.
     move     address-1  to  l2-s-address l3-r-address.
     move     address-2  to  l3-s-address l4-r-address.
     move     address-3  to  l4-s-address l5-r-address.
     move     address-4  to  l5-s-address.
*>
*> Note that ws-test-date still has the pre-converted date from the accept
*>
     move     ws-test-date  to  l8-s-date l8-r-date.
*>
     move     WS-Sales-Key  to  l8-r-cust l11-s-cust.
*>
     perform  headings-1.
*>
     if       sales-unapplied = zero
              go to read-open-item.
*>
*> Note that ws-test-date still has the pre-converted date from the accept
*>
     move     zero to l12-s-invoice l12-r-invoice.
     move     ws-test-date to l12-date.
     move     "Unapplied Credit Balance" to l12-desc.
     multiply -1 by sales-unapplied.
     move     sales-unapplied to l12-s-amount l12-r-amount.
     add      sales-unapplied to bal-0.
     add      sales-unapplied to bal-t.
     add      1 to l-p.
     write    print-record from line-12 after 1.
*>
 Read-Open-Item.
*>*************
*>
*> Here we have two choices
*> 1: If using files (Cobol), work via the sort file
*>    to help select records then read the Sales file.
*> 2: If using RDB tables then process via ORDERED OTM3
*>     table  therefore only work with the one table & row
*>      [instead of 2 files].
*>
*> This way we get rid of a sort for Table processing as all done in RDBMS.
*>
     if       last-read = spaces
              if       NOT FS-Cobol-Files-Used
                       perform OTM3-Read-Next-Sorted-By-Cust
                       if      fs-reply not = zero     *> = 10
                               perform  End-Statement thru End-Stat-Do
                               perform OTM3-Close
                               go to Main-End
                       end-if
                       go to Read-Loop-Tests
              end-if
*>
              read  open-item-file-s  record at end
                    perform  End-Statement thru End-Stat-Do
                    close    open-item-file-s
                    go to    Main-End
     end-if.
*>
     move     open-item-record-s  to  oi-header.
*>
 Read-Loop-Tests.
     move     oi-customer  to  last-read.
*>
     if       last-read  >  WS-Sales-Key
              go to  end-statement.
*>
     if       last-read  not = WS-Sales-Key
              move  spaces  to  last-read
              go to  read-open-item.
*>
*> O-I transaction is for this customer.
*>
     if       oi-type = 1  or  4
              move  spaces  to  last-read
              go to  read-open-item.
*>
*> Can safely ignore receipts & pro-formas.
*>
     if       oi-type = 5 or 6
              move  zero  to  oi-invoice.
*>
     move     oi-invoice  to  l12-s-invoice
                              l12-r-invoice.
*>
     move     oi-date  to  u-bin.
     perform  zz060-Convert-Date.
     move     ws-date   to  l12-date.
*>
     move     spaces  to  l12-desc.
     move     1  to  z.
*>
     if       oi-type = 2
              string  "Goods As Per - "  delimited  by size
                         into l12-desc  pointer  z
              string  oi-description  delimited  by  "  "
                         into l12-desc  pointer  z
     else
      if      oi-type = 3
              move  1  to  y
              string  "Credit Note Re. Inv - " delimited by size
                         into l12-desc  pointer  y
              move oi-cr to ws-inv8
              string  ws-inv delimited  by  size
                         into l12-desc  pointer  y
      else
       if     oi-type = 6
              move "Unapplied Credit Allocated" to l12-desc
       else
              move  "Payment. Received With Thanks" to  l12-desc.
*>
     if       oi-type = 2 and
              oi-description = spaces
              move "Goods" to l12-desc.
*>
     add      oi-net        oi-extra      oi-carriage
              oi-vat        oi-discount   oi-e-vat
              oi-deduct-amt oi-deduct-vat oi-c-vat     giving  inv-amount.
*>
*> Invoices Only Here
*>
     if       oi-type = 2
              subtract   oi-paid  from  inv-amount  giving  amount-out
              add   amount-out oi-p-c  giving l12-s-amount
              move  amount-out  to  l12-r-amount.
     if       oi-type = 2
       and    amount-out = zero
       and    oi-p-c = zero
              move  inv-amount  to  l12-s-amount.
*>
*> Cr. Notes Only Here.
*>
     if       oi-type = 3
              move  inv-amount to  l12-s-amount
              add   oi-paid    to inv-amount
              move inv-amount  to l12-r-amount.
*>
*> Payments  Only Here.
*>
     if       oi-type = 5  or  6
              if  oi-paid  not negative
                  multiply  -1  by  oi-paid.
     if       oi-type = 5  or  6
              if  oi-approp  not negative
                  multiply  -1  by  oi-approp.
     if       oi-type = 5  or  6
              subtract oi-approp from oi-paid giving amount-out
              move  amount-out  to  l12-r-amount
              move  oi-paid  to  l12-s-amount.
*>
     subtract oi-date  from  pay-date  giving  work-1.
*>
*> Now work out ageing
*> (dont worry if paid in full - routine still works ok)
*>
     if       work-1  <  30
              add  amount-out  to  bal-0
     else
      if      work-1  <  60
              add  amount-out  to  bal-30
      else
       if     work-1  <  90
              add  amount-out  to  bal-60
       else
              add  amount-out  to  bal-90.
*>
     add      amount-out  to  bal-t.
*>
 bypass-ageing.
*>
     move     zero  to  amount-out.
*>
*> Now to print. What joy !!!!!!.
*>
     add      1  to  l-p.
*>
     if       l-p  >  28
              move  spaces  to  print-record
              write  print-record after 8 lines
              perform  headings-1
              move  1  to  l-p.
*>
     write    print-record  from  line-12 after 1.
*>
*> Loop back for next item....
*>
     move     spaces  to  last-read.
     go       to read-open-item.
*>
*> Can only get here at the end of a statement.
*>
 end-statement.
*>************
*>
*> Now let us (or cabbage) compute the skip......wow !!!!!!
*>
     compute  l-p  =  32  -  l-p.
*>
     if       l-p  <  6
              go to  end-stat-do.
     subtract 4  from  l-p.
     write    print-record  from  flavour-line1 after l-p.
     write    print-record  from  flavour-line2 after 1.
     move     3  to  l-p.
*>
 end-stat-do.
*>**********
*>
     if       SL-Comp-Stat
              subtract 1 from l-p
              write  print-record from Line-Total-Heads after l-p lines
              move   1 to l-p.

     move     bal-0   to  l13-amount1.
     move     bal-30  to  l13-amount2.
     move     bal-60  to  l13-amount3.
     move     bal-90  to  l13-amount4.
     move     bal-t   to  l13-amount5
                          l13-amount6.
*>
     write    print-record from line-13 after l-p lines.
*>
*> Set-up the form for the next one then zeroise counter fields
*>
     move     spaces  to  print-record.
     write    print-record after 4 lines.
*>
*> As promised some zeroising.
*>
     move     zero  to  bal-0   bal-30
                        bal-60  bal-90
                        bal-t   l-p.
*>
*> Loop time.......get another punter.
*>
 end-statement-end.
*>
     go       to read-sales.
*>
 Main-End.
*>*******
*>
     close    print-file.                    *> open-item-file-s sales-file
     perform  Sales-Close.
     if       FS-Cobol-Files-Used             *> Make it a null file
              close open-item-file-s
              open  output open-item-file-s   *> clear it.
              close open-item-file-s
     end-if.
*>
*>================================================================================================*
*>  Here you might want to change the spool name to reflect the printer containing any preprinted *
*>   special paper, eg continuous forms or a laser that has the statement Template pre-loaded.    *
*>    If preprinted paper has company details on unset the SL-Comp-Stat field in system setup.    *
*>================================================================================================*
*>
 main-exit.   exit section.
*>********    ****
*>
 headings-1          section.
*>==========================
*>
 Headings-1-Main.
     write    print-record  from  line-1 after 1.
     write    print-record  from  line-2 after 1.
     write    print-record  from  line-3 after 1.
     write    print-record  from  line-4 after 1.
     write    print-record  from  line-5 after 1.
     write    print-record  from  line-7 after 2.
     write    print-record  from  line-8 after 1.
     write    print-record  from  line-9 after 1.
     write    print-record  from  line-10 after 1.
     write    print-record  from  line-14 after 1.
     write    print-record  from  line-11 after 1.
*>
     if       first-time  not = zero
              go to  main-end.
*>
*>####################################################################################################
*>  These lines are for directly used printers, spooled systems do not need this unless the spooler
*>       can not handle alignment tests etc so adjust to suit your needs. For those spoolers that
*>       do, just hit return on first question  if code left in but a bypass has been added so that
*>      this block of code is NOT run - REMOVE or remark it, the Go TO MAIN-END
*>####################################################################################################
*>
*> Paper alignment is bypassed for paper in inkjet, laser type printers
*>    if needed just add *> at the start of next line (GO TO MAIN-END.
*>
     Go TO MAIN-END.
*>
     display  "Statement correctly aligned.... (Y/N) ? - [ ]" at 2001 with foreground-color 2.
*>
     move     "Y"  to  ws-reply.
     accept   ws-reply at 2044 with foreground-color 6 update.
     move     function upper-case (ws-reply) to ws-reply.
*>
     if       ws-reply = "Y"
              go to  main-end.
*>
     display  SL150    at 2101 with foreground-color 2. *> realign paper msg
     display  SL003    at 2201 with foreground-color 2. *> hit ret
     accept   ws-reply at 2044 with foreground-color 2.
*>
     display  " " at 2101 with erase eol.
     display  " " at 2201 with erase eol.
     go       to headings-1-Main.
*>
 main-end.
*>*******
*>
     if       first-time = zero
              display ws-spaces at 2001.
     move     1 to first-time.
     move     spaces  to  print-record.
     if       SL-Comp-Stat
              write print-record from Line-15 after 1
              move   spaces to Print-Record
              write print-record after 1
              write print-record from Line-16 after 1
     else
              write    print-record after 3 lines.
*>
 main-exit.   exit section.
*>
 zz050-Validate-Date        section.
*>*********************************
*>
*>  Converts USA/Intl to UK date format for processing.
*>****************************************************
*> Input:   ws-test-date
*> output:  u-date/ws-date as uk date format
*>          u-bin not zero if valid date
*>
     move     ws-test-date to ws-date.
     if       Date-Form = zero
              move 1 to Date-Form.
     if       Date-UK
              go to zz050-test-date.
     if       Date-USA                *> swap month and days
              move ws-days to ws-swap
              move ws-month to ws-days
              move ws-swap to ws-month
              go to zz050-test-date.
*>
*> So its International date format
*>
     move     "dd/mm/ccyy" to ws-date.  *> swap Intl to UK form
     move     ws-test-date (1:4) to ws-Year.
     move     ws-test-date (6:2) to ws-Month.
     move     ws-test-date (9:2) to ws-Days.
*>
 zz050-test-date.
     move     ws-date to u-date.
     move     zero to u-bin.
     perform  maps04.
*>
 zz050-exit.
     exit     section.
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
 zz080-Issue-Email  section.
*>*************************
*>
*> This one for mailx - the variables not created !
*>
  *>   STRING   "echo "
  *>            FUNCTION TRIM (mail-body TRAILING)
  *>            " | mailx -r "
  *>            FUNCTION TRIM (mail-from-address TRAILING)
  *>            " -s "
  *>            FUNCTION TRIM (mail-subject TRAILING)
  *>            " -a "
  *>            FUNCTION TRIM (mail-attachment-filename TRAILING)
  *>            " "
  *>            FUNCTION TRIM (mail-to-address TRAILING)
  *>            x"00" DELIMITED BY SIZE
  *>                   INTO mail-command.
*>
 zz080-Exit.  exit section.
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
