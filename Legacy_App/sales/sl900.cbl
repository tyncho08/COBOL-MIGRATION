       >>source free
*>****************************************************************
*>                                                               *
*>           Invoice  Menu  &  Fixed  Data  Maintenance          *
*>                                                               *
*>****************************************************************
*>
 identification          division.
*>================================
*>
      program-id.         sl900.
*>**
*>    Author.             Cis Cobol Conversion By V B Coen FBCS, FIDM, FIDPM, 26/10/83
*>                        For Applewood Computers.
*>**
*>    Security.           Copyright (C) 1976-2025 & later, Vincent Bryan Coen.
*>                        Distributed under the GNU General Public License.
*>                        See the file COPYING for details.
*>**
*>    Remarks.            Invoice Menu & Fixed Data.
*>**
*>    Version.            See Prog-Name In Ws.
*>**
*>    Called Modules.     sl200
*>                        sl910
*>                        sl920
*>                        sl930
*>                        sl940
*>                        sl950.
*>                        { sl800 }   auto gen. currently rem'd out
*>**
*>    Error messages used.
*>                        SL121
*>**
*> Changes:
*> 03/03/09 vbc - .03 Migration to Open Cobol v3.00.00.
*> 13/03/09 vbc - .04 Added support for Extra-Print needed in inv print module.
*> 26/11/11 vbc - .05 Error msgs to SLnnn.Support for dates other than UK
*>                    Invoicing values displayed changed from 2 & 3 to 1 & 2 as later
*>                    used in test, see fixed-data section. MUST check
*>                    SL910 thru sl960 (and any others) to see what they use.
*> 09/12/11 vbc -     Updated version to 3.01.nn
*> 11/12/11 vbc - .06 Changed usage of Stk-Date-Form to the global field
*>                    Date-Form making former redundent.
*> 24/03/12 vbc - .07 Added in missing Delivery note / Picking List Print.
*>                    How did I miss that?
*> 16/04/13 vbc - .08 Clean up discount/charge fields if descrition is blank.
*> 15/05/13 vbc - .09 Fixed data: Added support for SL-Stock-Link & SL-Stock-Audit.
*> 24/10/16 vbc - .10 ALL programs now using wsnames.cob in copybooks
*> 15/01/17 vbc - .   All programs upgraded to v3.02 for RDB processing.
*>                    Clean up of some tests in menu capture.
*> 25/01/17 vbc       Dry testing completed.
*> 20/03/18 vbc - .11 Forced Stock audit if stock link set - Must be used.
*>                    Issue warning if invoicer is set to 1.
*>                    Remark out Extra-Charge-AC disp, accept & process as not
*>                    used anywhere.
*> 02/04/18 vbc - .12 Started work on support for Autogen.using sl800 ?
*> 19.03.24 vbc - .13 Support for sl970 BO reporting and amendments.
*> 16/04/24 vbc       Copyright notice update superseding all previous notices.
*>
*>                    QUESTIONS arise regarding system usage of param fields:
*>     Extra-Charge-AC  NOT USED anywhere in the O/S version.
*>     Vat-AC           Used in sl060 for GL posting only.
*>           within the General-Ledger-Block.
*> SO FAR CANNOT FIND USAGE OF Extra-Charge-AC param field anywhere in SL
*>    but if any where it would be in sl100 Cash Posting.
*>      May be for a later function upgrade that was never implemented.
*>
*>  These are displayed at para Fixed-Data and accepted at Data-Entry.
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
*>================================
*>
 copy "envdiv.cob".
*>
 data                    division.
*>================================
*>
 working-storage section.
*>-----------------------
 77  prog-name               pic x(15) value "SL900 (3.02.12)".
*>
 copy "wsmaps03.cob".
 copy "wsfnctn.cob".
*>
 copy "Test-Data-Flags.cob".  *> set sw-testing to zero to stop logging.
*>
 01  ws-data.
     03  menu-reply          pic x.
     03  ws-reply            pic x.
     03  ws-next-invoice     pic 9(8).
     03  ws-extra-rate       pic 99v99.
     03  ws-pf-retention     pic 999.
     03  ws-extra-charge-ac  pic 9(6).
     03  ws-vat-ac           pic 9(6).
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
*> Module specific
     03  SL121           pic x(31) value "SL121 Invalid option, try again".
*>
 01  error-code          pic 999.
*>
 01  ws-discount-display.
     03  ws-disca1       pic 99.
     03  ws-disca2       pic x        value ".".
     03  ws-disca3       pic v99.
 01  ws-discount-accept redefines ws-discount-display.
     03  ws-discb1       pic 99.
     03  filler          pic x.
     03  ws-discb3       pic v99.
 01  ws-discount-work.
     03  ws-disc-wka     pic 99.
     03  ws-disc-wkb     pic v99.
 01  ws-discount redefines ws-discount-work  pic 99v99.
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
*> Force Esc, PgUp, PgDown, PrtSC to be detected
     set      ENVIRONMENT "COB_SCREEN_EXCEPTIONS" to "Y".
     set      ENVIRONMENT "COB_SCREEN_ESC" to "Y".
*>
     perform  zz070-Convert-Date.
     move     ws-caller to ws-del-link.
     move     ws-called to ws-caller.
     if       next-invoice  = zero
              perform  fixed-data.
*>
 menu-return.
     move     zero  to  menu-reply.
     display  prog-name at 0101 with erase eos foreground-color 2.
     display  "Invoicing Menu" at 0134 with foreground-color 2.
     display  ws-date at 0171 with foreground-color 2.
*>
     display  "Select one of the following by number :- [ ]"  at 0501 with foreground-color 2.
     display  "(1)  Enter or Amend Invoicing Fixed Data"      at 0811 with foreground-color 2.
*>
     if       not-invoicing
              display "(2)  Enter Invoice Details"      at 0911 with foreground-color 2
     else
              display "(2)  Enter Invoices For Batch Printing"     at 0911 with foreground-color 2
              display "(3)  Enter Invoice For Immediate Printing"  at 1011 with foreground-color 2
              display "(7)  Print Delivery Notes/Picking Lists"    at 1411 with foreground-color 2.
              display "(8)  Print Invoices"                        at 1511 with foreground-color 2
*>
     display  "(4)  Amend Invoices" at 1111                     with foreground-color 2.
     display  "(5)  Delete Invoices" at 1211                    with foreground-color 2.
     display  "(6)  Invoice Deletion Report" at 1311            with foreground-color 2.
     display  "(9)  Recurring Invoice Processing" at 1611       with foreground-color 2.
     display  "(A)  BO Reporting and amendments"  at 1711       with foreground-color 2.
     display  "(X)  Return To System menu" at 1911              with foreground-color 2.
*>
 menu-input.
*>**********
*>
     accept   menu-reply at 0543 with foreground-color 6 auto UPPER.
*>
     if       menu-reply  = "X"
              go to  menu-exit.
*>
     if       not-invoicing
         and  (menu-reply  = 3 or 7 or 8)
              display SL121 at 2015 with foreground-color 3 highlight
              go to  menu-input.
*>
     if       menu-reply  = 1
              perform  Fixed-Data
              go to menu-return.
*>
     if       menu-reply  = 4
              move  menu-reply  to  pass-value
              move "sl920" to ws-called
              go to loadit.
*>
     if       menu-reply  = 5
              move  menu-reply  to  pass-value
              move "sl940" to ws-called
              go to loadit.
*>
     if       menu-reply  = 6
              move  menu-reply  to  pass-value
              move "sl200" to ws-called
              go to loadit.
*>
     if       menu-reply  = 7
              move   menu-reply  to  pass-value
              move "sl950" to ws-called
              go to loadit.
*>
     if       menu-reply  = 8
              move   menu-reply  to  pass-value
              move "sl930" to ws-called
              go to loadit.
*>
     if       menu-reply  = 2 or 3
              move   menu-reply  to  pass-value
              move "sl910" to ws-called
              go to loadit.
*>
     if       menu-reply  = 9                     *> Autogen
              move   menu-reply  to  pass-value
              move "sl800" to ws-called
              go to loadit.
*>
     if       Menu-Reply = "A"
              move "sl970" to WS-Called
              move zero to Pass-Value
              go to LoadIt.

     go       to menu-return.
*>
 loadit.
*>*****
*>
     call     ws-called using ws-calling-data
                              system-record
                              to-day
                              file-defs.
     go       to menu-return.
*>
 menu-exit.
*>********
*>
     move     zero        to  pass-value.
     move     ws-caller   to ws-called.
     move     ws-del-link to ws-caller.
*>
 menu-ex.
     exit     program.
*>
*>********************************************************
*>      P R O C E D U R E S                              *
*>********************************************************
*>
 Fixed-Data              section.
*>==============================
*>
     display  prog-name at 0101      with foreground-color 2 erase eos.
     display  "Invoicing Fixed Data" at 0131  with foreground-color 2.
     display  ws-date at 0171        with foreground-color 2.
*>
     display  "Next Invoice" at 0511 with foreground-color 2.
     display  "- [        ]" at 0545 with foreground-color 2.
*>
     display  "Extra Charge/Discount Description - ["   at 0711  with foreground-color 2.
     display  "]" at 0762            with foreground-color 2.
*>
     display  "Extra Charge/Discount Type (C/D)  - [ ]" at 0911 with foreground-color 2.
*>
     display  "Print Extra on Invoices (N/Y)     - [ ]" at 1011 with foreground-color 2.
     display  "Extra Charge/Discount Percentage  - [     ]" at 1111 with foreground-color 2.
*>
     display  "Invoice Entry Level (1, 2) "             at 1311 with foreground-color 2.
     display  "- [ ]" at 1345       with foreground-color 2.
     display  "Invoice to Stock Link  - [ ]"            at 1411 with foreground-color 2.
*>
     display  "Invoice to Stock Audit - [ ]"            at 1511 with foreground-color 2.
*>
*>  We could have NO GL or IRS, not a good idea as could cause problems with
*>   system when running.
*>  GL implies GL or IRS or Both.  Need to check processing happens to
*>    System Param fields = YES:
*>     Extra-Charge-AC    = NO is not used.
*>     Vat-AC           Used in sl060 for GL posting only (Not IRS).
*>           within the General-Ledger-Block.
*> SO FAR CANNOT FIND USAGE OF Extra-Charge-AC param field anywhere in SL.
*> ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
*>
*> Rem out Extra-charge-ac as not used.  20/03/18.
*>
     if       G-L or IRS
 *>             display "Extra Charge Discount Account"   at 1611 with foreground-color 2
 *>             display "- [      ]"  at 1645                     with foreground-color 2
              display "Vat Account" at 1711                     with foreground-color 2
              display "- [      ]"  at 1745                     with foreground-color 2
     end-if
*>
     display  "Proforma Retention In Days " at 1911             with foreground-color 2.
     display  "- [   ]" at 1945 with foreground-color 2.
*>
     display  "Details Ok To File (Y/N) ...........[Y]" at 2111 with foreground-color 2.
     move     next-invoice to ws-next-invoice.
     display  ws-next-invoice at 0548 with foreground-color 3.
     display  Extra-Desc      at 0748 with foreground-color 2.
     display  Extra-Type      at 0948    with foreground-color 3.
     display  Extra-Print     at 1048   with foreground-color 3.
     move     Extra-Rate  to ws-discount.
     move     ws-disc-wka to ws-disca1.
     move     ws-disc-wkb to ws-disca3.
     display  ws-discount-display at 1148 with foreground-color 3.
     display  invoicer        at 1348 with foreground-color 3.
     display  SL-Stock-Link   at 1437 with foreground-color 3.
     display  SL-Stock-Audit  at 1537 with foreground-color 3.
 *>    move     Extra-Charge-Ac to ws-extra-charge-ac.
 *>    display  ws-Extra-Charge-Ac at 1648 with foreground-color 3.
     move     VAT-Ac to ws-vat-ac.
     display  ws-vat-ac       at 1748 with foreground-color 3.
     move     pf-retention    to ws-pf-retention.
     display  ws-pf-retention at 1948 with foreground-color 3.
*>
 Data-Entry.
*>*********
*>
     accept   ws-next-invoice at 0548 with update  foreground-color 3.
     move     ws-next-invoice to next-invoice.
     if       next-invoice  = zero
              go to  data-entry.
*>
     accept   Extra-Desc at 0748 with foreground-color 2 update.
     if       Extra-Desc = spaces
              move spaces to Extra-Type Extra-Print
              move zero   to Extra-Rate
              go to entry-x.
*>
 Extra-Type-Entry.
     accept   Extra-Type at 0948  with foreground-color 3 update UPPER.
     if       Extra-Type not = "C" and not = "D"
              go to extra-type-entry.
*>
 Extra-Print-Entry.
     accept   Extra-Print at 1048 with foreground-color 3 update UPPER.
     if       Extra-Print not = "Y" and not = "N"
              go to Extra-Print-Entry.
*>
     accept   ws-discount-accept at 1148  with foreground-color 3 update.
     move     ws-disca1 to ws-disc-wka.
     move     ws-disca3 to ws-disc-wkb.
     move     ws-discount to Extra-Rate.
*>
 Entry-x.
*>
     if       Invoicer = zero
              move 2 to Invoicer.                        *> set default to 2 - Full invoicing + stock Control
     accept   invoicer at 1348 with foreground-color 3 update.
     if       invoicer not = 1 and not = 2               *> option 1 shows net, vat. 2 only shows totals & where
              go to entry-x.                             *>  does option 9 or 0 come from???????
     if       Invoicer = 1
              display "Weak option, Select 2 "  at 1351 with foreground-color 3 blink.
*>
 Stock-Link-Check.
     accept   SL-Stock-Link  at 1437  with update foreground-color 3 UPPER.
     if       SL-Stock-Link not = "Y" and not = "N"
              go to Stock-Link-Check.
     if       SL-Stock-Link = "N"
              move "N" to SL-Stock-Audit
              display  SL-Stock-Audit  at 1537 with foreground-color 3
              go to Extra-Chgs
     else
              move "Y" to SL-Stock-Audit
              display  SL-Stock-Audit  at 1537 with foreground-color 3.
*>
*> Audit is forced on if stock link set so cannot change it - It NEEDS to be used.
*>
 Audit-Check.
 *>    accept   SL-Stock-Audit  at 1537  with update foreground-color 3.
 *>    move     function upper-case (SL-Stock-Audit) to SL-Stock-Audit.
 *>    if       SL-Stock-Audit not = "Y" and not = "N"
 *>             go to Audit-Check.
*>
 Extra-Chgs.
     if       G-L or IRS
 *>             accept   ws-extra-charge-ac at 1648 with update   foreground-color 3
 *>             move     ws-extra-charge-ac to Extra-Charge-AC
              accept   ws-vat-ac at 1748 with foreground-color 3 update
              move     ws-vat-ac to Vat-AC
     end-if.
*>
 Confirmation.
*>***********
*>
     accept   ws-pf-retention at 1948   with foreground-color 3 update.
     move     ws-pf-retention to pf-retention.
     move     "Y"  to  ws-reply.
     accept   ws-reply at 2148 with foreground-color 6 update auto UPPER.
*>
     if       ws-reply = "N"
              go to  data-entry.
*>
     if       ws-reply not = "Y"
              go to  confirmation.
*>
 main-exit.   exit.
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
