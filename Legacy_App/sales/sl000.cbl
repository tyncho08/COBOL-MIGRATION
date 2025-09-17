       >>source free
*>****************************************************************
*>                                                               *
*>                   Sales  Ledger  Start Of Day                 *
*>                                                               *
*>****************************************************************
*>
 identification          division.
*>===============================
*>
*>**
      program-id.         sl000.
*>**
*>    Author.             Cis Cobol Conversion By V B Coen FBCS, FIDM, FIDPM, 1/11/82
*>                        For Applewood Computers.
*>**
*>    Security.           Copyright (C) 1976-2025 & later, Vincent Bryan Coen.
*>                        Distributed under the GNU General Public License.
*>                        See the file COPYING for details.
*>**
*>    Remarks.            Sales Ledger Start of Day Program.
*>
*>**
*>    Version.            see prog-name & date-comped in ws.
*>
*>    Called modules.     { maps01.  }         *> code remarked out.
*>                        maps04. (Replaces 3)
*>**
*>    Error messages used.
*>  System wide:
*>                        SL005
*>                        SY044.     Not used in O/S versions.
*>                        SY045.        -  -  Ditto  -  -
*>                        SY046.        -  -  Ditto  -  -
*>**
*> Changes:
*> 03/03/09 vbc - Migration to Open Cobol v3.00.00.
*>                Removed high security encryption code/modules
*>                as cannot be passed out of the UK as source.
*> 18/11/11 vbc - .01 Support for multi date formats (UK, USA, Intl)
*>                    Support for path+filenames (but not used in this module).
*> 09/12/11 vbc -     Updated version to 3.01.nn
*> 11/12/11 vbc - .02 Changed usage of Stk-Date-Form to the global field Date-Form making former redundent.
*> 24/10/16 vbc - .03 ALL programs now using wsnames.cob in copybooks
*> 15/01/17 vbc - .04 All programs upgraded to v3.02 for RDB processing.
*>                    Remarked out all maps01 procs for O/S versions.
*>                    Removed maps99 usage, abort msg made literal & remd out.
*> 16/04/24 vbc       Copyright notice update superseding all previous notices.
*> 28/07/25 vbc   .05 Force "/" for UK and USA into temp date field.
*>**
*>
*>*************************************************************************
*>
*>  From copyright.cob.
*>
*> Copyright Notice.
*> ****************
*>
*> These files and programs is part of the Applewood Computers Accounting
*> System and is copyright (c) Vincent B Coen. 1976-2025 and later.
*>
*> This program is now free software; you can redistribute it and/or modify it
*> under the terms of the GNU General Public License as published by the
*> Free Software Foundation; version 3 and later as revised for personal
*> usage only and that includes for use within a business but without
*> repackaging or for Resale in any way.
*>
*> Persons interested in repackaging, redevelopment for the purpose of resale or
*> distribution in a rental mode must get in touch with the copyright holder
*> with your commercial plans and proposals.
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
 copy  "envdiv.cob".
 input-output            section.
 file-control.
 data                    division.
 file section.
 working-storage section.
*>----------------------
 77  prog-name           pic x(15) value "SL000 (3.02.05)".
*> copy "wsmaps01.cob".
 copy "wsmaps03.cob".
 copy "wsfnctn.cob".
*>
 01  ws-data.
     03  menu-reply      pic 9.
     03  ws-reply        pic x.
     03  wsa-date.
       05  wsa-cc        pic 99.
       05  wsa-yy        pic 99.
       05  wsa-mm        pic 99.
       05  wsa-dd        pic 99.
     03  wsb-time.
       05  wsb-hh        pic 99.
       05  wsb-mm        pic 99.
       05  wsb-ss        pic 99.
       05  filler        pic xx.
     03  wsd-time.
       05  wsd-hh        pic 99.
       05  wsd-c1        pic x  value ":".
       05  wsd-mm        pic 99.
       05  wsd-c2        pic x  value ":".
       05  wsd-ss        pic 99.
*>
 01  ws-date-formats.
     03  ws-swap             pic xx.
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
     03  SL005           pic x(18) value "SL005 Invalid Date".
 *>    03  SY044           pic x(66) value "SY044 The system has detected an un-authorised change of user-name".
 *>    03  SY045           pic x(60) value "SY045 Contact your supplier and do not use the system again!".
 *>    03  SY046           pic x(33) value "SY046 Unauthorised Usage Aborting".
*>
 01  error-code          pic 999.
*>
 linkage section.
*>==============
*>
 01  to-day              pic x(10).
 copy "wsnames.cob".
 copy "wscall.cob".
 copy "wssystem.cob".
*>
 procedure  division using ws-calling-data
                           system-record
                           to-day
                           file-defs.
*>========================================
*>
 main.
*>***
*>
*> Force Esc, PgUp, PgDown, PrtSC to be detected
     set      ENVIRONMENT "COB_SCREEN_EXCEPTIONS" to "Y".
     set      ENVIRONMENT "COB_SCREEN_ESC" to "Y".
*>
     move     to-day to u-date.
*>
     accept   wsa-date from date YYYYMMDD.
     if       wsa-date not = "000000"
              move wsa-cc to u-cc
              move wsa-yy to u-yy
              move wsa-mm to u-month
              move wsa-dd to u-days.
*>
     move     u-date  to  to-day.
*>
     display  "Client -" at 0101 with foreground-color 2 erase eos.
     display  usera at 0110 with foreground-color 3.
     display  prog-name at 0301 with foreground-color 2.
     display  "Sales Ledger  Start Of Day" at 0328 with foreground-color 2
     display  maps-ser-xx at 2474 with foreground-color 2.
     move     maps-ser-nn to curs2.
     display  curs2 at 2476 with foreground-color 2.
*>
     accept   wsb-time from time.
     if       wsb-time not = "00000000"
              move wsb-hh to wsd-hh
              move wsb-mm to wsd-mm
              move wsb-ss to wsd-ss
              display "at " at 0360 with foreground-color 2
              display wsd-time at 0363 with foreground-color 2.
*>
 date-entry.
*>*********
*>
     if       Date-Form not > zero and < 4
              move 1 to Date-Form.
*>
*> Convert from UK to selected form
*>
     if       Date-UK or Date-USA
              move "/" to WS-Date (3:1)
                          WS-Date (6:1)
     end-if
     if       Date-USA
              move u-date to ws-date
              move ws-days to ws-swap
              move ws-month to ws-days
              move ws-swap to ws-month
              move ws-date to u-date
     end-if
     if       Date-Intl
              move "ccyy/mm/dd" to ws-date  *> swap Intl to UK form
              move u-date (7:4) to ws-Intl-Year
              move u-date (4:2) to ws-Intl-Month
              move u-date (1:2) to ws-Intl-Days
              move ws-date to u-date
     end-if
*>
     if       Date-UK
              display "Enter todays date as dd/mm/yyyy - [          ]" at 0812 with
                                                                foreground-color 2.
     if       Date-USA
              display "Enter todays date as mm/dd/yyyy - [          ]" at 0812 with
                                                                foreground-color 2.
     if       Date-Intl
              display "Enter todays date as yyyy/mm/dd - [          ]" at 0812 with
                                                                foreground-color 2.
     display  u-date at 0847 with foreground-color 3.
     accept   u-date at 0847 with foreground-color 3 update.
*>
*> convert to the Standard - UK form
*>
     if       Date-USA
              move u-date to ws-date
              move ws-days to ws-swap
              move ws-month to ws-days
              move ws-swap to ws-month
              move ws-date to u-date
     end-if
     if       Date-Intl
              move "dd/mm/ccyy" to ws-date   *> swap Intl to UK form
              move u-date (1:4) to ws-Year
              move u-date (6:2) to ws-Month
              move u-date (9:2) to ws-Days
              move ws-date to u-date
     end-if
*>
     move     zero  to  u-bin.
     call     "maps04"  using  maps03-ws.
*>
     if       u-bin = zero
              display SL005  at 0860 with foreground-color 4
              go to  date-entry
     else
              display " " at 0860 with erase eol.
*>
*> Bypassed security 1 & 2 for OS version
*>
     go       to chain-menu.
*>
*> verify user name.
*>
 *>    move         usera  to  pass-name.
 *>    move         "N"   to  encode.
 *>    call         "maps01"  using  maps01-ws.
 *>    if           pass-name  equal  user-code
 *>                 go to  chain-menu.
*>
*> If here suspected Un-Authorised / Un-Licensed usage.
*>
 *>    display  " " at 0101 with erase eos.
 *>    display  SY044               at 0501 with foreground-color 4 erase eos.
 *>    display  SY045               at 0701 with foreground-color 4.
*>
 *>    display  SY046               at 2401 with foreground-color 5 blinking.
 *>    go       to main-exit.
*>
 chain-menu.
*>*********
*>
     move     u-bin  to  run-date.
     move     u-date to  to-day.
     move     zero to ws-term-code.
*>
 main-exit.
     exit     program.
