       >>source free
*>*****************************************************************
*>                                                                *
*>         Check digit calulation and verification routine        *
*>                           MOD 11 only                          *
*>*****************************************************************
*>
 identification          division.
*>===============================
*>
*>**
      program-id.         maps09.
*>**
*>    author.             Cis Cobol Conversion By V B Coen FBCS, FIDM, FIDPM, 1/11/82
*>                        For Applewood Computers.
*>**
*>    Security.           Copyright (C) 1967-2025, Vincent Bryan Coen.
*>                        Distributed under the GNU General Public License.
*>                        See the file COPYING for details.
*>**
*>    remarks.            Check-Digit (Mod 11) / Calculation Verification.
*>**
*>    version.            1.02 of 08/11/82  01:30.
*>****
*> Changes:
*> 29/01/09 vbc - Migration to Open Cobol.
*> 16/04/24 vbc       Copyright notice update superseding all previous notices.
*>
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
 copy  "envdiv.cob".
 input-output            section.
*>------------------------------
*>
 data                    division.
*>===============================
 working-storage section.
*>----------------------
*>
 01  ws-data.
     03  alpha           pic x(37)     value "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ-".
     03  filler  redefines  alpha.
       05  ar1           pic x         occurs  37  indexed by q.
     03  work-array.
       05  array         pic x         occurs  6.
     03  suma            pic s9(5).
*>
     77  a               pic s9(5)      comp.
     77  y               pic s9(5)      comp.
     77  z               pic s9(5)      comp.
 linkage section.
*>--------------
*>
 copy  "wsmaps09.cob".
*>
 procedure division  using  maps09-ws.
*>===================================
*>
 main.
*>---
*>
     move     customer-nos  to  work-array.
     move     zero  to  suma.
     perform  addition-loop through addition-end varying a from 1 by 1 until a > 6.
*>
     if       suma = zero
              move  "N"  to  maps09-reply
              go to  main-exit.
*>
     divide   suma  by  11  giving  z.
     compute  a  =  11 - (suma - (11 * z)).
*>
     if       maps09-reply = "C"
              move   a   to  check-digit
              move  "Y"  to  maps09-reply.
*>
     if       maps09-reply = "V"
       and    a = check-digit
              move  "Y"  to  maps09-reply.
*>
     go       to main-exit.
*>
 addition-loop.
*>************
*>
     set      q  to  1.
     search   ar1  at end  go to  addition-error
              when ar1 (q) = array (a)
                   go to  addition-do.
*>
 addition-error.
*>*************
*>
     move     zero  to  suma.
     move     7     to  a.
     go to    addition-end.
*>
 addition-do.
*>**********
*>
     set      y  to  q.
     compute  z  =   y * (8 - a).
     add      z  to  suma.
*>
 addition-end.
     exit.
*>
 main-exit.   exit program.
*>********    ************
