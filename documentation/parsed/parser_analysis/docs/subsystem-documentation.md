# Subsystem Documentation
Generated: 2025-09-17T19:53:05.730Z

## Identified Subsystems

### Sales Subsystem

**Programs**: 37

#### Program List

| Program ID | Type | Purpose | Lines | Complexity |
|------------|------|---------|-------|------------|
| dummmy | Unknown | General Processing | 174 | 4 |
| sales | main | General Processing | 956 | 66 |
| sl000 | main | General Processing | 293 | 6 |
| sl010 | main | General Processing | 2139 | 184 |
| sl020 | main | General Processing | 1042 | 54 |
| sl050 | Unknown | General Processing | 790 | 40 |
| sl055 | Unknown | General Processing | 734 | 62 |
| sl060 | main | General Processing | 1296 | 92 |
| sl070 | Unknown | General Processing | 927 | 103 |
| sl080 | main | General Processing | 879 | 43 |
| sl085 | main | General Processing | 718 | 38 |
| sl090 | Unknown | General Processing | 279 | 11 |
| sl095 | main | General Processing | 620 | 31 |
| sl100 | main | General Processing | 805 | 56 |
| sl110 | main | General Processing | 1089 | 41 |
| sl115 | Unknown | General Processing | 245 | 9 |
| sl120 | main | General Processing | 1163 | 53 |
| sl130 | Unknown | General Processing | 495 | 23 |
| sl140 | Unknown | General Processing | 539 | 28 |
| sl160 | main | General Processing | 916 | 46 |
| sl165 | Unknown | General Processing | 219 | 8 |
| sl170 | main | General Processing | 863 | 38 |
| sl180 | main | General Processing | 677 | 36 |
| sl190 | batch | General Processing | 909 | 50 |
| sl200 | main | General Processing | 351 | 16 |
| sl800 | main | General Processing | 232 | 6 |
| sl810 | main | General Processing | 1979 | 121 |
| sl820 | Unknown | General Processing | 740 | 39 |
| sl830 | Unknown | General Processing | 845 | 81 |
| sl900 | main | General Processing | 486 | 13 |
| sl910 | main | General Processing | 3374 | 258 |
| sl920 | main | General Processing | 2454 | 153 |
| sl930 | Unknown | General Processing | 1492 | 70 |
| sl940 | main | General Processing | 760 | 55 |
| sl950 | main | General Processing | 1075 | 60 |
| sl960 | main | General Processing | 560 | 29 |
| sl970 | Unknown | General Processing | 1105 | 57 |

#### Key Interactions

Programs in Sales interact with:

- External programs: maps04, SYSTEM, -, to, in, blk, ws-called, if, maps01, maps09 (+11 more)

### Purchase Subsystem

**Programs**: 38

#### Program List

| Program ID | Type | Purpose | Lines | Complexity |
|------------|------|---------|-------|------------|
| dummmy | Unknown | General Processing | 174 | 4 |
| pl000 | main | General Processing | 303 | 7 |
| pl010 | main | General Processing | 1625 | 131 |
| pl015 | batch | General Processing | 1036 | 54 |
| pl020 | Unknown | General Processing | 1376 | 91 |
| pl025 | main | General Processing | 432 | 21 |
| pl030 | Unknown | General Processing | 1248 | 75 |
| pl040 | main | General Processing | 448 | 27 |
| pl050 | Unknown | General Processing | 679 | 34 |
| pl055 | Unknown | General Processing | 637 | 49 |
| pl060 | main | General Processing | 1151 | 77 |
| pl070 | Unknown | General Processing | 936 | 103 |
| pl080 | Unknown | General Processing | 846 | 42 |
| pl085 | Unknown | General Processing | 690 | 37 |
| pl090 | Unknown | General Processing | 282 | 11 |
| pl095 | Unknown | General Processing | 614 | 31 |
| pl100 | main | General Processing | 799 | 56 |
| pl115 | Unknown | General Processing | 239 | 9 |
| pl120 | Unknown | General Processing | 1192 | 54 |
| pl130 | Unknown | General Processing | 494 | 23 |
| pl140 | Unknown | General Processing | 518 | 28 |
| pl160 | main | General Processing | 791 | 36 |
| pl165 | Unknown | General Processing | 221 | 8 |
| pl170 | main | General Processing | 669 | 37 |
| pl180 | Unknown | General Processing | 232 | 7 |
| pl190 | main | General Processing | 786 | 36 |
| pl800 | main | General Processing | 251 | 7 |
| pl900 | main | General Processing | 244 | 6 |
| pl910 | main | General Processing | 666 | 38 |
| pl920 | main | General Processing | 355 | 20 |
| pl930 | main | General Processing | 495 | 23 |
| pl940 | Unknown | General Processing | 654 | 30 |
| pl950 | main | General Processing | 834 | 70 |
| pl960 | Unknown | General Processing | 303 | 8 |
| purchase | main | General Processing | 940 | 63 |
| sl810 | main | General Processing | 1979 | 121 |
| sl820 | Unknown | General Processing | 740 | 39 |
| sl830 | Unknown | General Processing | 845 | 81 |

#### Key Interactions

Programs in Purchase interact with:

- External programs: maps04, maps01, maps09, SYSTEM, blk, failed, sl070, if, call, from (+9 more)

### Stock Subsystem

**Programs**: 12

#### Program List

| Program ID | Type | Purpose | Lines | Complexity |
|------------|------|---------|-------|------------|
| acasconvert1 | main | General Processing | 173 | 7 |
| dummmy | Unknown | General Processing | 174 | 4 |
| st000 | main | General Processing | 285 | 6 |
| st010 | main | General Processing | 2266 | 184 |
| st020 | Unknown | General Processing | 2340 | 168 |
| st030 | main | General Processing | 2368 | 145 |
| st040 | Unknown | General Processing | 484 | 17 |
| st050 | Unknown | General Processing | 534 | 31 |
| st060 | main | General Processing | 859 | 53 |
| stock | main | General Processing | 731 | 42 |
| stockconvert2 | main | General Processing | 252 | 6 |
| stockconvert3 | main | General Processing | 254 | 6 |

#### Key Interactions

Programs in Stock interact with:

- External programs: maps04, maps01, maps09, sl070, pl010, SYSTEM, to, blk, if, end-if (+3 more)

### General Ledger Subsystem

**Programs**: 18

#### Program List

| Program ID | Type | Purpose | Lines | Complexity |
|------------|------|---------|-------|------------|
| dummmy | Unknown | General Processing | 174 | 4 |
| general | main | General Processing | 909 | 70 |
| gl000 | Unknown | General Processing | 296 | 6 |
| gl020 | main | General Processing | 790 | 37 |
| gl030 | Unknown | General Processing | 2582 | 200 |
| gl050 | Unknown | General Processing | 1728 | 118 |
| gl051 | Unknown | General Processing | 1284 | 79 |
| gl060 | Unknown | General Processing | 532 | 26 |
| gl070 | Unknown | General Processing | 614 | 38 |
| gl071 | Unknown | General Processing | 184 | 4 |
| gl072 | Unknown | General Processing | 500 | 34 |
| gl080 | main | General Processing | 752 | 54 |
| gl090 | Unknown | General Processing | 216 | 6 |
| gl090a | Unknown | General Processing | 657 | 41 |
| gl090b | Unknown | General Processing | 436 | 15 |
| gl100 | Unknown | General Processing | 272 | 7 |
| gl105 | Unknown | General Processing | 585 | 26 |
| gl120 | Unknown | General Processing | 813 | 43 |

#### Key Interactions

Programs in General Ledger interact with:

- External programs: maps04, SYSTEM, blk, ws-called, if, maps01

### IRS Subsystem

**Programs**: 16

#### Program List

| Program ID | Type | Purpose | Lines | Complexity |
|------------|------|---------|-------|------------|
| dummmy | Unknown | General Processing | 174 | 4 |
| irs000 | main | General Processing | 273 | 6 |
| irs010 | main | General Processing | 1366 | 90 |
| irs020 | Unknown | General Processing | 1142 | 59 |
| irs030 | Unknown | General Processing | 1728 | 110 |
| irs040 | main | General Processing | 680 | 35 |
| irs050 | main | General Processing | 1410 | 69 |
| irs055 | main | General Processing | 270 | 10 |
| irs060 | Unknown | General Processing | 1806 | 121 |
| irs065 | Unknown | General Processing | 257 | 11 |
| irs070 | main | General Processing | 829 | 53 |
| irs080 | main | General Processing | 457 | 28 |
| irs085 | main | General Processing | 263 | 11 |
| irs090 | batch | General Processing | 541 | 33 |
| irs | main | General Processing | 1040 | 67 |
| irsubp | main | General Processing | 167 | 4 |

#### Key Interactions

Programs in IRS interact with:

- External programs: maps04, irsubn, SYSTEM, cpybk, blk, copybook, backup, maps03, ws-called, if (+5 more)

### Common Subsystem

**Programs**: 157

#### Program List

| Program ID | Type | Purpose | Lines | Complexity |
|------------|------|---------|-------|------------|
| ACAS-Sysout | Unknown | General Processing | 146 | 4 |
| ACAS | main | General Processing | 774 | 36 |
| UNKNOWN | main | General Processing | 35 | 1 |
| acas-get-params | Unknown | General Processing | 226 | 8 |
| takeon-1 | main | General Processing | 698 | 41 |
| takeon-2 | main | General Processing | 702 | 43 |
| acas000 | main | General Processing | 611 | 20 |
| acas004 | main | General Processing | 595 | 20 |
| acas005 | main | General Processing | 686 | 25 |
| acas006 | main | General Processing | 676 | 23 |
| acas007 | main | General Processing | 664 | 23 |
| acas008 | main | General Processing | 603 | 21 |
| acas010 | main | General Processing | 571 | 21 |
| acas011 | main | General Processing | 848 | 26 |
| acas012 | main | General Processing | 681 | 22 |
| acas013 | main | General Processing | 687 | 23 |
| acas014 | main | General Processing | 662 | 23 |
| acas015 | main | General Processing | 677 | 22 |
| acas016 | main | General Processing | 643 | 22 |
| acas017 | main | General Processing | 661 | 23 |
| acas019 | main | General Processing | 633 | 29 |
| acas022 | main | General Processing | 682 | 22 |
| acas023 | main | General Processing | 662 | 23 |
| acas026 | main | General Processing | 633 | 29 |
| acas029 | main | General Processing | 627 | 29 |
| acas030 | main | General Processing | 600 | 28 |
| acas032 | main | General Processing | 651 | 22 |
| acasconvert1 | main | General Processing | 173 | 7 |
| acasconvert2 | main | General Processing | 462 | 12 |
| acasconvert3 | main | General Processing | 275 | 6 |
| acasirsub1 | main | General Processing | 775 | 27 |
| acasirsub3 | main | General Processing | 545 | 32 |
| acasirsub4 | main | General Processing | 555 | 22 |
| acasirsub5 | main | General Processing | 536 | 31 |
| analLD | main | Data Loading | 513 | 58 |
| analMT | main | Maintenance | 1110 | 44 |
| analRES | main | General Processing | 228 | 16 |
| analUNL | main | Data Extraction | 220 | 14 |
| auditLD | main | Data Loading | 455 | 63 |
| auditLD | main | Data Loading | 455 | 63 |
| auditMT | main | Maintenance | 1409 | 51 |
| auditRES | main | General Processing | 229 | 16 |
| auditUNL | main | Data Extraction | 220 | 14 |
| CBL | main | General Processing | 250 | 24 |
| cobdump | main | General Processing | 135 | 8 |
| DelFolioLD | main | Data Loading | 532 | 63 |
| delfolioMT | main | Maintenance | 1168 | 47 |
| delfolioRES | main | General Processing | 229 | 16 |
| delfolioUNL | main | Data Extraction | 220 | 14 |
| deliveryLD | main | Data Loading | 526 | 62 |
| deliveryMT | main | Maintenance | 1157 | 47 |
| deliveryRES | main | General Processing | 229 | 16 |
| deliveryUNL | main | Data Extraction | 220 | 14 |
| dfltLD | main | Data Loading | 473 | 41 |
| dfltMT | main | Maintenance | 896 | 44 |
| dummmy | Unknown | General Processing | 174 | 4 |
| fhlogger | main | General Processing | 258 | 6 |
| finalLD | main | Data Loading | 483 | 42 |
| finalMT | main | Maintenance | 830 | 43 |
| glbatchLD | main | Data Loading | 524 | 61 |
| glbatchMT | main | Maintenance | 1713 | 58 |
| glbatchRES | main | Batch Processing | 229 | 16 |
| glbatchUNL | main | Data Extraction | 220 | 14 |
| glpostingLD | main | Data Loading | 526 | 61 |
| glpostingMT | main | Maintenance | 1495 | 53 |
| postingRES | main | General Processing | 229 | 16 |
| postingUNL | main | Data Extraction | 220 | 14 |
| irsdfltLD | main | Data Loading | 460 | 45 |
| irsdfltMT | main | Maintenance | 916 | 45 |
| irsdfltRES | main | General Processing | 235 | 16 |
| irsdfltUNL | main | Data Extraction | 226 | 14 |
| irsfinalLD | main | Data Loading | 451 | 42 |
| irsfinalMT | main | Maintenance | 727 | 40 |
| irsfinalRES | main | General Processing | 235 | 16 |
| irsfinalUNL | main | Data Extraction | 226 | 14 |
| irsnominalLD | main | Data Loading | 547 | 65 |
| irsnominalMT | main | Maintenance | 1757 | 73 |
| irsnominalRES | main | General Processing | 235 | 16 |
| irsnominalUNL2 | main | Data Extraction | 279 | 15 |
| irsnominalUNL | main | Data Extraction | 226 | 14 |
| irspostingLD | Unknown | Data Loading | 589 | 62 |
| irspostingMT | main | Maintenance | 1389 | 54 |
| irspostingRES | main | General Processing | 248 | 16 |
| irspostingUNL | main | Data Extraction | 239 | 14 |
| MAKESQLTABLE | batch | General Processing | 324 | 11 |
| MAKESQLTABLE | batch | General Processing | 324 | 11 |
| maps01 | main | General Processing | 212 | 5 |
| maps04 | main | General Processing | 195 | 4 |
| maps09 | main | General Processing | 146 | 4 |
| nominalLD | main | Data Loading | 508 | 63 |
| nominalMT | main | Maintenance | 1375 | 50 |
| nominalRES | main | General Processing | 229 | 16 |
| nominalUNL | main | Data Extraction | 220 | 14 |
| otm3LD | main | Data Loading | 502 | 57 |
| otm3MT | main | Maintenance | 2192 | 75 |
| otm3RES | main | General Processing | 232 | 16 |
| otm3UNL | main | Data Extraction | 222 | 14 |
| otm5LD | main | Data Loading | 514 | 63 |
| otm5MT | main | Maintenance | 2219 | 76 |
| otm5RES | main | General Processing | 232 | 16 |
| otm5UNL | main | Data Extraction | 222 | 14 |
| paymentsLD | main | Data Loading | 516 | 57 |
| paymentsMT | main | Maintenance | 2022 | 98 |
| paymentsRES | main | General Processing | 229 | 16 |
| paymentsUNL | main | Data Extraction | 220 | 14 |
| plautogenLD | main | Data Loading | 507 | 60 |
| plautogenMT | main | Maintenance | 3284 | 123 |
| plautogenRES | main | General Processing | 230 | 16 |
| plautogenUNL | main | Data Extraction | 220 | 14 |
| plinvoiceLD | main | Data Loading | 515 | 59 |
| plinvoiceMT | main | Maintenance | 3226 | 122 |
| pInvoiceRES | main | General Processing | 230 | 16 |
| pInvoiceUNL | main | Data Extraction | 220 | 14 |
| purchLD | main | Data Loading | 536 | 64 |
| purchMT | main | Maintenance | 2084 | 68 |
| PurchaseRES | main | General Processing | 229 | 16 |
| PurchaseUNL | main | Data Extraction | 220 | 14 |
| salesLD | main | Data Loading | 526 | 60 |
| salesMT | main | Maintenance | 2269 | 72 |
| salesRES | main | General Processing | 235 | 17 |
| salesUNL | main | Data Extraction | 220 | 14 |
| UNKNOWN | main | General Processing | 35 | 1 |
| sendsomemail | main | General Processing | 62 | 2 |
| sendsomemail | main | General Processing | 62 | 2 |
| slautogenLD | main | Data Loading | 507 | 59 |
| slautogenMT | main | Maintenance | 3297 | 123 |
| slautogenRES | main | General Processing | 231 | 16 |
| slautogenUNL | main | Data Extraction | 221 | 14 |
| sldelinvnosLD | main | Data Loading | 504 | 55 |
| sldelinvnosMT | main | Maintenance | 1173 | 47 |
| sldelinvnosRES | main | Data Loading | 230 | 16 |
| sldelinvnosUNL | main | Data Loading | 220 | 14 |
| slinvoiceLD | main | Data Loading | 514 | 59 |
| slinvoiceMT | main | Maintenance | 3259 | 123 |
| invoiceRES | main | General Processing | 229 | 16 |
| invoiceUNL | main | Data Extraction | 220 | 14 |
| slpostingLD | main | Data Loading | 485 | 54 |
| slpostingMT | main | Maintenance | 1337 | 52 |
| slpostingRES | main | General Processing | 229 | 16 |
| slpostingUNL | main | Data Extraction | 220 | 14 |
| StockLD | main | Data Loading | 522 | 59 |
| stockMT | main | Maintenance | 3380 | 90 |
| stockRES | main | General Processing | 235 | 17 |
| stockUNL | main | Data Extraction | 220 | 14 |
| sys002 | utility | General Processing | 3105 | 127 |
| sys4LD | main | Data Loading | 505 | 41 |
| sys4MT | main | Maintenance | 1589 | 48 |
| SystemLD | main | Data Loading | 480 | 41 |
| systemMT | main | Maintenance | 5258 | 121 |
| SystemRES | main | General Processing | 314 | 10 |
| SystemUNL | main | Data Extraction | 337 | 13 |
| valueLD | main | Data Loading | 515 | 60 |
| valueMT | main | Maintenance | 1412 | 53 |
| valueRES | main | Validation | 230 | 16 |
| valueUNL | main | Data Extraction | 219 | 14 |
| xl150 | utility | General Processing | 2256 | 345 |
| xl160 | batch | General Processing | 237 | 5 |

#### Key Interactions

Programs in Common interact with:

- External programs: all, each, to, the, ws-called, CBL-ACCEPT, before, only, when, end-evaluate (+13 more)

### Copybooks Subsystem

**Programs**: 174

#### Program List

| Program ID | Type | Purpose | Lines | Complexity |
|------------|------|---------|-------|------------|
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |
| UNKNOWN | main | General Processing | 35 | 1 |

#### Key Interactions

Programs in Copybooks interact with:

- External programs: CBL-ACCEPT

