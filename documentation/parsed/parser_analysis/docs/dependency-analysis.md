# Dependency Analysis
Generated: 2025-09-17T19:53:05.741Z

## Overview

This document analyzes the dependencies between programs and copybooks in the COBOL system.

## Program Dependencies

### Call Graph Statistics

- **Total Programs**: 453
- **Programs with CALL statements**: 265
- **Total CALL relationships**: 937

### Most Called Programs

| Program | Called By Count | Callers |
|---------|-----------------|---------|
| blk | 100 | general, gl020, gl030, gl050, gl051 (+95 more) |
| maps04 | 89 | ACAS, ACAS, general, general, gl000 (+84 more) |
| ACAS-Sysout | 79 | analLD, analRES, analUNL, auditLD, auditRES (+74 more) |
| SYSTEM | 74 | general, gl030, gl030, gl030, gl051 (+69 more) |
| to | 73 | ACAS, ACAS, ACAS, ACAS, acas000 (+68 more) |
| fhlogger | 55 | acas000, acas004, acas005, acas006, acas007 (+50 more) |
| ws-called | 34 | ACAS, ACAS, ACAS, ACAS, general (+29 more) |
| move | 30 | analMT, auditMT, delfolioMT, deliveryMT, dfltMT (+25 more) |
| acas-get-params | 28 | analLD, auditLD, DelFolioLD, deliveryLD, dfltLD (+23 more) |
| the | 27 | ACAS, acas000, acas004, acas005, acas006 (+22 more) |
| only | 25 | acas000, acas004, acas005, acas006, acas007 (+20 more) |
| if | 24 | general, general, irs, irs, pl055 (+19 more) |
| for | 20 | analMT, auditMT, dfltMT, finalMT, irsdfltMT (+15 more) |
| before | 15 | acas000, acas005, acas006, acas007, acas008 (+10 more) |
| cpybk | 12 | irs, irs010, irs020, irs040, irs050 (+7 more) |
| sl070 | 12 | pl030, pl055, sl055, sl810, sl810 (+7 more) |
| maps01 | 11 | gl000, gl020, pl000, sl000, st000 (+6 more) |
| irsubn | 11 | irs010, irs020, irs030, irs040, irs050 (+6 more) |
| fputs | 10 | UNKNOWN, UNKNOWN, UNKNOWN, UNKNOWN, UNKNOWN (+5 more) |
| acas000 | 8 | ACAS, dfltLD, finalLD, sys4LD, SystemLD (+3 more) |


### Programs with Most Dependencies

| Program | Call Count | Called Programs |
|---------|------------|-----------------|
| irs | 39 | maps04, irs000, irs000, irs010, irs020 (+34 more) |
| UNKNOWN | 21 | acas000, acas004, acas005, acas006, acas007 (+16 more) |
| UNKNOWN | 20 | tmpnam, fopen, fputs, fputs, fputs (+15 more) |
| purchase | 15 | maps04, SYSTEM, maps04, -, to (+10 more) |
| sales | 15 | maps04, SYSTEM, maps04, -, to (+10 more) |
| ACAS | 14 | maps04, maps04, acas000, all, each (+9 more) |
| acas000 | 13 | systemMT, dfltMT, finalMT, sys4MT, fhlogger (+8 more) |
| st010 | 13 | maps04, maps09, sl070, pl010, pl010 (+8 more) |
| general | 11 | maps04, SYSTEM, maps04, blk, ws-called (+6 more) |
| st020 | 11 | SYSTEM, SYSTEM, SYSTEM, SYSTEM, maps04 (+6 more) |
| sl910 | 10 | sl930, sl960, sl070, maps04, blk (+5 more) |
| stock | 10 | sl070, maps04, SYSTEM, maps04, blk (+5 more) |
| acas013 | 8 | valueMT, fhlogger, before, to, to (+3 more) |
| sys002 | 8 | maps01, maps99, maps04, maps04, maps01 (+3 more) |
| acas010 | 7 | auditMT, fhlogger, before, to, the (+2 more) |
| acas032 | 7 | paymentsMT, fhlogger, before, the, only (+2 more) |
| irs050 | 7 | irsubn, irs055, irsubp, SYSTEM, SYSTEM (+2 more) |
| irs070 | 7 | irsubn, irs055, irsubp, irsubp, irsubp (+2 more) |
| sl810 | 7 | sl960, sl070, maps04, blk, SL (+2 more) |
| sl810 | 7 | sl960, sl070, maps04, blk, SL (+2 more) |


## Copybook Dependencies

### Most Used Copybooks

| Copybook | Usage Count | Type | Purpose |
|----------|-------------|------|---------|
| ACCEPTNM | 0 | GENERAL | Data Structure |
| ACAS-SQLstate-error-list | 0 | GENERAL | Error Handling |
| FileStat-Msgs | 0 | GENERAL | Error Handling |
| MySQL-SQLCA | 0 | GENERAL | Database Interface |
| Proc-ACAS-FH-Calls | 0 | PROCEDURE | Data Structure |
| Proc-ACAS-Mapser-RDB | 0 | PROCEDURE | Screen Mapping |
| Proc-ACAS-Mapser | 0 | PROCEDURE | Screen Mapping |
| Proc-ACAS-Param-Get-Rewrite | 0 | PROCEDURE | Parameter Passing |
| Proc-Get-Env-Set-Files | 0 | PROCEDURE | Data Structure |
| Proc-ZZ100-ACAS-IRS-Calls | 0 | PROCEDURE | Data Structure |
| Test-Data-Flags | 0 | GENERAL | Data Structure |
| WSfdpinv-1 | 0 | WORKING-STORAGE | Data Structure |
| WSfdpinv2-1 | 0 | WORKING-STORAGE | Data Structure |
| copyright | 0 | GENERAL | Data Structure |
| envdiv | 0 | GENERAL | Data Structure |
| fdanal | 0 | FILE-DESCRIPTION | Data Structure |
| fdaudit | 0 | FILE-DESCRIPTION | Data Structure |
| fdbatch | 0 | FILE-DESCRIPTION | Data Structure |
| fdboitm | 0 | FILE-DESCRIPTION | Data Structure |
| fddel | 0 | FILE-DESCRIPTION | Data Structure |


## Circular Dependencies

No circular dependencies detected in top-level analysis.

## Dependency Clusters


Major dependency clusters identified:

1. **Sales Processing Cluster**: SL* programs with shared copybooks
2. **Purchase Processing Cluster**: PL* programs with shared data structures
3. **General Ledger Cluster**: GL* programs with accounting functions
4. **Common Utilities Cluster**: Shared utility programs (ACAS*, SYS*, XL*)

