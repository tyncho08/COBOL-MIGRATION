# ACAS Component Catalog
## Complete Program Inventory by Business Function

Generated: ${new Date().toISOString()}

## Module Organization Overview

The ACAS system is organized into the following primary modules:

1. **General Ledger (GL)** - Core financial accounting
2. **Sales Ledger (SL)** - Accounts receivable and sales
3. **Purchase Ledger (PL)** - Accounts payable and purchasing
4. **Stock Control (ST)** - Inventory management
5. **IRS Tax Module (IRS)** - Tax processing and compliance
6. **Common Components** - Shared utilities and functions
7. **System Components** - Infrastructure and administration

## Classification System

Programs are classified by:
- **Processing Type**: Interactive (Online) vs Batch
- **Function Type**: Maintenance, Transaction, Report, Utility
- **Data Operation**: Load (LD), Unload (UNL), Maintenance (MT), Restore (RES)

---

## 1. General Ledger Module (GL)

### Core Components

| Program ID | Description | Type | Category | Dependencies |
|------------|-------------|------|----------|--------------|
| general.cbl | GL Main Menu Program | Interactive | Menu | GL000 |
| GL000 | GL Menu Navigation | Interactive | Menu | maps01 |
| GL020 | Account Master Maintenance | Interactive | Maintenance | glpostingMT |
| GL030 | Account Inquiry | Interactive | Inquiry | glpostingLD |
| GL050 | Journal Entry | Interactive | Transaction | glbatchMT |
| GL051 | Journal Entry Posting | Batch | Transaction | glpostingMT |
| GL060 | Recurring Entries | Interactive | Transaction | glbatchMT |
| GL070 | Trial Balance | Interactive | Report | glpostingLD |
| GL071 | Balance Sheet | Interactive | Report | glpostingLD |
| GL072 | P&L Statement | Interactive | Report | glpostingLD |
| GL080 | Account Analysis | Interactive | Report | glpostingLD |
| GL090 | Period Close | Batch | Utility | glpostingMT |
| GL090a | Month-End Processing | Batch | Utility | GL090 |
| GL090b | Year-End Processing | Batch | Utility | GL090 |
| GL100 | GL Parameters | Interactive | Setup | systemMT |
| GL105 | Chart of Accounts List | Interactive | Report | glpostingLD |
| GL120 | GL Utilities | Interactive | Utility | various |

### GL Data Operations

| Program ID | Description | Operation | Related Files |
|------------|-------------|-----------|---------------|
| glpostingLD | Load GL Posting Data | Load | glpost.dat |
| glpostingMT | Maintain GL Postings | Maintain | glpost.dat |
| glpostingRES | Restore GL Postings | Restore | glpost.dat |
| glpostingUNL | Unload GL Postings | Unload | glpost.dat |
| glbatchLD | Load GL Batch | Load | glbatch.dat |
| glbatchMT | Maintain GL Batch | Maintain | glbatch.dat |
| glbatchRES | Restore GL Batch | Restore | glbatch.dat |
| glbatchUNL | Unload GL Batch | Unload | glbatch.dat |

---

## 2. Sales Ledger Module (SL)

### Core Components

| Program ID | Description | Type | Category | Dependencies |
|------------|-------------|------|----------|--------------|
| sales.cbl | Sales Main Menu Program | Interactive | Menu | SL000 |
| SL000 | Sales Menu Navigation | Interactive | Menu | maps01 |
| SL010 | Customer Master Maintenance | Interactive | Maintenance | salesMT |
| SL020 | Sales Order Entry | Interactive | Transaction | slautogenMT |
| SL050 | Invoice Generation | Interactive | Transaction | slinvoiceMT |
| SL055 | Credit Note Processing | Interactive | Transaction | slinvoiceMT |
| SL060 | Cash Receipt | Interactive | Transaction | salesMT |
| SL070 | Customer Statement | Interactive | Report | salesLD |
| SL080 | Aged Debtors Report | Interactive | Report | salesLD |
| SL085 | Sales Analysis by Customer | Interactive | Report | salesLD |
| SL090 | Sales Analysis by Product | Interactive | Report | salesLD |
| SL095 | Commission Calculation | Batch | Calculation | salesLD |
| SL100 | Customer Inquiry | Interactive | Inquiry | salesLD |
| SL110 | Order Status Inquiry | Interactive | Inquiry | slautogenLD |
| SL115 | Back Order Report | Interactive | Report | slautogenLD |
| SL120 | Delivery Processing | Interactive | Transaction | deliveryMT |
| SL130 | Price List Maintenance | Interactive | Maintenance | salesMT |
| SL140 | Sales Tax Report | Interactive | Report | salesLD |
| SL160 | Customer Labels | Batch | Report | salesLD |
| SL165 | Sales Register | Interactive | Report | slinvoiceLD |
| SL170 | Daily Sales Report | Interactive | Report | slinvoiceLD |
| SL180 | Monthly Sales Summary | Batch | Report | slinvoiceLD |
| SL190 | Year-to-Date Analysis | Interactive | Report | salesLD |
| SL200 | Export Sales Data | Batch | Utility | salesUNL |

### Sales Batch Processing

| Program ID | Description | Type | Processing |
|------------|-------------|------|------------|
| SL800 | End of Day Processing | Batch | Daily |
| SL810 | Invoice Print Run | Batch | Daily |
| SL820 | Statement Print Run | Batch | Monthly |
| SL830 | Dunning Letters | Batch | Monthly |
| SL900 | Month End Sales | Batch | Monthly |
| SL910 | Sales GL Interface | Batch | Daily |
| SL920 | Commission Calculation | Batch | Monthly |
| SL930 | Sales Statistics Update | Batch | Daily |
| SL940 | Archive Old Transactions | Batch | Quarterly |
| SL950 | Purge Completed Orders | Batch | Monthly |
| SL960 | Rebuild Sales Indices | Batch | Utility |
| SL970 | Sales System Check | Batch | Utility |

### Sales Data Operations

| Program ID | Description | Operation | Related Files |
|------------|-------------|-----------|---------------|
| salesLD | Load Sales Master | Load | sales.dat |
| salesMT | Maintain Sales Master | Maintain | sales.dat |
| salesRES | Restore Sales Master | Restore | sales.dat |
| salesUNL | Unload Sales Master | Unload | sales.dat |
| slinvoiceLD | Load Invoice Data | Load | slinv.dat |
| slinvoiceMT | Maintain Invoices | Maintain | slinv.dat |
| slautogenLD | Load Order Data | Load | slorder.dat |
| slautogenMT | Maintain Orders | Maintain | slorder.dat |
| deliveryLD | Load Delivery Data | Load | delivery.dat |
| deliveryMT | Maintain Deliveries | Maintain | delivery.dat |
| slpostingLD | Load SL Postings | Load | slpost.dat |
| slpostingMT | Maintain SL Postings | Maintain | slpost.dat |

---

## 3. Purchase Ledger Module (PL)

### Core Components

| Program ID | Description | Type | Category | Dependencies |
|------------|-------------|------|----------|--------------|
| purchase.cbl | Purchase Main Menu Program | Interactive | Menu | PL000 |
| PL000 | Purchase Menu Navigation | Interactive | Menu | maps01 |
| PL010 | Vendor Master Maintenance | Interactive | Maintenance | purchMT |
| PL015 | Vendor Inquiry | Interactive | Inquiry | purchLD |
| PL020 | Purchase Order Entry | Interactive | Transaction | plautogenMT |
| PL025 | PO Amendment | Interactive | Transaction | plautogenMT |
| PL030 | Goods Receipt | Interactive | Transaction | purchMT |
| PL040 | Purchase Invoice Entry | Interactive | Transaction | plinvoiceMT |
| PL050 | Invoice Matching | Interactive | Transaction | plinvoiceMT |
| PL055 | Credit Note Entry | Interactive | Transaction | plinvoiceMT |
| PL060 | Payment Selection | Interactive | Transaction | paymentsMT |
| PL070 | Check/Payment Run | Batch | Transaction | paymentsMT |
| PL080 | Vendor Statement | Interactive | Report | purchLD |
| PL085 | Aged Creditors Report | Interactive | Report | purchLD |
| PL090 | Purchase Analysis | Interactive | Report | purchLD |
| PL095 | Vendor Performance | Interactive | Report | purchLD |
| PL100 | Payment History | Interactive | Report | paymentsLD |
| PL115 | Open PO Report | Interactive | Report | plautogenLD |
| PL120 | Receipts Register | Interactive | Report | purchLD |
| PL130 | Purchase Price Variance | Interactive | Report | plinvoiceLD |
| PL140 | Purchase Tax Report | Interactive | Report | plinvoiceLD |
| PL160 | Vendor Labels | Batch | Report | purchLD |
| PL165 | Purchase Register | Interactive | Report | plinvoiceLD |
| PL170 | Daily Purchase Report | Interactive | Report | plinvoiceLD |
| PL180 | Monthly Purchase Summary | Batch | Report | plinvoiceLD |
| PL190 | YTD Purchase Analysis | Interactive | Report | purchLD |

### Purchase Batch Processing

| Program ID | Description | Type | Processing |
|------------|-------------|------|------------|
| PL800 | End of Day Processing | Batch | Daily |
| PL900 | Month End Purchase | Batch | Monthly |
| PL910 | Purchase GL Interface | Batch | Daily |
| PL920 | Vendor Statistics Update | Batch | Monthly |
| PL930 | Payment Statistics | Batch | Monthly |
| PL940 | Archive Old Transactions | Batch | Quarterly |
| PL950 | Purge Completed POs | Batch | Monthly |
| PL960 | Rebuild Purchase Indices | Batch | Utility |

### Purchase Data Operations

| Program ID | Description | Operation | Related Files |
|------------|-------------|-----------|---------------|
| purchLD | Load Purchase Master | Load | purch.dat |
| purchMT | Maintain Purchase Master | Maintain | purch.dat |
| purchRES | Restore Purchase Master | Restore | purch.dat |
| purchUNL | Unload Purchase Master | Unload | purch.dat |
| plinvoiceLD | Load PL Invoice Data | Load | plinv.dat |
| plinvoiceMT | Maintain PL Invoices | Maintain | plinv.dat |
| plautogenLD | Load PO Data | Load | plorder.dat |
| plautogenMT | Maintain POs | Maintain | plorder.dat |
| paymentsLD | Load Payment Data | Load | payment.dat |
| paymentsMT | Maintain Payments | Maintain | payment.dat |

---

## 4. Stock Control Module (ST)

### Core Components

| Program ID | Description | Type | Category | Dependencies |
|------------|-------------|------|----------|--------------|
| stock.cbl | Stock Main Menu Program | Interactive | Menu | ST000 |
| ST000 | Stock Menu Navigation | Interactive | Menu | maps01 |
| ST010 | Item Master Maintenance | Interactive | Maintenance | stockMT |
| ST020 | Stock Receipt | Interactive | Transaction | stockMT |
| ST030 | Stock Issue | Interactive | Transaction | stockMT |
| ST040 | Stock Transfer | Interactive | Transaction | stockMT |
| ST050 | Stock Adjustment | Interactive | Transaction | stockMT |
| ST060 | Stock Inquiry | Interactive | Inquiry | stockLD |

### Stock Data Operations

| Program ID | Description | Operation | Related Files |
|------------|-------------|-----------|---------------|
| stockLD | Load Stock Master | Load | stock.dat |
| stockMT | Maintain Stock Master | Maintain | stock.dat |
| stockRES | Restore Stock Master | Restore | stock.dat |
| stockUNL | Unload Stock Master | Unload | stock.dat |
| valueLD | Load Stock Values | Load | value.dat |
| valueMT | Maintain Stock Values | Maintain | value.dat |

### Stock Utilities

| Program ID | Description | Type |
|------------|-------------|------|
| stockconvert2 | Stock File Conversion | Utility |
| stockconvert3 | Stock Data Migration | Utility |
| acasconvert1 | Legacy Data Import | Utility |

---

## 5. IRS Tax Module

### Core Components

| Program ID | Description | Type | Category | Dependencies |
|------------|-------------|------|----------|--------------|
| irs.cbl | IRS Main Menu Program | Interactive | Menu | IRS000 |
| IRS000 | IRS Menu Navigation | Interactive | Menu | maps01 |
| IRS010 | Tax Rate Maintenance | Interactive | Setup | irsdfltMT |
| IRS020 | Tax Code Setup | Interactive | Setup | irsdfltMT |
| IRS030 | Tax Calculation Rules | Interactive | Setup | irsdfltMT |
| IRS040 | Tax Period Setup | Interactive | Setup | irsfinalMT |
| IRS050 | Tax Report Configuration | Interactive | Setup | irspostingMT |
| IRS055 | Tax Exemptions | Interactive | Setup | irsdfltMT |
| IRS060 | VAT Return | Interactive | Report | irspostingLD |
| IRS065 | Sales Tax Report | Interactive | Report | irspostingLD |
| IRS070 | Tax Audit Trail | Interactive | Report | irspostingLD |
| IRS080 | Period Tax Summary | Interactive | Report | irspostingLD |
| IRS085 | Tax Reconciliation | Interactive | Report | irspostingLD |
| IRS090 | Year End Tax Processing | Batch | Utility | irspostingMT |

### IRS Utilities

| Program ID | Description | Type |
|------------|-------------|------|
| irsubp | IRS Subroutine Library | Library |
| acasirsub1 | Tax Calculation Sub 1 | Subroutine |
| acasirsub3 | Tax Posting Sub | Subroutine |
| acasirsub4 | Tax Report Sub | Subroutine |
| acasirsub5 | Tax Utility Sub | Subroutine |

### IRS Data Operations

| Program ID | Description | Operation | Related Files |
|------------|-------------|-----------|---------------|
| irsdfltLD | Load Tax Defaults | Load | irsdflt.dat |
| irsdfltMT | Maintain Tax Defaults | Maintain | irsdflt.dat |
| irsfinalLD | Load Tax Finals | Load | irsfinal.dat |
| irsfinalMT | Maintain Tax Finals | Maintain | irsfinal.dat |
| irspostingLD | Load Tax Postings | Load | irspost.dat |
| irspostingMT | Maintain Tax Postings | Maintain | irspost.dat |
| irsnominalLD | Load Tax Nominals | Load | irsnom.dat |
| irsnominalMT | Maintain Tax Nominals | Maintain | irsnom.dat |

---

## 6. Common Components

### System Utilities

| Program ID | Description | Type | Purpose |
|------------|-------------|------|---------|
| ACAS.cbl | Main System Controller | Interactive | System startup |
| acas000 | System Initialization | Utility | Initialize system |
| maps01 | Menu Navigation Map | Utility | Menu system |
| maps04 | Screen Map Handler | Utility | Screen I/O |
| maps09 | Report Map Handler | Utility | Report formatting |
| xl150 | Data Export Utility | Utility | Excel export |
| xl160 | Data Import Utility | Utility | Excel import |
| sys002 | System Security | Utility | Access control |
| fhlogger | File Handler Logger | Utility | Audit logging |

### Parameter Management

| Program ID | Description | Function |
|------------|-------------|----------|
| acas-get-params | Parameter Retrieval | Get system parameters |
| systemLD | Load System Parameters | Load config |
| systemMT | Maintain System Parameters | Update config |
| sys4LD | Load System Tables | Load lookups |
| sys4MT | Maintain System Tables | Update lookups |

### Data Operations

| Program ID | Description | Function |
|------------|-------------|----------|
| analLD | Load Analysis Data | Analytics support |
| analMT | Maintain Analysis Data | Analytics update |
| auditLD | Load Audit Data | Audit trail |
| auditMT | Maintain Audit Data | Audit update |
| dfltLD | Load Defaults | System defaults |
| dfltMT | Maintain Defaults | Default update |
| finalLD | Load Final Data | Period finals |
| finalMT | Maintain Final Data | Finals update |
| nominalLD | Load Nominal Data | GL nominals |
| nominalMT | Maintain Nominal Data | Nominal update |

### Batch Processing Framework

| Program ID | Description | Type |
|------------|-------------|------|
| masterLD.sh | Master Load Script | Shell |
| masterRES.sh | Master Restore Script | Shell |
| masterUNL.sh | Master Unload Script | Shell |
| acasbkup.sh | System Backup | Shell |
| acasbkup-Pre-EOY.sh | Pre Year-End Backup | Shell |
| acasbkup-Post-EOY.sh | Post Year-End Backup | Shell |

---

## 7. Copybook Library

### Core Copybooks

| Copybook | Description | Used By |
|----------|-------------|---------|
| wssystem.cob | System Working Storage | All modules |
| security.cob | Security Definitions | All programs |
| copyright.cob | Copyright Notice | All programs |
| envdiv.cob | Environment Division | All programs |
| screenio.cpy | Screen I/O Routines | Interactive programs |

### File Definitions

| Copybook | Description | Module |
|----------|-------------|--------|
| fdledger.cob | GL Ledger File | GL |
| fdsl.cob | Sales Ledger File | SL |
| fdpl.cob | Purchase Ledger File | PL |
| fdstock.cob | Stock Master File | ST |
| fdsys.cob | System File | System |
| fdanal.cob | Analysis File | All |
| fdaudit.cob | Audit File | All |
| fdbatch.cob | Batch File | All |
| fdpost.cob | Posting File | GL |
| fdval.cob | Value File | ST |

### Working Storage Definitions

| Copybook | Description | Module |
|----------|-------------|--------|
| wsledger.cob | GL Working Storage | GL |
| wssl.cob | Sales Working Storage | SL |
| wspl.cob | Purchase Working Storage | PL |
| wsstock.cob | Stock Working Storage | ST |
| wsanal.cob | Analysis Working Storage | All |
| wsbatch.cob | Batch Working Storage | All |
| wspost.cob | Posting Working Storage | GL |

### Select Statements

| Copybook | Description | Module |
|----------|-------------|--------|
| seledger.cob | GL File Select | GL |
| selsl.cob | Sales File Select | SL |
| selpl.cob | Purchase File Select | PL |
| selstock.cob | Stock File Select | ST |
| selsys.cob | System File Select | System |

---

## Program Interdependencies

### Critical Dependencies

1. **Menu System**: All modules depend on maps01 for navigation
2. **Security**: All programs use security.cob for access control
3. **Parameters**: System parameters loaded by acas-get-params
4. **Logging**: fhlogger provides audit trail for all file operations
5. **GL Interface**: All transaction modules post to GL through interface programs

### Integration Points

1. **Sales → Stock**: Order processing updates inventory
2. **Purchase → Stock**: Receipts update inventory
3. **Sales/Purchase → GL**: Automatic GL posting
4. **All → IRS**: Tax calculation and reporting
5. **All → Audit**: Transaction logging

### Batch Dependencies

1. Daily: Transaction posting, report generation
2. Monthly: Statement generation, aging, statistics
3. Quarterly: Archive and purge operations
4. Yearly: Year-end processing, tax finalization

---

## Module Statistics Summary

| Module | Interactive Programs | Batch Programs | Data Operations | Copybooks | Total Components |
|--------|---------------------|----------------|-----------------|-----------|------------------|
| GL | 15 | 3 | 8 | 12 | 38 |
| SL | 23 | 13 | 18 | 25 | 79 |
| PL | 21 | 8 | 14 | 20 | 63 |
| ST | 7 | 3 | 8 | 8 | 26 |
| IRS | 11 | 1 | 12 | 15 | 39 |
| Common | 12 | 6 | 24 | 35 | 77 |
| **Total** | **89** | **34** | **84** | **115** | **322** |

---

## Technical Architecture Notes

### Program Naming Conventions
- **XXX000**: Module main menu
- **XXX0nn**: Interactive programs (01-99)
- **XXXnnn**: Batch programs (100-999)
- **xxxLD**: Load/Read operations
- **xxxMT**: Maintenance/Update operations
- **xxxRES**: Restore operations
- **xxxUNL**: Unload/Extract operations

### File Organization
- Master files: Indexed organization
- Transaction files: Sequential organization
- Report files: Sequential print files
- Archive files: Sequential compressed

### Processing Patterns
- Real-time: Interactive transaction entry
- Batch: End-of-period processing
- Hybrid: Real-time entry with batch posting