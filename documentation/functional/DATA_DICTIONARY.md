# ACAS Data Dictionary
## Complete Database and File Structure Documentation

Generated: ${new Date().toISOString()}

## Overview

This document provides comprehensive documentation of all data structures within the ACAS system, including file layouts, field definitions, relationships, and business rules.

---

## File Organization Summary

### File Types
- **Master Files**: Indexed files containing reference data (customers, vendors, items)
- **Transaction Files**: Sequential files for daily transactions
- **History Files**: Archive files for completed transactions
- **Parameter Files**: System configuration and control
- **Work Files**: Temporary processing files

### Access Methods
- **ISAM**: Indexed Sequential Access Method for master files
- **Sequential**: For transaction logs and reports
- **Relative**: For direct access work files

---

## 1. General Ledger Files

### GL Master File (GLMAST)
**File**: glpost.dat  
**Organization**: Indexed  
**Key**: GL-ACCOUNT-NUMBER  

| Field Name | Type | Size | Description | Business Rules |
|------------|------|------|-------------|----------------|
| GL-ACCOUNT-NUMBER | Alphanumeric | 15 | GL account code | Unique, hierarchical |
| GL-ACCOUNT-DESC | Alphanumeric | 40 | Account description | Required |
| GL-ACCOUNT-TYPE | Alphanumeric | 2 | Account type code | See GL-TYPE-CODES |
| GL-NORMAL-BALANCE | Alphanumeric | 1 | D=Debit, C=Credit | Required |
| GL-CURRENCY-CODE | Alphanumeric | 3 | Currency code | Default: USD |
| GL-STATUS | Alphanumeric | 1 | A=Active, I=Inactive | Default: A |
| GL-PARENT-ACCOUNT | Alphanumeric | 15 | Parent for hierarchy | Optional |
| GL-CURRENT-BALANCE | Numeric | 15,2 | Current period balance | System maintained |
| GL-YTD-BALANCE | Numeric | 15,2 | Year-to-date balance | System maintained |
| GL-BUDGET-AMOUNT | Numeric | 15,2 | Budget for period | Optional |
| GL-LAST-POST-DATE | Date | 8 | Last posting date | System maintained |
| GL-CREATE-DATE | Date | 8 | Account creation date | System assigned |
| GL-CREATE-USER | Alphanumeric | 10 | User who created | System assigned |

### GL Transaction File (GLTRANS)
**File**: glbatch.dat  
**Organization**: Sequential  
**Sort**: Transaction date, batch number

| Field Name | Type | Size | Description | Business Rules |
|------------|------|------|-------------|----------------|
| GL-TRANS-DATE | Date | 8 | Transaction date | Required |
| GL-BATCH-NUMBER | Numeric | 6 | Batch identifier | System assigned |
| GL-TRANS-NUMBER | Numeric | 6 | Transaction in batch | Sequential |
| GL-ACCOUNT-NUMBER | Alphanumeric | 15 | GL account | Must exist |
| GL-DEBIT-AMOUNT | Numeric | 15,2 | Debit amount | Zero if credit |
| GL-CREDIT-AMOUNT | Numeric | 15,2 | Credit amount | Zero if debit |
| GL-REFERENCE | Alphanumeric | 20 | Reference number | Required |
| GL-DESCRIPTION | Alphanumeric | 50 | Transaction desc | Required |
| GL-SOURCE-MODULE | Alphanumeric | 2 | Source (SL,PL,GL) | System assigned |
| GL-SOURCE-TRANS | Alphanumeric | 20 | Source transaction | For audit |
| GL-POST-STATUS | Alphanumeric | 1 | P=Posted, U=Unposted | Default: U |
| GL-POST-DATE | Date | 8 | Actual posting date | When posted |
| GL-PERIOD | Numeric | 2 | Fiscal period | 1-12 |
| GL-YEAR | Numeric | 4 | Fiscal year | Current year |

### GL Account Type Codes (GL-TYPE-CODES)
| Code | Description | Normal Balance | Financial Statement |
|------|-------------|----------------|---------------------|
| AS | Asset | Debit | Balance Sheet |
| LI | Liability | Credit | Balance Sheet |
| EQ | Equity | Credit | Balance Sheet |
| RE | Revenue | Credit | P&L Statement |
| EX | Expense | Debit | P&L Statement |
| OI | Other Income | Credit | P&L Statement |
| OE | Other Expense | Debit | P&L Statement |

---

## 2. Sales Ledger Files

### Customer Master File (SLMAST)
**File**: sales.dat  
**Organization**: Indexed  
**Key**: SL-CUSTOMER-NUMBER  

| Field Name | Type | Size | Description | Business Rules |
|------------|------|------|-------------|----------------|
| SL-CUSTOMER-NUMBER | Alphanumeric | 10 | Customer code | Unique |
| SL-CUSTOMER-NAME | Alphanumeric | 40 | Customer name | Required |
| SL-ADDRESS-1 | Alphanumeric | 40 | Address line 1 | Required |
| SL-ADDRESS-2 | Alphanumeric | 40 | Address line 2 | Optional |
| SL-CITY | Alphanumeric | 30 | City | Required |
| SL-STATE | Alphanumeric | 2 | State/Province | Required |
| SL-ZIP-CODE | Alphanumeric | 10 | Postal code | Required |
| SL-COUNTRY | Alphanumeric | 3 | Country code | Default: USA |
| SL-PHONE | Alphanumeric | 20 | Phone number | Optional |
| SL-FAX | Alphanumeric | 20 | Fax number | Optional |
| SL-EMAIL | Alphanumeric | 50 | Email address | Optional |
| SL-CONTACT-NAME | Alphanumeric | 40 | Primary contact | Optional |
| SL-CREDIT-LIMIT | Numeric | 15,2 | Credit limit | Default: 0 |
| SL-PAYMENT-TERMS | Alphanumeric | 3 | Terms code | See TERMS-TABLE |
| SL-TAX-CODE | Alphanumeric | 3 | Tax code | Required |
| SL-TAX-EXEMPT | Alphanumeric | 1 | Y/N | Default: N |
| SL-TAX-NUMBER | Alphanumeric | 20 | Tax ID number | If exempt |
| SL-PRICE-LEVEL | Numeric | 1 | Price level 1-5 | Default: 1 |
| SL-DISCOUNT-PCT | Numeric | 5,2 | Trade discount | Default: 0 |
| SL-CURRENT-BALANCE | Numeric | 15,2 | Current balance | System maintained |
| SL-YTD-SALES | Numeric | 15,2 | Year-to-date sales | System maintained |
| SL-LAST-SALE-DATE | Date | 8 | Last sale date | System maintained |
| SL-LAST-PAY-DATE | Date | 8 | Last payment date | System maintained |
| SL-STATUS | Alphanumeric | 1 | A=Active, I=Inactive, H=Hold | Default: A |
| SL-GL-ACCOUNT | Alphanumeric | 15 | AR control account | From parameters |

### Sales Order Header (SLORDER)
**File**: slorder.dat  
**Organization**: Indexed  
**Key**: SL-ORDER-NUMBER  

| Field Name | Type | Size | Description | Business Rules |
|------------|------|------|-------------|----------------|
| SL-ORDER-NUMBER | Alphanumeric | 10 | Order number | System assigned |
| SL-ORDER-DATE | Date | 8 | Order date | Default: Today |
| SL-CUSTOMER-NUMBER | Alphanumeric | 10 | Customer code | Must exist |
| SL-SHIP-TO-CODE | Alphanumeric | 3 | Ship-to address | Optional |
| SL-PO-NUMBER | Alphanumeric | 20 | Customer PO | Optional |
| SL-ORDER-TOTAL | Numeric | 15,2 | Total order value | Calculated |
| SL-TAX-AMOUNT | Numeric | 15,2 | Total tax | Calculated |
| SL-STATUS | Alphanumeric | 1 | Order status | See ORDER-STATUS |
| SL-SHIP-VIA | Alphanumeric | 20 | Shipping method | Optional |
| SL-TERMS-CODE | Alphanumeric | 3 | Payment terms | From customer |
| SL-SALESPERSON | Alphanumeric | 3 | Sales rep code | Optional |
| SL-COMMISSION-PCT | Numeric | 5,2 | Commission rate | From salesperson |
| SL-REQUESTED-DATE | Date | 8 | Customer request | Optional |
| SL-PROMISED-DATE | Date | 8 | Delivery promise | Required |

### Sales Order Lines (SLORDLN)
**File**: slordln.dat  
**Organization**: Indexed  
**Key**: SL-ORDER-NUMBER + SL-LINE-NUMBER  

| Field Name | Type | Size | Description | Business Rules |
|------------|------|------|-------------|----------------|
| SL-ORDER-NUMBER | Alphanumeric | 10 | Order number | From header |
| SL-LINE-NUMBER | Numeric | 4 | Line number | Sequential |
| SL-ITEM-NUMBER | Alphanumeric | 20 | Item code | Must exist |
| SL-DESCRIPTION | Alphanumeric | 40 | Line description | From item/manual |
| SL-QUANTITY-ORDERED | Numeric | 11,3 | Quantity ordered | Required > 0 |
| SL-QUANTITY-SHIPPED | Numeric | 11,3 | Quantity shipped | <= ordered |
| SL-QUANTITY-BO | Numeric | 11,3 | Quantity backordered | Calculated |
| SL-UNIT-PRICE | Numeric | 15,4 | Unit price | From pricing |
| SL-DISCOUNT-PCT | Numeric | 5,2 | Line discount | Optional |
| SL-LINE-TOTAL | Numeric | 15,2 | Extended amount | Calculated |
| SL-TAX-CODE | Alphanumeric | 3 | Tax code | From item/customer |
| SL-TAX-AMOUNT | Numeric | 15,2 | Line tax | Calculated |
| SL-GL-ACCOUNT | Alphanumeric | 15 | Revenue account | From item |
| SL-COST | Numeric | 15,4 | Unit cost | From inventory |
| SL-STATUS | Alphanumeric | 1 | Line status | O=Open, S=Shipped, C=Closed |

### Sales Invoice Header (SLINV)
**File**: slinv.dat  
**Organization**: Indexed  
**Key**: SL-INVOICE-NUMBER  

| Field Name | Type | Size | Description | Business Rules |
|------------|------|------|-------------|----------------|
| SL-INVOICE-NUMBER | Alphanumeric | 10 | Invoice number | System assigned |
| SL-INVOICE-DATE | Date | 8 | Invoice date | Default: Today |
| SL-CUSTOMER-NUMBER | Alphanumeric | 10 | Customer code | From order |
| SL-ORDER-NUMBER | Alphanumeric | 10 | Source order | Optional |
| SL-INVOICE-TOTAL | Numeric | 15,2 | Total amount | Calculated |
| SL-TAX-AMOUNT | Numeric | 15,2 | Total tax | Calculated |
| SL-PAID-AMOUNT | Numeric | 15,2 | Amount paid | Updated by cash |
| SL-BALANCE-DUE | Numeric | 15,2 | Outstanding | Calculated |
| SL-DUE-DATE | Date | 8 | Payment due date | From terms |
| SL-DISCOUNT-DATE | Date | 8 | Discount deadline | From terms |
| SL-DISCOUNT-AMOUNT | Numeric | 15,2 | Available discount | Calculated |
| SL-STATUS | Alphanumeric | 1 | O=Open, P=Paid, V=Void | System maintained |
| SL-GL-POST-DATE | Date | 8 | GL posting date | When posted |
| SL-GL-BATCH | Numeric | 6 | GL batch number | From posting |

### Order Status Codes (ORDER-STATUS)
| Code | Description | Next Status |
|------|-------------|-------------|
| E | Entered | R, C |
| R | Released | S, B, C |
| S | Shipped | I, C |
| I | Invoiced | C |
| C | Closed/Cancelled | None |
| B | Backordered | R, C |

### Payment Terms Table (TERMS-TABLE)
| Code | Description | Days | Discount % | Discount Days |
|------|-------------|------|------------|---------------|
| N30 | Net 30 days | 30 | 0 | 0 |
| 210 | 2% 10 Net 30 | 30 | 2 | 10 |
| 215 | 2% 15 Net 30 | 30 | 2 | 15 |
| COD | Cash on Delivery | 0 | 0 | 0 |
| N45 | Net 45 days | 45 | 0 | 0 |
| EOM | End of Month | EOM | 0 | 0 |

---

## 3. Purchase Ledger Files

### Vendor Master File (PLMAST)
**File**: purch.dat  
**Organization**: Indexed  
**Key**: PL-VENDOR-NUMBER  

| Field Name | Type | Size | Description | Business Rules |
|------------|------|------|-------------|----------------|
| PL-VENDOR-NUMBER | Alphanumeric | 10 | Vendor code | Unique |
| PL-VENDOR-NAME | Alphanumeric | 40 | Vendor name | Required |
| PL-ADDRESS-1 | Alphanumeric | 40 | Address line 1 | Required |
| PL-ADDRESS-2 | Alphanumeric | 40 | Address line 2 | Optional |
| PL-CITY | Alphanumeric | 30 | City | Required |
| PL-STATE | Alphanumeric | 2 | State/Province | Required |
| PL-ZIP-CODE | Alphanumeric | 10 | Postal code | Required |
| PL-COUNTRY | Alphanumeric | 3 | Country code | Default: USA |
| PL-PHONE | Alphanumeric | 20 | Phone number | Optional |
| PL-FAX | Alphanumeric | 20 | Fax number | Optional |
| PL-EMAIL | Alphanumeric | 50 | Email address | Optional |
| PL-CONTACT-NAME | Alphanumeric | 40 | Primary contact | Optional |
| PL-PAYMENT-TERMS | Alphanumeric | 3 | Terms code | Required |
| PL-TAX-ID | Alphanumeric | 20 | Tax ID number | Required |
| PL-1099-VENDOR | Alphanumeric | 1 | Y/N for 1099 | Default: N |
| PL-DEFAULT-GL | Alphanumeric | 15 | Default expense | Optional |
| PL-BANK-ACCOUNT | Alphanumeric | 30 | Vendor bank account | For EFT |
| PL-BANK-ROUTING | Alphanumeric | 20 | Bank routing | For EFT |
| PL-CURRENT-BALANCE | Numeric | 15,2 | Current balance | System maintained |
| PL-YTD-PURCHASES | Numeric | 15,2 | YTD purchases | System maintained |
| PL-LAST-PURCHASE | Date | 8 | Last purchase date | System maintained |
| PL-LAST-PAYMENT | Date | 8 | Last payment date | System maintained |
| PL-STATUS | Alphanumeric | 1 | A=Active, I=Inactive, H=Hold | Default: A |

### Purchase Order Header (PLORDER)
**File**: plorder.dat  
**Organization**: Indexed  
**Key**: PL-ORDER-NUMBER  

| Field Name | Type | Size | Description | Business Rules |
|------------|------|------|-------------|----------------|
| PL-ORDER-NUMBER | Alphanumeric | 10 | PO number | System assigned |
| PL-ORDER-DATE | Date | 8 | Order date | Default: Today |
| PL-VENDOR-NUMBER | Alphanumeric | 10 | Vendor code | Must exist |
| PL-SHIP-TO-LOC | Alphanumeric | 3 | Delivery location | From parameters |
| PL-BUYER-CODE | Alphanumeric | 3 | Buyer/purchaser | Optional |
| PL-ORDER-TOTAL | Numeric | 15,2 | Total order value | Calculated |
| PL-TAX-AMOUNT | Numeric | 15,2 | Total tax | Calculated |
| PL-STATUS | Alphanumeric | 1 | PO status | See PO-STATUS |
| PL-APPROVAL-CODE | Alphanumeric | 10 | Approval reference | When approved |
| PL-APPROVED-BY | Alphanumeric | 10 | Approver ID | When approved |
| PL-APPROVED-DATE | Date | 8 | Approval date | When approved |
| PL-TERMS-CODE | Alphanumeric | 3 | Payment terms | From vendor |
| PL-SHIP-VIA | Alphanumeric | 20 | Shipping method | Optional |
| PL-FOB-POINT | Alphanumeric | 20 | FOB terms | Optional |
| PL-REQUESTED-DATE | Date | 8 | Requested delivery | Required |
| PL-CONFIRMED-DATE | Date | 8 | Vendor confirmation | Optional |

### Purchase Invoice Header (PLINV)
**File**: plinv.dat  
**Organization**: Indexed  
**Key**: PL-INVOICE-NUMBER  

| Field Name | Type | Size | Description | Business Rules |
|------------|------|------|-------------|----------------|
| PL-INVOICE-NUMBER | Alphanumeric | 10 | Internal number | System assigned |
| PL-VENDOR-INV-NO | Alphanumeric | 20 | Vendor invoice # | Required |
| PL-INVOICE-DATE | Date | 8 | Invoice date | Required |
| PL-VENDOR-NUMBER | Alphanumeric | 10 | Vendor code | Must exist |
| PL-ORDER-NUMBER | Alphanumeric | 10 | Related PO | Optional |
| PL-INVOICE-TOTAL | Numeric | 15,2 | Total amount | Required |
| PL-TAX-AMOUNT | Numeric | 15,2 | Total tax | Calculated |
| PL-PAID-AMOUNT | Numeric | 15,2 | Amount paid | System updated |
| PL-BALANCE-DUE | Numeric | 15,2 | Outstanding | Calculated |
| PL-DUE-DATE | Date | 8 | Payment due date | From terms |
| PL-DISCOUNT-DATE | Date | 8 | Discount deadline | From terms |
| PL-DISCOUNT-AMOUNT | Numeric | 15,2 | Available discount | Calculated |
| PL-STATUS | Alphanumeric | 1 | O=Open, P=Paid, V=Void, H=Hold | Default: O |
| PL-APPROVAL-STATUS | Alphanumeric | 1 | A=Approved, U=Unapproved, R=Rejected | Default: U |
| PL-GL-POST-DATE | Date | 8 | GL posting date | When posted |
| PL-CHECK-NUMBER | Alphanumeric | 10 | Payment reference | When paid |
| PL-CHECK-DATE | Date | 8 | Payment date | When paid |

### PO Status Codes (PO-STATUS)
| Code | Description | Next Status |
|------|-------------|-------------|
| E | Entered | A, C |
| A | Approved | O, C |
| O | Open | P, R, C |
| P | Partially Received | R, C |
| R | Received | I, C |
| I | Invoiced | C |
| C | Closed/Cancelled | None |

---

## 4. Inventory Files

### Item Master File (STMAST)
**File**: stock.dat  
**Organization**: Indexed  
**Key**: ST-ITEM-NUMBER  

| Field Name | Type | Size | Description | Business Rules |
|------------|------|------|-------------|----------------|
| ST-ITEM-NUMBER | Alphanumeric | 20 | Item/SKU code | Unique |
| ST-DESCRIPTION | Alphanumeric | 40 | Item description | Required |
| ST-LONG-DESC | Alphanumeric | 80 | Extended description | Optional |
| ST-ITEM-CLASS | Alphanumeric | 3 | Item classification | See ITEM-CLASS |
| ST-ITEM-TYPE | Alphanumeric | 1 | S=Stock, N=Non-stock, K=Kit | Default: S |
| ST-UOM | Alphanumeric | 3 | Unit of measure | Required |
| ST-LOCATION | Alphanumeric | 3 | Primary location | Required |
| ST-QTY-ON-HAND | Numeric | 11,3 | Current quantity | System maintained |
| ST-QTY-AVAILABLE | Numeric | 11,3 | Available to sell | Calculated |
| ST-QTY-ON-ORDER | Numeric | 11,3 | On purchase order | System maintained |
| ST-QTY-ALLOCATED | Numeric | 11,3 | Allocated to sales | System maintained |
| ST-REORDER-POINT | Numeric | 11,3 | Reorder trigger | Optional |
| ST-REORDER-QTY | Numeric | 11,3 | Order quantity | Optional |
| ST-LEAD-TIME | Numeric | 3 | Days lead time | Default: 0 |
| ST-UNIT-COST | Numeric | 15,4 | Current unit cost | Maintained by system |
| ST-LAST-COST | Numeric | 15,4 | Last receipt cost | System maintained |
| ST-AVG-COST | Numeric | 15,4 | Average cost | Calculated |
| ST-STD-COST | Numeric | 15,4 | Standard cost | Manual entry |
| ST-LIST-PRICE | Numeric | 15,4 | List selling price | Required |
| ST-PRICE-1 | Numeric | 15,4 | Price level 1 | Optional |
| ST-PRICE-2 | Numeric | 15,4 | Price level 2 | Optional |
| ST-PRICE-3 | Numeric | 15,4 | Price level 3 | Optional |
| ST-PRICE-4 | Numeric | 15,4 | Price level 4 | Optional |
| ST-PRICE-5 | Numeric | 15,4 | Price level 5 | Optional |
| ST-GL-SALES | Alphanumeric | 15 | Sales GL account | Required |
| ST-GL-INVENTORY | Alphanumeric | 15 | Inventory GL account | Required |
| ST-GL-COGS | Alphanumeric | 15 | COGS GL account | Required |
| ST-TAX-CODE | Alphanumeric | 3 | Default tax code | Required |
| ST-WEIGHT | Numeric | 11,3 | Unit weight | Optional |
| ST-VENDOR-NUMBER | Alphanumeric | 10 | Primary vendor | Optional |
| ST-VENDOR-ITEM | Alphanumeric | 20 | Vendor item code | Optional |
| ST-STATUS | Alphanumeric | 1 | A=Active, I=Inactive, O=Obsolete | Default: A |
| ST-LAST-SALE | Date | 8 | Last sale date | System maintained |
| ST-LAST-RECEIPT | Date | 8 | Last receipt date | System maintained |
| ST-YTD-SALES-QTY | Numeric | 11,3 | YTD quantity sold | System maintained |
| ST-YTD-SALES-AMT | Numeric | 15,2 | YTD sales amount | System maintained |

### Stock Transaction File (STTRANS)
**File**: sttrans.dat  
**Organization**: Sequential  
**Sort**: Transaction date, time

| Field Name | Type | Size | Description | Business Rules |
|------------|------|------|-------------|----------------|
| ST-TRANS-DATE | Date | 8 | Transaction date | Required |
| ST-TRANS-TIME | Time | 6 | Transaction time | System assigned |
| ST-TRANS-TYPE | Alphanumeric | 2 | Transaction type | See TRANS-TYPE |
| ST-ITEM-NUMBER | Alphanumeric | 20 | Item code | Must exist |
| ST-LOCATION | Alphanumeric | 3 | Stock location | Required |
| ST-QUANTITY | Numeric | 11,3 | Transaction qty | Can be negative |
| ST-UNIT-COST | Numeric | 15,4 | Transaction cost | Required |
| ST-REFERENCE | Alphanumeric | 20 | Source reference | Required |
| ST-REASON-CODE | Alphanumeric | 3 | Reason for trans | For adjustments |
| ST-USER-ID | Alphanumeric | 10 | User performing | System assigned |
| ST-PROGRAM-ID | Alphanumeric | 10 | Source program | System assigned |

### Stock Transaction Types (TRANS-TYPE)
| Code | Description | Quantity Impact | Cost Impact |
|------|-------------|-----------------|-------------|
| RR | Receipt from PO | Increase | Update avg cost |
| RI | Receipt from production | Increase | Update avg cost |
| IS | Issue to sales | Decrease | Use current cost |
| IP | Issue to production | Decrease | Use current cost |
| TR | Transfer between locations | Net zero | No change |
| AJ | Adjustment | Variable | May change |
| PI | Physical inventory | Set to count | May change |
| RV | Return from customer | Increase | Use original |

### Item Classifications (ITEM-CLASS)
| Code | Description | GL Impact |
|------|-------------|-----------|
| FG | Finished Goods | Inventory asset |
| RM | Raw Material | Inventory asset |
| WP | Work in Process | WIP asset |
| SU | Supplies | Expense when purchased |
| PK | Packaging | Inventory asset |
| SP | Spare Parts | Inventory asset |

---

## 5. Tax Files (IRS Module)

### Tax Rate Master (IRSTAX)
**File**: irstax.dat  
**Organization**: Indexed  
**Key**: TAX-CODE  

| Field Name | Type | Size | Description | Business Rules |
|------------|------|------|-------------|----------------|
| TAX-CODE | Alphanumeric | 3 | Tax code | Unique |
| TAX-DESCRIPTION | Alphanumeric | 30 | Description | Required |
| TAX-RATE | Numeric | 5,3 | Tax percentage | Required |
| TAX-TYPE | Alphanumeric | 1 | S=Sales, P=Purchase, B=Both | Required |
| TAX-GL-ACCOUNT | Alphanumeric | 15 | Tax liability GL | Required |
| TAX-AUTHORITY | Alphanumeric | 30 | Tax authority | Required |
| TAX-REGISTRATION | Alphanumeric | 20 | Registration # | Optional |
| TAX-EFFECTIVE-DATE | Date | 8 | Effective from | Required |
| TAX-EXPIRY-DATE | Date | 8 | Effective to | Optional |
| TAX-STATUS | Alphanumeric | 1 | A=Active, I=Inactive | Default: A |

### Tax Transaction Log (IRSTRANS)
**File**: irstrans.dat  
**Organization**: Sequential  
**Sort**: Transaction date

| Field Name | Type | Size | Description | Business Rules |
|------------|------|------|-------------|----------------|
| TAX-TRANS-DATE | Date | 8 | Transaction date | Required |
| TAX-MODULE | Alphanumeric | 2 | Source module | SL, PL |
| TAX-TRANS-NUMBER | Alphanumeric | 20 | Source transaction | Required |
| TAX-CODE | Alphanumeric | 3 | Tax code applied | Must exist |
| TAX-BASE-AMOUNT | Numeric | 15,2 | Taxable amount | Required |
| TAX-AMOUNT | Numeric | 15,2 | Tax calculated | Required |
| TAX-GL-POSTED | Alphanumeric | 1 | Y/N | Default: N |
| TAX-PERIOD | Numeric | 2 | Tax period | 1-12 |
| TAX-YEAR | Numeric | 4 | Tax year | Required |

---

## 6. System Control Files

### System Parameters (SYSPARM)
**File**: system.dat  
**Organization**: Indexed  
**Key**: PARM-ID  

| Field Name | Type | Size | Description | Business Rules |
|------------|------|------|-------------|----------------|
| PARM-ID | Alphanumeric | 20 | Parameter ID | Unique |
| PARM-VALUE | Alphanumeric | 80 | Parameter value | Required |
| PARM-DESC | Alphanumeric | 60 | Description | Documentation |
| PARM-TYPE | Alphanumeric | 1 | N=Numeric, A=Alpha, D=Date | Validation |
| PARM-MODULE | Alphanumeric | 2 | Module (GL,SL,PL,ST,IR,SY) | For grouping |
| PARM-CHANGEABLE | Alphanumeric | 1 | Y=User can change, N=System | Security |
| PARM-LAST-CHANGE | Date | 8 | Last changed date | Audit |
| PARM-CHANGED-BY | Alphanumeric | 10 | User who changed | Audit |

### Key System Parameters
| Parameter ID | Description | Typical Value |
|--------------|-------------|---------------|
| COMPANY-NAME | Company name | ACAS Corporation |
| FISCAL-YEAR | Current fiscal year | 2024 |
| CURRENT-PERIOD | Current fiscal period | 1-12 |
| BASE-CURRENCY | Base currency code | USD |
| GL-ACCOUNTS-AR | AR control account | 11000 |
| GL-ACCOUNTS-AP | AP control account | 20000 |
| GL-ACCOUNTS-INV | Inventory control | 13000 |
| NEXT-CUST-NUMBER | Next customer # | 100001 |
| NEXT-VEND-NUMBER | Next vendor # | 200001 |
| NEXT-INV-NUMBER | Next invoice # | 300001 |
| NEXT-PO-NUMBER | Next PO # | 400001 |

### User Security File (SECUSER)
**File**: security.dat  
**Organization**: Indexed  
**Key**: USER-ID  

| Field Name | Type | Size | Description | Business Rules |
|------------|------|------|-------------|----------------|
| USER-ID | Alphanumeric | 10 | User login ID | Unique |
| USER-NAME | Alphanumeric | 40 | Full name | Required |
| USER-PASSWORD | Alphanumeric | 20 | Encrypted password | Required |
| USER-LEVEL | Numeric | 1 | Security level 1-9 | 9=Admin |
| USER-MODULE-ACCESS | Alphanumeric | 10 | Module permissions | Bit flags |
| USER-STATUS | Alphanumeric | 1 | A=Active, I=Inactive, L=Locked | Default: A |
| USER-LAST-LOGIN | Date-Time | 14 | Last login timestamp | System maintained |
| USER-FAILED-LOGINS | Numeric | 2 | Failed login count | Lock at 3 |
| USER-CREATED-DATE | Date | 8 | Account created | System assigned |
| USER-EXPIRY-DATE | Date | 8 | Account expiry | Optional |

### Audit Trail File (AUDIT)
**File**: audit.dat  
**Organization**: Sequential  
**Sort**: Date-time descending

| Field Name | Type | Size | Description | Business Rules |
|------------|------|------|-------------|----------------|
| AUDIT-DATE-TIME | Date-Time | 14 | Timestamp | System assigned |
| AUDIT-USER-ID | Alphanumeric | 10 | User performing | From security |
| AUDIT-PROGRAM | Alphanumeric | 10 | Program name | System assigned |
| AUDIT-ACTION | Alphanumeric | 1 | A=Add, C=Change, D=Delete, V=View | Required |
| AUDIT-TABLE | Alphanumeric | 20 | File/table affected | Required |
| AUDIT-KEY | Alphanumeric | 50 | Record key value | Required |
| AUDIT-FIELD | Alphanumeric | 30 | Field changed | For changes |
| AUDIT-OLD-VALUE | Alphanumeric | 80 | Previous value | For changes |
| AUDIT-NEW-VALUE | Alphanumeric | 80 | New value | For changes |
| AUDIT-TERMINAL | Alphanumeric | 20 | Terminal/workstation | System info |

---

## Data Relationships

### Primary Relationships

1. **Customer → Orders → Invoices → Payments**
   - Customer master links to all sales transactions
   - Orders generate invoices
   - Payments apply to invoices

2. **Vendor → Purchase Orders → Receipts → AP Invoices → Payments**
   - Vendor master links to all purchase transactions
   - POs generate receipts and invoices
   - Payments apply to AP invoices

3. **Items → Stock Transactions → GL Postings**
   - Item master defines products
   - All movements create transactions
   - Transactions generate GL entries

4. **All Transactions → GL Postings**
   - Sales create AR and revenue entries
   - Purchases create AP and expense entries
   - Inventory creates asset adjustments

### Referential Integrity Rules

1. **Cannot Delete**:
   - Customers with orders or balances
   - Vendors with POs or balances
   - Items with stock or history
   - GL accounts with balances

2. **Cascading Updates**:
   - Customer terms update orders
   - Vendor terms update POs
   - Item costs update valuations

3. **Validation Rules**:
   - All GL accounts must exist
   - All tax codes must be valid
   - Dates must be in open periods
   - Amounts must balance (debits = credits)

---

## Data Lifecycle

### Transaction Flow
1. Entry → Validation → Posting → Archive → Purge
2. Retention periods vary by type:
   - Transactions: 7 years
   - Masters: Permanent
   - Audit: 7 years
   - Work files: Daily purge

### Period Processing
1. Daily: Transaction posting, reports
2. Monthly: Statements, aging, close
3. Yearly: Archive, purge, reset

### Backup Strategy
1. Daily: All transaction files
2. Weekly: Full system backup
3. Monthly: Archive to permanent media
4. Special: Before/after year-end