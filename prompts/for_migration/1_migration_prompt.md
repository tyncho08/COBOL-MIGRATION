# COMPLETE COBOL TO MODERN STACK MIGRATION PROMPT

## MISSION
Migrate the complete ACAS (Accounting and Control Application System) from COBOL to a modern technology stack. You MUST create a fully functional, professional accounting system that preserves ALL business logic while providing a clean, modern user experience.

## CRITICAL SUCCESS FACTORS

### ❌ ABSOLUTE DONT'S (Lessons Learned)
1. **NO migration references in UI** - Users don't care it was migrated from COBOL
2. **NO 404 errors** - Every navigation link must work 
3. **NO giant icons** - Always specify exact sizes (w-6 h-6)
4. **NO broken imports** - Use `from sqlalchemy.types import Numeric as Decimal`
5. **NO deprecated configs** - Don't use experimental.appDir in Next.js
6. **NO empty statistics** - Don't show made-up migration metrics
7. **NO confusing technical jargon** - Focus on business functionality

### ✅ MANDATORY REQUIREMENTS
1. **Clean, professional UI** - Modern business application appearance
2. **All modules working** - Create placeholder pages if needed
3. **Proper icon sizing** - Consistent w-6 h-6 throughout
4. **Real business value** - Focus on accounting functionality
5. **Complete navigation** - Every module accessible and functional
6. **Modern UX patterns** - Intuitive, responsive design
7. **Production-ready code** - Clean, maintainable, documented

## TECHNOLOGY STACK
- **Backend**: Python 3.11+ with FastAPI
- **Frontend**: Next.js 14 with TypeScript
- **Database**: PostgreSQL 15+
- **Styling**: Tailwind CSS + Heroicons
- **API**: RESTful with automatic OpenAPI docs

## SYSTEM UNDERSTANDING

### ACAS Overview
ACAS is a comprehensive enterprise accounting solution supporting:

1. **General Ledger (GL)** - Chart of accounts, journal entries, financial statements
2. **Accounts Receivable (AR)** - Customer management, sales orders, invoicing
3. **Accounts Payable (AP)** - Vendor management, purchase orders, payments
4. **Inventory Control (Stock)** - Item management, stock tracking, valuation
5. **Tax Processing (IRS)** - Tax calculation, compliance, reporting
6. **Master Data Management** - System parameters, code tables
7. **Reporting & Analytics** - Financial reports, business intelligence

### Key Business Processes
- **Order-to-Cash**: Customer orders → Delivery → Invoicing → Payment collection
- **Procure-to-Pay**: Purchase orders → Goods receipt → Invoice → Payment
- **Financial Close**: Transaction processing → Reconciliation → Financial statements
- **Inventory Cycle**: Demand planning → Purchasing → Receipt → Valuation

## MIGRATION STRATEGY

### Phase 1: Analysis and Foundation (MANDATORY FIRST)

1. **Explore Legacy System Structure**
   ```bash
   # Understand the COBOL codebase layout
   Legacy_App/
   ├── common/         # Shared utilities and data access
   ├── general/        # General Ledger (38 programs)
   ├── sales/          # Sales Ledger (79 programs) 
   ├── purchase/       # Purchase Ledger (63 programs)
   ├── stock/          # Stock Control (26 programs)
   ├── irs/            # Tax Processing (39 programs)
   └── copybooks/      # Data structures and shared code
   ```

2. **Study Documentation Architecture**
   ```
   documentation/
   ├── functional/     # Business requirements and processes
   ├── parsed/         # Code structure analysis
   └── subsystems/     # System architecture breakdown
   ```

3. **Analyze Key COBOL Programs** (Read these files directly!)
   - `Legacy_App/general/gl*.cbl` - Core GL functionality
   - `Legacy_App/sales/sl*.cbl` - Sales processing
   - `Legacy_App/purchase/pl*.cbl` - Purchase processing
   - `Legacy_App/stock/st*.cbl` - Inventory management
   - `Legacy_App/copybooks/*.cob` - Data structures

### Phase 2: Database Schema Creation

Extract ALL data structures from COBOL and create PostgreSQL schema:

```sql
-- Example: From Legacy_App/copybooks/fdledger.cob
-- 01 GL-MASTER-REC.
--    05 GL-ACCOUNT-NUMBER    PIC X(15).
--    05 GL-ACCOUNT-NAME      PIC X(40).
--    05 GL-ACCOUNT-TYPE      PIC X(2).

CREATE TABLE gl_accounts (
    account_number VARCHAR(15) PRIMARY KEY,
    account_name VARCHAR(40) NOT NULL,
    account_type CHAR(2) CHECK (account_type IN ('AS', 'LI', 'EQ', 'RE', 'EX')),
    created_date TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_date TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);
```

**CRITICAL**: Preserve exact field names, sizes, and business rules from COBOL.

### Phase 3: Backend Services Architecture

Create Python services that mirror COBOL program functionality:

```python
# backend/app/services/general_ledger_service.py
# Migrates logic from Legacy_App/general/gl050.cbl

class GeneralLedgerService:
    def __init__(self, db_session):
        self.db = db_session
    
    def post_journal_entry(self, entry_data: dict) -> dict:
        """
        Posts journal entry to GL
        Business logic preserved from gl050.cbl PROCEDURE DIVISION
        """
        # 1. Validate entry (exact COBOL validation rules)
        self._validate_journal_entry(entry_data)
        
        # 2. Check account numbers exist (COBOL file lookups)
        self._verify_accounts(entry_data['line_items'])
        
        # 3. Ensure debits = credits (COBOL arithmetic)
        self._validate_balance(entry_data['line_items'])
        
        # 4. Post to GL (exact COBOL posting logic)
        return self._create_gl_entries(entry_data)
```

### Phase 4: API Layer Design

```python
# backend/app/api/general_ledger.py
from fastapi import APIRouter, Depends, HTTPException
from app.services.general_ledger_service import GeneralLedgerService

router = APIRouter(prefix="/api/gl", tags=["General Ledger"])

@router.post("/journal-entries")
async def create_journal_entry(
    entry_data: JournalEntryRequest,
    gl_service: GeneralLedgerService = Depends()
):
    """Post new journal entry - mirrors gl050.cbl functionality"""
    try:
        result = gl_service.post_journal_entry(entry_data.dict())
        return {"success": True, "entry_id": result["entry_id"]}
    except ValidationError as e:
        raise HTTPException(status_code=400, detail=str(e))
```

### Phase 5: Frontend Architecture

Create clean, modern business application:

```typescript
// frontend/app/gl/journal-entry/page.tsx
export default function JournalEntryPage() {
  return (
    <div className="max-w-6xl mx-auto p-6">
      <h1 className="text-3xl font-bold text-gray-900 mb-6">
        Journal Entry
      </h1>
      
      <JournalEntryForm />
      <RecentEntriesTable />
    </div>
  )
}
```

**UI Design Principles:**
- Clean, professional accounting software look
- No migration or technical references
- Consistent icon sizing (w-6 h-6)
- Responsive design for all screen sizes
- Intuitive navigation between modules

## PROJECT STRUCTURE TO CREATE

```
Migrated_App/
├── backend/
│   ├── app/
│   │   ├── models/                 # SQLAlchemy models from COBOL structures
│   │   │   ├── __init__.py
│   │   │   ├── gl_models.py       # General Ledger entities
│   │   │   ├── sales_models.py    # Sales/AR entities  
│   │   │   ├── purchase_models.py # Purchase/AP entities
│   │   │   ├── inventory_models.py# Stock entities
│   │   │   └── system_models.py   # Master data entities
│   │   ├── services/              # Business logic from COBOL programs
│   │   │   ├── __init__.py
│   │   │   ├── gl_service.py      # GL program logic
│   │   │   ├── sales_service.py   # Sales program logic
│   │   │   ├── purchase_service.py# Purchase program logic
│   │   │   └── inventory_service.py# Stock program logic
│   │   ├── api/                   # REST endpoints
│   │   │   ├── __init__.py
│   │   │   ├── gl_routes.py
│   │   │   ├── sales_routes.py
│   │   │   ├── purchase_routes.py
│   │   │   └── inventory_routes.py
│   │   ├── core/                  # Configuration and utilities
│   │   │   ├── __init__.py
│   │   │   ├── config.py
│   │   │   ├── database.py
│   │   │   └── security.py
│   │   └── main.py               # FastAPI application
│   ├── requirements.txt
│   └── .env
├── frontend/
│   ├── app/                      # Next.js 14 app router
│   │   ├── gl/                   # General Ledger pages
│   │   │   ├── accounts/         
│   │   │   ├── entries/
│   │   │   ├── reports/
│   │   │   └── page.tsx
│   │   ├── sales/                # Sales module pages
│   │   ├── purchase/             # Purchase module pages  
│   │   ├── inventory/            # Inventory pages
│   │   ├── reports/              # Reporting pages
│   │   ├── globals.css           # Tailwind + custom styles
│   │   ├── layout.tsx            # Main layout
│   │   └── page.tsx              # Dashboard
│   ├── components/               # Reusable UI components
│   │   ├── forms/
│   │   ├── tables/
│   │   └── navigation/
│   ├── services/                 # API client code
│   ├── types/                    # TypeScript definitions
│   ├── next.config.js
│   ├── tailwind.config.js
│   ├── package.json
│   └── tsconfig.json
├── database/
│   ├── schema.sql               # Complete PostgreSQL schema
│   ├── seed_data.sql            # Initial system data
│   └── migrations/              # Schema evolution scripts
├── logs/                        # Application logs
├── start-acas.sh               # Complete setup and run script
└── README.md                   # Setup instructions
```

## DETAILED IMPLEMENTATION STEPS

### Step 1: Database Schema Generation
1. Read ALL `.cob` files in `Legacy_App/copybooks/`
2. Extract every FD (File Description) and 01 level record
3. Map COBOL data types to PostgreSQL:
   - `PIC X(n)` → `VARCHAR(n)`
   - `PIC 9(n)` → `NUMERIC(n,0)`
   - `PIC 9(n)V9(m)` → `NUMERIC(n,m)`
   - `PIC S9(n)` → `INTEGER` or `BIGINT`
4. Add proper constraints, indexes, and foreign keys
5. Create audit columns (created_date, updated_date, created_by)

### Step 2: SQLAlchemy Models
```python
# backend/app/models/gl_models.py
from sqlalchemy import Column, String, Numeric, DateTime, Integer
from sqlalchemy.types import Numeric as Decimal  # CRITICAL: Correct import
from sqlalchemy.ext.declarative import declarative_base

Base = declarative_base()

class GLAccount(Base):
    __tablename__ = "gl_accounts"
    
    account_number = Column(String(15), primary_key=True)
    account_name = Column(String(40), nullable=False)
    account_type = Column(String(2), nullable=False)
    balance = Column(Decimal(15, 2), default=0)
    # ... other fields from COBOL structure
```

### Step 3: Business Logic Migration
```python
# backend/app/services/gl_service.py
class GLService:
    def post_journal_entry(self, entry_data: dict) -> dict:
        """
        Migrates exact logic from Legacy_App/general/gl050.cbl
        """
        # VALIDATION SECTION (from COBOL validation paragraphs)
        if not entry_data.get('reference'):
            raise ValueError("Reference number required")
            
        # BALANCE CHECK (from COBOL arithmetic)
        debit_total = sum(line['debit'] for line in entry_data['lines'])
        credit_total = sum(line['credit'] for line in entry_data['lines'])
        
        if abs(debit_total - credit_total) > 0.01:  # COBOL precision
            raise ValueError("Entry not in balance")
            
        # POSTING LOGIC (from COBOL file updates)
        return self._post_to_ledger(entry_data)
```

### Step 4: Frontend Pages (Clean & Professional)
```typescript
// frontend/app/page.tsx (Dashboard)
export default function Dashboard() {
  const modules = [
    {
      name: 'General Ledger',
      description: 'Chart of accounts, journal entries, financial reports',
      href: '/gl',
      icon: ChartBarIcon,
      color: 'bg-blue-500'
    },
    // ... other modules
  ]

  return (
    <div>
      <h1 className="text-3xl font-bold text-gray-900 mb-8">
        ACAS Dashboard
      </h1>
      
      <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-6">
        {modules.map((module) => (
          <Link key={module.name} href={module.href}>
            <div className="acas-card hover:shadow-lg transition-shadow">
              <div className="flex items-center mb-4">
                <div className={`p-3 rounded-lg ${module.color}`}>
                  <module.icon className="w-6 h-6 text-white" />
                </div>
                <h3 className="ml-4 text-lg font-semibold">
                  {module.name}
                </h3>
              </div>
              <p className="text-gray-600">{module.description}</p>
            </div>
          </Link>
        ))}
      </div>
    </div>
  )
}
```

### Step 5: Setup Script
```bash
#!/bin/bash
# start-acas.sh - Complete setup and run script

# Create logs directory
mkdir -p logs

# Kill existing processes
pkill -f "uvicorn.*main:app" || true
pkill -f "next.*dev" || true

# Wait for ports to free
sleep 3

# Setup Python backend
cd backend
python -m venv venv
source venv/bin/activate
pip install -r requirements.txt

# Start backend
nohup uvicorn app.main:app --reload --host 0.0.0.0 --port 8000 > ../logs/backend.log 2>&1 &

# Setup frontend
cd ../frontend
npm install

# Start frontend  
nohup npm run dev > ../logs/frontend.log 2>&1 &

echo "ACAS started successfully!"
echo "Frontend: http://localhost:3000"
echo "Backend API: http://localhost:8000"
echo "API Docs: http://localhost:8000/docs"
```

## VALIDATION CHECKLIST

Before considering migration complete, verify:

### ✅ Technical Requirements
- [ ] All COBOL programs have equivalent Python services
- [ ] All COBOL data structures converted to PostgreSQL tables
- [ ] All business calculations produce identical results
- [ ] All validation rules preserved exactly
- [ ] Complete API coverage for all business functions
- [ ] All frontend modules accessible and functional
- [ ] Icons properly sized (w-6 h-6) throughout
- [ ] No 404 errors or broken links
- [ ] Clean, professional UI with no migration references

### ✅ Business Requirements  
- [ ] All GL functionality working (accounts, entries, reports)
- [ ] All sales processes functional (orders, invoicing, receipts)
- [ ] All purchase processes functional (POs, receipts, payments)
- [ ] All inventory tracking operational
- [ ] All tax calculations accurate
- [ ] All financial reports generating correctly
- [ ] User workflows intuitive and efficient

### ✅ Production Readiness
- [ ] Error handling comprehensive
- [ ] Logging properly configured
- [ ] Database migrations available
- [ ] Security properly implemented
- [ ] Performance acceptable
- [ ] Documentation complete

## EXECUTION APPROACH

1. **Start with analysis** - Read COBOL code and documentation thoroughly
2. **Build foundation** - Database schema and models first
3. **Migrate incrementally** - One module at a time (start with simplest)
4. **Test extensively** - Compare outputs with COBOL system
5. **Refine continuously** - Improve UI/UX based on business feedback

## SUCCESS CRITERIA

The migration is successful when:
1. **Functional parity** - All business processes work exactly as before
2. **User satisfaction** - Clean, intuitive interface that users prefer
3. **Performance improvement** - Faster than COBOL system
4. **Maintainability** - Modern, well-documented codebase
5. **Scalability** - Can handle business growth

Remember: This is not just a technical migration - it's a business transformation. Create an accounting system that users will love to use while preserving the robust business logic that has served the organization well.