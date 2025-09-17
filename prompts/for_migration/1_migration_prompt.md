# PROMPT: COBOL to Modern Stack Migration

## Context
You have a COBOL accounting system (ACAS) that needs to be migrated to a modern technology stack. You MUST analyze BOTH the original COBOL source code AND the generated documentation to understand the complete system.

## Available Resources
```
project_root/
├── Legacy_App/                 # ORIGINAL COBOL SOURCE CODE - READ THIS!
│
├── documentation/              # GENERATED DOCUMENTATION - READ THIS TOO!
│   ├── parsed/                # Code structure and dependencies
│   ├── functional/            # Business logic documentation  
│   └── subsystems/            # System architecture
│
└── Migrated_App/              # CREATE EVERYTHING HERE
```

## Target Stack
- **Backend**: Python with FastAPI
- **Frontend**: Next.js with TypeScript
- **Database**: PostgreSQL

## CRITICAL: Use BOTH Sources

### From COBOL Source Code (Legacy_App/):
- Exact business logic and calculations
- Actual field names and data types
- Real validation rules
- Actual program flow and conditions
- Precise formulas and algorithms
- File structures and record layouts

### From Documentation (documentation/):
- System overview and architecture
- Business purpose of each program
- Data relationships and dependencies
- Process flows and sequences
- Subsystem boundaries
- Modernization recommendations

## MIGRATION PROCESS

### Step 1: Analyze COBOL Source First
```
For each .cbl/.cob file in Legacy_App/:
1. Read the IDENTIFICATION DIVISION - understand program purpose
2. Read the DATA DIVISION - extract all data structures
3. Read the PROCEDURE DIVISION - extract exact business logic
4. Note all CALL statements - understand program dependencies
5. Note all file operations - understand data flow
```

### Step 2: Cross-Reference with Documentation
```
For each COBOL program found:
1. Find it in documentation/functional/COMPONENT_CATALOG.md
2. Understand its business context
3. Check documentation/functional/DATA_DICTIONARY.md for data mappings
4. Review documentation/subsystems/ for architectural placement
```

### Step 3: Generate Modern Code

## PROJECT STRUCTURE TO CREATE

```
Migrated_App/
├── backend/
│   ├── app/
│   │   ├── models/
│   │   │   └── [Create one file per COBOL file/table]
│   │   ├── services/
│   │   │   └── [Create one file per COBOL program]
│   │   ├── api/
│   │   │   └── routes.py
│   │   └── main.py
│   └── requirements.txt
│
├── frontend/
│   ├── app/
│   │   └── [Create pages for main functions]
│   ├── components/
│   ├── services/
│   └── package.json
│
└── database/
    └── schema.sql
```

## DIRECT CONVERSION RULES

### 1. COBOL Data to PostgreSQL
```sql
-- Read from Legacy_App/[module]/[program].cbl:
-- Look for: FD entries, 01 level records, WORKING-STORAGE

-- Example from actual COBOL:
-- FD CUSTOMER-FILE.
-- 01 CUSTOMER-REC.
--    05 CUST-CODE    PIC X(6).
--    05 CUST-NAME    PIC X(30).

-- Convert to:
CREATE TABLE customers (
    cust_code VARCHAR(6) PRIMARY KEY,
    cust_name VARCHAR(30)
);
```

### 2. COBOL Logic to Python
```python
# Read from Legacy_App/[module]/[program].cbl:
# Look for: PROCEDURE DIVISION paragraphs

# Example from actual COBOL:
# CALCULATE-BALANCE.
#     MOVE ZERO TO WS-BALANCE.
#     PERFORM VARYING I FROM 1 BY 1 UNTIL I > 12
#         ADD MONTH-AMOUNT(I) TO WS-BALANCE
#     END-PERFORM.

# Convert to Python:
def calculate_balance(month_amounts):
    balance = 0
    for amount in month_amounts:
        balance += amount
    return balance
```

### 3. COBOL Program to Service
```python
# For each program like GL001.cbl in Legacy_App/general/:
# Migrated_App/backend/app/services/gl001_service.py

class GL001Service:
    """
    Direct migration of GL001.cbl
    Original location: Legacy_App/general/GL001.cbl
    """
    
    def __init__(self, db_session):
        self.db = db_session
    
    # Create one method per COBOL paragraph/section
    def post_entry(self, entry_data):
        # Exact logic from PROCEDURE DIVISION
        pass
```

### 4. Create REST Endpoints
```python
# Migrated_App/backend/app/api/routes.py
# Create one endpoint per COBOL program main function

@router.post("/gl/post-entry")
def post_journal_entry(data: dict):
    # Call the migrated COBOL logic
    service = GL001Service(db)
    return service.post_entry(data)
```

## EXAMPLES FROM ACTUAL CODE

### Example 1: Find and Migrate a COBOL Program
```python
# 1. Open Legacy_App/general/GL001.cbl
# 2. Read IDENTIFICATION DIVISION:
#    PROGRAM-ID. GL001.
#    *> General Ledger Entry Posting

# 3. Read DATA DIVISION for structures
# 4. Read PROCEDURE DIVISION for logic
# 5. Create: Migrated_App/backend/app/services/general_ledger.py

def post_journal_entry(entry):
    # Migrate exact logic from GL001.cbl
    # Preserve all validations
    # Keep same calculations
    pass
```

### Example 2: Find and Convert Data Structure
```python
# 1. Open Legacy_App/copybooks/CUSTOMER.cpy
# 2. Find the record structure
# 3. Create matching PostgreSQL table
# 4. Create matching Python model
```

## DELIVERABLES

### 1. Database Schema (`Migrated_App/database/schema.sql`)
- Read ALL .cbl files in Legacy_App/
- Extract ALL FD (File Description) entries
- Convert each to a CREATE TABLE statement
- Preserve exact field names and sizes

### 2. Python Services (`Migrated_App/backend/app/services/`)
- Read ALL .cbl programs in Legacy_App/
- Create one Python file per COBOL program
- Migrate PROCEDURE DIVISION logic exactly
- Keep all business rules and validations

### 3. API Endpoints (`Migrated_App/backend/app/api/routes.py`)
- Create REST endpoints for each COBOL program function
- Map to the migrated Python services

### 4. Frontend Pages (`Migrated_App/frontend/app/`)
- Create pages for main business functions
- Base on COBOL screen programs (if any)
- Use documentation to understand user workflows

### 5. Data Models (`Migrated_App/backend/app/models/`)
- SQLAlchemy models matching database schema
- Based on COBOL record structures

## EXECUTION ORDER

1. **First**: Open and read actual COBOL files in Legacy_App/
2. **Second**: Read documentation to understand context
3. **Third**: Create database schema from COBOL FD entries
4. **Fourth**: Migrate each COBOL program to Python service
5. **Fifth**: Create API endpoints
6. **Sixth**: Create frontend pages
7. **Seventh**: Create data migration script

## IMPORTANT NOTES

- **READ THE COBOL CODE** - Don't just rely on documentation
- **PRESERVE EXACT LOGIC** - Every IF, PERFORM, COMPUTE must be migrated
- **KEEP FIELD NAMES** - Use same names as COBOL (just lowercase)
- **MATCH CALCULATIONS** - Financial math must be identical
- **NO ASSUMPTIONS** - If it's in COBOL, migrate it exactly

## START HERE

1. List all .cbl files in Legacy_App/
2. Read each file to understand what it does
3. Cross-reference with documentation/functional/COMPONENT_CATALOG.md
4. Begin migration with the simplest program first
5. Generate complete working code in Migrated_App/

Remember: The COBOL source code is the truth. The documentation helps explain it, but the actual code must be migrated exactly as it works.