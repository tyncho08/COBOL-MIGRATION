# Comprehensive Functional Documentation - ACAS Business Logic Extraction

## Context
You are analyzing ACAS - Applewood Computers Accounting System, a COBOL-based accounting application that includes Sales, Purchase and Nominal Ledgers (AR/AP/GL) plus Stock Control and IRS (Incomplete Records System). The system uses GnuCOBOL compiler and supports both standard COBOL files (sequential/indexed) and MySQL/MariaDB databases.

## Primary Objective
Generate exhaustive functional documentation of this COBOL accounting system to enable the technical team to identify subsystems, create architectural diagrams, develop comprehensive technical documentation, and plan a complete modernization strategy. Focus on WHAT the system does (functional) before HOW it does it (technical implementation).

## Phase 1: COBOL-Specific Repository Analysis
**Important:** In this step do the analysis directly, don’t generate a script.

### 1.1 Intelligent Repository Scanning
```
Perform a comprehensive scan with COBOL-specific criteria:

INCLUDE (Priority Order):
- COBOL source files (.cob, .cbl, .cpy)
- Copybooks and includes (.copy, .cpy)
- JCL files if present (.jcl)
- SQL scripts and database schemas (.sql)
- Configuration files (config.*, *.conf, *.ini)
- Screen definitions and forms (.scr, .frm)
- Report layouts (.rpt)
- Batch processing scripts (.sh, .bat, .cmd)
- Make files and compilation scripts
- Test data files and validation scripts

EXCLUDE:
- README.md, CHANGELOG, LICENSE files
- .gitignore, .dockerignore
- Compiled objects (.o, .so, .exe)
- Temporary and backup files
- Documentation folders (unless containing schemas)
```

### 1.2 COBOL Architecture Identification
```
Document the following COBOL-specific patterns:
1. Program organization (main programs vs subprograms)
2. DIVISION structure patterns across programs
3. File handling approach (sequential, indexed, relative)
4. Database integration method (embedded SQL, file-based)
5. COPY book usage and shared data structures
6. CALL hierarchy and program dependencies
7. Batch vs interactive program identification
8. Report generation mechanisms
9. Error handling patterns
10. Transaction processing approach
```

## Phase 2: Accounting Domain Analysis

### 2.1 Module Identification by Business Function
```
Map COBOL programs to accounting modules:

CORE ACCOUNTING MODULES:
- General Ledger (GL/Nominal Ledger)
  * Chart of Accounts management
  * Journal entries
  * Trial balance
  * Financial statements
  
- Accounts Receivable (AR/Sales Ledger)
  * Customer master management
  * Invoice generation
  * Payment processing
  * Aging reports
  
- Accounts Payable (AP/Purchase Ledger)
  * Vendor management
  * Purchase orders
  * Payment scheduling
  * Liability tracking
  
- Inventory Control (Stock Control)
  * Item master maintenance
  * Stock movements
  * Valuation methods
  * Reorder processing
  
- IRS (Incomplete Records System)
  * Special handling routines
  * Reconciliation processes
```

### 2.2 For Each COBOL Program, Document:
```yaml
program_name: [PROGRAM-ID from IDENTIFICATION DIVISION]
source_file: [path/to/file.cob]
program_type: [main|subprogram|copybook|batch|interactive]
business_module: [GL|AR|AP|INVENTORY|IRS|COMMON]

data_division_analysis:
  files_used:
    - file_name: [from SELECT statements]
      organization: [sequential|indexed|relative]
      key_fields: [if indexed]
      record_layout: [key fields and types]
  
  working_storage:
    - key_variables: [business-critical fields]
    - control_flags: [processing indicators]
    - accumulators: [totals, counters]
  
  linkage_section: [parameters passed between programs]

procedure_division_analysis:
  main_sections:
    - section_name:
      purpose: [business function]
      database_operations: [CRUD operations performed]
      calculations: [business rules implemented]
      validations: [data integrity checks]
  
  called_programs:
    - program_id: [CALL targets]
      purpose: [why called]
      parameters: [data passed]
  
  file_operations:
    - operation: [READ|WRITE|REWRITE|DELETE]
      file: [target file]
      business_purpose: [why this operation]

business_rules:
  - rule: [accounting principle or business logic]
  - implementation: [how coded]

error_handling:
  - condition: [error scenario]
  - action: [response/recovery]

report_generation: [if applicable]
  - report_name:
  - frequency: [daily|monthly|on-demand]
  - key_calculations:
```

## Phase 3: Data Flow and Integration Analysis

### 3.1 File/Database Schema Mapping
```
Create comprehensive data dictionary:

For Each File/Table:
- Physical name and location
- Logical business name
- Record/Row structure
- Key fields and indexes
- Relationships to other files
- Programs that access it
- CRUD matrix (which programs Create/Read/Update/Delete)
- Data retention rules
- Backup/Recovery considerations
```

### 3.2 Transaction Flow Documentation
```
Map complete transaction lifecycles:

Example - Sales Invoice Flow:
1. Entry point (program/screen)
2. Data validation steps
3. File updates sequence
4. GL posting logic
5. Inventory adjustments
6. Report generation triggers
7. Audit trail creation
8. Error rollback procedures
```

## Phase 4: COBOL-Specific Visualizations

### 4.1 Program Call Hierarchy
```mermaid
graph TD
    %% Generate a tree showing CALL relationships
    %% Include PERFORM chains within programs
    %% Show copybook dependencies
    %% Indicate batch vs online programs with different styles
```

### 4.2 File Access Matrix
```
Create a matrix showing:
- Rows: COBOL Programs
- Columns: Files/Tables
- Cells: C(reate), R(ead), U(pdate), D(elete) operations
- Color coding: Frequency of access
```

### 4.3 Accounting Module Interaction Diagram
```mermaid
graph LR
    %% Show data flow between accounting modules
    %% Include batch processing sequences
    %% Mark critical integration points
    %% Show external interfaces
```

## Phase 5: Technical Debt and Migration Readiness

### 5.1 Code Quality Assessment
```
Identify for each program:
- Obsolete COBOL constructs (if any)
- GnuCOBOL compatibility issues
- Hard-coded values that should be parameterized
- Missing error handling
- Performance bottlenecks (nested loops, inefficient searches)
- Dead code sections
- Duplicate logic across programs
```

### 5.2 Database Integration Analysis
```
Document current state and recommendations:
- File-based vs MySQL/MariaDB usage
- Migration candidates (files that should be tables)
- Transaction boundary issues
- Locking and concurrency handling
- Backup and recovery gaps
```

## Phase 6: Comprehensive Documentation Generation

### 6.1 Executive Summary Structure
```
1. System Overview
   - Business purpose and scope
   - Key accounting functions supported
   - User base and transaction volumes
   - Technology stack summary

2. Architecture Assessment
   - Current COBOL architecture pattern
   - Module interdependencies
   - Data management approach
   - Integration points

3. Functional Coverage
   - Implemented accounting standards
   - Compliance features
   - Reporting capabilities
   - Audit trail completeness

4. Technical Health
   - Code quality metrics
   - Maintenance complexity
   - Security considerations
   - Performance characteristics

5. Modernization Recommendations
   - Priority refactoring areas
   - Database migration strategy
   - UI modernization options
   - API enablement possibilities
```

### 6.2 Detailed Documentation Artifacts
`documentation/functional/analysis/`

Generate these documents:

1. **documentation/functional/analysis/functional_overview.md**
   - System purpose and scope
   - Business functions (GL, AR, AP, Stock, IRS)
   - User roles and workflows
   - Technology stack (GnuCOBOL, file vs MySQL/MariaDB)
   - User base / transaction volumes
   - Functional coverage / compliance

2. **documentation/functional/analysis/program_catalog.md**
   - Alphabetical listing of all programs
   - Business function mapping
   - Dependency information
   - Maintenance priority

3. **documentation/functional/analysis/data_dictionary.md**
   - All files and database tables
   - Field-level documentation
   - Business meaning of codes
   - Validation rules

4. **documentation/functional/analysis/business_flow.md**
   - End-to-end process documentation
   - Decision trees for complex logic
   - Accounting period processing
   - Year-end procedures

5. **documentation/functional/analysis/system_architecture.md**
   - Call graphs and dependencies
   - File organization strategy
   - Batch job scheduling
   - Error handling patterns
   - Module structure
   - Integration patterns (embedded SQL vs file)
   - GnuCOBOL compatibility
   - Technical debt analysis
   - Migration recommendations

6. **documentation/functional/analysis/cobol_patterns.md**
   - Common coding patterns used
   - Naming conventions
   - Program structure standards
   - Best practices followed/violated

7. **documentation/functional/analysis/migration_roadmap.md**
   - Modernization opportunities
   - Risk assessment
   - Phased approach recommendations
   - Technology alternatives

8. **documentation/functional/analysis/calculation_engine.md**
   - Financial calculation documentation
   - Formula/algorithm details
   - Rounding rules
   - Currency handling
   - Tax calculations
   - Discount/pricing logic
   - Interest computations
   - Depreciation methods

## Phase 7: Accounting-Specific Analysis

### 7.1 Compliance and Controls
```
Document:
- Segregation of duties implementation
- Audit trail completeness
- Data validation rules
- Approval workflows
- Security controls
- Regulatory compliance features
```

## Output Quality Criteria

Your documentation should enable:
1. New developers to understand system functionality without reading code
2. Architects to design modernization strategies
3. Business analysts to validate requirements coverage
4. Testers to design comprehensive test cases
5. Operations teams to understand batch dependencies
6. Database administrators to optimize data structures

## Interactive Analysis Process

Before starting, answer these questions:
1. What version of GnuCOBOL compatibility is required?
2. Is this using file-based or database storage primarily?
3. What accounting standards/regulations does it support?
4. What is the typical transaction volume?
5. Are there any custom business rules or industry-specific features?

Then proceed with iterative analysis:
- **First pass**: Program inventory and structure
- **Second pass**: Business function mapping
- **Third pass**: Data flow and dependencies
- **Fourth pass**: Detailed logic documentation
- **Fifth pass**: Synthesis and recommendations

## Summary

Remember: Focus on WHAT the system does (functional) before HOW it does it (technical implementation). The goal is to create documentation that bridges business understanding and technical implementation. Every business rule, calculation, and process flow must be captured and documented in business terms that stakeholders can understand.

## Quality Checklist:
- [ ] All business processes have clear start/end points
- [ ] Every program mapped to a business function
- [ ] All calculations explained in business terms
- [ ] Data dictionary complete with business meanings
- [ ] No technical jargon in functional descriptions
- [ ] Compliance requirements identified
- [ ] All user roles and workflows documented