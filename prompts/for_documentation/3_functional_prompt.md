# Comprehensive Functional Documentation - ACAS Business Logic Extraction

## Context
You are analyzing ACAS - Applewood Computers Accounting System, a COBOL-based accounting application that includes Sales, Purchase and Nominal Ledgers (AR/AP/GL) plus Stock Control and IRS (Incomplete Records System). The system uses GnuCOBOL compiler and supports both standard COBOL files (sequential/indexed) and MySQL/MariaDB databases.

## Primary Objective
Generate exhaustive functional documentation of this COBOL accounting system to enable the technical team to identify subsystems, create architectural diagrams, develop comprehensive technical documentation, and plan a complete modernization strategy. Focus on WHAT the system does (functional) before HOW it does it (technical implementation).

## Phase 1: COBOL-Specific Repository Analysis

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

Generate these documents:

1. **FUNCTIONAL_OVERVIEW.md**
   - System purpose and scope
   - Business functions (GL, AR, AP, Stock, IRS)
   - User roles and workflows
   - Technology stack (GnuCOBOL, file vs MySQL/MariaDB)
   - User base / transaction volumes
   - Functional coverage / compliance

2. **PROGRAM_CATALOG.md**
   - Alphabetical listing of all programs
   - Business function mapping
   - Dependency information
   - Maintenance priority

3. **DATA_DICTIONARY.md**
   - All files and database tables
   - Field-level documentation
   - Business meaning of codes
   - Validation rules

4. **BUSINESS_FLOWS.md**
   - End-to-end process documentation
   - Decision trees for complex logic
   - Accounting period processing
   - Year-end procedures

5. **TECHNICAL_ARCHITECTURE.md**
   - Call graphs and dependencies
   - File organization strategy
   - Batch job scheduling
   - Error handling patterns

6. **COBOL_PATTERNS.md**
   - Common coding patterns used
   - Naming conventions
   - Program structure standards
   - Best practices followed/violated

7. **MIGRATION_ROADMAP.md**
   - Modernization opportunities
   - Risk assessment
   - Phased approach recommendations
   - Technology alternatives

8. **ARCHITECTURE_ANALYSIS.md**
   - Module structure
   - Integration patterns (embedded SQL vs file)
   - GnuCOBOL compatibility
   - Technical debt analysis
   - Migration recommendations

9. **CALCULATION_ENGINE.md**
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

### 7.2 Calculation Engine Documentation
```
For each financial calculation:
- Formula/algorithm used
- Rounding rules
- Currency handling
- Tax calculations
- Discount/pricing logic
- Interest computations
- Depreciation methods
```

## Special Considerations for COBOL Analysis

### COBOL-Specific Patterns to Document:
```
1. PERFORM paragraph/section usage patterns
2. GO TO usage (if any) and control flow impact
3. Level 88 condition names and business meaning
4. REDEFINES clauses and data structure variants
5. OCCURS clauses and table handling
6. STRING/UNSTRING operations for data parsing
7. SORT/MERGE usage for batch processing
8. File status checking patterns
9. COMMIT/ROLLBACK transaction boundaries
10. Embedded SQL or file-based persistence
```

### Legacy System Indicators:
```
Flag these for modernization consideration:
- ALTER statements (obsolete)
- GO TO DEPENDING ON (consider refactoring)
- Excessive GO TO usage
- Missing structured programming constructs
- Hardcoded file paths
- Platform-specific code
- Y2K workarounds still in place
- Commented-out code blocks
- Inconsistent naming conventions
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

## Visualization Requirements

Create interactive visualizations in `documentation/functional/visualization/`:

1. **call-graph.html**: Interactive program dependency graph
   - CALL hierarchy visualization
   - PERFORM chains within programs
   - Batch vs online program identification
   - Copybook dependency mapping

2. **procedure-flow.html**: Critical procedure flowcharts
   - Main business process flows
   - Decision points and branching
   - Error handling paths

3. **copybook-usage.html**: COPYBOOK dependency maps
   - Which programs use which copybooks
   - Impact analysis for changes

4. **file-access-matrix.html**: Programs × Files CRUD matrix
   - Color-coded by operation type
   - Frequency indicators
   - Access pattern analysis

5. **accounting-module-interaction.html**: Module interaction diagram
   - GL, AR, AP, Inventory, IRS data flows
   - Integration points
   - Batch processing sequences

## Summary

Remember: Focus on WHAT the system does (functional) before HOW it does it (technical implementation). The goal is to create documentation that bridges business understanding and technical implementation. Every business rule, calculation, and process flow must be captured and documented in business terms that stakeholders can understand.

Think ultra mega hard at each step.