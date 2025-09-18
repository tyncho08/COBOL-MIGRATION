# Execute Functional Analysis (ACAS)

Task: Generate exhaustive functional documentation and artifacts for ACAS (Applewood Computers Accounting System) to enable understanding, subsystem identification, architecture diagrams, and modernization roadmap.

Input:
- Original COBOL source: `.cob`, `.cbl`, `.cpy`, `.copy`
- Parsed JSON structures from Phases 1–2
- Business domain: Accounting System (AR/AP/GL + Stock + IRS)
- Runtime: GnuCOBOL
- Storage: COBOL files and MySQL/MariaDB

Repository Scan:
- INCLUDE: `.cob`, `.cbl`, `.cpy`, `.copy`, `.jcl`, `.sql`, `config.*`, `*.conf`, `*.ini`, `.scr`, `.frm`, `.rpt`, `.sh`, `.bat`, `.cmd`, make/compile scripts, test data
- EXCLUDE: README, CHANGELOG, LICENSE, `.gitignore`, `.dockerignore`, compiled objects (`.o`, `.so`, `.exe`), temporary/backup files, non-schema documentation folders

Outputs (`documentation/functional/`):

1. FUNCTIONAL_OVERVIEW.md
   - System purpose and scope
   - Business functions (GL, AR, AP, Stock, IRS)
   - User roles and workflows
   - Technology stack (GnuCOBOL, file vs MySQL/MariaDB)
   - User base / transaction volumes
   - Functional coverage / compliance
   - Executive summary structure: System Overview, Architecture Assessment, Functional Coverage, Technical Health, Modernization Recommendations

2. COMPONENT_CATALOG.md / PROGRAM_CATALOG.md
   - Alphabetical program list with PROGRAM-ID + source_file
   - Business module mapping
   - Module organization (GL, AR, AP, Stock, IRS)
   - Batch vs interactive classification
   - Dependencies: CALL targets, PERFORM chains
   - Maintenance priority / migration candidates
   - Template per program:
```yaml
program_name:
source_file:
program_type: [main|subprogram|copybook|batch|interactive]
business_module:
data_division_analysis:
  files_used: [...]
  working_storage: [...]
  linkage_section: [...]
procedure_division_analysis:
  main_sections: [...]
  called_programs: [...]
  file_operations: [...]
business_rules: [...]
error_handling: [...]
report_generation: [...]
```

3. `BUSINESS_FLOWS.md`

   * End-to-end flows: Sales, Purchase, Financial Close, Inventory
   * Step-by-step maps: entry → validations → file updates → GL posting → inventory → reports → audit → rollback

4. `DATA_DICTIONARY.md`

   * File/table name, structure, key fields, indexes
   * Programs accessing the file
   * CRUD matrix
   * Retention, backup/recovery
   * Validations and code lookups

5. `ARCHITECTURE_ANALYSIS.md`

   * Module structure
   * Integration patterns (embedded SQL vs file)
   * GnuCOBOL compatibility
   * Technical debt: obsolete constructs, hard-coded values, missing error handling, performance bottlenecks, dead/duplicate code
   * Batch vs interactive, scheduling, dependencies
   * Transaction boundaries, commit/rollback
   * Migration recommendations: DB migration, API, UI modernization

Additional Artifacts:

* `COBOL_PATTERNS.md`: PERFORM, GO TO, Level-88, REDEFINES, OCCURS, STRING/UNSTRING, SORT/MERGE, file status, embedded SQL
* `TECHNICAL_ARCHITECTURE.md`: call graphs, file organization, batch scheduling, error patterns
* `MIGRATION_ROADMAP.md`: modernization opportunities, risk, phased recommendations
* `COBOL_COMPATIBILITY_ISSUES.md`: GnuCOBOL compatibility notes
* `CALCULATION_ENGINE.md`: formulas, rounding, currency, taxes, discounts, interest, depreciation

Visualizations:

* `call-graph.html`: CALL hierarchy, PERFORM chains, batch vs online, copybook dependencies
* `procedure-flow\.html`: critical procedure flowcharts
* `copybook-usage.html`: dependency maps
* `file-access-matrix`: programs × files/tables CRUD, color-coded
* `accounting-module-interaction`: GL, AR, AP, Inventory, IRS interactions

Accounting Analysis:

* Chart of Accounts, GL posting, trial balance, AR aging, AP liabilities, tax rules, audit trail, IRS routines, discounts/pricing/interest/depreciation rules

Technical Debt / Code Quality:

* Obsolete COBOL constructs
* Hard-coded values
* Missing error handling
* Performance bottlenecks
* Dead/duplicate code
* Inconsistent naming

Database Integration:

* File vs MySQL/MariaDB usage
* Migration candidates
* Transaction boundaries
* Locking/concurrency
* Backup/recovery

Output Goals:

* Enable developers, architects, analysts, testers, operations, DBAs to understand and act on system

Interactive Analysis:

* Iterative passes: program inventory, business mapping, data flows, detailed logic, synthesis/recommendations
* Focus: *what system does* before *how it does it*

Think ultra mega hard at each step.