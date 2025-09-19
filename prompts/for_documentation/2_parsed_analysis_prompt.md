# COBOL Analysis Suite - Comprehensive Code Intelligence Platform

## Context
You are creating an advanced analysis suite for the parsed ACAS COBOL structures. This suite will provide deep insights into code quality, architecture patterns, dependencies, and migration readiness through visualizations and comprehensive documentation.

## Primary Objective
Create a comprehensive analysis and visualization documentation for the parsed COBOL structures. In this step do the analysis directly, don't generate analysis scripts. However, DO create the SQL schema files (cobol-metrics-schema.sql and analysis-queries.sql) shown in section 4.

## Input Sources
- Parsed JSON structures in `documentation/parsed/parsed-structures/`
- Parser summary from `documentation/parsed/parser-summary.md`
- Original COBOL source in `Legacy_App/` for reference and validation

## Output Structure

### Root Directory
`documentation/parsed/parser_analysis/`

### 1. Visualizations Suite
`documentation/parsed/parser_analysis/visualizations/`

#### 1.1 Program Dependency Graph
**File**: `call-graph.md`  
**Instructions**:  
- Generate a **Markdown file** with a **Mermaid graph** showing:  
  - Hierarchical program call relationships  
  - Bidirectional dependency arrows  
  - Node sizing by complexity  
  - Color coding by module (GL/AR/AP/Stock/IRS)  
  - Optional click-to-expand notes (if supported by Mermaid)  
- The Markdown file should contain only valid Mermaid syntax ready to render.

#### 1.2 Procedure Flow Diagrams
**File**: `procedure-flow.md`  
**Instructions**:  
- Generate **flowcharts in Mermaid** inside a Markdown file that show:  
  - PERFORM chains within programs  
  - Section and paragraph relationships  
  - Conditional branching  
  - Loops and exit points  
  - Dead code identification  
- The Markdown file should contain only valid Mermaid syntax ready to render.

#### 1.3 COPYBOOK Usage Map
**File**: `copybook-usage.md`  
**Instructions**:  
- Generate **flowcharts in Mermaid** inside a Markdown file that show:  
  - COPYBOOK → Program relationships  
  - Shared data structure impact  
  - Change impact / version considerations  
  - Usage frequency heatmap (conceptual, via node coloring)  
- The Markdown file should contain only valid Mermaid syntax ready to render.

#### 1.4 Data Flow Visualization
**File**: `data-flow.md`  
**Instructions**:  
- Generate **flowcharts in Mermaid** inside a Markdown file that show:  
  - File access patterns (CRUD matrix)  
  - Data transformation flows  
  - Cross-program data dependencies  
  - Transaction boundaries  
  - Batch job sequences  
- The Markdown file should contain only valid Mermaid syntax ready to render.

#### 1.5 Program Flows
**Pattern**: `flow-[program-id].md`  
**Instructions**:  
- Produce **per-program detailed flows** (1 program flow per program) using Mermaid in Markdown:  
  - Internal control flow  
  - Data access sequence  
  - External calls  
  - Error handling paths  
  - Business logic visualization  
- The Markdown file should contain only valid Mermaid syntax ready to render.

### 2. Documentation Generation
`documentation/parsed/parser_analysis/technical_docs/`

#### 2.1 System Documentation
**File**: `system-documentation.md`
```markdown
# ACAS System Documentation

## Executive Summary
- System overview and purpose
- Key metrics and statistics
- Architecture patterns identified
- Technology stack assessment
- Migration readiness score

## System Architecture
- Module organization
- Integration patterns
- Data flow architecture
- Batch vs online processing
- External interfaces

## Quality Metrics
- Overall complexity analysis
- Maintainability assessment
- Technical debt inventory
- Code smell detection
- Refactoring candidates

## Dependency Analysis
- Program interdependencies
- COPYBOOK usage patterns
- External system interfaces
- Database/file dependencies
- Circular dependency detection
```

#### 2.3 Code Quality Analysis Report
**File**: `code-quality-analysis.md`
```markdown
# Code Quality Analysis

## Technical Debt Inventory
- Legacy constructs found
- GO TO usage statistics
- Missing error handling locations
- Hardcoded values inventory
- Dead code detection

## Code Smell Analysis
- Long methods/paragraphs
- Duplicate code blocks
- Complex conditionals
- Obsolete patterns

## Refactoring Priorities
[Programs requiring urgent refactoring]

## Modernization Readiness
[Assessment of code readiness for migration]
```

#### 2.2 COPYBOOK Index
**File**: `copybook-index.md`
```markdown
# COPYBOOK Index

| COPYBOOK | Path | Type | Used By | Data Elements | Change Risk |
|----------|------|------|---------|---------------|-------------|
| WSSTOCK | copybooks/wsstock.cob | Data | 15 programs | Stock record layout | High |
| ... | ... | ... | ... | ... | ... |

## Usage Patterns
- Most used COPYBOOKs
- Least used COPYBOOKs
- Candidates for consolidation
```

#### 2.4 Dependency Analysis Report
**File**: `dependency-analysis.md`
```markdown
# Dependency Analysis

## Call Dependencies
- Direct program calls
- Dynamic program calls
- External program interfaces
- Recursive call detection

## Data Dependencies
- File sharing patterns
- COPYBOOK dependencies
- Global data usage
- Parameter passing analysis

## Circular Dependencies
[List of circular dependency chains]

## Recommended Refactoring
[Dependency breaking strategies]
```

### 3. Metrics
`documentation/parsed/parser_analysis/metrics/`

#### 3.1 Report (Markdown Report)
**File**: `metrics_report.md`  
**Instructions**:  
- Produce a **Markdown report** that summarizes all metrics.
- Include the following sections:

##### 3.1.1 System Overview
- Total Programs
- Total Lines of Code
- Average Complexity
- Migration Risk
- Represent summary metrics as **cards or bullet points**.

##### 3.1.2 Complexity Metrics
- Average Cyclomatic Complexity
- Maximum Cyclomatic Complexity
- Complexity Distribution (Low, Medium, High, Very High)  
  - Represent as **tables or simple ASCII/Markdown charts**
- Hotspots:  
  - List programs with high complexity and recommendations  
  - Include cyclomatic, cognitive complexity, nesting level, parameter count

##### 3.1.3 Maintainability Index
- Overall System Maintainability Index with interpretation
- By Module Maintainability (GL/AR/AP/Stock/IRS)  
  - Use **tables**
- Contributing Factors (complexity, volume, comments, duplication, test coverage)
- Recommendations for improvement

### 4. Database Storage
`documentation/parsed/parser_analysis/database/`

#### 4.1 Database Schema
**File**: `cobol-metrics-schema.sql`
```sql
-- Programs table
CREATE TABLE programs (
    program_id VARCHAR(30) PRIMARY KEY,
    source_path VARCHAR(255) NOT NULL,
    program_type VARCHAR(20),
    module VARCHAR(20),
    lines_of_code INTEGER,
    complexity INTEGER,
    last_modified TIMESTAMP
);

-- Dependencies table
CREATE TABLE dependencies (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    caller_program VARCHAR(30),
    called_program VARCHAR(30),
    call_type VARCHAR(20),
    line_number INTEGER,
    FOREIGN KEY (caller_program) REFERENCES programs(program_id),
    FOREIGN KEY (called_program) REFERENCES programs(program_id)
);

-- Copybooks table
CREATE TABLE copybooks (
    copybook_name VARCHAR(50) PRIMARY KEY,
    source_path VARCHAR(255),
    data_elements INTEGER,
    used_by_count INTEGER
);

-- Metrics table
CREATE TABLE metrics (
    program_id VARCHAR(30),
    metric_type VARCHAR(50),
    metric_value REAL,
    calculated_at TIMESTAMP,
    FOREIGN KEY (program_id) REFERENCES programs(program_id)
);

-- File usage table
CREATE TABLE file_usage (
    program_id VARCHAR(30),
    file_name VARCHAR(50),
    operation VARCHAR(10),
    access_mode VARCHAR(20),
    FOREIGN KEY (program_id) REFERENCES programs(program_id)
);

-- Copybook usage table
CREATE TABLE copybook_usage (
    program_id VARCHAR(30),
    copybook_name VARCHAR(50),
    line_number INTEGER,
    division VARCHAR(20),
    section VARCHAR(50),
    FOREIGN KEY (program_id) REFERENCES programs(program_id),
    FOREIGN KEY (copybook_name) REFERENCES copybooks(copybook_name)
);
```

#### 4.2 Analysis Queries
**File**: `analysis-queries.sql`
```sql
-- Most complex programs
SELECT program_id, complexity, source_path
FROM programs
ORDER BY complexity DESC
LIMIT 20;

-- Circular dependencies
WITH RECURSIVE dep_chain AS (
    SELECT caller_program, called_program, 
           caller_program || ' -> ' || called_program AS chain
    FROM dependencies
    UNION ALL
    SELECT d.caller_program, d.called_program,
           dc.chain || ' -> ' || d.called_program
    FROM dependencies d
    JOIN dep_chain dc ON d.caller_program = dc.called_program
    WHERE dc.chain NOT LIKE '%' || d.called_program || '%'
)
SELECT chain FROM dep_chain
WHERE chain LIKE '%' || SUBSTR(chain, 1, INSTR(chain, ' ') - 1) || '%';

-- Programs without callers (potential dead code)
SELECT p.program_id, p.source_path
FROM programs p
LEFT JOIN dependencies d ON p.program_id = d.called_program
WHERE d.called_program IS NULL
  AND p.program_type != 'main';

-- COPYBOOK impact analysis
SELECT c.copybook_name, COUNT(*) as usage_count,
       GROUP_CONCAT(p.program_id) as used_by
FROM copybook_usage cu
JOIN programs p ON cu.program_id = p.program_id
JOIN copybooks c ON cu.copybook_name = c.copybook_name
GROUP BY c.copybook_name
ORDER BY usage_count DESC;
```

## Metrics Calculation Specifications

### 1. Cyclomatic Complexity
- Count decision points (IF, EVALUATE, PERFORM UNTIL)
- Add 1 for each binary decision
- Include error handling paths
- Consider GO TO statements

### 2. Halstead Metrics
- Operators: COBOL verbs and operations
- Operands: Variables and literals
- Calculate difficulty, volume, effort, bugs

### 3. Maintainability Index
- Formula: MAX(0, (171 - 5.2 * ln(HV) - 0.23 * CC - 16.2 * ln(LOC)) * 100 / 171)
- HV = Halstead Volume
- CC = Cyclomatic Complexity
- LOC = Lines of Code

### 4. Code Duplication
- Token-based similarity detection
- Minimum block size: 10 lines
- Similarity threshold: 90%

### 5. Dead Code Detection
- Unreachable paragraphs
- Unused variables
- Uncalled programs
- Orphaned copybooks

## Quality Checks

### Completeness Validation
- All parsed files processed
- All metrics calculated
- All visualizations generated
- Database fully populated

### Accuracy Verification
- Cross-reference validation
- Dependency chain verification
- Metrics sanity checks
- Manual sampling validation

### Performance Requirements
- Analysis completion < 5 minutes
- Interactive visualizations < 100ms response
- Database queries < 1 second
- Memory usage < 4GB

## Success Criteria

1. **Comprehensive Coverage**
   - 100% of parsed files analyzed
   - All dependency relationships mapped
   - Complete metrics for all programs

2. **Actionable Insights**
   - Clear refactoring priorities
   - Migration complexity assessment
   - Risk identification and mitigation

3. **Interactive Experience**
   - Smooth visualization navigation
   - Responsive dashboard
   - Exportable reports

4. **Integration Ready**
   - Database for custom queries
   - API-ready metrics
   - CI/CD integration capable

Remember: This analysis is critical for understanding the current system state and planning the migration strategy. The insights generated here will drive architectural decisions and risk assessments throughout the modernization project.

## Quality Checklist:
- [ ] All visualizations render correctly in Markdown
- [ ] Metrics calculated for 100% of parsed programs
- [ ] No orphaned dependencies in graphs
- [ ] SQL schema creates without errors
- [ ] All high-complexity programs identified
- [ ] Technical debt clearly quantified
- [ ] Migration risks documented with severity