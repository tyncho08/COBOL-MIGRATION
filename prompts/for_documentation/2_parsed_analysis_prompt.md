# COBOL Analysis Suite - Comprehensive Code Intelligence Platform

## Context
You are creating an advanced analysis suite for the parsed ACAS COBOL structures. This suite will provide deep insights into code quality, architecture patterns, dependencies, and migration readiness through interactive visualizations and comprehensive documentation.

## Primary Objective
Build a comprehensive analysis and visualization platform that transforms parsed COBOL structures into actionable intelligence for architects, developers, and migration teams.

## Input Sources
- Parsed JSON structures in `documentation/parsed/parsed-structures/`
- Parser summary from `documentation/parsed/parser-summary.json`
- Original COBOL source in `Legacy_App/` for reference and validation

## Output Structure

### Root Directory
`documentation/parsed/parser_analysis/`

### 1. Visualizations Suite
`documentation/parsed/parser_analysis/visualizations/`

#### 1.1 Program Dependency Graph
**File**: `call-graph.html`
```html
<!-- Interactive D3.js visualization showing: -->
- Hierarchical program call relationships
- Bidirectional dependency arrows
- Node sizing by complexity
- Color coding by module (GL/AR/AP/Stock/IRS)
- Click-to-expand details
- Search and filter capabilities
- Export to SVG/PNG
```

#### 1.2 Procedure Flow Diagrams
**File**: `procedure-flow.html`
```html
<!-- Interactive flowcharts showing: -->
- PERFORM chains within programs
- Section and paragraph relationships
- Conditional branching visualization
- Loop detection and highlighting
- Dead code identification
- Complexity hot spots
```

#### 1.3 COPYBOOK Usage Map
**File**: `copybook-usage.html`
```html
<!-- Dependency visualization showing: -->
- COPYBOOK to program relationships
- Shared data structure impact
- Version control considerations
- Change impact analysis
- Usage frequency heat map
```

#### 1.4 Data Flow Visualization
**File**: `data-flow.html`
```html
<!-- File and data movement showing: -->
- File access patterns (CRUD matrix)
- Data transformation flows
- Cross-program data dependencies
- Transaction boundaries
- Batch job sequences
```

#### 1.5 Individual Program Flows
**Pattern**: `flow-[program-id].html`
```html
<!-- Per-program detailed flow showing: -->
- Internal control flow
- Data access sequence
- External calls
- Error handling paths
- Business logic visualization
```

### 2. Documentation Generation
`documentation/parsed/parser_analysis/docs/`

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

#### 2.2 Subsystem Documentation
**File**: `subsystem-documentation.md`
```markdown
# Subsystem Analysis

## Identified Subsystems
1. General Ledger (GL)
2. Accounts Receivable (AR)  
3. Accounts Payable (AP)
4. Inventory Management (Stock)
5. IRS Compliance
6. Common Services

## Per Subsystem:
- Component programs
- Internal cohesion metrics
- External coupling analysis
- Business functions
- Migration complexity
```

#### 2.3 Program Index
**File**: `program-index.md`
```markdown
# Program Index

| Program ID | Source Path | Type | Module | Lines | Complexity | Dependencies | Risk Level |
|------------|-------------|------|--------|-------|------------|--------------|------------|
| SL000 | sales/sl000.cbl | Main | Sales | 1500 | 15 | 5 | Medium |
| ... | ... | ... | ... | ... | ... | ... | ... |

## Program Categories
- Main Programs: [list]
- Subprograms: [list]
- Batch Programs: [list]
- Online Programs: [list]
- Utility Programs: [list]
```

#### 2.4 COPYBOOK Index
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
- Version control recommendations
```

#### 2.5 Dependency Analysis Report
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

### 3. Metrics Dashboard
`documentation/parsed/parser_analysis/dashboard/`

#### 3.1 Interactive Dashboard
**File**: `index.html`
```html
<!DOCTYPE html>
<html>
<head>
    <title>ACAS COBOL Metrics Dashboard</title>
    <!-- Include Chart.js, D3.js -->
</head>
<body>
    <!-- Navigation -->
    <nav>System Overview | Complexity Analysis | Quality Metrics | Migration Readiness</nav>
    
    <!-- Summary Cards -->
    <div class="metrics-summary">
        <div class="card">Total Programs: 278</div>
        <div class="card">Total Lines: 500K</div>
        <div class="card">Avg Complexity: 12.5</div>
        <div class="card">Migration Risk: Medium</div>
    </div>
    
    <!-- Interactive Charts -->
    <div class="charts">
        <!-- Complexity Distribution -->
        <!-- Module Size Comparison -->
        <!-- Dependency Network -->
        <!-- Quality Trends -->
    </div>
    
    <!-- Detailed Tables -->
    <div class="data-tables">
        <!-- Sortable/Filterable Program List -->
        <!-- Risk Assessment Matrix -->
        <!-- Refactoring Priorities -->
    </div>
</body>
</html>
```

#### 3.2 Complexity Metrics
**File**: `complexity-metrics.json`
```json
{
  "timestamp": "2024-01-15T12:00:00Z",
  "summary": {
    "averageCyclomaticComplexity": 12.5,
    "maxCyclomaticComplexity": 85,
    "complexityDistribution": {
      "low (1-10)": 150,
      "medium (11-20)": 80,
      "high (21-50)": 40,
      "very high (>50)": 8
    }
  },
  "programs": [
    {
      "programId": "SL000",
      "metrics": {
        "cyclomaticComplexity": 15,
        "cognitiveComplexity": 22,
        "halstead": {
          "difficulty": 45.2,
          "volume": 5823,
          "effort": 263200,
          "bugs": 1.94
        },
        "nestingLevel": 4,
        "parameterCount": 8
      }
    }
  ],
  "hotspots": [
    {
      "program": "GL100",
      "reason": "Cyclomatic complexity > 50",
      "recommendation": "Split into smaller procedures"
    }
  ]
}
```

#### 3.3 Maintainability Index
**File**: `maintainability-index.json`
```json
{
  "timestamp": "2024-01-15T12:00:00Z",
  "systemMaintainabilityIndex": 65.4,
  "interpretation": "Moderate - Some refactoring needed",
  "byModule": {
    "GL": 72.1,
    "AR": 68.5,
    "AP": 66.3,
    "Stock": 61.2,
    "IRS": 58.9
  },
  "factors": {
    "codeComplexity": 0.3,
    "codeVolume": 0.2,
    "commentRatio": 0.15,
    "duplication": 0.25,
    "testCoverage": 0.1
  },
  "recommendations": [
    "Increase documentation in IRS module",
    "Reduce duplication in Stock module",
    "Simplify complex procedures in GL"
  ]
}
```

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

### 5. Analysis Scripts
`documentation/parsed/parser_analysis/`

#### 5.1 Main Analysis Orchestrator
**File**: `run-analysis-suite.js`
```javascript
#!/usr/bin/env node

const fs = require('fs');
const path = require('path');
const { generateVisualizations } = require('./generate-visualizations');
const { generateDocumentation } = require('./generate-documentation');
const { generateMetrics } = require('./generate-metrics');
const { generateDatabase } = require('./generate-database');

async function runAnalysisSuite() {
    console.log('Starting COBOL Analysis Suite...');
    
    // Load parsed structures
    const structures = loadParsedStructures();
    console.log(`Loaded ${structures.length} parsed structures`);
    
    // Run analysis phases
    await generateMetrics(structures);
    await generateVisualizations(structures);
    await generateDocumentation(structures);
    await generateDatabase(structures);
    
    // Generate final report
    generateAnalysisReport();
    
    console.log('Analysis complete!');
}
```

#### 5.2 Package Configuration
**File**: `package.json`
```json
{
  "name": "cobol-analysis-suite",
  "version": "1.0.0",
  "scripts": {
    "analyze": "node run-analysis-suite.js",
    "visualize": "node generate-visualizations.js",
    "document": "node generate-documentation.js",
    "metrics": "node generate-metrics.js",
    "database": "node generate-database.js",
    "serve": "http-server dashboard -p 8080",
    "test": "jest"
  },
  "dependencies": {
    "d3": "^7.0.0",
    "chart.js": "^3.0.0",
    "sqlite3": "^5.0.0",
    "markdown-pdf": "^latest",
    "http-server": "^latest"
  }
}
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

Remember: This analysis suite is critical for understanding the current system state and planning the migration strategy. The insights generated here will drive architectural decisions and risk assessments throughout the modernization project.

Think ultra mega hard at each step.