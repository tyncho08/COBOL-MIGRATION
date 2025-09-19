# COBOL Parser Creation - Comprehensive Structure Extraction

## Context
You are creating a parser for ACAS (Applewood Computers Accounting System), a COBOL-based accounting system. The parser must extract all structural and business-critical information to enable complete system understanding and migration.

## Primary Objective
Build a robust Node.js-based COBOL parser that extracts comprehensive program structure, business logic patterns, and dependencies to create a complete digital representation of the COBOL system.

## Input Specifications

### Source Directory
- Root: `Legacy_App/`
- Subdirectories: `common/`, `sales/`, `purchase/`, `stock/`, `general/`, `irs/`, `copybooks/`
- File extensions: `.cbl`, `.cpy`, `.CPY`, `.cob`
- Expected volume: 250+ programs, 150+ copybooks

### File Types to Process
- COBOL Programs (main and subprograms)
- Copybooks (shared data structures and procedures)
- Include files
- Any embedded SQL or script files referenced
- Unsupported file types should be ignored

## Output Specifications

### 1. Parsed Structures Directory
`documentation/parsed/parsed-structures/`

#### Individual JSON Files
- One JSON file per COBOL source
- Naming convention: `[subdirectory]_[filename].json`
  - Example: `sales_sl000_cbl.json`, `copybooks_wsstock_cob.json`
- Preserve full path information for traceability

#### JSON Structure per File
```json
{
  "metadata": {
    "filename": "sl000.cbl",
    "filepath": "sales/sl000.cbl",
    "parseTimestamp": "2024-01-15T10:30:00Z",
    "fileSize": 45678,
    "lastModified": "2023-12-01T08:00:00Z",
    "parseVersion": "1.0",
    "dialect": "GnuCOBOL"
  },
  "identification": {
    "programId": "SL000",
    "programType": "main|sub|copybook",
    "author": "extracted-author",
    "dateWritten": "extracted-date",
    "dateCompiled": "extracted-date",
    "remarks": ["extracted-remarks"]
  },
  "environment": {
    "sourceComputer": "extracted-value",
    "objectComputer": "extracted-value",
    "fileControl": [
      {
        "fileName": "SALES-FILE",
        "assignTo": "external-name",
        "organization": "INDEXED|SEQUENTIAL|RELATIVE",
        "accessMode": "SEQUENTIAL|RANDOM|DYNAMIC",
        "recordKey": "field-name",
        "alternateKeys": [{"field": "name", "duplicates": true}],
        "fileStatus": "status-field"
      }
    ]
  },
  "data": {
    "fileSection": [
      {
        "fdName": "SALES-FILE",
        "recordName": "SALES-RECORD",
        "fields": [
          {
            "level": "01",
            "name": "SALES-RECORD",
            "picture": null,
            "usage": null,
            "value": null,
            "occurs": null,
            "redefines": null,
            "children": []
          }
        ]
      }
    ],
    "workingStorage": {
      "fields": [],
      "constants": [],
      "tables": [],
      "flags": [],
      "counters": [],
      "accumulators": []
    },
    "localStorage": {
      "fields": []
    },
    "linkageSection": {
      "parameters": []
    }
  },
  "procedure": {
    "divisions": [],
    "sections": [
      {
        "name": "SECTION-NAME",
        "paragraphs": [
          {
            "name": "PARAGRAPH-NAME",
            "statements": [],
            "performedBy": [],
            "performs": []
          }
        ]
      }
    ],
    "paragraphs": []
  },
  "dependencies": {
    "copyStatements": [
      {
        "copybook": "WSSTOCK",
        "line": 150,
        "inDivision": "DATA",
        "inSection": "WORKING-STORAGE"
      }
    ],
    "callStatements": [
      {
        "program": "SL010",
        "type": "STATIC|DYNAMIC",
        "line": 450,
        "using": ["PARAM1", "PARAM2"],
        "context": "UPDATE-INVENTORY"
      }
    ],
    "sqlStatements": [
      {
        "type": "SELECT|INSERT|UPDATE|DELETE",
        "tables": ["table-names"],
        "line": 300,
        "embedded": true
      }
    ]
  },
  "controlFlow": {
    "performStatements": [
      {
        "type": "PERFORM|PERFORM UNTIL|PERFORM VARYING",
        "target": "PARAGRAPH-NAME",
        "thru": "END-PARAGRAPH",
        "condition": "extracted-condition",
        "line": 200
      }
    ],
    "goToStatements": [
      {
        "target": "PARAGRAPH-NAME",
        "line": 350,
        "conditional": false
      }
    ],
    "conditionals": [
      {
        "type": "IF|EVALUATE",
        "condition": "extracted-condition",
        "line": 400,
        "complexity": 3
      }
    ]
  },
  "businessLogic": {
    "calculations": [
      {
        "target": "TOTAL-AMOUNT",
        "formula": "extracted-formula",
        "line": 500,
        "rounding": "ROUNDED|TRUNCATED"
      }
    ],
    "validations": [
      {
        "field": "CUSTOMER-CODE",
        "condition": "extracted-validation",
        "line": 250,
        "errorHandling": "extracted-error-action"
      }
    ],
    "fileOperations": [
      {
        "operation": "READ|WRITE|REWRITE|DELETE|START",
        "file": "FILE-NAME",
        "line": 600,
        "errorHandling": "AT END|INVALID KEY handling"
      }
    ]
  },
  "metrics": {
    "linesOfCode": 1500,
    "linesOfComment": 200,
    "cyclomaticComplexity": 15,
    "numberOfSections": 5,
    "numberOfParagraphs": 25,
    "numberOfCalls": 10,
    "numberOfFiles": 3,
    "numberOfSQLStatements": 5,
    "deepestNesting": 4
  },
  "qualityIndicators": {
    "hasErrorHandling": true,
    "hasComments": true,
    "usesGoTo": false,
    "hasDeadCode": false,
    "followsNamingConventions": true
  }
}
```

### 2. Parser Summary
`documentation/parsed/parser-summary.md`

**Parser Metadata**
- Information about the analysis execution:
- Statistics: Overall analysis results, By file type, By directory
- Inventory
- Errors (Detected issues)

**Example Output:**
```markdown
# COBOL Parser Summary

## Execution Statistics
- Total files processed: 453
- Successfully parsed: 443 (97.8%)
- Failed parsing: 10 (2.2%)
- Total lines of code: 523,456
- Parse duration: 4m 32s

## Inventory by Type
| Type | Count | Success Rate |
|------|-------|--------------|
| Programs (.cbl) | 278 | 98.2% |
| Copybooks (.cpy) | 175 | 97.1% |

## Failed Files
| File | Error | Severity |
|------|-------|----------|
| legacy/old001.cbl | Unsupported COBOL-74 syntax | High |
```

## Extraction Requirements

### Core Structural Elements
1. **Program Identification**
   - PROGRAM-ID extraction with validation
   - Program type classification (main/sub/copybook)
   - Author and date information preservation

2. **Division Analysis**
   - Complete IDENTIFICATION DIVISION parsing
   - ENVIRONMENT DIVISION with all file mappings
   - DATA DIVISION with full field hierarchy
   - PROCEDURE DIVISION with flow analysis

3. **Data Structures**
   - Complete field definitions with all attributes
   - Level number hierarchy preservation  
   - PICTURE clause parsing and validation
   - REDEFINES and OCCURS handling
   - VALUE clause extraction
   - 88-level condition names with values

4. **Control Flow**
   - All PERFORM variations and their targets
   - GO TO statements and their impact
   - Conditional logic (IF/EVALUATE) with nesting
   - Loop constructs and exit conditions
   - CALL chain analysis

5. **Business Logic Extraction**
   - Arithmetic operations and formulas
   - String manipulation operations
   - Date/time handling patterns
   - Validation rules and conditions
   - Error handling strategies

### COBOL-Specific Patterns
1. **File Handling**
   - Sequential, indexed, and relative file access
   - Sort/merge operations
   - File status checking patterns

2. **Data Manipulation**
   - MOVE operations with type considerations
   - STRING/UNSTRING operations
   - INSPECT and transformations
   - Table handling with subscripts/indices

3. **Legacy Patterns**
   - ALTER statements (if any)
   - GO TO DEPENDING patterns
   - Fall-through logic
   - Dead code identification

## Implementation Requirements

### Technical Stack
```javascript
// package.json structure
{
  "name": "acas-cobol-parser",
  "version": "1.0.0",
  "scripts": {
    "parse": "node parse-cobol-simple.js",
    "analyze": "node analyze-structures.js",
    "test": "node test-parser.js",
    "parse:single": "node parse-cobol-simple.js --file",
    "validate": "node validate-output.js",
    "report": "node generate-report.js"
  },
  "dependencies": {
    "cobol-parser": "^latest",
    "commander": "^latest",
    "chalk": "^latest",
    "ora": "^latest"
  }
}
```

### Core Scripts
`documentation/parsed/scripts/`: All the scripts needed for COBOL parsing, including node_modules folder

1. **parse-cobol-simple.js**
   - Main parsing engine
   - Pattern-based extraction
   - Progress reporting
   - Error recovery

2. **analyze-structures.js**
   - Dependency analysis
   - Complexity calculations
   - Cross-reference generation
   - Quality metrics

3. **test-parser.js**
   - Single file parsing test
   - Validation of output structure
   - Performance benchmarking

4. **validate-output.js**
   - JSON schema validation
   - Completeness checking
   - Consistency verification

5. **generate-report.js**
   - Summary report generation
   - Visualization preparation
   - Migration readiness assessment
   - Results must be in markdown format

## Error Handling Strategy

1. **Graceful Degradation**
   - Continue parsing on non-fatal errors
   - Mark partially parsed sections
   - Log all issues with context

2. **Recovery Mechanisms**
   - Section-level recovery
   - Statement-level recovery
   - Default value assignment

3. **Error Classification**
   - Syntax errors
   - Unsupported constructs
   - Missing dependencies
   - Encoding issues

## Performance Optimization

1. **Efficient Parsing**
   - Stream-based file reading
   - Incremental parsing
   - Memory-conscious data structures

2. **Parallel Processing**
   - Worker threads for large codebases
   - Batch processing capabilities
   - Progress monitoring

3. **Caching Strategy**
   - Parsed copybook caching
   - Incremental updates
   - Dependency graph caching

## Quality Assurance

1. **Validation Rules**
   - All programs must have PROGRAM-ID
   - File definitions must match usage
   - Called programs must exist
   - Data types must be consistent

2. **Completeness Checks**
   - All divisions parsed
   - All statements captured
   - All dependencies tracked
   - All metrics calculated

3. **Output Verification**
   - JSON schema compliance
   - Cross-reference integrity
   - Metric accuracy
   - Relationship consistency

## Success Criteria

1. **Coverage**
   - 100% of COBOL files attempted
   - 95%+ successful parsing rate
   - All critical business logic captured

2. **Accuracy**
   - Correct program structure
   - Accurate dependency mapping
   - Valid complexity metrics

3. **Usability**
   - Clear progress indication
   - Actionable error messages
   - Comprehensive final report

Remember: This parser is the foundation for all subsequent analysis and migration efforts. Every piece of business logic, every data structure, and every dependency must be captured accurately. The quality of the migration depends on the completeness of this parsing phase.

## Quality Checklist:
- [ ] 100% of files attempted (check parser-summary.md)
- [ ] <5% parsing failure rate
- [ ] All PROGRAM-IDs extracted correctly
- [ ] All CALL dependencies mapped
- [ ] All COPY statements tracked
- [ ] JSON validates against schema
- [ ] No critical business logic missed