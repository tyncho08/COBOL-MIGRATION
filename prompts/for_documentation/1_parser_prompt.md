// Execute Parser Creation
Task: Create a Node.js-based COBOL parser

Input Context:
- All files in Legacy_App/ directory
- Focus on: .cbl, .cpy, .CPY, .cob extensions
- Process all subdirectories

Required Outputs in documentation/parsed/:
1. parsed-structures/
   - Individual JSON files for each COBOL source
   - Naming: [path]_[filename].json
   
2. parser-summary.json
   - Statistics and metrics
   - Error log
   - File inventory

Key Extraction Requirements:
- Program identification and structure
- CALL statements and dependencies
- COPY statements for copybook usage
- PERFORM statements for flow analysis
- File descriptions (SELECT, FD)
- Data definitions and working storage
- Procedure division sections

Implementation Files to Generate:
- parse-cobol-simple.js
- analyze-structures.js
- test-parser.js
- package.json