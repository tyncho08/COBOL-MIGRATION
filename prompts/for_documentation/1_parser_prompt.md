# Execute Parser Creation

Task: Build a Node.js-based COBOL parser

Input:
- Directory: `Legacy_App/`
- File extensions: `.cbl`, `.cpy`, `.CPY`, `.cob`
- Process all subdirectories
- Handle COBOL programs + copybooks

Outputs (in `documentation/parsed/`):

1. `documentation/parsed/parsed-structures/`
   - One JSON per COBOL source
   - Naming: [path]_[filename].json (e.g., `sales_sl000.cbl.json`)
   - Include parse timestamps + file metadata

2. `documentation/parsed/parser-summary.json`
   - File inventory
   - Statistics + metrics
   - Error log (detailed, per file)
   - Progress feedback

Extraction Requirements:
- Program identification + structure
- Divisions: IDENTIFICATION, ENVIRONMENT, DATA, PROCEDURE
- Sections + paragraphs
- CALL statements + dependencies
- COPY statements (copybook usage)
- PERFORM statements (flow analysis)
- File descriptions (SELECT, FD)
- Data definitions + working storage

Error Handling:
- Continue on failure
- Log errors in `documentation/parsed/parser-summary.json`

Performance:
- Efficient pattern matching (not full AST)
- Sequential processing
- Progress feedback

Files to Generate:
- `documentation/parsed/parse-cobol-simple.js`
- `documentation/parsed/analyze-structures.js`
- `documentation/parsed/test-parser.js`
- `documentation/parsed/package.json` (with npm scripts)

Notes:
- Use cobol-parsers npm package or similar
- JSON output must be clean + structured
- Support multiple COBOL dialects

Think ultra mega hard at each step.