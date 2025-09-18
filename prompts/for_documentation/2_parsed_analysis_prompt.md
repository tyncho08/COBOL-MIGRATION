# Execute Analysis Suite Creation

Task: Build analysis and visualization suite for COBOL system

Input:
- Parsed JSON files in parsed-structures/
- Original COBOL source for reference

Outputs (in documentation/parsed/, organized by subfolders):

1. visualizations/
   - call-graph.html (interactive program dependency graph)
   - procedure-flow.html (procedure flowcharts)
   - copybook-usage.html (COPYBOOK dependency maps)

2. docs/
   - system-documentation.md
   - subsystem-documentation.md
   - program-index.md
   - copybook-index.md
   - dependency-analysis.md

3. dashboard/
   - index.html (interactive metrics dashboard)
   - complexity-metrics.json (Cyclomatic + Halstead)
   - maintainability-index.json
   - charts and graphs integrated

4. database/
   - metrics.db (SQLite with all metrics)
   - dependency-relationships.sql

5. scripts/
   - Individual scripts to generate each output
   - Scripts runnable standalone or via npm workflow

6. package.json
   - Dependencies + npm scripts for suite orchestration

Metrics to Calculate:
- Cyclomatic complexity
- Halstead metrics
- Maintainability index
- Code duplication
- Dead code detection

Execution:
- Process all parsed JSON files in parsed-structures/
- Write outputs following folder structure
- Each tool/script must run standalone or via unified npm workflow

Think ultra mega hard at each step.