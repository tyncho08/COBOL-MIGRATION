# Execute Analysis Suite Creation

Task: Build analysis and visualization suite for COBOL system

Input:
- Parsed JSON files in `documentation/parsed/parsed-structures/`
- Original COBOL source for reference

Outputs (in `documentation/parsed/parser_analysis/`, organized by subfolders):

1. `documentation/parsed/parser_analysis/visualizations/`
   - `documentation/parsed/parser_analysis/visualizations/call-graph.html` (interactive program dependency graph)
   - `documentation/parsed/parser_analysis/visualizations/procedure-flow.html` (procedure flowcharts)
   - `documentation/parsed/parser_analysis/visualizations/copybook-usage.html` (COPYBOOK dependency maps)

2. `documentation/parsed/parser_analysis/docs/`
   - `documentation/parsed/parser_analysis/docs/system-documentation.md`
   - `documentation/parsed/parser_analysis/docs/subsystem-documentation.md`
   - `documentation/parsed/parser_analysis/docs/program-index.md`
   - `documentation/parsed/parser_analysis/docs/copybook-index.md`
   - `documentation/parsed/parser_analysis/docs/dependency-analysis.md`

3. `documentation/parsed/parser_analysis/dashboard/`
   - `documentation/parsed/parser_analysis/dashboard/index.html` (interactive metrics dashboard)
   - `documentation/parsed/parser_analysis/dashboard/complexity-metrics.json` (Cyclomatic + Halstead)
   - `documentation/parsed/parser_analysis/dashboard/maintainability-index.json`
   - charts and graphs integrated

4. `documentation/parsed/parser_analysis/database/`
   - `documentation/parsed/parser_analysis/database/metrics.db` (SQLite with all metrics)
   - `documentation/parsed/parser_analysis/database/dependency-relationships.sql`

5. `documentation/parsed/parser_analysis/scripts/`
   - Individual scripts to generate each output
   - Scripts runnable standalone or via npm workflow

6. `documentation/parsed/parser_analysis/package.json`
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