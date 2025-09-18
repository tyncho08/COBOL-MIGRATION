# Execute Analysis Suite Creation

Task: Build analysis and visualization suite for COBOL system

Input:
- Parsed JSON files in `documentation/parsed/parsed-structures/`
- Original COBOL source for reference

Outputs (in `documentation/parser/parsed_analysis/`, organized by subfolders):

1. `documentation/parser/parsed_analysis/visualizations/`
   - `documentation/parser/parsed_analysis/visualizations/call-graph.html` (interactive program dependency graph)
   - `documentation/parser/parsed_analysis/visualizations/procedure-flow.html` (procedure flowcharts)
   - `documentation/parser/parsed_analysis/visualizations/copybook-usage.html` (COPYBOOK dependency maps)

2. `documentation/parser/parsed_analysis/docs/`
   - `documentation/parser/parsed_analysis/docs/system-documentation.md`
   - `documentation/parser/parsed_analysis/docs/subsystem-documentation.md`
   - `documentation/parser/parsed_analysis/docs/program-index.md`
   - `documentation/parser/parsed_analysis/docs/copybook-index.md`
   - `documentation/parser/parsed_analysis/docs/dependency-analysis.md`

3. `documentation/parser/parsed_analysis/dashboard/`
   - `documentation/parser/parsed_analysis/dashboard/index.html` (interactive metrics dashboard)
   - `documentation/parser/parsed_analysis/dashboard/complexity-metrics.json` (Cyclomatic + Halstead)
   - `documentation/parser/parsed_analysis/dashboard/maintainability-index.json`
   - charts and graphs integrated

4. `documentation/parser/parsed_analysis/database/`
   - `documentation/parser/parsed_analysis/database/metrics.db` (SQLite with all metrics)
   - `documentation/parser/parsed_analysis/database/dependency-relationships.sql`

5. `documentation/parser/parsed_analysis/scripts/`
   - Individual scripts to generate each output
   - Scripts runnable standalone or via npm workflow

6. `documentation/parser/parsed_analysis/package.json`
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