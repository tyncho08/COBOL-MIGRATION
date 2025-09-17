const fs = require('fs');
const path = require('path');
const { execSync } = require('child_process');

// Import all generators
const VisualizationGenerator = require('./generate-visualizations');
const DocumentationGenerator = require('./generate-documentation');
const MetricsGenerator = require('./generate-metrics');
const DatabaseGenerator = require('./generate-database');

console.log('=== COBOL Analysis Suite ===');
console.log('Starting comprehensive analysis...\n');

const parsedDir = path.resolve(__dirname);

// Ensure output directories exist
const dirs = [
    'parser_analysis',
    'parser_analysis/visualizations',
    'parser_analysis/docs',
    'parser_analysis/dashboard',
    'parser_analysis/database'
];

dirs.forEach(dir => {
    const fullPath = path.join(parsedDir, dir);
    if (!fs.existsSync(fullPath)) {
        fs.mkdirSync(fullPath, { recursive: true });
    }
});

// Run analyzers in sequence
try {
    console.log('\n[Step 1] Generating Visualizations...');
    const vizGen = new VisualizationGenerator(parsedDir);
    vizGen.generateAll();
    
    console.log('\n[Step 2] Generating Documentation...');
    const docGen = new DocumentationGenerator(parsedDir);
    docGen.generateAll();
    
    console.log('\n[Step 3] Generating Metrics...');
    const metricsGen = new MetricsGenerator(parsedDir);
    metricsGen.generateAll();
    
    console.log('\n[Step 4] Generating Database...');
    const dbGen = new DatabaseGenerator(parsedDir);
    dbGen.generateAll();
    
    console.log('\n=== Analysis Complete! ===\n');
    console.log('Output locations:');
    console.log('- Visualizations: parser_analysis/visualizations/');
    console.log('  - call-graph.html - Interactive program dependency graph');
    console.log('  - procedure-flow.html - Internal procedure flow diagrams');
    console.log('  - copybook-usage.html - Copybook dependency visualization');
    console.log('');
    console.log('- Documentation: parser_analysis/docs/');
    console.log('  - system-documentation.md - Complete system overview');
    console.log('  - subsystem-documentation.md - Subsystem analysis');
    console.log('  - program-index.md - All programs reference');
    console.log('  - copybook-index.md - All copybooks reference');
    console.log('  - dependency-analysis.md - Dependency relationships');
    console.log('');
    console.log('- Metrics Dashboard: parser_analysis/dashboard/');
    console.log('  - index.html - Interactive metrics dashboard');
    console.log('  - complexity-metrics.json - Complexity data');
    console.log('  - maintainability-index.json - Maintainability data');
    console.log('');
    console.log('- Database: parser_analysis/database/');
    console.log('  - cobol-metrics-schema.sql - Database schema');
    console.log('  - create-database.sh - SQLite initialization script');
    console.log('  - dependency-analysis-queries.sql - Analysis queries');
    
    // Generate summary report
    generateSummaryReport();
    
} catch (error) {
    console.error('Error during analysis:', error.message);
    process.exit(1);
}

function generateSummaryReport() {
    const summaryPath = path.join(parsedDir, 'parser_analysis', 'ANALYSIS_SUMMARY.md');
    
    const summary = `# COBOL Analysis Suite - Summary Report
Generated: ${new Date().toISOString()}

## Overview
This report summarizes the comprehensive analysis performed on the COBOL codebase.

## Key Findings

### System Statistics
- Review the metrics dashboard at \`dashboard/index.html\` for interactive visualizations
- See \`docs/system-documentation.md\` for detailed system architecture

### Visualizations Available
1. **Call Graph** (\`visualizations/call-graph.html\`)
   - Interactive network diagram showing program dependencies
   - Color-coded by program type
   - Click nodes for detailed information

2. **Procedure Flow** (\`visualizations/procedure-flow.html\`)
   - Individual program flow diagrams
   - Shows internal structure and control flow
   - Useful for understanding program logic

3. **Copybook Usage** (\`visualizations/copybook-usage.html\`)
   - Visual representation of copybook dependencies
   - Identifies most-used and unused copybooks
   - Helps in consolidation efforts

### Documentation Generated
- **System Documentation**: High-level architecture overview
- **Subsystem Documentation**: Detailed breakdown by functional area
- **Program Index**: Searchable reference of all programs
- **Copybook Index**: Complete copybook inventory
- **Dependency Analysis**: Detailed dependency relationships

### Metrics Highlights
- Programs analyzed: See dashboard for counts
- Complexity distribution available in dashboard
- Maintainability scores calculated for all programs
- Dead code and duplication analysis completed

### Database
A SQLite database schema has been generated with:
- Complete program inventory
- Dependency relationships
- Metrics data
- Pre-written analysis queries

To create the database:
\`\`\`bash
cd parser_analysis/database
./create-database.sh
\`\`\`

## Next Steps
1. Review high-complexity programs in the dashboard
2. Examine circular dependencies in the dependency analysis
3. Plan refactoring based on maintainability scores
4. Use the database for custom queries and analysis

## Migration Recommendations
See \`docs/system-documentation.md\` for detailed migration recommendations based on the analysis.
`;

    fs.writeFileSync(summaryPath, summary);
    console.log('\nSummary report generated: parser_analysis/ANALYSIS_SUMMARY.md');
}