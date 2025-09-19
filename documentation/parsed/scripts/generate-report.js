#!/usr/bin/env node

const fs = require('fs').promises;
const path = require('path');
const { glob } = require('glob');
const chalk = require('chalk');
const ora = require('ora');

const PARSED_DIR = path.join(__dirname, '../parsed-structures');

// Generate comprehensive analysis report
async function generateReport() {
  const spinner = ora('Generating comprehensive report...').start();
  
  try {
    const files = await glob('*.json', { cwd: PARSED_DIR });
    const analysis = {
      overview: {
        totalPrograms: 0,
        mainPrograms: 0,
        subPrograms: 0,
        copybooks: 0,
        totalLOC: 0,
        totalComplexity: 0
      },
      byDirectory: {},
      complexity: {
        distribution: {},
        top20: []
      },
      dependencies: {
        mostCalled: {},
        mostUsedCopybooks: {},
        orphaned: []
      },
      quality: {
        withGoTo: [],
        withoutErrorHandling: [],
        highComplexity: [],
        possibleDeadCode: []
      },
      sql: {
        programsWithSQL: [],
        tables: new Set()
      }
    };

    spinner.text = `Analyzing ${files.length} files...`;

    // Analyze each file
    const allPrograms = [];
    for (const file of files) {
      const data = JSON.parse(await fs.readFile(path.join(PARSED_DIR, file), 'utf-8'));
      allPrograms.push({ file, data });
      
      // Update overview
      analysis.overview.totalPrograms++;
      analysis.overview.totalLOC += data.metrics.linesOfCode;
      analysis.overview.totalComplexity += data.metrics.cyclomaticComplexity;
      
      if (data.identification.programType === 'copybook') {
        analysis.overview.copybooks++;
      } else if (data.identification.programType === 'main') {
        analysis.overview.mainPrograms++;
      } else {
        analysis.overview.subPrograms++;
      }

      // By directory
      const dir = data.metadata.filepath.split('/')[0];
      if (!analysis.byDirectory[dir]) {
        analysis.byDirectory[dir] = { programs: 0, loc: 0, complexity: 0 };
      }
      analysis.byDirectory[dir].programs++;
      analysis.byDirectory[dir].loc += data.metrics.linesOfCode;
      analysis.byDirectory[dir].complexity += data.metrics.cyclomaticComplexity;

      // Quality issues
      if (data.qualityIndicators.usesGoTo) {
        analysis.quality.withGoTo.push(data.identification.programId);
      }
      if (!data.qualityIndicators.hasErrorHandling) {
        analysis.quality.withoutErrorHandling.push(data.identification.programId);
      }
      if (data.metrics.cyclomaticComplexity > 50) {
        analysis.quality.highComplexity.push({
          program: data.identification.programId,
          complexity: data.metrics.cyclomaticComplexity
        });
      }
      if (data.qualityIndicators.hasDeadCode) {
        analysis.quality.possibleDeadCode.push(data.identification.programId);
      }

      // SQL usage
      if (data.dependencies.sqlStatements.length > 0) {
        analysis.sql.programsWithSQL.push(data.identification.programId);
        data.dependencies.sqlStatements.forEach(stmt => {
          stmt.tables.forEach(table => analysis.sql.tables.add(table));
        });
      }
    }

    // Calculate complexity distribution
    allPrograms.forEach(({ data }) => {
      const complexity = data.metrics.cyclomaticComplexity;
      const bucket = Math.floor(complexity / 10) * 10;
      const key = `${bucket}-${bucket + 9}`;
      analysis.complexity.distribution[key] = (analysis.complexity.distribution[key] || 0) + 1;
    });

    // Find top 20 most complex programs
    analysis.complexity.top20 = allPrograms
      .filter(p => p.data.identification.programType !== 'copybook')
      .sort((a, b) => b.data.metrics.cyclomaticComplexity - a.data.metrics.cyclomaticComplexity)
      .slice(0, 20)
      .map(p => ({
        program: p.data.identification.programId,
        file: p.data.metadata.filepath,
        complexity: p.data.metrics.cyclomaticComplexity,
        loc: p.data.metrics.linesOfCode
      }));

    // Analyze dependencies
    const callCounts = {};
    const copyCounts = {};
    allPrograms.forEach(({ data }) => {
      data.dependencies.callStatements.forEach(call => {
        callCounts[call.program] = (callCounts[call.program] || 0) + 1;
      });
      data.dependencies.copyStatements.forEach(copy => {
        copyCounts[copy.copybook] = (copyCounts[copy.copybook] || 0) + 1;
      });
    });

    analysis.dependencies.mostCalled = Object.entries(callCounts)
      .sort((a, b) => b[1] - a[1])
      .slice(0, 20)
      .map(([prog, count]) => ({ program: prog, calls: count }));

    analysis.dependencies.mostUsedCopybooks = Object.entries(copyCounts)
      .sort((a, b) => b[1] - a[1])
      .slice(0, 20)
      .map(([copy, count]) => ({ copybook: copy, uses: count }));

    // Generate report
    spinner.text = 'Generating markdown report...';
    const report = generateMarkdownReport(analysis);
    
    await fs.writeFile(path.join(__dirname, '../analysis-report.md'), report);
    spinner.succeed('Report generated successfully!');
    
    console.log(chalk.blue('\nReport Summary:'));
    console.log(`- Total programs: ${analysis.overview.totalPrograms}`);
    console.log(`- Total lines of code: ${analysis.overview.totalLOC.toLocaleString()}`);
    console.log(`- Average complexity: ${(analysis.overview.totalComplexity / analysis.overview.totalPrograms).toFixed(2)}`);
    console.log(`- Programs with SQL: ${analysis.sql.programsWithSQL.length}`);
    console.log(`- Quality issues found: ${analysis.quality.withGoTo.length + analysis.quality.withoutErrorHandling.length + analysis.quality.highComplexity.length}`);
    console.log(chalk.green('\nFull report saved to: analysis-report.md'));

  } catch (error) {
    spinner.fail('Report generation failed');
    throw error;
  }
}

// Generate markdown report
function generateMarkdownReport(analysis) {
  return `# ACAS COBOL System Analysis Report

Generated: ${new Date().toISOString()}

## Executive Summary

The ACAS (Applewood Computers Accounting System) codebase consists of **${analysis.overview.totalPrograms} programs** totaling **${analysis.overview.totalLOC.toLocaleString()} lines of code**. The system shows an average cyclomatic complexity of **${(analysis.overview.totalComplexity / analysis.overview.totalPrograms).toFixed(2)}**, indicating ${analysis.overview.totalComplexity / analysis.overview.totalPrograms < 20 ? 'manageable' : 'significant'} complexity.

### Key Findings
- **${analysis.overview.mainPrograms}** main programs (entry points)
- **${analysis.overview.subPrograms}** subprograms (called routines)
- **${analysis.overview.copybooks}** copybooks (shared data structures)
- **${analysis.sql.programsWithSQL.length}** programs use embedded SQL
- **${analysis.quality.highComplexity.length}** programs have high complexity (>50)
- **${analysis.quality.withGoTo.length}** programs use GO TO statements

## System Overview

### Program Distribution by Type
| Type | Count | Percentage |
|------|-------|------------|
| Main Programs | ${analysis.overview.mainPrograms} | ${((analysis.overview.mainPrograms / analysis.overview.totalPrograms) * 100).toFixed(1)}% |
| Subprograms | ${analysis.overview.subPrograms} | ${((analysis.overview.subPrograms / analysis.overview.totalPrograms) * 100).toFixed(1)}% |
| Copybooks | ${analysis.overview.copybooks} | ${((analysis.overview.copybooks / analysis.overview.totalPrograms) * 100).toFixed(1)}% |

### Code Distribution by Module
| Module | Programs | Lines of Code | Avg Complexity |
|--------|----------|---------------|----------------|
${Object.entries(analysis.byDirectory)
  .sort((a, b) => b[1].loc - a[1].loc)
  .map(([dir, stats]) => 
    `| ${dir} | ${stats.programs} | ${stats.loc.toLocaleString()} | ${(stats.complexity / stats.programs).toFixed(2)} |`
  ).join('\n')}

## Complexity Analysis

### Complexity Distribution
| Range | Count | Percentage |
|-------|-------|------------|
${Object.entries(analysis.complexity.distribution)
  .sort((a, b) => parseInt(a[0]) - parseInt(b[0]))
  .map(([range, count]) => 
    `| ${range} | ${count} | ${((count / analysis.overview.totalPrograms) * 100).toFixed(1)}% |`
  ).join('\n')}

### Top 20 Most Complex Programs
| Program | File | Complexity | Lines |
|---------|------|------------|-------|
${analysis.complexity.top20
  .map(p => `| ${p.program} | ${p.file} | ${p.complexity} | ${p.loc} |`)
  .join('\n')}

## Dependency Analysis

### Most Called Programs
| Program | Times Called |
|---------|--------------|
${analysis.dependencies.mostCalled
  .map(p => `| ${p.program} | ${p.calls} |`)
  .join('\n')}

### Most Used Copybooks
| Copybook | Times Used |
|----------|------------|
${analysis.dependencies.mostUsedCopybooks
  .map(c => `| ${c.copybook} | ${c.uses} |`)
  .join('\n')}

## Quality Analysis

### Programs with GO TO Statements (${analysis.quality.withGoTo.length})
${analysis.quality.withGoTo.length > 0 ? analysis.quality.withGoTo.slice(0, 20).join(', ') + (analysis.quality.withGoTo.length > 20 ? '...' : '') : 'None found'}

### Programs without Error Handling (${analysis.quality.withoutErrorHandling.length})
${analysis.quality.withoutErrorHandling.length > 0 ? analysis.quality.withoutErrorHandling.slice(0, 20).join(', ') + (analysis.quality.withoutErrorHandling.length > 20 ? '...' : '') : 'None found'}

### High Complexity Programs (>50) (${analysis.quality.highComplexity.length})
${analysis.quality.highComplexity
  .map(p => `- ${p.program} (complexity: ${p.complexity})`)
  .join('\n')}

### Programs with Possible Dead Code (${analysis.quality.possibleDeadCode.length})
${analysis.quality.possibleDeadCode.length > 0 ? analysis.quality.possibleDeadCode.slice(0, 20).join(', ') + (analysis.quality.possibleDeadCode.length > 20 ? '...' : '') : 'None found'}

## Database Integration

### Programs Using SQL (${analysis.sql.programsWithSQL.length})
${analysis.sql.programsWithSQL.length > 0 ? analysis.sql.programsWithSQL.slice(0, 30).join(', ') + (analysis.sql.programsWithSQL.length > 30 ? '...' : '') : 'None found'}

### Database Tables Referenced (${analysis.sql.tables.size})
${analysis.sql.tables.size > 0 ? [...analysis.sql.tables].sort().join(', ') : 'None found'}

## Migration Readiness Assessment

### Strengths
- Well-structured modular design with clear separation between modules
- Consistent use of copybooks for shared data structures
- ${100 - ((analysis.quality.withGoTo.length / analysis.overview.totalPrograms) * 100).toFixed(1)}% of programs avoid GO TO statements

### Challenges
- ${analysis.quality.highComplexity.length} programs require refactoring due to high complexity
- ${analysis.quality.withoutErrorHandling.length} programs lack proper error handling
- Mixed file-based and SQL-based data access patterns

### Recommendations
1. **Priority Refactoring**: Focus on the ${analysis.complexity.top20.length} most complex programs
2. **Error Handling**: Add comprehensive error handling to ${analysis.quality.withoutErrorHandling.length} programs
3. **GO TO Elimination**: Refactor ${analysis.quality.withGoTo.length} programs to use structured programming
4. **Data Access Layer**: Standardize data access patterns across the system
5. **Incremental Migration**: Start with low-complexity, well-isolated modules

## Next Steps

1. Generate detailed functional documentation for business logic understanding
2. Create subsystem boundaries based on dependency analysis
3. Develop migration strategy for each module
4. Establish testing framework for validation
5. Plan phased migration approach

---
*This report provides a comprehensive technical analysis of the ACAS COBOL system. For business-level documentation, refer to the functional analysis documents.*`;
}

// Run report generation
if (require.main === module) {
  generateReport().catch(console.error);
}

module.exports = { generateReport };