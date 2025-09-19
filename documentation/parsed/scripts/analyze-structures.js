#!/usr/bin/env node

const fs = require('fs').promises;
const path = require('path');
const { glob } = require('glob');
const chalk = require('chalk');
const ora = require('ora');

const PARSED_DIR = path.join(__dirname, '../parsed-structures');

// Analyze all parsed structures
async function analyzeStructures() {
  const spinner = ora('Analyzing parsed structures...').start();
  
  try {
    const files = await glob('*.json', { cwd: PARSED_DIR });
    const analysis = {
      programs: [],
      copybooks: [],
      dependencies: {
        calls: new Map(),
        copies: new Map()
      },
      metrics: {
        totalComplexity: 0,
        totalLines: 0,
        averageComplexity: 0,
        maxComplexity: 0,
        minComplexity: Infinity
      }
    };

    spinner.text = `Analyzing ${files.length} parsed files...`;

    for (const file of files) {
      const data = JSON.parse(await fs.readFile(path.join(PARSED_DIR, file), 'utf-8'));
      
      if (data.identification.programType === 'copybook') {
        analysis.copybooks.push({
          name: data.identification.programId,
          file: data.metadata.filepath,
          usedBy: []
        });
      } else {
        analysis.programs.push({
          name: data.identification.programId,
          file: data.metadata.filepath,
          type: data.identification.programType,
          complexity: data.metrics.cyclomaticComplexity,
          lines: data.metrics.linesOfCode
        });

        // Update metrics
        analysis.metrics.totalComplexity += data.metrics.cyclomaticComplexity;
        analysis.metrics.totalLines += data.metrics.linesOfCode;
        analysis.metrics.maxComplexity = Math.max(analysis.metrics.maxComplexity, data.metrics.cyclomaticComplexity);
        analysis.metrics.minComplexity = Math.min(analysis.metrics.minComplexity, data.metrics.cyclomaticComplexity);
      }

      // Track dependencies
      data.dependencies.callStatements.forEach(call => {
        if (!analysis.dependencies.calls.has(call.program)) {
          analysis.dependencies.calls.set(call.program, []);
        }
        analysis.dependencies.calls.get(call.program).push(data.identification.programId);
      });

      data.dependencies.copyStatements.forEach(copy => {
        if (!analysis.dependencies.copies.has(copy.copybook)) {
          analysis.dependencies.copies.set(copy.copybook, []);
        }
        analysis.dependencies.copies.get(copy.copybook).push(data.identification.programId);
      });
    }

    // Calculate averages
    if (analysis.programs.length > 0) {
      analysis.metrics.averageComplexity = analysis.metrics.totalComplexity / analysis.programs.length;
    }

    // Update copybook usage
    analysis.copybooks.forEach(cb => {
      cb.usedBy = analysis.dependencies.copies.get(cb.name) || [];
    });

    spinner.succeed('Structure analysis complete!');
    console.log(chalk.blue('\nAnalysis Summary:'));
    console.log(`- Programs analyzed: ${analysis.programs.length}`);
    console.log(`- Copybooks found: ${analysis.copybooks.length}`);
    console.log(`- Average complexity: ${analysis.metrics.averageComplexity.toFixed(2)}`);
    console.log(`- Total lines of code: ${analysis.metrics.totalLines.toLocaleString()}`);

    return analysis;
  } catch (error) {
    spinner.fail('Analysis failed');
    throw error;
  }
}

// Run analysis
if (require.main === module) {
  analyzeStructures().catch(console.error);
}

module.exports = { analyzeStructures };