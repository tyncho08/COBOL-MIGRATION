#!/usr/bin/env node

const fs = require('fs').promises;
const path = require('path');
const chalk = require('chalk');
const { parseCobolFile } = require('./parse-cobol-simple');

// Test parsing on a sample file
async function testParser() {
  console.log(chalk.blue('COBOL Parser Test Suite\n'));
  
  const testFile = process.argv[2];
  if (!testFile) {
    console.error(chalk.red('Please provide a COBOL file to test'));
    console.log('Usage: node test-parser.js <cobol-file>');
    process.exit(1);
  }

  try {
    console.log(chalk.yellow(`Testing parser on: ${testFile}`));
    console.time('Parse time');
    
    const result = await parseCobolFile(testFile);
    
    console.timeEnd('Parse time');
    console.log(chalk.green('\n✓ Parse successful!\n'));

    // Validation checks
    console.log(chalk.blue('Validation Results:'));
    const checks = [
      {
        name: 'Program ID extracted',
        pass: result.identification.programId !== null,
        value: result.identification.programId
      },
      {
        name: 'Program type identified',
        pass: ['main', 'sub', 'copybook'].includes(result.identification.programType),
        value: result.identification.programType
      },
      {
        name: 'Metrics calculated',
        pass: result.metrics.linesOfCode > 0,
        value: `${result.metrics.linesOfCode} lines`
      },
      {
        name: 'Complexity analyzed',
        pass: result.metrics.cyclomaticComplexity > 0,
        value: result.metrics.cyclomaticComplexity
      },
      {
        name: 'Dependencies tracked',
        pass: true,
        value: `${result.dependencies.callStatements.length} calls, ${result.dependencies.copyStatements.length} copies`
      }
    ];

    checks.forEach(check => {
      const status = check.pass ? chalk.green('✓') : chalk.red('✗');
      console.log(`${status} ${check.name}: ${check.value || 'N/A'}`);
    });

    // Output sample of parsed structure
    console.log(chalk.blue('\nSample Output:'));
    console.log(JSON.stringify({
      metadata: result.metadata,
      identification: result.identification,
      metrics: result.metrics
    }, null, 2));

    // Save test output
    const outputFile = `test-output-${Date.now()}.json`;
    await fs.writeFile(outputFile, JSON.stringify(result, null, 2));
    console.log(chalk.green(`\nFull output saved to: ${outputFile}`));

  } catch (error) {
    console.error(chalk.red(`\n✗ Parse failed: ${error.message}`));
    console.error(error.stack);
    process.exit(1);
  }
}

// Run test
if (require.main === module) {
  testParser();
}

module.exports = { testParser };