#!/usr/bin/env node

const fs = require('fs').promises;
const path = require('path');
const { glob } = require('glob');
const chalk = require('chalk');
const ora = require('ora');

const PARSED_DIR = path.join(__dirname, '../parsed-structures');

// JSON schema for validation
const SCHEMA = {
  required: ['metadata', 'identification', 'environment', 'data', 'procedure', 'dependencies', 'controlFlow', 'businessLogic', 'metrics', 'qualityIndicators'],
  metadata: {
    required: ['filename', 'filepath', 'parseTimestamp', 'fileSize', 'lastModified', 'parseVersion', 'dialect']
  },
  identification: {
    required: ['programId', 'programType']
  },
  metrics: {
    required: ['linesOfCode', 'linesOfComment', 'cyclomaticComplexity']
  }
};

// Validate a single parsed file
function validateStructure(data, filename) {
  const errors = [];
  
  // Check top-level required fields
  SCHEMA.required.forEach(field => {
    if (!data[field]) {
      errors.push(`Missing required field: ${field}`);
    }
  });

  // Check nested required fields
  if (data.metadata) {
    SCHEMA.metadata.required.forEach(field => {
      if (!data.metadata[field]) {
        errors.push(`Missing metadata.${field}`);
      }
    });
  }

  if (data.identification) {
    SCHEMA.identification.required.forEach(field => {
      if (!data.identification[field]) {
        errors.push(`Missing identification.${field}`);
      }
    });
  }

  if (data.metrics) {
    SCHEMA.metrics.required.forEach(field => {
      if (data.metrics[field] === undefined) {
        errors.push(`Missing metrics.${field}`);
      }
    });
  }

  // Validate data types
  if (data.metrics) {
    if (typeof data.metrics.linesOfCode !== 'number') {
      errors.push('metrics.linesOfCode must be a number');
    }
    if (typeof data.metrics.cyclomaticComplexity !== 'number') {
      errors.push('metrics.cyclomaticComplexity must be a number');
    }
  }

  // Validate consistency
  if (data.dependencies) {
    if (!Array.isArray(data.dependencies.callStatements)) {
      errors.push('dependencies.callStatements must be an array');
    }
    if (!Array.isArray(data.dependencies.copyStatements)) {
      errors.push('dependencies.copyStatements must be an array');
    }
  }

  return errors;
}

// Validate all parsed files
async function validateAll() {
  const spinner = ora('Validating parsed structures...').start();
  const results = {
    valid: 0,
    invalid: 0,
    errors: []
  };

  try {
    const files = await glob('*.json', { cwd: PARSED_DIR });
    spinner.text = `Validating ${files.length} files...`;

    for (const file of files) {
      try {
        const data = JSON.parse(await fs.readFile(path.join(PARSED_DIR, file), 'utf-8'));
        const errors = validateStructure(data, file);
        
        if (errors.length === 0) {
          results.valid++;
        } else {
          results.invalid++;
          results.errors.push({ file, errors });
        }
      } catch (error) {
        results.invalid++;
        results.errors.push({
          file,
          errors: [`JSON parse error: ${error.message}`]
        });
      }
    }

    spinner.succeed('Validation complete!');
    
    console.log(chalk.blue('\nValidation Summary:'));
    console.log(chalk.green(`✓ Valid files: ${results.valid}`));
    console.log(chalk.red(`✗ Invalid files: ${results.invalid}`));
    
    if (results.errors.length > 0) {
      console.log(chalk.red('\nValidation Errors:'));
      results.errors.forEach(({ file, errors }) => {
        console.log(chalk.yellow(`\n${file}:`));
        errors.forEach(error => {
          console.log(`  - ${error}`);
        });
      });
    }

    // Generate validation report
    const report = `# Validation Report

## Summary
- Total files: ${files.length}
- Valid files: ${results.valid}
- Invalid files: ${results.invalid}
- Success rate: ${((results.valid / files.length) * 100).toFixed(1)}%

## Errors
${results.errors.length === 0 ? 'No validation errors found!' : 
  results.errors.map(({ file, errors }) => 
    `### ${file}\n${errors.map(e => `- ${e}`).join('\n')}`
  ).join('\n\n')}
`;

    await fs.writeFile(path.join(__dirname, '../validation-report.md'), report);
    console.log(chalk.green('\nValidation report saved to: validation-report.md'));

    return results;
  } catch (error) {
    spinner.fail('Validation failed');
    throw error;
  }
}

// Check cross-references
async function checkCrossReferences() {
  console.log(chalk.blue('\nChecking cross-references...'));
  
  const files = await glob('*.json', { cwd: PARSED_DIR });
  const programs = new Set();
  const copybooks = new Set();
  const missingCalls = new Set();
  const missingCopies = new Set();

  // First pass: collect all program and copybook names
  for (const file of files) {
    const data = JSON.parse(await fs.readFile(path.join(PARSED_DIR, file), 'utf-8'));
    if (data.identification.programType === 'copybook') {
      copybooks.add(data.identification.programId);
    } else {
      programs.add(data.identification.programId);
    }
  }

  // Second pass: check references
  for (const file of files) {
    const data = JSON.parse(await fs.readFile(path.join(PARSED_DIR, file), 'utf-8'));
    
    // Check CALL references
    data.dependencies.callStatements.forEach(call => {
      if (!programs.has(call.program)) {
        missingCalls.add(call.program);
      }
    });

    // Check COPY references
    data.dependencies.copyStatements.forEach(copy => {
      if (!copybooks.has(copy.copybook)) {
        missingCopies.add(copy.copybook);
      }
    });
  }

  console.log(chalk.yellow('\nCross-reference Summary:'));
  console.log(`- Programs found: ${programs.size}`);
  console.log(`- Copybooks found: ${copybooks.size}`);
  console.log(`- Missing CALL targets: ${missingCalls.size}`);
  console.log(`- Missing COPY targets: ${missingCopies.size}`);

  if (missingCalls.size > 0) {
    console.log(chalk.red('\nMissing programs (called but not found):'));
    [...missingCalls].slice(0, 10).forEach(p => console.log(`  - ${p}`));
    if (missingCalls.size > 10) {
      console.log(`  ... and ${missingCalls.size - 10} more`);
    }
  }

  if (missingCopies.size > 0) {
    console.log(chalk.red('\nMissing copybooks (copied but not found):'));
    [...missingCopies].slice(0, 10).forEach(c => console.log(`  - ${c}`));
    if (missingCopies.size > 10) {
      console.log(`  ... and ${missingCopies.size - 10} more`);
    }
  }
}

// Run validation
if (require.main === module) {
  validateAll()
    .then(() => checkCrossReferences())
    .catch(console.error);
}

module.exports = { validateStructure, validateAll };