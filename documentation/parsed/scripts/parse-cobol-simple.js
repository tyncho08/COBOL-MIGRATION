#!/usr/bin/env node

const fs = require('fs').promises;
const path = require('path');
const { glob } = require('glob');
const ora = require('ora');
const chalk = require('chalk');
const { program } = require('commander');

// Parser configuration
const CONFIG = {
  rootDir: path.join(__dirname, '../../../Legacy_App'),
  outputDir: path.join(__dirname, '../parsed-structures'),
  patterns: ['**/*.cbl', '**/*.cpy', '**/*.CPY', '**/*.cob'],
  excludeDirs: ['node_modules', '.git', 'ACAS-Manuals', 'mysql']
};

// COBOL regex patterns
const PATTERNS = {
  programId: /PROGRAM-ID\.\s+([A-Za-z0-9-]+)/i,
  author: /AUTHOR\.\s+([^\n]+)/i,
  dateWritten: /DATE-WRITTEN\.\s+([^\n]+)/i,
  dateCompiled: /DATE-COMPILED\.\s+([^\n]+)/i,
  copyStatement: /COPY\s+([A-Za-z0-9-]+)/gi,
  callStatement: /CALL\s+["']([A-Za-z0-9-]+)["']/gi,
  fileControl: /SELECT\s+([A-Za-z0-9-]+)\s+ASSIGN\s+TO\s+([^\n]+)/gi,
  fdEntry: /FD\s+([A-Za-z0-9-]+)/gi,
  performStatement: /PERFORM\s+([A-Za-z0-9-]+)(?:\s+THRU\s+([A-Za-z0-9-]+))?/gi,
  goToStatement: /GO\s+TO\s+([A-Za-z0-9-]+)/gi,
  sqlStatement: /EXEC\s+SQL\s+([\s\S]+?)END-EXEC/gi,
  section: /^[\s]*([A-Za-z0-9-]+)\s+SECTION\./gm,
  paragraph: /^[\s]*([A-Za-z0-9-]+)\.\s*$/gm,
  dataLevel: /^[\s]*(0[1-9]|[1-4][0-9]|77|88)\s+([A-Za-z0-9-]+)/gm,
  picClause: /PIC(?:TURE)?\s+(?:IS\s+)?([^\s.]+)/i,
  computeStatement: /COMPUTE\s+([A-Za-z0-9-]+)\s*=\s*([^.]+)\./gi,
  ifStatement: /IF\s+([^.]+?)(?:\s+THEN)?/gi,
  evaluateStatement: /EVALUATE\s+([^.]+?)\s+WHEN/gi,
  moveStatement: /MOVE\s+([^\s]+)\s+TO\s+([^.]+)/gi,
  comment: /^\s*\*/gm
};

// Parse a single COBOL file
async function parseCobolFile(filePath) {
  try {
    const content = await fs.readFile(filePath, 'utf-8');
    const lines = content.split('\n');
    const stats = await fs.stat(filePath);
    
    // Extract metadata
    const metadata = {
      filename: path.basename(filePath),
      filepath: path.relative(CONFIG.rootDir, filePath),
      parseTimestamp: new Date().toISOString(),
      fileSize: stats.size,
      lastModified: stats.mtime.toISOString(),
      parseVersion: "1.0",
      dialect: "GnuCOBOL"
    };

    // Extract identification division
    const identification = {
      programId: extractPattern(content, PATTERNS.programId) || path.basename(filePath, path.extname(filePath)).toUpperCase(),
      programType: determineProgType(filePath, content),
      author: extractPattern(content, PATTERNS.author),
      dateWritten: extractPattern(content, PATTERNS.dateWritten),
      dateCompiled: extractPattern(content, PATTERNS.dateCompiled),
      remarks: extractRemarks(content)
    };

    // Extract environment division
    const environment = parseEnvironmentDivision(content);

    // Extract data division
    const data = parseDataDivision(content);

    // Extract procedure division
    const procedure = parseProcedureDivision(content);

    // Extract dependencies
    const dependencies = parseDependencies(content, lines);

    // Extract control flow
    const controlFlow = parseControlFlow(content, lines);

    // Extract business logic
    const businessLogic = parseBusinessLogic(content, lines);

    // Calculate metrics
    const metrics = calculateMetrics(content, lines, procedure, controlFlow, dependencies);

    // Assess quality indicators
    const qualityIndicators = assessQuality(content, controlFlow, metrics);

    return {
      metadata,
      identification,
      environment,
      data,
      procedure,
      dependencies,
      controlFlow,
      businessLogic,
      metrics,
      qualityIndicators
    };
  } catch (error) {
    throw new Error(`Failed to parse ${filePath}: ${error.message}`);
  }
}

// Determine program type
function determineProgType(filePath, content) {
  if (filePath.includes('copybook') || path.extname(filePath).toLowerCase() === '.cpy') {
    return 'copybook';
  }
  if (/PROCEDURE\s+DIVISION\s+USING/i.test(content)) {
    return 'sub';
  }
  return 'main';
}

// Extract single pattern match
function extractPattern(content, pattern) {
  const match = content.match(pattern);
  return match ? match[1].trim() : null;
}

// Extract all pattern matches
function extractAllMatches(content, pattern) {
  const matches = [];
  let match;
  while ((match = pattern.exec(content)) !== null) {
    matches.push(match);
  }
  return matches;
}

// Extract remarks from identification division
function extractRemarks(content) {
  const remarks = [];
  const remarksMatch = content.match(/REMARKS\.([\s\S]*?)(?=ENVIRONMENT|DATA|PROCEDURE|$)/i);
  if (remarksMatch) {
    const lines = remarksMatch[1].split('\n').filter(line => line.trim() && !line.trim().startsWith('*'));
    remarks.push(...lines.map(line => line.trim()));
  }
  return remarks;
}

// Parse environment division
function parseEnvironmentDivision(content) {
  const fileControl = [];
  const matches = extractAllMatches(content, PATTERNS.fileControl);
  
  matches.forEach(match => {
    const fileName = match[1];
    const assignTo = match[2].trim();
    const fileControlEntry = {
      fileName,
      assignTo,
      organization: 'SEQUENTIAL', // Default
      accessMode: 'SEQUENTIAL',
      recordKey: null,
      alternateKeys: [],
      fileStatus: null
    };

    // Extract additional file attributes
    const fileSection = content.substring(match.index, match.index + 500);
    if (/ORGANIZATION\s+IS\s+([A-Z]+)/i.test(fileSection)) {
      fileControlEntry.organization = fileSection.match(/ORGANIZATION\s+IS\s+([A-Z]+)/i)[1];
    }
    if (/ACCESS\s+MODE\s+IS\s+([A-Z]+)/i.test(fileSection)) {
      fileControlEntry.accessMode = fileSection.match(/ACCESS\s+MODE\s+IS\s+([A-Z]+)/i)[1];
    }
    if (/RECORD\s+KEY\s+IS\s+([A-Za-z0-9-]+)/i.test(fileSection)) {
      fileControlEntry.recordKey = fileSection.match(/RECORD\s+KEY\s+IS\s+([A-Za-z0-9-]+)/i)[1];
    }
    if (/FILE\s+STATUS\s+IS\s+([A-Za-z0-9-]+)/i.test(fileSection)) {
      fileControlEntry.fileStatus = fileSection.match(/FILE\s+STATUS\s+IS\s+([A-Za-z0-9-]+)/i)[1];
    }

    fileControl.push(fileControlEntry);
  });

  return {
    sourceComputer: extractPattern(content, /SOURCE-COMPUTER\.\s+([^\n.]+)/i),
    objectComputer: extractPattern(content, /OBJECT-COMPUTER\.\s+([^\n.]+)/i),
    fileControl
  };
}

// Parse data division
function parseDataDivision(content) {
  const fileSection = [];
  const workingStorage = {
    fields: [],
    constants: [],
    tables: [],
    flags: [],
    counters: [],
    accumulators: []
  };
  const localStorage = { fields: [] };
  const linkageSection = { parameters: [] };

  // Extract FD entries
  const fdMatches = extractAllMatches(content, PATTERNS.fdEntry);
  fdMatches.forEach(match => {
    const fdName = match[1];
    const fdSection = extractSection(content, match.index, /^[\s]*0[1-9]/m);
    fileSection.push({
      fdName,
      recordName: extractRecordName(fdSection),
      fields: parseDataFields(fdSection)
    });
  });

  // Parse working storage section
  const wsMatch = content.match(/WORKING-STORAGE\s+SECTION\.([\s\S]*?)(?=LOCAL-STORAGE|LINKAGE|PROCEDURE|$)/i);
  if (wsMatch) {
    const wsFields = parseDataFields(wsMatch[1]);
    categorizeWorkingStorageFields(wsFields, workingStorage);
  }

  // Parse local storage section
  const lsMatch = content.match(/LOCAL-STORAGE\s+SECTION\.([\s\S]*?)(?=LINKAGE|PROCEDURE|$)/i);
  if (lsMatch) {
    localStorage.fields = parseDataFields(lsMatch[1]);
  }

  // Parse linkage section
  const linkMatch = content.match(/LINKAGE\s+SECTION\.([\s\S]*?)(?=PROCEDURE|$)/i);
  if (linkMatch) {
    linkageSection.parameters = parseDataFields(linkMatch[1]);
  }

  return {
    fileSection,
    workingStorage,
    localStorage,
    linkageSection
  };
}

// Extract section content
function extractSection(content, startIndex, endPattern) {
  const remaining = content.substring(startIndex);
  const endMatch = remaining.match(endPattern);
  return endMatch ? remaining.substring(0, endMatch.index) : remaining.substring(0, 1000);
}

// Extract record name from FD section
function extractRecordName(fdSection) {
  const match = fdSection.match(/^\s*01\s+([A-Za-z0-9-]+)/m);
  return match ? match[1] : null;
}

// Parse data fields with hierarchy
function parseDataFields(section) {
  const fields = [];
  const lines = section.split('\n');
  const fieldStack = [];

  lines.forEach((line, index) => {
    const match = line.match(/^\s*(0[1-9]|[1-4][0-9]|77|88)\s+([A-Za-z0-9-]+)/);
    if (match) {
      const level = parseInt(match[1]);
      const name = match[2];
      const field = {
        level: match[1],
        name,
        picture: extractPattern(line, PATTERNS.picClause),
        usage: extractPattern(line, /USAGE\s+(?:IS\s+)?([A-Z-]+)/i),
        value: extractFieldValue(line),
        occurs: extractPattern(line, /OCCURS\s+(\d+)(?:\s+TIMES)?/i),
        redefines: extractPattern(line, /REDEFINES\s+([A-Za-z0-9-]+)/i),
        children: []
      };

      // Handle field hierarchy
      if (level === 1 || level === 77) {
        fields.push(field);
        fieldStack.length = 0;
        fieldStack.push(field);
      } else if (level === 88) {
        // Condition names
        if (fieldStack.length > 0) {
          const parent = fieldStack[fieldStack.length - 1];
          if (!parent.conditions) parent.conditions = [];
          parent.conditions.push({
            name,
            value: extractFieldValue(line)
          });
        }
      } else {
        // Find parent level
        while (fieldStack.length > 0 && parseInt(fieldStack[fieldStack.length - 1].level) >= level) {
          fieldStack.pop();
        }
        if (fieldStack.length > 0) {
          fieldStack[fieldStack.length - 1].children.push(field);
          fieldStack.push(field);
        }
      }
    }
  });

  return fields;
}

// Extract field value
function extractFieldValue(line) {
  const valueMatch = line.match(/VALUE\s+(?:IS\s+)?([^\s.]+)/i);
  if (valueMatch) {
    return valueMatch[1].replace(/["']/g, '');
  }
  // Check for 88-level values
  const conditionMatch = line.match(/88\s+[A-Za-z0-9-]+\s+VALUE(?:S)?\s+(?:IS\s+|ARE\s+)?(.+)\./i);
  if (conditionMatch) {
    return conditionMatch[1].trim();
  }
  return null;
}

// Categorize working storage fields
function categorizeWorkingStorageFields(fields, workingStorage) {
  fields.forEach(field => {
    if (field.name.includes('FLAG') || field.name.includes('SW')) {
      workingStorage.flags.push(field);
    } else if (field.name.includes('COUNT') || field.name.includes('CTR')) {
      workingStorage.counters.push(field);
    } else if (field.name.includes('TOTAL') || field.name.includes('SUM') || field.name.includes('ACCUM')) {
      workingStorage.accumulators.push(field);
    } else if (field.occurs) {
      workingStorage.tables.push(field);
    } else if (field.value && !field.children.length) {
      workingStorage.constants.push(field);
    } else {
      workingStorage.fields.push(field);
    }
  });
}

// Parse procedure division
function parseProcedureDivision(content) {
  const divisions = [];
  const sections = [];
  const paragraphs = [];

  // Extract procedure division parameters
  const procMatch = content.match(/PROCEDURE\s+DIVISION(?:\s+USING\s+([^.]+))?\./i);
  if (procMatch) {
    if (procMatch[1]) {
      divisions.push({
        name: 'PROCEDURE DIVISION',
        using: procMatch[1].split(/\s+/).filter(p => p && p !== 'USING')
      });
    }
  }

  // Extract sections
  const sectionMatches = extractAllMatches(content, PATTERNS.section);
  sectionMatches.forEach(match => {
    const sectionName = match[1];
    const sectionContent = extractSectionContent(content, match.index);
    const sectionParagraphs = parseSectionParagraphs(sectionContent);
    
    sections.push({
      name: sectionName,
      paragraphs: sectionParagraphs
    });
  });

  // Extract standalone paragraphs (not in sections)
  const paragraphMatches = extractAllMatches(content, PATTERNS.paragraph);
  paragraphMatches.forEach(match => {
    const paragName = match[1];
    if (!sections.some(s => s.paragraphs.some(p => p.name === paragName))) {
      paragraphs.push({
        name: paragName,
        statements: [],
        performedBy: [],
        performs: []
      });
    }
  });

  return { divisions, sections, paragraphs };
}

// Extract section content
function extractSectionContent(content, startIndex) {
  const remaining = content.substring(startIndex);
  const nextSectionMatch = remaining.match(/^\s*[A-Za-z0-9-]+\s+SECTION\./m);
  if (nextSectionMatch && nextSectionMatch.index > 0) {
    return remaining.substring(0, nextSectionMatch.index);
  }
  const procEndMatch = remaining.match(/^\s*(IDENTIFICATION|ENVIRONMENT|DATA)\s+DIVISION/m);
  if (procEndMatch) {
    return remaining.substring(0, procEndMatch.index);
  }
  return remaining;
}

// Parse paragraphs within a section
function parseSectionParagraphs(sectionContent) {
  const paragraphs = [];
  const paragraphMatches = extractAllMatches(sectionContent, PATTERNS.paragraph);
  
  paragraphMatches.forEach((match, index) => {
    const paragName = match[1];
    const startIndex = match.index;
    const endIndex = index < paragraphMatches.length - 1 ? 
      paragraphMatches[index + 1].index : sectionContent.length;
    const paragContent = sectionContent.substring(startIndex, endIndex);
    
    paragraphs.push({
      name: paragName,
      statements: extractStatements(paragContent),
      performedBy: [],
      performs: extractPerforms(paragContent)
    });
  });

  return paragraphs;
}

// Extract statements from paragraph
function extractStatements(content) {
  const statements = [];
  const statementPatterns = [
    { type: 'MOVE', pattern: /MOVE\s+/i },
    { type: 'COMPUTE', pattern: /COMPUTE\s+/i },
    { type: 'PERFORM', pattern: /PERFORM\s+/i },
    { type: 'IF', pattern: /IF\s+/i },
    { type: 'EVALUATE', pattern: /EVALUATE\s+/i },
    { type: 'CALL', pattern: /CALL\s+/i },
    { type: 'READ', pattern: /READ\s+/i },
    { type: 'WRITE', pattern: /WRITE\s+/i },
    { type: 'DISPLAY', pattern: /DISPLAY\s+/i },
    { type: 'ACCEPT', pattern: /ACCEPT\s+/i }
  ];

  statementPatterns.forEach(({ type, pattern }) => {
    if (pattern.test(content)) {
      statements.push(type);
    }
  });

  return [...new Set(statements)];
}

// Extract PERFORM targets
function extractPerforms(content) {
  const performs = [];
  const performMatches = extractAllMatches(content, PATTERNS.performStatement);
  
  performMatches.forEach(match => {
    performs.push(match[1]);
    if (match[2]) performs.push(match[2]);
  });

  return [...new Set(performs)];
}

// Parse dependencies
function parseDependencies(content, lines) {
  const copyStatements = [];
  const callStatements = [];
  const sqlStatements = [];

  // Extract COPY statements
  const copyMatches = extractAllMatches(content, PATTERNS.copyStatement);
  copyMatches.forEach(match => {
    const lineNum = getLineNumber(content, match.index);
    const context = getContext(content, match.index);
    copyStatements.push({
      copybook: match[1],
      line: lineNum,
      inDivision: getDivision(content, match.index),
      inSection: getSection(content, match.index)
    });
  });

  // Extract CALL statements
  const callMatches = extractAllMatches(content, PATTERNS.callStatement);
  callMatches.forEach(match => {
    const lineNum = getLineNumber(content, match.index);
    const context = getContext(content, match.index);
    const callStmt = {
      program: match[1],
      type: 'STATIC',
      line: lineNum,
      using: extractCallParams(context),
      context: getParagraphName(content, match.index)
    };
    callStatements.push(callStmt);
  });

  // Extract SQL statements
  const sqlMatches = extractAllMatches(content, PATTERNS.sqlStatement);
  sqlMatches.forEach(match => {
    const sqlContent = match[1];
    const sqlType = extractSqlType(sqlContent);
    const tables = extractSqlTables(sqlContent);
    sqlStatements.push({
      type: sqlType,
      tables,
      line: getLineNumber(content, match.index),
      embedded: true
    });
  });

  return { copyStatements, callStatements, sqlStatements };
}

// Get line number for a position
function getLineNumber(content, position) {
  return content.substring(0, position).split('\n').length;
}

// Get context around a position
function getContext(content, position, range = 100) {
  const start = Math.max(0, position - range);
  const end = Math.min(content.length, position + range);
  return content.substring(start, end);
}

// Get current division
function getDivision(content, position) {
  const before = content.substring(0, position);
  if (/PROCEDURE\s+DIVISION/i.test(before)) return 'PROCEDURE';
  if (/DATA\s+DIVISION/i.test(before)) return 'DATA';
  if (/ENVIRONMENT\s+DIVISION/i.test(before)) return 'ENVIRONMENT';
  if (/IDENTIFICATION\s+DIVISION/i.test(before)) return 'IDENTIFICATION';
  return 'UNKNOWN';
}

// Get current section
function getSection(content, position) {
  const before = content.substring(0, position);
  const sectionMatch = before.match(/([A-Z-]+)\s+SECTION\.\s*$/mi);
  return sectionMatch ? sectionMatch[1] : null;
}

// Get current paragraph name
function getParagraphName(content, position) {
  const before = content.substring(0, position);
  const lines = before.split('\n');
  for (let i = lines.length - 1; i >= 0; i--) {
    const match = lines[i].match(/^[\s]*([A-Za-z0-9-]+)\.\s*$/);
    if (match) return match[1];
  }
  return null;
}

// Extract CALL parameters
function extractCallParams(context) {
  const usingMatch = context.match(/USING\s+([^.]+)/i);
  if (usingMatch) {
    return usingMatch[1].split(/\s+/).filter(p => p && p !== 'USING');
  }
  return [];
}

// Extract SQL type
function extractSqlType(sqlContent) {
  if (/^\s*SELECT/i.test(sqlContent)) return 'SELECT';
  if (/^\s*INSERT/i.test(sqlContent)) return 'INSERT';
  if (/^\s*UPDATE/i.test(sqlContent)) return 'UPDATE';
  if (/^\s*DELETE/i.test(sqlContent)) return 'DELETE';
  return 'OTHER';
}

// Extract SQL tables
function extractSqlTables(sqlContent) {
  const tables = [];
  const fromMatch = sqlContent.match(/FROM\s+([A-Za-z0-9_]+)/i);
  if (fromMatch) tables.push(fromMatch[1]);
  const intoMatch = sqlContent.match(/INTO\s+([A-Za-z0-9_]+)/i);
  if (intoMatch) tables.push(intoMatch[1]);
  const updateMatch = sqlContent.match(/UPDATE\s+([A-Za-z0-9_]+)/i);
  if (updateMatch) tables.push(updateMatch[1]);
  return [...new Set(tables)];
}

// Parse control flow
function parseControlFlow(content, lines) {
  const performStatements = [];
  const goToStatements = [];
  const conditionals = [];

  // Extract PERFORM statements
  const performMatches = extractAllMatches(content, PATTERNS.performStatement);
  performMatches.forEach(match => {
    const stmt = {
      type: 'PERFORM',
      target: match[1],
      thru: match[2] || null,
      condition: null,
      line: getLineNumber(content, match.index)
    };

    // Check for PERFORM UNTIL
    const context = getContext(content, match.index, 200);
    if (/PERFORM\s+UNTIL/i.test(context)) {
      stmt.type = 'PERFORM UNTIL';
      const untilMatch = context.match(/UNTIL\s+([^.]+)/i);
      if (untilMatch) stmt.condition = untilMatch[1].trim();
    }

    // Check for PERFORM VARYING
    if (/PERFORM\s+VARYING/i.test(context)) {
      stmt.type = 'PERFORM VARYING';
      const varyingMatch = context.match(/VARYING\s+([^.]+)/i);
      if (varyingMatch) stmt.condition = varyingMatch[1].trim();
    }

    performStatements.push(stmt);
  });

  // Extract GO TO statements
  const goToMatches = extractAllMatches(content, PATTERNS.goToStatement);
  goToMatches.forEach(match => {
    goToStatements.push({
      target: match[1],
      line: getLineNumber(content, match.index),
      conditional: /IF.*GO\s+TO/i.test(getContext(content, match.index))
    });
  });

  // Extract IF statements
  const ifMatches = extractAllMatches(content, PATTERNS.ifStatement);
  ifMatches.forEach(match => {
    conditionals.push({
      type: 'IF',
      condition: match[1].trim(),
      line: getLineNumber(content, match.index),
      complexity: calculateConditionComplexity(match[1])
    });
  });

  // Extract EVALUATE statements
  const evalMatches = extractAllMatches(content, PATTERNS.evaluateStatement);
  evalMatches.forEach(match => {
    conditionals.push({
      type: 'EVALUATE',
      condition: match[1].trim(),
      line: getLineNumber(content, match.index),
      complexity: countWhenClauses(getContext(content, match.index, 500))
    });
  });

  return { performStatements, goToStatements, conditionals };
}

// Calculate condition complexity
function calculateConditionComplexity(condition) {
  const operators = (condition.match(/\b(AND|OR)\b/gi) || []).length;
  return operators + 1;
}

// Count WHEN clauses in EVALUATE
function countWhenClauses(context) {
  return (context.match(/\bWHEN\b/gi) || []).length;
}

// Parse business logic
function parseBusinessLogic(content, lines) {
  const calculations = [];
  const validations = [];
  const fileOperations = [];

  // Extract COMPUTE statements
  const computeMatches = extractAllMatches(content, PATTERNS.computeStatement);
  computeMatches.forEach(match => {
    calculations.push({
      target: match[1],
      formula: match[2].trim(),
      line: getLineNumber(content, match.index),
      rounding: /ROUNDED/i.test(match[2]) ? 'ROUNDED' : null
    });
  });

  // Extract arithmetic operations in MOVE, ADD, SUBTRACT, etc.
  const arithmeticPatterns = [
    /ADD\s+([^\s]+)\s+TO\s+([A-Za-z0-9-]+)/gi,
    /SUBTRACT\s+([^\s]+)\s+FROM\s+([A-Za-z0-9-]+)/gi,
    /MULTIPLY\s+([^\s]+)\s+BY\s+([A-Za-z0-9-]+)/gi,
    /DIVIDE\s+([^\s]+)\s+INTO\s+([A-Za-z0-9-]+)/gi
  ];

  arithmeticPatterns.forEach(pattern => {
    const matches = extractAllMatches(content, pattern);
    matches.forEach(match => {
      calculations.push({
        target: match[2],
        formula: match[0],
        line: getLineNumber(content, match.index),
        rounding: /ROUNDED/i.test(getContext(content, match.index)) ? 'ROUNDED' : null
      });
    });
  });

  // Extract validations from IF statements
  const validationPatterns = [
    /IF\s+([A-Za-z0-9-]+)\s+(=|NOT\s*=|>|<|>=|<=)\s*([^\s]+)/gi,
    /IF\s+([A-Za-z0-9-]+)\s+IS\s+(NUMERIC|ALPHABETIC|POSITIVE|NEGATIVE|ZERO)/gi
  ];

  validationPatterns.forEach(pattern => {
    const matches = extractAllMatches(content, pattern);
    matches.forEach(match => {
      validations.push({
        field: match[1],
        condition: match[0],
        line: getLineNumber(content, match.index),
        errorHandling: extractErrorHandling(content, match.index)
      });
    });
  });

  // Extract file operations
  const fileOpPatterns = [
    { op: 'READ', pattern: /READ\s+([A-Za-z0-9-]+)/gi },
    { op: 'WRITE', pattern: /WRITE\s+([A-Za-z0-9-]+)/gi },
    { op: 'REWRITE', pattern: /REWRITE\s+([A-Za-z0-9-]+)/gi },
    { op: 'DELETE', pattern: /DELETE\s+([A-Za-z0-9-]+)/gi },
    { op: 'START', pattern: /START\s+([A-Za-z0-9-]+)/gi }
  ];

  fileOpPatterns.forEach(({ op, pattern }) => {
    const matches = extractAllMatches(content, pattern);
    matches.forEach(match => {
      fileOperations.push({
        operation: op,
        file: match[1],
        line: getLineNumber(content, match.index),
        errorHandling: extractFileErrorHandling(content, match.index)
      });
    });
  });

  return { calculations, validations, fileOperations };
}

// Extract error handling
function extractErrorHandling(content, position) {
  const context = getContext(content, position, 200);
  if (/ELSE|PERFORM|MOVE|DISPLAY/i.test(context)) {
    const elseMatch = context.match(/ELSE\s+([^.]+)/i);
    return elseMatch ? elseMatch[1].trim() : 'ELSE clause';
  }
  return null;
}

// Extract file error handling
function extractFileErrorHandling(content, position) {
  const context = getContext(content, position, 200);
  const handlers = [];
  if (/AT\s+END/i.test(context)) handlers.push('AT END');
  if (/NOT\s+AT\s+END/i.test(context)) handlers.push('NOT AT END');
  if (/INVALID\s+KEY/i.test(context)) handlers.push('INVALID KEY');
  if (/NOT\s+INVALID\s+KEY/i.test(context)) handlers.push('NOT INVALID KEY');
  return handlers.join(', ') || null;
}

// Calculate metrics
function calculateMetrics(content, lines, procedure, controlFlow, dependencies) {
  const codeLines = lines.filter(line => line.trim() && !line.trim().startsWith('*'));
  const commentLines = lines.filter(line => line.trim().startsWith('*'));

  return {
    linesOfCode: codeLines.length,
    linesOfComment: commentLines.length,
    cyclomaticComplexity: calculateCyclomaticComplexity(controlFlow),
    numberOfSections: procedure.sections.length,
    numberOfParagraphs: procedure.paragraphs.length + 
      procedure.sections.reduce((sum, s) => sum + s.paragraphs.length, 0),
    numberOfCalls: dependencies.callStatements.length,
    numberOfFiles: countUniqueFiles(content),
    numberOfSQLStatements: dependencies.sqlStatements.length,
    deepestNesting: calculateDeepestNesting(lines)
  };
}

// Calculate cyclomatic complexity
function calculateCyclomaticComplexity(controlFlow) {
  let complexity = 1;
  complexity += controlFlow.conditionals.reduce((sum, c) => sum + c.complexity, 0);
  complexity += controlFlow.goToStatements.length;
  complexity += controlFlow.performStatements.filter(p => p.type !== 'PERFORM').length;
  return complexity;
}

// Count unique files
function countUniqueFiles(content) {
  const files = new Set();
  const filePatterns = [
    /SELECT\s+([A-Za-z0-9-]+)/gi,
    /FD\s+([A-Za-z0-9-]+)/gi
  ];

  filePatterns.forEach(pattern => {
    const matches = extractAllMatches(content, pattern);
    matches.forEach(match => files.add(match[1]));
  });

  return files.size;
}

// Calculate deepest nesting
function calculateDeepestNesting(lines) {
  let maxNesting = 0;
  let currentNesting = 0;

  lines.forEach(line => {
    if (/\b(IF|EVALUATE|PERFORM)\b/i.test(line)) {
      currentNesting++;
      maxNesting = Math.max(maxNesting, currentNesting);
    }
    if (/\b(END-IF|END-EVALUATE|END-PERFORM)\b/i.test(line) || /\.\s*$/.test(line)) {
      currentNesting = Math.max(0, currentNesting - 1);
    }
  });

  return maxNesting;
}

// Assess quality indicators
function assessQuality(content, controlFlow, metrics) {
  return {
    hasErrorHandling: /AT\s+END|INVALID\s+KEY|ON\s+ERROR/i.test(content),
    hasComments: metrics.linesOfComment > 0,
    usesGoTo: controlFlow.goToStatements.length > 0,
    hasDeadCode: detectDeadCode(content),
    followsNamingConventions: checkNamingConventions(content)
  };
}

// Detect potential dead code
function detectDeadCode(content) {
  // Simple heuristic: check for unreachable paragraphs
  const performTargets = new Set();
  const goToTargets = new Set();
  
  extractAllMatches(content, PATTERNS.performStatement).forEach(m => {
    performTargets.add(m[1]);
    if (m[2]) performTargets.add(m[2]);
  });
  
  extractAllMatches(content, PATTERNS.goToStatement).forEach(m => {
    goToTargets.add(m[1]);
  });

  const allTargets = new Set([...performTargets, ...goToTargets]);
  const paragraphs = extractAllMatches(content, PATTERNS.paragraph).map(m => m[1]);
  
  // Check if there are paragraphs that are never called
  return paragraphs.some(p => !allTargets.has(p) && !['MAIN', 'STARTUP', 'INITIALIZATION'].includes(p));
}

// Check naming conventions
function checkNamingConventions(content) {
  const programId = extractPattern(content, PATTERNS.programId);
  if (!programId) return false;
  
  // Check if program ID matches common patterns
  return /^[A-Z]{2}[0-9]{3}/.test(programId) || /^[A-Z]+(-[A-Z]+)*$/.test(programId);
}

// Main parsing function
async function parseAllFiles() {
  const spinner = ora('Initializing COBOL parser...').start();
  const results = {
    successful: [],
    failed: [],
    stats: {
      totalFiles: 0,
      successfullyParsed: 0,
      failedParsing: 0,
      startTime: new Date(),
      endTime: null
    }
  };

  try {
    // Find all COBOL files
    spinner.text = 'Searching for COBOL files...';
    const files = [];
    
    for (const pattern of CONFIG.patterns) {
      const found = await glob(pattern, {
        cwd: CONFIG.rootDir,
        ignore: CONFIG.excludeDirs
      });
      files.push(...found);
    }

    results.stats.totalFiles = files.length;
    spinner.text = `Found ${files.length} COBOL files. Starting parse...`;

    // Ensure output directory exists
    await fs.mkdir(CONFIG.outputDir, { recursive: true });

    // Parse each file
    for (let i = 0; i < files.length; i++) {
      const file = files[i];
      const filePath = path.join(CONFIG.rootDir, file);
      spinner.text = `Parsing ${i + 1}/${files.length}: ${file}`;

      try {
        const parsed = await parseCobolFile(filePath);
        
        // Generate output filename
        const outputName = file.replace(/[\/\\]/g, '_').replace(/\.(cbl|cpy|cob|CPY)$/i, '') + '.json';
        const outputPath = path.join(CONFIG.outputDir, outputName);
        
        // Write parsed structure
        await fs.writeFile(outputPath, JSON.stringify(parsed, null, 2));
        
        results.successful.push({
          file,
          outputFile: outputName,
          programId: parsed.identification.programId,
          type: parsed.identification.programType,
          metrics: parsed.metrics
        });
        results.stats.successfullyParsed++;
      } catch (error) {
        results.failed.push({
          file,
          error: error.message,
          severity: assessErrorSeverity(error)
        });
        results.stats.failedParsing++;
      }
    }

    results.stats.endTime = new Date();
    spinner.succeed(`Parsing complete! ${results.stats.successfullyParsed}/${results.stats.totalFiles} files parsed successfully.`);

    // Generate summary report
    await generateSummaryReport(results);
    
    // Generate dependency graph
    await generateDependencyGraph(results);
    
    // Generate error report
    await generateErrorReport(results);

  } catch (error) {
    spinner.fail(`Parser failed: ${error.message}`);
    throw error;
  }

  return results;
}

// Assess error severity
function assessErrorSeverity(error) {
  if (error.message.includes('COBOL-74') || error.message.includes('unsupported')) {
    return 'High';
  }
  if (error.message.includes('encoding') || error.message.includes('character')) {
    return 'Medium';
  }
  return 'Low';
}

// Generate summary report
async function generateSummaryReport(results) {
  const duration = (results.stats.endTime - results.stats.startTime) / 1000;
  const successRate = ((results.stats.successfullyParsed / results.stats.totalFiles) * 100).toFixed(1);

  const summary = `# COBOL Parser Summary

## Execution Statistics
- Total files processed: ${results.stats.totalFiles}
- Successfully parsed: ${results.stats.successfullyParsed} (${successRate}%)
- Failed parsing: ${results.stats.failedParsing} (${(100 - successRate).toFixed(1)}%)
- Total lines of code: ${calculateTotalLines(results)}
- Parse duration: ${formatDuration(duration)}

## Inventory by Type
${generateInventoryTable(results)}

## Inventory by Directory
${generateDirectoryTable(results)}

## Program Complexity Distribution
${generateComplexityTable(results)}

## Top 10 Most Complex Programs
${generateTopComplexPrograms(results)}

## File Dependencies Summary
${generateDependencySummary(results)}
`;

  await fs.writeFile(path.join(__dirname, '../parser-summary.md'), summary);
}

// Calculate total lines of code
function calculateTotalLines(results) {
  return results.successful.reduce((sum, r) => sum + r.metrics.linesOfCode, 0).toLocaleString();
}

// Format duration
function formatDuration(seconds) {
  const minutes = Math.floor(seconds / 60);
  const secs = Math.floor(seconds % 60);
  return `${minutes}m ${secs}s`;
}

// Generate inventory table
function generateInventoryTable(results) {
  const types = {};
  results.successful.forEach(r => {
    const ext = path.extname(r.file).toLowerCase();
    types[ext] = (types[ext] || 0) + 1;
  });

  let table = '| Type | Count | Success Rate |\n|------|-------|--------------|\n';
  Object.entries(types).forEach(([ext, count]) => {
    const total = results.successful.filter(r => path.extname(r.file).toLowerCase() === ext).length +
                  results.failed.filter(r => path.extname(r.file).toLowerCase() === ext).length;
    const rate = ((count / total) * 100).toFixed(1);
    table += `| ${ext} | ${count} | ${rate}% |\n`;
  });

  return table;
}

// Generate directory table
function generateDirectoryTable(results) {
  const dirs = {};
  results.successful.forEach(r => {
    const dir = path.dirname(r.file).split('/')[0] || 'root';
    if (!dirs[dir]) dirs[dir] = { success: 0, failed: 0 };
    dirs[dir].success++;
  });
  results.failed.forEach(r => {
    const dir = path.dirname(r.file).split('/')[0] || 'root';
    if (!dirs[dir]) dirs[dir] = { success: 0, failed: 0 };
    dirs[dir].failed++;
  });

  let table = '| Directory | Files | Parsed | Failed | Success Rate |\n|-----------|-------|--------|--------|-------------|\n';
  Object.entries(dirs).forEach(([dir, stats]) => {
    const total = stats.success + stats.failed;
    const rate = ((stats.success / total) * 100).toFixed(1);
    table += `| ${dir} | ${total} | ${stats.success} | ${stats.failed} | ${rate}% |\n`;
  });

  return table;
}

// Generate complexity table
function generateComplexityTable(results) {
  const ranges = {
    'Low (1-10)': 0,
    'Medium (11-20)': 0,
    'High (21-50)': 0,
    'Very High (>50)': 0
  };

  results.successful.forEach(r => {
    const complexity = r.metrics.cyclomaticComplexity;
    if (complexity <= 10) ranges['Low (1-10)']++;
    else if (complexity <= 20) ranges['Medium (11-20)']++;
    else if (complexity <= 50) ranges['High (21-50)']++;
    else ranges['Very High (>50)']++;
  });

  let table = '| Complexity Range | Count | Percentage |\n|-----------------|-------|-----------|\n';
  const total = results.successful.length;
  Object.entries(ranges).forEach(([range, count]) => {
    const pct = ((count / total) * 100).toFixed(1);
    table += `| ${range} | ${count} | ${pct}% |\n`;
  });

  return table;
}

// Generate top complex programs
function generateTopComplexPrograms(results) {
  const sorted = results.successful
    .sort((a, b) => b.metrics.cyclomaticComplexity - a.metrics.cyclomaticComplexity)
    .slice(0, 10);

  let table = '| Program | File | Complexity | Lines |\n|---------|------|------------|-------|\n';
  sorted.forEach(r => {
    table += `| ${r.programId} | ${r.file} | ${r.metrics.cyclomaticComplexity} | ${r.metrics.linesOfCode} |\n`;
  });

  return table;
}

// Generate dependency summary
async function generateDependencySummary(results) {
  let totalCalls = 0;
  let totalCopybooks = 0;
  let uniquePrograms = new Set();
  let uniqueCopybooks = new Set();

  for (const result of results.successful) {
    const jsonPath = path.join(CONFIG.outputDir, result.outputFile);
    try {
      const data = JSON.parse(await fs.readFile(jsonPath, 'utf-8'));
      totalCalls += data.dependencies.callStatements.length;
      totalCopybooks += data.dependencies.copyStatements.length;
      data.dependencies.callStatements.forEach(c => uniquePrograms.add(c.program));
      data.dependencies.copyStatements.forEach(c => uniqueCopybooks.add(c.copybook));
    } catch (e) {
      // Skip if file can't be read
    }
  }

  return `- Total CALL statements: ${totalCalls}
- Unique programs called: ${uniquePrograms.size}
- Total COPY statements: ${totalCopybooks}
- Unique copybooks used: ${uniqueCopybooks.size}`;
}

// Generate dependency graph
async function generateDependencyGraph(results) {
  const graph = {
    nodes: [],
    edges: [],
    statistics: {
      totalPrograms: 0,
      totalCopybooks: 0,
      totalCalls: 0,
      totalCopies: 0
    }
  };

  // Add nodes for all parsed programs
  for (const result of results.successful) {
    graph.nodes.push({
      id: result.programId,
      file: result.file,
      type: result.type,
      metrics: {
        complexity: result.metrics.cyclomaticComplexity,
        lines: result.metrics.linesOfCode
      }
    });
    
    if (result.type === 'copybook') {
      graph.statistics.totalCopybooks++;
    } else {
      graph.statistics.totalPrograms++;
    }
  }

  // Add edges from dependencies
  for (const result of results.successful) {
    const jsonPath = path.join(CONFIG.outputDir, result.outputFile);
    try {
      const data = JSON.parse(await fs.readFile(jsonPath, 'utf-8'));
      
      // Add call edges
      data.dependencies.callStatements.forEach(call => {
        graph.edges.push({
          from: result.programId,
          to: call.program,
          type: 'CALL',
          line: call.line
        });
        graph.statistics.totalCalls++;
      });

      // Add copy edges
      data.dependencies.copyStatements.forEach(copy => {
        graph.edges.push({
          from: result.programId,
          to: copy.copybook,
          type: 'COPY',
          line: copy.line
        });
        graph.statistics.totalCopies++;
      });
    } catch (e) {
      // Skip if file can't be read
    }
  }

  const graphMd = `# Dependency Graph Data

## Statistics
- Total Programs: ${graph.statistics.totalPrograms}
- Total Copybooks: ${graph.statistics.totalCopybooks}
- Total CALL relationships: ${graph.statistics.totalCalls}
- Total COPY relationships: ${graph.statistics.totalCopies}

## Graph Structure

### Nodes (${graph.nodes.length})
\`\`\`json
${JSON.stringify(graph.nodes.slice(0, 10), null, 2)}
${graph.nodes.length > 10 ? '... (truncated)' : ''}
\`\`\`

### Edges (${graph.edges.length})
\`\`\`json
${JSON.stringify(graph.edges.slice(0, 20), null, 2)}
${graph.edges.length > 20 ? '... (truncated)' : ''}
\`\`\`

## Visualization Instructions
To visualize this dependency graph:
1. Use the data above to create a directed graph
2. Nodes represent programs/copybooks
3. Edges represent dependencies (CALL or COPY)
4. Node size can represent complexity or lines of code
5. Edge color can differentiate CALL (blue) from COPY (green)
`;

  await fs.writeFile(path.join(__dirname, '../dependency-graph.md'), graphMd);
}

// Generate error report
async function generateErrorReport(results) {
  if (results.failed.length === 0) {
    await fs.writeFile(path.join(__dirname, '../parser-errors.md'), '# Parser Errors\n\nNo errors encountered during parsing!');
    return;
  }

  let report = `# Parser Errors

## Summary
- Total errors: ${results.failed.length}
- High severity: ${results.failed.filter(e => e.severity === 'High').length}
- Medium severity: ${results.failed.filter(e => e.severity === 'Medium').length}
- Low severity: ${results.failed.filter(e => e.severity === 'Low').length}

## Error Details
| File | Error | Severity |
|------|-------|----------|
`;

  results.failed.forEach(error => {
    report += `| ${error.file} | ${error.error} | ${error.severity} |\n`;
  });

  report += `
## Resolution Suggestions
`;

  // Group errors by type
  const errorTypes = {};
  results.failed.forEach(e => {
    const type = e.error.split(':')[0];
    if (!errorTypes[type]) errorTypes[type] = [];
    errorTypes[type].push(e.file);
  });

  Object.entries(errorTypes).forEach(([type, files]) => {
    report += `
### ${type} (${files.length} files)
Files affected: ${files.slice(0, 5).join(', ')}${files.length > 5 ? ', ...' : ''}

Suggested resolution:
`;
    
    if (type.includes('COBOL-74')) {
      report += '- These files use legacy COBOL-74 syntax. Consider manual review or specialized parsing.\n';
    } else if (type.includes('encoding')) {
      report += '- Character encoding issues. Try converting files to UTF-8 or ASCII.\n';
    } else {
      report += '- Review the specific error and adjust parser patterns if needed.\n';
    }
  });

  await fs.writeFile(path.join(__dirname, '../parser-errors.md'), report);
}

// CLI setup
program
  .name('parse-cobol-simple')
  .description('Parse COBOL programs and extract comprehensive structure information')
  .version('1.0.0')
  .option('-f, --file <path>', 'Parse a single file')
  .option('-d, --directory <path>', 'Parse all files in directory')
  .action(async (options) => {
    if (options.file) {
      console.log(chalk.blue(`Parsing single file: ${options.file}`));
      try {
        const result = await parseCobolFile(options.file);
        console.log(JSON.stringify(result, null, 2));
      } catch (error) {
        console.error(chalk.red(`Error: ${error.message}`));
      }
    } else {
      await parseAllFiles();
    }
  });

// Run if called directly
if (require.main === module) {
  program.parse();
}

module.exports = { parseCobolFile, parseAllFiles };