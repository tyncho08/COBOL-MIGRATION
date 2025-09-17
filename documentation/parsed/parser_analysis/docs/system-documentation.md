# COBOL System Documentation
Generated: 2025-09-17T19:53:05.704Z

## System Overview

This document provides a comprehensive overview of the COBOL system architecture.

### Statistics

- **Total Programs**: 453
- **Total Copybooks**: 175
- **Total File Definitions**: 101
- **Lines of Code**: 213,597

### Program Distribution by Type

| Type | Count | Percentage |
|------|-------|------------|
| main | 384 | 84.8% |
| Unknown | 61 | 13.5% |
| batch | 6 | 1.3% |
| utility | 2 | 0.4% |


### Subsystem Organization

The system is organized into the following main subsystems:

- **Sales**: 37 programs
- **Purchase**: 38 programs
- **Stock**: 12 programs
- **General Ledger**: 18 programs
- **IRS**: 16 programs
- **Common**: 157 programs
- **Copybooks**: 174 programs


## Architecture Patterns

### Call Hierarchy

The system follows a hierarchical call pattern with the following characteristics:


- **Maximum call depth**: 5 levels
- **Average calls per program**: 2.07
- **Programs with no calls**: 188
- **Entry points** (programs not called by others): 372


### Data Flow Patterns


The system uses the following file operation patterns:

- **READ operations**: 2 files
- **WRITE operations**: 31 files  
- **UPDATE operations**: 0 files
- **Unknown operations**: 166 files


### Common Programming Patterns

Common patterns identified in the codebase:



## Technical Debt Analysis


### Summary

- **High Priority Issues**: 167
- **Medium Priority Issues**: 8
- **Low Priority Issues**: 175

### Top Issues

- **REFACTOR**: ACAS - High cyclomatic complexity
- **REFACTOR**: takeon-1 - High cyclomatic complexity
- **REFACTOR**: takeon-2 - High cyclomatic complexity
- **REFACTOR**: acas005 - High cyclomatic complexity
- **REFACTOR**: acas006 - High cyclomatic complexity
- **REFACTOR**: acas007 - High cyclomatic complexity
- **REFACTOR**: acas008 - High cyclomatic complexity
- **REFACTOR**: acas010 - High cyclomatic complexity
- **REFACTOR**: acas011 - High cyclomatic complexity
- **REFACTOR**: acas012 - High cyclomatic complexity


## Migration Recommendations


Based on the analysis, here are key recommendations for migration:

1. **Modularization**: Break down programs with high cyclomatic complexity
2. **Data Layer**: Separate file I/O operations into a data access layer
3. **Service Orientation**: Convert batch programs to microservices
4. **Copybook Consolidation**: Merge related copybooks to reduce duplication
5. **Dead Code Removal**: Eliminate unused programs and copybooks

