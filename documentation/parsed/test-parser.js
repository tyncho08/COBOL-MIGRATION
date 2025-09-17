const fs = require('fs');
const path = require('path');
const COBOLParser = require('./parse-cobol-simple');

class ParserTester {
    constructor() {
        this.parser = new COBOLParser();
        this.testResults = {
            passed: 0,
            failed: 0,
            tests: []
        };
    }

    runAllTests() {
        console.log('=== COBOL Parser Test Suite ===\n');
        
        this.testProgramIdExtraction();
        this.testDivisionExtraction();
        this.testCopyStatementExtraction();
        this.testCallStatementExtraction();
        this.testFileDescriptionExtraction();
        this.testDataDefinitionExtraction();
        this.testRealFilesParsing();
        
        this.printResults();
    }

    testProgramIdExtraction() {
        console.log('Testing PROGRAM-ID extraction...');
        
        const testCases = [
            {
                name: 'Simple program ID',
                content: 'PROGRAM-ID. TEST-PROG.',
                expected: 'TEST-PROG'
            },
            {
                name: 'Program ID with spaces',
                content: 'PROGRAM-ID.    SAMPLE-123.',
                expected: 'SAMPLE-123'
            },
            {
                name: 'No program ID',
                content: 'IDENTIFICATION DIVISION.',
                expected: 'UNKNOWN'
            }
        ];
        
        testCases.forEach(test => {
            const result = this.parser.extractProgramId(test.content);
            this.assertTest(test.name, result, test.expected);
        });
    }

    testDivisionExtraction() {
        console.log('\nTesting division extraction...');
        
        const content = `
            IDENTIFICATION DIVISION.
            ENVIRONMENT DIVISION.
            DATA DIVISION.
            PROCEDURE DIVISION.
        `;
        
        const divisions = this.parser.extractDivisions(content);
        this.assertTest(
            'All divisions found',
            divisions.length,
            4
        );
        
        this.assertTest(
            'Division names correct',
            divisions.map(d => d.name).sort(),
            ['DATA', 'ENVIRONMENT', 'IDENTIFICATION', 'PROCEDURE']
        );
    }

    testCopyStatementExtraction() {
        console.log('\nTesting COPY statement extraction...');
        
        const content = `
            COPY CUSTOMER-REC.
            COPY SQLCA OF MYSQLDB.
            COPY wspay.
        `;
        
        const copies = this.parser.extractCopyStatements(content);
        this.assertTest(
            'Number of COPY statements',
            copies.length,
            3
        );
        
        this.assertTest(
            'First copybook name',
            copies[0].copybook,
            'CUSTOMER-REC'
        );
        
        this.assertTest(
            'COPY with library',
            copies[1].library,
            'MYSQLDB'
        );
    }

    testCallStatementExtraction() {
        console.log('\nTesting CALL statement extraction...');
        
        const content = `
            CALL "SUBPROG1"
            CALL 'SUBPROG2' USING WS-PARAM
            CALL WS-PROGRAM-NAME
        `;
        
        const calls = this.parser.extractCallStatements(content);
        this.assertTest(
            'Number of CALL statements',
            calls.length,
            3
        );
        
        this.assertTest(
            'Static call detection',
            calls[0].type,
            'static'
        );
        
        this.assertTest(
            'Dynamic call detection',
            calls[2].type,
            'dynamic'
        );
    }

    testFileDescriptionExtraction() {
        console.log('\nTesting file description extraction...');
        
        const content = `
            SELECT CUSTOMER-FILE ASSIGN TO "CUSTFILE.DAT"
                   ORGANIZATION IS INDEXED
                   ACCESS MODE IS DYNAMIC.
            
            FD CUSTOMER-FILE
               RECORDING MODE IS F.
        `;
        
        const files = this.parser.extractFileDescriptions(content);
        this.assertTest(
            'SELECT statement found',
            files.filter(f => f.type === 'SELECT').length,
            1
        );
        
        this.assertTest(
            'FD statement found',
            files.filter(f => f.type === 'FD').length,
            1
        );
    }

    testDataDefinitionExtraction() {
        console.log('\nTesting data definition extraction...');
        
        const content = `
            WORKING-STORAGE SECTION.
            01 WS-COUNTER PIC 9(3).
            77 WS-FLAG PIC X.
            01 WS-RECORD.
               05 WS-NAME PIC X(30).
            
            LINKAGE SECTION.
            01 LK-PARAM PIC X(10).
        `;
        
        const dataItems = this.parser.extractDataDefinitions(content);
        this.assertTest(
            'Working storage items found',
            dataItems.filter(d => d.section === 'WORKING-STORAGE').length,
            3
        );
        
        this.assertTest(
            'Linkage items found',
            dataItems.filter(d => d.section === 'LINKAGE').length,
            1
        );
    }

    testRealFilesParsing() {
        console.log('\nTesting real COBOL file parsing...');
        
        // Find a sample COBOL file to test
        const legacyDir = path.resolve(__dirname, '../../Legacy_App');
        const testFiles = [
            path.join(legacyDir, 'common', 'acas000.cbl'),
            path.join(legacyDir, 'sales', 'sales.cbl'),
            path.join(legacyDir, 'copybooks', 'wstime.cob')
        ];
        
        testFiles.forEach(filePath => {
            if (fs.existsSync(filePath)) {
                console.log(`\nParsing ${path.basename(filePath)}...`);
                const structure = this.parser.parseFile(filePath);
                
                this.assertTest(
                    `${path.basename(filePath)} - has program ID`,
                    structure !== null && structure.programId !== 'UNKNOWN',
                    true
                );
                
                if (structure) {
                    console.log(`  Program ID: ${structure.programId}`);
                    console.log(`  Lines: ${structure.metadata.lines}`);
                    console.log(`  Divisions: ${structure.divisions.length}`);
                    console.log(`  COPY statements: ${structure.copyStatements.length}`);
                    console.log(`  CALL statements: ${structure.callStatements.length}`);
                }
            }
        });
    }

    assertTest(testName, actual, expected) {
        const passed = JSON.stringify(actual) === JSON.stringify(expected);
        
        this.testResults.tests.push({
            name: testName,
            passed: passed,
            actual: actual,
            expected: expected
        });
        
        if (passed) {
            this.testResults.passed++;
            console.log(`  ✓ ${testName}`);
        } else {
            this.testResults.failed++;
            console.log(`  ✗ ${testName}`);
            console.log(`    Expected: ${JSON.stringify(expected)}`);
            console.log(`    Actual: ${JSON.stringify(actual)}`);
        }
    }

    printResults() {
        console.log('\n=== Test Results ===');
        console.log(`Total tests: ${this.testResults.passed + this.testResults.failed}`);
        console.log(`Passed: ${this.testResults.passed}`);
        console.log(`Failed: ${this.testResults.failed}`);
        
        if (this.testResults.failed === 0) {
            console.log('\n✓ All tests passed!');
        } else {
            console.log('\n✗ Some tests failed.');
        }
    }
}

// Main execution
if (require.main === module) {
    const tester = new ParserTester();
    tester.runAllTests();
}

module.exports = ParserTester;