const fs = require('fs');
const path = require('path');

class COBOLParser {
    constructor() {
        this.results = [];
        this.errors = [];
        this.fileStats = {
            total: 0,
            parsed: 0,
            errors: 0,
            byExtension: {}
        };
    }

    parseFile(filePath) {
        try {
            const content = fs.readFileSync(filePath, 'utf-8');
            const fileName = path.basename(filePath);
            const relativePath = path.relative(process.cwd(), filePath);
            
            console.log(`Parsing: ${relativePath}`);
            
            const structure = {
                filePath: relativePath,
                fileName: fileName,
                programId: this.extractProgramId(content),
                divisions: this.extractDivisions(content),
                copyStatements: this.extractCopyStatements(content),
                callStatements: this.extractCallStatements(content),
                performStatements: this.extractPerformStatements(content),
                fileDescriptions: this.extractFileDescriptions(content),
                dataDefinitions: this.extractDataDefinitions(content),
                sections: this.extractSections(content),
                paragraphs: this.extractParagraphs(content),
                metadata: {
                    lines: content.split('\n').length,
                    parseDate: new Date().toISOString(),
                    hasMainProgram: this.detectMainProgram(content)
                }
            };

            const ext = path.extname(filePath).toLowerCase();
            this.fileStats.byExtension[ext] = (this.fileStats.byExtension[ext] || 0) + 1;
            this.fileStats.parsed++;

            return structure;
        } catch (error) {
            this.errors.push({
                file: filePath,
                error: error.message
            });
            this.fileStats.errors++;
            console.error(`Error parsing ${filePath}: ${error.message}`);
            return null;
        }
    }

    extractProgramId(content) {
        const match = content.match(/PROGRAM-ID\.\s+([A-Za-z0-9-]+)/i);
        return match ? match[1].trim() : 'UNKNOWN';
    }

    extractDivisions(content) {
        const divisions = [];
        const divisionPattern = /(IDENTIFICATION|ENVIRONMENT|DATA|PROCEDURE)\s+DIVISION/gi;
        let match;
        
        while ((match = divisionPattern.exec(content)) !== null) {
            divisions.push({
                name: match[1].toUpperCase(),
                line: this.getLineNumber(content, match.index)
            });
        }
        
        return divisions;
    }

    extractCopyStatements(content) {
        const copies = [];
        const copyPattern = /COPY\s+([A-Za-z0-9-]+)(?:\s+OF\s+([A-Za-z0-9-]+))?\s*\./gi;
        let match;
        
        while ((match = copyPattern.exec(content)) !== null) {
            copies.push({
                copybook: match[1],
                library: match[2] || null,
                line: this.getLineNumber(content, match.index)
            });
        }
        
        return copies;
    }

    extractCallStatements(content) {
        const calls = [];
        const callPattern = /CALL\s+["']([A-Za-z0-9-]+)["']/gi;
        let match;
        
        while ((match = callPattern.exec(content)) !== null) {
            calls.push({
                program: match[1],
                line: this.getLineNumber(content, match.index),
                type: 'static'
            });
        }
        
        // Dynamic calls
        const dynCallPattern = /CALL\s+([A-Za-z0-9-]+)(?:\s+USING)?/gi;
        while ((match = dynCallPattern.exec(content)) !== null) {
            if (!match[0].includes('"') && !match[0].includes("'")) {
                calls.push({
                    program: match[1],
                    line: this.getLineNumber(content, match.index),
                    type: 'dynamic'
                });
            }
        }
        
        return calls;
    }

    extractPerformStatements(content) {
        const performs = [];
        const performPattern = /PERFORM\s+([A-Za-z0-9-]+)(?:\s+THRU\s+([A-Za-z0-9-]+))?/gi;
        let match;
        
        while ((match = performPattern.exec(content)) !== null) {
            performs.push({
                target: match[1],
                thru: match[2] || null,
                line: this.getLineNumber(content, match.index)
            });
        }
        
        return performs;
    }

    extractFileDescriptions(content) {
        const files = [];
        
        // SELECT statements
        const selectPattern = /SELECT\s+([A-Za-z0-9-]+)\s+ASSIGN\s+TO\s+([^\s.]+)/gi;
        let match;
        
        while ((match = selectPattern.exec(content)) !== null) {
            files.push({
                type: 'SELECT',
                fileName: match[1],
                assignTo: match[2],
                line: this.getLineNumber(content, match.index)
            });
        }
        
        // FD statements
        const fdPattern = /FD\s+([A-Za-z0-9-]+)/gi;
        while ((match = fdPattern.exec(content)) !== null) {
            const fdName = match[1];
            const recordPattern = new RegExp(`FD\\s+${fdName}[\\s\\S]*?(?:01|WORKING-STORAGE|PROCEDURE)`, 'i');
            const fdMatch = content.match(recordPattern);
            
            files.push({
                type: 'FD',
                fileName: fdName,
                line: this.getLineNumber(content, match.index),
                hasRecording: fdMatch ? fdMatch[0].includes('RECORDING') : false
            });
        }
        
        return files;
    }

    extractDataDefinitions(content) {
        const dataItems = [];
        const wsPattern = /WORKING-STORAGE\s+SECTION[\s\S]*?(?:PROCEDURE|LOCAL-STORAGE|LINKAGE|$)/i;
        const wsMatch = content.match(wsPattern);
        
        if (wsMatch) {
            const wsContent = wsMatch[0];
            const itemPattern = /^\s*(01|77)\s+([A-Za-z0-9-]+)/gm;
            let match;
            
            while ((match = itemPattern.exec(wsContent)) !== null) {
                dataItems.push({
                    level: match[1],
                    name: match[2],
                    section: 'WORKING-STORAGE'
                });
            }
        }
        
        // Linkage section
        const linkagePattern = /LINKAGE\s+SECTION[\s\S]*?(?:PROCEDURE|$)/i;
        const linkageMatch = content.match(linkagePattern);
        
        if (linkageMatch) {
            const linkageContent = linkageMatch[0];
            const itemPattern = /^\s*(01|77)\s+([A-Za-z0-9-]+)/gm;
            let match;
            
            while ((match = itemPattern.exec(linkageContent)) !== null) {
                dataItems.push({
                    level: match[1],
                    name: match[2],
                    section: 'LINKAGE'
                });
            }
        }
        
        return dataItems;
    }

    extractSections(content) {
        const sections = [];
        const sectionPattern = /^[\s]*([A-Za-z0-9-]+)\s+SECTION\s*\./gm;
        let match;
        
        while ((match = sectionPattern.exec(content)) !== null) {
            // Skip division sections
            if (!match[1].match(/WORKING-STORAGE|FILE|LINKAGE|LOCAL-STORAGE|CONFIGURATION|INPUT-OUTPUT/i)) {
                sections.push({
                    name: match[1],
                    line: this.getLineNumber(content, match.index)
                });
            }
        }
        
        return sections;
    }

    extractParagraphs(content) {
        const paragraphs = [];
        const paragraphPattern = /^[\s]*([A-Za-z0-9-]+)\s*\.\s*$/gm;
        let match;
        
        while ((match = paragraphPattern.exec(content)) !== null) {
            const name = match[1];
            // Filter out common non-paragraph matches
            if (!name.match(/^(PROGRAM-ID|AUTHOR|DATE-WRITTEN|ENVIRONMENT|DATA|PROCEDURE|WORKING-STORAGE|FILE|LINKAGE|END-PROGRAM)$/i)) {
                paragraphs.push({
                    name: name,
                    line: this.getLineNumber(content, match.index)
                });
            }
        }
        
        return paragraphs;
    }

    detectMainProgram(content) {
        // Simple heuristic: main programs often have certain patterns
        return content.match(/STOP\s+RUN/i) !== null ||
               content.match(/GOBACK/i) === null ||
               content.match(/PROCEDURE\s+DIVISION\s*\./i) !== null;
    }

    getLineNumber(content, index) {
        return content.substring(0, index).split('\n').length;
    }

    processDirectory(dirPath, outputDir) {
        const files = this.getAllCobolFiles(dirPath);
        this.fileStats.total = files.length;
        
        console.log(`Found ${files.length} COBOL files to parse`);
        
        files.forEach(file => {
            const structure = this.parseFile(file);
            if (structure) {
                // Create output filename
                const relativePath = path.relative(dirPath, file);
                const outputFileName = relativePath.replace(/[\/\\]/g, '_').replace(/\./g, '_') + '.json';
                const outputPath = path.join(outputDir, outputFileName);
                
                // Write individual file
                fs.writeFileSync(outputPath, JSON.stringify(structure, null, 2));
                this.results.push(structure);
            }
        });
        
        // Write summary
        const summary = {
            parseDate: new Date().toISOString(),
            statistics: this.fileStats,
            errors: this.errors,
            dependencies: this.analyzeDependencies(),
            fileInventory: this.results.map(r => ({
                file: r.filePath,
                programId: r.programId,
                lines: r.metadata.lines,
                divisions: r.divisions.length,
                copies: r.copyStatements.length,
                calls: r.callStatements.length
            }))
        };
        
        fs.writeFileSync(
            path.join(outputDir, '..', 'parser-summary.json'),
            JSON.stringify(summary, null, 2)
        );
        
        console.log(`\nParsing complete:
        - Total files: ${this.fileStats.total}
        - Successfully parsed: ${this.fileStats.parsed}
        - Errors: ${this.fileStats.errors}
        - Output directory: ${outputDir}`);
    }

    analyzeDependencies() {
        const deps = {
            programCalls: {},
            copybookUsage: {},
            callGraph: []
        };
        
        this.results.forEach(result => {
            // Track program calls
            if (result.callStatements.length > 0) {
                deps.programCalls[result.programId] = result.callStatements.map(c => c.program);
                result.callStatements.forEach(call => {
                    deps.callGraph.push({
                        from: result.programId,
                        to: call.program,
                        type: call.type
                    });
                });
            }
            
            // Track copybook usage
            if (result.copyStatements.length > 0) {
                deps.copybookUsage[result.programId] = result.copyStatements.map(c => c.copybook);
            }
        });
        
        return deps;
    }

    getAllCobolFiles(dirPath) {
        const files = [];
        const extensions = ['.cbl', '.cpy', '.cob', '.CPY'];
        
        function walk(dir) {
            const items = fs.readdirSync(dir);
            items.forEach(item => {
                const fullPath = path.join(dir, item);
                const stat = fs.statSync(fullPath);
                
                if (stat.isDirectory()) {
                    walk(fullPath);
                } else if (stat.isFile()) {
                    const ext = path.extname(fullPath);
                    if (extensions.includes(ext)) {
                        files.push(fullPath);
                    }
                }
            });
        }
        
        walk(dirPath);
        return files.sort();
    }
}

// Main execution
if (require.main === module) {
    const parser = new COBOLParser();
    const legacyDir = path.resolve(__dirname, '../../Legacy_App');
    const outputDir = path.join(__dirname, 'parsed-structures');
    
    if (!fs.existsSync(outputDir)) {
        fs.mkdirSync(outputDir, { recursive: true });
    }
    
    parser.processDirectory(legacyDir, outputDir);
}

module.exports = COBOLParser;