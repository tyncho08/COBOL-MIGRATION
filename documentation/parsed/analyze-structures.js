const fs = require('fs');
const path = require('path');

class StructureAnalyzer {
    constructor(parsedDir) {
        this.parsedDir = parsedDir;
        this.structures = [];
        this.analysis = {
            programs: {},
            copybooks: {},
            files: {},
            dataFlows: [],
            complexity: {},
            patterns: {}
        };
    }

    loadParsedStructures() {
        const structuresDir = path.join(this.parsedDir, 'parsed-structures');
        const files = fs.readdirSync(structuresDir).filter(f => f.endsWith('.json'));
        
        console.log(`Loading ${files.length} parsed structures...`);
        
        files.forEach(file => {
            try {
                const content = fs.readFileSync(path.join(structuresDir, file), 'utf-8');
                const structure = JSON.parse(content);
                this.structures.push(structure);
            } catch (error) {
                console.error(`Error loading ${file}: ${error.message}`);
            }
        });
        
        console.log(`Loaded ${this.structures.length} structures successfully`);
    }

    analyzeAll() {
        console.log('\nAnalyzing COBOL structures...');
        
        this.analyzePrograms();
        this.analyzeCopybooks();
        this.analyzeFiles();
        this.analyzeDataFlow();
        this.analyzeComplexity();
        this.identifyPatterns();
        
        this.generateReport();
    }

    analyzePrograms() {
        this.structures.forEach(struct => {
            const programId = struct.programId;
            
            this.analysis.programs[programId] = {
                file: struct.filePath,
                type: this.determineProgramType(struct),
                divisions: struct.divisions.map(d => d.name),
                dependencies: {
                    calls: struct.callStatements.map(c => c.program),
                    copies: struct.copyStatements.map(c => c.copybook),
                    files: struct.fileDescriptions.map(f => f.fileName)
                },
                metrics: {
                    lines: struct.metadata.lines,
                    sections: struct.sections.length,
                    paragraphs: struct.paragraphs.length,
                    performs: struct.performStatements.length,
                    complexity: this.calculateComplexity(struct)
                }
            };
        });
    }

    analyzeCopybooks() {
        // Collect all copybook references
        const copybookUsage = {};
        
        this.structures.forEach(struct => {
            struct.copyStatements.forEach(copy => {
                if (!copybookUsage[copy.copybook]) {
                    copybookUsage[copy.copybook] = {
                        usedBy: [],
                        frequency: 0
                    };
                }
                copybookUsage[copy.copybook].usedBy.push(struct.programId);
                copybookUsage[copy.copybook].frequency++;
            });
        });
        
        // Find actual copybook files
        this.structures.forEach(struct => {
            if (struct.filePath.includes('/copybooks/') || 
                struct.filePath.endsWith('.cpy') || 
                struct.filePath.endsWith('.CPY')) {
                
                const name = path.basename(struct.filePath, path.extname(struct.filePath));
                this.analysis.copybooks[name] = {
                    file: struct.filePath,
                    usage: copybookUsage[name] || { usedBy: [], frequency: 0 },
                    dataItems: struct.dataDefinitions.filter(d => d.level === '01' || d.level === '77'),
                    type: this.determineCopybookType(struct)
                };
            }
        });
    }

    analyzeFiles() {
        const fileUsage = {};
        
        this.structures.forEach(struct => {
            struct.fileDescriptions.forEach(file => {
                const key = file.fileName;
                if (!fileUsage[key]) {
                    fileUsage[key] = {
                        programs: [],
                        operations: [],
                        type: file.type
                    };
                }
                fileUsage[key].programs.push(struct.programId);
                
                // Detect file operations
                const operations = this.detectFileOperations(struct, file.fileName);
                fileUsage[key].operations.push(...operations);
            });
        });
        
        this.analysis.files = fileUsage;
    }

    analyzeDataFlow() {
        const flows = [];
        
        this.structures.forEach(struct => {
            // Analyze CALL chains
            struct.callStatements.forEach(call => {
                flows.push({
                    type: 'CALL',
                    from: struct.programId,
                    to: call.program,
                    line: call.line
                });
            });
            
            // Analyze file flows (simplified)
            struct.fileDescriptions.forEach(file => {
                const operations = this.detectFileOperations(struct, file.fileName);
                operations.forEach(op => {
                    flows.push({
                        type: 'FILE',
                        program: struct.programId,
                        file: file.fileName,
                        operation: op,
                        line: file.line
                    });
                });
            });
        });
        
        this.analysis.dataFlows = flows;
    }

    analyzeComplexity() {
        this.structures.forEach(struct => {
            const programId = struct.programId;
            
            this.analysis.complexity[programId] = {
                cyclomaticComplexity: this.calculateComplexity(struct),
                nestingDepth: this.calculateNestingDepth(struct),
                cohesion: this.calculateCohesion(struct),
                coupling: this.calculateCoupling(struct)
            };
        });
    }

    identifyPatterns() {
        this.analysis.patterns = {
            mainPrograms: [],
            subPrograms: [],
            batchPrograms: [],
            onlinePrograms: [],
            utilityPrograms: [],
            dataModules: []
        };
        
        this.structures.forEach(struct => {
            const type = this.determineProgramType(struct);
            const programId = struct.programId;
            
            switch(type) {
                case 'MAIN':
                    this.analysis.patterns.mainPrograms.push(programId);
                    break;
                case 'SUBPROGRAM':
                    this.analysis.patterns.subPrograms.push(programId);
                    break;
                case 'BATCH':
                    this.analysis.patterns.batchPrograms.push(programId);
                    break;
                case 'ONLINE':
                    this.analysis.patterns.onlinePrograms.push(programId);
                    break;
                case 'UTILITY':
                    this.analysis.patterns.utilityPrograms.push(programId);
                    break;
                case 'DATA':
                    this.analysis.patterns.dataModules.push(programId);
                    break;
            }
        });
    }

    determineProgramType(struct) {
        // Main program indicators
        if (struct.metadata.hasMainProgram) {
            if (struct.fileDescriptions.length > 2) return 'BATCH';
            if (struct.programId.includes('MENU') || struct.programId.includes('SCREEN')) return 'ONLINE';
            return 'MAIN';
        }
        
        // Subprogram indicators
        if (struct.divisions.includes('LINKAGE')) return 'SUBPROGRAM';
        
        // Utility indicators
        if (struct.programId.match(/^(SYS|UTL|XL)/i)) return 'UTILITY';
        
        // Data module indicators
        if (struct.copyStatements.length > 5 && struct.performStatements.length < 5) return 'DATA';
        
        return 'UNKNOWN';
    }

    determineCopybookType(struct) {
        const content = struct.filePath.toLowerCase();
        
        if (content.includes('ws')) return 'WORKING-STORAGE';
        if (content.includes('fd')) return 'FILE-DESCRIPTION';
        if (content.includes('sel')) return 'FILE-SELECT';
        if (content.includes('proc')) return 'PROCEDURE';
        
        return 'GENERAL';
    }

    detectFileOperations(struct, fileName) {
        // This is a simplified operation detection
        // In a real parser, you'd analyze the actual COBOL statements
        const operations = [];
        
        if (struct.filePath.includes('LD') || struct.filePath.includes('load')) {
            operations.push('READ');
        }
        if (struct.filePath.includes('UNL') || struct.filePath.includes('unload')) {
            operations.push('WRITE');
        }
        if (struct.filePath.includes('MT') || struct.filePath.includes('maint')) {
            operations.push('UPDATE');
        }
        
        return operations.length > 0 ? operations : ['UNKNOWN'];
    }

    calculateComplexity(struct) {
        // Simplified cyclomatic complexity
        let complexity = 1;
        
        // Add complexity for control structures (approximated)
        complexity += struct.performStatements.length;
        complexity += struct.sections.length;
        
        // Add for decision points (would need actual IF/EVALUATE parsing)
        complexity += Math.floor(struct.metadata.lines / 50);
        
        return complexity;
    }

    calculateNestingDepth(struct) {
        // Simplified nesting depth calculation
        return Math.min(5, Math.floor(struct.performStatements.length / 3));
    }

    calculateCohesion(struct) {
        // Simplified cohesion metric
        if (struct.sections.length === 0) return 1.0;
        
        const avgParagraphsPerSection = struct.paragraphs.length / struct.sections.length;
        return Math.min(1.0, avgParagraphsPerSection / 10);
    }

    calculateCoupling(struct) {
        // Simplified coupling metric based on external dependencies
        const totalDeps = struct.callStatements.length + 
                         struct.copyStatements.length + 
                         struct.fileDescriptions.length;
        
        return Math.min(1.0, totalDeps / 20);
    }

    generateReport() {
        const report = {
            summary: {
                totalPrograms: this.structures.length,
                programTypes: Object.keys(this.analysis.patterns).reduce((acc, key) => {
                    acc[key] = this.analysis.patterns[key].length;
                    return acc;
                }, {}),
                totalCopybooks: Object.keys(this.analysis.copybooks).length,
                totalFiles: Object.keys(this.analysis.files).length,
                analysisDate: new Date().toISOString()
            },
            programs: this.analysis.programs,
            copybooks: this.analysis.copybooks,
            files: this.analysis.files,
            dataFlows: this.analysis.dataFlows,
            complexity: this.analysis.complexity,
            patterns: this.analysis.patterns,
            recommendations: this.generateRecommendations()
        };
        
        const outputPath = path.join(this.parsedDir, 'structure-analysis.json');
        fs.writeFileSync(outputPath, JSON.stringify(report, null, 2));
        
        console.log(`\nAnalysis complete. Report written to: ${outputPath}`);
        this.printSummary(report);
    }

    generateRecommendations() {
        const recommendations = [];
        
        // High complexity programs
        Object.entries(this.analysis.complexity).forEach(([program, metrics]) => {
            if (metrics.cyclomaticComplexity > 20) {
                recommendations.push({
                    type: 'REFACTOR',
                    program: program,
                    reason: 'High cyclomatic complexity',
                    priority: 'HIGH'
                });
            }
        });
        
        // Highly coupled programs
        Object.entries(this.analysis.programs).forEach(([program, info]) => {
            if (info.dependencies.calls.length > 10) {
                recommendations.push({
                    type: 'DECOUPLE',
                    program: program,
                    reason: 'Too many program dependencies',
                    priority: 'MEDIUM'
                });
            }
        });
        
        // Unused copybooks
        Object.entries(this.analysis.copybooks).forEach(([copybook, info]) => {
            if (info.usage.frequency === 0) {
                recommendations.push({
                    type: 'REMOVE',
                    copybook: copybook,
                    reason: 'Unused copybook',
                    priority: 'LOW'
                });
            }
        });
        
        return recommendations;
    }

    printSummary(report) {
        console.log('\n=== COBOL Structure Analysis Summary ===');
        console.log(`Total Programs: ${report.summary.totalPrograms}`);
        console.log(`Total Copybooks: ${report.summary.totalCopybooks}`);
        console.log(`Total Files: ${report.summary.totalFiles}`);
        
        console.log('\nProgram Types:');
        Object.entries(report.summary.programTypes).forEach(([type, count]) => {
            console.log(`  ${type}: ${count}`);
        });
        
        console.log(`\nRecommendations: ${report.recommendations.length}`);
        console.log('  High Priority:', report.recommendations.filter(r => r.priority === 'HIGH').length);
        console.log('  Medium Priority:', report.recommendations.filter(r => r.priority === 'MEDIUM').length);
        console.log('  Low Priority:', report.recommendations.filter(r => r.priority === 'LOW').length);
    }
}

// Main execution
if (require.main === module) {
    const analyzer = new StructureAnalyzer(path.resolve(__dirname));
    analyzer.loadParsedStructures();
    analyzer.analyzeAll();
}

module.exports = StructureAnalyzer;