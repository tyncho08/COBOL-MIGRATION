const fs = require('fs');
const path = require('path');

class MetricsGenerator {
    constructor(parsedDir) {
        this.parsedDir = parsedDir;
        this.structures = [];
        this.metrics = {
            complexity: {},
            halstead: {},
            maintainability: {},
            duplication: {},
            deadCode: []
        };
    }

    loadData() {
        const structuresDir = path.join(this.parsedDir, 'parsed-structures');
        const files = fs.readdirSync(structuresDir).filter(f => f.endsWith('.json'));
        
        files.forEach(file => {
            const content = fs.readFileSync(path.join(structuresDir, file), 'utf-8');
            this.structures.push(JSON.parse(content));
        });
    }

    calculateAllMetrics() {
        console.log('Calculating metrics for all programs...');
        
        this.structures.forEach(struct => {
            const programId = struct.programId;
            
            // Cyclomatic Complexity
            this.metrics.complexity[programId] = this.calculateCyclomaticComplexity(struct);
            
            // Halstead Metrics
            this.metrics.halstead[programId] = this.calculateHalsteadMetrics(struct);
            
            // Maintainability Index
            this.metrics.maintainability[programId] = this.calculateMaintainabilityIndex(struct);
        });
        
        // Calculate duplication
        this.detectCodeDuplication();
        
        // Detect dead code
        this.detectDeadCode();
    }

    calculateCyclomaticComplexity(struct) {
        let complexity = 1; // Base complexity
        
        // Add complexity for control structures
        // Note: This is simplified - real implementation would parse actual statements
        complexity += struct.performStatements.length;
        complexity += struct.sections.length;
        complexity += struct.paragraphs.length / 10; // Estimate decision points
        
        // Estimate based on program size
        const estimatedDecisions = Math.floor(struct.metadata.lines / 50);
        complexity += estimatedDecisions;
        
        return Math.round(complexity);
    }

    calculateHalsteadMetrics(struct) {
        // Simplified Halstead metrics calculation
        // In reality, would need to tokenize and analyze the actual code
        
        const operators = struct.performStatements.length + 
                         struct.callStatements.length + 
                         struct.fileDescriptions.length * 3; // Estimate file operations
        
        const operands = struct.dataDefinitions.length + 
                        struct.copyStatements.length +
                        struct.paragraphs.length;
        
        const n1 = Math.max(1, Math.floor(operators * 0.7)); // Unique operators
        const n2 = Math.max(1, Math.floor(operands * 0.8));  // Unique operands
        const N1 = operators;  // Total operators
        const N2 = operands;   // Total operands
        
        const N = N1 + N2; // Program length
        const n = n1 + n2; // Program vocabulary
        
        const volume = N * Math.log2(n);
        const difficulty = (n1 / 2) * (N2 / n2);
        const effort = volume * difficulty;
        const time = effort / 18; // Time to implement in seconds
        const bugs = volume / 3000; // Estimated bugs
        
        return {
            vocabulary: n,
            length: N,
            volume: Math.round(volume),
            difficulty: Math.round(difficulty),
            effort: Math.round(effort),
            time: Math.round(time),
            bugs: Math.round(bugs * 100) / 100
        };
    }

    calculateMaintainabilityIndex(struct) {
        // Simplified Maintainability Index
        // MI = 171 - 5.2 * ln(HV) - 0.23 * CC - 16.2 * ln(LOC)
        
        const halstead = this.metrics.halstead[struct.programId];
        const complexity = this.metrics.complexity[struct.programId];
        const loc = struct.metadata.lines;
        
        let mi = 171;
        mi -= 5.2 * Math.log(halstead.volume);
        mi -= 0.23 * complexity;
        mi -= 16.2 * Math.log(loc);
        
        // Normalize to 0-100 scale
        mi = Math.max(0, Math.min(100, mi));
        
        return {
            index: Math.round(mi),
            rating: this.getMaintainabilityRating(mi),
            factors: {
                volume: halstead.volume,
                complexity: complexity,
                loc: loc
            }
        };
    }

    getMaintainabilityRating(index) {
        if (index > 85) return 'Excellent';
        if (index > 70) return 'Good';
        if (index > 50) return 'Fair';
        if (index > 30) return 'Poor';
        return 'Very Poor';
    }

    detectCodeDuplication() {
        console.log('Detecting code duplication...');
        
        // Simplified duplication detection based on similar structures
        const signatures = {};
        
        this.structures.forEach(struct => {
            // Create a simple signature based on structure
            const signature = `${struct.sections.length}-${struct.paragraphs.length}-${struct.callStatements.length}`;
            
            if (!signatures[signature]) {
                signatures[signature] = [];
            }
            signatures[signature].push(struct.programId);
        });
        
        // Find potential duplicates
        Object.entries(signatures).forEach(([sig, programs]) => {
            if (programs.length > 1) {
                programs.forEach(prog => {
                    this.metrics.duplication[prog] = {
                        signature: sig,
                        similarPrograms: programs.filter(p => p !== prog),
                        confidence: 'Low' // Simplified
                    };
                });
            }
        });
    }

    detectDeadCode() {
        console.log('Detecting dead code...');
        
        // Find programs that are never called
        const calledPrograms = new Set();
        
        this.structures.forEach(struct => {
            struct.callStatements.forEach(call => {
                calledPrograms.add(call.program);
            });
        });
        
        this.structures.forEach(struct => {
            if (!calledPrograms.has(struct.programId)) {
                // Check if it's a main program
                if (!struct.metadata.hasMainProgram && !struct.programId.match(/^(MAIN|MENU)/i)) {
                    this.metrics.deadCode.push({
                        program: struct.programId,
                        file: struct.filePath,
                        reason: 'Never called by other programs',
                        confidence: 'Medium'
                    });
                }
            }
        });
    }

    generateMetricsDashboard() {
        console.log('Generating metrics dashboard...');
        
        const html = `<!DOCTYPE html>
<html>
<head>
    <title>COBOL Metrics Dashboard</title>
    <script src="https://cdn.jsdelivr.net/npm/chart.js"></script>
    <style>
        body {
            font-family: Arial, sans-serif;
            margin: 0;
            padding: 0;
            background: #f5f5f5;
        }
        .header {
            background: #333;
            color: white;
            padding: 20px;
            text-align: center;
        }
        .container {
            max-width: 1400px;
            margin: 0 auto;
            padding: 20px;
        }
        .metrics-grid {
            display: grid;
            grid-template-columns: repeat(auto-fit, minmax(300px, 1fr));
            gap: 20px;
            margin: 20px 0;
        }
        .metric-card {
            background: white;
            border-radius: 8px;
            padding: 20px;
            box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        }
        .metric-value {
            font-size: 2.5em;
            font-weight: bold;
            color: #333;
        }
        .metric-label {
            color: #666;
            margin-top: 5px;
        }
        .chart-container {
            background: white;
            border-radius: 8px;
            padding: 20px;
            box-shadow: 0 2px 4px rgba(0,0,0,0.1);
            margin: 20px 0;
        }
        .table-container {
            background: white;
            border-radius: 8px;
            padding: 20px;
            box-shadow: 0 2px 4px rgba(0,0,0,0.1);
            margin: 20px 0;
            overflow-x: auto;
        }
        table {
            width: 100%;
            border-collapse: collapse;
        }
        th, td {
            text-align: left;
            padding: 10px;
            border-bottom: 1px solid #ddd;
        }
        th {
            background: #f0f0f0;
            font-weight: bold;
        }
        tr:hover {
            background: #f5f5f5;
        }
        .complexity-high { color: #d32f2f; }
        .complexity-medium { color: #f57c00; }
        .complexity-low { color: #388e3c; }
        .maintainability-excellent { color: #2e7d32; }
        .maintainability-good { color: #558b2f; }
        .maintainability-fair { color: #f57f17; }
        .maintainability-poor { color: #e65100; }
        .maintainability-very-poor { color: #b71c1c; }
    </style>
</head>
<body>
    <div class="header">
        <h1>COBOL System Metrics Dashboard</h1>
        <p>Generated: ${new Date().toISOString()}</p>
    </div>
    
    <div class="container">
        <div class="metrics-grid">
            <div class="metric-card">
                <div class="metric-value">${this.structures.length}</div>
                <div class="metric-label">Total Programs</div>
            </div>
            <div class="metric-card">
                <div class="metric-value">${this.calculateTotalLOC().toLocaleString()}</div>
                <div class="metric-label">Lines of Code</div>
            </div>
            <div class="metric-card">
                <div class="metric-value">${this.calculateAvgComplexity().toFixed(1)}</div>
                <div class="metric-label">Average Complexity</div>
            </div>
            <div class="metric-card">
                <div class="metric-value">${this.calculateAvgMaintainability().toFixed(1)}</div>
                <div class="metric-label">Average Maintainability</div>
            </div>
        </div>

        <div class="chart-container">
            <h2>Complexity Distribution</h2>
            <canvas id="complexityChart" height="100"></canvas>
        </div>

        <div class="chart-container">
            <h2>Maintainability Index Distribution</h2>
            <canvas id="maintainabilityChart" height="100"></canvas>
        </div>

        <div class="table-container">
            <h2>Programs Requiring Attention</h2>
            <table>
                <thead>
                    <tr>
                        <th>Program</th>
                        <th>Complexity</th>
                        <th>Maintainability</th>
                        <th>LOC</th>
                        <th>Halstead Volume</th>
                        <th>Estimated Bugs</th>
                    </tr>
                </thead>
                <tbody>
                    ${this.getHighRiskPrograms()}
                </tbody>
            </table>
        </div>

        <div class="table-container">
            <h2>Potential Dead Code</h2>
            <table>
                <thead>
                    <tr>
                        <th>Program</th>
                        <th>File</th>
                        <th>Reason</th>
                        <th>Confidence</th>
                    </tr>
                </thead>
                <tbody>
                    ${this.getDeadCodeRows()}
                </tbody>
            </table>
        </div>

        <div class="table-container">
            <h2>Potential Duplicates</h2>
            <table>
                <thead>
                    <tr>
                        <th>Program</th>
                        <th>Similar To</th>
                        <th>Signature</th>
                        <th>Confidence</th>
                    </tr>
                </thead>
                <tbody>
                    ${this.getDuplicationRows()}
                </tbody>
            </table>
        </div>
    </div>

    <script>
        // Complexity Chart
        const complexityCtx = document.getElementById('complexityChart').getContext('2d');
        ${this.generateComplexityChartData()}

        // Maintainability Chart
        const maintainabilityCtx = document.getElementById('maintainabilityChart').getContext('2d');
        ${this.generateMaintainabilityChartData()}
    </script>
</body>
</html>`;

        fs.writeFileSync(
            path.join(this.parsedDir, 'parser_analysis/dashboard/index.html'),
            html
        );
    }

    generateComplexityMetrics() {
        const complexityData = Object.entries(this.metrics.complexity).map(([prog, complexity]) => ({
            program: prog,
            complexity: complexity,
            level: this.getComplexityLevel(complexity)
        }));

        fs.writeFileSync(
            path.join(this.parsedDir, 'parser_analysis/dashboard/complexity-metrics.json'),
            JSON.stringify(complexityData, null, 2)
        );
    }

    generateMaintainabilityMetrics() {
        const maintainabilityData = Object.entries(this.metrics.maintainability).map(([prog, data]) => ({
            program: prog,
            index: data.index,
            rating: data.rating,
            factors: data.factors
        }));

        fs.writeFileSync(
            path.join(this.parsedDir, 'parser_analysis/dashboard/maintainability-index.json'),
            JSON.stringify(maintainabilityData, null, 2)
        );
    }

    getComplexityLevel(complexity) {
        if (complexity <= 10) return 'Low';
        if (complexity <= 20) return 'Medium';
        return 'High';
    }

    calculateTotalLOC() {
        return this.structures.reduce((sum, s) => sum + s.metadata.lines, 0);
    }

    calculateAvgComplexity() {
        const values = Object.values(this.metrics.complexity);
        return values.reduce((sum, val) => sum + val, 0) / values.length;
    }

    calculateAvgMaintainability() {
        const values = Object.values(this.metrics.maintainability);
        return values.reduce((sum, val) => sum + val.index, 0) / values.length;
    }

    getHighRiskPrograms() {
        return Object.entries(this.metrics.complexity)
            .map(([prog, complexity]) => ({
                program: prog,
                complexity: complexity,
                maintainability: this.metrics.maintainability[prog],
                halstead: this.metrics.halstead[prog],
                struct: this.structures.find(s => s.programId === prog)
            }))
            .filter(data => data.complexity > 20 || data.maintainability.index < 50)
            .sort((a, b) => b.complexity - a.complexity)
            .slice(0, 20)
            .map(data => `
                <tr>
                    <td><strong>${data.program}</strong></td>
                    <td class="complexity-${this.getComplexityLevel(data.complexity).toLowerCase()}">
                        ${data.complexity}
                    </td>
                    <td class="maintainability-${data.maintainability.rating.toLowerCase().replace(' ', '-')}">
                        ${data.maintainability.index} (${data.maintainability.rating})
                    </td>
                    <td>${data.struct?.metadata.lines || 'N/A'}</td>
                    <td>${data.halstead.volume}</td>
                    <td>${data.halstead.bugs}</td>
                </tr>
            `)
            .join('');
    }

    getDeadCodeRows() {
        return this.metrics.deadCode
            .slice(0, 20)
            .map(dead => `
                <tr>
                    <td><strong>${dead.program}</strong></td>
                    <td>${dead.file}</td>
                    <td>${dead.reason}</td>
                    <td>${dead.confidence}</td>
                </tr>
            `)
            .join('');
    }

    getDuplicationRows() {
        return Object.entries(this.metrics.duplication)
            .slice(0, 20)
            .map(([prog, dup]) => `
                <tr>
                    <td><strong>${prog}</strong></td>
                    <td>${dup.similarPrograms.slice(0, 3).join(', ')}</td>
                    <td>${dup.signature}</td>
                    <td>${dup.confidence}</td>
                </tr>
            `)
            .join('');
    }

    generateComplexityChartData() {
        const distribution = { Low: 0, Medium: 0, High: 0 };
        
        Object.values(this.metrics.complexity).forEach(complexity => {
            const level = this.getComplexityLevel(complexity);
            distribution[level]++;
        });

        return `
        new Chart(complexityCtx, {
            type: 'doughnut',
            data: {
                labels: ['Low (≤10)', 'Medium (11-20)', 'High (>20)'],
                datasets: [{
                    data: [${distribution.Low}, ${distribution.Medium}, ${distribution.High}],
                    backgroundColor: ['#4caf50', '#ff9800', '#f44336']
                }]
            },
            options: {
                responsive: true,
                maintainAspectRatio: false
            }
        });`;
    }

    generateMaintainabilityChartData() {
        const distribution = {
            'Excellent': 0,
            'Good': 0,
            'Fair': 0,
            'Poor': 0,
            'Very Poor': 0
        };
        
        Object.values(this.metrics.maintainability).forEach(data => {
            distribution[data.rating]++;
        });

        return `
        new Chart(maintainabilityCtx, {
            type: 'bar',
            data: {
                labels: ${JSON.stringify(Object.keys(distribution))},
                datasets: [{
                    label: 'Number of Programs',
                    data: ${JSON.stringify(Object.values(distribution))},
                    backgroundColor: ['#2e7d32', '#558b2f', '#f57f17', '#e65100', '#b71c1c']
                }]
            },
            options: {
                responsive: true,
                maintainAspectRatio: false,
                scales: {
                    y: {
                        beginAtZero: true
                    }
                }
            }
        });`;
    }

    generateAll() {
        console.log('Generating all metrics...');
        this.loadData();
        this.calculateAllMetrics();
        this.generateMetricsDashboard();
        this.generateComplexityMetrics();
        this.generateMaintainabilityMetrics();
        console.log('Metrics generation complete!');
    }
}

// Main execution
if (require.main === module) {
    const generator = new MetricsGenerator(path.resolve(__dirname));
    generator.generateAll();
}

module.exports = MetricsGenerator;