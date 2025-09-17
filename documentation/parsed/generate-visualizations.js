const fs = require('fs');
const path = require('path');

class VisualizationGenerator {
    constructor(parsedDir) {
        this.parsedDir = parsedDir;
        this.structures = [];
        this.analysis = null;
    }

    loadData() {
        // Load parsed structures
        const structuresDir = path.join(this.parsedDir, 'parsed-structures');
        const files = fs.readdirSync(structuresDir).filter(f => f.endsWith('.json'));
        
        files.forEach(file => {
            const content = fs.readFileSync(path.join(structuresDir, file), 'utf-8');
            this.structures.push(JSON.parse(content));
        });

        // Load structure analysis
        const analysisPath = path.join(this.parsedDir, 'structure-analysis.json');
        if (fs.existsSync(analysisPath)) {
            this.analysis = JSON.parse(fs.readFileSync(analysisPath, 'utf-8'));
        }
    }

    generateCallGraph() {
        console.log('Generating call graph...');
        const nodes = new Map();
        const edges = [];

        // Create nodes for all programs
        this.structures.forEach(struct => {
            nodes.set(struct.programId, {
                id: struct.programId,
                label: struct.programId,
                group: this.getNodeGroup(struct),
                size: Math.log(struct.metadata.lines) * 5,
                file: struct.filePath
            });
        });

        // Create edges from CALL statements
        this.structures.forEach(struct => {
            struct.callStatements.forEach(call => {
                edges.push({
                    from: struct.programId,
                    to: call.program,
                    type: call.type,
                    arrows: 'to'
                });
            });
        });

        const html = this.generateCallGraphHTML(Array.from(nodes.values()), edges);
        fs.writeFileSync(
            path.join(this.parsedDir, 'parser_analysis/visualizations/call-graph.html'),
            html
        );
    }

    generateProcedureFlow() {
        console.log('Generating procedure flow diagrams...');
        
        // Generate top-level index
        let indexHtml = `<!DOCTYPE html>
<html>
<head>
    <title>COBOL Procedure Flow Diagrams</title>
    <style>
        body { font-family: Arial, sans-serif; margin: 20px; }
        .program-list { list-style-type: none; padding: 0; }
        .program-item { 
            margin: 10px 0; 
            padding: 10px;
            background: #f0f0f0;
            border-radius: 5px;
        }
        .program-item a {
            text-decoration: none;
            color: #0066cc;
            font-weight: bold;
        }
        .metrics {
            margin-top: 5px;
            font-size: 0.9em;
            color: #666;
        }
    </style>
</head>
<body>
    <h1>COBOL Procedure Flow Diagrams</h1>
    <p>Click on a program to view its internal procedure flow.</p>
    <ul class="program-list">`;

        // Select representative programs to visualize
        const programsToVisualize = this.structures
            .filter(s => s.sections.length > 0 || s.performStatements.length > 0)
            .slice(0, 50); // Top 50 programs with sections/performs

        programsToVisualize.forEach(struct => {
            const flowData = this.generateProgramFlow(struct);
            const programHtml = this.generateFlowDiagramHTML(struct, flowData);
            const filename = `flow-${struct.programId}.html`;
            
            fs.writeFileSync(
                path.join(this.parsedDir, 'parser_analysis/visualizations', filename),
                programHtml
            );

            indexHtml += `
        <li class="program-item">
            <a href="${filename}">${struct.programId}</a>
            <div class="metrics">
                File: ${struct.filePath}<br>
                Sections: ${struct.sections.length}, 
                Paragraphs: ${struct.paragraphs.length}, 
                PERFORM calls: ${struct.performStatements.length}
            </div>
        </li>`;
        });

        indexHtml += `
    </ul>
</body>
</html>`;

        fs.writeFileSync(
            path.join(this.parsedDir, 'parser_analysis/visualizations/procedure-flow.html'),
            indexHtml
        );
    }

    generateCopybookUsage() {
        console.log('Generating copybook usage visualization...');
        
        const copybookUsage = {};
        
        // Collect copybook usage data
        this.structures.forEach(struct => {
            struct.copyStatements.forEach(copy => {
                if (!copybookUsage[copy.copybook]) {
                    copybookUsage[copy.copybook] = {
                        name: copy.copybook,
                        usedBy: [],
                        frequency: 0
                    };
                }
                copybookUsage[copy.copybook].usedBy.push(struct.programId);
                copybookUsage[copy.copybook].frequency++;
            });
        });

        const html = this.generateCopybookUsageHTML(copybookUsage);
        fs.writeFileSync(
            path.join(this.parsedDir, 'parser_analysis/visualizations/copybook-usage.html'),
            html
        );
    }

    generateCallGraphHTML(nodes, edges) {
        return `<!DOCTYPE html>
<html>
<head>
    <title>COBOL Call Graph</title>
    <script type="text/javascript" src="https://unpkg.com/vis-network/standalone/umd/vis-network.min.js"></script>
    <style type="text/css">
        body { margin: 0; padding: 0; font-family: Arial, sans-serif; }
        #mynetwork { 
            width: 100%; 
            height: calc(100vh - 100px); 
            border: 1px solid #ccc; 
        }
        #controls { 
            padding: 10px; 
            background: #f0f0f0; 
            height: 80px;
        }
        #info { 
            position: absolute; 
            right: 10px; 
            top: 110px; 
            background: white; 
            padding: 10px;
            border: 1px solid #ccc;
            max-width: 300px;
            display: none;
        }
        .legend {
            position: absolute;
            left: 10px;
            top: 110px;
            background: white;
            padding: 10px;
            border: 1px solid #ccc;
        }
        .legend-item {
            margin: 5px 0;
        }
        .color-box {
            display: inline-block;
            width: 20px;
            height: 20px;
            margin-right: 5px;
            vertical-align: middle;
        }
    </style>
</head>
<body>
    <div id="controls">
        <h2>COBOL Program Call Graph</h2>
        <button onclick="network.fit()">Fit to Screen</button>
        <label>
            <input type="checkbox" id="physics" checked onchange="togglePhysics()"> 
            Enable Physics
        </label>
        <span style="margin-left: 20px;">Nodes: ${nodes.length}, Edges: ${edges.length}</span>
    </div>
    <div class="legend">
        <h4>Legend</h4>
        <div class="legend-item">
            <span class="color-box" style="background: #97C2FC;"></span>Main Programs
        </div>
        <div class="legend-item">
            <span class="color-box" style="background: #FB7E81;"></span>Batch Programs
        </div>
        <div class="legend-item">
            <span class="color-box" style="background: #FFA500;"></span>Utilities
        </div>
        <div class="legend-item">
            <span class="color-box" style="background: #C5C5C5;"></span>Unknown
        </div>
    </div>
    <div id="info"></div>
    <div id="mynetwork"></div>

    <script type="text/javascript">
        // Create nodes and edges
        var nodes = new vis.DataSet(${JSON.stringify(nodes)});
        var edges = new vis.DataSet(${JSON.stringify(edges)});

        // Create network
        var container = document.getElementById('mynetwork');
        var data = {
            nodes: nodes,
            edges: edges
        };
        
        var options = {
            nodes: {
                shape: 'dot',
                font: {
                    size: 12,
                    face: 'Arial'
                }
            },
            edges: {
                arrows: {
                    to: { enabled: true, scaleFactor: 0.5 }
                },
                smooth: {
                    type: 'continuous'
                }
            },
            physics: {
                forceAtlas2Based: {
                    gravitationalConstant: -50,
                    centralGravity: 0.005,
                    springLength: 200,
                    springConstant: 0.08
                },
                solver: 'forceAtlas2Based',
                maxVelocity: 50,
                minVelocity: 0.1
            },
            groups: {
                'main': { color: '#97C2FC' },
                'batch': { color: '#FB7E81' },
                'utility': { color: '#FFA500' },
                'unknown': { color: '#C5C5C5' }
            }
        };
        
        var network = new vis.Network(container, data, options);
        
        // Handle node clicks
        network.on("click", function(params) {
            if (params.nodes.length > 0) {
                var nodeId = params.nodes[0];
                var node = nodes.get(nodeId);
                var info = document.getElementById('info');
                
                var callsTo = edges.get({
                    filter: function(edge) { return edge.from === nodeId; }
                }).map(e => e.to);
                
                var calledBy = edges.get({
                    filter: function(edge) { return edge.to === nodeId; }
                }).map(e => e.from);
                
                info.innerHTML = '<h3>' + node.label + '</h3>' +
                    '<p><strong>File:</strong> ' + node.file + '</p>' +
                    '<p><strong>Group:</strong> ' + node.group + '</p>' +
                    '<p><strong>Calls to:</strong> ' + 
                    (callsTo.length > 0 ? callsTo.join(', ') : 'None') + '</p>' +
                    '<p><strong>Called by:</strong> ' + 
                    (calledBy.length > 0 ? calledBy.join(', ') : 'None') + '</p>';
                
                info.style.display = 'block';
            }
        });
        
        function togglePhysics() {
            var physics = document.getElementById('physics').checked;
            network.setOptions({ physics: { enabled: physics } });
        }
    </script>
</body>
</html>`;
    }

    generateFlowDiagramHTML(struct, flowData) {
        return `<!DOCTYPE html>
<html>
<head>
    <title>Procedure Flow: ${struct.programId}</title>
    <script type="text/javascript" src="https://unpkg.com/vis-network/standalone/umd/vis-network.min.js"></script>
    <style type="text/css">
        body { margin: 0; padding: 0; font-family: Arial, sans-serif; }
        #mynetwork { 
            width: 100%; 
            height: calc(100vh - 100px); 
            border: 1px solid #ccc; 
        }
        #header { 
            padding: 10px; 
            background: #f0f0f0; 
            height: 80px;
        }
        .back-link {
            float: right;
            margin-top: 20px;
        }
    </style>
</head>
<body>
    <div id="header">
        <h2>Procedure Flow: ${struct.programId}</h2>
        <p>File: ${struct.filePath}</p>
        <a href="procedure-flow.html" class="back-link">← Back to Index</a>
    </div>
    <div id="mynetwork"></div>

    <script type="text/javascript">
        var nodes = new vis.DataSet(${JSON.stringify(flowData.nodes)});
        var edges = new vis.DataSet(${JSON.stringify(flowData.edges)});

        var container = document.getElementById('mynetwork');
        var data = {
            nodes: nodes,
            edges: edges
        };
        
        var options = {
            layout: {
                hierarchical: {
                    direction: 'UD',
                    sortMethod: 'directed'
                }
            },
            nodes: {
                shape: 'box',
                font: {
                    face: 'monospace',
                    align: 'left'
                }
            },
            edges: {
                arrows: {
                    to: { enabled: true, scaleFactor: 1 }
                }
            },
            physics: false
        };
        
        var network = new vis.Network(container, data, options);
    </script>
</body>
</html>`;
    }

    generateCopybookUsageHTML(copybookUsage) {
        const sortedCopybooks = Object.values(copybookUsage)
            .sort((a, b) => b.frequency - a.frequency);

        return `<!DOCTYPE html>
<html>
<head>
    <title>COBOL Copybook Usage Analysis</title>
    <script src="https://cdn.jsdelivr.net/npm/chart.js"></script>
    <style>
        body { 
            font-family: Arial, sans-serif; 
            margin: 20px;
            background: #f5f5f5;
        }
        .container {
            max-width: 1200px;
            margin: 0 auto;
        }
        h1 { color: #333; }
        .chart-container {
            background: white;
            padding: 20px;
            border-radius: 8px;
            margin: 20px 0;
            box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        }
        .copybook-list {
            background: white;
            padding: 20px;
            border-radius: 8px;
            box-shadow: 0 2px 4px rgba(0,0,0,0.1);
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
        .usage-bar {
            background: #4CAF50;
            height: 20px;
            border-radius: 3px;
            display: inline-block;
        }
        .expand-btn {
            cursor: pointer;
            color: #0066cc;
            text-decoration: underline;
        }
        .users-list {
            display: none;
            margin-top: 5px;
            font-size: 0.9em;
            color: #666;
        }
    </style>
</head>
<body>
    <div class="container">
        <h1>COBOL Copybook Usage Analysis</h1>
        
        <div class="chart-container">
            <h2>Top 20 Most Used Copybooks</h2>
            <canvas id="usageChart" width="400" height="200"></canvas>
        </div>

        <div class="copybook-list">
            <h2>Complete Copybook List</h2>
            <table>
                <thead>
                    <tr>
                        <th>Copybook</th>
                        <th>Usage Count</th>
                        <th>Usage Visualization</th>
                        <th>Used By</th>
                    </tr>
                </thead>
                <tbody>
                    ${sortedCopybooks.map((cb, index) => `
                    <tr>
                        <td><strong>${cb.name}</strong></td>
                        <td>${cb.frequency}</td>
                        <td>
                            <span class="usage-bar" style="width: ${(cb.frequency / sortedCopybooks[0].frequency) * 200}px"></span>
                        </td>
                        <td>
                            <span class="expand-btn" onclick="toggleUsers('users-${index}')">
                                Show ${cb.usedBy.length} programs
                            </span>
                            <div id="users-${index}" class="users-list">
                                ${cb.usedBy.join(', ')}
                            </div>
                        </td>
                    </tr>
                    `).join('')}
                </tbody>
            </table>
        </div>
    </div>

    <script>
        // Chart.js configuration
        const ctx = document.getElementById('usageChart').getContext('2d');
        const chartData = ${JSON.stringify(sortedCopybooks.slice(0, 20))};
        
        new Chart(ctx, {
            type: 'bar',
            data: {
                labels: chartData.map(d => d.name),
                datasets: [{
                    label: 'Usage Count',
                    data: chartData.map(d => d.frequency),
                    backgroundColor: 'rgba(54, 162, 235, 0.6)',
                    borderColor: 'rgba(54, 162, 235, 1)',
                    borderWidth: 1
                }]
            },
            options: {
                scales: {
                    y: {
                        beginAtZero: true
                    }
                },
                responsive: true,
                maintainAspectRatio: false
            }
        });

        function toggleUsers(id) {
            const elem = document.getElementById(id);
            elem.style.display = elem.style.display === 'none' ? 'block' : 'none';
        }
    </script>
</body>
</html>`;
    }

    generateProgramFlow(struct) {
        const nodes = [];
        const edges = [];
        let nodeId = 0;

        // Add entry point
        nodes.push({
            id: nodeId++,
            label: 'PROCEDURE DIVISION',
            level: 0,
            color: '#90EE90'
        });

        // Add sections
        struct.sections.forEach(section => {
            const sectionId = nodeId++;
            nodes.push({
                id: sectionId,
                label: section.name + ' SECTION',
                level: 1,
                color: '#87CEEB'
            });
            edges.push({
                from: 0,
                to: sectionId
            });
        });

        // Add paragraphs and PERFORM relationships
        const paragraphMap = {};
        struct.paragraphs.forEach(para => {
            const paraId = nodeId++;
            paragraphMap[para.name] = paraId;
            nodes.push({
                id: paraId,
                label: para.name,
                level: 2,
                color: '#FFD700'
            });
        });

        // Add PERFORM edges
        struct.performStatements.forEach(perform => {
            if (paragraphMap[perform.target]) {
                // Find which paragraph this PERFORM is likely in (simplified)
                const sourceId = nodes.length > 1 ? 1 : 0;
                edges.push({
                    from: sourceId,
                    to: paragraphMap[perform.target],
                    label: 'PERFORM',
                    dashes: true
                });
            }
        });

        return { nodes, edges };
    }

    getNodeGroup(struct) {
        if (struct.filePath.includes('batch') || struct.programId.includes('BATCH')) {
            return 'batch';
        }
        if (struct.programId.match(/^(SYS|UTL|XL)/i)) {
            return 'utility';
        }
        if (struct.metadata.hasMainProgram) {
            return 'main';
        }
        return 'unknown';
    }

    generateAll() {
        console.log('Generating all visualizations...');
        this.loadData();
        this.generateCallGraph();
        this.generateProcedureFlow();
        this.generateCopybookUsage();
        console.log('Visualization generation complete!');
    }
}

// Main execution
if (require.main === module) {
    const generator = new VisualizationGenerator(path.resolve(__dirname));
    generator.generateAll();
}

module.exports = VisualizationGenerator;