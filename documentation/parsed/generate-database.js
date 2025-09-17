const fs = require('fs');
const path = require('path');

class DatabaseGenerator {
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

        // Load analysis if exists
        const analysisPath = path.join(this.parsedDir, 'structure-analysis.json');
        if (fs.existsSync(analysisPath)) {
            this.analysis = JSON.parse(fs.readFileSync(analysisPath, 'utf-8'));
        }
    }

    generateDatabaseSchema() {
        console.log('Generating database schema...');
        
        const schema = `-- COBOL System Metrics Database Schema
-- Generated: ${new Date().toISOString()}

-- Programs table
CREATE TABLE IF NOT EXISTS programs (
    program_id VARCHAR(50) PRIMARY KEY,
    file_path VARCHAR(255) NOT NULL,
    program_type VARCHAR(50),
    lines_of_code INTEGER,
    parse_date TIMESTAMP,
    has_main_program BOOLEAN
);

-- Copybooks table
CREATE TABLE IF NOT EXISTS copybooks (
    copybook_name VARCHAR(50) PRIMARY KEY,
    file_path VARCHAR(255),
    copybook_type VARCHAR(50),
    usage_count INTEGER DEFAULT 0
);

-- Files table
CREATE TABLE IF NOT EXISTS files (
    file_name VARCHAR(50) PRIMARY KEY,
    file_type VARCHAR(20),
    access_programs INTEGER DEFAULT 0
);

-- Metrics table
CREATE TABLE IF NOT EXISTS metrics (
    program_id VARCHAR(50) PRIMARY KEY,
    cyclomatic_complexity INTEGER,
    maintainability_index DECIMAL(5,2),
    halstead_volume INTEGER,
    halstead_difficulty INTEGER,
    halstead_effort INTEGER,
    estimated_bugs DECIMAL(10,2),
    FOREIGN KEY (program_id) REFERENCES programs(program_id)
);

-- Program calls relationship
CREATE TABLE IF NOT EXISTS program_calls (
    caller_program_id VARCHAR(50),
    called_program_id VARCHAR(50),
    call_type VARCHAR(20),
    line_number INTEGER,
    PRIMARY KEY (caller_program_id, called_program_id, line_number),
    FOREIGN KEY (caller_program_id) REFERENCES programs(program_id)
);

-- Copybook usage relationship
CREATE TABLE IF NOT EXISTS copybook_usage (
    program_id VARCHAR(50),
    copybook_name VARCHAR(50),
    line_number INTEGER,
    PRIMARY KEY (program_id, copybook_name, line_number),
    FOREIGN KEY (program_id) REFERENCES programs(program_id),
    FOREIGN KEY (copybook_name) REFERENCES copybooks(copybook_name)
);

-- File access relationship
CREATE TABLE IF NOT EXISTS file_access (
    program_id VARCHAR(50),
    file_name VARCHAR(50),
    access_type VARCHAR(20),
    line_number INTEGER,
    PRIMARY KEY (program_id, file_name, line_number),
    FOREIGN KEY (program_id) REFERENCES programs(program_id),
    FOREIGN KEY (file_name) REFERENCES files(file_name)
);

-- Program sections
CREATE TABLE IF NOT EXISTS program_sections (
    program_id VARCHAR(50),
    section_name VARCHAR(100),
    line_number INTEGER,
    PRIMARY KEY (program_id, section_name),
    FOREIGN KEY (program_id) REFERENCES programs(program_id)
);

-- Program paragraphs
CREATE TABLE IF NOT EXISTS program_paragraphs (
    program_id VARCHAR(50),
    paragraph_name VARCHAR(100),
    line_number INTEGER,
    PRIMARY KEY (program_id, paragraph_name),
    FOREIGN KEY (program_id) REFERENCES programs(program_id)
);

-- Create indexes for performance
CREATE INDEX idx_program_type ON programs(program_type);
CREATE INDEX idx_copybook_usage ON copybook_usage(copybook_name);
CREATE INDEX idx_file_access ON file_access(file_name);
CREATE INDEX idx_complexity ON metrics(cyclomatic_complexity);
CREATE INDEX idx_maintainability ON metrics(maintainability_index);

-- Create views for common queries
CREATE VIEW high_complexity_programs AS
SELECT 
    p.program_id,
    p.file_path,
    p.lines_of_code,
    m.cyclomatic_complexity,
    m.maintainability_index
FROM programs p
JOIN metrics m ON p.program_id = m.program_id
WHERE m.cyclomatic_complexity > 20
ORDER BY m.cyclomatic_complexity DESC;

CREATE VIEW low_maintainability_programs AS
SELECT 
    p.program_id,
    p.file_path,
    p.lines_of_code,
    m.cyclomatic_complexity,
    m.maintainability_index
FROM programs p
JOIN metrics m ON p.program_id = m.program_id
WHERE m.maintainability_index < 50
ORDER BY m.maintainability_index ASC;

CREATE VIEW copybook_usage_summary AS
SELECT 
    c.copybook_name,
    c.copybook_type,
    COUNT(DISTINCT cu.program_id) as usage_count,
    GROUP_CONCAT(cu.program_id) as used_by_programs
FROM copybooks c
LEFT JOIN copybook_usage cu ON c.copybook_name = cu.copybook_name
GROUP BY c.copybook_name, c.copybook_type
ORDER BY usage_count DESC;

CREATE VIEW program_dependencies AS
SELECT 
    pc.caller_program_id,
    COUNT(DISTINCT pc.called_program_id) as calls_count,
    COUNT(DISTINCT cu.copybook_name) as copybooks_count,
    COUNT(DISTINCT fa.file_name) as files_count
FROM programs p
LEFT JOIN program_calls pc ON p.program_id = pc.caller_program_id
LEFT JOIN copybook_usage cu ON p.program_id = cu.program_id
LEFT JOIN file_access fa ON p.program_id = fa.program_id
WHERE p.program_id = pc.caller_program_id
GROUP BY pc.caller_program_id;
`;

        fs.writeFileSync(
            path.join(this.parsedDir, 'parser_analysis/database/cobol-metrics-schema.sql'),
            schema
        );

        return schema;
    }

    generateDataInserts() {
        console.log('Generating data insert statements...');
        
        let inserts = `-- Data Insert Statements
-- Generated: ${new Date().toISOString()}

BEGIN TRANSACTION;

`;

        // Insert programs
        this.structures.forEach(struct => {
            const programType = this.getProgramType(struct);
            inserts += `INSERT INTO programs VALUES ('${struct.programId}', '${struct.filePath}', '${programType}', ${struct.metadata.lines}, '${struct.metadata.parseDate}', ${struct.metadata.hasMainProgram ? 1 : 0});\n`;
        });

        // Insert copybooks
        const copybooks = new Set();
        this.structures.forEach(struct => {
            struct.copyStatements.forEach(copy => {
                if (!copybooks.has(copy.copybook)) {
                    copybooks.add(copy.copybook);
                    const type = this.getCopybookType(copy.copybook);
                    inserts += `INSERT INTO copybooks VALUES ('${copy.copybook}', 'copybooks/${copy.copybook}', '${type}', 0);\n`;
                }
            });
        });

        // Insert files
        const files = new Set();
        this.structures.forEach(struct => {
            struct.fileDescriptions.forEach(file => {
                if (!files.has(file.fileName)) {
                    files.add(file.fileName);
                    inserts += `INSERT INTO files VALUES ('${file.fileName}', '${file.type}', 0);\n`;
                }
            });
        });

        // Insert relationships
        this.structures.forEach(struct => {
            // Program calls
            struct.callStatements.forEach(call => {
                inserts += `INSERT INTO program_calls VALUES ('${struct.programId}', '${call.program}', '${call.type}', ${call.line});\n`;
            });

            // Copybook usage
            struct.copyStatements.forEach(copy => {
                inserts += `INSERT INTO copybook_usage VALUES ('${struct.programId}', '${copy.copybook}', ${copy.line});\n`;
            });

            // File access
            struct.fileDescriptions.forEach(file => {
                inserts += `INSERT INTO file_access VALUES ('${struct.programId}', '${file.fileName}', '${file.type}', ${file.line});\n`;
            });

            // Sections
            struct.sections.forEach(section => {
                inserts += `INSERT INTO program_sections VALUES ('${struct.programId}', '${section.name}', ${section.line});\n`;
            });

            // Paragraphs (limit to avoid too many inserts)
            struct.paragraphs.slice(0, 50).forEach(para => {
                inserts += `INSERT INTO program_paragraphs VALUES ('${struct.programId}', '${para.name}', ${para.line});\n`;
            });
        });

        inserts += `\nCOMMIT;\n`;

        fs.writeFileSync(
            path.join(this.parsedDir, 'parser_analysis/database/cobol-data-inserts.sql'),
            inserts
        );

        return inserts;
    }

    generateDependencyQueries() {
        console.log('Generating dependency analysis queries...');
        
        const queries = `-- Dependency Analysis Queries
-- Generated: ${new Date().toISOString()}

-- 1. Find all programs that call a specific program
-- Replace 'TARGET_PROGRAM' with the program you're looking for
SELECT DISTINCT caller_program_id, call_type
FROM program_calls
WHERE called_program_id = 'TARGET_PROGRAM'
ORDER BY caller_program_id;

-- 2. Find all programs called by a specific program
SELECT DISTINCT called_program_id, call_type, COUNT(*) as call_count
FROM program_calls
WHERE caller_program_id = 'SOURCE_PROGRAM'
GROUP BY called_program_id, call_type
ORDER BY call_count DESC;

-- 3. Find call chains (2 levels deep)
SELECT 
    pc1.caller_program_id as level1_caller,
    pc1.called_program_id as level2_program,
    pc2.called_program_id as level3_program
FROM program_calls pc1
LEFT JOIN program_calls pc2 ON pc1.called_program_id = pc2.caller_program_id
WHERE pc1.caller_program_id = 'START_PROGRAM'
ORDER BY level1_caller, level2_program, level3_program;

-- 4. Find programs with no incoming calls (potential entry points)
SELECT p.program_id, p.file_path, p.program_type
FROM programs p
LEFT JOIN program_calls pc ON p.program_id = pc.called_program_id
WHERE pc.called_program_id IS NULL
  AND p.has_main_program = 1
ORDER BY p.program_id;

-- 5. Find programs with no outgoing calls (leaf programs)
SELECT p.program_id, p.file_path, p.lines_of_code
FROM programs p
LEFT JOIN program_calls pc ON p.program_id = pc.caller_program_id
WHERE pc.caller_program_id IS NULL
ORDER BY p.lines_of_code DESC;

-- 6. Find copybooks shared between programs
SELECT 
    cu1.copybook_name,
    COUNT(DISTINCT cu1.program_id) as program_count,
    GROUP_CONCAT(DISTINCT cu1.program_id) as programs
FROM copybook_usage cu1
GROUP BY cu1.copybook_name
HAVING COUNT(DISTINCT cu1.program_id) > 1
ORDER BY program_count DESC;

-- 7. Find programs that share multiple copybooks
SELECT 
    p1.program_id as program1,
    p2.program_id as program2,
    COUNT(*) as shared_copybooks
FROM copybook_usage p1
JOIN copybook_usage p2 ON p1.copybook_name = p2.copybook_name
WHERE p1.program_id < p2.program_id
GROUP BY p1.program_id, p2.program_id
HAVING COUNT(*) > 2
ORDER BY shared_copybooks DESC;

-- 8. Analyze file access patterns
SELECT 
    f.file_name,
    f.file_type,
    COUNT(DISTINCT fa.program_id) as accessor_count,
    GROUP_CONCAT(DISTINCT fa.access_type) as access_types
FROM files f
JOIN file_access fa ON f.file_name = fa.file_name
GROUP BY f.file_name, f.file_type
ORDER BY accessor_count DESC;

-- 9. Find potential circular dependencies (simplified - 2 levels)
SELECT DISTINCT
    pc1.caller_program_id as program_a,
    pc1.called_program_id as program_b
FROM program_calls pc1
JOIN program_calls pc2 ON pc1.called_program_id = pc2.caller_program_id
WHERE pc2.called_program_id = pc1.caller_program_id
ORDER BY program_a;

-- 10. Generate a complexity report
SELECT 
    p.program_id,
    p.program_type,
    p.lines_of_code,
    m.cyclomatic_complexity,
    m.maintainability_index,
    COUNT(DISTINCT pc.called_program_id) as calls_out,
    COUNT(DISTINCT pc2.caller_program_id) as calls_in,
    COUNT(DISTINCT cu.copybook_name) as copybooks_used
FROM programs p
LEFT JOIN metrics m ON p.program_id = m.program_id
LEFT JOIN program_calls pc ON p.program_id = pc.caller_program_id
LEFT JOIN program_calls pc2 ON p.program_id = pc2.called_program_id
LEFT JOIN copybook_usage cu ON p.program_id = cu.program_id
GROUP BY p.program_id, p.program_type, p.lines_of_code, 
         m.cyclomatic_complexity, m.maintainability_index
ORDER BY m.cyclomatic_complexity DESC NULLS LAST
LIMIT 50;
`;

        fs.writeFileSync(
            path.join(this.parsedDir, 'parser_analysis/database/dependency-analysis-queries.sql'),
            queries
        );

        return queries;
    }

    generateMetricsInserts() {
        console.log('Generating metrics insert statements...');
        
        // Load metrics if available
        const complexityPath = path.join(this.parsedDir, 'parser_analysis/dashboard/complexity-metrics.json');
        const maintainabilityPath = path.join(this.parsedDir, 'parser_analysis/dashboard/maintainability-index.json');
        
        let metricsInserts = `-- Metrics Insert Statements
-- Generated: ${new Date().toISOString()}

BEGIN TRANSACTION;

`;

        if (fs.existsSync(complexityPath) && fs.existsSync(maintainabilityPath)) {
            const complexity = JSON.parse(fs.readFileSync(complexityPath, 'utf-8'));
            const maintainability = JSON.parse(fs.readFileSync(maintainabilityPath, 'utf-8'));
            
            // Create a map for easy lookup
            const maintainMap = {};
            maintainability.forEach(m => {
                maintainMap[m.program] = m;
            });
            
            complexity.forEach(comp => {
                const maint = maintainMap[comp.program];
                if (maint) {
                    metricsInserts += `INSERT INTO metrics VALUES ('${comp.program}', ${comp.complexity}, ${maint.index}, 0, 0, 0, 0.0);\n`;
                }
            });
        }

        metricsInserts += `\nCOMMIT;\n`;

        fs.writeFileSync(
            path.join(this.parsedDir, 'parser_analysis/database/metrics-inserts.sql'),
            metricsInserts
        );

        return metricsInserts;
    }

    generateSQLiteScript() {
        console.log('Generating SQLite initialization script...');
        
        const script = `#!/bin/bash
# SQLite Database Creation Script
# Generated: ${new Date().toISOString()}

DB_FILE="cobol-metrics.db"

# Remove existing database
if [ -f "$DB_FILE" ]; then
    rm "$DB_FILE"
    echo "Removed existing database"
fi

# Create new database and load schema
echo "Creating database schema..."
sqlite3 "$DB_FILE" < cobol-metrics-schema.sql

# Load data
echo "Loading program data..."
sqlite3 "$DB_FILE" < cobol-data-inserts.sql

# Load metrics if available
if [ -f "metrics-inserts.sql" ]; then
    echo "Loading metrics data..."
    sqlite3 "$DB_FILE" < metrics-inserts.sql
fi

# Run some basic queries to verify
echo "Running verification queries..."
echo "Total programs:"
sqlite3 "$DB_FILE" "SELECT COUNT(*) FROM programs;"

echo "Total copybooks:"
sqlite3 "$DB_FILE" "SELECT COUNT(*) FROM copybooks;"

echo "Total file definitions:"
sqlite3 "$DB_FILE" "SELECT COUNT(*) FROM files;"

echo "Programs with high complexity (>20):"
sqlite3 "$DB_FILE" "SELECT COUNT(*) FROM high_complexity_programs;"

echo "Database creation complete: $DB_FILE"
`;

        fs.writeFileSync(
            path.join(this.parsedDir, 'parser_analysis/database/create-database.sh'),
            script
        );

        // Make it executable
        fs.chmodSync(path.join(this.parsedDir, 'parser_analysis/database/create-database.sh'), 0o755);

        return script;
    }

    getProgramType(struct) {
        if (struct.filePath.includes('batch')) return 'BATCH';
        if (struct.programId.match(/^(SYS|UTL|XL)/i)) return 'UTILITY';
        if (struct.metadata.hasMainProgram) return 'MAIN';
        return 'SUBPROGRAM';
    }

    getCopybookType(name) {
        const lower = name.toLowerCase();
        if (lower.includes('ws')) return 'WORKING-STORAGE';
        if (lower.includes('fd')) return 'FILE-DESCRIPTION';
        if (lower.includes('sel')) return 'FILE-SELECT';
        return 'GENERAL';
    }

    generateAll() {
        console.log('Generating database files...');
        this.loadData();
        this.generateDatabaseSchema();
        this.generateDataInserts();
        this.generateDependencyQueries();
        this.generateMetricsInserts();
        this.generateSQLiteScript();
        console.log('Database generation complete!');
    }
}

// Main execution
if (require.main === module) {
    const generator = new DatabaseGenerator(path.resolve(__dirname));
    generator.generateAll();
}

module.exports = DatabaseGenerator;