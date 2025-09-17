-- COBOL System Metrics Database Schema
-- Generated: 2025-09-17T19:53:05.803Z

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
