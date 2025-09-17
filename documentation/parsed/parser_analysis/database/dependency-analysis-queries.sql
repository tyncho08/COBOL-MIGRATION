-- Dependency Analysis Queries
-- Generated: 2025-09-17T19:53:05.808Z

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
