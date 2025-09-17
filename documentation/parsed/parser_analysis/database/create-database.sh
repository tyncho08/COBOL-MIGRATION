#!/bin/bash
# SQLite Database Creation Script
# Generated: 2025-09-17T19:53:05.809Z

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
