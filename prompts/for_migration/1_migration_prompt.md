# COBOL TO MODERN STACK MIGRATION - DIRECT INSTRUCTIONS

## OBJECTIVE
Migrate the ACAS system from COBOL to a modern stack. Deliver a fully functional, production-ready accounting system preserving all business logic. Users should see it as a brand-new app.

## INPUT SOURCES
- `Legacy_App/` (root): original COBOL application; serves as the source of truth for any missing or unclear information
- `documentation/` (root): parsed COBOL project in JSON, functional documentation, subsystem documentation

## CRITICAL CONSTRAINTS
- No migration references in UI
- All navigation links must work; every tab/window fully functional
- No non-functional buttons or features; data must persist correctly
- Clean, professional UI; frontend inspired by modern banking apps
- Correct icon sizing (w-6 h-6)
- Responsive design and intuitive navigation
- Production-ready, maintainable code
- Preserve all COBOL business logic exactly

## TECHNOLOGY STACK
- Backend: Python 3.11+ with FastAPI
- Frontend: Next.js 14 + TypeScript
- Database: PostgreSQL 15+
- Styling: Tailwind CSS + Heroicons
- API: RESTful with automatic OpenAPI docs

## SYSTEM MODULES
- General Ledger (GL)
- Accounts Receivable (AR)
- Accounts Payable (AP)
- Inventory / Stock
- Tax / IRS
- Master Data Management
- Reporting & Analytics

## MIGRATION STEPS

### 1. Analysis
- Explore COBOL codebase (`Legacy_App/`) and documentation (`documentation/functional/`, `documentation/parsed/`, `documentation/subsystems/`)
- Identify key COBOL programs for each module
- When documentation is missing or unclear, use `Legacy_App/` as the source of truth

### 2. Database
- Extract all COBOL data structures and create PostgreSQL schema
- Preserve field names, sizes, types, and business rules
- Add constraints, indexes, foreign keys, audit columns

### 3. Backend
- Implement Python services per COBOL program
- Preserve all validation, calculations, and logic
- Implement complete API endpoints for all business functions

### 4. Frontend
- Create professional UI for all modules
- Ensure all tabs/windows fully functional
- Use responsive design and modern UX patterns
- Avoid migration mentions
- Consistent icon sizing (w-6 h-6)

### 5. Setup & Scripts
- Provide scripts to initialize database, backend, and frontend
- Ensure app runs end-to-end without errors

## PROJECT STRUCTURE
```
Migrated\_App/
├─ backend/
│  ├─ app/models/         # SQLAlchemy models
│  ├─ app/services/       # Business logic
│  ├─ app/api/            # REST endpoints
│  ├─ app/core/           # Configs and utilities
│  └─ main.py             # FastAPI entry point
├─ frontend/
│  ├─ app/                # Next.js pages per module
│  ├─ components/         # Forms, tables, navigation
│  ├─ services/           # API clients
│  ├─ types/              # TypeScript types
│  └─ global.css, layout.tsx
├─ database/
│  ├─ schema.sql
│  ├─ seed\_data.sql
│  └─ migrations/
├─ logs/
├─ start-acas.sh
└─ README.md
```

## VALIDATION CHECKLIST
- All COBOL programs mapped to Python services
- All data structures in PostgreSQL
- Exact business calculations and validations preserved
- All frontend modules accessible, working, and visually appealing
- No broken links, non-functional buttons, or empty pages
- Error handling, logging, security, and performance in place

## SUCCESS CRITERIA
- Functional parity with original COBOL system
- User-friendly, professional interface
- Fully operational app with end-to-end functionality
- Modern, maintainable, and scalable codebase

Think ultra mega hard at each step.