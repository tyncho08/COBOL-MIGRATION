# Execute Subsystem Analysis

Task: Identify and document system subsystems

Input:
- Outputs from all previous phases
- Functional documentation from Phase 3 (`documentation/functional/`)

Outputs (in `documentation/subsystems/`):

Master Documentation:
- `documentation/subsystems/00_MASTER_SUBSYSTEM_ARCHITECTURE.md`
- `documentation/subsystems/01_SUBSYSTEM_INVENTORY.md`
- `documentation/subsystems/02_INTEGRATION_ARCHITECTURE.md`
- `documentation/subsystems/03_DATA_OWNERSHIP_MAP.md`
- `documentation/subsystems/04_PROCESS_ALLOCATION.md`
- `documentation/subsystems/05_DEPENDENCY_ANALYSIS.md`

Subsystem Folders:
- `documentation/subsystems/GL_CORE/`
  - `documentation/subsystems/GL_CORE/GL_CORE_SPECIFICATION.md`
  - `documentation/subsystems/GL_CORE/GL_CORE_INTERFACES.md`
  - `documentation/subsystems/GL_CORE/GL_CORE_FLOWS.md`
  - `documentation/subsystems/GL_CORE/GL_CORE_DIAGRAMS.md`
- `documentation/subsystems/AR_MGMT/`
- `documentation/subsystems/AP_MGMT/`
- `documentation/subsystems/INV_CTRL/`
- `documentation/subsystems/IRS_PROC/`
- `documentation/subsystems/MDM/`
- `documentation/subsystems/RPT_ENGINE/`
- `documentation/subsystems/BATCH_FW/`
- `documentation/subsystems/INTEGRATION/`
- `documentation/subsystems/SEC_AUDIT/`
- `documentation/subsystems/DATE_UTIL/`
- `documentation/subsystems/CURR_UTIL/`
- `documentation/subsystems/FILE_SVC/`
- `documentation/subsystems/ERROR_FW/`
- [Other identified subsystems]

Analysis Artifacts:
- `documentation/subsystems/Diagrams/`
  - `documentation/subsystems/Diagrams/system_context.mermaid`
  - `documentation/subsystems/Diagrams/subsystem_interactions.mermaid`
  - `documentation/subsystems/Diagrams/data_flow_complete.mermaid`
  - `documentation/subsystems/Diagrams/state_transitions.mermaid`
  - [per-subsystem diagrams]
- `documentation/subsystems/Analysis/`
  - `documentation/subsystems/Analysis/coupling_analysis.md`
  - `documentation/subsystems/Analysis/cohesion_metrics.md`
  - `documentation/subsystems/Analysis/modernization_impact.md`
  - `documentation/subsystems/Analysis/risk_assessment.md`

Subsystem Identification Criteria:
- Functional cohesion (shared programs, data, business rules, error handling)
- Loose coupling (minimal dependencies, clear interfaces, asynchronous interactions)
- Business alignment (maps to org structure, domain boundaries, user roles)
- Data ownership
- Transaction/temporal boundaries
- User interaction patterns
- Independent evolution potential

Subsystem Documentation Template:
- Executive Summary: Purpose, Business Value, Key Users, Criticality
- Functional Capabilities: Core Functions, Supported Business Processes
- Data Domain: Owned/Referenced Entities
- Interface Contracts: Inbound, Outbound, Internal APIs/Services
- Business Rules Engine: Validation, Calculation, Workflow Rules
- Operational Characteristics: Processing Patterns, Peak Periods, Data Volumes & Retention
- Dependencies: Upstream, Downstream, External
- Quality Attributes: Performance, Reliability, Compliance
- Evolution Potential: Enhancements, Modernization Candidates, Known Limitations

Subsystem Interaction Diagrams:
- Context Diagram (`.mermaid`)
- Functional Flow Diagrams (`.mermaid sequence`)
- Data Flow Diagrams (`.mermaid graph`)
- State Transition Diagrams (`.mermaid state`)

Integration Layer Documentation:
- Subsystem Communication Matrix (From/To, type, data, frequency, protocol)
- Event Catalog (events, payloads, subscribers, processing mode, error handling)

Governance Model:
- Ownership: Business Owner, Technical Owner, Data Steward
- Responsibilities: Enhancements, Maintenance, Data Quality, Interfaces, Performance
- Change Impact Analysis

Validation & Quality Checks:
- Completeness: all programs allocated, data entities owned, processes mapped, interfaces bidirectional, critical paths identified, compliance covered
- Consistency: no duplicate functions, no orphaned dependencies, interfaces matched, unambiguous data ownership, complete event flows, comprehensive error handling

Execution Steps:
1. Review `documentation/functional/`
2. Identify domain-specific rules, requirements, pain points
3. Define candidate subsystems
4. Validate with data ownership and transaction boundaries
5. Document interfaces and integration points
6. Create subsystem flow and state diagrams
7. Run completeness and consistency checks
8. Deliver final documentation tree

Think ultra mega hard at each step.