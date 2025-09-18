# Execute Subsystem Analysis

Task: Identify and document system subsystems

Input:
- Outputs from all previous phases
- Functional documentation from Phase 3 (`documentation/functional/`)

Outputs (in `documentation/subsystems/`):

Master Documentation:
- `00_MASTER_SUBSYSTEM_ARCHITECTURE.md`
- `01_SUBSYSTEM_INVENTORY.md`
- `02_INTEGRATION_ARCHITECTURE.md`
- `03_DATA_OWNERSHIP_MAP.md`
- `04_PROCESS_ALLOCATION.md`
- `05_DEPENDENCY_ANALYSIS.md`

Subsystem Folders:
- Subsystems/GL_CORE/
  - `GL_CORE_SPECIFICATION.md`
  - `GL_CORE_INTERFACES.md`
  - `GL_CORE_FLOWS.md`
  - `GL_CORE_DIAGRAMS.md`
- Subsystems/AR_MGMT/
- Subsystems/AP_MGMT/
- Subsystems/INV_CTRL/
- Subsystems/IRS_PROC/
- Subsystems/MDM/
- Subsystems/RPT_ENGINE/
- Subsystems/BATCH_FW/
- Subsystems/INTEGRATION/
- Subsystems/SEC_AUDIT/
- Subsystems/DATE_UTIL/
- Subsystems/CURR_UTIL/
- Subsystems/FILE_SVC/
- Subsystems/ERROR_FW/
- [Other identified subsystems]

Analysis Artifacts:
- Diagrams/
  - `system_context.mermaid`
  - `subsystem_interactions.mermaid`
  - `data_flow_complete.mermaid`
  - `state_transitions.mermaid`
  - [per-subsystem diagrams]
- Analysis/
  - `coupling_analysis.md`
  - `cohesion_metrics.md`
  - `modernization_impact.md`
  - `risk_assessment.md`

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