# Execute Subsystem Analysis
Task: Identify and document system subsystems

Input Context:
- All previous phase outputs
- Special focus on functional documentation from Phase 3

Required Outputs in documentation/subsystems/:

## Master Documentation:
00_MASTER_SUBSYSTEM_ARCHITECTURE.md
01_SUBSYSTEM_INVENTORY.md
02_INTEGRATION_ARCHITECTURE.md
03_DATA_OWNERSHIP_MAP.md
04_PROCESS_ALLOCATION.md
05_DEPENDENCY_ANALYSIS.md

## Subsystem Folders:
Subsystems/
├── GL_CORE/
│   ├── GL_CORE_SPECIFICATION.md
│   ├── GL_CORE_INTERFACES.md
│   ├── GL_CORE_FLOWS.md
│   └── GL_CORE_DIAGRAMS.md
├── AR_MGMT/
├── AP_MGMT/
├── INV_CTRL/
├── IRS_PROC/
└── [Other identified subsystems]

## Analysis Artifacts:
Diagrams/
├── system_context.mermaid
├── subsystem_interactions.mermaid
├── data_flow_complete.mermaid
└── state_transitions.mermaid

Analysis/
├── coupling_analysis.md
├── cohesion_metrics.md
├── modernization_impact.md
└── risk_assessment.md

Subsystem Identification Criteria:
- Functional cohesion
- Data ownership
- Business domain alignment
- Minimal coupling
- Independent evolution potential