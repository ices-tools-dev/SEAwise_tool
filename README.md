# SEAwise_tool

## Overview

SEAwise_tool is a comprehensive ecosystem-based fisheries management (EBFM) application that serves as the central hub for integrating and displaying outputs from multiple Work Packages (WP2-WP6) of the SEAwise project. This tool provides fisheries managers, researchers, and stakeholders with a unified platform to access and analyze ecosystem-based fisheries management data and insights.

## Purpose

The SEAwise_tool addresses the critical need for integrated ecosystem-based fisheries management by:

- **Centralizing Data**: Consolidating outputs from multiple specialized work packages into a single, accessible interface
- **Supporting Decision-Making**: Providing comprehensive insights for sustainable fisheries management decisions
- **Enabling Ecosystem Approach**: Facilitating the integration of social, economic, and ecological factors in fisheries management
- **Promoting Collaboration**: Creating a unified platform for stakeholders across different domains

## Features

- **Integrated Dashboard**: Single interface to access all work package outputs
- **Cross-Domain Analysis**: Connect insights from social, economic, and ecological assessments
- **Spatial Management Tools**: Support for area-based fisheries management strategies
- **Strategy Evaluation**: Tools for assessing fisheries management strategies in ecosystem context
- **User-Friendly Interface**: Designed for both technical and non-technical users

## System Architecture & Module Interaction

The following sequence diagram illustrates how the SEAwise_tool modules work together to provide integrated ecosystem-based fisheries management insights:

```mermaid
sequenceDiagram
    participant User as Fisheries Manager/Researcher
    participant UI as Main UI (app_ui.R)
    participant Server as Main Server (app_server.R)
    participant Results as Results Module (mod_results.R)
    participant WP2 as WP2: Social & Economic
    participant WP3 as WP3: Ecological Effects on Fisheries
    participant WP4 as WP4: Ecological Effects of Fisheries
    participant WP5 as WP5: Spatial Management
    participant WP6 as WP6: Strategy Evaluation
    participant Data as Regional Data Files (.rds)

    User->>UI: Access SEAwise EBFM Toolbox
    UI->>Server: Initialize application
    
    Note over UI: Navigation Structure:<br/>- Home<br/>- About (SEAwise, Themes, Case Studies)<br/>- Results (Baltic, GNS, Med, WW)<br/>- Resources
    
    User->>UI: Select Results Tab (e.g., "Greater North Sea")
    UI->>Server: Tab selection event
    Server->>Results: Initialize results module with case_study
    
    Note over Results: Dynamic tab generation based on region:<br/>- Baltic Sea: WP6 only<br/>- Other regions: WP2, WP3, WP4, WP5, WP6
    
    Results->>WP2: Initialize WP2 module
    Results->>WP3: Initialize WP3 module
    Results->>WP4: Initialize WP4 module
    Results->>WP5: Initialize WP5 module
    Results->>WP6: Initialize WP6 module
    
    par Parallel Module Initialization
        WP2->>Data: Load regional data (fleet, socioeco, carbon, fuel, portions, projections)
        WP3->>Data: Load regional ecological data
        WP4->>Data: Load regional fisheries impact data
        WP5->>Data: Load regional spatial data
        WP6->>Data: Load regional strategy evaluation data
    end
    
    Data-->>WP2: Return WP2 datasets
    Data-->>WP3: Return WP3 datasets
    Data-->>WP4: Return WP4 datasets
    Data-->>WP5: Return WP5 datasets
    Data-->>WP6: Return WP6 datasets
    
    Note over WP2: WP2 Sub-modules:<br/>- Fleet Characteristics<br/>- Communities<br/>- Carbon Emissions<br/>- Fuel Use & Cost<br/>- Meal Provision<br/>- Climate Projections
    
    Note over WP3: WP3 Sub-modules:<br/>- Ecological effects analysis
    
    Note over WP4: WP4 Sub-modules:<br/>- Bycatch analysis<br/>- Ecosystem risk<br/>- Marine litter<br/>- RBS indicators
    
    Note over WP5: WP5 Sub-modules:<br/>- Spatial management analysis
    
    Note over WP6: WP6 Sub-modules:<br/>- MSE analysis<br/>- MCDA evaluation
    
    Results-->>UI: Render dynamic tabs with all modules
    UI-->>User: Display integrated results dashboard
    
    User->>UI: Navigate between WP tabs
    UI->>Results: Tab selection
    Results->>WP2: Display WP2 content
    Results->>WP3: Display WP3 content
    Results->>WP4: Display WP4 content
    Results->>WP5: Display WP5 content
    Results->>WP6: Display WP6 content
    
    Note over User: Access to integrated insights from all work packages
```

### Key Integration Points:

1. **Shiny Module Architecture**: Main application uses Shiny modules for each work package
2. **Dynamic Tab Generation**: Results module dynamically creates tabs based on selected region
3. **Regional Data Loading**: Each module loads region-specific data files (.rds format)
4. **Conditional Module Display**: Baltic Sea shows only WP6, other regions show all WPs
5. **Sub-module Organization**: Each WP contains specialized analysis sub-modules

### Data Flow:

- **Input**: Regional case study selection (Baltic, GNS, Mediterranean, Western Waters)
- **Processing**: Shiny modules initialize and load region-specific data
- **Integration**: Results module coordinates all WP modules in unified interface
- **Output**: Interactive dashboard with tabs for each work package

### Regional Coverage:

- **Baltic Sea**: Management strategy evaluation (WP6)
- **Greater North Sea**: All work packages (WP2-WP6)
- **Mediterranean**: All work packages with sub-region selection (GSA 17-19, GSA 20)
- **Western Waters**: All work packages with sub-region selection (Celtic Sea, Bay of Biscay)

## Target Users

- **Fisheries Managers**: Government agencies and regulatory bodies
- **Researchers**: Marine scientists and fisheries biologists
- **Stakeholders**: Fishing industry representatives and conservation groups
- **Policy Makers**: Decision-makers involved in marine resource management

## Getting Started

[To be added: Installation instructions, usage examples, and documentation links]

## Contributing

[To be added: Contribution guidelines and development setup]

## License

[To be added: License information]

## Contact

[To be added: Contact information and support details]
