# EXECUTE COBOL ANALYSIS PIPELINE:

Step 1: Parse all COBOL files in `Legacy_App/` using the parser prompt `1_parser_prompt.md`
        Output to `documentation/parsed/`

Step 2: Using `Legacy_App/` + `documentation/parsed/parsed-structures/`, execute the analysis prompt `2_parsed_analysis_prompt.md`
        Output to `documentation/parsed/parser_analysis/`

Step 3: Using `Legacy_App/` + all `documentation/parsed/` outputs, execute the functional documentation prompt `3_functional_prompt.md`
        Output to `documentation/functional/`

Step 4: Using `Legacy_App/` + `documentation/parsed/` + `documentation/functional/`, execute the subsystems identification prompt `4_subsystems_prompt.md`
        Output to `documentation/subsystems/`

Each step must complete before the next begins.
Each step uses all previous outputs as context.
Think ultra mega hard at each step.
Document everything exhaustively.