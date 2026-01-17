* Use Delphi naming standards for newly added types.
* When API functions fail and the code raises exception, always include OS error code and error message in the exception text.
* Use inline variables when variables are used only for a short time inside if, while, repeat, and for statements.
* Don't capitalize 'assigned' function call.
* Use \_ms suffix for variables that store time in millisecond units. Use \_us suffix for variables that store time in microsecond units.
* Before using GetLastError in Delphi code, first check if the codebase already implements its own GetLastError method (search for 'function GetLastError'). If found, always qualify Windows API error checks with Winapi.Windows.GetLastError to avoid calling the wrong function.
* When modifying source files, always check for a corresponding .md documentation file in the same directory (e.g., GpTimestamp.pas → GpTimestamp.md). If found, update it to reflect version changes, new features, modified behavior, API changes, new examples, and updated test coverage.
* When reading existing code, check it for 'copy and paste' errors.
* When trying to understand existing code, also read information from a corresponding .md documentation file in the same directory.

## Compiling and Running Delphi Code

**Delphi Compiler Location**: `"C:\Program Files (x86)\Embarcadero\Studio\37.0\bin\dcc32.exe"`

**Common Compilation Pattern**:
1. Navigate to the directory containing the .dpr/.dproj file (e.g., `cd src/tests`)
2. Run: `"C:\Program Files (x86)\Embarcadero\Studio\37.0\bin\dcc32.exe" [options] ProjectName.dpr`
3. Common compiler options:
   - `-B` : Rebuild all (build from scratch)
   - `-U<path>` : Add unit search path (e.g., `-U..` for parent directory)
   - `-E<path>` : Specify output directory for executables
   - `-N<path>` : Specify output directory for DCU files

**Running Compiled Executables**:
- Use relative path from current directory: `./ProjectName.exe`
- Or absolute path: `./src/tests/ProjectName.exe`
- Note: Just `ProjectName.exe` won't work in bash shell - always use `./` prefix or full path

**Example Workflow**:
```bash
cd src/tests
"C:\Program Files (x86)\Embarcadero\Studio\37.0\bin\dcc32.exe" -B -U.. MyProject.dpr
./MyProject.exe
```

**Path Issues**:
- Bash in MSYS doesn't handle Windows paths well without quotes
- Always use forward slashes in cd commands: `cd src/tests` (not `cd H:\RAZVOJ\...`)
- Always quote the compiler path due to spaces in "Program Files (x86)"