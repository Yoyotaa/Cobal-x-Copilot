# Python Accounting Application

This is a modernized version of a legacy COBOL accounting system, converted to Python.

## Overview

The application provides a simple account management system with the following features:
- View current account balance
- Credit money to the account
- Debit money from the account
- Interactive command-line interface

## Files

- `main.py` - Main program entry point (converted from `main.cob`)
- `operations.py` - Handles account operations (converted from `operations.cob`)
- `data.py` - Manages data storage (converted from `data.cob`)

## Installation

1. Ensure you have Python installed (version 2.7 or higher, Python 3.6+ recommended)
2. Navigate to the project directory
3. No additional dependencies required (uses only Python standard library)

## Usage

**Important**: Use `python3` command if you have both Python 2 and 3 installed.

Run the application:

```bash
# If you have Python 3 as default
python main.py

# If you have both Python 2 and 3, use:
python3 main.py
```

Or with Python 3 explicitly:

```bash
python3 main.py
```

## Features

### Menu Options

1. **View Balance** - Display current account balance
2. **Credit Account** - Add money to the account
3. **Debit Account** - Remove money from the account (with insufficient funds check)
4. **Exit** - Close the application

### Key Improvements from COBOL Version

- **Modern Python syntax** with classes and string formatting
- **Better error handling** with try-except blocks and input validation
- **Modular architecture** with separate files for different concerns
- **Clean and readable code** following Python best practices
- **Cross-version compatibility** (Python 2.7+ and 3.6+)

## Architecture

The application follows a modular design:

- **DataProgram** (`data.py`) - Handles balance storage and retrieval
- **Operations** (`operations.py`) - Manages account operations and user input
- **MainProgram** (`main.py`) - Controls the main application flow and menu

## Original COBOL Structure

This application was converted from three COBOL programs:
- `main.cob` - Main program with menu logic
- `operations.cob` - Account operation handling
- `data.cob` - Data storage and retrieval

The conversion maintains the same logical flow while modernizing the implementation with Python best practices.

## Testing

Run the automated tests:

```bash
# With Python 3
python3 test_conversion.py

# With Python 2 (if available)
python test_conversion.py
```

## Troubleshooting

If you encounter syntax errors:
1. Make sure you're using Python 3.6+ or Python 2.7+
2. Use `python3` instead of `python` if you have both versions installed
3. Check your Python version with `python --version` or `python3 --version`
