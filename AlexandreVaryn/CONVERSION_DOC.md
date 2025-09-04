# Documentation: COBOL to Python Conversion for Account Management System

## Overview
This project modernizes a legacy COBOL account management system by converting its logic to Python. The system allows users to view their account balance, credit their account, and debit their account through a simple menu-driven interface.

## Converted Files
- **main.cob → main.py**: The main program loop and menu logic.
- **operations.cob → operations.py**: The account operations (view, credit, debit) logic.

## How the Python Code Works
- **main.py**: Presents a menu to the user. Based on the user's choice, it calls the `operations()` function from `operations.py` with the appropriate operation type ("TOTAL", "CREDIT", "DEBIT").
- **operations.py**:
  - Uses a text file (`balance.txt`) to store the account balance, simulating the COBOL data storage.
  - Provides three main operations:
    - **TOTAL**: Displays the current balance.
    - **CREDIT**: Prompts the user for an amount, adds it to the balance, and updates the file.
    - **DEBIT**: Prompts the user for an amount, subtracts it from the balance if sufficient funds exist, and updates the file.
  - Includes helper functions to read and write the balance.

## Key Modifications
- Replaced COBOL's `DISPLAY` and `ACCEPT` statements with Python's `print()` and `input()` functions.
- Replaced COBOL's file and program calls with Python file I/O for balance management.
- Added error handling for missing balance file (defaults to 1000.00).
- Modularized the code for clarity and maintainability.

## Usage
1. Run `main.py` to start the menu-driven program.
2. The program will prompt for actions and interact with the user via the console.
3. Account balance is stored in `balance.txt` in the project directory.

## Example
```
python main.py
```

## Notes
- The Python code is designed to closely match the original COBOL logic for easy understanding and maintenance.
- All modifications are documented in code comments and docstrings within the Python files.
