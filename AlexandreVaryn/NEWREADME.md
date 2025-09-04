# Modernized Bank Account Project (COBOL → Python)

This project is a **modernized Python version** of an old COBOL banking program.  
It allows a user to interact with a simple bank account through the command line.

## Features
- **Check balance** – see the current amount of money in your account.  
- **Add credit** – deposit a positive amount of money.  
- **Debit credit** – withdraw a positive amount of money.  
- **Exit program** – quit the application safely.  

## Improvements over the original COBOL version
While converting the project from COBOL to Python, several issues were fixed:
- Prevented depositing or withdrawing negative values.  
- Added input validation for the main menu choices.  
- Added input validation when entering credit/debit amounts.  
- Improved overall stability and user experience.  

## How to Run
1. Make sure you have **Python 3.8+** installed.  
2. Clone this repository or download the project files.  
3. Run the program in your terminal:  
   ```bash
   python bank_account.py