"""
Operations module for account management system.
Handles credit, debit, and balance viewing operations.
"""

from data import DataProgram

class Operations:
    def __init__(self):
        """Initialize the Operations module with a DataProgram instance."""
        self.data_program = DataProgram()
    
    def view_balance(self):
        """
        Display the current account balance.
        """
        try:
            balance = self.data_program.perform_operation('READ')
            print(f"Current balance: {balance:.2f}")
            return balance
        except Exception as e:
            print(f"Error retrieving balance: {e}")
            return None
    
    def credit_account(self):
        """
        Credit an amount to the account.
        Prompts user for amount and updates the balance.
        """
        try:
            # Get credit amount from user
            while True:
                try:
                    amount_input = input("Enter credit amount: ")
                    amount = float(amount_input)
                    if amount <= 0:
                        print("Please enter a positive amount.")
                        continue
                    break
                except ValueError:
                    print("Please enter a valid number.")
                    continue
            
            # Read current balance
            current_balance = self.data_program.perform_operation('READ')
            
            # Calculate new balance
            new_balance = current_balance + amount
            
            # Write new balance
            self.data_program.perform_operation('WRITE', new_balance)
            
            print(f"Amount credited. New balance: {new_balance:.2f}")
            return new_balance
            
        except Exception as e:
            print(f"Error processing credit: {e}")
            return None
    
    def debit_account(self):
        """
        Debit an amount from the account.
        Prompts user for amount and updates the balance if sufficient funds exist.
        """
        try:
            # Get debit amount from user
            while True:
                try:
                    amount_input = input("Enter debit amount: ")
                    amount = float(amount_input)
                    if amount <= 0:
                        print("Please enter a positive amount.")
                        continue
                    break
                except ValueError:
                    print("Please enter a valid number.")
                    continue
            
            # Read current balance
            current_balance = self.data_program.perform_operation('READ')
            
            # Check if sufficient funds
            if current_balance >= amount:
                # Calculate new balance
                new_balance = current_balance - amount
                
                # Write new balance
                self.data_program.perform_operation('WRITE', new_balance)
                
                print(f"Amount debited. New balance: {new_balance:.2f}")
                return new_balance
            else:
                print("Insufficient funds for this debit.")
                return current_balance
                
        except Exception as e:
            print(f"Error processing debit: {e}")
            return None
    
    def perform_operation(self, operation_type):
        """
        Perform the specified operation.
        
        Args:
            operation_type (str): The type of operation ('TOTAL', 'CREDIT', 'DEBIT')
            
        Returns:
            float: The resulting balance, or None if operation failed
        """
        operation_type = operation_type.strip().upper()
        
        if operation_type == 'TOTAL':
            return self.view_balance()
        elif operation_type == 'CREDIT':
            return self.credit_account()
        elif operation_type == 'DEBIT':
            return self.debit_account()
        else:
            print(f"Unknown operation type: {operation_type}")
            return None
