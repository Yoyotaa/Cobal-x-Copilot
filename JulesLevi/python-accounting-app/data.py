"""
Data module for account management system.
Handles reading and writing of account balance data.
"""

import json
import os

class DataProgram:
    def __init__(self, data_file='account_balance.json'):
        """
        Initialize the DataProgram with a data file.
        
        Args:
            data_file (str): Path to the JSON file storing balance data
        """
        self.data_file = data_file
        self.default_balance = 1000.00
        
        # Initialize data file if it doesn't exist
        if not os.path.exists(self.data_file):
            self.write_balance(self.default_balance)
    
    def read_balance(self):
        """
        Read the current balance from the data file.
        
        Returns:
            float: The current account balance
        """
        try:
            with open(self.data_file, 'r') as file:
                data = json.load(file)
                return float(data.get('balance', self.default_balance))
        except (FileNotFoundError, json.JSONDecodeError, KeyError):
            # If file doesn't exist or is corrupted, return default balance
            self.write_balance(self.default_balance)
            return self.default_balance
    
    def write_balance(self, balance):
        """
        Write the balance to the data file.
        
        Args:
            balance (float): The balance to write
        """
        try:
            data = {'balance': float(balance)}
            with open(self.data_file, 'w') as file:
                json.dump(data, file, indent=2)
        except Exception as e:
            print(f"Error writing balance to file: {e}")
    
    def perform_operation(self, operation_type, balance=None):
        """
        Perform read or write operation on balance data.
        
        Args:
            operation_type (str): 'READ' or 'WRITE'
            balance (float, optional): Balance to write (required for WRITE)
            
        Returns:
            float: Current balance (for READ operations)
        """
        if operation_type.upper() == 'READ':
            return self.read_balance()
        elif operation_type.upper() == 'WRITE' and balance is not None:
            self.write_balance(balance)
            return balance
        else:
            raise ValueError(f"Invalid operation type: {operation_type}")
