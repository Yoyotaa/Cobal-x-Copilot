# Data Program - Handles balance storage operations
class DataProgram:
    def __init__(self):
        self.storage_balance = 1000.00  # Initial balance

    # Read operation - returns current balance
    def read(self):
        return self.storage_balance

    # Write operation - updates balance
    def write(self, new_balance):
        self.storage_balance = new_balance
        return self.storage_balance

    # Get current balance
    def get_balance(self):
        return self.read()

    # Set balance
    def set_balance(self, amount):
        return self.write(amount)
