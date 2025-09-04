# Operations Program - Handles account operations
from data import DataProgram

class Operations:
    def __init__(self):
        self.data_program = DataProgram()

    # Display current balance
    def view_balance(self):
        balance = self.data_program.read()
        print("Current balance: ${:.2f}".format(balance))
        return balance

    # Credit account with specified amount
    def credit_account(self, amount):
        current_balance = self.data_program.read()
        new_balance = current_balance + amount
        self.data_program.write(new_balance)
        print("Amount credited. New balance: ${:.2f}".format(new_balance))
        return new_balance

    # Debit account with specified amount
    def debit_account(self, amount):
        current_balance = self.data_program.read()
        if current_balance >= amount:
            new_balance = current_balance - amount
            self.data_program.write(new_balance)
            print("Amount debited. New balance: ${:.2f}".format(new_balance))
            return new_balance
        else:
            print("Insufficient funds for this debit.")
            return current_balance

    # Process operation based on type
    def process_operation(self, operation_type):
        if operation_type == 'TOTAL':
            return self.view_balance()
        elif operation_type == 'CREDIT':
            return self.prompt_for_amount('credit')
        elif operation_type == 'DEBIT':
            return self.prompt_for_amount('debit')
        else:
            print("Invalid operation type.")
            return None

    # Prompt user for amount and process operation
    def prompt_for_amount(self, operation):
        while True:
            try:
                amount_input = input("Enter {} amount: $".format(operation))
                amount = float(amount_input)

                if amount <= 0:
                    print("Invalid amount. Please enter a positive number.")
                    continue

                if operation == 'credit':
                    return self.credit_account(amount)
                elif operation == 'debit':
                    return self.debit_account(amount)

            except ValueError:
                print("Invalid amount. Please enter a valid number.")
                continue
