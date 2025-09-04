BALANCE_FILE = "balance.txt"

def read_balance():
    try:
        with open(BALANCE_FILE, "r") as f:
            return float(f.read())
    except FileNotFoundError:
        return 1000.0 # Default initial balance

def write_balance(balance):
    with open(BALANCE_FILE, "w") as f:
        f.write(f"{balance:.2f}")

def operations(operation_type):
    operation_type = operation_type.strip().upper()
    final_balance = read_balance()

    if operation_type == "TOTAL":
        print(f"Current balance: {final_balance:.2f}")

    elif operation_type == "CREDIT":
        try:
            amount = float(input("Enter credit amount: "))
            if amount < 0:
                raise ValueError
        except ValueError:
            print("Invalid input. Please enter a positive numeric value for the credit amount.")
            return
        
        final_balance += amount
        write_balance(final_balance)
        print(f"Amount credited. New balance: {final_balance:.2f}")

    elif operation_type == "DEBIT":
        try:
            amount = float(input("Enter credit amount: "))
            if amount < 0:
                raise ValueError
        except ValueError:
            print("Invalid input. Please enter a positive numeric value for the debit amount.")
            return
        if final_balance >= amount:
            final_balance -= amount
            write_balance(final_balance)
            print(f"Amount debited. New balance: {final_balance:.2f}")
        else:
            print("Insufficient funds for this debit.")

if __name__ == "__main__":
    # Example usage: operations("TOTAL")
    pass