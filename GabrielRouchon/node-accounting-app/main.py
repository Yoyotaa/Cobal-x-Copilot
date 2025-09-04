# Main Program - Account Management System Entry Point
from operations import Operations

class MainProgram:
    def __init__(self):
        self.operations = Operations()
        self.continue_flag = True

    # Display the main menu
    def display_menu(self):
        print("--------------------------------")
        print("Account Management System")
        print("1. View Balance")
        print("2. Credit Account")
        print("3. Debit Account")
        print("4. Exit")
        print("--------------------------------")

    # Process user choice
    def process_choice(self, choice):
        if choice == '1':
            self.operations.view_balance()
        elif choice == '2':
            self.operations.prompt_for_amount('credit')
        elif choice == '3':
            self.operations.prompt_for_amount('debit')
        elif choice == '4':
            self.continue_flag = False
        else:
            print("Invalid choice, please select 1-4.")

    # Main program loop
    def run(self):
        while self.continue_flag:
            self.display_menu()

            choice = input("Enter your choice (1-4): ").strip()
            self.process_choice(choice)

            if self.continue_flag:
                print()  # Add spacing between operations

        print("Exiting the program. Goodbye!")

# Start the program if this file is run directly
if __name__ == "__main__":
    main_program = MainProgram()
    main_program.run()
