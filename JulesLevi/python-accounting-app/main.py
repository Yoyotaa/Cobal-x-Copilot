"""
Main program for the Account Management System.
Provides a user interface for account operations.

This is the Python equivalent of the COBOL main.cob file.
"""

from operations import Operations

class MainProgram:
    def __init__(self):
        """Initialize the main program with an Operations instance."""
        self.operations = Operations()
        self.continue_flag = True
    
    def display_menu(self):
        """Display the main menu to the user."""
        print("--------------------------------")
        print("Account Management System")
        print("1. View Balance")
        print("2. Credit Account")
        print("3. Debit Account")
        print("4. Exit")
        print("--------------------------------")
    
    def get_user_choice(self):
        """
        Get and validate user choice.
        
        Returns:
            int: User's menu choice (1-4), or 0 if invalid
        """
        try:
            choice = input("Enter your choice (1-4): ")
            return int(choice)
        except ValueError:
            return 0
    
    def process_choice(self, choice):
        """
        Process the user's menu choice.
        
        Args:
            choice (int): The user's menu selection
        """
        if choice == 1:
            # View Balance
            self.operations.perform_operation('TOTAL')
        elif choice == 2:
            # Credit Account
            self.operations.perform_operation('CREDIT')
        elif choice == 3:
            # Debit Account
            self.operations.perform_operation('DEBIT')
        elif choice == 4:
            # Exit
            self.continue_flag = False
        else:
            print("Invalid choice, please select 1-4.")
    
    def run(self):
        """
        Main program loop.
        Displays menu, gets user input, and processes choices until user exits.
        """
        try:
            while self.continue_flag:
                self.display_menu()
                choice = self.get_user_choice()
                self.process_choice(choice)
                
                # Add a blank line for better readability
                if self.continue_flag:
                    print()
            
            print("Exiting the program. Goodbye!")
            
        except KeyboardInterrupt:
            print("\n\nProgram interrupted by user.")
            print("Exiting the program. Goodbye!")
        except Exception as e:
            print(f"An unexpected error occurred: {e}")
            print("Exiting the program. Goodbye!")

def main():
    """Entry point for the application."""
    program = MainProgram()
    program.run()

if __name__ == "__main__":
    main()
