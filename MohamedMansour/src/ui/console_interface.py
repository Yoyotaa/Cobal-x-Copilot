"""
Interface utilisateur console pour le système de gestion de comptes.

Ce module gère l'interaction avec l'utilisateur via la console,
équivalent aux DISPLAY/ACCEPT du module main.cob en COBOL.
"""

from decimal import Decimal, InvalidOperation
from typing import Optional
import logging
import sys

from services.account_service import AccountService

logger = logging.getLogger(__name__)


class ConsoleInterface:
    """
    Interface utilisateur console pour le système de gestion de comptes.
    
    Équivalent COBOL : MainProgram avec DISPLAY/ACCEPT
    
    Responsabilités :
    - Affichage du menu principal
    - Gestion des choix utilisateur
    - Validation des saisies
    - Affichage des résultats
    """
    
    def __init__(self, account_service: AccountService = None):
        """
        Initialise l'interface console.
        
        Args:
            account_service: Service des opérations bancaires.
                           Si None, crée une instance par défaut.
        """
        self.account_service = account_service or AccountService()
        self.continue_flag = True  # Équivalent COBOL : CONTINUE-FLAG
        # logger.info("Interface console initialisée")
    
    def display_menu(self) -> None:
        """
        Affiche le menu principal.
        
        Équivalent COBOL :
        DISPLAY "--------------------------------"
        DISPLAY "Account Management System"
        DISPLAY "1. View Balance"
        DISPLAY "2. Credit Account"  
        DISPLAY "3. Debit Account"
        DISPLAY "4. Exit"
        DISPLAY "--------------------------------"
        """
        print("\n" + "=" * 40)
        print("Account Management System")
        print("=" * 40)
        print("1. View Balance")
        print("2. Credit Account")
        print("3. Debit Account")
        print("4. Exit")
        print("=" * 40)
    
    def get_user_choice(self) -> int:
        """
        Demande et valide le choix de l'utilisateur.
        
        Équivalent COBOL :
        DISPLAY "Enter your choice (1-4): "
        ACCEPT USER-CHOICE
        
        Returns:
            int: Choix de l'utilisateur (1-4) ou 0 si invalide
        """
        try:
            choice_str = input("Enter your choice (1-4): ").strip()
            choice = int(choice_str)
            
            if choice in [1, 2, 3, 4]:
                return choice
            else:
                print("Invalid choice, please select 1-4.")
                return 0
                
        except ValueError:
            print("Invalid input, please enter a number between 1-4.")
            return 0
        except (EOFError, KeyboardInterrupt):
            print("\nProgram interrupted by user.")
            return 4  # Traiter comme une sortie
    
    def get_amount_input(self, operation_type: str) -> Optional[Decimal]:
        """
        Demande et valide un montant à l'utilisateur.
        
        Équivalent COBOL :
        DISPLAY "Enter credit/debit amount: "
        ACCEPT AMOUNT
        
        Args:
            operation_type: 'credit' ou 'debit' pour le message
            
        Returns:
            Optional[Decimal]: Montant saisi ou None si invalide/annulé
        """
        try:
            prompt = f"Enter {operation_type} amount: "
            amount_str = input(prompt).strip()
            
            if not amount_str:
                print("Amount cannot be empty.")
                return None
            
            # Permettre l'annulation avec 'q' ou 'quit'
            if amount_str.lower() in ['q', 'quit', 'cancel']:
                print("Operation cancelled.")
                return None
            
            amount = Decimal(amount_str)
            
            if amount <= 0:
                print(f"The {operation_type} amount must be positive.")
                return None
            
            # Validation : montant raisonnable (pas plus de 6 chiffres + 2 décimales)
            # Équivalent COBOL : PIC 9(6)V99
            if amount >= Decimal('1000000.00'):
                print("Amount too large. Maximum is 999999.99")
                return None
            
            return amount
            
        except InvalidOperation:
            print("Invalid amount format. Please enter a valid number.")
            return None
        except (EOFError, KeyboardInterrupt):
            print("\nOperation cancelled by user.")
            return None
    
    def handle_view_balance(self) -> None:
        """
        Gère l'affichage du solde.
        
        Équivalent COBOL :
        WHEN 1
            CALL 'Operations' USING 'TOTAL '
        """
        # logger.info("Traitement : consultation du solde")
        success, message, balance = self.account_service.view_balance()
        
        if success:
            print(f"\n✓ {message}")
        else:
            print(f"\n✗ Error: {message}")
            # logger.error(f"Erreur consultation solde: {message}")
    
    def handle_credit_account(self) -> None:
        """
        Gère le crédit du compte.
        
        Équivalent COBOL :
        WHEN 2
            CALL 'Operations' USING 'CREDIT'
        """
        # logger.info("Traitement : crédit du compte")
        amount = self.get_amount_input("credit")
        
        if amount is None:
            return  # Opération annulée ou montant invalide
        
        success, message, new_balance = self.account_service.credit_account(amount)
        
        if success:
            print(f"\n✓ {message}")
        else:
            print(f"\n✗ Error: {message}")
            # logger.error(f"Erreur crédit: {message}")
    
    def handle_debit_account(self) -> None:
        """
        Gère le débit du compte.
        
        Équivalent COBOL :
        WHEN 3
            CALL 'Operations' USING 'DEBIT '
        """
        # logger.info("Traitement : débit du compte")
        amount = self.get_amount_input("debit")
        
        if amount is None:
            return  # Opération annulée ou montant invalide
        
        success, message, current_balance = self.account_service.debit_account(amount)
        
        if success:
            print(f"\n✓ {message}")
        else:
            print(f"\n✗ {message}")
            if "Insufficient funds" in message:
                                # logger.warning(f"Tentative de débit avec fonds insuffisants: {amount}")
                pass
            else:
                                # logger.error(f"Erreur débit: {message}")
                pass
    
    def handle_exit(self) -> None:
        """
        Gère la sortie du programme.
        
        Équivalent COBOL :
        WHEN 4
            MOVE 'NO' TO CONTINUE-FLAG
        """
        # logger.info("Demande de sortie du programme")
        self.continue_flag = False
        print("\nExiting the program. Goodbye!")
    
    def run(self) -> None:
        """
        Boucle principale de l'interface utilisateur.
        
        Équivalent COBOL :
        PERFORM UNTIL CONTINUE-FLAG = 'NO'
            [menu and operations]
        END-PERFORM
        """
        # logger.info("Démarrage de l'interface console")
        print("Welcome to the Account Management System!")
        
        try:
            while self.continue_flag:
                self.display_menu()
                choice = self.get_user_choice()
                
                # Équivalent COBOL : EVALUATE USER-CHOICE
                if choice == 1:
                    self.handle_view_balance()
                elif choice == 2:
                    self.handle_credit_account()
                elif choice == 3:
                    self.handle_debit_account()
                elif choice == 4:
                    self.handle_exit()
                # choice == 0 : choix invalide, déjà géré dans get_user_choice
                
        except KeyboardInterrupt:
            print("\n\nProgram interrupted. Goodbye!")
            # logger.info("Programme interrompu par l'utilisateur")
        except Exception as e:
            print(f"\nUnexpected error: {e}")
            # logger.error(f"Erreur inattendue dans l'interface: {e}")
            sys.exit(1)
        
        # logger.info("Fin de l'interface console")
    
    def display_account_info(self) -> None:
        """
        Affiche les informations détaillées du compte (mode debug).
        """
        info = self.account_service.get_account_info()
        if info:
            print("\n" + "-" * 30)
            print("Account Information:")
            print(f"  Account ID: {info.get('account_id', 'N/A')}")
            print(f"  Balance: {info.get('balance', 'N/A'):.2f}")
            print(f"  Created: {info.get('created_at', 'N/A')}")
            print(f"  Last Modified: {info.get('last_modified', 'N/A')}")
            print("-" * 30)
        else:
            print("Unable to retrieve account information.")
    
    def reset_account_interactive(self) -> None:
        """
        Remet le compte à zéro avec confirmation utilisateur.
        """
        print("\n⚠️  WARNING: This will reset the account balance to 1000.00")
        confirm = input("Are you sure? (yes/no): ").strip().lower()
        
        if confirm in ['yes', 'y']:
            success, message, balance = self.account_service.reset_account()
            if success:
                print(f"\n✓ {message}")
            else:
                print(f"\n✗ Error: {message}")
        else:
            print("Reset operation cancelled.")
