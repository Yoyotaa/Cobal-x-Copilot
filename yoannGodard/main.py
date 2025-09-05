#!/usr/bin/env python3
"""
Programme principal - Équivalent Python de main.cob
Interface utilisateur pour le système de gestion de compte bancaire.
"""

import sys
from typing import Optional
from operations import BankOperations


class BankAccountManager:
    """
    Classe principale gérant l'interface utilisateur du système bancaire.
    Équivalent au programme MainProgram en COBOL.
    """
    
    def __init__(self):
        """Initialise le gestionnaire de compte bancaire."""
        self.operations = BankOperations()
        self.running = True
    
    def display_menu(self) -> None:
        """
        Affiche le menu principal de l'application.
        """
        print("\n" + "=" * 40)
        print("    SYSTÈME DE GESTION DE COMPTE")
        print("=" * 40)
        print("1. Consulter le solde")
        print("2. Créditer le compte")
        print("3. Débiter le compte")
        print("4. Quitter")
        print("=" * 40)
    
    def get_user_choice(self) -> int:
        """
        Récupère et valide le choix de l'utilisateur.
        
        Returns:
            int: Choix de l'utilisateur (1-4)
        """
        while True:
            try:
                choice = input("Entrez votre choix (1-4): ").strip()
                choice_int = int(choice)
                
                if 1 <= choice_int <= 4:
                    return choice_int
                else:
                    print("Erreur: Veuillez sélectionner un choix entre 1 et 4.")
            except ValueError:
                print("Erreur: Veuillez saisir un nombre valide.")
    
    def handle_choice(self, choice: int) -> None:
        """
        Traite le choix de l'utilisateur.
        
        Args:
            choice (int): Choix de l'utilisateur
        """
        if choice == 1:
            self.operations.view_balance()
        elif choice == 2:
            self.operations.credit_account()
        elif choice == 3:
            self.operations.debit_account()
        elif choice == 4:
            self.running = False
            print("Sortie du programme. Au revoir!")
    
    def run(self) -> None:
        """
        Lance la boucle principale de l'application.
        """
        print("Bienvenue dans le système de gestion de compte bancaire!")
        
        while self.running:
            self.display_menu()
            choice = self.get_user_choice()
            self.handle_choice(choice)
            
            if self.running:
                input("\nAppuyez sur Entrée pour continuer...")


def main():
    """
    Fonction principale du programme.
    """
    try:
        manager = BankAccountManager()
        manager.run()
    except KeyboardInterrupt:
        print("\n\nProgramme interrompu par l'utilisateur.")
        print("Au revoir!")
    except Exception as e:
        print(f"\nErreur inattendue: {e}")
        print("Le programme va se terminer.")
        sys.exit(1)


if __name__ == "__main__":
    main()
