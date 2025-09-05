"""
Module des opérations bancaires - Équivalent Python de operations.cob
Gère les opérations de crédit, débit et consultation du solde.
"""

from typing import Optional
from data_manager import DataManager


class BankOperations:
    """
    Classe responsable des opérations bancaires.
    Équivalent au module Operations en COBOL.
    """
    
    def __init__(self, data_manager: Optional[DataManager] = None):
        """
        Initialise les opérations bancaires.
        
        Args:
            data_manager (DataManager, optional): Gestionnaire de données.
                Si None, un nouveau DataManager sera créé.
        """
        self.data_manager = data_manager or DataManager()
    
    def view_balance(self) -> float:
        """
        Affiche le solde actuel du compte.
        Équivalent à l'opération 'TOTAL' en COBOL.
        
        Returns:
            float: Solde actuel du compte
        """
        balance = self.data_manager.read_balance()
        print(f"Solde actuel: {balance:.2f} €")
        return balance
    
    def credit_account(self, amount: Optional[float] = None) -> float:
        """
        Crédite le compte avec un montant spécifié.
        Équivalent à l'opération 'CREDIT' en COBOL.
        
        Args:
            amount (float, optional): Montant à créditer.
                Si None, demande à l'utilisateur.
        
        Returns:
            float: Nouveau solde après crédit
        """
        if amount is None:
            amount = self._get_amount_from_user("Montant à créditer")
        
        if amount <= 0:
            print("Erreur: Le montant doit être positif.")
            return self.data_manager.get_balance()
        
        current_balance = self.data_manager.read_balance()
        new_balance = current_balance + amount
        self.data_manager.write_balance(new_balance)
        
        print(f"Montant crédité: {amount:.2f} €")
        print(f"Nouveau solde: {new_balance:.2f} €")
        
        return new_balance
    
    def debit_account(self, amount: Optional[float] = None) -> float:
        """
        Débite le compte d'un montant spécifié.
        Équivalent à l'opération 'DEBIT' en COBOL.
        
        Args:
            amount (float, optional): Montant à débiter.
                Si None, demande à l'utilisateur.
        
        Returns:
            float: Nouveau solde après débit (ou solde actuel si insuffisant)
        """
        if amount is None:
            amount = self._get_amount_from_user("Montant à débiter")
        
        if amount <= 0:
            print("Erreur: Le montant doit être positif.")
            return self.data_manager.get_balance()
        
        current_balance = self.data_manager.read_balance()
        
        if current_balance >= amount:
            new_balance = current_balance - amount
            self.data_manager.write_balance(new_balance)
            
            print(f"Montant débité: {amount:.2f} €")
            print(f"Nouveau solde: {new_balance:.2f} €")
            
            return new_balance
        else:
            print("Fonds insuffisants pour effectuer ce débit.")
            print(f"Solde disponible: {current_balance:.2f} €")
            print(f"Montant demandé: {amount:.2f} €")
            return current_balance
    
    def _get_amount_from_user(self, prompt: str) -> float:
        """
        Demande un montant à l'utilisateur avec validation.
        
        Args:
            prompt (str): Message à afficher à l'utilisateur
        
        Returns:
            float: Montant saisi par l'utilisateur
        """
        while True:
            try:
                user_input = input(f"{prompt}: ")
                amount = float(user_input)
                if amount < 0:
                    print("Erreur: Le montant ne peut pas être négatif.")
                    continue
                return amount
            except ValueError:
                print("Erreur: Veuillez saisir un montant valide (nombre).")
    
    def get_current_balance(self) -> float:
        """
        Récupère le solde actuel sans l'afficher.
        
        Returns:
            float: Solde actuel du compte
        """
        return self.data_manager.get_balance()
