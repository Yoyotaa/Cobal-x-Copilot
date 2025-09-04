"""
Service de gestion des opérations bancaires.

Ce module implémente la logique métier des opérations bancaires,
équivalent au module operations.cob en COBOL.
"""

from decimal import Decimal
from typing import Tuple
import logging

from models.account import Account
from services.data_service import DataService

logger = logging.getLogger(__name__)


class AccountService:
    """
    Service gérant les opérations bancaires.
    
    Équivalent COBOL : Operations program
    
    Responsabilités :
    - Exécution des opérations TOTAL, CREDIT, DEBIT
    - Validation des montants
    - Coordination avec le service de données
    - Gestion des erreurs métier
    """
    
    def __init__(self, data_service: DataService = None):
        """
        Initialise le service d'opérations bancaires.
        
        Args:
            data_service: Service de persistance des données.
                         Si None, crée une instance par défaut.
        """
        self.data_service = data_service or DataService()
        # logger.info("AccountService initialisé")
    
    def view_balance(self) -> Tuple[bool, str, Decimal]:
        """
        Affiche le solde actuel du compte.
        
        Équivalent COBOL :
        IF OPERATION-TYPE = 'TOTAL '
            CALL 'DataProgram' USING 'READ', FINAL-BALANCE
            DISPLAY "Current balance: " FINAL-BALANCE
        
        Returns:
            Tuple[bool, str, Decimal]: (succès, message, solde)
        """
        try:
            account = self.data_service.read_account()
            balance = account.get_balance()
            message = f"Current balance: {balance:.2f}"
            
            # logger.info(f"Consultation du solde: {balance}")
            return True, message, balance
            
        except Exception as e:
            error_msg = f"Erreur lors de la consultation du solde: {e}"
            # logger.error(error_msg)
            return False, error_msg, Decimal('0.00')
    
    def credit_account(self, amount: Decimal) -> Tuple[bool, str, Decimal]:
        """
        Crédite le compte d'un montant donné.
        
        Équivalent COBOL :
        ELSE IF OPERATION-TYPE = 'CREDIT'
            DISPLAY "Enter credit amount: "
            ACCEPT AMOUNT
            CALL 'DataProgram' USING 'read', FINAL-BALANCE
            ADD AMOUNT TO FINAL-BALANCE
            CALL 'DataProgram' USING 'WRITE', FINAL-BALANCE
            DISPLAY "Amount credited. New balance: " FINAL-BALANCE
        
        Args:
            amount: Montant à créditer
            
        Returns:
            Tuple[bool, str, Decimal]: (succès, message, nouveau_solde)
        """
        try:
            # Validation du montant
            if isinstance(amount, (int, float, str)):
                amount = Decimal(str(amount))
            
            if amount <= 0:
                error_msg = "Le montant du crédit doit être positif"
                # logger.warning(f"Tentative de crédit avec montant invalide: {amount}")
                return False, error_msg, Decimal('0.00')
            
            # Charger le compte actuel
            account = self.data_service.read_account()
            old_balance = account.get_balance()
            
            # Effectuer le crédit
            success = account.credit(amount)
            if not success:
                error_msg = "Échec du crédit"
                return False, error_msg, old_balance
            
            # Sauvegarder les modifications
            save_success = self.data_service.write_account(account)
            if not save_success:
                error_msg = "Erreur lors de la sauvegarde du crédit"
                # logger.error("Impossible de sauvegarder après crédit")
                return False, error_msg, old_balance
            
            new_balance = account.get_balance()
            message = f"Amount credited. New balance: {new_balance:.2f}"
            
            # logger.info(f"Crédit réussi: {amount} -> nouveau solde: {new_balance}")
            return True, message, new_balance
            
        except ValueError as e:
            error_msg = f"Montant invalide: {e}"
            # logger.error(error_msg)
            return False, error_msg, Decimal('0.00')
        except Exception as e:
            error_msg = f"Erreur lors du crédit: {e}"
            # logger.error(error_msg)
            return False, error_msg, Decimal('0.00')
    
    def debit_account(self, amount: Decimal) -> Tuple[bool, str, Decimal]:
        """
        Débite le compte d'un montant donné si les fonds sont suffisants.
        
        Équivalent COBOL :
        ELSE IF OPERATION-TYPE = 'DEBIT '
            DISPLAY "Enter debit amount: "
            ACCEPT AMOUNT
            CALL 'DataProgram' USING 'read', FINAL-BALANCE
            IF FINAL-BALANCE >= AMOUNT
                SUBTRACT AMOUNT FROM FINAL-BALANCE
                CALL 'DataProgram' USING 'WRITE', FINAL-BALANCE
                DISPLAY "Amount debited. New balance: " FINAL-BALANCE
            ELSE
                DISPLAY "Insufficient funds for this debit."
            END-IF
        
        Args:
            amount: Montant à débiter
            
        Returns:
            Tuple[bool, str, Decimal]: (succès, message, solde_actuel)
        """
        try:
            # Validation du montant
            if isinstance(amount, (int, float, str)):
                amount = Decimal(str(amount))
            
            if amount <= 0:
                error_msg = "Le montant du débit doit être positif"
                # logger.warning(f"Tentative de débit avec montant invalide: {amount}")
                return False, error_msg, Decimal('0.00')
            
            # Charger le compte actuel
            account = self.data_service.read_account()
            current_balance = account.get_balance()
            
            # Vérifier les fonds suffisants
            if current_balance < amount:
                message = "Insufficient funds for this debit."
                # logger.warning(f"Fonds insuffisants: solde={current_balance}, débit={amount}")
                return False, message, current_balance
            
            # Effectuer le débit
            success = account.debit(amount)
            if not success:
                # Ceci ne devrait pas arriver car on a déjà vérifié les fonds
                error_msg = "Échec du débit"
                return False, error_msg, current_balance
            
            # Sauvegarder les modifications
            save_success = self.data_service.write_account(account)
            if not save_success:
                error_msg = "Erreur lors de la sauvegarde du débit"
                # logger.error("Impossible de sauvegarder après débit")
                # En cas d'erreur de sauvegarde, on ne peut pas garantir l'état
                return False, error_msg, current_balance
            
            new_balance = account.get_balance()
            message = f"Amount debited. New balance: {new_balance:.2f}"
            
            # logger.info(f"Débit réussi: {amount} -> nouveau solde: {new_balance}")
            return True, message, new_balance
            
        except ValueError as e:
            error_msg = f"Montant invalide: {e}"
            # logger.error(error_msg)
            return False, error_msg, Decimal('0.00')
        except Exception as e:
            error_msg = f"Erreur lors du débit: {e}"
            # logger.error(error_msg)
            return False, error_msg, Decimal('0.00')
    
    def process_operation(self, operation_type: str, amount: Decimal = None) -> Tuple[bool, str, Decimal]:
        """
        Traite une opération selon son type.
        
        Équivalent COBOL : Structure EVALUATE WHEN dans operations.cob
        
        Args:
            operation_type: Type d'opération ('TOTAL', 'CREDIT', 'DEBIT')
            amount: Montant pour les opérations CREDIT/DEBIT
            
        Returns:
            Tuple[bool, str, Decimal]: (succès, message, solde_résultant)
        """
        operation_type = operation_type.upper().strip()
        
        if operation_type == 'TOTAL':
            return self.view_balance()
        elif operation_type == 'CREDIT':
            if amount is None:
                return False, "Montant requis pour l'opération de crédit", Decimal('0.00')
            return self.credit_account(amount)
        elif operation_type == 'DEBIT':
            if amount is None:
                return False, "Montant requis pour l'opération de débit", Decimal('0.00')
            return self.debit_account(amount)
        else:
            error_msg = f"Type d'opération non reconnu: {operation_type}"
            # logger.error(error_msg)
            return False, error_msg, Decimal('0.00')
    
    def get_account_info(self) -> dict:
        """
        Retourne les informations complètes du compte.
        
        Returns:
            dict: Informations du compte
        """
        try:
            account = self.data_service.read_account()
            return {
                'balance': account.balance,
                'account_id': account.account_id,
                'created_at': account.created_at,
                'last_modified': account.last_modified
            }
        except Exception as e:
            # logger.error(f"Erreur lors de la récupération des infos du compte: {e}")
            return {}
    
    def reset_account(self) -> Tuple[bool, str, Decimal]:
        """
        Remet le compte à son état initial.
        
        Returns:
            Tuple[bool, str, Decimal]: (succès, message, nouveau_solde)
        """
        try:
            account = self.data_service.reset_account()
            message = f"Compte réinitialisé. Solde: {account.balance:.2f}"
            # logger.info("Compte réinitialisé avec succès")
            return True, message, account.balance
        except Exception as e:
            error_msg = f"Erreur lors de la réinitialisation: {e}"
            # logger.error(error_msg)
            return False, error_msg, Decimal('0.00')
