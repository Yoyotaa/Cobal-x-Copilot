"""
Tests unitaires pour le service AccountService.

Ces tests valident la logique métier et équivalent aux tests
du module operations.cob en COBOL selon le plan de test TESTPLAN.md.
"""

import pytest
from decimal import Decimal
import tempfile
from pathlib import Path
import sys

# Ajouter src au PYTHONPATH pour les tests
sys.path.insert(0, str(Path(__file__).parent.parent / "src"))

from models.account import Account
from services.account_service import AccountService
from services.data_service import DataService


class TestAccountService:
    """Tests pour le service AccountService basés sur TESTPLAN.md."""
    
    def setup_method(self):
        """Setup pour chaque test avec un fichier temporaire."""
        self.temp_dir = tempfile.mkdtemp()
        self.temp_file = Path(self.temp_dir) / "test_account.json"
        self.data_service = DataService(str(self.temp_file))
        self.account_service = AccountService(self.data_service)
    
    def teardown_method(self):
        """Cleanup après chaque test."""
        if self.temp_file.exists():
            self.temp_file.unlink()
        self.temp_file.parent.rmdir()
    
    # Tests basés sur le TESTPLAN.md
    
    def test_tc_1_1_view_current_balance(self):
        """Test Case TC-1.1: View Current Balance."""
        # Pre-conditions: Application started (service initialized)
        # Test Steps: Select option 1 to view the balance
        # Expected Result: The application should display the current balance
        
        success, message, balance = self.account_service.view_balance()
        
        assert success is True
        assert "Current balance: 1000.00" in message
        assert balance == Decimal('1000.00')
    
    def test_tc_2_1_credit_account_with_valid_amount(self):
        """Test Case TC-2.1: Credit Account with Valid Amount."""
        # Pre-conditions: Application started
        # Test Steps: Credit account with 100.00
        # Expected Result: New balance after adding the credit amount
        
        success, message, new_balance = self.account_service.credit_account(Decimal('100.00'))
        
        assert success is True
        assert "Amount credited. New balance: 1100.00" in message
        assert new_balance == Decimal('1100.00')
    
    def test_tc_2_2_credit_account_with_zero_amount(self):
        """Test Case TC-2.2: Credit Account with Zero Amount."""
        # Pre-conditions: Application started
        # Test Steps: Credit account with 0.00
        # Expected Result: Error message for invalid amount
        
        success, message, balance = self.account_service.credit_account(Decimal('0.00'))
        
        assert success is False
        assert "positif" in message.lower()
    
    def test_tc_3_1_debit_account_with_valid_amount(self):
        """Test Case TC-3.1: Debit Account with Valid Amount."""
        # Pre-conditions: Application started (balance = 1000.00)
        # Test Steps: Debit account with 50.00 (less than balance)
        # Expected Result: New balance after subtracting the debit amount
        
        success, message, new_balance = self.account_service.debit_account(Decimal('50.00'))
        
        assert success is True
        assert "Amount debited. New balance: 950.00" in message
        assert new_balance == Decimal('950.00')
    
    def test_tc_3_2_debit_account_amount_greater_than_balance(self):
        """Test Case TC-3.2: Debit Account with Amount Greater Than Balance."""
        # Pre-conditions: Application started (balance = 1000.00)
        # Test Steps: Debit account with 2000.00 (greater than balance)
        # Expected Result: "Insufficient funds" message and balance unchanged
        
        success, message, current_balance = self.account_service.debit_account(Decimal('2000.00'))
        
        assert success is False
        assert "Insufficient funds for this debit." in message
        assert current_balance == Decimal('1000.00')
        
        # Vérifier que le solde n'a pas changé en consultant à nouveau
        success2, message2, balance2 = self.account_service.view_balance()
        assert success2 is True
        assert balance2 == Decimal('1000.00')
    
    def test_tc_3_3_debit_account_with_zero_amount(self):
        """Test Case TC-3.3: Debit Account with Zero Amount."""
        # Pre-conditions: Application started
        # Test Steps: Debit account with 0.00
        # Expected Result: Error message for invalid amount
        
        success, message, balance = self.account_service.debit_account(Decimal('0.00'))
        
        assert success is False
        assert "positif" in message.lower()
    
    # Tests additionnels pour la robustesse
    
    def test_credit_with_string_amount(self):
        """Test crédit avec montant en chaîne de caractères."""
        success, message, new_balance = self.account_service.credit_account('250.75')
        
        assert success is True
        assert new_balance == Decimal('1250.75')
    
    def test_debit_with_string_amount(self):
        """Test débit avec montant en chaîne de caractères."""
        success, message, new_balance = self.account_service.debit_account('100.50')
        
        assert success is True
        assert new_balance == Decimal('899.50')
    
    def test_credit_negative_amount(self):
        """Test crédit avec montant négatif."""
        success, message, balance = self.account_service.credit_account(Decimal('-50.00'))
        
        assert success is False
        assert "positif" in message.lower()
    
    def test_debit_negative_amount(self):
        """Test débit avec montant négatif."""
        success, message, balance = self.account_service.debit_account(Decimal('-50.00'))
        
        assert success is False
        assert "positif" in message.lower()
    
    def test_debit_exact_balance_amount(self):
        """Test débit avec montant exact du solde."""
        success, message, new_balance = self.account_service.debit_account(Decimal('1000.00'))
        
        assert success is True
        assert new_balance == Decimal('0.00')
    
    def test_process_operation_total(self):
        """Test de l'opération TOTAL via process_operation."""
        success, message, balance = self.account_service.process_operation('TOTAL')
        
        assert success is True
        assert balance == Decimal('1000.00')
    
    def test_process_operation_credit(self):
        """Test de l'opération CREDIT via process_operation."""
        success, message, balance = self.account_service.process_operation('CREDIT', Decimal('150.00'))
        
        assert success is True
        assert balance == Decimal('1150.00')
    
    def test_process_operation_debit(self):
        """Test de l'opération DEBIT via process_operation."""
        success, message, balance = self.account_service.process_operation('DEBIT', Decimal('200.00'))
        
        assert success is True
        assert balance == Decimal('800.00')
    
    def test_process_operation_invalid_type(self):
        """Test d'opération avec type invalide."""
        success, message, balance = self.account_service.process_operation('INVALID')
        
        assert success is False
        assert "non reconnu" in message.lower()
    
    def test_process_operation_credit_without_amount(self):
        """Test crédit sans montant spécifié."""
        success, message, balance = self.account_service.process_operation('CREDIT')
        
        assert success is False
        assert "requis" in message.lower()
    
    def test_process_operation_debit_without_amount(self):
        """Test débit sans montant spécifié."""
        success, message, balance = self.account_service.process_operation('DEBIT')
        
        assert success is False
        assert "requis" in message.lower()
    
    def test_get_account_info(self):
        """Test récupération des informations du compte."""
        info = self.account_service.get_account_info()
        
        assert 'balance' in info
        assert 'account_id' in info
        assert 'created_at' in info
        assert 'last_modified' in info
        assert info['balance'] == Decimal('1000.00')
    
    def test_reset_account(self):
        """Test remise à zéro du compte."""
        # Modifier d'abord le solde
        self.account_service.credit_account(Decimal('500.00'))
        
        # Reset
        success, message, balance = self.account_service.reset_account()
        
        assert success is True
        assert balance == Decimal('1000.00')
        assert "réinitialisé" in message.lower()
    
    def test_data_persistence_between_operations(self):
        """Test persistance des données entre opérations."""
        # Crédit
        success1, _, balance1 = self.account_service.credit_account(Decimal('300.00'))
        assert success1 is True
        assert balance1 == Decimal('1300.00')
        
        # Créer un nouveau service avec le même fichier
        new_service = AccountService(DataService(str(self.temp_file)))
        
        # Vérifier que le solde est persisté
        success2, _, balance2 = new_service.view_balance()
        assert success2 is True
        assert balance2 == Decimal('1300.00')
    
    def test_multiple_operations_sequence(self):
        """Test séquence d'opérations multiples (simulation session utilisateur)."""
        operations = [
            ('CREDIT', Decimal('500.00'), Decimal('1500.00')),
            ('DEBIT', Decimal('200.00'), Decimal('1300.00')),
            ('CREDIT', Decimal('150.25'), Decimal('1450.25')),
            ('DEBIT', Decimal('50.25'), Decimal('1400.00')),
        ]
        
        for op_type, amount, expected_balance in operations:
            success, message, balance = self.account_service.process_operation(op_type, amount)
            assert success is True, f"Échec de l'opération {op_type} avec {amount}"
            assert balance == expected_balance, f"Solde incorrect après {op_type}: {balance} vs {expected_balance}"
    
    def test_insufficient_funds_edge_cases(self):
        """Test cas limites des fonds insuffisants."""
        # Débit de exactement 1000.01 (1 centime de plus que le solde)
        success, message, balance = self.account_service.debit_account(Decimal('1000.01'))
        
        assert success is False
        assert "Insufficient funds" in message
        assert balance == Decimal('1000.00')
    
    def test_precision_handling(self):
        """Test gestion de la précision décimale."""
        # Crédit avec beaucoup de décimales
        success, message, balance = self.account_service.credit_account(Decimal('123.456789'))
        
        assert success is True
        # Doit être arrondi à 2 décimales
        assert balance == Decimal('1123.46')  # Arrondi à 2 décimales
    
    def test_large_amounts(self):
        """Test avec des montants importants."""
        # Test avec un montant proche de la limite PIC 9(6)V99 (999999.99)
        large_amount = Decimal('50000.00')
        success, message, balance = self.account_service.credit_account(large_amount)
        
        assert success is True
        assert balance == Decimal('51000.00')
    
    def test_error_handling_with_corrupted_data(self):
        """Test gestion d'erreurs avec données corrompues."""
        # Corrompre le fichier de données
        with open(self.temp_file, 'w') as f:
            f.write("{ invalid json")
        
        # Le service doit gérer l'erreur et créer un nouveau compte
        success, message, balance = self.account_service.view_balance()
        
        assert success is True
        assert balance == Decimal('1000.00')  # Solde initial par défaut


class TestAccountServiceIntegration:
    """Tests d'intégration du service AccountService."""
    
    def setup_method(self):
        """Réinitialise le solde avant chaque test d'intégration."""
        # Supprimer le fichier de données pour repartir avec le solde initial
        data_file = Path(__file__).parent.parent / "data" / "account_balance.json"
        if data_file.exists():
            data_file.unlink()
    
    def test_full_business_scenario(self):
        """Test complet d'un scénario métier."""
        # Utiliser le chemin par défaut
        service = AccountService()
        
        # Scénario : consultation -> crédit -> débit -> consultation finale
        
        # 1. Consultation initiale
        success1, msg1, balance1 = service.view_balance()
        assert success1 is True
        assert balance1 == Decimal('1000.00')
        
        # 2. Crédit de salaire
        success2, msg2, balance2 = service.credit_account(Decimal('2500.00'))
        assert success2 is True
        assert balance2 == Decimal('3500.00')
        
        # 3. Débit pour achat
        success3, msg3, balance3 = service.debit_account(Decimal('750.50'))
        assert success3 is True
        assert balance3 == Decimal('2749.50')
        
        # 4. Consultation finale
        success4, msg4, balance4 = service.view_balance()
        assert success4 is True
        assert balance4 == Decimal('2749.50')
        
        # Nettoyage
        service.reset_account()
