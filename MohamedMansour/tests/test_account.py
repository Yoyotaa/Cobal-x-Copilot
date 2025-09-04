"""
Tests unitaires pour le modèle Account.

Ces tests valident que la logique métier du modèle Account
respecte fidèlement le comportement du COBOL original.
"""

import pytest
from decimal import Decimal
from datetime import datetime
import sys
from pathlib import Path

# Ajouter src au PYTHONPATH pour les tests
sys.path.insert(0, str(Path(__file__).parent.parent / "src"))

from models.account import Account


class TestAccount:
    """Tests pour la classe Account."""
    
    def test_account_creation_default_values(self):
        """Test TC-INIT-1: Création d'un compte avec valeurs par défaut."""
        # Équivalent COBOL : PIC 9(6)V99 VALUE 1000.00
        account = Account()
        
        assert account.balance == Decimal('1000.00')
        assert account.account_id == 'DEFAULT_ACCOUNT'
        assert account.created_at is not None
        assert account.last_modified is not None
    
    def test_account_creation_custom_balance(self):
        """Test TC-INIT-2: Création d'un compte avec solde personnalisé."""
        account = Account(balance=Decimal('500.50'))
        
        assert account.balance == Decimal('500.50')
    
    def test_account_creation_negative_balance_correction(self):
        """Test TC-INIT-3: Correction automatique d'un solde négatif."""
        account = Account(balance=Decimal('-100.00'))
        
        # Doit être corrigé à 0.00
        assert account.balance == Decimal('0.00')
    
    def test_balance_precision_normalization(self):
        """Test TC-INIT-4: Normalisation de la précision décimale."""
        # Test avec différents formats d'entrée
        test_cases = [
            ('1000', Decimal('1000.00')),
            ('1000.1', Decimal('1000.10')),
            ('1000.123', Decimal('1000.12')),  # Arrondi à 2 décimales
            (1000.50, Decimal('1000.50')),
        ]
        
        for input_val, expected in test_cases:
            account = Account(balance=input_val)
            assert account.balance == expected
    
    def test_credit_valid_amount(self):
        """Test TC-2.1: Crédit avec montant valide (équivalent test plan)."""
        # Équivalent COBOL : ADD AMOUNT TO FINAL-BALANCE
        account = Account()  # Solde initial 1000.00
        initial_balance = account.balance
        
        success = account.credit(Decimal('100.00'))
        
        assert success is True
        assert account.balance == initial_balance + Decimal('100.00')
        assert account.balance == Decimal('1100.00')
    
    def test_credit_zero_amount_error(self):
        """Test TC-2.2: Crédit avec montant zéro (doit lever une erreur)."""
        account = Account()
        
        with pytest.raises(ValueError, match="Le montant du crédit doit être positif"):
            account.credit(Decimal('0.00'))
    
    def test_credit_negative_amount_error(self):
        """Test TC-2.3: Crédit avec montant négatif (doit lever une erreur)."""
        account = Account()
        
        with pytest.raises(ValueError, match="Le montant du crédit doit être positif"):
            account.credit(Decimal('-50.00'))
    
    def test_credit_string_amount_conversion(self):
        """Test TC-2.4: Crédit avec montant en chaîne de caractères."""
        account = Account()
        
        success = account.credit('250.75')
        
        assert success is True
        assert account.balance == Decimal('1250.75')
    
    def test_debit_valid_amount_sufficient_funds(self):
        """Test TC-3.1: Débit avec montant valide et fonds suffisants."""
        # Équivalent COBOL : IF FINAL-BALANCE >= AMOUNT SUBTRACT AMOUNT FROM FINAL-BALANCE
        account = Account()  # Solde initial 1000.00
        initial_balance = account.balance
        
        success = account.debit(Decimal('50.00'))
        
        assert success is True
        assert account.balance == initial_balance - Decimal('50.00')
        assert account.balance == Decimal('950.00')
    
    def test_debit_amount_greater_than_balance(self):
        """Test TC-3.2: Débit avec montant supérieur au solde."""
        # Équivalent COBOL : ELSE DISPLAY "Insufficient funds for this debit."
        account = Account()  # Solde initial 1000.00
        initial_balance = account.balance
        
        success = account.debit(Decimal('2000.00'))
        
        assert success is False
        assert account.balance == initial_balance  # Solde inchangé
        assert account.balance == Decimal('1000.00')
    
    def test_debit_exact_balance_amount(self):
        """Test TC-3.3: Débit avec montant égal au solde."""
        account = Account()  # Solde initial 1000.00
        
        success = account.debit(Decimal('1000.00'))
        
        assert success is True
        assert account.balance == Decimal('0.00')
    
    def test_debit_zero_amount_error(self):
        """Test TC-3.4: Débit avec montant zéro (doit lever une erreur)."""
        account = Account()
        
        with pytest.raises(ValueError, match="Le montant du débit doit être positif"):
            account.debit(Decimal('0.00'))
    
    def test_debit_negative_amount_error(self):
        """Test TC-3.5: Débit avec montant négatif (doit lever une erreur)."""
        account = Account()
        
        with pytest.raises(ValueError, match="Le montant du débit doit être positif"):
            account.debit(Decimal('-100.00'))
    
    def test_get_balance(self):
        """Test TC-1.1: Consultation du solde actuel."""
        # Équivalent COBOL : DISPLAY "Current balance: " FINAL-BALANCE
        account = Account(balance=Decimal('750.25'))
        
        balance = account.get_balance()
        
        assert balance == Decimal('750.25')
        assert isinstance(balance, Decimal)
    
    def test_last_modified_update_on_credit(self):
        """Test TC-AUDIT-1: Mise à jour de last_modified lors d'un crédit."""
        account = Account()
        initial_time = account.last_modified
        
        # Attendre un petit délai pour s'assurer que le timestamp change
        import time
        time.sleep(0.01)
        
        account.credit(Decimal('100.00'))
        
        assert account.last_modified > initial_time
    
    def test_last_modified_update_on_debit(self):
        """Test TC-AUDIT-2: Mise à jour de last_modified lors d'un débit."""
        account = Account()
        initial_time = account.last_modified
        
        import time
        time.sleep(0.01)
        
        account.debit(Decimal('100.00'))
        
        assert account.last_modified > initial_time
    
    def test_to_dict_serialization(self):
        """Test TC-SERIAL-1: Sérialisation en dictionnaire."""
        account = Account(
            balance=Decimal('1500.75'),
            account_id='TEST_ACCOUNT'
        )
        
        data = account.to_dict()
        
        assert data['balance'] == '1500.75'
        assert data['account_id'] == 'TEST_ACCOUNT'
        assert 'created_at' in data
        assert 'last_modified' in data
    
    def test_from_dict_deserialization(self):
        """Test TC-SERIAL-2: Désérialisation depuis un dictionnaire."""
        data = {
            'balance': '2500.50',
            'account_id': 'RESTORED_ACCOUNT',
            'created_at': '2024-01-01T12:00:00',
            'last_modified': '2024-01-01T12:30:00'
        }
        
        account = Account.from_dict(data)
        
        assert account.balance == Decimal('2500.50')
        assert account.account_id == 'RESTORED_ACCOUNT'
        assert account.created_at == datetime.fromisoformat('2024-01-01T12:00:00')
        assert account.last_modified == datetime.fromisoformat('2024-01-01T12:30:00')
    
    def test_string_representation(self):
        """Test TC-DISPLAY-1: Représentation string du compte."""
        account = Account(
            balance=Decimal('1234.56'),
            account_id='DISPLAY_TEST'
        )
        
        str_repr = str(account)
        
        assert 'DISPLAY_TEST' in str_repr
        assert '1234.56' in str_repr
    
    def test_multiple_operations_sequence(self):
        """Test TC-SEQUENCE-1: Séquence d'opérations multiples."""
        # Simulation d'une session utilisateur complète
        account = Account()  # Solde initial 1000.00
        
        # Crédit de 200.00
        success1 = account.credit(Decimal('200.00'))
        assert success1 is True
        assert account.balance == Decimal('1200.00')
        
        # Débit de 300.00
        success2 = account.debit(Decimal('300.00'))
        assert success2 is True
        assert account.balance == Decimal('900.00')
        
        # Crédit de 150.25
        success3 = account.credit(Decimal('150.25'))
        assert success3 is True
        assert account.balance == Decimal('1050.25')
        
        # Tentative de débit de 2000.00 (fonds insuffisants)
        success4 = account.debit(Decimal('2000.00'))
        assert success4 is False
        assert account.balance == Decimal('1050.25')  # Inchangé
        
        # Débit final de 50.25
        success5 = account.debit(Decimal('50.25'))
        assert success5 is True
        assert account.balance == Decimal('1000.00')  # Retour au solde initial


# Tests d'intégration avec des scénarios métier
class TestAccountBusinessScenarios:
    """Tests de scénarios métier complets."""
    
    def test_daily_banking_scenario(self):
        """Test TC-BUSINESS-1: Scénario bancaire quotidien."""
        account = Account()  # Client commence avec 1000.00
        
        # Dépôt de salaire
        account.credit(Decimal('2500.00'))
        assert account.balance == Decimal('3500.00')
        
        # Retrait au distributeur
        account.debit(Decimal('100.00'))
        assert account.balance == Decimal('3400.00')
        
        # Paiement facture
        account.debit(Decimal('750.50'))
        assert account.balance == Decimal('2649.50')
        
        # Remboursement
        account.credit(Decimal('50.50'))
        assert account.balance == Decimal('2700.00')
    
    def test_insufficient_funds_protection(self):
        """Test TC-BUSINESS-2: Protection contre les découverts."""
        account = Account(balance=Decimal('100.00'))
        
        # Tentative de retrait supérieur au solde
        initial_balance = account.balance
        success = account.debit(Decimal('150.00'))
        
        assert success is False
        assert account.balance == initial_balance
        
        # Vérification qu'on peut toujours faire des opérations valides
        success2 = account.debit(Decimal('50.00'))
        assert success2 is True
        assert account.balance == Decimal('50.00')
