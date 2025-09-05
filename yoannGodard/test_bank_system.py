"""
Tests unitaires pour le système bancaire converti de COBOL vers Python.
Valide que toutes les fonctionnalités originales sont préservées.
"""

import unittest
import tempfile
import os
import json
from unittest.mock import patch, MagicMock

from data_manager import DataManager
from operations import BankOperations


class TestDataManager(unittest.TestCase):
    """Tests pour le module de gestion des données."""
    
    def setUp(self):
        """Configuration initiale pour chaque test."""
        # Créer un fichier temporaire pour les tests
        self.temp_file = tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.json')
        self.temp_file.close()
        self.data_manager = DataManager(self.temp_file.name)
    
    def tearDown(self):
        """Nettoyage après chaque test."""
        if os.path.exists(self.temp_file.name):
            os.unlink(self.temp_file.name)
    
    def test_initial_balance(self):
        """Test que le solde initial est correct (1000.00 comme en COBOL)."""
        balance = self.data_manager.read_balance()
        self.assertEqual(balance, 1000.00)
    
    def test_write_and_read_balance(self):
        """Test l'écriture et la lecture du solde."""
        test_balance = 1500.50
        self.data_manager.write_balance(test_balance)
        read_balance = self.data_manager.read_balance()
        self.assertEqual(read_balance, test_balance)
    
    def test_file_creation(self):
        """Test que le fichier de données est créé automatiquement."""
        new_file = "test_account.json"
        if os.path.exists(new_file):
            os.unlink(new_file)
        
        dm = DataManager(new_file)
        self.assertTrue(os.path.exists(new_file))
        
        # Nettoyage
        os.unlink(new_file)
    
    def test_corrupted_file_handling(self):
        """Test la gestion des fichiers corrompus."""
        # Créer un fichier corrompu
        with open(self.temp_file.name, 'w') as f:
            f.write("invalid json content")
        
        # Le gestionnaire doit retourner le solde par défaut
        balance = self.data_manager.read_balance()
        self.assertEqual(balance, 1000.00)


class TestBankOperations(unittest.TestCase):
    """Tests pour le module des opérations bancaires."""
    
    def setUp(self):
        """Configuration initiale pour chaque test."""
        self.temp_file = tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.json')
        self.temp_file.close()
        self.data_manager = DataManager(self.temp_file.name)
        self.operations = BankOperations(self.data_manager)
    
    def tearDown(self):
        """Nettoyage après chaque test."""
        if os.path.exists(self.temp_file.name):
            os.unlink(self.temp_file.name)
    
    def test_view_balance(self):
        """Test de la consultation du solde."""
        with patch('builtins.print') as mock_print:
            balance = self.operations.view_balance()
            mock_print.assert_called_with("Solde actuel: 1000.00 €")
            self.assertEqual(balance, 1000.00)
    
    def test_credit_account(self):
        """Test du crédit de compte."""
        with patch('builtins.print') as mock_print:
            new_balance = self.operations.credit_account(500.00)
            self.assertEqual(new_balance, 1500.00)
            # Vérifier que les messages appropriés ont été affichés
            calls = [call[0][0] for call in mock_print.call_args_list]
            self.assertIn("Montant crédité: 500.00 €", calls)
            self.assertIn("Nouveau solde: 1500.00 €", calls)
    
    def test_debit_account_sufficient_funds(self):
        """Test du débit avec fonds suffisants."""
        with patch('builtins.print') as mock_print:
            new_balance = self.operations.debit_account(300.00)
            self.assertEqual(new_balance, 700.00)
            calls = [call[0][0] for call in mock_print.call_args_list]
            self.assertIn("Montant débité: 300.00 €", calls)
            self.assertIn("Nouveau solde: 700.00 €", calls)
    
    def test_debit_account_insufficient_funds(self):
        """Test du débit avec fonds insuffisants."""
        with patch('builtins.print') as mock_print:
            new_balance = self.operations.debit_account(1500.00)
            self.assertEqual(new_balance, 1000.00)  # Solde inchangé
            calls = [call[0][0] for call in mock_print.call_args_list]
            self.assertIn("Fonds insuffisants pour effectuer ce débit.", calls)
    
    def test_credit_negative_amount(self):
        """Test du crédit avec un montant négatif."""
        with patch('builtins.print') as mock_print:
            new_balance = self.operations.credit_account(-100.00)
            self.assertEqual(new_balance, 1000.00)  # Solde inchangé
            mock_print.assert_called_with("Erreur: Le montant doit être positif.")
    
    def test_debit_negative_amount(self):
        """Test du débit avec un montant négatif."""
        with patch('builtins.print') as mock_print:
            new_balance = self.operations.debit_account(-100.00)
            self.assertEqual(new_balance, 1000.00)  # Solde inchangé
            mock_print.assert_called_with("Erreur: Le montant doit être positif.")
    
    @patch('builtins.input', side_effect=['500.00'])
    def test_credit_with_user_input(self, mock_input):
        """Test du crédit avec saisie utilisateur."""
        with patch('builtins.print') as mock_print:
            new_balance = self.operations.credit_account()
            self.assertEqual(new_balance, 1500.00)
            mock_input.assert_called_once_with("Montant à créditer: ")
    
    @patch('builtins.input', side_effect=['invalid', '300.00'])
    def test_credit_with_invalid_input(self, mock_input):
        """Test du crédit avec saisie invalide puis valide."""
        with patch('builtins.print') as mock_print:
            new_balance = self.operations.credit_account()
            self.assertEqual(new_balance, 1300.00)
            # Vérifier que l'erreur a été affichée
            calls = [call[0][0] for call in mock_print.call_args_list]
            self.assertIn("Erreur: Veuillez saisir un montant valide (nombre).", calls)


class TestIntegration(unittest.TestCase):
    """Tests d'intégration pour valider le comportement global."""
    
    def setUp(self):
        """Configuration initiale pour chaque test."""
        self.temp_file = tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.json')
        self.temp_file.close()
        self.data_manager = DataManager(self.temp_file.name)
        self.operations = BankOperations(self.data_manager)
    
    def tearDown(self):
        """Nettoyage après chaque test."""
        if os.path.exists(self.temp_file.name):
            os.unlink(self.temp_file.name)
    
    def test_complete_banking_workflow(self):
        """Test d'un workflow bancaire complet."""
        # Solde initial
        initial_balance = self.operations.get_current_balance()
        self.assertEqual(initial_balance, 1000.00)
        
        # Crédit
        balance_after_credit = self.operations.credit_account(500.00)
        self.assertEqual(balance_after_credit, 1500.00)
        
        # Débit
        balance_after_debit = self.operations.debit_account(200.00)
        self.assertEqual(balance_after_debit, 1300.00)
        
        # Vérification finale
        final_balance = self.operations.get_current_balance()
        self.assertEqual(final_balance, 1300.00)
    
    def test_data_persistence(self):
        """Test que les données persistent entre les sessions."""
        # Première session
        self.operations.credit_account(500.00)
        
        # Nouvelle instance (nouvelle session)
        new_data_manager = DataManager(self.temp_file.name)
        new_operations = BankOperations(new_data_manager)
        
        # Vérifier que le solde persiste
        balance = new_operations.get_current_balance()
        self.assertEqual(balance, 1500.00)


if __name__ == '__main__':
    # Exécuter les tests avec plus de détails
    unittest.main(verbosity=2)
