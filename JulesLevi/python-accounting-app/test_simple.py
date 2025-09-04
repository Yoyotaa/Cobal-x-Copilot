"""
Version simplifiée de la suite de tests automatisés
Utilise uniquement les modules Python intégrés pour maximum de compatibilité
"""

import unittest
import json
import os
import tempfile
from unittest.mock import patch, MagicMock
from datetime import datetime
import sys

# Add current directory to path
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

from operations import Operations
from data import DataProgram


class TestResult:
    """Classe pour stocker les résultats de test détaillés"""
    def __init__(self, test_id, description, preconditions, test_steps, expected_result):
        self.test_id = test_id
        self.description = description
        self.preconditions = preconditions
        self.test_steps = test_steps
        self.expected_result = expected_result
        self.actual_result = ""
        self.status = "FAIL"
        self.comments = ""
        self.execution_time = None


class SimpleTestSuite(unittest.TestCase):
    """Suite de tests simplifiée et autonome"""
    
    @classmethod
    def setUpClass(cls):
        """Configuration initiale de la classe de test"""
        cls.test_results = []
        print("=" * 60)
        print("SUITE DE TESTS AUTOMATISÉS - APPLICATION COMPTABLE PYTHON")
        print("=" * 60)
        print(f"Début des tests : {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")
        print()
    
    def setUp(self):
        """Configuration avant chaque test"""
        # Créer fichier temporaire
        self.temp_file = tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.json')
        self.temp_file.close()
        
        # Initialiser avec solde par défaut
        with open(self.temp_file.name, 'w') as f:
            json.dump({'balance': 1000.00}, f)
        
        # Créer instance Operations
        self.operations = Operations()
        self.operations.data_program = DataProgram(self.temp_file.name)
    
    def tearDown(self):
        """Nettoyage après chaque test"""
        if os.path.exists(self.temp_file.name):
            os.unlink(self.temp_file.name)
    
    def _record_result(self, test_result, success, actual_result, comments=""):
        """Enregistrer le résultat du test"""
        test_result.actual_result = actual_result
        test_result.status = "PASS" if success else "FAIL"
        test_result.comments = comments
        test_result.execution_time = datetime.now().strftime("%Y-%m-%d %H:%M:%S")
        self.__class__.test_results.append(test_result)
        
        # Afficher résultat immédiat
        status_symbol = "✓" if success else "✗"
        print(f"{status_symbol} {test_result.test_id}: {test_result.description}")
        if not success:
            print(f"  Attendu: {test_result.expected_result}")
            print(f"  Obtenu:  {actual_result}")
        print()
    
    # ==================== TC-1: TESTS CONSULTATION SOLDE ====================
    
    def test_tc_1_1_view_balance_default(self):
        """TC-1.1: Consultation du solde par défaut"""
        test_result = TestResult(
            test_id="TC-1.1",
            description="Consultation du solde avec solde par défaut (1000.00)",
            preconditions="Application initialisée avec solde par défaut de 1000.00",
            test_steps=[
                "1. Initialiser le module Operations",
                "2. Appeler la méthode view_balance()",
                "3. Vérifier l'affichage du solde"
            ],
            expected_result="Solde actuel: 1000.00 affiché et retourné"
        )
        
        with patch('builtins.print') as mock_print:
            result = self.operations.view_balance()
        
        success = (result == 1000.00 and mock_print.called)
        actual_result = f"Solde retourné: {result}, Affichage appelé: {mock_print.called}"
        self._record_result(test_result, success, actual_result)
        
        self.assertEqual(result, 1000.00)
    
    def test_tc_1_2_view_balance_custom(self):
        """TC-1.2: Consultation du solde personnalisé"""
        test_result = TestResult(
            test_id="TC-1.2",
            description="Consultation du solde avec solde personnalisé (2500.75)",
            preconditions="Application initialisée avec solde personnalisé de 2500.75",
            test_steps=[
                "1. Définir le solde à 2500.75",
                "2. Appeler la méthode view_balance()",
                "3. Vérifier l'affichage correct du solde"
            ],
            expected_result="Solde actuel: 2500.75 affiché et retourné"
        )
        
        # Définir solde personnalisé
        with open(self.temp_file.name, 'w') as f:
            json.dump({'balance': 2500.75}, f)
        
        with patch('builtins.print') as mock_print:
            result = self.operations.view_balance()
        
        success = result == 2500.75
        actual_result = f"Solde retourné: {result}"
        self._record_result(test_result, success, actual_result)
        
        self.assertEqual(result, 2500.75)
    
    # ==================== TC-2: TESTS CRÉDIT COMPTE ====================
    
    def test_tc_2_1_credit_valid_amount(self):
        """TC-2.1: Crédit avec montant valide"""
        test_result = TestResult(
            test_id="TC-2.1",
            description="Crédit du compte avec montant valide (250.50)",
            preconditions="Solde du compte est 1000.00",
            test_steps=[
                "1. Saisir montant de crédit de 250.50",
                "2. Appeler la méthode credit_account()",
                "3. Vérifier le calcul du nouveau solde",
                "4. Vérifier la persistance du solde"
            ],
            expected_result="Nouveau solde: 1250.50, message de succès affiché"
        )
        
        with patch('builtins.input', return_value='250.50'):
            with patch('builtins.print') as mock_print:
                result = self.operations.credit_account()
        
        # Vérifier solde stocké
        with open(self.temp_file.name, 'r') as f:
            data = json.load(f)
            new_balance = data['balance']
        
        success = (result == 1250.50 and new_balance == 1250.50)
        actual_result = f"Solde retourné: {result}, Solde stocké: {new_balance}"
        self._record_result(test_result, success, actual_result)
        
        self.assertEqual(result, 1250.50)
    
    def test_tc_2_2_credit_invalid_input(self):
        """TC-2.2: Crédit avec saisie invalide puis valide"""
        test_result = TestResult(
            test_id="TC-2.2",
            description="Crédit avec saisie invalide (-100) puis valide (100)",
            preconditions="Solde du compte est 1000.00",
            test_steps=[
                "1. Saisir montant négatif -100.00",
                "2. Saisir montant valide 100.00 lors de la nouvelle tentative",
                "3. Vérifier la gestion des erreurs",
                "4. Vérifier le succès final"
            ],
            expected_result="Message d'erreur pour montant négatif, puis succès avec montant valide"
        )
        
        with patch('builtins.input', side_effect=['-100.00', '100.00']):
            with patch('builtins.print') as mock_print:
                result = self.operations.credit_account()
        
        success = result == 1100.00
        actual_result = f"Solde final: {result}"
        self._record_result(test_result, success, actual_result, 
                          "Système a correctement géré la saisie négative et recommencé")
        
        self.assertEqual(result, 1100.00)
    
    # ==================== TC-3: TESTS DÉBIT COMPTE ====================
    
    def test_tc_3_1_debit_sufficient_funds(self):
        """TC-3.1: Débit avec fonds suffisants"""
        test_result = TestResult(
            test_id="TC-3.1",
            description="Débit du compte avec montant valide et fonds suffisants (300.00)",
            preconditions="Solde du compte est 1000.00",
            test_steps=[
                "1. Saisir montant de débit de 300.00",
                "2. Appeler la méthode debit_account()",
                "3. Vérifier la vérification des fonds",
                "4. Vérifier le calcul du nouveau solde"
            ],
            expected_result="Nouveau solde: 700.00, message de succès affiché"
        )
        
        with patch('builtins.input', return_value='300.00'):
            with patch('builtins.print') as mock_print:
                result = self.operations.debit_account()
        
        # Vérifier solde stocké
        with open(self.temp_file.name, 'r') as f:
            data = json.load(f)
            new_balance = data['balance']
        
        success = (result == 700.00 and new_balance == 700.00)
        actual_result = f"Solde retourné: {result}, Solde stocké: {new_balance}"
        self._record_result(test_result, success, actual_result)
        
        self.assertEqual(result, 700.00)
    
    def test_tc_3_2_debit_insufficient_funds(self):
        """TC-3.2: Débit avec fonds insuffisants"""
        test_result = TestResult(
            test_id="TC-3.2",
            description="Débit du compte avec montant supérieur au solde (1500.00)",
            preconditions="Solde du compte est 1000.00",
            test_steps=[
                "1. Saisir montant de débit de 1500.00",
                "2. Appeler la méthode debit_account()",
                "3. Vérifier la détection de fonds insuffisants",
                "4. Vérifier que le solde reste inchangé"
            ],
            expected_result="Message de fonds insuffisants, solde reste 1000.00"
        )
        
        with patch('builtins.input', return_value='1500.00'):
            with patch('builtins.print') as mock_print:
                result = self.operations.debit_account()
        
        # Vérifier solde stocké
        with open(self.temp_file.name, 'r') as f:
            data = json.load(f)
            new_balance = data['balance']
        
        success = (result == 1000.00 and new_balance == 1000.00)
        actual_result = f"Solde retourné: {result}, Solde stocké: {new_balance}"
        self._record_result(test_result, success, actual_result,
                          "Fonds insuffisants correctement détectés")
        
        self.assertEqual(result, 1000.00)
    
    def test_tc_3_3_debit_exact_balance(self):
        """TC-3.3: Débit du montant exact du solde"""
        test_result = TestResult(
            test_id="TC-3.3",
            description="Débit du compte avec montant exact du solde (1000.00)",
            preconditions="Solde du compte est 1000.00",
            test_steps=[
                "1. Saisir montant de débit de 1000.00",
                "2. Appeler la méthode debit_account()",
                "3. Vérifier la gestion du montant exact",
                "4. Vérifier que le solde devient zéro"
            ],
            expected_result="Nouveau solde: 0.00, message de succès affiché"
        )
        
        with patch('builtins.input', return_value='1000.00'):
            with patch('builtins.print') as mock_print:
                result = self.operations.debit_account()
        
        # Vérifier solde stocké
        with open(self.temp_file.name, 'r') as f:
            data = json.load(f)
            new_balance = data['balance']
        
        success = (result == 0.00 and new_balance == 0.00)
        actual_result = f"Solde retourné: {result}, Solde stocké: {new_balance}"
        self._record_result(test_result, success, actual_result)
        
        self.assertEqual(result, 0.00)
    
    # ==================== TC-4: TESTS INTÉGRATION ====================
    
    def test_tc_4_1_multiple_operations_sequence(self):
        """TC-4.1: Séquence d'opérations multiples"""
        test_result = TestResult(
            test_id="TC-4.1",
            description="Séquence d'opérations multiples (Crédit→Débit→Consultation)",
            preconditions="Solde du compte est 1000.00",
            test_steps=[
                "1. Créditer 500.00 (solde devrait être 1500.00)",
                "2. Débiter 300.00 (solde devrait être 1200.00)",
                "3. Consulter le solde (devrait afficher 1200.00)",
                "4. Vérifier que toutes les opérations fonctionnent ensemble"
            ],
            expected_result="Solde final: 1200.00 après séquence d'opérations"
        )
        
        # Crédit 500.00
        with patch('builtins.input', return_value='500.00'):
            with patch('builtins.print'):
                credit_result = self.operations.credit_account()
        
        # Débit 300.00
        with patch('builtins.input', return_value='300.00'):
            with patch('builtins.print'):
                debit_result = self.operations.debit_account()
        
        # Consultation solde
        with patch('builtins.print'):
            view_result = self.operations.view_balance()
        
        success = (credit_result == 1500.00 and 
                  debit_result == 1200.00 and 
                  view_result == 1200.00)
        
        actual_result = f"Crédit: {credit_result}, Débit: {debit_result}, Consultation: {view_result}"
        self._record_result(test_result, success, actual_result)
        
        self.assertEqual(view_result, 1200.00)
    
    @classmethod
    def tearDownClass(cls):
        """Génération du rapport final"""
        print("=" * 60)
        print("RAPPORT DE TESTS FINAL")
        print("=" * 60)
        
        # Statistiques
        total_tests = len(cls.test_results)
        passed_tests = sum(1 for result in cls.test_results if result.status == "PASS")
        failed_tests = total_tests - passed_tests
        success_rate = (passed_tests / total_tests * 100) if total_tests > 0 else 0
        
        print(f"Tests totaux: {total_tests}")
        print(f"Réussis: {passed_tests}")
        print(f"Échoués: {failed_tests}")
        print(f"Taux de réussite: {success_rate:.1f}%")
        print()
        
        # Génération rapport markdown
        cls._generate_markdown_report()
        
        print(f"Fin des tests : {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")
        print("=" * 60)
    
    @classmethod
    def _generate_markdown_report(cls):
        """Générer le rapport markdown détaillé"""
        report = []
        report.append("# Rapport de Tests Automatisés - Application Comptable Python")
        report.append(f"Généré le : {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")
        report.append("")
        
        # Résumé
        total_tests = len(cls.test_results)
        passed_tests = sum(1 for result in cls.test_results if result.status == "PASS")
        failed_tests = total_tests - passed_tests
        success_rate = (passed_tests / total_tests * 100) if total_tests > 0 else 0
        
        report.append("## Résumé des Tests")
        report.append(f"- Tests totaux : {total_tests}")
        report.append(f"- Réussis : {passed_tests}")
        report.append(f"- Échoués : {failed_tests}")
        report.append(f"- Taux de réussite : {success_rate:.1f}%")
        report.append("")
        
        # Tableau détaillé
        report.append("## Résultats Détaillés des Tests")
        report.append("")
        report.append("| Test Case ID | Description | Pré-conditions | Étapes de Test | Résultat Attendu | Résultat Obtenu | Statut | Commentaires |")
        report.append("|--------------|-------------|----------------|----------------|------------------|-----------------|---------|--------------|")
        
        for result in cls.test_results:
            steps_str = " | ".join(result.test_steps) if isinstance(result.test_steps, list) else str(result.test_steps)
            report.append(f"| {result.test_id} | {result.description} | {result.preconditions} | {steps_str} | {result.expected_result} | {result.actual_result} | {result.status} | {result.comments} |")
        
        # Sauvegarder le rapport
        try:
            with open('rapport_tests_simple.md', 'w', encoding='utf-8') as f:
                f.write("\n".join(report))
            print("Rapport détaillé généré : rapport_tests_simple.md")
        except Exception as e:
            print(f"Erreur lors de la génération du rapport : {e}")


def main():
    """Point d'entrée principal pour lancer les tests"""
    # Lancer la suite de tests
    unittest.main(verbosity=2, exit=False)


if __name__ == '__main__':
    main()
