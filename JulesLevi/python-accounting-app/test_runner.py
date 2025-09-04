#!/usr/bin/env python3
"""
Tests automatisés basiques pour l'application comptable Python
Version simplifiée sans dépendances externes
"""

import sys
import os
import json
import tempfile
from datetime import datetime

# Ajouter le répertoire courant au path
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

try:
    from operations import Operations
    from data import DataProgram
except ImportError as e:
    print(f"Erreur d'importation: {e}")
    print("Assurez-vous que les fichiers operations.py et data.py sont présents")
    sys.exit(1)


class TestRunner:
    """Gestionnaire de tests simple"""
    
    def __init__(self):
        self.test_results = []
        self.passed = 0
        self.failed = 0
    
    def run_test(self, test_id, description, test_func):
        """Exécuter un test et enregistrer le résultat"""
        print(f"Exécution {test_id}: {description}")
        try:
            result = test_func()
            if result['success']:
                print(f"✓ PASS - {result['message']}")
                self.passed += 1
                status = "PASS"
            else:
                print(f"✗ FAIL - {result['message']}")
                self.failed += 1
                status = "FAIL"
            
            self.test_results.append({
                'test_id': test_id,
                'description': description,
                'preconditions': result.get('preconditions', ''),
                'test_steps': result.get('test_steps', []),
                'expected_result': result.get('expected_result', ''),
                'actual_result': result['message'],
                'status': status,
                'comments': result.get('comments', ''),
                'execution_time': datetime.now().strftime('%Y-%m-%d %H:%M:%S')
            })
            
        except Exception as e:
            print(f"✗ ERROR - Exception: {str(e)}")
            self.failed += 1
            self.test_results.append({
                'test_id': test_id,
                'description': description,
                'preconditions': 'Test failed with exception',
                'test_steps': ['Test execution failed'],
                'expected_result': 'Test should complete without exception',
                'actual_result': f'Exception: {str(e)}',
                'status': 'FAIL',
                'comments': 'Test execution failed',
                'execution_time': datetime.now().strftime('%Y-%m-%d %H:%M:%S')
            })
        
        print()
    
    def create_temp_operations(self, initial_balance=1000.00):
        """Créer une instance Operations avec fichier temporaire"""
        temp_file = tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.json')
        temp_file.close()
        
        # Initialiser avec le solde
        with open(temp_file.name, 'w') as f:
            json.dump({'balance': initial_balance}, f)
        
        operations = Operations()
        operations.data_program = DataProgram(temp_file.name)
        
        return operations, temp_file.name
    
    def cleanup_temp_file(self, temp_file):
        """Nettoyer le fichier temporaire"""
        if os.path.exists(temp_file):
            os.unlink(temp_file)
    
    def generate_report(self):
        """Générer le rapport de tests"""
        total = self.passed + self.failed
        success_rate = (self.passed / total * 100) if total > 0 else 0
        
        print("=" * 60)
        print("RAPPORT FINAL DES TESTS")
        print("=" * 60)
        print(f"Tests totaux: {total}")
        print(f"Réussis: {self.passed}")
        print(f"Échoués: {self.failed}")
        print(f"Taux de réussite: {success_rate:.1f}%")
        print()
        
        # Générer rapport markdown
        self._create_markdown_report()
    
    def _create_markdown_report(self):
        """Créer le rapport markdown"""
        report_lines = []
        report_lines.append("# Rapport de Tests Automatisés - Application Comptable Python")
        report_lines.append(f"Généré le : {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")
        report_lines.append("")
        
        total = self.passed + self.failed
        success_rate = (self.passed / total * 100) if total > 0 else 0
        
        report_lines.append("## Résumé des Tests")
        report_lines.append(f"- Tests totaux : {total}")
        report_lines.append(f"- Réussis : {self.passed}")
        report_lines.append(f"- Échoués : {self.failed}")
        report_lines.append(f"- Taux de réussite : {success_rate:.1f}%")
        report_lines.append("")
        
        report_lines.append("## Résultats Détaillés des Tests")
        report_lines.append("")
        report_lines.append("| Test Case ID | Description | Pré-conditions | Étapes de Test | Résultat Attendu | Résultat Obtenu | Statut | Commentaires |")
        report_lines.append("|--------------|-------------|----------------|----------------|------------------|-----------------|---------|--------------|")
        
        for result in self.test_results:
            steps = " | ".join(result['test_steps']) if isinstance(result['test_steps'], list) else str(result['test_steps'])
            report_lines.append(f"| {result['test_id']} | {result['description']} | {result['preconditions']} | {steps} | {result['expected_result']} | {result['actual_result']} | {result['status']} | {result['comments']} |")
        
        try:
            with open('rapport_tests_automatises.md', 'w', encoding='utf-8') as f:
                f.write('\n'.join(report_lines))
            print("Rapport détaillé sauvegardé : rapport_tests_automatises.md")
        except Exception as e:
            print(f"Erreur lors de la sauvegarde du rapport : {e}")


# Tests individuels
def test_view_balance_default(runner):
    """TC-1.1: Consultation du solde par défaut"""
    def test_func():
        operations, temp_file = runner.create_temp_operations(1000.00)
        try:
            result = operations.view_balance()
            success = result == 1000.00
            
            return {
                'success': success,
                'message': f'Solde retourné: {result}',
                'preconditions': 'Application initialisée avec solde par défaut de 1000.00',
                'test_steps': [
                    '1. Initialiser le module Operations',
                    '2. Appeler la méthode view_balance()',
                    '3. Vérifier que le solde retourné est 1000.00'
                ],
                'expected_result': 'Solde retourné: 1000.00',
                'comments': 'Test de consultation basique du solde'
            }
        finally:
            runner.cleanup_temp_file(temp_file)
    
    return test_func


def test_credit_valid_amount(runner):
    """TC-2.1: Crédit avec montant valide"""
    def test_func():
        operations, temp_file = runner.create_temp_operations(1000.00)
        try:
            # Simuler l'entrée utilisateur
            original_input = __builtins__['input'] if 'input' in __builtins__ else input
            __builtins__['input'] = lambda prompt: '250.50'
            
            # Rediriger print pour éviter les sorties
            original_print = __builtins__['print'] if 'print' in __builtins__ else print
            __builtins__['print'] = lambda *args, **kwargs: None
            
            try:
                result = operations.credit_account()
                success = result == 1250.50
                
                # Vérifier la persistance
                with open(temp_file, 'r') as f:
                    data = json.load(f)
                    stored_balance = data['balance']
                
                success = success and stored_balance == 1250.50
                
                return {
                    'success': success,
                    'message': f'Nouveau solde: {result}, Stocké: {stored_balance}',
                    'preconditions': 'Solde du compte est 1000.00',
                    'test_steps': [
                        '1. Saisir montant de crédit de 250.50',
                        '2. Appeler la méthode credit_account()',
                        '3. Vérifier le nouveau solde (1250.50)',
                        '4. Vérifier la persistance des données'
                    ],
                    'expected_result': 'Nouveau solde: 1250.50',
                    'comments': 'Test de crédit avec montant valide'
                }
            finally:
                __builtins__['input'] = original_input
                __builtins__['print'] = original_print
                
        finally:
            runner.cleanup_temp_file(temp_file)
    
    return test_func


def test_debit_sufficient_funds(runner):
    """TC-3.1: Débit avec fonds suffisants"""
    def test_func():
        operations, temp_file = runner.create_temp_operations(1000.00)
        try:
            # Simuler l'entrée utilisateur
            original_input = __builtins__['input'] if 'input' in __builtins__ else input
            __builtins__['input'] = lambda prompt: '300.00'
            
            # Rediriger print
            original_print = __builtins__['print'] if 'print' in __builtins__ else print
            __builtins__['print'] = lambda *args, **kwargs: None
            
            try:
                result = operations.debit_account()
                success = result == 700.00
                
                # Vérifier la persistance
                with open(temp_file, 'r') as f:
                    data = json.load(f)
                    stored_balance = data['balance']
                
                success = success and stored_balance == 700.00
                
                return {
                    'success': success,
                    'message': f'Nouveau solde: {result}, Stocké: {stored_balance}',
                    'preconditions': 'Solde du compte est 1000.00',
                    'test_steps': [
                        '1. Saisir montant de débit de 300.00',
                        '2. Appeler la méthode debit_account()',
                        '3. Vérifier la vérification des fonds',
                        '4. Vérifier le nouveau solde (700.00)'
                    ],
                    'expected_result': 'Nouveau solde: 700.00',
                    'comments': 'Test de débit avec fonds suffisants'
                }
            finally:
                __builtins__['input'] = original_input
                __builtins__['print'] = original_print
                
        finally:
            runner.cleanup_temp_file(temp_file)
    
    return test_func


def test_debit_insufficient_funds(runner):
    """TC-3.2: Débit avec fonds insuffisants"""
    def test_func():
        operations, temp_file = runner.create_temp_operations(1000.00)
        try:
            # Simuler l'entrée utilisateur
            original_input = __builtins__['input'] if 'input' in __builtins__ else input
            __builtins__['input'] = lambda prompt: '1500.00'
            
            # Rediriger print
            original_print = __builtins__['print'] if 'print' in __builtins__ else print
            __builtins__['print'] = lambda *args, **kwargs: None
            
            try:
                result = operations.debit_account()
                success = result == 1000.00  # Solde doit rester inchangé
                
                # Vérifier la persistance
                with open(temp_file, 'r') as f:
                    data = json.load(f)
                    stored_balance = data['balance']
                
                success = success and stored_balance == 1000.00
                
                return {
                    'success': success,
                    'message': f'Solde inchangé: {result}, Stocké: {stored_balance}',
                    'preconditions': 'Solde du compte est 1000.00',
                    'test_steps': [
                        '1. Saisir montant de débit de 1500.00',
                        '2. Appeler la méthode debit_account()',
                        '3. Vérifier la détection de fonds insuffisants',
                        '4. Vérifier que le solde reste 1000.00'
                    ],
                    'expected_result': 'Solde inchangé: 1000.00, message de fonds insuffisants',
                    'comments': 'Test de débit avec fonds insuffisants'
                }
            finally:
                __builtins__['input'] = original_input
                __builtins__['print'] = original_print
                
        finally:
            runner.cleanup_temp_file(temp_file)
    
    return test_func


def test_multiple_operations_sequence(runner):
    """TC-4.1: Séquence d'opérations multiples"""
    def test_func():
        operations, temp_file = runner.create_temp_operations(1000.00)
        try:
            # Rediriger print
            original_print = __builtins__['print'] if 'print' in __builtins__ else print
            __builtins__['print'] = lambda *args, **kwargs: None
            
            try:
                # Étape 1: Crédit de 500.00
                original_input = __builtins__['input'] if 'input' in __builtins__ else input
                __builtins__['input'] = lambda prompt: '500.00'
                credit_result = operations.credit_account()
                
                # Étape 2: Débit de 300.00
                __builtins__['input'] = lambda prompt: '300.00'
                debit_result = operations.debit_account()
                
                # Étape 3: Consultation du solde
                view_result = operations.view_balance()
                
                # Vérification de la séquence
                success = (credit_result == 1500.00 and 
                          debit_result == 1200.00 and 
                          view_result == 1200.00)
                
                return {
                    'success': success,
                    'message': f'Crédit: {credit_result}, Débit: {debit_result}, Final: {view_result}',
                    'preconditions': 'Solde du compte est 1000.00',
                    'test_steps': [
                        '1. Créditer 500.00 (solde devrait être 1500.00)',
                        '2. Débiter 300.00 (solde devrait être 1200.00)',
                        '3. Consulter le solde (devrait afficher 1200.00)',
                        '4. Vérifier que toutes les opérations fonctionnent ensemble'
                    ],
                    'expected_result': 'Solde final: 1200.00 après séquence d\'opérations',
                    'comments': 'Test d\'intégration avec séquence complète d\'opérations'
                }
            finally:
                __builtins__['print'] = original_print
                
        finally:
            runner.cleanup_temp_file(temp_file)
    
    return test_func


def main():
    """Point d'entrée principal"""
    print("=" * 60)
    print("SUITE DE TESTS AUTOMATISÉS")
    print("Application Comptable Python")
    print("=" * 60)
    print(f"Début des tests : {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")
    print()
    
    runner = TestRunner()
    
    # Exécuter tous les tests
    runner.run_test("TC-1.1", "Consultation du solde par défaut", 
                   test_view_balance_default(runner))
    
    runner.run_test("TC-2.1", "Crédit avec montant valide", 
                   test_credit_valid_amount(runner))
    
    runner.run_test("TC-3.1", "Débit avec fonds suffisants", 
                   test_debit_sufficient_funds(runner))
    
    runner.run_test("TC-3.2", "Débit avec fonds insuffisants", 
                   test_debit_insufficient_funds(runner))
    
    runner.run_test("TC-4.1", "Séquence d'opérations multiples", 
                   test_multiple_operations_sequence(runner))
    
    # Générer le rapport final
    runner.generate_report()


if __name__ == '__main__':
    main()
