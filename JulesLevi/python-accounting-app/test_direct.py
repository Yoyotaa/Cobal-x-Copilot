#!/usr/bin/env python3
"""
Version directe et simple des tests automatisés
"""

import os
import sys
import json
import tempfile
from datetime import datetime

# Ajouter le répertoire courant au path
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

from operations import Operations
from data import DataProgram

def print_header():
    """Afficher l'en-tête des tests"""
    print("=" * 70)
    print("SUITE DE TESTS AUTOMATISÉS - APPLICATION COMPTABLE PYTHON")
    print("=" * 70)
    print(f"Date d'exécution : {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")
    print()

def create_test_operations(balance=1000.00):
    """Créer une instance Operations pour les tests"""
    temp_file = tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.json')
    temp_file.close()
    
    # Initialiser avec le solde
    with open(temp_file.name, 'w') as f:
        json.dump({'balance': balance}, f)
    
    operations = Operations()
    operations.data_program = DataProgram(temp_file.name)
    
    return operations, temp_file.name

def cleanup_file(filepath):
    """Nettoyer un fichier temporaire"""
    if os.path.exists(filepath):
        os.unlink(filepath)

def run_tests():
    """Exécuter tous les tests et générer le rapport"""
    print_header()
    
    test_results = []
    passed = 0
    failed = 0
    
    # TC-1.1: Test consultation solde par défaut
    print("TC-1.1: Test consultation du solde par défaut...")
    try:
        operations, temp_file = create_test_operations(1000.00)
        
        # Rediriger la sortie print pour éviter l'affichage
        import io
        from contextlib import redirect_stdout
        f = io.StringIO()
        
        with redirect_stdout(f):
            result = operations.view_balance()
        
        success = result == 1000.00
        if success:
            print("✓ PASS - Solde par défaut correctement affiché (1000.00)")
            passed += 1
        else:
            print(f"✗ FAIL - Solde attendu: 1000.00, obtenu: {result}")
            failed += 1
        
        test_results.append({
            'test_id': 'TC-1.1',
            'description': 'Consultation du solde par défaut',
            'preconditions': 'Application initialisée avec solde par défaut de 1000.00',
            'test_steps': '1. Initialiser Operations | 2. Appeler view_balance() | 3. Vérifier le retour',
            'expected_result': 'Solde retourné: 1000.00',
            'actual_result': f'Solde retourné: {result}',
            'status': 'PASS' if success else 'FAIL',
            'comments': 'Test de base de consultation de solde'
        })
        
        cleanup_file(temp_file)
        
    except Exception as e:
        print(f"✗ ERROR - Exception lors du test TC-1.1: {e}")
        failed += 1
        test_results.append({
            'test_id': 'TC-1.1',
            'description': 'Consultation du solde par défaut',
            'preconditions': 'Test échoué avec exception',
            'test_steps': 'Exécution du test',
            'expected_result': 'Test sans exception',
            'actual_result': f'Exception: {e}',
            'status': 'FAIL',
            'comments': 'Erreur d\'exécution'
        })
    
    print()
    
    # TC-2.1: Test crédit avec montant valide
    print("TC-2.1: Test crédit avec montant valide...")
    try:
        operations, temp_file = create_test_operations(1000.00)
        
        # Simuler l'entrée utilisateur en modifiant temporairement input
        def mock_input(prompt):
            return '250.50'
        
        # Sauvegarder l'input original
        original_input = input
        
        # Rediriger print et input
        import builtins
        builtins.input = mock_input
        
        import io
        from contextlib import redirect_stdout
        f = io.StringIO()
        
        with redirect_stdout(f):
            result = operations.credit_account()
        
        # Restaurer input
        builtins.input = original_input
        
        # Vérifier le résultat
        success = result == 1250.50
        
        # Vérifier la persistance
        with open(temp_file, 'r') as file:
            data = json.load(file)
            stored_balance = data['balance']
        
        success = success and stored_balance == 1250.50
        
        if success:
            print(f"✓ PASS - Crédit effectué correctement (nouveau solde: {result})")
            passed += 1
        else:
            print(f"✗ FAIL - Solde attendu: 1250.50, obtenu: {result}, stocké: {stored_balance}")
            failed += 1
        
        test_results.append({
            'test_id': 'TC-2.1',
            'description': 'Crédit du compte avec montant valide (250.50)',
            'preconditions': 'Solde du compte est 1000.00',
            'test_steps': '1. Saisir 250.50 | 2. Appeler credit_account() | 3. Vérifier nouveau solde | 4. Vérifier persistance',
            'expected_result': 'Nouveau solde: 1250.50',
            'actual_result': f'Nouveau solde: {result}, Stocké: {stored_balance}',
            'status': 'PASS' if success else 'FAIL',
            'comments': 'Test de crédit avec montant valide'
        })
        
        cleanup_file(temp_file)
        
    except Exception as e:
        print(f"✗ ERROR - Exception lors du test TC-2.1: {e}")
        failed += 1
        test_results.append({
            'test_id': 'TC-2.1',
            'description': 'Crédit du compte avec montant valide',
            'preconditions': 'Test échoué avec exception',
            'test_steps': 'Exécution du test',
            'expected_result': 'Test sans exception',
            'actual_result': f'Exception: {e}',
            'status': 'FAIL',
            'comments': 'Erreur d\'exécution'
        })
    
    print()
    
    # TC-3.1: Test débit avec fonds suffisants
    print("TC-3.1: Test débit avec fonds suffisants...")
    try:
        operations, temp_file = create_test_operations(1000.00)
        
        # Simuler l'entrée utilisateur
        def mock_input(prompt):
            return '300.00'
        
        import builtins
        original_input = builtins.input
        builtins.input = mock_input
        
        import io
        from contextlib import redirect_stdout
        f = io.StringIO()
        
        with redirect_stdout(f):
            result = operations.debit_account()
        
        builtins.input = original_input
        
        # Vérifier le résultat
        success = result == 700.00
        
        # Vérifier la persistance
        with open(temp_file, 'r') as file:
            data = json.load(file)
            stored_balance = data['balance']
        
        success = success and stored_balance == 700.00
        
        if success:
            print(f"✓ PASS - Débit effectué correctement (nouveau solde: {result})")
            passed += 1
        else:
            print(f"✗ FAIL - Solde attendu: 700.00, obtenu: {result}, stocké: {stored_balance}")
            failed += 1
        
        test_results.append({
            'test_id': 'TC-3.1',
            'description': 'Débit du compte avec fonds suffisants (300.00)',
            'preconditions': 'Solde du compte est 1000.00',
            'test_steps': '1. Saisir 300.00 | 2. Appeler debit_account() | 3. Vérifier nouveau solde | 4. Vérifier persistance',
            'expected_result': 'Nouveau solde: 700.00',
            'actual_result': f'Nouveau solde: {result}, Stocké: {stored_balance}',
            'status': 'PASS' if success else 'FAIL',
            'comments': 'Test de débit avec fonds suffisants'
        })
        
        cleanup_file(temp_file)
        
    except Exception as e:
        print(f"✗ ERROR - Exception lors du test TC-3.1: {e}")
        failed += 1
        test_results.append({
            'test_id': 'TC-3.1',
            'description': 'Débit du compte avec fonds suffisants',
            'preconditions': 'Test échoué avec exception',
            'test_steps': 'Exécution du test',
            'expected_result': 'Test sans exception',
            'actual_result': f'Exception: {e}',
            'status': 'FAIL',
            'comments': 'Erreur d\'exécution'
        })
    
    print()
    
    # TC-3.2: Test débit avec fonds insuffisants
    print("TC-3.2: Test débit avec fonds insuffisants...")
    try:
        operations, temp_file = create_test_operations(1000.00)
        
        # Simuler l'entrée utilisateur avec montant trop élevé
        def mock_input(prompt):
            return '1500.00'
        
        import builtins
        original_input = builtins.input
        builtins.input = mock_input
        
        import io
        from contextlib import redirect_stdout
        f = io.StringIO()
        
        with redirect_stdout(f):
            result = operations.debit_account()
        
        builtins.input = original_input
        
        # Vérifier que le solde n'a pas changé
        success = result == 1000.00
        
        # Vérifier la persistance
        with open(temp_file, 'r') as file:
            data = json.load(file)
            stored_balance = data['balance']
        
        success = success and stored_balance == 1000.00
        
        if success:
            print(f"✓ PASS - Fonds insuffisants détectés, solde inchangé: {result}")
            passed += 1
        else:
            print(f"✗ FAIL - Solde devrait rester 1000.00, obtenu: {result}, stocké: {stored_balance}")
            failed += 1
        
        test_results.append({
            'test_id': 'TC-3.2',
            'description': 'Débit du compte avec fonds insuffisants (1500.00)',
            'preconditions': 'Solde du compte est 1000.00',
            'test_steps': '1. Saisir 1500.00 | 2. Appeler debit_account() | 3. Vérifier fonds insuffisants | 4. Vérifier solde inchangé',
            'expected_result': 'Fonds insuffisants, solde reste 1000.00',
            'actual_result': f'Solde retourné: {result}, Stocké: {stored_balance}',
            'status': 'PASS' if success else 'FAIL',
            'comments': 'Test de débit avec fonds insuffisants'
        })
        
        cleanup_file(temp_file)
        
    except Exception as e:
        print(f"✗ ERROR - Exception lors du test TC-3.2: {e}")
        failed += 1
        test_results.append({
            'test_id': 'TC-3.2',
            'description': 'Débit du compte avec fonds insuffisants',
            'preconditions': 'Test échoué avec exception',
            'test_steps': 'Exécution du test',
            'expected_result': 'Test sans exception',
            'actual_result': f'Exception: {e}',
            'status': 'FAIL',
            'comments': 'Erreur d\'exécution'
        })
    
    print()
    
    # TC-4.1: Test séquence d'opérations multiples
    print("TC-4.1: Test séquence d'opérations multiples...")
    try:
        operations, temp_file = create_test_operations(1000.00)
        
        import builtins
        original_input = builtins.input
        
        import io
        from contextlib import redirect_stdout
        f = io.StringIO()
        
        with redirect_stdout(f):
            # Étape 1: Crédit de 500.00
            builtins.input = lambda prompt: '500.00'
            credit_result = operations.credit_account()
            
            # Étape 2: Débit de 300.00
            builtins.input = lambda prompt: '300.00'
            debit_result = operations.debit_account()
            
            # Étape 3: Consultation du solde
            view_result = operations.view_balance()
        
        builtins.input = original_input
        
        # Vérifier la séquence complète
        success = (credit_result == 1500.00 and 
                  debit_result == 1200.00 and 
                  view_result == 1200.00)
        
        if success:
            print(f"✓ PASS - Séquence d'opérations réussie (Final: {view_result})")
            passed += 1
        else:
            print(f"✗ FAIL - Crédit: {credit_result}, Débit: {debit_result}, Final: {view_result}")
            failed += 1
        
        test_results.append({
            'test_id': 'TC-4.1',
            'description': 'Séquence d\'opérations multiples (Crédit→Débit→Consultation)',
            'preconditions': 'Solde du compte est 1000.00',
            'test_steps': '1. Créditer 500.00 | 2. Débiter 300.00 | 3. Consulter solde | 4. Vérifier séquence complète',
            'expected_result': 'Solde final: 1200.00 après séquence',
            'actual_result': f'Crédit: {credit_result}, Débit: {debit_result}, Final: {view_result}',
            'status': 'PASS' if success else 'FAIL',
            'comments': 'Test d\'intégration avec séquence complète'
        })
        
        cleanup_file(temp_file)
        
    except Exception as e:
        print(f"✗ ERROR - Exception lors du test TC-4.1: {e}")
        failed += 1
        test_results.append({
            'test_id': 'TC-4.1',
            'description': 'Séquence d\'opérations multiples',
            'preconditions': 'Test échoué avec exception',
            'test_steps': 'Exécution du test',
            'expected_result': 'Test sans exception',
            'actual_result': f'Exception: {e}',
            'status': 'FAIL',
            'comments': 'Erreur d\'exécution'
        })
    
    print()
    
    # Afficher le résumé
    total = passed + failed
    success_rate = (passed / total * 100) if total > 0 else 0
    
    print("=" * 70)
    print("RÉSUMÉ DES TESTS")
    print("=" * 70)
    print(f"Tests totaux      : {total}")
    print(f"Réussis          : {passed}")
    print(f"Échoués          : {failed}")
    print(f"Taux de réussite : {success_rate:.1f}%")
    print()
    
    # Générer le rapport markdown
    generate_markdown_report(test_results, passed, failed, total, success_rate)
    
    print("Rapport détaillé généré : rapport_tests_final.md")
    print("=" * 70)

def generate_markdown_report(test_results, passed, failed, total, success_rate):
    """Générer le rapport markdown détaillé"""
    report_lines = []
    
    report_lines.append("# Rapport de Tests Automatisés - Application Comptable Python")
    report_lines.append("")
    report_lines.append(f"**Date de génération :** {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")
    report_lines.append("")
    
    # Résumé
    report_lines.append("## Résumé Exécutif")
    report_lines.append("")
    report_lines.append(f"- **Tests totaux :** {total}")
    report_lines.append(f"- **Tests réussis :** {passed}")
    report_lines.append(f"- **Tests échoués :** {failed}")
    report_lines.append(f"- **Taux de réussite :** {success_rate:.1f}%")
    report_lines.append("")
    
    # Tableau détaillé avec tous les éléments requis
    report_lines.append("## Résultats Détaillés des Tests")
    report_lines.append("")
    report_lines.append("Le tableau ci-dessous présente les **8 éléments requis** pour chaque cas de test :")
    report_lines.append("")
    report_lines.append("| Test Case ID | Test Case Description | Pre-conditions | Test Steps | Expected Result | Actual Result | Status (Pass/Fail) | Comments |")
    report_lines.append("|--------------|----------------------|----------------|------------|------------------|---------------|-------------------|----------|")
    
    for result in test_results:
        report_lines.append(f"| {result['test_id']} | {result['description']} | {result['preconditions']} | {result['test_steps']} | {result['expected_result']} | {result['actual_result']} | {result['status']} | {result['comments']} |")
    
    report_lines.append("")
    
    # Section détaillée par catégorie
    report_lines.append("## Analyse par Catégorie de Tests")
    report_lines.append("")
    
    # Tests de consultation
    report_lines.append("### TC-1: Tests de Consultation de Solde")
    view_tests = [r for r in test_results if r['test_id'].startswith('TC-1')]
    for test in view_tests:
        status_icon = "✅" if test['status'] == 'PASS' else "❌"
        report_lines.append(f"- {status_icon} **{test['test_id']}** : {test['description']}")
    report_lines.append("")
    
    # Tests de crédit
    report_lines.append("### TC-2: Tests de Crédit de Compte")
    credit_tests = [r for r in test_results if r['test_id'].startswith('TC-2')]
    for test in credit_tests:
        status_icon = "✅" if test['status'] == 'PASS' else "❌"
        report_lines.append(f"- {status_icon} **{test['test_id']}** : {test['description']}")
    report_lines.append("")
    
    # Tests de débit
    report_lines.append("### TC-3: Tests de Débit de Compte")
    debit_tests = [r for r in test_results if r['test_id'].startswith('TC-3')]
    for test in debit_tests:
        status_icon = "✅" if test['status'] == 'PASS' else "❌"
        report_lines.append(f"- {status_icon} **{test['test_id']}** : {test['description']}")
    report_lines.append("")
    
    # Tests d'intégration
    report_lines.append("### TC-4: Tests d'Intégration")
    integration_tests = [r for r in test_results if r['test_id'].startswith('TC-4')]
    for test in integration_tests:
        status_icon = "✅" if test['status'] == 'PASS' else "❌"
        report_lines.append(f"- {status_icon} **{test['test_id']}** : {test['description']}")
    report_lines.append("")
    
    # Validation des exigences
    report_lines.append("## Validation des Exigences")
    report_lines.append("")
    report_lines.append("Cette suite de tests automatisés valide les 8 éléments requis :")
    report_lines.append("")
    report_lines.append("1. ✅ **Test Case ID** - Identifiant unique pour chaque test")
    report_lines.append("2. ✅ **Test Case Description** - Description claire et concise")
    report_lines.append("3. ✅ **Pre-conditions** - Conditions initiales avant le test")
    report_lines.append("4. ✅ **Test Steps** - Étapes détaillées d'exécution")
    report_lines.append("5. ✅ **Expected Result** - Résultat attendu du test")
    report_lines.append("6. ✅ **Actual Result** - Résultat réellement obtenu")
    report_lines.append("7. ✅ **Status (Pass/Fail)** - Statut final du test")
    report_lines.append("8. ✅ **Comments** - Commentaires et observations")
    report_lines.append("")
    
    # Couverture fonctionnelle
    report_lines.append("## Couverture Fonctionnelle")
    report_lines.append("")
    report_lines.append("Les tests couvrent toutes les fonctionnalités principales :")
    report_lines.append("")
    report_lines.append("- **Consultation de solde** : Vérification de l'affichage correct")
    report_lines.append("- **Crédit de compte** : Validation des calculs et de la persistance")
    report_lines.append("- **Débit de compte** : Test des fonds suffisants et insuffisants")
    report_lines.append("- **Gestion des erreurs** : Validation des cas limites")
    report_lines.append("- **Intégration** : Test des séquences d'opérations complètes")
    report_lines.append("")
    
    # Conclusion
    if success_rate >= 80:
        report_lines.append("## ✅ Conclusion")
        report_lines.append("")
        report_lines.append(f"**L'application a réussi {success_rate:.1f}% des tests automatisés**, ce qui indique un bon niveau de qualité et de fiabilité.")
    else:
        report_lines.append("## ⚠️ Conclusion")
        report_lines.append("")
        report_lines.append(f"**L'application a réussi seulement {success_rate:.1f}% des tests automatisés**. Des améliorations sont nécessaires avant la mise en production.")
    
    # Sauvegarder le rapport
    try:
        with open('rapport_tests_final.md', 'w', encoding='utf-8') as f:
            f.write('\n'.join(report_lines))
    except Exception as e:
        print(f"Erreur lors de la sauvegarde du rapport : {e}")

if __name__ == '__main__':
    run_tests()
