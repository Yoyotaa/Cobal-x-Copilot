#!/usr/bin/env python3
"""
Script de démonstration pour le système bancaire converti de COBOL vers Python.
Ce script montre les fonctionnalités principales de l'application.
"""

import sys
import os
from operations import BankOperations
from data_manager import DataManager


def demo_basic_operations():
    """Démonstration des opérations de base."""
    print("=" * 60)
    print("DÉMONSTRATION DU SYSTÈME BANCAIRE COBOL → PYTHON")
    print("=" * 60)
    
    # Créer une instance avec un fichier temporaire pour la démo
    demo_file = "demo_account.json"
    if os.path.exists(demo_file):
        os.remove(demo_file)
    
    data_manager = DataManager(demo_file)
    operations = BankOperations(data_manager)
    
    print("\n1. CONSULTATION DU SOLDE INITIAL")
    print("-" * 40)
    operations.view_balance()
    
    print("\n2. CRÉDIT DE COMPTE")
    print("-" * 40)
    operations.credit_account(500.00)
    
    print("\n3. DÉBIT DE COMPTE (FONDS SUFFISANTS)")
    print("-" * 40)
    operations.debit_account(200.00)
    
    print("\n4. DÉBIT DE COMPTE (FONDS INSUFFISANTS)")
    print("-" * 40)
    operations.debit_account(2000.00)
    
    print("\n5. CRÉDIT AVEC MONTANT NÉGATIF")
    print("-" * 40)
    operations.credit_account(-100.00)
    
    print("\n6. SOLDE FINAL")
    print("-" * 40)
    operations.view_balance()
    
    # Nettoyage
    if os.path.exists(demo_file):
        os.remove(demo_file)
    
    print("\n" + "=" * 60)
    print("DÉMONSTRATION TERMINÉE")
    print("=" * 60)


def demo_data_persistence():
    """Démonstration de la persistance des données."""
    print("\n" + "=" * 60)
    print("DÉMONSTRATION DE LA PERSISTANCE DES DONNÉES")
    print("=" * 60)
    
    demo_file = "persistence_demo.json"
    if os.path.exists(demo_file):
        os.remove(demo_file)
    
    print("\nSession 1 - Création et modification du compte")
    print("-" * 50)
    data_manager1 = DataManager(demo_file)
    operations1 = BankOperations(data_manager1)
    
    print("Solde initial:")
    operations1.view_balance()
    
    print("\nCrédit de 750€:")
    operations1.credit_account(750.00)
    
    print("\nSession 2 - Récupération des données")
    print("-" * 50)
    data_manager2 = DataManager(demo_file)
    operations2 = BankOperations(data_manager2)
    
    print("Solde récupéré:")
    operations2.view_balance()
    
    print("\nDébit de 250€:")
    operations2.debit_account(250.00)
    
    print("\nSession 3 - Vérification finale")
    print("-" * 50)
    data_manager3 = DataManager(demo_file)
    operations3 = BankOperations(data_manager3)
    
    print("Solde final:")
    operations3.view_balance()
    
    # Nettoyage
    if os.path.exists(demo_file):
        os.remove(demo_file)
    
    print("\n" + "=" * 60)
    print("PERSISTANCE DÉMONTRÉE AVEC SUCCÈS")
    print("=" * 60)


def demo_error_handling():
    """Démonstration de la gestion d'erreurs."""
    print("\n" + "=" * 60)
    print("DÉMONSTRATION DE LA GESTION D'ERREURS")
    print("=" * 60)
    
    demo_file = "error_demo.json"
    if os.path.exists(demo_file):
        os.remove(demo_file)
    
    data_manager = DataManager(demo_file)
    operations = BankOperations(data_manager)
    
    print("\n1. Test avec montant négatif pour crédit")
    print("-" * 45)
    operations.credit_account(-50.00)
    
    print("\n2. Test avec montant négatif pour débit")
    print("-" * 45)
    operations.debit_account(-30.00)
    
    print("\n3. Test avec débit supérieur au solde")
    print("-" * 45)
    operations.debit_account(2000.00)
    
    print("\n4. Test de persistance avec fichier corrompu")
    print("-" * 45)
    # Créer un fichier corrompu
    with open(demo_file, 'w') as f:
        f.write("invalid json content")
    
    # Le gestionnaire doit récupérer gracieusement
    data_manager_corrupted = DataManager(demo_file)
    operations_corrupted = BankOperations(data_manager_corrupted)
    operations_corrupted.view_balance()
    
    # Nettoyage
    if os.path.exists(demo_file):
        os.remove(demo_file)
    
    print("\n" + "=" * 60)
    print("GESTION D'ERREURS DÉMONTRÉE AVEC SUCCÈS")
    print("=" * 60)


def main():
    """Fonction principale de démonstration."""
    try:
        print("🚀 DÉMARRAGE DE LA DÉMONSTRATION")
        print("Cette démonstration montre la conversion COBOL → Python")
        
        # Démonstrations
        demo_basic_operations()
        demo_data_persistence()
        demo_error_handling()
        
        print("\n✅ TOUTES LES DÉMONSTRATIONS TERMINÉES AVEC SUCCÈS")
        print("\n📋 RÉSUMÉ DES FONCTIONNALITÉS DÉMONTRÉES:")
        print("   • Consultation du solde")
        print("   • Crédit de compte")
        print("   • Débit de compte")
        print("   • Validation des montants")
        print("   • Gestion des fonds insuffisants")
        print("   • Persistance des données")
        print("   • Gestion d'erreurs robuste")
        
        print("\n🎯 L'APPLICATION PYTHON PRÉSERVE TOUTES LES FONCTIONNALITÉS COBOL")
        print("   tout en apportant des améliorations significatives!")
        
    except Exception as e:
        print(f"\n❌ Erreur lors de la démonstration: {e}")
        sys.exit(1)


if __name__ == "__main__":
    main()
