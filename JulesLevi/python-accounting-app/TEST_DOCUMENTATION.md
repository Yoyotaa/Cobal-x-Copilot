# Suite de Tests Automatisés - Application Comptable Python

Cette suite de tests automatisés fournit une couverture complète de test pour l'application comptable Python avec tous les éléments demandés :

## Éléments de Test Inclus

Chaque test inclut les 8 éléments requis :

1. **Test Case ID** - Identifiant unique du cas de test (ex: TC-1.1, TC-2.1)
2. **Test Case Description** - Description détaillée du test
3. **Pre-conditions** - Conditions préalables avant l'exécution
4. **Test Steps** - Étapes détaillées du test
5. **Expected Result** - Résultat attendu
6. **Actual Result** - Résultat réel obtenu
7. **Status (Pass/Fail)** - Statut du test
8. **Comments** - Commentaires additionnels

## Structure des Tests

### TC-1: Tests de Consultation de Solde
- **TC-1.1** : Consultation avec solde par défaut (1000.00)
- **TC-1.2** : Consultation avec solde personnalisé (2500.75)
- **TC-1.3** : Consultation avec solde zéro

### TC-2: Tests de Crédit
- **TC-2.1** : Crédit avec montant valide (250.50)
- **TC-2.2** : Crédit avec gros montant (10000.00)
- **TC-2.3** : Crédit avec haute précision décimale (123.456)
- **TC-2.4** : Crédit avec montant négatif invalide (-100.00)
- **TC-2.5** : Crédit avec texte invalide ('abc')

### TC-3: Tests de Débit
- **TC-3.1** : Débit avec fonds suffisants (300.00)
- **TC-3.2** : Débit avec fonds insuffisants (1500.00)
- **TC-3.3** : Débit du montant exact du solde (1000.00)
- **TC-3.4** : Débit avec montant négatif invalide (-50.00)

### TC-4: Tests d'Intégration
- **TC-4.1** : Séquence d'opérations multiples (Crédit→Débit→Consultation)
- **TC-4.2** : Persistance des données entre instances

## Lancement des Tests

### Méthode 1 : Script Automatisé (Recommandé)
```bash
cd /home/jules/github/modernize-legacy-cobol-app-main/python-accounting-app
./run_tests.sh
```

### Méthode 2 : Tests Unittest
```bash
python3 test_suite_automated.py
```

### Méthode 3 : Tests Pytest
```bash
# Installer les dépendances
pip3 install -r requirements_test.txt

# Lancer tous les tests
pytest test_accounting_pytest.py -v

# Tests par catégorie
pytest test_accounting_pytest.py -v -m "view_balance"  # Tests de consultation
pytest test_accounting_pytest.py -v -m "credit"       # Tests de crédit
pytest test_accounting_pytest.py -v -m "debit"        # Tests de débit
pytest test_accounting_pytest.py -v -m "integration"  # Tests d'intégration

# Avec rapport HTML et couverture
pytest test_accounting_pytest.py -v --html=report.html --cov=operations --cov=data --cov=main
```

## Rapports Générés

### 1. Rapport Unittest Détaillé
- **Fichier** : `test_report.md`
- **Contenu** : Tableau complet avec les 8 éléments pour chaque test
- **Format** : Markdown avec tableau structuré

### 2. Rapport Pytest Détaillé
- **Fichier** : `pytest_test_report.md`
- **Contenu** : Rapport similaire mais optimisé pour pytest
- **Format** : Markdown avec métriques détaillées

### 3. Rapport HTML Interactif
- **Fichier** : `test_report.html`
- **Contenu** : Interface web interactive avec résultats détaillés
- **Avantages** : Navigation facile, filtres, graphiques

### 4. Rapport de Couverture
- **Fichier** : `htmlcov/index.html`
- **Contenu** : Analyse de couverture de code ligne par ligne
- **Utilité** : Identifier les parties non testées

## Exemples de Résultats

### Tableau de Résultats (Format Markdown)
```markdown
| Test Case ID | Description | Pre-conditions | Test Steps | Expected Result | Actual Result | Status | Comments |
|--------------|-------------|----------------|------------|-----------------|---------------|---------|----------|
| TC-1.1 | View Current Balance with Default Balance (1000.00) | Application initialized with default balance of 1000.00 | 1. Initialize Operations module \| 2. Call view_balance() method \| 3. Verify balance display | Current balance: 1000.00 is displayed and returned | Balance returned: 1000.00, Print called: True | PASS | |
| TC-2.1 | Credit Account with Valid Amount (250.50) | Account balance is 1000.00 | 1. Input credit amount of 250.50 \| 2. Call credit_account() method \| 3. Verify new balance calculation \| 4. Verify balance persistence | New balance: 1250.50, success message displayed | Returned balance: 1250.50, Stored balance: 1250.50 | PASS | |
```

## Vérifications Multiples par Fonctionnalité

### Pour chaque fonctionnalité, plusieurs tests vérifient :

#### Consultation de Solde (3 tests)
- Solde par défaut
- Solde personnalisé
- Solde zéro

#### Crédit de Compte (5 tests)
- Montant valide standard
- Gros montant
- Haute précision décimale
- Validation d'entrée négative
- Validation d'entrée texte

#### Débit de Compte (4 tests)
- Fonds suffisants
- Fonds insuffisants  
- Montant exact du solde
- Validation d'entrée négative

#### Tests d'Intégration (2 tests)
- Séquence d'opérations
- Persistance des données

## Métriques de Test

La suite génère automatiquement :
- **Nombre total de tests** : 14 cas de test
- **Taux de réussite** : Pourcentage calculé automatiquement
- **Temps d'exécution** : Horodatage pour chaque test
- **Couverture de code** : Pourcentage de code testé

## Automatisation

### Exécution Automatique
- Tous les tests s'exécutent automatiquement
- Aucune intervention manuelle requise
- Résultats collectés et formatés automatiquement

### Validation Automatique
- Assertions automatiques pour chaque condition
- Comparaison automatique des résultats attendus vs réels
- Calcul automatique du statut Pass/Fail

### Rapport Automatique
- Génération automatique des rapports
- Formatage en tableau Markdown
- Export en HTML pour visualisation web

## Structure des Fichiers

```
python-accounting-app/
├── main.py                     # Application principale
├── operations.py               # Module des opérations
├── data.py                     # Module de gestion des données
├── test_suite_automated.py     # Suite de tests unittest
├── test_accounting_pytest.py   # Suite de tests pytest
├── conftest.py                 # Configuration pytest
├── pytest.ini                  # Configuration pytest
├── requirements_test.txt       # Dépendances de test
├── run_tests.sh               # Script de lancement automatisé
├── TEST_DOCUMENTATION.md      # Cette documentation
├── test_report.md             # Rapport unittest (généré)
├── pytest_test_report.md      # Rapport pytest (généré)
├── test_report.html           # Rapport HTML (généré)
└── htmlcov/                   # Rapport de couverture (généré)
    └── index.html
```

Cette suite de tests automatisés fournit une validation complète de l'application avec tous les éléments requis et plusieurs niveaux de vérification pour chaque fonctionnalité.
