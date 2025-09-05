# Modernisation d'Application COBOL Legacy vers Python

## 🎯 Objectif du Projet

Ce projet démontre la modernisation d'une application bancaire legacy écrite en COBOL vers Python. L'objectif est de préserver toutes les fonctionnalités métier tout en améliorant la maintenabilité, la robustesse et l'extensibilité du code.

## 📋 Fonctionnalités

### ✅ Fonctionnalités Originales Préservées
- **Consultation du solde** : Affichage du solde actuel du compte
- **Crédit de compte** : Ajout de montants au solde
- **Débit de compte** : Retrait avec vérification des fonds disponibles
- **Validation des montants** : Rejet des montants négatifs
- **Solde initial** : 1000.00 € comme dans l'application COBOL originale

### 🚀 Améliorations Apportées
- **Interface utilisateur moderne** : Menu plus intuitif et messages d'erreur clairs
- **Gestion d'erreurs robuste** : Validation des entrées et récupération gracieuse des erreurs
- **Persistance des données** : Stockage JSON au lieu de la mémoire volatile
- **Tests automatisés** : Suite de tests complète pour valider toutes les fonctionnalités
- **Architecture modulaire** : Code organisé en classes et modules séparés

## 🏗️ Architecture

### Structure Originale (COBOL)
```
main.cob      → Interface utilisateur
operations.cob → Logique métier
data.cob      → Gestion des données
```

### Structure Moderne (Python)
```
main.py           → Interface utilisateur (BankAccountManager)
operations.py     → Logique métier (BankOperations)
data_manager.py   → Gestion des données (DataManager)
test_bank_system.py → Tests unitaires et d'intégration
```

## 🚀 Installation et Utilisation

### Prérequis
- Python 3.6 ou supérieur
- Aucune dépendance externe requise

### Lancement de l'Application
```bash
python3 main.py
```

### Exécution des Tests
```bash
python3 -m unittest test_bank_system.py -v
```

## 📁 Structure des Fichiers

```
modernize-legacy-cobol-app/
├── main.py              # Programme principal Python
├── operations.py        # Logique métier des opérations bancaires
├── data_manager.py      # Gestion de la persistance des données
├── test_bank_system.py  # Tests unitaires et d'intégration
├── requirements.txt     # Dépendances (vide)
├── DOCUMENTATION.md     # Documentation détaillée de la conversion
├── README.md           # Ce fichier
├── account_data.json    # Données du compte (créé automatiquement)
├── main.cob            # Programme principal COBOL original
├── operations.cob      # Module des opérations COBOL original
├── data.cob            # Module de données COBOL original
└── images/             # Captures d'écran et diagrammes
```

## 🧪 Tests

### Couverture des Tests
- ✅ Tests unitaires pour chaque module
- ✅ Tests d'intégration pour les workflows complets
- ✅ Tests de validation des entrées utilisateur
- ✅ Tests de gestion d'erreurs
- ✅ Tests de persistance des données

### Exécution des Tests
```bash
# Tous les tests
python3 -m unittest test_bank_system.py -v

# Tests spécifiques
python3 -m unittest test_bank_system.TestDataManager -v
python3 -m unittest test_bank_system.TestBankOperations -v
python3 -m unittest test_bank_system.TestIntegration -v
```

## 🔄 Correspondances COBOL ↔ Python

| Concept COBOL | Équivalent Python | Description |
|---------------|-------------------|-------------|
| `PROGRAM-ID` | `class` | Définition de programme |
| `PIC 9(6)V99` | `float` | Type monétaire |
| `CALL` | `method()` | Appel de fonction |
| `EVALUATE` | `if/elif/else` | Structure conditionnelle |
| `PERFORM UNTIL` | `while` | Boucle conditionnelle |
| `DISPLAY` | `print()` | Affichage |
| `ACCEPT` | `input()` | Saisie utilisateur |

## 📊 Exemple d'Utilisation

```
========================================
    SYSTÈME DE GESTION DE COMPTE
========================================
1. Consulter le solde
2. Créditer le compte
3. Débiter le compte
4. Quitter
========================================
Entrez votre choix (1-4): 1

Solde actuel: 1000.00 €

Entrez votre choix (1-4): 2
Montant à créditer: 500
Montant crédité: 500.00 €
Nouveau solde: 1500.00 €

Entrez votre choix (1-4): 3
Montant à débiter: 200
Montant débité: 200.00 €
Nouveau solde: 1300.00 €
```

## 🎓 Difficultés et Solutions

### 1. **Compréhension du Code Legacy**
- **Difficulté** : Syntaxe COBOL très différente de Python
- **Solution** : Analyse ligne par ligne avec documentation des correspondances

### 2. **Gestion de la Persistance**
- **Difficulté** : COBOL utilise la mémoire, Python nécessite un stockage
- **Solution** : Implémentation d'un système de fichiers JSON

### 3. **Validation des Entrées**
- **Difficulté** : Validation limitée en COBOL
- **Solution** : Implémentation de validation robuste en Python

### 4. **Tests Automatisés**
- **Difficulté** : Pas de tests dans l'original
- **Solution** : Création d'une suite de tests complète

## 📚 Documentation

Pour plus de détails sur la conversion, consultez le fichier `DOCUMENTATION.md` qui contient :
- Analyse détaillée de l'architecture
- Correspondances complètes COBOL ↔ Python
- Politique de tests
- Difficultés rencontrées et solutions

## 🤝 Contribution

Ce projet est un exercice de modernisation de code legacy. Les améliorations suggérées sont les bienvenues :
- Ajout de nouvelles fonctionnalités bancaires
- Amélioration de l'interface utilisateur
- Extension des tests
- Optimisation des performances

## 📄 Licence

Ce projet est fourni à des fins éducatives pour démontrer les techniques de modernisation de code legacy.

---

**Note** : Cette conversion démontre l'importance de comprendre le code legacy avant de le refactoriser, et l'utilité des tests pour valider la conversion.
