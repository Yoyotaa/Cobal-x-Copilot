# Réécriture Python du Système de Gestion de Comptes COBOL

## Vue d'ensemble

Cette application est une modernisation complète d'un système de gestion de comptes bancaires originalement écrit en COBOL. La réécriture respecte fidèlement la logique métier tout en appliquant les standards et bonnes pratiques Python modernes.

## Architecture

### Structure du Projet

```
python_reworked_project/
├── src/
│   ├── models/          # Modèles de données
│   ├── services/        # Logique métier et persistance
│   ├── ui/             # Interface utilisateur
│   └── main.py         # Point d'entrée
├── tests/              # Tests unitaires
├── data/               # Fichiers de données
└── docs/               # Documentation
```

### Correspondance COBOL → Python

| Fichier COBOL | Module Python | Responsabilité |
|---------------|---------------|----------------|
| `main.cob` | `main.py` + `ui/console_interface.py` | Interface et contrôleur principal |
| `operations.cob` | `services/account_service.py` | Logique métier des opérations |
| `data.cob` | `services/data_service.py` | Persistance des données |

## Installation

```bash
# Cloner le projet
cd python_reworked_project

# Installer les dépendances
pip install -r requirements.txt

# Installer en mode développement
pip install -e .
```

## Utilisation

```bash
# Lancer l'application
python src/main.py
```

## Architecture et Tests

L'application utilise une architecture **modulaire inspirée de MVC** :

- **Modèles** : Gestion des données avec `models/account.py`
- **Services** : Logique métier et persistance dans `services/`
- **Interface utilisateur** : Menu interactif dans `ui/console_interface.py`

Les tests unitaires garantissent la conformité avec le COBOL original :

- **Tests de modèle** : Validation des données et opérations de base
- **Tests de service** : Vérification de la logique métier
- **Tests d'intégration** : Scénarios complets et conformité fonctionnelle
- **Golden Master** : Comparaison des sorties COBOL et Python

Exécutez les tests avec :

```bash
# Lancer tous les tests
pytest

# Tests avec couverture
pytest --cov=src

# Tests avec rapport détaillé
pytest -v --cov=src --cov-report=html
```

## Fonctionnalités

### 1. Consultation du solde
- Affiche le solde actuel du compte
- Équivalent COBOL : `CALL 'Operations' USING 'TOTAL'`

### 2. Crédit du compte
- Ajoute un montant au solde
- Validation des montants positifs
- Équivalent COBOL : `CALL 'Operations' USING 'CREDIT'`

### 3. Débit du compte
- Soustrait un montant du solde
- Vérification des fonds suffisants
- Équivalent COBOL : `CALL 'Operations' USING 'DEBIT'`

### 4. Persistance des données
- Sauvegarde automatique en JSON
- Chargement au démarrage
- Équivalent COBOL : WORKING-STORAGE en mémoire

## Standards et Bonnes Pratiques

- **PEP 8** : Style de code Python
- **Type Hints** : Annotations de types
- **Dataclasses** : Modèles de données modernes
- **Logging** : Journalisation structurée
- **Tests unitaires** : Couverture complète
- **Documentation** : Docstrings et commentaires explicatifs

## Conformité Fonctionnelle

La logique métier COBOL est respectée fidèlement :
- Solde initial : 1000.00
- Validation des fonds pour les débits
- Messages d'erreur identiques
- Flux d'interaction préservé

## Modernisations Apportées

1. **Gestion des erreurs** : Exceptions Python au lieu de codes retour
2. **Types de données** : `Decimal` pour la précision monétaire
3. **Persistance** : JSON au lieu de mémoire volatile
4. **Logging** : Traçabilité des opérations
5. **Tests** : Validation automatisée de la logique
6. **Configuration** : Paramètres externalisés
