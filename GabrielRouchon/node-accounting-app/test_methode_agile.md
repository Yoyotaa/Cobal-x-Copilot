## Méthode Agile – Stratégie et exécution des tests

### Pyramide de tests
- **Tests unitaires**: rapides, précis, isolés; majorité du volume.
- **Tests d’intégration**: vérifient l’assemblage de plusieurs composants.
- (Optionnel) **End-to-End (E2E)**: parcours utilisateur complet; peu nombreux et plus lents.

### Organisation du projet (tests)
```
node-accounting-app/
  data.py
  operations.py
  main.py
  pytest.ini
  tests/
    conftest.py
    TESTS_EXPLICATION.txt
    unit/
      test_dataprogram.py
      test_operations.py
    integration/
      test_main_integration.py
```

### Ce que couvre la suite actuelle
- **DataProgram** (tests/unit/test_dataprogram.py)
  - Solde initial à 1000.00
  - Lecture/écriture (read/write)
  - set_balance / get_balance
- **Operations** (tests/unit/test_operations.py)
  - Affichage du solde (view_balance)
  - Crédit / débit avec messages
  - Débit insuffisant (solde inchangé)
  - Saisie utilisateur simulée (validations, messages d’erreurs)
- **MainProgram** (tests/integration/test_main_integration.py)
  - Parcours: voir solde puis quitter
  - Parcours: crédit 300, débit 100, afficher solde, quitter

Pour une description détaillée de chaque test, voir `tests/TESTS_EXPLICATION.txt`.

### Configuration de test
- `pytest.ini`
  - `testpaths = tests` → recherche des tests sous `tests/`
  - `pythonpath = .` → les modules `data.py`, `operations.py`, `main.py` sont importables
  - `addopts = -q` → sortie compacte
- `tests/conftest.py` ajoute la racine applicative au PYTHONPATH pendant les tests.

### Commandes – exécuter la suite et faire une démo
Se placer dans le répertoire:
```bash
cd "/home/rouchon/delivery/tek5/Cobal x Copilot/modernize-legacy-cobol-app/node-accounting-app"
```

- Tout exécuter
```bash
python3 -m pytest -q
```

- Par catégorie
```bash
python3 -m pytest -q tests/unit
python3 -m pytest -q tests/integration
```

- Un fichier précis
```bash
python3 -m pytest -q tests/unit/test_operations.py
```

- Un test précis (nodeid)
```bash
python3 -m pytest -q tests/unit/test_operations.py::test_credit_account
```

- Filtrer par nom
```bash
python3 -m pytest -q -k "credit and not debit"
```

- Voir les `print()` (utile en démo)
```bash
python3 -m pytest -q -s tests/unit/test_operations.py::test_view_balance
```

- Mode verbeux et arrêt au 1er échec
```bash
python3 -m pytest -v --maxfail=1 -x tests/unit/test_operations.py
```

### Notes pratiques
- `__init__.py` dans `tests/` n’est pas nécessaire; supprimés pour alléger. La découverte fonctionne via `pytest.ini` et `conftest.py`.
- Les fichiers `.pyc` sont du bytecode compilé (cachés dans `__pycache__/`), régénérés automatiquement, et ignorables en VCS.

### Prochaines étapes possibles
- Ajouter quelques tests E2E simples (CLI end-to-end) si besoin.
- Ajouter un workflow CI (GitHub Actions) qui exécute unitaires + intégration à chaque push.

### Statut actuel
Tous les tests passent: `python3 -m pytest -q`.


