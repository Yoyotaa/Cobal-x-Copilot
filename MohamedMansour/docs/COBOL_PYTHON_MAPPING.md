# Correspondance COBOL → Python

## Vue d'ensemble de la Migration

Cette documentation détaille la correspondance exacte entre le code COBOL original et l'implémentation Python moderne.

## Correspondance des Fichiers

| Fichier COBOL | Module Python | Description |
|---------------|---------------|-------------|
| `main.cob` | `src/main.py` + `src/ui/console_interface.py` | Interface utilisateur et boucle principale |
| `operations.cob` | `src/services/account_service.py` | Logique métier des opérations bancaires |
| `data.cob` | `src/services/data_service.py` | Persistance et gestion des données |
| N/A | `src/models/account.py` | Modélisation moderne des données |

## Correspondance du Code

### 1. Programme Principal (main.cob → main.py + console_interface.py)

#### COBOL Original
```cobol
IDENTIFICATION DIVISION.
PROGRAM-ID. MainProgram.

DATA DIVISION.
WORKING-STORAGE SECTION.
01  USER-CHOICE       PIC 9 VALUE 0.
01  CONTINUE-FLAG     PIC X(3) VALUE 'YES'.

PROCEDURE DIVISION.
MAIN-LOGIC.
    PERFORM UNTIL CONTINUE-FLAG = 'NO'
        DISPLAY "--------------------------------"
        DISPLAY "Account Management System"
        DISPLAY "1. View Balance"
        DISPLAY "2. Credit Account"
        DISPLAY "3. Debit Account"
        DISPLAY "4. Exit"
        DISPLAY "--------------------------------"
        DISPLAY "Enter your choice (1-4): "
        ACCEPT USER-CHOICE
        
        EVALUATE USER-CHOICE
            WHEN 1
                CALL 'Operations' USING 'TOTAL '
            WHEN 2
                CALL 'Operations' USING 'CREDIT'
            WHEN 3
                CALL 'Operations' USING 'DEBIT '
            WHEN 4
                MOVE 'NO' TO CONTINUE-FLAG
            WHEN OTHER
                DISPLAY "Invalid choice, please select 1-4."
        END-EVALUATE
    END-PERFORM
    DISPLAY "Exiting the program. Goodbye!"
    STOP RUN.
```

#### Python Équivalent
```python
class ConsoleInterface:
    def __init__(self, account_service: AccountService = None):
        self.account_service = account_service or AccountService()
        self.continue_flag = True  # Équivalent CONTINUE-FLAG
    
    def run(self) -> None:
        """Équivalent MAIN-LOGIC."""
        while self.continue_flag:  # Équivalent PERFORM UNTIL
            self.display_menu()    # Équivalent DISPLAY menu
            choice = self.get_user_choice()  # Équivalent ACCEPT USER-CHOICE
            
            # Équivalent EVALUATE USER-CHOICE
            if choice == 1:        # Équivalent WHEN 1
                self.handle_view_balance()
            elif choice == 2:      # Équivalent WHEN 2
                self.handle_credit_account()
            elif choice == 3:      # Équivalent WHEN 3
                self.handle_debit_account()
            elif choice == 4:      # Équivalent WHEN 4
                self.handle_exit()
```

### 2. Opérations Bancaires (operations.cob → account_service.py)

#### COBOL Original
```cobol
IDENTIFICATION DIVISION.
PROGRAM-ID. Operations.

DATA DIVISION.
WORKING-STORAGE SECTION.
01 OPERATION-TYPE     PIC X(6).
01 AMOUNT             PIC 9(6)V99.
01 FINAL-BALANCE      PIC 9(6)V99 VALUE 1000.00.

LINKAGE SECTION.
01 PASSED-OPERATION   PIC X(6).

PROCEDURE DIVISION USING PASSED-OPERATION.
    MOVE PASSED-OPERATION TO OPERATION-TYPE

    IF OPERATION-TYPE = 'TOTAL '
        CALL 'DataProgram' USING 'read', FINAL-BALANCE
        DISPLAY "Current balance: " FINAL-BALANCE
        
    ELSE IF OPERATION-TYPE = 'CREDIT'
        DISPLAY "Enter credit amount: "
        ACCEPT AMOUNT
        CALL 'DataProgram' USING 'read', FINAL-BALANCE
        ADD AMOUNT TO FINAL-BALANCE
        CALL 'DataProgram' USING 'WRITE', FINAL-BALANCE
        DISPLAY "Amount credited. New balance: " FINAL-BALANCE
        
    ELSE IF OPERATION-TYPE = 'DEBIT '
        DISPLAY "Enter debit amount: "
        ACCEPT AMOUNT
        CALL 'DataProgram' USING 'read', FINAL-BALANCE
        IF FINAL-BALANCE >= AMOUNT
            SUBTRACT AMOUNT FROM FINAL-BALANCE
            CALL 'DataProgram' USING 'WRITE', FINAL-BALANCE
            DISPLAY "Amount debited. New balance: " FINAL-BALANCE
        ELSE
            DISPLAY "Insufficient funds for this debit."
        END-IF
    END-IF
    GOBACK.
```

#### Python Équivalent
```python
class AccountService:
    def view_balance(self) -> Tuple[bool, str, Decimal]:
        """Équivalent IF OPERATION-TYPE = 'TOTAL'."""
        account = self.data_service.read_account()  # Équivalent CALL 'DataProgram' USING 'read'
        balance = account.get_balance()
        message = f"Current balance: {balance:.2f}"  # Équivalent DISPLAY
        return True, message, balance

    def credit_account(self, amount: Decimal) -> Tuple[bool, str, Decimal]:
        """Équivalent ELSE IF OPERATION-TYPE = 'CREDIT'."""
        account = self.data_service.read_account()  # Équivalent CALL 'DataProgram' USING 'read'
        account.credit(amount)                      # Équivalent ADD AMOUNT TO FINAL-BALANCE
        self.data_service.write_account(account)    # Équivalent CALL 'DataProgram' USING 'WRITE'
        message = f"Amount credited. New balance: {account.balance:.2f}"
        return True, message, account.balance

    def debit_account(self, amount: Decimal) -> Tuple[bool, str, Decimal]:
        """Équivalent ELSE IF OPERATION-TYPE = 'DEBIT'."""
        account = self.data_service.read_account()
        
        if account.balance >= amount:               # Équivalent IF FINAL-BALANCE >= AMOUNT
            account.debit(amount)                   # Équivalent SUBTRACT AMOUNT FROM FINAL-BALANCE
            self.data_service.write_account(account)
            message = f"Amount debited. New balance: {account.balance:.2f}"
            return True, message, account.balance
        else:                                       # Équivalent ELSE
            message = "Insufficient funds for this debit."
            return False, message, account.balance
```

### 3. Gestion des Données (data.cob → data_service.py)

#### COBOL Original
```cobol
IDENTIFICATION DIVISION.
PROGRAM-ID. DataProgram.

DATA DIVISION.
WORKING-STORAGE SECTION.
01  STORAGE-BALANCE    PIC 9(6)V99 VALUE 1000.00.
01  OPERATION-TYPE     PIC X(6).

LINKAGE SECTION.
01  PASSED-OPERATION   PIC X(6).
01  BALANCE            PIC 9(6)V99.

PROCEDURE DIVISION USING PASSED-OPERATION BALANCE.
    MOVE PASSED-OPERATION TO OPERATION-TYPE

    IF OPERATION-TYPE = 'READ'
        MOVE STORAGE-BALANCE TO BALANCE
        
    ELSE IF OPERATION-TYPE = 'WRITE'
        MOVE BALANCE TO STORAGE-BALANCE
        
    END-IF
    GOBACK.
```

#### Python Équivalent
```python
class DataService:
    def read_account(self) -> Account:
        """Équivalent IF OPERATION-TYPE = 'READ'."""
        if not self.data_file_path.exists():
            # Équivalent STORAGE-BALANCE VALUE 1000.00
            account = Account(balance=Decimal('1000.00'))
            self.write_account(account)
            return account
        
        with open(self.data_file_path, 'r') as file:
            data = json.load(file)
            return Account.from_dict(data)

    def write_account(self, account: Account) -> bool:
        """Équivalent IF OPERATION-TYPE = 'WRITE'."""
        with open(self.data_file_path, 'w') as file:
            json.dump(account.to_dict(), file, indent=2)
        return True
```

## Correspondance des Types de Données

| COBOL | Python | Description |
|-------|---------|-------------|
| `PIC 9(6)V99` | `Decimal` avec 2 décimales | Montants monétaires |
| `PIC X(6)` | `str` | Chaînes de caractères |
| `PIC 9` | `int` | Nombres entiers |
| `VALUE 1000.00` | `default_factory=lambda: Decimal('1000.00')` | Valeurs par défaut |

## Correspondance des Opérations

| COBOL | Python | Description |
|-------|---------|-------------|
| `ACCEPT variable` | `input()` | Saisie utilisateur |
| `DISPLAY message` | `print()` | Affichage |
| `ADD amount TO balance` | `balance += amount` | Addition |
| `SUBTRACT amount FROM balance` | `balance -= amount` | Soustraction |
| `MOVE value TO variable` | `variable = value` | Affectation |
| `CALL 'Program' USING params` | `method_call(params)` | Appel de fonction |
| `PERFORM UNTIL condition` | `while condition:` | Boucle |
| `EVALUATE variable WHEN value` | `if variable == value:` | Structure conditionnelle |
| `GOBACK` | `return` | Retour de fonction |
| `STOP RUN` | `sys.exit()` | Arrêt du programme |

## Améliorations Apportées

### 1. Gestion des Erreurs
- **COBOL** : Codes de retour implicites
- **Python** : Exceptions explicites et gestion d'erreurs structurée

### 2. Persistance des Données
- **COBOL** : Données en mémoire (WORKING-STORAGE)
- **Python** : Persistance JSON sur disque

### 3. Modularité
- **COBOL** : Programmes séparés avec CALL
- **Python** : Classes et modules avec import

### 4. Types et Validation
- **COBOL** : Types PIC implicites
- **Python** : Types explicites avec validation

### 5. Tests
- **COBOL** : Pas de tests unitaires
- **Python** : Suite complète de tests unitaires avec pytest

### 6. Logging
- **COBOL** : Pas de journalisation
- **Python** : Logging structuré pour le debugging et l'audit

### 7. Configuration
- **COBOL** : Valeurs codées en dur
- **Python** : Configuration externalisable

## Équivalence Fonctionnelle

La réécriture Python garantit une équivalence fonctionnelle stricte :

1. **Solde initial** : 1000.00 (identique)
2. **Crédit** : Addition au solde (logique identique)
3. **Débit** : Soustraction avec vérification des fonds (logique identique)
4. **Messages** : Textes identiques au COBOL
5. **Validation** : Mêmes règles métier
6. **Interface** : Menu et interactions identiques

## Tests de Conformité

Les tests unitaires Python valident la conformité avec le plan de test original (TESTPLAN.md) :

- TC-1.1 : Consultation du solde ✓
- TC-2.1 : Crédit avec montant valide ✓
- TC-2.2 : Crédit avec montant zéro ✓
- TC-3.1 : Débit avec montant valide ✓
- TC-3.2 : Débit avec fonds insuffisants ✓
- TC-3.3 : Débit avec montant zéro ✓
- TC-4.1 : Sortie de l'application ✓
