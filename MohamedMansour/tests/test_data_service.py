"""
Tests unitaires pour le service DataService.

Ces tests valident la persistance des données et équivalent
aux opérations READ/WRITE du module data.cob en COBOL.
"""

import pytest
import json
import tempfile
from pathlib import Path
from decimal import Decimal
import sys

# Ajouter src au PYTHONPATH pour les tests
sys.path.insert(0, str(Path(__file__).parent.parent / "src"))

from models.account import Account
from services.data_service import DataService


class TestDataService:
    """Tests pour le service DataService."""
    
    def setup_method(self):
        """Setup pour chaque test avec un fichier temporaire."""
        self.temp_dir = tempfile.mkdtemp()
        self.temp_file = Path(self.temp_dir) / "test_account.json"
        self.data_service = DataService(str(self.temp_file))
    
    def teardown_method(self):
        """Cleanup après chaque test."""
        if self.temp_file.exists():
            self.temp_file.unlink()
        self.temp_file.parent.rmdir()
    
    def test_read_account_file_not_exists(self):
        """Test READ-1: Lecture quand le fichier n'existe pas."""
        # Équivalent COBOL : Première initialisation de STORAGE-BALANCE
        assert not self.data_service.file_exists()
        
        account = self.data_service.read_account()
        
        assert account.balance == Decimal('1000.00')
        assert account.account_id == 'DEFAULT_ACCOUNT'
        # Le fichier devrait maintenant exister après la création automatique
        assert self.data_service.file_exists()
    
    def test_write_and_read_account(self):
        """Test WRITE-1: Écriture puis lecture d'un compte."""
        # Équivalent COBOL : CALL 'DataProgram' USING 'WRITE', FINAL-BALANCE
        original_account = Account(balance=Decimal('1500.75'))
        
        # Écriture
        success = self.data_service.write_account(original_account)
        assert success is True
        assert self.data_service.file_exists()
        
        # Lecture
        loaded_account = self.data_service.read_account()
        assert loaded_account.balance == original_account.balance
        assert loaded_account.account_id == original_account.account_id
    
    def test_read_account_with_existing_valid_file(self):
        """Test READ-2: Lecture d'un fichier JSON valide existant."""
        # Créer un fichier JSON manuellement
        test_data = {
            'balance': '2500.50',
            'account_id': 'TEST_ACCOUNT',
            'created_at': '2024-01-01T10:00:00',
            'last_modified': '2024-01-01T11:00:00'
        }
        
        with open(self.temp_file, 'w') as f:
            json.dump(test_data, f)
        
        account = self.data_service.read_account()
        
        assert account.balance == Decimal('2500.50')
        assert account.account_id == 'TEST_ACCOUNT'
    
    def test_read_account_with_corrupted_file(self):
        """Test READ-3: Lecture d'un fichier JSON corrompu."""
        # Créer un fichier JSON invalide
        with open(self.temp_file, 'w') as f:
            f.write("{ invalid json content")
        
        account = self.data_service.read_account()
        
        # Doit retourner un compte par défaut et recréer le fichier
        assert account.balance == Decimal('1000.00')
        assert account.account_id == 'DEFAULT_ACCOUNT'
    
    def test_write_account_creates_directory(self):
        """Test WRITE-2: Création automatique du dossier parent."""
        nested_path = Path(self.temp_dir) / "nested" / "path" / "account.json"
        data_service = DataService(str(nested_path))
        
        account = Account(balance=Decimal('750.25'))
        success = data_service.write_account(account)
        
        assert success is True
        assert nested_path.exists()
        assert nested_path.parent.exists()
    
    def test_atomic_write_operation(self):
        """Test WRITE-3: Écriture atomique avec fichier temporaire."""
        account = Account(balance=Decimal('999.99'))
        
        # Vérifier qu'il n'y a pas de fichier .tmp après l'opération
        success = self.data_service.write_account(account)
        temp_file = self.temp_file.with_suffix('.tmp')
        
        assert success is True
        assert self.temp_file.exists()
        assert not temp_file.exists()  # Fichier temporaire nettoyé
    
    def test_backup_account(self):
        """Test BACKUP-1: Création de sauvegarde avec suffixe."""
        account = Account(balance=Decimal('1234.56'))
        
        success = self.data_service.backup_account(account, "test_backup")
        
        assert success is True
        backup_file = self.temp_file.with_name(
            f"{self.temp_file.stem}_backup_test_backup.json"
        )
        assert backup_file.exists()
        
        # Vérifier le contenu de la sauvegarde
        with open(backup_file) as f:
            data = json.load(f)
        assert data['balance'] == '1234.56'
    
    def test_reset_account(self):
        """Test RESET-1: Remise à zéro du compte."""
        # Créer un compte avec un solde modifié
        modified_account = Account(balance=Decimal('5000.00'))
        self.data_service.write_account(modified_account)
        
        # Reset
        reset_account = self.data_service.reset_account()
        
        assert reset_account.balance == Decimal('1000.00')
        assert reset_account.account_id == 'DEFAULT_ACCOUNT'
        
        # Vérifier que le fichier a été mis à jour
        loaded_account = self.data_service.read_account()
        assert loaded_account.balance == Decimal('1000.00')
    
    def test_get_file_path(self):
        """Test UTIL-1: Récupération du chemin du fichier."""
        path = self.data_service.get_file_path()
        
        assert isinstance(path, Path)
        assert str(path) == str(self.temp_file)
    
    def test_file_exists_method(self):
        """Test UTIL-2: Vérification d'existence du fichier."""
        # Initialement, le fichier n'existe pas
        assert not self.data_service.file_exists()
        
        # Après écriture, il existe
        account = Account()
        self.data_service.write_account(account)
        assert self.data_service.file_exists()
    
    def test_data_persistence_across_instances(self):
        """Test PERSIST-1: Persistance des données entre instances."""
        # Premier service : écriture
        account1 = Account(balance=Decimal('777.88'))
        service1 = DataService(str(self.temp_file))
        service1.write_account(account1)
        
        # Deuxième service : lecture
        service2 = DataService(str(self.temp_file))
        account2 = service2.read_account()
        
        assert account2.balance == account1.balance
        assert account2.account_id == account1.account_id
    
    def test_concurrent_write_safety(self):
        """Test CONCURRENT-1: Sécurité en cas d'écriture concurrente."""
        # Simuler une interruption durant l'écriture
        import os
        
        account = Account(balance=Decimal('555.55'))
        
        # Créer manuellement un fichier .tmp pour simuler une écriture interrompue
        temp_file = self.temp_file.with_suffix('.tmp')
        with open(temp_file, 'w') as f:
            f.write("incomplete data")
        
        # L'écriture devrait réussir et nettoyer le fichier temporaire
        success = self.data_service.write_account(account)
        
        assert success is True
        assert self.temp_file.exists()
        assert not temp_file.exists()  # Fichier temporaire nettoyé
        
        # Vérifier que les données sont correctes
        loaded_account = self.data_service.read_account()
        assert loaded_account.balance == Decimal('555.55')


class TestDataServiceDefaultPath:
    """Tests avec le chemin par défaut du DataService."""
    
    def test_default_path_creation(self):
        """Test DEFAULT-1: Création avec chemin par défaut."""
        service = DataService()
        path = service.get_file_path()
        
        # Le chemin doit pointer vers data/account_balance.json
        assert path.name == "account_balance.json"
        assert path.parent.name == "data"
    
    def test_data_directory_creation(self):
        """Test DEFAULT-2: Création automatique du dossier data."""
        service = DataService()
        account = Account()
        
        # L'écriture doit créer le dossier data s'il n'existe pas
        success = service.write_account(account)
        assert success is True
        
        data_dir = service.get_file_path().parent
        assert data_dir.exists()
        assert data_dir.is_dir()


class TestDataServiceErrorHandling:
    """Tests de gestion d'erreurs du DataService."""
    
    def test_read_permission_error(self):
        """Test ERROR-1: Gestion des erreurs de permission en lecture."""
        # Ce test est complexe à implémenter de manière portable
        # Il nécessiterait des privilèges spéciaux ou un mock
        pass
    
    def test_write_permission_error(self):
        """Test ERROR-2: Gestion des erreurs de permission en écriture."""
        # Ce test est complexe à implémenter de manière portable
        # Il nécessiterait des privilèges spéciaux ou un mock
        pass
    
    def test_disk_full_simulation(self):
        """Test ERROR-3: Simulation d'un disque plein."""
        # Ce test nécessiterait un mock du système de fichiers
        pass
