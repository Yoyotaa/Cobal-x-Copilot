"""
Service de persistance des données.

Ce module gère la sauvegarde et le chargement des données du compte,
équivalent aux opérations READ/WRITE du module data.cob en COBOL.
"""

import json
import os
from pathlib import Path
from typing import Optional
import logging
from decimal import Decimal

from models.account import Account

logger = logging.getLogger(__name__)


class DataService:
    """
    Service de persistance des données du compte.
    
    Équivalent COBOL : DataProgram avec opérations READ/WRITE
    
    Responsabilités :
    - Sauvegarde du solde en JSON (vs WORKING-STORAGE en mémoire COBOL)
    - Chargement du solde au démarrage
    - Gestion des erreurs de fichier
    """
    
    def __init__(self, data_file_path: Optional[str] = None):
        """
        Initialise le service de données.
        
        Args:
            data_file_path: Chemin vers le fichier de données JSON.
                           Si None, utilise le chemin par défaut.
        """
        if data_file_path is None:
            # Créer le dossier data à la racine du projet
            project_root = Path(__file__).parent.parent.parent
            data_dir = project_root / "data"
            data_dir.mkdir(exist_ok=True)
            self.data_file_path = data_dir / "account_balance.json"
        else:
            self.data_file_path = Path(data_file_path)
        
        # logger.info(f"DataService initialisé avec le fichier: {self.data_file_path}")
    
    def read_account(self) -> Account:
        """
        Lit les données du compte depuis le fichier JSON.
        
        Équivalent COBOL : 
        CALL 'DataProgram' USING 'READ', FINAL-BALANCE
        
        Returns:
            Account: Objet Account avec les données chargées
            
        Note:
            Si le fichier n'existe pas, retourne un compte avec solde initial 1000.00
        """
        try:
            if not self.data_file_path.exists():
                # logger.info("Fichier de données non trouvé, création d'un nouveau compte")
                account = Account()  # Solde initial 1000.00
                self.write_account(account)  # Sauvegarde immédiate
                return account
            
            with open(self.data_file_path, 'r', encoding='utf-8') as file:
                data = json.load(file)
                account = Account.from_dict(data)
                # logger.info(f"Compte chargé avec succès: solde = {account.balance}")
                return account
                
        except (json.JSONDecodeError, KeyError, ValueError) as e:
            # logger.error(f"Erreur lors du chargement du fichier: {e}")
            # logger.info("Création d'un nouveau compte avec solde initial")
            account = Account()
            self.write_account(account)
            return account
        except Exception as e:
            # logger.error(f"Erreur inattendue lors de la lecture: {e}")
            raise
    
    def write_account(self, account: Account) -> bool:
        """
        Sauvegarde les données du compte dans le fichier JSON.
        
        Équivalent COBOL : 
        CALL 'DataProgram' USING 'WRITE', FINAL-BALANCE
        
        Args:
            account: Objet Account à sauvegarder
            
        Returns:
            bool: True si la sauvegarde a réussi, False sinon
        """
        try:
            # Créer le dossier parent si nécessaire
            self.data_file_path.parent.mkdir(parents=True, exist_ok=True)
            
            # Sauvegarde atomique : écrire dans un fichier temporaire puis renommer
            temp_file = self.data_file_path.with_suffix('.tmp')
            
            with open(temp_file, 'w', encoding='utf-8') as file:
                json.dump(account.to_dict(), file, indent=2, ensure_ascii=False)
            
            # Renommer le fichier temporaire (opération atomique)
            temp_file.replace(self.data_file_path)
            
            # logger.info(f"Compte sauvegardé avec succès: solde = {account.balance}")
            return True
            
        except Exception as e:
            # logger.error(f"Erreur lors de la sauvegarde: {e}")
            # Nettoyer le fichier temporaire en cas d'erreur
            temp_file = self.data_file_path.with_suffix('.tmp')
            if temp_file.exists():
                temp_file.unlink()
            return False
    
    def backup_account(self, account: Account, backup_suffix: str = None) -> bool:
        """
        Crée une sauvegarde du compte avec un suffixe donné.
        
        Args:
            account: Compte à sauvegarder
            backup_suffix: Suffixe pour le fichier de sauvegarde (défaut: timestamp)
            
        Returns:
            bool: True si la sauvegarde a réussi
        """
        try:
            if backup_suffix is None:
                from datetime import datetime
                backup_suffix = datetime.now().strftime("%Y%m%d_%H%M%S")
            
            backup_path = self.data_file_path.with_name(
                f"{self.data_file_path.stem}_backup_{backup_suffix}.json"
            )
            
            with open(backup_path, 'w', encoding='utf-8') as file:
                json.dump(account.to_dict(), file, indent=2, ensure_ascii=False)
            
            # logger.info(f"Sauvegarde créée: {backup_path}")
            return True
            
        except Exception as e:
            # logger.error(f"Erreur lors de la création de la sauvegarde: {e}")
            return False
    
    def file_exists(self) -> bool:
        """
        Vérifie si le fichier de données existe.
        
        Returns:
            bool: True si le fichier existe
        """
        return self.data_file_path.exists()
    
    def get_file_path(self) -> Path:
        """
        Retourne le chemin du fichier de données.
        
        Returns:
            Path: Chemin vers le fichier de données
        """
        return self.data_file_path
    
    def reset_account(self) -> Account:
        """
        Remet le compte à zéro avec le solde initial de 1000.00.
        
        Équivalent COBOL : Réinitialisation de STORAGE-BALANCE à 1000.00
        
        Returns:
            Account: Nouveau compte avec solde initial
        """
        # logger.info("Réinitialisation du compte au solde initial")
        account = Account()  # Solde initial 1000.00
        self.write_account(account)
        return account
