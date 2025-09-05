"""
Module de gestion des données - Équivalent Python de data.cob
Gère la persistance et la récupération du solde du compte bancaire.
"""

import json
import os
from typing import Union


class DataManager:
    """
    Classe responsable de la gestion des données du compte bancaire.
    Équivalent au module DataProgram en COBOL.
    """
    
    def __init__(self, data_file: str = "account_data.json"):
        """
        Initialise le gestionnaire de données.
        
        Args:
            data_file (str): Chemin vers le fichier de données
        """
        self.data_file = data_file
        self._ensure_data_file_exists()
    
    def _ensure_data_file_exists(self) -> None:
        """
        S'assure que le fichier de données existe avec un solde initial.
        """
        if not os.path.exists(self.data_file):
            initial_data = {"balance": 1000.00}
            self._write_data(initial_data)
    
    def _read_data(self) -> dict:
        """
        Lit les données depuis le fichier JSON.
        
        Returns:
            dict: Données du compte
        """
        try:
            with open(self.data_file, 'r', encoding='utf-8') as file:
                return json.load(file)
        except (FileNotFoundError, json.JSONDecodeError):
            # En cas d'erreur, retourner les données par défaut
            return {"balance": 1000.00}
    
    def _write_data(self, data: dict) -> None:
        """
        Écrit les données dans le fichier JSON.
        
        Args:
            data (dict): Données à écrire
        """
        with open(self.data_file, 'w', encoding='utf-8') as file:
            json.dump(data, file, indent=2, ensure_ascii=False)
    
    def read_balance(self) -> float:
        """
        Lit le solde actuel du compte.
        Équivalent à l'opération 'READ' en COBOL.
        
        Returns:
            float: Solde actuel du compte
        """
        data = self._read_data()
        return data.get("balance", 1000.00)
    
    def write_balance(self, balance: float) -> None:
        """
        Écrit le nouveau solde du compte.
        Équivalent à l'opération 'WRITE' en COBOL.
        
        Args:
            balance (float): Nouveau solde à enregistrer
        """
        data = self._read_data()
        data["balance"] = balance
        self._write_data(data)
    
    def get_balance(self) -> float:
        """
        Méthode utilitaire pour obtenir le solde actuel.
        
        Returns:
            float: Solde actuel du compte
        """
        return self.read_balance()
