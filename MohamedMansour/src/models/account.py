"""
Modèle de données pour le compte bancaire.

Ce module définit la structure de données représentant un compte bancaire,
équivalent aux définitions WORKING-STORAGE du COBOL original.
"""

from dataclasses import dataclass, field
from decimal import Decimal, ROUND_HALF_UP
from typing import Optional
from datetime import datetime
import logging

logger = logging.getLogger(__name__)


@dataclass
class Account:
    """
    Modèle de données pour un compte bancaire.
    
    Équivalent COBOL : FINAL-BALANCE PIC 9(6)V99 VALUE 1000.00
    
    Attributes:
        balance: Solde du compte avec précision décimale (2 décimales)
        account_id: Identifiant unique du compte
        created_at: Date de création du compte
        last_modified: Date de dernière modification
    """
    
    balance: Decimal = field(default_factory=lambda: Decimal('1000.00'))
    account_id: str = field(default='DEFAULT_ACCOUNT')
    created_at: Optional[datetime] = field(default_factory=datetime.now)
    last_modified: Optional[datetime] = field(default_factory=datetime.now)
    
    def __post_init__(self):
        """Validation et normalisation des données après initialisation."""
        # Assurer que le solde a exactement 2 décimales (équivalent PIC 9(6)V99)
        if isinstance(self.balance, (int, float, str)):
            self.balance = Decimal(str(self.balance))
        
        # Normaliser à 2 décimales
        self.balance = self.balance.quantize(
            Decimal('0.01'), 
            rounding=ROUND_HALF_UP
        )
        
        # Validation : solde ne peut pas être négatif (contrainte métier)
        if self.balance < 0:
            # logger.warning(f"Balance négative détectée: {self.balance}, correction à 0.00")
            self.balance = Decimal('0.00')
    
    def credit(self, amount: Decimal) -> bool:
        """
        Crédite le compte d'un montant donné.
        
        Équivalent COBOL : ADD AMOUNT TO FINAL-BALANCE
        
        Args:
            amount: Montant à créditer (doit être positif)
            
        Returns:
            bool: True si l'opération a réussi, False sinon
            
        Raises:
            ValueError: Si le montant est négatif ou nul
        """
        if isinstance(amount, (int, float, str)):
            amount = Decimal(str(amount))
        
        amount = amount.quantize(Decimal('0.01'), rounding=ROUND_HALF_UP)
        
        if amount <= 0:
            raise ValueError(f"Le montant du crédit doit être positif: {amount}")
        
        old_balance = self.balance
        self.balance += amount
        self.last_modified = datetime.now()
        
        # logger.info(f"Crédit de {amount}: {old_balance} -> {self.balance}")
        return True
    
    def debit(self, amount: Decimal) -> bool:
        """
        Débite le compte d'un montant donné si les fonds sont suffisants.
        
        Équivalent COBOL : 
        IF FINAL-BALANCE >= AMOUNT
            SUBTRACT AMOUNT FROM FINAL-BALANCE
        ELSE
            DISPLAY "Insufficient funds for this debit."
        
        Args:
            amount: Montant à débiter (doit être positif)
            
        Returns:
            bool: True si l'opération a réussi, False si fonds insuffisants
            
        Raises:
            ValueError: Si le montant est négatif ou nul
        """
        if isinstance(amount, (int, float, str)):
            amount = Decimal(str(amount))
        
        amount = amount.quantize(Decimal('0.01'), rounding=ROUND_HALF_UP)
        
        if amount <= 0:
            raise ValueError(f"Le montant du débit doit être positif: {amount}")
        
        if self.balance < amount:
            # logger.warning(f"Fonds insuffisants: solde={self.balance}, débit demandé={amount}")
            return False
        
        old_balance = self.balance
        self.balance -= amount
        self.last_modified = datetime.now()
        
        # logger.info(f"Débit de {amount}: {old_balance} -> {self.balance}")
        return True
    
    def get_balance(self) -> Decimal:
        """
        Retourne le solde actuel du compte.
        
        Équivalent COBOL : DISPLAY "Current balance: " FINAL-BALANCE
        
        Returns:
            Decimal: Solde actuel avec 2 décimales
        """
        return self.balance
    
    def to_dict(self) -> dict:
        """Convertit l'objet Account en dictionnaire pour la sérialisation."""
        return {
            'balance': str(self.balance),
            'account_id': self.account_id,
            'created_at': self.created_at.isoformat() if self.created_at else None,
            'last_modified': self.last_modified.isoformat() if self.last_modified else None
        }
    
    @classmethod
    def from_dict(cls, data: dict) -> 'Account':
        """Crée un objet Account à partir d'un dictionnaire."""
        account = cls(
            balance=Decimal(data.get('balance', '1000.00')),
            account_id=data.get('account_id', 'DEFAULT_ACCOUNT')
        )
        
        if data.get('created_at'):
            account.created_at = datetime.fromisoformat(data['created_at'])
        if data.get('last_modified'):
            account.last_modified = datetime.fromisoformat(data['last_modified'])
            
        return account
    
    def __str__(self) -> str:
        """Représentation string pour l'affichage."""
        return f"Account({self.account_id}): {self.balance:.2f}"
    
    def __repr__(self) -> str:
        """Représentation détaillée pour le debug."""
        return (f"Account(balance={self.balance}, account_id='{self.account_id}', "
                f"created_at={self.created_at}, last_modified={self.last_modified})")
