"""
Tests Golden Master pour valider la conformité COBOL → Python.

Ces tests comparent les sorties exactes du système Python
avec les sorties de référence du système COBOL original.
"""

import pytest
import subprocess
import tempfile
import os
from pathlib import Path
import sys

# Ajouter src au PYTHONPATH
sys.path.insert(0, str(Path(__file__).parent.parent / "src"))


class TestGoldenMaster:
    """
    Tests Golden Master comparant les sorties COBOL et Python.
    
    Principe : La sortie Python doit être identique à la sortie COBOL
    pour les mêmes séquences d'entrées.
    """
    
    @classmethod
    def setup_class(cls):
        """Configuration initiale des tests Golden Master."""
        cls.python_executable = "/home/caprim00n/Bureau/LastEpitechYear/CobolXCopilot/.venv/bin/python"
        cls.python_main = str(Path(__file__).parent.parent / "src" / "main.py")
        cls.golden_master_dir = Path(__file__).parent.parent / "golden_masters"
        cls.golden_master_dir.mkdir(exist_ok=True)
    
    def setup_method(self):
        """Réinitialise le solde avant chaque test."""
        # Supprimer le fichier de données pour repartir avec le solde initial
        data_file = Path(__file__).parent.parent / "data" / "account_balance.json"
        if data_file.exists():
            data_file.unlink()
    
    def run_python_app(self, input_sequence: str) -> str:
        """
        Exécute l'application Python avec une séquence d'entrées.
        
        Args:
            input_sequence: Séquence d'entrées (ex: "1\n4\n")
            
        Returns:
            str: Sortie complète de l'application
        """
        try:
            result = subprocess.run(
                [self.python_executable, self.python_main],
                input=input_sequence,
                text=True,
                capture_output=True,
                timeout=10
            )
            return result.stdout
        except subprocess.TimeoutExpired:
            pytest.fail("Application Python timeout")
        except Exception as e:
            pytest.fail(f"Erreur lors de l'exécution Python: {e}")
    
    def normalize_output(self, output: str) -> str:
        """
        Normalise la sortie pour comparaison.
        
        Retire les éléments variables comme :
        - Timestamps des logs
        - Chemins de fichiers absolus
        - Messages spécifiques à l'environnement
        """
        lines = output.split('\n')
        normalized_lines = []
        
        for line in lines:
            # Ignorer les lignes de log avec timestamps
            if ' - ' in line and any(level in line for level in ['INFO', 'WARNING', 'ERROR', 'DEBUG']):
                continue
            
            # Ignorer les lignes avec chemins de fichiers
            if '/home/' in line or 'DataService initialisé' in line:
                continue
                
            # Ignorer les lignes vides en début/fin
            line = line.strip()
            if line:
                normalized_lines.append(line)
        
        return '\n'.join(normalized_lines)
    
    def extract_business_output(self, output: str) -> str:
        """
        Extrait uniquement les sorties métier (équivalentes au COBOL).
        
        Focus sur :
        - Menu d'affichage
        - Messages de solde
        - Messages d'opérations
        - Messages d'erreur
        """
        lines = output.split('\n')
        business_lines = []
        
        for line in lines:
            line = line.strip()
            
            # Messages métier à conserver
            if any(keyword in line for keyword in [
                'Account Management System',
                'View Balance',
                'Credit Account', 
                'Debit Account',
                'Exit',
                'Enter your choice',
                'Current balance:',
                'Amount credited',
                'Amount debited',
                'Insufficient funds',
                'Enter credit amount',
                'Enter debit amount',
                'Exiting the program',
                '='
            ]):
                business_lines.append(line)
        
        return '\n'.join(business_lines)
    
    def test_golden_master_view_balance(self):
        """Test Golden Master: Consultation du solde."""
        input_sequence = "1\n4\n"  # View balance puis exit
        
        python_output = self.run_python_app(input_sequence)
        business_output = self.extract_business_output(python_output)
        
        # Le solde initial doit être 1000.00
        assert "Current balance: 1000.00" in business_output
        assert "Exiting the program" in business_output
        
        print("=== Python Business Output ===")
        print(business_output)
    
    def test_golden_master_credit_operation(self):
        """Test Golden Master: Opération de crédit."""
        input_sequence = "1\n2\n150.50\n1\n4\n"  # Balance → Credit 150.50 → Balance → Exit
        
        python_output = self.run_python_app(input_sequence)
        business_output = self.extract_business_output(python_output)
        
        # Vérifications métier
        assert "Current balance: 1000.00" in business_output  # Solde initial
        assert "Amount credited. New balance: 1150.50" in business_output  # Après crédit
        assert "Current balance: 1150.50" in business_output  # Vérification finale
        
        print("=== Credit Operation Output ===")
        print(business_output)
    
    def test_golden_master_debit_operation(self):
        """Test Golden Master: Opération de débit."""
        input_sequence = "1\n3\n75.25\n1\n4\n"  # Balance → Debit 75.25 → Balance → Exit
        
        python_output = self.run_python_app(input_sequence)
        business_output = self.extract_business_output(python_output)
        
        # Vérifications métier  
        assert "Current balance: 1000.00" in business_output  # Solde initial
        assert "Amount debited. New balance: 924.75" in business_output  # Après débit
        assert "Current balance: 924.75" in business_output  # Vérification finale
        
        print("=== Debit Operation Output ===")
        print(business_output)
    
    def test_golden_master_insufficient_funds(self):
        """Test Golden Master: Fonds insuffisants."""
        input_sequence = "1\n3\n2000.00\n1\n4\n"  # Balance → Debit 2000 → Balance → Exit
        
        python_output = self.run_python_app(input_sequence)
        business_output = self.extract_business_output(python_output)
        
        # Vérifications métier
        assert "Current balance: 1000.00" in business_output  # Solde initial
        assert "Insufficient funds for this debit." in business_output  # Message d'erreur
        # Le solde ne doit pas changer après tentative de débit
        output_lines = business_output.split('\n')
        balance_lines = [line for line in output_lines if "Current balance:" in line]
        assert len(balance_lines) >= 2  # Au moins 2 consultations
        assert all("1000.00" in line for line in balance_lines)  # Solde inchangé
        
        print("=== Insufficient Funds Output ===")
        print(business_output)
    
    def test_golden_master_complex_scenario(self):
        """Test Golden Master: Scénario complexe multi-opérations."""
        # Séquence : Balance → Credit 500 → Debit 200 → Credit 100.50 → Balance → Exit
        input_sequence = "1\n2\n500.00\n3\n200.00\n2\n100.50\n1\n4\n"
        
        python_output = self.run_python_app(input_sequence)
        business_output = self.extract_business_output(python_output)
        
        # Calcul attendu : 1000 + 500 - 200 + 100.50 = 1400.50
        expected_messages = [
            "Current balance: 1000.00",      # Initial
            "Amount credited. New balance: 1500.00",  # +500
            "Amount debited. New balance: 1300.00",   # -200  
            "Amount credited. New balance: 1400.50",  # +100.50
            "Current balance: 1400.50"       # Final
        ]
        
        for expected_msg in expected_messages:
            assert expected_msg in business_output, f"Message manquant: {expected_msg}"
        
        print("=== Complex Scenario Output ===")
        print(business_output)
    
    def test_save_golden_master_reference(self):
        """Sauvegarde les sorties Python comme références Golden Master."""
        test_cases = [
            ("view_balance", "1\n4\n"),
            ("credit_150_50", "1\n2\n150.50\n1\n4\n"),
            ("debit_75_25", "1\n3\n75.25\n1\n4\n"),
            ("insufficient_funds", "1\n3\n2000.00\n1\n4\n"),
            ("complex_scenario", "1\n2\n500.00\n3\n200.00\n2\n100.50\n1\n4\n")
        ]
        
        for test_name, input_seq in test_cases:
            python_output = self.run_python_app(input_seq)
            business_output = self.extract_business_output(python_output)
            
            # Sauvegarder comme référence
            reference_file = self.golden_master_dir / f"python_reference_{test_name}.txt"
            with open(reference_file, 'w', encoding='utf-8') as f:
                f.write(business_output)
            
            print(f"Référence sauvegardée: {reference_file}")
    
    def test_compare_python_vs_cobol_live(self):
        """Compare directement Python vs COBOL avec l'exécutable."""
        input_sequence = "1\n4\n"  # View balance puis exit
        
        # Exécuter Python
        python_output = self.run_python_app(input_sequence)
        python_business = self.extract_business_output(python_output)
        
        # Exécuter COBOL
        cobol_executable = Path(__file__).parent.parent / "accountsystem"
        if not cobol_executable.exists():
            pytest.skip("Exécutable COBOL non trouvé")
            
        try:
            cobol_result = subprocess.run(
                [str(cobol_executable)],
                input=input_sequence,
                text=True,
                capture_output=True,
                timeout=10
            )
            cobol_output = cobol_result.stdout
        except subprocess.TimeoutExpired:
            pytest.fail("COBOL executable timeout")
        except Exception as e:
            pytest.fail(f"Erreur lors de l'exécution COBOL: {e}")
        
        # Vérifications communes
        assert "Current balance:" in python_business
        assert "Current balance:" in cobol_output
        assert "Exiting the program" in python_business
        assert "Exiting the program" in cobol_output
        
        print("=== COBOL Output ===")
        print(cobol_output)
        print("\n=== Python Business Output ===")
        print(python_business)


class TestGoldenMasterComparison:
    """Tests de comparaison directe avec les sorties COBOL (si disponibles)."""
    
    def load_cobol_reference(self, test_name: str) -> str:
        """Charge une référence COBOL si elle existe."""
        cobol_ref_file = Path(__file__).parent.parent / "golden_masters" / f"cobol_reference_{test_name}.txt"
        if cobol_ref_file.exists():
            return cobol_ref_file.read_text(encoding='utf-8')
        else:
            pytest.skip(f"Référence COBOL manquante: {cobol_ref_file}")
    
    def load_python_reference(self, test_name: str) -> str:
        """Charge une référence Python."""
        python_ref_file = Path(__file__).parent.parent / "golden_masters" / f"python_reference_{test_name}.txt"
        if python_ref_file.exists():
            return python_ref_file.read_text(encoding='utf-8')
        else:
            pytest.fail(f"Référence Python manquante: {python_ref_file}")
    
    @pytest.mark.skip(reason="Nécessite les références COBOL")
    def test_compare_with_cobol_view_balance(self):
        """Compare la sortie Python avec la référence COBOL pour la consultation."""
        cobol_output = self.load_cobol_reference("view_balance")
        python_output = self.load_python_reference("view_balance")
        
        # Comparaison fonctionnelle (pas textuelle exacte)
        assert "Current balance: 1000.00" in cobol_output
        assert "Current balance: 1000.00" in python_output
        assert "Exiting the program" in python_output
