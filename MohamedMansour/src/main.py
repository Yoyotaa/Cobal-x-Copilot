"""
Point d'entrée principal du système de gestion de comptes.

Ce module initialise l'application et lance l'interface utilisateur,
équivalent au programme principal main.cob en COBOL.
"""

import logging
import sys
from pathlib import Path

# Ajouter le dossier src au PYTHONPATH pour les imports
sys.path.insert(0, str(Path(__file__).parent))

from ui.console_interface import ConsoleInterface
from services.account_service import AccountService
from services.data_service import DataService


def setup_logging() -> None:
    """
    Configure le système de logging pour l'application.
    """
    # Configuration du logging entièrement désactivée
    import logging
    logging.disable(logging.CRITICAL)
    pass


def main() -> None:
    """
    Fonction principale de l'application.
    
    Équivalent COBOL : MAIN-LOGIC dans main.cob
    
    Initialise les services et lance l'interface utilisateur.
    """
    # Configuration du logging
    setup_logging()
    logger = logging.getLogger(__name__)
    
    try:
        # logger.info("=" * 50)
        # logger.info("Démarrage du système de gestion de comptes")
        # logger.info("Version Python - Réécriture du système COBOL")
        # logger.info("=" * 50)
        
        # Initialisation des services
        # logger.info("Initialisation des services...")
        
        # Service de données (équivalent data.cob)
        data_service = DataService()
        # logger.info(f"Service de données initialisé - Fichier: {data_service.get_file_path()}")
        
        # Service des opérations (équivalent operations.cob)
        account_service = AccountService(data_service)
        # logger.info("Service des opérations initialisé")
        
        # Interface utilisateur (équivalent main.cob)
        console_interface = ConsoleInterface(account_service)
        # logger.info("Interface console initialisée")
        
        # Vérification de l'état initial
        success, message, initial_balance = account_service.view_balance()
        if success:
            # logger.info(f"État initial du compte: {initial_balance}")
            pass
        else:
            # logger.warning(f"Problème lors de la vérification initiale: {message}")
            pass
        
        # Lancement de l'application
        # logger.info("Lancement de l'interface utilisateur...")
        console_interface.run()
        
        # logger.info("Arrêt normal du système")
        
    except KeyboardInterrupt:
        # logger.info("Arrêt demandé par l'utilisateur (Ctrl+C)")
        print("\nSystem shutdown requested. Goodbye!")
    except Exception as e:
        # logger.error(f"Erreur critique dans l'application: {e}", exc_info=True)
        print(f"\nCritical error: {e}")
        print("Please check the logs for more information.")
        sys.exit(1)
    finally:
        # logger.info("Fin de l'exécution du système")
        pass


def debug_mode() -> None:
    """
    Mode debug avec fonctionnalités supplémentaires.
    """
    setup_logging()
    logger = logging.getLogger(__name__)
    
    print("=" * 50)
    print("ACCOUNT MANAGEMENT SYSTEM - DEBUG MODE")
    print("=" * 50)
    
    try:
        # Services
        data_service = DataService()
        account_service = AccountService(data_service)
        console_interface = ConsoleInterface(account_service)
        
        # Menu debug
        while True:
            print("\nDEBUG MENU:")
            print("1. Run normal application")
            print("2. Show account information")
            print("3. Reset account to initial state")
            print("4. Show data file path")
            print("5. Exit debug mode")
            
            choice = input("Debug choice: ").strip()
            
            if choice == '1':
                console_interface.run()
                break
            elif choice == '2':
                console_interface.display_account_info()
            elif choice == '3':
                console_interface.reset_account_interactive()
            elif choice == '4':
                print(f"Data file: {data_service.get_file_path()}")
                print(f"File exists: {data_service.file_exists()}")
            elif choice == '5':
                print("Exiting debug mode.")
                break
            else:
                print("Invalid choice.")
                
    except Exception as e:
        # logger.error(f"Erreur en mode debug: {e}", exc_info=True)
        print(f"Debug error: {e}")


if __name__ == "__main__":
    # Vérifier si le mode debug est demandé
    if len(sys.argv) > 1 and sys.argv[1] == "--debug":
        debug_mode()
    else:
        main()
