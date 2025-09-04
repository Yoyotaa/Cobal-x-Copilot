#!/usr/bin/env python3
"""
Tests de performance pour l'application comptable Python
Conforme aux standards RNCP - Niveau production
"""

import time
import statistics
import concurrent.futures
import pytest
import sys
import os
from unittest.mock import patch
import tempfile

# Ajouter le chemin du module
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

from operations import Operations
from data import DataProgram

class PerformanceTestSuite:
    """Suite de tests de performance pour l'application comptable"""
    
    def __init__(self):
        self.setup_test_environment()
        
    def setup_test_environment(self):
        """Configuration de l'environnement de test"""
        self.temp_file = tempfile.NamedTemporaryFile(mode='w+', delete=False)
        self.temp_file.write('{"balance": 1000.0}')
        self.temp_file.close()
        
        self.operations = Operations()
        self.operations.data_program = DataProgram(self.temp_file.name)
        
    def measure_execution_time(self, func, *args, **kwargs):
        """Mesure le temps d'exécution d'une fonction"""
        start_time = time.perf_counter()
        result = func(*args, **kwargs)
        end_time = time.perf_counter()
        return end_time - start_time, result
    
    def test_view_balance_performance(self, iterations=100):
        """Test de performance pour la consultation du solde"""
        print("\n🔍 Test de performance - Consultation du solde")
        print("=" * 50)
        
        execution_times = []
        
        for i in range(iterations):
            with patch('builtins.print'):
                execution_time, result = self.measure_execution_time(
                    self.operations.view_balance
                )
                execution_times.append(execution_time)
        
        # Statistiques
        avg_time = statistics.mean(execution_times)
        min_time = min(execution_times)
        max_time = max(execution_times)
        median_time = statistics.median(execution_times)
        
        print(f"📊 Résultats pour {iterations} itérations :")
        print(f"   ⏱️  Temps moyen : {avg_time*1000:.2f} ms")
        print(f"   ⚡ Temps minimum : {min_time*1000:.2f} ms")
        print(f"   🐌 Temps maximum : {max_time*1000:.2f} ms")
        print(f"   📐 Temps médian : {median_time*1000:.2f} ms")
        
        # Critères de performance
        assert avg_time < 0.01, f"Performance dégradée : {avg_time*1000:.2f}ms > 10ms"
        assert max_time < 0.05, f"Pic de latence : {max_time*1000:.2f}ms > 50ms"
        
        return {
            'average': avg_time,
            'min': min_time,
            'max': max_time,
            'median': median_time
        }
    
    def test_credit_performance(self, iterations=500):
        """Test de performance pour les opérations de crédit"""
        print("\n💰 Test de performance - Opérations de crédit")
        print("=" * 50)
        
        execution_times = []
        
        for i in range(iterations):
            with patch('builtins.input', return_value='100.00'):
                with patch('builtins.print'):
                    execution_time, result = self.measure_execution_time(
                        self.operations.credit_account
                    )
                    execution_times.append(execution_time)
        
        avg_time = statistics.mean(execution_times)
        min_time = min(execution_times)
        max_time = max(execution_times)
        
        print(f"📊 Résultats pour {iterations} crédits :")
        print(f"   ⏱️  Temps moyen : {avg_time*1000:.2f} ms")
        print(f"   ⚡ Temps minimum : {min_time*1000:.2f} ms")
        print(f"   🐌 Temps maximum : {max_time*1000:.2f} ms")
        
        # Critères de performance pour les écritures
        assert avg_time < 0.02, f"Performance écriture dégradée : {avg_time*1000:.2f}ms > 20ms"
        
        return {'average': avg_time, 'min': min_time, 'max': max_time}
    
    def test_debit_performance(self, iterations=500):
        """Test de performance pour les opérations de débit"""
        print("\n💸 Test de performance - Opérations de débit")
        print("=" * 50)
        
        execution_times = []
        
        for i in range(iterations):
            with patch('builtins.input', return_value='50.00'):
                with patch('builtins.print'):
                    execution_time, result = self.measure_execution_time(
                        self.operations.debit_account
                    )
                    execution_times.append(execution_time)
        
        avg_time = statistics.mean(execution_times)
        min_time = min(execution_times)
        max_time = max(execution_times)
        
        print(f"📊 Résultats pour {iterations} débits :")
        print(f"   ⏱️  Temps moyen : {avg_time*1000:.2f} ms")
        print(f"   ⚡ Temps minimum : {min_time*1000:.2f} ms")
        print(f"   🐌 Temps maximum : {max_time*1000:.2f} ms")
        
        assert avg_time < 0.02, f"Performance débit dégradée : {avg_time*1000:.2f}ms > 20ms"
        
        return {'average': avg_time, 'min': min_time, 'max': max_time}
    
    def test_concurrent_operations(self, num_threads=5, operations_per_thread=10):
        """Test de performance sous charge concurrente"""
        print(f"\n🔄 Test de performance - {num_threads} threads concurrents")
        print("=" * 50)
        
        def concurrent_operation():
            """Opération à exécuter de manière concurrente"""
            local_ops = Operations()
            local_ops.data_program = DataProgram(self.temp_file.name)
            
            start_time = time.perf_counter()
            
            for _ in range(operations_per_thread):
                with patch('builtins.print'):
                    local_ops.view_balance()
            
            return time.perf_counter() - start_time
        
        # Exécution concurrente
        start_total = time.perf_counter()
        
        with concurrent.futures.ThreadPoolExecutor(max_workers=num_threads) as executor:
            futures = [executor.submit(concurrent_operation) for _ in range(num_threads)]
            results = [future.result() for future in concurrent.futures.as_completed(futures)]
        
        total_time = time.perf_counter() - start_total
        total_operations = num_threads * operations_per_thread
        avg_throughput = total_operations / total_time
        
        print(f"📊 Résultats concurrence :")
        print(f"   🧵 Threads : {num_threads}")
        print(f"   🔢 Total opérations : {total_operations}")
        print(f"   ⏱️  Temps total : {total_time:.2f}s")
        print(f"   ⚡ Débit moyen : {avg_throughput:.0f} ops/sec")
        
        # Critères de performance concurrentielle
        assert avg_throughput > 100, f"Débit insuffisant : {avg_throughput:.0f} < 100 ops/sec"
        
        return {
            'total_time': total_time,
            'throughput': avg_throughput,
            'operations': total_operations
        }
    
    def test_memory_usage(self, iterations=1000):
        """Test d'utilisation mémoire"""
        print("\n🧠 Test d'utilisation mémoire")
        print("=" * 50)
        
        import psutil
        import gc
        
        # Mesure initiale
        process = psutil.Process()
        initial_memory = process.memory_info().rss / 1024 / 1024  # MB
        
        # Exécution des opérations
        for i in range(iterations):
            with patch('builtins.print'):
                self.operations.view_balance()
                if i % 100 == 0:
                    gc.collect()  # Force garbage collection
        
        final_memory = process.memory_info().rss / 1024 / 1024  # MB
        memory_increase = final_memory - initial_memory
        
        print(f"📊 Utilisation mémoire :")
        print(f"   📏 Mémoire initiale : {initial_memory:.1f} MB")
        print(f"   📏 Mémoire finale : {final_memory:.1f} MB")
        print(f"   📈 Augmentation : {memory_increase:.1f} MB")
        print(f"   📊 Par opération : {memory_increase*1024/iterations:.2f} KB")
        
        # Critère : pas plus de 50MB d'augmentation pour 1000 opérations
        assert memory_increase < 50, f"Fuite mémoire détectée : +{memory_increase:.1f}MB"
        
        return {
            'initial': initial_memory,
            'final': final_memory,
            'increase': memory_increase
        }
    
    def test_stress_test(self, duration_seconds=10):
        """Test de stress sur une durée donnée"""
        print(f"\n🔥 Test de stress - {duration_seconds} secondes")
        print("=" * 50)
        
        start_time = time.time()
        operations_count = 0
        errors_count = 0
        
        while time.time() - start_time < duration_seconds:
            try:
                with patch('builtins.print'):
                    self.operations.view_balance()
                operations_count += 1
            except Exception as e:
                errors_count += 1
                print(f"❌ Erreur : {e}")
        
        actual_duration = time.time() - start_time
        ops_per_second = operations_count / actual_duration
        error_rate = errors_count / operations_count if operations_count > 0 else 0
        
        print(f"📊 Résultats stress test :")
        print(f"   ⏱️  Durée réelle : {actual_duration:.1f}s")
        print(f"   🔢 Opérations : {operations_count}")
        print(f"   ❌ Erreurs : {errors_count}")
        print(f"   ⚡ Ops/seconde : {ops_per_second:.0f}")
        print(f"   📊 Taux d'erreur : {error_rate*100:.2f}%")
        
        # Critères de stress
        assert ops_per_second > 50, f"Performance stress insuffisante : {ops_per_second:.0f} < 50 ops/sec"
        assert error_rate < 0.01, f"Taux d'erreur trop élevé : {error_rate*100:.2f}% > 1%"
        
        return {
            'duration': actual_duration,
            'operations': operations_count,
            'errors': errors_count,
            'ops_per_second': ops_per_second,
            'error_rate': error_rate
        }
    
    def generate_performance_report(self):
        """Génère un rapport complet de performance"""
        print("\n" + "="*60)
        print("🚀 RAPPORT COMPLET DE PERFORMANCE")
        print("="*60)
        
        results = {}
        
        try:
            # Tests de base
            results['view_balance'] = self.test_view_balance_performance()
            results['credit'] = self.test_credit_performance()
            results['debit'] = self.test_debit_performance()
            
            # Tests avancés
            results['concurrent'] = self.test_concurrent_operations()
            results['memory'] = self.test_memory_usage()
            results['stress'] = self.test_stress_test()
            
            # Résumé global
            print("\n📋 RÉSUMÉ GLOBAL")
            print("-" * 30)
            print("✅ Tous les tests de performance réussis")
            print(f"⚡ Débit maximal : {results['concurrent']['throughput']:.0f} ops/sec")
            print(f"🧠 Empreinte mémoire : {results['memory']['increase']:.1f} MB/1000 ops")
            print(f"🎯 Performance stress : {results['stress']['ops_per_second']:.0f} ops/sec")
            
            # Sauvegarde du rapport
            self._save_performance_report(results)
            
        except Exception as e:
            print(f"❌ Erreur durant les tests : {e}")
            raise
        
        finally:
            self.cleanup()
    
    def _save_performance_report(self, results):
        """Sauvegarde le rapport de performance"""
        import json
        from datetime import datetime
        
        report = {
            'timestamp': datetime.now().isoformat(),
            'results': results,
            'environment': {
                'python_version': sys.version,
                'platform': sys.platform
            }
        }
        
        with open('performance_report.json', 'w') as f:
            json.dump(report, f, indent=2)
        
        print("\n💾 Rapport sauvegardé : performance_report.json")
    
    def cleanup(self):
        """Nettoyage après les tests"""
        try:
            os.unlink(self.temp_file.name)
        except:
            pass

def main():
    """Point d'entrée principal"""
    print("🎯 SUITE DE TESTS DE PERFORMANCE")
    print("Application Comptable Python - Standards RNCP")
    print("=" * 60)
    
    # Vérification des prérequis
    try:
        import psutil
    except ImportError:
        print("❌ Erreur : pip install psutil requis pour les tests mémoire")
        sys.exit(1)
    
    # Exécution de la suite de tests
    suite = PerformanceTestSuite()
    suite.generate_performance_report()
    
    print("\n🎉 Tests de performance terminés avec succès !")

if __name__ == "__main__":
    main()
