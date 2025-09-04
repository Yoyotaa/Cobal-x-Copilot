"""
Account Management System - Python Rework
==========================================

Cette application est une réécriture en Python d'un système de gestion de comptes
originalement écrit en COBOL. Elle respecte fidèlement la logique métier tout en
appliquant les bonnes pratiques Python modernes.

Fonctionnalités :
- Consultation du solde
- Crédit de compte
- Débit de compte avec vérification des fonds
- Interface console interactive

Architecture :
- Modèle de données : Account (dataclass)
- Services métier : AccountService
- Persistance : DataService (JSON)
- Interface : ConsoleInterface
"""

from setuptools import setup, find_packages

setup(
    name="account-management-system",
    version="1.0.0",
    description="Système de gestion de comptes bancaires (réécriture Python d'un système COBOL)",
    long_description=__doc__,
    author="Expert Developer",
    packages=find_packages(where="src"),
    package_dir={"": "src"},
    python_requires=">=3.8",
    install_requires=[],
    extras_require={
        "dev": [
            "pytest>=7.0.0",
            "pytest-cov>=4.0.0",
            "black>=22.0.0",
            "flake8>=5.0.0",
            "mypy>=1.0.0",
        ]
    },
    entry_points={
        "console_scripts": [
            "account-system=main:main",
        ],
    },
    classifiers=[
        "Development Status :: 5 - Production/Stable",
        "Intended Audience :: Financial and Insurance Industry",
        "License :: OSI Approved :: MIT License",
        "Programming Language :: Python :: 3",
        "Programming Language :: Python :: 3.8",
        "Programming Language :: Python :: 3.9",
        "Programming Language :: Python :: 3.10",
        "Programming Language :: Python :: 3.11",
    ],
)
