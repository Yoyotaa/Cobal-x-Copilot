import os
import sys

# Ensure the project root (where data.py, operations.py, main.py live) is importable
PROJECT_ROOT = os.path.dirname(os.path.abspath(__file__))
APP_ROOT = os.path.dirname(PROJECT_ROOT)
if APP_ROOT not in sys.path:
    sys.path.insert(0, APP_ROOT)
