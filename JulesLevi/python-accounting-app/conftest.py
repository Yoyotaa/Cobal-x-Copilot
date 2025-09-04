"""
Pytest configuration and runner for the automated test suite
"""

import pytest
import sys
import os
from datetime import datetime

# Add the parent directory to the path so we can import our modules
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

def pytest_configure(config):
    """Configure pytest with custom markers and settings"""
    config.addinivalue_line(
        "markers", "unit: mark test as a unit test"
    )
    config.addinivalue_line(
        "markers", "integration: mark test as an integration test"
    )
    config.addinivalue_line(
        "markers", "view_balance: mark test as testing view balance functionality"
    )
    config.addinivalue_line(
        "markers", "credit: mark test as testing credit functionality"
    )
    config.addinivalue_line(
        "markers", "debit: mark test as testing debit functionality"
    )

def pytest_html_report_title(report):
    """Customize HTML report title"""
    report.title = f"Python Accounting App Test Report - {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}"

def pytest_collection_modifyitems(config, items):
    """Modify test items during collection"""
    for item in items:
        # Add markers based on test names
        if "view_balance" in item.name:
            item.add_marker(pytest.mark.view_balance)
        elif "credit" in item.name:
            item.add_marker(pytest.mark.credit)
        elif "debit" in item.name:
            item.add_marker(pytest.mark.debit)
        
        if "tc_4" in item.name:
            item.add_marker(pytest.mark.integration)
        else:
            item.add_marker(pytest.mark.unit)
