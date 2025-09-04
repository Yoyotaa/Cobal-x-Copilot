#!/usr/bin/env python3
"""
Test script to verify the COBOL to Python conversion works correctly.
This script tests the basic functionality without requiring user input.
"""

from data import DataProgram
from operations import Operations

def test_data_program():
    """Test the DataProgram class functionality"""
    print("Testing DataProgram...")

    data = DataProgram()

    # Test initial balance
    assert data.get_balance() == 1000.00, f"Expected 1000.00, got {data.get_balance()}"
    print("OK Initial balance is correct")

    # Test write operation
    data.write(1500.00)
    assert data.get_balance() == 1500.00, f"Expected 1500.00, got {data.get_balance()}"
    print("OK Write operation works correctly")

    # Test read operation
    balance = data.read()
    assert balance == 1500.00, f"Expected 1500.00, got {balance}"
    print("OK Read operation works correctly")

    print("DataProgram tests passed!\n")

def test_operations():
    """Test the Operations class functionality"""
    print("Testing Operations...")

    ops = Operations()

    # Test view balance
    balance = ops.view_balance()
    assert balance == 1000.00, f"Expected 1000.00, got {balance}"
    print("OK View balance works correctly")

    # Test credit account
    new_balance = ops.credit_account(500.00)
    assert new_balance == 1500.00, f"Expected 1500.00, got {new_balance}"
    print("OK Credit account works correctly")

    # Test debit account with sufficient funds
    new_balance = ops.debit_account(200.00)
    assert new_balance == 1300.00, f"Expected 1300.00, got {new_balance}"
    print("OK Debit account with sufficient funds works correctly")

    # Test debit account with insufficient funds
    new_balance = ops.debit_account(2000.00)
    assert new_balance == 1300.00, f"Expected 1300.00 (unchanged), got {new_balance}"
    print("OK Debit account with insufficient funds works correctly")

    print("Operations tests passed!\n")

def test_integration():
    """Test integration between modules"""
    print("Testing Integration...")

    from main import MainProgram

    # Test that MainProgram can be instantiated
    main = MainProgram()
    assert main.continue_flag == True, "MainProgram should start with continue_flag = True"
    print("OK MainProgram can be instantiated")

    # Test that operations are accessible
    assert hasattr(main.operations, 'view_balance'), "Operations should have view_balance method"
    assert hasattr(main.operations, 'credit_account'), "Operations should have credit_account method"
    assert hasattr(main.operations, 'debit_account'), "Operations should have debit_account method"
    print("OK MainProgram has access to all operations")

    print("Integration tests passed!\n")

def main():
    """Run all tests"""
    print("=" * 50)
    print("COBOL to Python Conversion Test Suite")
    print("=" * 50)

    try:
        test_data_program()
        test_operations()
        test_integration()

        print("=" * 50)
        print("ALL TESTS PASSED!")
        print("The COBOL to Python conversion is working correctly.")
        print("=" * 50)

    except Exception as e:
        print(f"❌ Test failed: {e}")
        return 1

    return 0

if __name__ == "__main__":
    exit(main())
