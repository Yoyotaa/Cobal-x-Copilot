"""
Automated Test Suite for Python Accounting Application
This test suite implements comprehensive automated tests with detailed reporting
including Test Case ID, Description, Pre-conditions, Test Steps, Expected Result, etc.
"""

import unittest
import json
import os
import tempfile
import io
import sys
from unittest.mock import patch, MagicMock
from contextlib import redirect_stdout, redirect_stderr
from datetime import datetime

from operations import Operations
from data import DataProgram
from main import MainProgram


class TestResult:
    """Class to store test results with all required fields"""
    def __init__(self, test_id, description, preconditions, test_steps, expected_result):
        self.test_id = test_id
        self.description = description
        self.preconditions = preconditions
        self.test_steps = test_steps
        self.expected_result = expected_result
        self.actual_result = ""
        self.status = "FAIL"
        self.comments = ""
        self.execution_time = None


class AutomatedTestSuite(unittest.TestCase):
    """Comprehensive automated test suite for the accounting application"""
    
    def setUp(self):
        """Set up test fixtures before each test method."""
        # Create a temporary file for testing
        self.temp_file = tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.json')
        self.temp_file.close()
        
        # Initialize test objects
        self.operations = Operations()
        self.operations.data_program = DataProgram(self.temp_file.name)
        self.main_program = MainProgram()
        self.main_program.operations = self.operations
        
        # Test results storage
        self.test_results = []
        
        # Set initial balance for consistency
        self._set_balance(1000.00)
    
    def tearDown(self):
        """Clean up after each test method."""
        # Remove temporary file
        if os.path.exists(self.temp_file.name):
            os.unlink(self.temp_file.name)
    
    def _set_balance(self, balance):
        """Helper method to set initial balance"""
        with open(self.temp_file.name, 'w') as f:
            json.dump({'balance': balance}, f)
    
    def _get_balance(self):
        """Helper method to get current balance"""
        with open(self.temp_file.name, 'r') as f:
            data = json.load(f)
            return data['balance']
    
    def _record_test_result(self, test_result, success, actual_result, comments=""):
        """Record test result with all required fields"""
        test_result.actual_result = actual_result
        test_result.status = "PASS" if success else "FAIL"
        test_result.comments = comments
        test_result.execution_time = datetime.now().strftime("%Y-%m-%d %H:%M:%S")
        self.test_results.append(test_result)
    
    # ==================== TC-1: VIEW BALANCE TESTS ====================
    
    def test_tc_1_1_view_balance_default(self):
        """TC-1.1: View Current Balance - Default Balance"""
        test_result = TestResult(
            test_id="TC-1.1",
            description="View Current Balance with Default Balance (1000.00)",
            preconditions="Application initialized with default balance of 1000.00",
            test_steps=[
                "1. Initialize Operations module",
                "2. Call view_balance() method",
                "3. Verify balance display"
            ],
            expected_result="Current balance: 1000.00 is displayed and returned"
        )
        
        try:
            # Capture output
            with patch('builtins.print') as mock_print:
                result = self.operations.view_balance()
            
            # Verify results
            success = (result == 1000.00 and 
                      mock_print.called and 
                      "Current balance: 1000.00" in str(mock_print.call_args))
            
            actual_result = f"Balance returned: {result}, Print called: {mock_print.called}"
            self._record_test_result(test_result, success, actual_result)
            
            self.assertEqual(result, 1000.00)
            mock_print.assert_called_with("Current balance: 1000.00")
            
        except Exception as e:
            self._record_test_result(test_result, False, f"Exception: {str(e)}")
            raise
    
    def test_tc_1_2_view_balance_custom(self):
        """TC-1.2: View Current Balance - Custom Balance"""
        test_result = TestResult(
            test_id="TC-1.2",
            description="View Current Balance with Custom Balance (2500.75)",
            preconditions="Application initialized with custom balance of 2500.75",
            test_steps=[
                "1. Set balance to 2500.75",
                "2. Call view_balance() method",
                "3. Verify correct balance display"
            ],
            expected_result="Current balance: 2500.75 is displayed and returned"
        )
        
        try:
            # Set custom balance
            self._set_balance(2500.75)
            
            with patch('builtins.print') as mock_print:
                result = self.operations.view_balance()
            
            success = (result == 2500.75 and 
                      "Current balance: 2500.75" in str(mock_print.call_args))
            
            actual_result = f"Balance returned: {result}"
            self._record_test_result(test_result, success, actual_result)
            
            self.assertEqual(result, 2500.75)
            
        except Exception as e:
            self._record_test_result(test_result, False, f"Exception: {str(e)}")
            raise
    
    def test_tc_1_3_view_balance_zero(self):
        """TC-1.3: View Current Balance - Zero Balance"""
        test_result = TestResult(
            test_id="TC-1.3",
            description="View Current Balance with Zero Balance",
            preconditions="Application initialized with zero balance",
            test_steps=[
                "1. Set balance to 0.00",
                "2. Call view_balance() method",
                "3. Verify zero balance display"
            ],
            expected_result="Current balance: 0.00 is displayed and returned"
        )
        
        try:
            self._set_balance(0.00)
            
            with patch('builtins.print') as mock_print:
                result = self.operations.view_balance()
            
            success = (result == 0.00 and 
                      "Current balance: 0.00" in str(mock_print.call_args))
            
            actual_result = f"Balance returned: {result}"
            self._record_test_result(test_result, success, actual_result)
            
            self.assertEqual(result, 0.00)
            
        except Exception as e:
            self._record_test_result(test_result, False, f"Exception: {str(e)}")
            raise
    
    # ==================== TC-2: CREDIT ACCOUNT TESTS ====================
    
    def test_tc_2_1_credit_valid_amount(self):
        """TC-2.1: Credit Account with Valid Amount"""
        test_result = TestResult(
            test_id="TC-2.1",
            description="Credit Account with Valid Amount (250.50)",
            preconditions="Account balance is 1000.00",
            test_steps=[
                "1. Input credit amount of 250.50",
                "2. Call credit_account() method",
                "3. Verify new balance calculation",
                "4. Verify balance persistence"
            ],
            expected_result="New balance: 1250.50, success message displayed"
        )
        
        try:
            with patch('builtins.input', return_value='250.50'):
                with patch('builtins.print') as mock_print:
                    result = self.operations.credit_account()
            
            new_balance = self._get_balance()
            success = (result == 1250.50 and new_balance == 1250.50)
            
            actual_result = f"Returned balance: {result}, Stored balance: {new_balance}"
            self._record_test_result(test_result, success, actual_result)
            
            self.assertEqual(result, 1250.50)
            self.assertEqual(new_balance, 1250.50)
            
        except Exception as e:
            self._record_test_result(test_result, False, f"Exception: {str(e)}")
            raise
    
    def test_tc_2_2_credit_large_amount(self):
        """TC-2.2: Credit Account with Large Amount"""
        test_result = TestResult(
            test_id="TC-2.2",
            description="Credit Account with Large Amount (10000.00)",
            preconditions="Account balance is 1000.00",
            test_steps=[
                "1. Input credit amount of 10000.00",
                "2. Call credit_account() method",
                "3. Verify new balance calculation",
                "4. Verify no overflow issues"
            ],
            expected_result="New balance: 11000.00, success message displayed"
        )
        
        try:
            with patch('builtins.input', return_value='10000.00'):
                with patch('builtins.print') as mock_print:
                    result = self.operations.credit_account()
            
            new_balance = self._get_balance()
            success = (result == 11000.00 and new_balance == 11000.00)
            
            actual_result = f"Returned balance: {result}, Stored balance: {new_balance}"
            self._record_test_result(test_result, success, actual_result)
            
            self.assertEqual(result, 11000.00)
            
        except Exception as e:
            self._record_test_result(test_result, False, f"Exception: {str(e)}")
            raise
    
    def test_tc_2_3_credit_decimal_precision(self):
        """TC-2.3: Credit Account with High Decimal Precision"""
        test_result = TestResult(
            test_id="TC-2.3",
            description="Credit Account with High Decimal Precision (123.456)",
            preconditions="Account balance is 1000.00",
            test_steps=[
                "1. Input credit amount of 123.456",
                "2. Call credit_account() method",
                "3. Verify decimal handling",
                "4. Check rounding behavior"
            ],
            expected_result="New balance: 1123.456, proper decimal handling"
        )
        
        try:
            with patch('builtins.input', return_value='123.456'):
                with patch('builtins.print') as mock_print:
                    result = self.operations.credit_account()
            
            success = abs(result - 1123.456) < 0.001
            actual_result = f"Returned balance: {result}"
            self._record_test_result(test_result, success, actual_result)
            
            self.assertAlmostEqual(result, 1123.456, places=3)
            
        except Exception as e:
            self._record_test_result(test_result, False, f"Exception: {str(e)}")
            raise
    
    def test_tc_2_4_credit_invalid_negative(self):
        """TC-2.4: Credit Account with Invalid Negative Amount"""
        test_result = TestResult(
            test_id="TC-2.4",
            description="Credit Account with Invalid Negative Amount (-100.00)",
            preconditions="Account balance is 1000.00",
            test_steps=[
                "1. Input negative credit amount of -100.00",
                "2. Input valid amount of 100.00 on retry",
                "3. Verify error handling",
                "4. Verify eventual success"
            ],
            expected_result="Error message for negative amount, then success with valid amount"
        )
        
        try:
            with patch('builtins.input', side_effect=['-100.00', '100.00']):
                with patch('builtins.print') as mock_print:
                    result = self.operations.credit_account()
            
            success = result == 1100.00
            actual_result = f"Final balance: {result}"
            self._record_test_result(test_result, success, actual_result, 
                                   "System correctly handled negative input and retried")
            
            self.assertEqual(result, 1100.00)
            
        except Exception as e:
            self._record_test_result(test_result, False, f"Exception: {str(e)}")
            raise
    
    def test_tc_2_5_credit_invalid_text(self):
        """TC-2.5: Credit Account with Invalid Text Input"""
        test_result = TestResult(
            test_id="TC-2.5",
            description="Credit Account with Invalid Text Input ('abc')",
            preconditions="Account balance is 1000.00",
            test_steps=[
                "1. Input invalid text 'abc'",
                "2. Input valid amount of 50.00 on retry",
                "3. Verify error handling",
                "4. Verify eventual success"
            ],
            expected_result="Error message for invalid input, then success with valid amount"
        )
        
        try:
            with patch('builtins.input', side_effect=['abc', '50.00']):
                with patch('builtins.print') as mock_print:
                    result = self.operations.credit_account()
            
            success = result == 1050.00
            actual_result = f"Final balance: {result}"
            self._record_test_result(test_result, success, actual_result,
                                   "System correctly handled text input and retried")
            
            self.assertEqual(result, 1050.00)
            
        except Exception as e:
            self._record_test_result(test_result, False, f"Exception: {str(e)}")
            raise
    
    # ==================== TC-3: DEBIT ACCOUNT TESTS ====================
    
    def test_tc_3_1_debit_valid_sufficient_funds(self):
        """TC-3.1: Debit Account with Valid Amount - Sufficient Funds"""
        test_result = TestResult(
            test_id="TC-3.1",
            description="Debit Account with Valid Amount and Sufficient Funds (300.00)",
            preconditions="Account balance is 1000.00",
            test_steps=[
                "1. Input debit amount of 300.00",
                "2. Call debit_account() method",
                "3. Verify funds check",
                "4. Verify new balance calculation"
            ],
            expected_result="New balance: 700.00, success message displayed"
        )
        
        try:
            with patch('builtins.input', return_value='300.00'):
                with patch('builtins.print') as mock_print:
                    result = self.operations.debit_account()
            
            new_balance = self._get_balance()
            success = (result == 700.00 and new_balance == 700.00)
            
            actual_result = f"Returned balance: {result}, Stored balance: {new_balance}"
            self._record_test_result(test_result, success, actual_result)
            
            self.assertEqual(result, 700.00)
            
        except Exception as e:
            self._record_test_result(test_result, False, f"Exception: {str(e)}")
            raise
    
    def test_tc_3_2_debit_insufficient_funds(self):
        """TC-3.2: Debit Account with Insufficient Funds"""
        test_result = TestResult(
            test_id="TC-3.2",
            description="Debit Account with Amount Greater Than Balance (1500.00)",
            preconditions="Account balance is 1000.00",
            test_steps=[
                "1. Input debit amount of 1500.00",
                "2. Call debit_account() method",
                "3. Verify insufficient funds check",
                "4. Verify balance remains unchanged"
            ],
            expected_result="Insufficient funds message, balance remains 1000.00"
        )
        
        try:
            with patch('builtins.input', return_value='1500.00'):
                with patch('builtins.print') as mock_print:
                    result = self.operations.debit_account()
            
            new_balance = self._get_balance()
            success = (result == 1000.00 and new_balance == 1000.00)
            
            actual_result = f"Returned balance: {result}, Stored balance: {new_balance}"
            self._record_test_result(test_result, success, actual_result,
                                   "Insufficient funds correctly detected")
            
            self.assertEqual(result, 1000.00)
            
        except Exception as e:
            self._record_test_result(test_result, False, f"Exception: {str(e)}")
            raise
    
    def test_tc_3_3_debit_exact_balance(self):
        """TC-3.3: Debit Account with Exact Balance Amount"""
        test_result = TestResult(
            test_id="TC-3.3",
            description="Debit Account with Exact Balance Amount (1000.00)",
            preconditions="Account balance is 1000.00",
            test_steps=[
                "1. Input debit amount of 1000.00",
                "2. Call debit_account() method",
                "3. Verify exact amount handling",
                "4. Verify balance becomes zero"
            ],
            expected_result="New balance: 0.00, success message displayed"
        )
        
        try:
            with patch('builtins.input', return_value='1000.00'):
                with patch('builtins.print') as mock_print:
                    result = self.operations.debit_account()
            
            new_balance = self._get_balance()
            success = (result == 0.00 and new_balance == 0.00)
            
            actual_result = f"Returned balance: {result}, Stored balance: {new_balance}"
            self._record_test_result(test_result, success, actual_result)
            
            self.assertEqual(result, 0.00)
            
        except Exception as e:
            self._record_test_result(test_result, False, f"Exception: {str(e)}")
            raise
    
    def test_tc_3_4_debit_invalid_negative(self):
        """TC-3.4: Debit Account with Invalid Negative Amount"""
        test_result = TestResult(
            test_id="TC-3.4",
            description="Debit Account with Invalid Negative Amount (-50.00)",
            preconditions="Account balance is 1000.00",
            test_steps=[
                "1. Input negative debit amount of -50.00",
                "2. Input valid amount of 50.00 on retry",
                "3. Verify error handling",
                "4. Verify eventual success"
            ],
            expected_result="Error message for negative amount, then success with valid amount"
        )
        
        try:
            with patch('builtins.input', side_effect=['-50.00', '50.00']):
                with patch('builtins.print') as mock_print:
                    result = self.operations.debit_account()
            
            success = result == 950.00
            actual_result = f"Final balance: {result}"
            self._record_test_result(test_result, success, actual_result,
                                   "System correctly handled negative input and retried")
            
            self.assertEqual(result, 950.00)
            
        except Exception as e:
            self._record_test_result(test_result, False, f"Exception: {str(e)}")
            raise
    
    # ==================== TC-4: INTEGRATION TESTS ====================
    
    def test_tc_4_1_multiple_operations_sequence(self):
        """TC-4.1: Multiple Operations Sequence"""
        test_result = TestResult(
            test_id="TC-4.1",
            description="Sequence of Multiple Operations (Credit->Debit->View)",
            preconditions="Account balance is 1000.00",
            test_steps=[
                "1. Credit 500.00 (balance should be 1500.00)",
                "2. Debit 300.00 (balance should be 1200.00)",
                "3. View balance (should show 1200.00)",
                "4. Verify all operations work together"
            ],
            expected_result="Final balance: 1200.00 after sequence of operations"
        )
        
        try:
            # Credit 500.00
            with patch('builtins.input', return_value='500.00'):
                with patch('builtins.print'):
                    credit_result = self.operations.credit_account()
            
            # Debit 300.00
            with patch('builtins.input', return_value='300.00'):
                with patch('builtins.print'):
                    debit_result = self.operations.debit_account()
            
            # View balance
            with patch('builtins.print'):
                view_result = self.operations.view_balance()
            
            success = (credit_result == 1500.00 and 
                      debit_result == 1200.00 and 
                      view_result == 1200.00)
            
            actual_result = f"Credit: {credit_result}, Debit: {debit_result}, View: {view_result}"
            self._record_test_result(test_result, success, actual_result)
            
            self.assertEqual(view_result, 1200.00)
            
        except Exception as e:
            self._record_test_result(test_result, False, f"Exception: {str(e)}")
            raise
    
    def test_tc_4_2_data_persistence(self):
        """TC-4.2: Data Persistence Between Operations"""
        test_result = TestResult(
            test_id="TC-4.2",
            description="Data Persistence Between Operations",
            preconditions="Account balance is 1000.00",
            test_steps=[
                "1. Create new Operations instance",
                "2. Credit 200.00",
                "3. Create another Operations instance",
                "4. Verify balance persists across instances"
            ],
            expected_result="Balance persists across different Operations instances"
        )
        
        try:
            # First operation instance
            ops1 = Operations()
            ops1.data_program = DataProgram(self.temp_file.name)
            
            with patch('builtins.input', return_value='200.00'):
                with patch('builtins.print'):
                    result1 = ops1.credit_account()
            
            # Second operation instance
            ops2 = Operations()
            ops2.data_program = DataProgram(self.temp_file.name)
            
            with patch('builtins.print'):
                result2 = ops2.view_balance()
            
            success = (result1 == 1200.00 and result2 == 1200.00)
            actual_result = f"First instance result: {result1}, Second instance result: {result2}"
            self._record_test_result(test_result, success, actual_result)
            
            self.assertEqual(result2, 1200.00)
            
        except Exception as e:
            self._record_test_result(test_result, False, f"Exception: {str(e)}")
            raise
    
    def generate_test_report(self):
        """Generate comprehensive test report"""
        report = []
        report.append("# Automated Test Suite Report")
        report.append(f"Generated on: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")
        report.append("")
        report.append("## Test Summary")
        
        total_tests = len(self.test_results)
        passed_tests = sum(1 for result in self.test_results if result.status == "PASS")
        failed_tests = total_tests - passed_tests
        
        report.append(f"- Total Tests: {total_tests}")
        report.append(f"- Passed: {passed_tests}")
        report.append(f"- Failed: {failed_tests}")
        report.append(f"- Success Rate: {(passed_tests/total_tests*100):.1f}%")
        report.append("")
        
        report.append("## Detailed Test Results")
        report.append("")
        report.append("| Test Case ID | Description | Pre-conditions | Test Steps | Expected Result | Actual Result | Status | Comments |")
        report.append("|--------------|-------------|----------------|------------|-----------------|---------------|---------|----------|")
        
        for result in self.test_results:
            steps_str = " | ".join(result.test_steps) if isinstance(result.test_steps, list) else str(result.test_steps)
            report.append(f"| {result.test_id} | {result.description} | {result.preconditions} | {steps_str} | {result.expected_result} | {result.actual_result} | {result.status} | {result.comments} |")
        
        return "\n".join(report)


def run_automated_tests():
    """Run all automated tests and generate report"""
    # Create test suite
    loader = unittest.TestLoader()
    suite = loader.loadTestsFromTestCase(AutomatedTestSuite)
    
    # Run tests
    runner = unittest.TextTestRunner(verbosity=2)
    result = runner.run(suite)
    
    # Generate and save report
    test_instance = AutomatedTestSuite()
    test_instance.setUp()
    
    # Run each test to collect results
    for test_method in [
        'test_tc_1_1_view_balance_default',
        'test_tc_1_2_view_balance_custom', 
        'test_tc_1_3_view_balance_zero',
        'test_tc_2_1_credit_valid_amount',
        'test_tc_2_2_credit_large_amount',
        'test_tc_2_3_credit_decimal_precision',
        'test_tc_2_4_credit_invalid_negative',
        'test_tc_2_5_credit_invalid_text',
        'test_tc_3_1_debit_valid_sufficient_funds',
        'test_tc_3_2_debit_insufficient_funds',
        'test_tc_3_3_debit_exact_balance',
        'test_tc_3_4_debit_invalid_negative',
        'test_tc_4_1_multiple_operations_sequence',
        'test_tc_4_2_data_persistence'
    ]:
        try:
            test_instance.setUp()
            getattr(test_instance, test_method)()
        except Exception:
            pass  # Results are recorded in the test methods
        finally:
            test_instance.tearDown()
    
    # Generate report
    report = test_instance.generate_test_report()
    
    # Save report to file
    with open('test_report.md', 'w') as f:
        f.write(report)
    
    print(f"\nTest report generated: test_report.md")
    print(f"Tests run: {result.testsRun}")
    print(f"Failures: {len(result.failures)}")
    print(f"Errors: {len(result.errors)}")


if __name__ == '__main__':
    run_automated_tests()
